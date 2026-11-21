(in-package #:clempo.executor)

;;; The transaction pipeline: tempo crates/evm/src/handler.rs (0x76) and handler/config.rs
;;; (legacy, 2930, 1559, 7702 with TempoHandlerHooks) over evm2's ethereum handler.

(define-constant +gas-price-scaling+ (expt 10 12) "TEMPO_GAS_PRICE_SCALING_FACTOR")
(define-constant +expiring-nonce-key+ (1- (ash 1 256)) "TEMPO_EXPIRING_NONCE_KEY")
(define-constant +storage-credits-address+ #x1060000000000000000000000000000000000000)
(define-constant +fee-manager-address+ #xfeec000000000000000000000000000000000000)
(define-constant +stablecoin-dex-address+ #xdec0000000000000000000000000000000000000)
(define-constant +max-u64+ (1- (ash 1 64)))

(defstruct (block-context (:conc-name ctx-))
  db env block-env-evm world host
  evm-spec                              ; (tempo-evm-spec (block-env-spec env)), looked up once
  (section :start)                      ; :start :non-shared :system
  (non-shared-left 0) (non-payment-left 0)
  (cumulative 0) (block-gas 0) (receipts '()) (index 0)
  skip-nonce-check skip-valid-after-check skip-base-fee-check skip-liquidity-check
  skip-gas-caps                         ; RPC requests: no transaction or block gas cap
  skip-fee-charge                       ; RPC requests: no fee collection, balance check or settlement
  skip-eip3607                          ; RPC requests: a sender with code may send
  key-expiry                            ; access-key expiry resolved for the running tx
  validate-only)                        ; stop after fee collection (pool admission)

(defvar *tx-index* nil "Index of the transaction being executed, for conditions.")
(defvar *ctx* nil "The block context of the running transaction.")

(defun calc-gas-balance-spending (gas price)
  "Fee in TIP-20 base units: ceil(gas * price / 10^12)."
  (ceiling (* gas price) +gas-price-scaling+))

(defmacro with-protocol-storage ((&key (gas-limit '+max-u64+) static) &body body)
  "StorageCtx::enter_evm_without_tip1060_accounting: precompile code run by the handler, metered
on its own gas (unlimited unless GAS-LIMIT) without TIP-1060 accounting. Returns BODY's values."
  `(let ((contract:*storage* (contract:make-metered-storage *host* :gas-limit ,gas-limit :static ,static
                                                                  :tip1060 nil)))
     ,@body))

;;; A transaction as the handler sees it.

(defstruct (txv (:constructor %make-txv))
  tx (type 0) (sender 0) (index 0)
  (nonce 0) (gas-limit 0) (gas-price 0) max-fee (max-priority 0)
  to (value 0) (input (make-octets 0))
  (access-list '())                     ; ((address slot ...) ...)
  (authorizations '())                  ; raw 7702 / tempo authorization items
  (calls '())                           ; AA: ((to-or-nil value input) ...)
  (nonce-key 0) valid-before valid-after fee-token sponsored-p
  key-authorization signature
  (%key-id :unknown)                    ; keychain access key, recovered once
  ;; Set for unsigned requests (eth_call), which have no envelope to derive them from.
  %unique-id %hash %fee-payer (%chain-id :unknown))

(defun txv-key-id (v)
  "The access key of a keychain-signed transaction (NIL otherwise), recovered once."
  (if (eq (txv-%key-id v) :unknown)
      (setf (txv-%key-id v) (tx:transaction-key-id (txv-tx v)))
      (txv-%key-id v)))

(defun txv-unique-id (v)
  "unique_tx_identifier: keccak(signing payload || sender), as an integer."
  (or (txv-%unique-id v)
      (setf (txv-%unique-id v) (octets->integer (tx:transaction-unique-identifier (txv-tx v) (txv-sender v))))))

(defun txv-hash (v)
  (or (txv-%hash v) (setf (txv-%hash v) (octets->integer (tx:transaction-hash (txv-tx v))))))

(defun txv-fee-payer (v)
  "The fee payer: the sender unless the transaction is sponsored."
  (cond ((txv-%fee-payer v))
        ((txv-sponsored-p v) (setf (txv-%fee-payer v) (tx:transaction-fee-payer (txv-tx v))))
        (t (txv-sender v))))

(defun txv-chain-id (v)
  (if (eq (txv-%chain-id v) :unknown)
      (setf (txv-%chain-id v) (tx:transaction-chain-id (txv-tx v)))
      (txv-%chain-id v)))

(defun parse-access-list (raw)
  (mapcar (lambda (entry)
            (cons (octets->integer (first entry)) (mapcar #'octets->integer (second entry))))
          raw))

(defun make-txv (tx sender index)
  (let* ((type (tx:transaction-type tx))
         (v (%make-txv :tx tx :type type :sender sender :index index
                       :nonce (tx:transaction-nonce tx) :gas-limit (tx:transaction-gas-limit tx))))
    (case type
      ((0 1) (setf (txv-gas-price v) (tx:transaction-gas-price tx)))
      (t (setf (txv-max-fee v) (tx:transaction-max-fee-per-gas tx)
               (txv-max-priority v) (tx:transaction-max-priority-fee-per-gas tx))))
    (if (= type #x76)
        (let ((fields (tx:transaction-fields tx)))
          (setf (txv-calls v) (mapcar (lambda (c)
                                        (list (if (zerop (length (first c))) nil (octets->integer (first c)))
                                              (rlp:rlp-decode-integer (second c))
                                              (third c)))
                                      (tx:transaction-calls tx))
                (txv-access-list v) (parse-access-list (tx:transaction-access-list tx))
                (txv-nonce-key v) (tx:transaction-nonce-key tx)
                (txv-valid-before v) (tx:transaction-valid-before tx)
                (txv-valid-after v) (tx:transaction-valid-after tx)
                (txv-fee-token v) (tx:transaction-fee-token tx)
                (txv-sponsored-p v) (consp (nth 11 fields))
                (txv-authorizations v) (tx:transaction-tempo-authorization-list tx)
                (txv-key-authorization v) (when (= (length fields) 15) (nth 13 fields))
                (txv-signature v) (car (last fields))))
        (setf (txv-to v) (tx:transaction-to tx)
              (txv-value v) (tx:transaction-value tx)
              (txv-input v) (tx:transaction-input tx)
              (txv-access-list v) (when (member type '(1 2 4))
                                    (parse-access-list (tx:transaction-access-list tx)))
              (txv-authorizations v) (when (= type 4) (tx:transaction-authorization-list tx))))
    v))

(defun txv-aa-p (v) (= (txv-type v) #x76))
(defun txv-first-call (v)
  "(kind input) of the first call: kind is the target address or :create."
  (if (txv-aa-p v)
      (let ((c (first (txv-calls v)))) (values (or (first c) :create) (third c)))
      (values (or (txv-to v) :create) (txv-input v))))

(defun txv-effective-gas-price (v base-fee)
  (if (txv-max-fee v)
      (min (txv-max-fee v) (+ base-fee (txv-max-priority v)))
      (txv-gas-price v)))

(defun txv-max-fee-per-gas (v) (or (txv-max-fee v) (txv-gas-price v)))

(defun invalid (fmt &rest args)
  (error 'invalid-transaction :tx-index *tx-index* :reason (apply #'format nil fmt args)))

;;; Intrinsic gas (evm2 intrinsic_and_floor_gas, Osaka: no EIP-2780).

(defun calldata-tokens (data)
  (declare (type octets data))
  (let ((zeros (count 0 data))) (+ zeros (* 4 (- (length data) zeros)))))

(defun access-list-counts (access-list)
  (values (length access-list) (reduce #'+ access-list :key (lambda (e) (length (rest e))))))

(defun floor-gas (gp tokens)
  (+ (evm:gas-params-tx-base gp) (* tokens (evm:gas-params-tx-floor-token gp))))

(defun ethereum-intrinsic-gas (gp v)
  (let ((data (txv-input v))
        (create (null (txv-to v))))
    (multiple-value-bind (accounts keys) (access-list-counts (txv-access-list v))
      (let ((tokens (calldata-tokens data)))
        (values (+ (evm:gas-params-tx-base gp) (* 4 tokens)
                   (* accounts (evm:gas-params-tx-access-address gp))
                   (* keys (evm:gas-params-tx-access-slot gp))
                   (if create (+ (evm:gas-params-tx-create gp)
                                 (* (evm:gas-params-initcode-word gp) (ceiling (length data) 32)))
                       0)
                   (* (length (txv-authorizations v)) (evm:gas-params-auth-empty-account gp)))
                (floor-gas gp tokens))))))

;;; Fee context (handler/config.rs resolve_fee_context, collect_fee, settle_transaction)

(defstruct (fee-context (:conc-name fee-)) payer token (collected 0) (max-fee 0) fallback-p)

(defun tip20-address-p (address) (pc:tip20-address-p address))

(defmacro selector (signature)
  "The 4-byte ABI selector of SIGNATURE as an integer, computed at load time."
  `(load-time-value (octets->integer (clempo.keccak:keccak256 (string->octets ,signature)) :end 4)))

(defun fee-inference-call-p (input)
  "common.rs is_tip20_fee_inference_call: transfer, transferWithMemo; distributeReward before T7."
  (and (>= (length input) 4)
       (let ((s (octets->integer input :end 4)))
         (or (= s (selector "transfer(address,uint256)"))
             (= s (selector "transferWithMemo(address,uint256,bytes32)"))
             (and (forks:spec< :t7) (= s (selector "distributeReward(uint256)")))))))

(defun valid-fee-token-p (token)
  (and (tip20-address-p token)
       (with-protocol-storage (:static t)
         (let ((currency (clempo.tip20:currency token)))
           (string= currency "USD")))))

(defun resolve-fee-token (v fee-payer max-fee)
  "fee_manager.rs resolve_fee_token -> (values token fallback-p)."
  (let ((aa (txv-aa-p v)) (sender (txv-sender v)))
    (when (txv-fee-token v) (return-from resolve-fee-token (values (txv-fee-token v) nil)))
    (multiple-value-bind (kind input) (txv-first-call v)
      (when (and (not aa) (= fee-payer sender) (eql kind +fee-manager-address+)
                 (>= (length input) 36) (= (octets->integer input :end 4) (selector "setUserToken(address)")))
        (return-from resolve-fee-token (values (octets->integer input :start 16 :end 36) nil)))
      (let ((user (with-protocol-storage (:static t) (clempo.fee-manager:user-token fee-payer))))
        (unless (zerop user) (return-from resolve-fee-token (values user nil))))
      (when (and (integerp kind)
                 (not (and aa (/= fee-payer sender)))
                 (every (lambda (c) (and (eql (first c) kind) (fee-inference-call-p (third c))))
                        (if aa (txv-calls v) (list (list (txv-to v) 0 (txv-input v)))))
                 (valid-fee-token-p kind))
        (return-from resolve-fee-token (values kind nil)))
      (when (and (eql kind +stablecoin-dex-address+)
                 (or (not aa) (= 1 (length (txv-calls v))))
                 (>= (length input) 36))
        (let ((selector (octets->integer input :end 4))
              (token-in (octets->integer input :start 16 :end 36)))
          ;; swapExactAmountIn / swapExactAmountOut(address tokenIn, ...)
          (when (and (or (= selector (selector "swapExactAmountIn(address,address,uint128,uint128)"))
                         (= selector (selector "swapExactAmountOut(address,address,uint128,uint128)")))
                     (valid-fee-token-p token-in))
            (return-from resolve-fee-token (values token-in nil)))))
      (if (or (forks:spec< :t13) (zerop max-fee))
          (values pc:+default-fee-token+ t)
          (let ((richest pc:+default-fee-token+) (highest 0))
            (dolist (token (pc:fallback-fee-tokens forks:*spec*)
                           (invalid "insufficient fee token balance: ~D < ~D in ~X" highest max-fee richest))
              (let ((balance (with-protocol-storage (:static t)
                               (clempo.tip20:balance-of token fee-payer))))
                (when (>= balance max-fee) (return (values token t)))
                (when (> balance highest) (setf richest token highest balance)))))))))

(defun resolve-fee-context (ctx v fee-payer)
  (let* ((env (ctx-env ctx))
         (price (txv-effective-gas-price v (block-env-base-fee env)))
         (collected (if (ctx-skip-fee-charge ctx) 0 (calc-gas-balance-spending (txv-gas-limit v) price)))
         (max-fee (calc-gas-balance-spending (txv-gas-limit v) (txv-max-fee-per-gas v))))
    (setf *non-creditable* nil)
    (with-protocol-storage ()
      (clempo.keychain:set-tx-origin (txv-sender v))
      (when (forks:spec>= :t14) (seed-upgrade-authority v))
      ;; A transient write only TIP20ChannelReserve (T5) reads, inside this transaction: before
      ;; T5 nothing can observe it.
      (when (forks:spec>= :t5)
        (clempo.channel-reserve:set-channel-open-context-hash (txv-unique-id v))))
    (multiple-value-bind (token fallback-p) (resolve-fee-token v fee-payer max-fee)
      (unless (tip20-address-p token) (invalid "fee token ~X is not TIP-20" token))
      (unless (zerop max-fee)
        (unless (valid-fee-token-p token) (invalid "fee token ~X is not USD" token)))
      ;; Read (and so warm) the payer's balance even when fees are not charged (RPC calls).
      (let ((balance (with-protocol-storage (:static t) (clempo.tip20:balance-of token fee-payer))))
        (when (and (not (ctx-skip-fee-charge ctx)) (< balance max-fee))
          (invalid "insufficient fee token balance: ~D < ~D" balance max-fee)))
      (make-fee-context :payer fee-payer :token token :collected collected :max-fee max-fee
                        :fallback-p fallback-p))))

(defun collect-fee (ctx fee key-id)
  "config.rs collect_fee: pull the maximum fee before execution; roll back on failure. From
T7 the fee payer's fee-token balance slot (and the access key's spending-limit slot) become
non-creditable for the rest of the transaction (NonCreditableSlots)."
  (unless (zerop (fee-collected fee))
    (let ((cp (state:checkpoint evm:*world*)))
      (handler-case
          (with-protocol-storage ()
            (clempo.fee-manager:collect-fee-pre-tx (fee-payer fee) (fee-token fee)
                   (fee-collected fee) (block-env-beneficiary (ctx-env ctx)) (ctx-skip-liquidity-check ctx)))
        (contract:precompile-condition (c)
          (state:rollback evm:*world* cp)
          (when (and (typep c 'contract:precompile-revert)
                     (eq (contract:precompile-revert-name c) 'clempo.fee-manager::insufficient-liquidity))
            (let ((validator-token
                    (handler-case (with-protocol-storage (:static t)
                                    (clempo.fee-manager:validator-token (block-env-beneficiary (ctx-env ctx))))
                      (contract:precompile-condition () nil))))
              (error 'insufficient-amm-liquidity
                     :tx-index *tx-index* :required (fee-collected fee)
                     :user-token (and validator-token (fee-token fee)) :validator-token validator-token
                     :reason (format nil "fee collection failed: ~A" c))))
          (invalid "fee collection failed: ~A" c)))))
  (when (and (forks:spec>= :t7) (not (zerop (fee-collected fee))))
    (let* ((token (fee-token fee)) (payer (fee-payer fee))
           (balance-slot (with-protocol-storage () (clempo.tip20:balance-slot token payer)))
           (limit-slot (when key-id
                         (with-protocol-storage ()
                           (clempo.keychain:spending-limit-remaining-slot payer key-id token)))))
      (setf *non-creditable*
            (lambda (owner slot)
              (or (and (= owner token) (= slot balance-slot))
                  (and limit-slot (= owner #xAAAAAAAA00000000000000000000000000000000)
                       (= slot limit-slot))))))))

(defun settle-storage-credits (success)
  "config.rs settle_storage_credit_refunds: T7 credits pending in transient storage are paid
back as refund gas. Returns the refund to add."
  (if (or (forks:spec< :t7) (not success))
      0
      (let ((slots (state:take-transient-storage evm:*world* +storage-credits-address+))
            (settled 0))
        (with-protocol-storage ()
          (loop for (key . word) in slots
                for pending = (ldb (byte 64 192) word)
                unless (zerop pending)
                  do (let* ((balance (contract:sload +storage-credits-address+ key))
                            (credits (min pending balance)))
                       (unless (zerop credits)
                         (contract:sstore +storage-credits-address+ key (- balance credits))
                         (incf settled credits)))))
        (* settled forks:+storage-credit-value+))))

(defstruct (tx-outcome (:constructor make-tx-outcome
                           (status gas-used block-gas logs
                            &key fee-token fallback-fee-token-p fee-payer key-expiry (validator-fee 0)
                              storage-changes (output (make-octets 0)) halt (refunded 0))))
  "One executed transaction: receipt status, gas, logs, and the fee metadata the pool and
builder read (Rust TempoTxResult / ResolvedFeeToken / key_expiry). OUTPUT is the top frame's
return or revert data; HALT is NIL on success or revert, else the EVM's halt keyword
\(:out-of-gas, :create-collision, :invalid-opcode, ...)."
  status gas-used block-gas (logs '() :type list)
  fee-token fallback-fee-token-p fee-payer key-expiry (validator-fee 0)
  (storage-changes '() :type list)      ; ((address slot original current) ...) of TIP-20 slots
  (output (make-octets 0)) halt
  (refunded 0))                         ; the refund settlement applied (evm2 final_refunded)

(defun halt-kind (status)
  "The halt keyword of an EVM frame STATUS: NIL for success and revert."
  (unless (member status '(:stop :return :revert)) status))

(defun settle (ctx v fee price gas-left extra-refund floor &key (status t))
  "config.rs settle_transaction + evm2 finalize_gas. Returns (values gas-used validator-fee
refund-applied)."
  (let* ((gp (evm:spec-gas evm:*spec*))
         (gas-limit (txv-gas-limit v))
         (refund-counter (+ (state:world-refund evm:*world*) extra-refund (settle-storage-credits status)))
         (spent (- gas-limit gas-left))
         ;; A net-negative refund counter refunds the full cap.
         (refund (if (zerop refund-counter) 0
                     (min (if (minusp refund-counter) most-positive-fixnum refund-counter)
                          (floor spent (evm:gas-params-refund-quotient gp)))))
         (used (max (- spent refund) floor))
         (actual (calc-gas-balance-spending used price))
         (collected (calc-gas-balance-spending gas-limit price))
         (validator-fee 0))
    ;; Without fee charging (RPC calls) settlement ends with the gas accounting.
    (unless (or (ctx-skip-fee-charge ctx) (and (zerop collected) (not (zerop actual))))
      (unless (and (zerop actual) (zerop (- collected actual)))
        (setf validator-fee
              (with-protocol-storage ()
                (clempo.fee-manager:collect-fee-post-tx (fee-payer fee) actual (- collected actual)
                       (fee-token fee) (block-env-beneficiary (ctx-env ctx)))))))
    (values used validator-fee refund)))

;;; The top-level frame (evm2 prepare_initial_frame + execute_initial_frame).

(defun run-initial-frame (caller nonce to input value gas)
  "Run a top-level call or creation. Returns (values status gas-left output); a call straight
into a precompile that runs out of gas ends :PRECOMPILE-OUT-OF-GAS (evm2 PrecompileOutOfGas)."
  (let ((world evm:*world*))
    (if (null to)
        (let ((address (evm:create-address caller nonce)))
          (state:warm-address world address)
          (if (evm:collision-p world (state:get-account world address))
              (progn
                ;; evm2 still reports the create frame, ended by the collision
                (when evm:*tracer*
                  (evm:trace-unstarted :create caller address address value (make-octets 0)
                                       input gas 0 :create-collision 0))
                (values :create-collision 0 (make-octets 0)))
              (evm:create-message caller address value input gas 0)))
        (let* ((code (state:get-code world to))
               (target (state:delegation-target code)))
          (when target
            (state:warm-address world target)
            (setf code (state:get-code world target)))
          (multiple-value-bind (status gas-left output)
              (evm:call-message :call caller to (or target to) value input gas 0 nil
                                :code code :disable-precompiles (and target t))
            (values (if (and (eq status :out-of-gas) (not target)
                             (or (pc:precompile-address-p to forks:*spec*)
                                 (member to (evm:precompile-addresses (evm:spec-precompiles evm:*spec*)))))
                        :precompile-out-of-gas
                        status)
                    gas-left output))))))

(defun tx-logs ()
  (mapcar (lambda (e) (receipts:make-event-log :address (state:log-entry-address e)
                                               :topics (mapcar (lambda (x) (integer->octets x 32))
                                                               (state:log-entry-topics e))
                                               :data (state:log-entry-data e)))
          (state:logs-since evm:*world*)))

;;; Legacy, 2930, 1559 and 7702 transactions.

(defun execute-ethereum-transaction (ctx v)
  (let* ((world evm:*world*) (gp (evm:spec-gas evm:*spec*)) (env (ctx-env ctx))
         (base-fee (block-env-base-fee env))
         (sender (txv-sender v))
         (gas-limit (txv-gas-limit v))
         (price (txv-effective-gas-price v base-fee)))
    ;; validate_no_native_value, then evm2's checks in its order
    (unless (zerop (txv-value v)) (invalid "value transfer not allowed"))
    (when (and (txv-max-fee v) (> (txv-max-priority v) (txv-max-fee v)))
      (invalid "priority fee greater than max fee"))
    (when (and (not (ctx-skip-base-fee-check ctx)) (< price base-fee)) (invalid "fee cap below base fee"))
    (let ((chain-id (txv-chain-id v)))
      (when (and chain-id (/= chain-id (block-env-chain-id env))) (invalid "wrong chain id")))
    (unless (ctx-skip-gas-caps ctx)
      (when (> gas-limit (evm:gas-params-tx-gas-cap gp)) (invalid "gas limit above the transaction cap"))
      (when (> gas-limit (block-env-gas-limit env)) (invalid "gas limit above the block's")))
    (when (and (null (txv-to v)) (> (length (txv-input v)) (evm:gas-params-max-initcode-size gp)))
      (invalid "initcode too large"))
    (when (= (txv-nonce v) +max-u64+) (invalid "nonce overflow"))
    (multiple-value-bind (intrinsic floor) (ethereum-intrinsic-gas gp v)
      ;; adjust_intrinsic_gas (T1+): T14's retired-root check (no gas for the sender), then new
      ;; accounts for a zero nonce and zero-nonce authorizations
      (when (forks:spec>= :t1)
        (incf intrinsic (validate-configurable-state v))
        (incf intrinsic (* (evm:gas-params-new-account gp)
                           (+ (if (zerop (txv-nonce v)) 1 0)
                              (count-if (lambda (a) (zerop (rlp:rlp-decode-integer (third a))))
                                        (txv-authorizations v))))))
      (when (< gas-limit intrinsic) (invalid "intrinsic gas too low"))
      (when (< gas-limit floor) (invalid "gas limit below the calldata floor"))
      ;; validate_sender_and_bump_nonce
      (let* ((account (state:get-account world sender))
             (code (state:account-code world account)))
        (when (and (not (ctx-skip-eip3607 ctx)) (plusp (length code)) (not (state:delegation-target code)))
          (invalid "sender has code"))
        (when (and (not (ctx-skip-nonce-check ctx)) (/= (state:account-nonce account) (txv-nonce v)))
          (if (< (txv-nonce v) (state:account-nonce account))
              (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-low :reason "nonce too low")
              (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-high :reason "nonce too high")))
        (state:warm-address world sender)
        (let ((sender-nonce (state:account-nonce account)))
          (state:increment-nonce world account)
          ;; warm_base_accounts
          (state:warm-address world (block-env-beneficiary env))
          (when (txv-to v) (state:warm-address world (txv-to v)))
          (dolist (a (evm:precompile-addresses (evm:spec-precompiles evm:*spec*))) (state:warm-address world a))
          (dolist (entry (txv-access-list v))
            (state:warm-address world (first entry))
            (dolist (slot (rest entry)) (state:warm-slot world (first entry) slot)))
          (when (txv-authorizations v)
            (state:add-refund world (apply-ethereum-authorizations (txv-authorizations v)
                                                                   (block-env-chain-id env))))
          ;; before_execution: fee context and collection
          (let ((fee (resolve-fee-context ctx v (txv-sender v))))
            (collect-fee ctx fee nil)
            (when (ctx-validate-only ctx) (return-from execute-ethereum-transaction (validated-outcome fee)))
            (let ((evm:*tx-env* (evm:make-tx-env :origin sender :gas-price price)))
              (multiple-value-bind (status gas-left output)
                  (run-initial-frame sender sender-nonce (txv-to v) (txv-input v) 0 (- gas-limit intrinsic))
                (let ((ok (evm:success-p status)))
                  (multiple-value-bind (used validator-fee refunded) (settle ctx v fee price gas-left 0 floor :status ok)
                    (make-tx-outcome ok used used (tx-logs) :fee-token (fee-token fee) :fee-payer (fee-payer fee)
                                                             :fallback-fee-token-p (fee-fallback-p fee)
                                                             :validator-fee validator-fee :refunded refunded
                                                             :output (or output (make-octets 0))
                                                             :halt (halt-kind status))))))))))))

;;; The block context: the incremental API execute-block is built on.


(defmacro with-block-context ((ctx) &body body)
  "Bind the EVM's and the precompiles' context to CTX's block."
  (let ((c (gensym "CTX")))
    `(let* ((,c ,ctx)
            (forks:*spec* (block-env-spec (ctx-env ,c)))
            (evm:*spec* (or (ctx-evm-spec ,c)
                            (setf (ctx-evm-spec ,c) (tempo-evm-spec forks:*spec*))))
            (evm:*world* (ctx-world ,c))
            (evm:*block* (ctx-block-env-evm ,c))
            (*host* (ctx-host ,c))
            ;; per thread: RESOLVE-FEE-CONTEXT and COLLECT-FEE set it for the running transaction
            (*non-creditable* nil))
       ,@body)))

(defun make-context (backend env &key world block-hash-fn overlay)
  "A block context over BACKEND; an OVERLAY context has no database: its writes stay in its world."
  (make-block-context
   :db (unless overlay backend) :env env
   :block-env-evm (evm-block-env backend env block-hash-fn)
   :world (or world (state:make-world backend))
   :host (make-instance 'world-host
                        :block-info (contract:make-block-info
                                     :number (block-env-number env)
                                     :timestamp (block-env-timestamp env)
                                     :beneficiary (block-env-beneficiary env)
                                     :chain-id (block-env-chain-id env)))
   :non-shared-left (- (block-env-gas-limit env) (block-env-shared-gas-limit env))
   :non-payment-left (block-env-general-gas-limit env)))

(defun begin-block (db env &key block (db-block t) world)
  "Open a block on DB for ENV (unless DB-BLOCK is NIL: the caller opened it) and apply the
pre-execution system changes; returns the context APPLY-TRANSACTION and FINISH-BLOCK take.
WORLD, when given, is an empty clempo.state world over DB to reuse (STATE:RESET-WORLD)."
  (let ((ctx (make-context db env :world world)))
    (when db-block (store:db-begin-block db))
    (with-block-context (ctx)
      (when (and block (blocks:block-withdrawals block)) (invalid "withdrawals are not permitted"))
      ;; parent-state checks, before any system call writes
      (check-proposer-beneficiary db env)
      (check-boundary-outcome env)
      (apply-history-and-beacon-calls ctx :parent-beacon-block-root (block-env-parent-beacon-block-root env))
      (apply-pre-execution-changes db env block)
      (apply-zone-factory ctx))
    ctx))

(defun next-section (ctx system-p payment-p gas)
  (case (ctx-section ctx)
    ((:start :non-shared)
     (if system-p :system
         (if (or (> gas (ctx-non-shared-left ctx)) (and (not payment-p) (> gas (ctx-non-payment-left ctx))))
             (invalid "transaction exceeds its gas lane")
             :non-shared)))
    (:system (if system-p (invalid "duplicate subblocks metadata system transaction")
                 (invalid "regular transaction can't follow system transaction")))))

(defun apply-transaction (ctx tx sender)
  "Execute TX in CTX's block and append its receipt; returns a TX-OUTCOME, or signals
INVALID-TRANSACTION with the block as it was before TX."
  (let* ((*tx-index* (ctx-index ctx))
         (*ctx* ctx)
         (*non-creditable* nil)            ; per transaction: RESOLVE-FEE-CONTEXT and COLLECT-FEE set it
         (system-p (system-transaction-p tx)))
    (with-block-context (ctx)
      (let ((outcome
              (if system-p
                  (handler-bind ((execution-failure (lambda (c) (declare (ignore c)) (state:discard-tx evm:*world*))))
                    (execute-system-transaction (ctx-env ctx) tx *tx-index*))
                  (execute-txv ctx (make-txv tx sender *tx-index*)))))
        (let ((section (next-section ctx system-p
                                     (and (not system-p) (payment-transaction-p tx forks:*spec*))
                                     (tx-outcome-block-gas outcome))))
          (setf (tx-outcome-storage-changes outcome) (collect-storage-changes evm:*world*))
          (state:finalize-tx evm:*world*)
          (setf (ctx-section ctx) section)
          (when (eq section :non-shared)
            (decf (ctx-non-shared-left ctx) (tx-outcome-block-gas outcome))
            (unless (payment-transaction-p tx forks:*spec*)
              (decf (ctx-non-payment-left ctx) (tx-outcome-block-gas outcome)))))
        (incf (ctx-cumulative ctx) (tx-outcome-gas-used outcome))
        (incf (ctx-block-gas ctx) (tx-outcome-block-gas outcome))
        (incf (ctx-index ctx))
        ;; the bloom is computed once here for both the receipts root and the block bloom
        ;; (none for no logs: the receipts code reads a shared zero bloom)
        (push (receipts:make-receipt :type (tx:transaction-type tx)
                                     :status (if (tx-outcome-status outcome) 1 0)
                                     :cumulative-gas-used (ctx-cumulative ctx)
                                     :logs (tx-outcome-logs outcome)
                                     :bloom (and (tx-outcome-logs outcome)
                                                 (receipts:logs-bloom (tx-outcome-logs outcome))))
              (ctx-receipts ctx))
        outcome))))

(defun execute-txv (ctx v)
  "Run V's handler in CTX's block (inside WITH-BLOCK-CONTEXT); a failed transaction leaves the
world as it was."
  (handler-bind ((execution-failure (lambda (c) (declare (ignore c)) (state:discard-tx evm:*world*))))
    (when (and (txv-aa-p v) (= (ldb (byte 8 248) (txv-nonce-key v)) #x5b))
      (invalid "subblock transactions are not supported"))
    (if (txv-aa-p v)
        (execute-aa-transaction ctx v)
        (execute-ethereum-transaction ctx v))))

(defun flush-transactions (ctx)
  "Write the changes of CTX's finished transactions (and its pre-execution changes) into its
state database's open block now rather than at FINISH-BLOCK, so the database's state root
covers them: the per-transaction roots of reth's debug_intermediateRoots. Execution goes on
unchanged; the world keeps its accounts and reads the written values back."
  (unless (ctx-db ctx) (error "FLUSH-TRANSACTIONS on an overlay context"))
  (write-changes (ctx-db ctx) (state:drain-changes (ctx-world ctx))))

(defun finish-block (ctx)
  "Run the post-execution system calls, write the block's changes into the state database and
return the BLOCK-RESULT (the db block stays open for the caller to commit or revert)."
  (unless (ctx-db ctx) (error "FINISH-BLOCK on an overlay context: use NEXT-CALL-BLOCK"))
  (with-block-context (ctx)
    (apply-committee-call ctx)
    (write-changes (ctx-db ctx) (state:drain-changes (ctx-world ctx))))
  (let ((receipts (reverse (ctx-receipts ctx))))
    (make-block-result :number (block-env-number (ctx-env ctx))
                       :gas-used (ctx-block-gas ctx)
                       :receipts receipts
                       :receipts-root (receipts:receipts-root receipts)
                       :bloom (receipts:receipts-bloom receipts)
                       :state-root (store:db-state-root (ctx-db ctx))
                       :tx-count (length receipts))))

(defmacro with-state-storage ((db env) &body body)
  "Run BODY with clempo.contract:*STORAGE* bound to a static, unmetered precompile storage
over a fresh world on DB under ENV's fork, so exported precompile readers (2D nonces, fee
tokens, TIP-20 balances, keychain keys) can be called. Writes signal; nothing reaches DB."
  (let ((e (gensym "ENV")))
    `(let* ((,e ,env)
            (forks:*spec* (block-env-spec ,e))
            (evm:*spec* (tempo-evm-spec forks:*spec*))
            (evm:*world* (state:make-world ,db))
            (evm:*block* (evm-block-env ,db ,e))
            (*host* (make-instance 'world-host
                                   :block-info (contract:make-block-info
                                                :number (block-env-number ,e)
                                                :timestamp (block-env-timestamp ,e)
                                                :beneficiary (block-env-beneficiary ,e)
                                                :chain-id (block-env-chain-id ,e))))
            (contract:*storage* (contract:make-metered-storage *host* :static t :tip1060 nil)))
       ,@body)))

(defun validated-outcome (fee)
  "The outcome of a validation-only run: everything up to fee collection passed."
  (make-tx-outcome t 0 0 '() :fee-token (fee-token fee) :fallback-fee-token-p (fee-fallback-p fee)
                             :fee-payer (fee-payer fee) :key-expiry (ctx-key-expiry *ctx*)))

(defun check-transaction (db env tx sender &key skip-nonce-check skip-valid-after-check
                                               skip-base-fee-check skip-liquidity-check execute
                                               overlay)
  "Validate TX on DB as the first transaction after ENV's pre-execution changes: every check
the handler makes through nonce, keychain and fee collection, without running its calls
(with EXECUTE, run it too). Everything is undone. Returns the TX-OUTCOME (gas 0 unless
EXECUTE) or signals INVALID-TRANSACTION. With OVERLAY the block and its pre-execution changes
live in a world over DB and DB is never written (safe for readers sharing DB)."
  (let ((ctx (if overlay
                 (open-call-block (make-context db env :overlay t))
                 (begin-block db env))))
    (setf (ctx-skip-nonce-check ctx) skip-nonce-check
          (ctx-skip-valid-after-check ctx) skip-valid-after-check
          (ctx-skip-base-fee-check ctx) skip-base-fee-check
          (ctx-skip-liquidity-check ctx) skip-liquidity-check
          (ctx-validate-only ctx) (not execute))
    (if overlay
        (apply-transaction ctx tx sender)
        (unwind-protect (apply-transaction ctx tx sender)
          (store:db-revert-block db)))))

;;; Overlay blocks: requests (eth_call, eth_estimateGas, eth_simulateV1, eth_callMany) and trace
;;; replays on a state backend. Pre-execution changes and every write stay in the context's
;;; world; nothing is written to the backend.

(defun open-call-block (ctx)
  (with-block-context (ctx)
    (let ((env (ctx-env ctx)))
      (apply-history-and-beacon-calls ctx :parent-beacon-block-root (block-env-parent-beacon-block-root env))
      (deploy-markers-in-world (ctx-world ctx) env)
      (apply-zone-factory ctx)))
  ctx)

(defun begin-call-block (backend env &key block-hash-fn world)
  "An overlay block context on BACKEND (any clempo.state backend: a state database or a
historical view) for ENV, after the block's pre-execution changes. BLOCK-HASH-FN (number ->
hash octets or NIL) answers BLOCKHASH; a state database answers it itself. WORLD, when given,
is a fresh clempo.state world over BACKEND to hold the block (a caller observing every access
sets its observer first). APPLY-TRANSACTION replays signed transactions on it; EXECUTE-REQUEST
runs unsigned requests."
  (open-call-block (make-context backend env :block-hash-fn block-hash-fn :overlay t :world world)))

(defun call-block-world (ctx)
  "The clempo.state world holding CTX's writes (for state overrides before the first request,
and for reading results)."
  (ctx-world ctx))

(defun finish-call-block (ctx)
  "Run CTX's post-execution system calls into its world: the CurrentCommittee call and, before
T13, the inner Ethereum executor's request calls (APPLY-REQUEST-CALLS)."
  (with-block-context (ctx)
    (apply-committee-call ctx)
    (apply-request-calls ctx))
  ctx)

(defun next-call-block (ctx env &key block-hash-fn)
  "Close CTX's block (FINISH-CALL-BLOCK) and open the next overlay block for ENV on the same
world: eth_simulateV1's block sequence."
  (finish-call-block ctx)
  (open-call-block (make-context (state:world-backend (ctx-world ctx)) env
                                 :world (ctx-world ctx) :block-hash-fn block-hash-fn :overlay t)))

(defun request-txv (request index)
  "The transaction view of an unsigned REQUEST, a plist of TempoTransactionRequest fields:
:from :to :gas :gas-price :max-fee :max-priority :value :nonce :chain-id :type :input
:access-list ((address slot ...) ...) :authorizations (raw EIP-7702 items) and, for Tempo
transactions (type #x76, the default when :calls is given), :calls ((to-or-nil value input) ...)
:nonce-key :fee-token :valid-before :valid-after :tempo-authorizations :key-authorization
:key-id :fee-payer and :signature (the mock signature, whose kind sets the signature gas).
:unique-id and :hash stand in for the envelope-derived identifiers (default 0)."
  (destructuring-bind (&key from to gas gas-price max-fee max-priority value nonce chain-id type
                         input access-list authorizations calls nonce-key fee-token valid-before
                         valid-after tempo-authorizations key-authorization key-id fee-payer
                         signature unique-id hash &allow-other-keys)
      request
    (let* ((type (or type (cond (calls #x76) (gas-price 0) (t 2))))
           (from (or from 0))
           (v (%make-txv :type type :sender from :index index :nonce (or nonce 0)
                         :gas-limit (or gas (error "request without :gas"))
                         :access-list access-list
                         :%unique-id (or unique-id 0) :%hash (or hash 0) :%chain-id chain-id
                         :%key-id key-id :%fee-payer (or fee-payer from))))
      (if (member type '(0 1))
          (setf (txv-gas-price v) (or gas-price 0))
          (setf (txv-max-fee v) (or max-fee 0) (txv-max-priority v) (or max-priority 0)))
      (if (= type #x76)
          (setf (txv-calls v) (or calls (list (list to (or value 0) (or input (make-octets 0)))))
                (txv-nonce-key v) (or nonce-key 0)
                (txv-valid-before v) valid-before
                (txv-valid-after v) valid-after
                (txv-fee-token v) fee-token
                (txv-sponsored-p v) (and fee-payer (/= fee-payer from))
                (txv-authorizations v) tempo-authorizations
                (txv-key-authorization v) key-authorization
                (txv-signature v) (or signature (error "Tempo request without its mock :signature")))
          (setf (txv-to v) to
                (txv-value v) (or value 0)
                (txv-input v) (or input (make-octets 0))
                (txv-authorizations v) authorizations))
      v)))

(defun execute-request (ctx request &key skip-nonce-check skip-base-fee-check skip-gas-caps
                                         skip-fee-charge skip-eip3607)
  "Execute the unsigned REQUEST (see REQUEST-TXV) in the overlay block CTX after the requests
before it; returns its TX-OUTCOME or signals INVALID-TRANSACTION with the world unchanged.
Requests take no gas lane and leave no receipt. The flags drop evm2 features as reth's
prepare_call_env does: SKIP-GAS-CAPS the transaction and block gas caps; SKIP-FEE-CHARGE fee
collection, the fee-token balance check and settlement (the fee token is still resolved);
SKIP-EIP3607 the sender-has-code check."
  (let ((*tx-index* (ctx-index ctx))
        (*ctx* ctx)
        (*non-creditable* nil))            ; per request: RPC threads must not share it
    (setf (ctx-skip-nonce-check ctx) skip-nonce-check
          (ctx-skip-base-fee-check ctx) skip-base-fee-check
          (ctx-skip-gas-caps ctx) skip-gas-caps
          (ctx-skip-fee-charge ctx) skip-fee-charge
          (ctx-skip-eip3607 ctx) skip-eip3607)
    (unwind-protect
         (with-block-context (ctx)
           (let ((outcome (execute-txv ctx (request-txv request *tx-index*))))
             (setf (tx-outcome-storage-changes outcome) (collect-storage-changes evm:*world*))
             (state:finalize-tx evm:*world*)
             (incf (ctx-index ctx))
             outcome))
      (setf (ctx-skip-nonce-check ctx) nil
            (ctx-skip-base-fee-check ctx) nil
            (ctx-skip-gas-caps ctx) nil
            (ctx-skip-fee-charge ctx) nil
            (ctx-skip-eip3607 ctx) nil))))
