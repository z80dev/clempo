;;; NOT YET IN clempo-tempo-executor.asd: needs clempo.state / clempo.evm (EvmAgent) pushed and
;;; the fee manager, TIP-20, keychain and channel-reserve entry points (PrecompilesAgent).
;;; Precompile entry points are resolved at run time (PROTOCOL-FN) so a missing one stops a
;;; block in UNSUPPORTED-TRANSACTION instead of breaking the build; replace with direct calls
;;; once they are exported.
(in-package #:clempo.executor)

;;; The transaction pipeline: tempo crates/evm/src/handler.rs (0x76) and handler/config.rs
;;; (legacy, 2930, 1559, 7702 with TempoHandlerHooks) over evm2's ethereum handler.

(define-constant +gas-price-scaling+ (expt 10 12) "TEMPO_GAS_PRICE_SCALING_FACTOR")
(define-constant +expiring-nonce-key+ (1- (ash 1 256)) "TEMPO_EXPIRING_NONCE_KEY")
(define-constant +storage-credits-address+ #x1060000000000000000000000000000000000000)
(define-constant +fee-manager-address+ #xfeec000000000000000000000000000000000000)
(define-constant +stablecoin-dex-address+ #xdec0000000000000000000000000000000000000)
(define-constant +system-call-gas-limit+ 250000000)
(define-constant +max-u64+ (1- (ash 1 64)))

(defstruct (block-context (:conc-name ctx-))
  db env block-env-evm world host
  (section :start)                      ; :start :non-shared :system
  (non-shared-left 0) (non-payment-left 0)
  (cumulative 0) (block-gas 0) (receipts '()) (index 0)
  skip-nonce-check skip-valid-after-check skip-base-fee-check skip-liquidity-check)

(defvar *tx-index* nil "Index of the transaction being executed, for conditions.")

(defun calc-gas-balance-spending (gas price)
  "Fee in TIP-20 base units: ceil(gas * price / 10^12)."
  (ceiling (* gas price) +gas-price-scaling+))

;;; Precompile entry points, by name until PrecompilesAgent exports them.

(defun protocol-fn (package name)
  (let* ((p (find-package package))
         (s (and p (find-symbol name p))))
    (if (and s (fboundp s))
        (symbol-function s)
        (fail-tx 'unsupported-transaction *tx-index* "precompile entry point ~A::~A is not available"
                 package name))))

(defmacro pcall (package name &rest args)
  `(funcall (protocol-fn ,package ,name) ,@args))


(defmacro with-protocol-storage ((&key (gas-limit '+max-u64+) static) &body body)
  "StorageCtx::enter_evm_without_tip1060_accounting: precompile code run by the handler, metered
on its own gas (unlimited unless GAS-LIMIT) without TIP-1060 accounting. Returns BODY's values
and the storage backend as an extra last value is not needed; use WITH-PROTOCOL-STORAGE* for gas."
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
  key-authorization signature)

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

(defun fee-inference-call-p (input)
  "common.rs is_tip20_fee_inference_call: transfer, transferWithMemo; distributeReward before T7."
  (and (>= (length input) 4)
       (let ((s (octets->integer input :end 4)))
         (or (= s #xa9059cbb) (= s #x95777d59)
             (and (forks:spec< :t7) (= s #x3b6e750f))))))

(defun valid-fee-token-p (token)
  (and (tip20-address-p token)
       (with-protocol-storage (:static t)
         (let ((currency (pcall "CLEMPO.TIP20" "CURRENCY" token)))
           (string= currency "USD")))))

(defun resolve-fee-token (v fee-payer max-fee)
  "fee_manager.rs resolve_fee_token -> (values token fallback-p)."
  (let ((aa (txv-aa-p v)) (sender (txv-sender v)))
    (when (txv-fee-token v) (return-from resolve-fee-token (values (txv-fee-token v) nil)))
    (multiple-value-bind (kind input) (txv-first-call v)
      (when (and (not aa) (= fee-payer sender) (eql kind +fee-manager-address+)
                 (>= (length input) 36) (= (octets->integer input :end 4) #xe7898ae0)) ; setUserToken(address)
        (return-from resolve-fee-token (values (octets->integer input :start 16 :end 36) nil)))
      (let ((user (with-protocol-storage (:static t) (pcall "CLEMPO.FEE-MANAGER" "USER-TOKEN" fee-payer))))
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
          (when (and (member selector '(#xf8856c0f #x9a35a7a5)) (valid-fee-token-p token-in))
            (return-from resolve-fee-token (values token-in nil)))))
      (if (or (forks:spec< :t13) (zerop max-fee))
          (values pc:+default-fee-token+ t)
          (let ((richest pc:+default-fee-token+) (highest 0))
            (dolist (token (pc:fallback-fee-tokens forks:*spec*)
                           (invalid "insufficient fee token balance: ~D < ~D in ~X" highest max-fee richest))
              (let ((balance (with-protocol-storage (:static t)
                               (pcall "CLEMPO.TIP20" "BALANCE-OF" token fee-payer))))
                (when (>= balance max-fee) (return (values token t)))
                (when (> balance highest) (setf richest token highest balance)))))))))

(defun resolve-fee-context (ctx v fee-payer)
  (let* ((env (ctx-env ctx))
         (price (txv-effective-gas-price v (block-env-base-fee env)))
         (collected (calc-gas-balance-spending (txv-gas-limit v) price))
         (max-fee (calc-gas-balance-spending (txv-gas-limit v) (txv-max-fee-per-gas v))))
    (setf *non-creditable* nil)
    (with-protocol-storage ()
      (pcall "CLEMPO.KEYCHAIN" "SET-TX-ORIGIN" (txv-sender v))
      (when (forks:spec>= :t14)
        (fail-tx 'unsupported-transaction *tx-index* "T14 upgrade-authority seeding"))
      (pcall "CLEMPO.CHANNEL-RESERVE" "SET-CHANNEL-OPEN-CONTEXT-HASH"
             (octets->integer (tx:transaction-unique-identifier (txv-tx v) (txv-sender v)))))
    (multiple-value-bind (token fallback-p) (resolve-fee-token v fee-payer max-fee)
      (unless (tip20-address-p token) (invalid "fee token ~X is not TIP-20" token))
      (unless (zerop max-fee)
        (unless (valid-fee-token-p token) (invalid "fee token ~X is not USD" token)))
      (let ((balance (with-protocol-storage (:static t) (pcall "CLEMPO.TIP20" "BALANCE-OF" token fee-payer))))
        (when (< balance max-fee)
          (invalid "insufficient fee token balance: ~D < ~D" balance max-fee)))
      (make-fee-context :payer fee-payer :token token :collected collected :max-fee max-fee
                        :fallback-p fallback-p))))

(defun collect-fee (ctx fee key-id)
  "config.rs collect_fee: pull the maximum fee before execution; roll back on failure."
  (declare (ignore key-id))
  (unless (zerop (fee-collected fee))
    (let ((cp (state:checkpoint evm:*world*)))
      (handler-case
          (with-protocol-storage ()
            (pcall "CLEMPO.FEE-MANAGER" "COLLECT-FEE-PRE-TX" (fee-payer fee) (fee-token fee)
                   (fee-collected fee) (block-env-beneficiary (ctx-env ctx)) (ctx-skip-liquidity-check ctx)))
        (contract:precompile-condition (c)
          (state:rollback evm:*world* cp)
          (invalid "fee collection failed: ~A" c)))))
  (when (and (forks:spec>= :t7) (not (zerop (fee-collected fee))))
    (fail-tx 'unsupported-transaction *tx-index* "T7 non-creditable fee slots")))

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
                            &key fee-token fallback-fee-token-p fee-payer key-expiry (validator-fee 0))))
  "One executed transaction: receipt status, gas, logs, and the fee metadata the pool and
builder read (Rust TempoTxResult / ResolvedFeeToken / key_expiry)."
  status gas-used block-gas (logs '() :type list)
  fee-token fallback-fee-token-p fee-payer key-expiry (validator-fee 0))

(defun settle (ctx v fee price gas-left extra-refund floor &key (status t))
  "config.rs settle_transaction + evm2 finalize_gas. Returns the gas used."
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
    (unless (and (zerop collected) (not (zerop actual)))
      (unless (and (zerop actual) (zerop (- collected actual)))
        (setf validator-fee
              (with-protocol-storage ()
                (pcall "CLEMPO.FEE-MANAGER" "COLLECT-FEE-POST-TX" (fee-payer fee) actual (- collected actual)
                       (fee-token fee) (block-env-beneficiary (ctx-env ctx)))))))
    (values used validator-fee)))

;;; The top-level frame (evm2 prepare_initial_frame + execute_initial_frame).

(defun run-initial-frame (caller nonce to input value gas)
  "Run a top-level call or creation. Returns (values status gas-left output)."
  (let ((world evm:*world*))
    (if (null to)
        (let ((address (evm:create-address caller nonce)))
          (state:warm-address world address)
          (if (evm:collision-p world (state:get-account world address))
              (values :create-collision 0 (make-octets 0))
              (evm:create-message caller address value input gas 0)))
        (let* ((code (state:get-code world to))
               (target (state:delegation-target code)))
          (when target
            (state:warm-address world target)
            (setf code (state:get-code world target)))
          (evm:call-message :call caller to (or target to) value input gas 0 nil
                            :code code :disable-precompiles (and target t))))))

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
    (let ((chain-id (tx:transaction-chain-id (txv-tx v))))
      (when (and chain-id (/= chain-id (block-env-chain-id env))) (invalid "wrong chain id")))
    (when (> gas-limit (evm:gas-params-tx-gas-cap gp)) (invalid "gas limit above the transaction cap"))
    (when (> gas-limit (block-env-gas-limit env)) (invalid "gas limit above the block's"))
    (when (and (null (txv-to v)) (> (length (txv-input v)) (evm:gas-params-max-initcode-size gp)))
      (invalid "initcode too large"))
    (when (= (txv-nonce v) +max-u64+) (invalid "nonce overflow"))
    (multiple-value-bind (intrinsic floor) (ethereum-intrinsic-gas gp v)
      ;; adjust_intrinsic_gas (T1+): new accounts for a zero nonce and zero-nonce authorizations
      (when (forks:spec>= :t1)
        (incf intrinsic (* (evm:gas-params-new-account gp)
                           (+ (if (zerop (txv-nonce v)) 1 0)
                              (count-if (lambda (a) (zerop (rlp:rlp-decode-integer (third a))))
                                        (txv-authorizations v))))))
      (when (< gas-limit intrinsic) (invalid "intrinsic gas too low"))
      (when (< gas-limit floor) (invalid "gas limit below the calldata floor"))
      ;; validate_sender_and_bump_nonce
      (let* ((account (state:get-account world sender))
             (code (state:account-code world account)))
        (when (and (plusp (length code)) (not (state:delegation-target code)))
          (invalid "sender has code"))
        (when (and (not (ctx-skip-nonce-check ctx)) (/= (state:account-nonce account) (txv-nonce v)))
          (error 'invalid-transaction :tx-index *tx-index*
                                      :reason (if (< (txv-nonce v) (state:account-nonce account))
                                                  "nonce too low" "nonce too high")))
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
            (fail-tx 'unsupported-transaction *tx-index* "EIP-7702 authorization lists"))
          ;; before_execution: fee context and collection
          (let ((fee (resolve-fee-context ctx v (txv-sender v))))
            (collect-fee ctx fee nil)
            (let ((evm:*tx-env* (evm:make-tx-env :origin sender :gas-price price)))
              (multiple-value-bind (status gas-left)
                  (run-initial-frame sender sender-nonce (txv-to v) (txv-input v) 0 (- gas-limit intrinsic))
                (let ((ok (evm:success-p status)))
                  (multiple-value-bind (used validator-fee) (settle ctx v fee price gas-left 0 floor :status ok)
                    (make-tx-outcome ok used used (tx-logs) :fee-token (fee-token fee) :fee-payer (fee-payer fee)
                                                             :fallback-fee-token-p (fee-fallback-p fee)
                                                             :validator-fee validator-fee)))))))))))

;;; The block context: the incremental API execute-block is built on.


(defmacro with-block-context ((ctx) &body body)
  "Bind the EVM's and the precompiles' context to CTX's block."
  (let ((c (gensym "CTX")))
    `(let* ((,c ,ctx)
            (forks:*spec* (block-env-spec (ctx-env ,c)))
            (evm:*spec* (tempo-evm-spec forks:*spec*))
            (evm:*world* (ctx-world ,c))
            (evm:*block* (ctx-block-env-evm ,c))
            (*host* (ctx-host ,c)))
       ,@body)))

(defun begin-block (db env &key block)
  "Open a block on DB for ENV and apply the pre-execution system changes; returns the context
APPLY-TRANSACTION and FINISH-BLOCK take."
  (let ((ctx (make-block-context
              :db db :env env :block-env-evm (evm-block-env db env)
              :world (state:make-world db)
              :host (make-instance 'world-host
                                   :block-info (contract:make-block-info
                                                :number (block-env-number env)
                                                :timestamp (block-env-timestamp env)
                                                :beneficiary (block-env-beneficiary env)
                                                :chain-id (block-env-chain-id env)))
              :non-shared-left (- (block-env-gas-limit env) (block-env-shared-gas-limit env))
              :non-payment-left (block-env-general-gas-limit env))))
    (store:db-begin-block db)
    (with-block-context (ctx)
      (when (and block (blocks:block-withdrawals block)) (invalid "withdrawals are not permitted"))
      (apply-pre-execution-changes db env block)
      (when (forks:spec>= :t13)
        (fail-tx 'unsupported-transaction nil "T13 EIP-2935 system call")))
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
         (system-p (system-transaction-p tx)))
    (with-block-context (ctx)
      (let ((outcome
              (handler-bind ((execution-failure (lambda (c) (declare (ignore c)) (state:discard-tx evm:*world*))))
                (if system-p
                    (execute-system-transaction (ctx-db ctx) (ctx-env ctx) tx *tx-index*)
                    (let ((v (make-txv tx sender *tx-index*)))
                      (when (and (txv-aa-p v) (= (ldb (byte 8 248) (txv-nonce-key v)) #x5b))
                        (invalid "subblock transactions are not supported"))
                      (if (txv-aa-p v)
                          (fail-tx 'unsupported-transaction *tx-index* "0x76 handler not wired yet")
                          (execute-ethereum-transaction ctx v)))))))
        (let ((section (next-section ctx system-p nil (tx-outcome-block-gas outcome))))
          (state:finalize-tx evm:*world*)
          (setf (ctx-section ctx) section)
          (when (eq section :non-shared)
            (decf (ctx-non-shared-left ctx) (tx-outcome-block-gas outcome))))
        (incf (ctx-cumulative ctx) (tx-outcome-gas-used outcome))
        (incf (ctx-block-gas ctx) (tx-outcome-block-gas outcome))
        (incf (ctx-index ctx))
        (push (receipts:make-receipt :type (tx:transaction-type tx)
                                     :status (if (tx-outcome-status outcome) 1 0)
                                     :cumulative-gas-used (ctx-cumulative ctx)
                                     :logs (tx-outcome-logs outcome))
              (ctx-receipts ctx))
        outcome))))

(defun finish-block (ctx)
  "Run the post-execution system calls, write the block's changes into the state database and
return the BLOCK-RESULT (the db block stays open for the caller to commit or revert)."
  (with-block-context (ctx)
    (when (and (forks:spec>= :t8)
               (zerop (mod (1+ (block-env-number (ctx-env ctx)))
                           (chainspec:chain-epoch-length (chainspec:load-chainspec *chain*)))))
      (fail-tx 'unsupported-transaction nil "T8 current-committee system call"))
    (write-changes (ctx-db ctx) (state:drain-changes (ctx-world ctx))))
  (let ((receipts (reverse (ctx-receipts ctx))))
    (make-block-result :number (block-env-number (ctx-env ctx))
                       :gas-used (ctx-block-gas ctx)
                       :receipts receipts
                       :receipts-root (receipts:receipts-root receipts)
                       :bloom (receipts:receipts-bloom receipts)
                       :state-root (store:db-state-root (ctx-db ctx))
                       :tx-count (length receipts))))

(defun check-transaction (db env tx sender &key skip-nonce-check skip-valid-after-check
                                               skip-base-fee-check skip-liquidity-check)
  "Validate and execute TX on DB as the first transaction after ENV's pre-execution changes,
then undo everything. Returns the TX-OUTCOME or signals INVALID-TRANSACTION."
  (let ((ctx (begin-block db env)))
    (setf (ctx-skip-nonce-check ctx) skip-nonce-check
          (ctx-skip-valid-after-check ctx) skip-valid-after-check
          (ctx-skip-base-fee-check ctx) skip-base-fee-check
          (ctx-skip-liquidity-check ctx) skip-liquidity-check)
    (unwind-protect (apply-transaction ctx tx sender)
      (store:db-revert-block db))))
