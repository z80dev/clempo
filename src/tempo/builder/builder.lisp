(in-package #:clempo.builder)

;;; Tempo's payload builder: tempo crates/payload/builder/src/lib.rs (TempoPayloadBuilder::
;;; build_payload) with the block assembler of crates/evm/src/assemble.rs and reth's
;;; EthBlockAssembler, as of the reference head on every fork. The reference removed the
;;; subblock machinery (tempo #7449, "remove live subblock machinery"): the builder no longer
;;; seals any block with the subblocks metadata system transaction, passes a shared gas limit
;;; of 0 and gives pool transactions the whole block gas limit. Historical pre-T4 blocks that
;;; carry that system transaction are only executed (clempo.executor), never built.

(define-condition payload-build-error (error)
  ((reason :initarg :reason :reader payload-build-error-reason))
  (:report (lambda (c s) (format s "payload build failed: ~A" (payload-build-error-reason c))))
  (:documentation "The builder cannot produce a valid block (PayloadBuilderError)."))

(defun build-fail (fmt &rest args)
  (error 'payload-build-error :reason (apply #'format nil fmt args)))

;;; Constants

(define-constant +max-block-transactions+ 65536
  "tempo crates/evm/src/consensus/mod.rs MAX_BLOCK_TRANSACTIONS.")
(define-constant +system-tx-count+ 1
  "tempo crates/chainspec/src/spec.rs SYSTEM_TX_COUNT: room kept for end-of-block system txs.")
(define-constant +max-rlp-block-size+ 8388608
  "reth consensus/common validation.rs MAX_RLP_BLOCK_SIZE (EIP-7934, Osaka).")
(define-constant +non-transaction-size-estimate+ 2048
  "lib.rs NON_TRANSACTION_SIZE_ESTIMATE: RLP bytes of a block that are not transactions.")
(define-constant +gas-limit-bound-divisor+ 1024 "alloy eip1559 GAS_LIMIT_BOUND_DIVISOR.")
(define-constant +empty-ommers-hash+ (clempo.keccak:keccak256 (rlp:rlp-encode '()))
  "EMPTY_OMMER_ROOT_HASH: keccak256(RLP([])).")
(define-constant +empty-requests-hash+ (clempo.sha2:sha256 (make-octets 0))
  "EIP-7685 requests hash of no requests: sha256(\"\"). Prague is active from genesis on
every Tempo chain clempo.chainspec knows.")

;;; Attributes: tempo crates/payload/types/src/attrs.rs TempoPayloadAttributes.

(defstruct (payload-attributes
            (:constructor make-payload-attributes
                (&key timestamp (timestamp-millis-part 0) (suggested-fee-recipient 0)
                      proposer-public-key consensus-context (extra-data (make-octets 0))
                      (prev-randao (make-octets 32)) (parent-beacon-block-root (make-octets 32))
                      target-gas-limit)))
  "What consensus asks the builder for. TIMESTAMP is in seconds and TIMESTAMP-MILLIS-PART
(0..999) its sub-second milliseconds. SUGGESTED-FEE-RECIPIENT is the beneficiary unless
PROPOSER-PUBLIC-KEY (32 octets) resolves to a non-zero fee recipient in ValidatorConfigV2.
EXTRA-DATA carries the DKG dealer log or the encoded DKG outcome exactly as consensus encodes
them. TARGET-GAS-LIMIT, when given, is the desired gas limit (bounded by the parent's)."
  (timestamp 0 :type (unsigned-byte 64))
  (timestamp-millis-part 0 :type (integer 0 999))
  (suggested-fee-recipient 0 :type (unsigned-byte 160))
  (proposer-public-key nil :type (or null octets))
  (consensus-context nil)
  (extra-data (make-octets 0) :type octets)
  (prev-randao (make-octets 32) :type octets)
  (parent-beacon-block-root (make-octets 32) :type octets)
  (target-gas-limit nil :type (or null (unsigned-byte 64))))

(defun payload-attributes-timestamp-millis (attributes)
  "attrs.rs timestamp_millis: seconds * 1000 + the millisecond part, saturating at u64."
  (min +u64-max+ (+ (min +u64-max+ (* 1000 (payload-attributes-timestamp attributes)))
                    (payload-attributes-timestamp-millis-part attributes))))

;;; The next block's environment

(defun next-block-gas-limit (parent-gas-limit desired-gas-limit)
  "alloy calculate_block_gas_limit: DESIRED-GAS-LIMIT clamped to the parent's gas limit
plus or minus (parent / 1024) - 1."
  (let* ((delta (max 0 (1- (floor parent-gas-limit +gas-limit-bound-divisor+))))
         (low (max 0 (- parent-gas-limit delta)))
         (high (min +u64-max+ (+ parent-gas-limit delta))))
    (min high (max low desired-gas-limit))))

(defstruct (next-block (:conc-name next-))
  number timestamp timestamp-millis-part gas-limit general-gas-limit
  base-fee spec chain-id parent-hash beneficiary env)

(defun plan-next-block (db parent attributes chain desired-gas-limit)
  (let* ((inner (headers:header-inner parent))
         (timestamp (payload-attributes-timestamp attributes))
         (parent-gas-limit (headers:ethereum-header-gas-limit inner))
         (gas-limit (next-block-gas-limit
                     parent-gas-limit
                     (or (payload-attributes-target-gas-limit attributes) desired-gas-limit
                         parent-gas-limit)))
         (spec (chainspec:fork-at timestamp chain))
         ;; lib.rs: the shared (subblock) gas limit is 0 on every fork since #7449.
         (general (chainspec:general-gas-limit-at timestamp chain gas-limit 0))
         (base-fee (chainspec:next-block-base-fee
                    timestamp chain (or (headers:ethereum-header-base-fee-per-gas inner) 0)
                    (headers:ethereum-header-gas-used inner)))
         (context (payload-attributes-consensus-context attributes))
         (plan (make-next-block
                :number (1+ (headers:ethereum-header-number inner))
                :timestamp timestamp
                :timestamp-millis-part (payload-attributes-timestamp-millis-part attributes)
                :gas-limit gas-limit :general-gas-limit general
                :base-fee base-fee :spec spec
                :chain-id (chainspec:chain-id (chainspec:load-chainspec chain))
                :parent-hash (headers:header-hash parent)
                :beneficiary (payload-attributes-suggested-fee-recipient attributes))))
    (when (<= (payload-attributes-timestamp-millis attributes) (headers:header-timestamp-millis parent))
      (build-fail "timestamp ~D ms is not after the parent's ~D ms"
                  (payload-attributes-timestamp-millis attributes)
                  (headers:header-timestamp-millis parent)))
    (flet ((env ()
             (ex:make-block-env :number (next-number plan) :timestamp timestamp
                                :timestamp-millis-part (next-timestamp-millis-part plan)
                                :beneficiary (next-beneficiary plan) :gas-limit gas-limit
                                :base-fee base-fee
                                :prevrandao (payload-attributes-prev-randao attributes)
                                :parent-hash (next-parent-hash plan)
                                :extra-data (payload-attributes-extra-data attributes)
                                :chain-id (next-chain-id plan) :spec spec
                                :general-gas-limit general :shared-gas-limit 0
                                :proposer (when context
                                            (headers:consensus-context-proposer context)))))
      ;; lib.rs maybe_override_fee_recipient: resolved before execution, outside any block.
      (let ((key (payload-attributes-proposer-public-key attributes)))
        (when key
          (let ((recipient (ex:proposer-fee-recipient db (env) key)))
            (when (and recipient (plusp recipient))
              (setf (next-beneficiary plan) recipient)))))
      (setf (next-env plan) (env)))
    plan))

(defun next-block-env (db parent attributes &key (chain ex:*chain*) desired-gas-limit)
  "The executor BLOCK-ENV of the block BUILD-PAYLOAD would build on PARENT (a header) with
ATTRIBUTES: number, millisecond timestamp, gas limit bounded by the parent's, the fork's
general gas limit (shared gas limit 0), the next base fee, and the beneficiary resolved from
ValidatorConfigV2 when the attributes name the proposer. Use it for pool admission."
  (next-env (plan-next-block db parent attributes chain desired-gas-limit)))

;;; Transactions

(defun block-transaction-length (tx)
  "encode.rs block_transaction_length: the bytes TX takes in the block body's list. Typed
transactions are RLP strings around their EIP-2718 envelope; legacy ones are inline lists."
  (let ((size (tx:transaction-encoded-length tx)))
    (+ size (if (zerop (tx:transaction-type tx)) 0 (rlp:header-length size)))))

(defun transaction-fit (gas-limit tx-gas-limit-cap cumulative-gas-used block-gas-limit
                        payment-p non-payment-gas-used general-gas-limit)
  "lib.rs block fill: NIL when a transaction declaring GAS-LIMIT may still be executed, else
why it is skipped. The transaction is charged min(GAS-LIMIT, TX-GAS-LIMIT-CAP) against the
block gas left and, unless it is a payment, against the general (non-payment) gas left."
  (let ((max-regular (min gas-limit tx-gas-limit-cap)))
    (cond ((> (+ cumulative-gas-used max-regular) block-gas-limit) :exceeds-block-gas-limit)
          ((and (not payment-p) (> (+ non-payment-gas-used max-regular) general-gas-limit))
           :exceeds-general-gas-limit)
          (t nil))))

(defun next-excess-blob-gas (parent)
  "Cancun is active from genesis on Tempo chains and Tempo rejects blob transactions, so every
parent carries zero blob gas and the next excess is zero."
  (let ((inner (headers:header-inner parent)))
    (invariant (and (eql 0 (or (headers:ethereum-header-blob-gas-used inner) 0))
                    (eql 0 (or (headers:ethereum-header-excess-blob-gas inner) 0)))
               "a Tempo parent with blob gas")
    0))

;;; Fee balances spent during the block: transaction-pool best.rs StateAwareBestTransactions.
;;; A TIP-20 balance slot is tracked once a transaction of this block lowered it; a pool
;;; transaction whose fee balance slot is tracked and now holds less than its fee cost is
;;; skipped with its lane before execution, unless it pays with a fallback fee token and
;;; another fallback token's balance (untracked, or tracked and sufficient) could pay.
;;; Every later write or read of a tracked slot updates the tracked value in the reference,
;;; so the tracked value is always the slot's value in the open block, read from DB.

(define-constant +tip20-prefix+ #x20c000000000000000000000
  "The 12-byte TIP-20 address prefix (tempo primitives address.rs TIP20_TOKEN_PREFIX).")

(defun tip20-address-p (address)
  (= (ash address -64) +tip20-prefix+))

(define-constant +path-usd-address+ #x20c0000000000000000000000000000000000000
  "tempo hardfork constants.rs PATH_USD_ADDRESS.")
(define-constant +ousd-address+ #x20c0000000000000000000006a37da5c996874be
  "tempo hardfork constants.rs OUSD_ADDRESS.")

(defun fallback-fee-tokens (fork)
  "tempo hardfork lib.rs fallback_fee_tokens: the tokens execution may fall back to when no
transaction field or fee-manager rule selects the fee token, in order."
  (if (forks:fork>= fork :t13)
      (list +path-usd-address+ +ousd-address+)
      (list +path-usd-address+)))

(defun note-fee-balance-changes (decreased outcome)
  "best.rs on_new_result / DecreasedBalancesSink: track TIP-20 slots OUTCOME lowered."
  (loop for (address slot original current) in (ex:tx-outcome-storage-changes outcome)
        when (and (tip20-address-p address) (< current original))
          do (setf (gethash (cons address slot) decreased) t)))

(defun fee-balance-insufficient-p (db decreased pool tx fallback-tokens)
  "best.rs next: true when TX's fee balance slot is tracked, holds less than its fee token
cost, and no other fallback token may pay (may_use_other_fallback_token; TIP-20 balance
slots share a layout, so the payer's slot is the same in every token)."
  (destructuring-bind (token . slot) (pool:pool-transaction-fee-balance-slot pool tx)
    (let ((fee (pool:pool-transaction-fee-cost pool tx)))
      (flet ((short-p (token)
               (and (gethash (cons token slot) decreased)
                    (< (store:db-storage db token slot) fee))))
        (and (short-p token)
             (not (and (pool:pool-transaction-fallback-fee-token-p pool tx)
                       (some (lambda (candidate)
                               (and (/= candidate token) (not (short-p candidate))))
                             fallback-tokens))))))))

;;; The payload

(defstruct built-payload
  "A built block. BLOCK is a CLEMPO.BLOCKS:BLOCK whose header holds every commitment;
SENDERS and RECEIPTS follow its transactions; RESULT is the executor's BLOCK-RESULT; FEES the
validator-credited fees; STOP-REASON why the pool fill stopped (:pool-empty, :gas-limit,
:rlp-block-size-limit or :transaction-count-limit); SKIPPED lists (hash . reason) for every
pool transaction not included, in the order they were considered; ENV the block's
executor environment; ESTIMATED-SIZE the builder's RLP size estimate."
  block (senders '()) (receipts '()) result (fees 0) stop-reason (skipped '()) env
  (estimated-size 0))

(defun assemble-header (plan parent attributes transactions result)
  "assemble.rs TempoBlockAssembler over reth EthBlockAssembler: London..Prague are active from
genesis on Tempo chains, ommers and withdrawals are empty, nonce and difficulty zero, mix hash
the prevrandao, no block access list hash or slot number."
  (headers:make-header
   :general-gas-limit (next-general-gas-limit plan)
   :shared-gas-limit 0
   :timestamp-millis-part (next-timestamp-millis-part plan)
   :inner (headers:make-ethereum-header
           :parent-hash (next-parent-hash plan)
           :ommers-hash +empty-ommers-hash+
           :beneficiary (next-beneficiary plan)
           :state-root (ex:block-result-state-root result)
           :transactions-root (blocks:transactions-root transactions)
           :receipts-root (ex:block-result-receipts-root result)
           :logs-bloom (ex:block-result-bloom result)
           :difficulty 0
           :number (next-number plan)
           :gas-limit (next-gas-limit plan)
           :gas-used (ex:block-result-gas-used result)
           :timestamp (next-timestamp plan)
           :extra-data (payload-attributes-extra-data attributes)
           :mix-hash (payload-attributes-prev-randao attributes)
           :nonce (make-octets 8)
           :base-fee-per-gas (next-base-fee plan)
           :withdrawals-root clempo.trie:+empty-trie-root+
           :blob-gas-used 0
           :excess-blob-gas (next-excess-blob-gas parent)
           :parent-beacon-block-root (payload-attributes-parent-beacon-block-root attributes)
           :requests-hash +empty-requests-hash+)
   :consensus-context (payload-attributes-consensus-context attributes)))

(defun build-payload (db parent attributes
                      &key pool (chain ex:*chain*) desired-gas-limit commit best-fees)
  "Build the block after PARENT (a CLEMPO.HEADERS:HEADER) on DB, whose committed state must be
PARENT's (its root equals PARENT's state root) with no block open. Transactions come from
POOL's best transactions for the block's timestamp and base fee, in order; POOL NIL builds
the empty payload. Each is executed on its own: one that does not fit the remaining block or
general gas, or the Osaka size limit, or that the executor rejects, is skipped (the executor
rolls its writes back) and so is the rest of its nonce lane, except that nonce-too-low and
expiring-nonce replays skip only themselves (lib.rs block fill). No system transaction is
added (the reference head builds none on any fork).
With BEST-FEES, a payload whose fees do not exceed them is abandoned: returns :ABORTED and
the fees. Otherwise returns a BUILT-PAYLOAD; with COMMIT the block is committed to DB under
its number and hash, else DB is left as it was. The pool itself is not changed.
Transactions are drawn from one snapshot of the pool's best list taken when the build
starts; the build ends when that list is exhausted or a block limit is reached, and
consensus decides when to build and how long to wait for transactions. EXTRA-DATA is
included as given (the DKG dealer log or the encoded DKG outcome, as consensus supplies it)."
  (invariant (not (store:db-block-open-p db)) "build-payload with a block open")
  (unless (octets= (store:db-root-hash db) (headers:header-state-root parent))
    (build-fail "state root ~A is not the parent's ~A" (octets->hex (store:db-root-hash db))
                (octets->hex (headers:header-state-root parent))))
  ;; The executor reads the chain (e.g. the epoch length of the boundary block's
  ;; CurrentCommittee call) from EX:*CHAIN*: run the block under CHAIN.
  (let* ((ex:*chain* chain)
         (plan (plan-next-block db parent attributes chain desired-gas-limit))
         (env (next-env plan))
         (spec (next-spec plan))
         (block-gas-limit (next-gas-limit plan))
         (general-limit (next-general-gas-limit plan))
         (cap (forks:tx-gas-limit-cap spec))
         (size (+ (rlp:rlp-encoded-length '())    ; the (empty) withdrawals
                  +non-transaction-size-estimate+
                  (rlp:rlp-encoded-length (payload-attributes-extra-data attributes))))
         (done nil))
    (when (> size +max-rlp-block-size+)
      (build-fail "block of ~D bytes exceeds ~D before any transaction" size +max-rlp-block-size+))
    (forks:with-spec (spec)
      (unwind-protect
           (let ((ctx (ex:begin-block db env))
                 (candidates (when pool
                               (pool:pool-best-transactions
                                pool :timestamp (next-timestamp plan)
                                     :base-fee (next-base-fee plan))))
                 (invalid-lanes (make-hash-table :test 'equal))
                 (decreased (make-hash-table :test 'equal))
                 (fallback-tokens (fallback-fee-tokens spec))
                 (included '()) (senders '()) (skipped '()) (count 0)
                 (cumulative 0) (non-payment 0) (fees 0) (oversized nil) (stop nil))
             (loop
               (when (>= (+ count +system-tx-count+) +max-block-transactions+)
                 (setf stop :transaction-count-limit)
                 (return))
               (when (null candidates)
                 (setf stop (cond ((>= cumulative block-gas-limit) :gas-limit)
                                  (oversized :rlp-block-size-limit)
                                  (t :pool-empty)))
                 (return))
               (let* ((tx (pop candidates))
                      (lane (pool:pool-transaction-lane pool tx)))
                 (flet ((skip (reason &key lane-invalid)
                          (when lane-invalid (setf (gethash lane invalid-lanes) t))
                          (push (cons (tx:transaction-hash tx) reason) skipped)))
                   (cond
                     ((gethash lane invalid-lanes) (skip :invalid-ancestor))
                     ((fee-balance-insufficient-p db decreased pool tx fallback-tokens)
                      (skip :insufficient-fee-balance :lane-invalid t))
                     (t
                       (let* ((payment (ex:payment-transaction-p tx spec))
                              (misfit (transaction-fit (tx:transaction-gas-limit tx) cap cumulative
                                                       block-gas-limit payment non-payment
                                                       general-limit))
                              (tx-length (block-transaction-length tx)))
                         (cond
                           (misfit (skip misfit :lane-invalid t))
                           ((> (+ size tx-length) +max-rlp-block-size+)
                            (setf oversized t)
                            (skip :oversized-block :lane-invalid t))
                           (t
                            (let* ((sender (pool:pool-transaction-sender pool tx))
                                   (outcome
                                     (handler-case (ex:apply-transaction ctx tx sender)
                                       (ex:invalid-transaction (c)
                                         (let ((kind (ex:invalid-transaction-kind c)))
                                           (case kind
                                             ;; Block validation, not an invalid transaction:
                                             ;; the reference build fails (lib.rs `_ => Err`).
                                             (:gas-lane
                                              (build-fail "block validation rejected ~A: ~A"
                                                          (octets->hex (tx:transaction-hash tx)) c))
                                             ((:nonce-too-low :expiring-nonce-replay) (skip kind))
                                             (t (skip kind :lane-invalid t))))
                                         nil))))
                              (when outcome
                                (let ((gas (ex:tx-outcome-block-gas outcome)))
                                  (incf cumulative gas)
                                  (unless payment (incf non-payment gas)))
                                (incf fees (ex:tx-outcome-validator-fee outcome))
                                (note-fee-balance-changes decreased outcome)
                                (incf size tx-length)
                                (incf count)
                                (push tx included)
                                (push sender senders)))))))))))
             (if (and best-fees (<= fees best-fees))
                 (progn (store:db-revert-block db)
                        (setf done t)
                        (values :aborted fees))
                 (progn
                   (let* ((result (ex:finish-block ctx))
                          (transactions (nreverse included))
                          (header (assemble-header plan parent attributes transactions result))
                          (block (blocks:make-block :header header :transactions transactions
                                                    :withdrawals '() :withdrawals-present-p t)))
                     (when (> size +max-rlp-block-size+)
                       (build-fail "block of ~D bytes exceeds ~D" size +max-rlp-block-size+))
                     (if commit
                         (store:db-commit-block db (next-number plan) (headers:header-hash header))
                         (store:db-revert-block db))
                     (setf done t)
                     (make-built-payload :block block :senders (nreverse senders)
                                         :receipts (ex:block-result-receipts result)
                                         :result result :fees fees :stop-reason stop
                                         :skipped (nreverse skipped) :env env
                                         :estimated-size size)))))
        (unless done
          (when (store:db-block-open-p db) (store:db-revert-block db)))))))
