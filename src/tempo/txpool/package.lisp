(defpackage #:clempo.txpool
  (:use #:cl #:clempo.base)
  (:documentation "Tempo's transaction pool. Admission is read-only against an explicit state
snapshot; successful validation never executes calls or commits fee/nonce/key writes.
Protocol and 2D nonce lanes share replacement and ordering rules. Expiring nonces are
independent, sender-scoped signing-payload identities. Call POOL-REVALIDATE after
canonical state changes; selection alone cannot detect revocations or spent balances.")
  (:export #:pool #:make-pool #:pool-count #:submit-transaction
           #:pool-best-transactions #:pool-remove-transaction #:pool-snapshot #:pool-clear #:pool-revalidate
           #:pool-sweep-expired #:pool-transaction-sender #:pool-transaction-lane
           #:pool-transaction-fee-balance-slot #:pool-transaction-fee-cost
           #:pool-transaction-fallback-fee-token-p
           #:pool-error #:pool-error-code #:pool-error-detail
           #:state-reader #:make-state-reader #:state-reader-db #:state-reader-env
           #:state-reader-nonce #:state-reader-check #:state-reader-validator-tokens
           #:state-reader-liquidity-p #:validator-fee-token))

(in-package #:clempo.txpool)

(define-condition pool-error (error)
  ((code :initarg :code :reader pool-error-code)
   (detail :initarg :detail :initform nil :reader pool-error-detail))
  (:report (lambda (condition stream)
             (format stream "Transaction pool ~A~@[: ~A~]"
                     (pool-error-code condition) (pool-error-detail condition)))))

(defun reject (code &optional detail)
  (error 'pool-error :code code :detail detail))

(defclass state-reader () ()
  (:documentation "Required typed snapshot protocol. Implement ENV, NONCE, CHECK and
LIQUIDITY-P; there are deliberately no default methods accepting unavailable state.
CHECK must perform the executor pipeline with only future nonce/valid-after and current
base-fee/liquidity readiness relaxed; LIQUIDITY-P checks the recent-validator routes.
Never relax balance, signature, replay or keychain checks. Roll back every temporary
write. Backend/database failures must propagate,
not be converted into invalid-transaction or a successful validation."))

(defgeneric state-reader-env (reader)
  (:documentation "Return the clempo.executor:block-env of this immutable snapshot."))
(defgeneric state-reader-nonce (reader sender nonce-key)
  (:documentation "Return the actual u64 protocol (key 0) or NonceManager lane nonce."))
(defgeneric state-reader-check (reader transaction sender)
  (:documentation "Return a validated executor TX-OUTCOME or signal INVALID-TRANSACTION.
Validate future lane nonces and future valid-after without executing transaction calls."))
(defgeneric state-reader-validator-tokens (reader)
  (:documentation "Actual fee tokens observed in the last ten canonical heads, plus
new token preferences from validators active in that window. Never inferred from balances."))
(defgeneric state-reader-liquidity-p (reader token fee)
  (:documentation "Whether TOKEN can pay FEE against any recent validator token.
Implement the direct route and T5+ quote-token two-hop fallback over actual state."))

(defclass database-state-reader (state-reader)
  ((db :initarg :db :reader state-reader-db)
   (env :initarg :env :reader state-reader-env)
   (tokens :initarg :tokens :reader state-reader-validator-tokens)))

(defconstant +fee-manager-address+ #xfeec000000000000000000000000000000000000)

(defun state-mapping-slot (key slot)
  (octets->integer (clempo.keccak:keccak256
                   (octets-concat (integer->octets key 32) (integer->octets slot 32)))))

(defun validator-fee-token (db env beneficiary)
  "Resolve a validator preference from FeeManager slot 0, with the protocol PATH_USD
fallback. Source: crates/precompiles/src/tip_fee_manager/mod.rs::get_validator_token."
  (check-type db clempo.store:statedb)
  (check-type env clempo.executor:block-env)
  (let ((token (ldb (byte 160 0)
                    (clempo.store:db-storage db +fee-manager-address+
                                            (state-mapping-slot beneficiary 0)))))
    (if (zerop token) #x20c0000000000000000000000000000000000000 token)))

(defun make-state-reader (db env &key (recent-validator-tokens nil history-supplied-p))
  "Wrap real state and its block-env. Keep the snapshot stable during validation.
RECENT-VALIDATOR-TOKENS must contain actual fee tokens observed in the last ten
canonical heads and updated preferences of those active validators. Non-genesis
state requires explicit history: latest state alone cannot reconstruct old preferences.
Only a genesis database may derive the sole beneficiary's token from current state."
  (check-type db clempo.store:statedb)
  (check-type env clempo.executor:block-env)
  (unless history-supplied-p
    (unless (and (<= (clempo.store:db-number db) 0)
                 (<= (clempo.executor:block-env-number env) 1))
      (reject :missing-validator-history))
    (setf recent-validator-tokens
          (list (validator-fee-token db env (clempo.executor:block-env-beneficiary env)))))
  (unless recent-validator-tokens (reject :missing-validator-history))
  (dolist (token recent-validator-tokens) (check-type token (unsigned-byte 160)))
  (make-instance 'database-state-reader :db db :env env
                 :tokens (remove-duplicates (copy-list recent-validator-tokens) :test #'=)))

(defmethod state-reader-nonce ((reader database-state-reader) sender nonce-key)
  (if (zerop nonce-key)
      (let ((account (clempo.store:db-account (state-reader-db reader) sender)))
        (if account (clempo.store:account-nonce account) 0))
      ;; NonceManager::nonces[account][key], mapping slot 0 (Rust nonce/mod.rs).
      (ldb (byte 64 0)
           (clempo.store:db-storage
            (state-reader-db reader) #x4e4f4e4345000000000000000000000000000000
            (state-mapping-slot nonce-key (state-mapping-slot sender 0))))))

(defmethod state-reader-check ((reader database-state-reader) tx sender)
  ;; OVERLAY: admission runs on RPC threads that share DB with other readers.
  (clempo.executor:check-transaction (state-reader-db reader) (state-reader-env reader)
                                    tx sender :skip-nonce-check t
                                    :skip-valid-after-check t :skip-base-fee-check t
                                    :skip-liquidity-check t :overlay t))

(defstruct (entry (:constructor make-entry))
  tx hash sender lane nonce state-nonce replay-hash authorities
  key-expiry fee-balance-slot fee-cost fallback-fee-token-p
  fee-cap priority-fee valid-before valid-after
  (id 0 :type integer))

(defstruct (pool (:constructor %make-pool))
  (entries (make-hash-table) :type hash-table) ; tx hash integer -> entry
  (transactions (make-hash-table :test 'eq) :type hash-table) ; admitted object -> entry
  (lanes (make-hash-table :test 'equal) :type hash-table) ; (sender . key) -> sorted entries
  (replays (make-hash-table) :type hash-table) ; expiring identity -> entry
  (authority-counts (make-hash-table) :type hash-table)
  (next-id 0 :type integer)
  (timestamp 0 :type integer)
  (base-fee 0 :type integer)
  (minimum-fee-cap clempo.forks:+t7-base-fee-floor+ :type integer)
  (max-transactions-per-sender 64 :type integer)
  (max-transactions-per-lane 1024 :type integer)
  (price-bump 10 :type integer)
  (valid-after-max-seconds 120 :type integer)
  (max-transaction-bytes 131072 :type integer)
  (max-authorizations 16 :type integer))

(defun make-pool (&key (minimum-fee-cap clempo.forks:+t7-base-fee-floor+)
                       (max-transactions-per-sender 64) (max-transactions-per-lane 1024)
                       (price-bump 10) (valid-after-max-seconds 120)
                       (max-transaction-bytes 131072) (max-authorizations 16))
  "Make an empty pool. The sender limit counts each distinct authority (sender,
separate sponsor, access key, inline key authorizer), not just nonce-lane senders.
PRICE-BUMP is a percentage required on BOTH fee cap and priority fee. Zero-fee
chains explicitly set MINIMUM-FEE-CAP to zero; this disables no other validation."
  (dolist (n (list minimum-fee-cap price-bump valid-after-max-seconds max-authorizations))
    (check-type n (integer 0 *)))
  (dolist (n (list max-transactions-per-sender max-transactions-per-lane max-transaction-bytes))
    (check-type n (integer 1 *)))
  (%make-pool :minimum-fee-cap minimum-fee-cap
              :max-transactions-per-sender max-transactions-per-sender
              :max-transactions-per-lane max-transactions-per-lane :price-bump price-bump
              :valid-after-max-seconds valid-after-max-seconds
              :max-transaction-bytes max-transaction-bytes :max-authorizations max-authorizations))

(defun pool-count (pool) (hash-table-count (pool-entries pool)))

(defun hash-key (hash)
  (if (integerp hash) hash (octets->integer hash)))

(defun transaction-entry (pool tx)
  (or (gethash tx (pool-transactions pool))
      (gethash (hash-key (clempo.transactions:transaction-hash tx)) (pool-entries pool))
      (reject :not-in-pool)))

(defun pool-transaction-sender (pool tx)
  "Return the recovered sender cached at admission. TX must still be in POOL."
  (entry-sender (transaction-entry pool tx)))

(defun pool-transaction-lane (pool tx)
  "Return an EQUAL-comparable nonce dependency identity. Expiring txs each have a
unique identity, so rejecting one never suppresses another expiring tx. Do not mutate it."
  (entry-lane (transaction-entry pool tx)))

(defun pool-transaction-fee-balance-slot (pool tx)
  "Return the cached (resolved-token . TIP20-balances[payer]-slot). Do not mutate it."
  (entry-fee-balance-slot (transaction-entry pool tx)))

(defun pool-transaction-fee-cost (pool tx)
  "Return the maximum gas cost in fee-token units, ceil(gas-limit*fee-cap/10^12)."
  (entry-fee-cost (transaction-entry pool tx)))

(defun pool-transaction-fallback-fee-token-p (pool tx)
  "True only if execution validation selected from the ordered fallback-token list."
  (entry-fallback-fee-token-p (transaction-entry pool tx)))
