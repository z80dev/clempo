(defpackage #:clempo.executor.mainnet-tests
  (:use #:cl #:clempo.base #:clempo.executor)
  (:local-nicknames (#:store #:clempo.store) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers) (#:json #:clempo.json)))

(in-package #:clempo.executor.mainnet-tests)

;;; Regression blocks from the R3 climb. Each is a mainnet block executed on a state database
;;; holding only what the block reads: the merged per-transaction prestate (prestateTracer,
;;; non-diff) recorded from the reference node (data/README.md). The state root cannot match a
;;; partial state, so the checks are the block's gas used, receipts root and logs bloom.

(defparameter *data*
  (asdf:system-relative-pathname "clempo-tempo-executor" "../../../tests/tempo/executor/data/"))

(defun data-file (fmt &rest args)
  (merge-pathnames (apply #'format nil fmt args) *data*))

(defun fixture-block (n)
  (blocks:decode-block
   (hex->octets (string-trim '(#\Newline #\Space) (clempo.io:read-file-string (data-file "block-~D.hex" n))))))

(defun quantity (x) (etypecase x (integer x) (string (hex->integer x)) (null 0)))

(defun prestate-db (n block)
  "A state database at block N-1 holding the first-seen state of every account the block's
transactions touched."
  (let* ((traces (json:parse-json (clempo.io:read-file-string (data-file "prestate-~D.json" n))))
         (accounts (make-hash-table :test 'equal))
         (db (store:make-statedb)))
    (loop for trace across traces
          for result = (json:jget trace "result")
          do (loop for (address . state) in (cdr result)
                   do (let ((seen (gethash address accounts)))
                        (if seen
                            ;; a slot first read by a later transaction still has its block-start value
                            (loop for (slot . value) in (cdr (json:jget state "storage"))
                                  unless (assoc slot (getf seen :storage) :test #'string=)
                                    do (push (cons slot value) (getf seen :storage)))
                            (setf (gethash address accounts)
                                  (list :state state :storage (copy-list (cdr (json:jget state "storage")))))))))
    (store:db-begin-block db)
    (maphash (lambda (address entry)
               (let* ((state (getf entry :state))
                      (a (hex->integer address))
                      (code (hex->octets (or (json:jget state "code") "0x"))))
                 (store:db-set-account db a (quantity (json:jget state "nonce")) (quantity (json:jget state "balance"))
                                       (if (zerop (length code)) clempo.trie:+empty-code-hash+ (store:db-put-code db code)))
                 (loop for (slot . value) in (getf entry :storage)
                       do (store:db-set-storage db a (hex->integer slot) (hex->integer value)))))
             accounts)
    (store:db-commit-block db (1- n) (headers:ethereum-header-parent-hash
                                      (headers:header-inner (blocks:block-header block))))
    db))

(defun check-block (n)
  (let* ((block (fixture-block n))
         (db (prestate-db n block))
         (result (execute-block db block :chain :mainnet))
         (fields (remove :state-root (block-mismatches result (blocks:block-header block)) :key #'first)))
    (is (null fields) "block ~D: ~S" n fields)))

(defmacro defblock-test (name n doc)
  `(deftest ,name () ,doc (check-block ,n)))

(defblock-test fee-token-from-set-user-token 3850
  "setUserToken on the fee manager picks the fee token (selector computed from its signature).")
(defblock-test key-authorization-pays-full-signature-gas 3865
  "A key authorization pays the secp256k1 verification the base 21,000 already covers for the tx.")
(defblock-test t1-sstore-set-includes-the-warm-read 4516256
  "T1 SSTORE zero-to-nonzero costs 250,100 (evm2 splits off the 100-gas warm read).")
(defblock-test t1-metered-key-authorization 4699557
  "T1/T1A key authorizations are metered with only the SSTORE-set and warm-read costs.")
(defblock-test t1-create2-costs-500000 4711932
  "CREATE2 pays the T1 create cost (TIP-1000), not 32,000.")
(defblock-test t1b-key-authorization-uses-sstore-without-load 6305645
  "T1B key-authorization intrinsic gas uses SstoreSetWithoutLoadCost (+100 per slot otherwise).")

;;; Overlay blocks: the same block replayed and requested without touching the database.

(deftest overlay-replay-matches-and-writes-nothing ()
  (let* ((n 4711932)
         (block (fixture-block n))
         (db (prestate-db n block))
         (root (clempo.store:db-root-hash db))
         (env (header-block-env (blocks:block-header block) :chain :mainnet))
         (ctx (begin-call-block db env))
         (txs (blocks:block-transactions block))
         (outcomes (loop for tx in txs
                         collect (apply-transaction ctx tx (clempo.transactions:transaction-sender tx)))))
    (is (= (reduce #'+ outcomes :key #'tx-outcome-block-gas)
           (headers:ethereum-header-gas-used (headers:header-inner (blocks:block-header block)))))
    (is (not (store:db-block-open-p db)))
    (is (equalp root (clempo.store:db-root-hash db)))
    ;; The block's first transaction again, as an unsigned request on a fresh overlay.
    (let* ((tx (first txs))
           (request (list :from (clempo.transactions:transaction-sender tx)
                          :to (clempo.transactions:transaction-to tx)
                          :gas (clempo.transactions:transaction-gas-limit tx)
                          :max-fee (clempo.transactions:transaction-max-fee-per-gas tx)
                          :max-priority (clempo.transactions:transaction-max-priority-fee-per-gas tx)
                          :value (clempo.transactions:transaction-value tx)
                          :nonce (clempo.transactions:transaction-nonce tx)
                          :chain-id (clempo.transactions:transaction-chain-id tx)
                          :type 2
                          :input (clempo.transactions:transaction-input tx)
                          :access-list (clempo.executor::parse-access-list
                                        (clempo.transactions:transaction-access-list tx))))
           (outcome (execute-request (begin-call-block db env) request)))
      (is (= (tx-outcome-gas-used outcome) (tx-outcome-gas-used (first outcomes))))
      (is (null (tx-outcome-halt outcome)))
      ;; one gas short fails (the inner CREATE2 runs out, so the factory reverts: no halt);
      ;; RPC's 50M default needs the caps lifted
      (let ((short (execute-request (begin-call-block db env)
                                    (list* :gas (1- (tx-outcome-gas-used outcome)) request))))
        (is (not (tx-outcome-status short)))
        (is (null (tx-outcome-halt short)) "halt ~S" (tx-outcome-halt short))
        (is (typep (tx-outcome-output short) 'octets)))
      (handler-case (progn (execute-request (begin-call-block db env) (list* :gas 50000000 request))
                           (is nil "a 50M request passed the transaction gas cap"))
        (invalid-transaction () nil))
      ;; (zero fees: the sender cannot pay 50M gas; eth_call runs without the base fee check)
      (is (tx-outcome-status (execute-request (begin-call-block db env)
                                              (list* :gas 50000000 :max-fee 0 :max-priority 0 request)
                                              :skip-gas-caps t :skip-base-fee-check t)))
      (is (equalp root (clempo.store:db-root-hash db))))))

(defblock-test t7-sstore-storage-credits 29152422
  "T7 (TIP-1060): an EVM SSTORE crossing zero pays or mints storage credits on top of 5,100.")
(defblock-test t7-sstore-storage-credits-2 29152424
  "T7 (TIP-1060): the second early T7 block with storage-credit SSTOREs.")
