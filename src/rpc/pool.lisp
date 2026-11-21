(defpackage #:clempo.rpc.pool
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:tx #:clempo.transactions)
                    (#:txpool #:clempo.txpool) (#:calls #:clempo.rpc.calls) (#:eth #:clempo.rpc.eth))
  (:documentation "The transaction pool methods over the backend's pool (CHAIN:BACKEND-POOL-*):
eth_sendRawTransaction(Sync), eth_pendingTransactions, the txpool namespace and
admin_clearTxpool, shaped as reth's TxPoolApi and EthTransactions render them. A backend
without a pool (the archive, a follower) serves an empty pool and refuses submissions.")
  (:export #:pending-tx-json #:pool-error-response #:inspect-summary #:rust-duration))

(in-package #:clempo.rpc.pool)

;;; Submission

(defun decode-or-fail (data)
  "reth's recover_raw_transaction errors: empty data, then a failed decode."
  (when (zerop (length data)) (rpc-fail +invalid-params+ "empty transaction data"))
  (handler-case (tx:decode-transaction data)
    (error () (rpc-fail +invalid-params+ "failed to decode signed transaction"))))

(defun pool-error-response (condition tx)
  "(values code message) of reth's RpcPoolError for the pool's rejection CONDITION of TX
\(rpc-eth-types error/mod.rs, Tempo's TempoPoolTransactionError for its own limits)."
  (let ((detail (txpool:pool-error-detail condition))
        (calls (and (= (tx:transaction-type tx) #x76) (tx:transaction-calls tx)))
        (access (and (member (tx:transaction-type tx) '(1 2 4 #x76)) (tx:transaction-access-list tx))))
    (case (txpool:pool-error-code condition)
      (:already-imported (values -32000 "already known"))
      (:replacement-underpriced (values -32000 "replacement transaction underpriced"))
      (:fee-cap-below-floor (values -32000 "transaction underpriced"))
      ((:lane-capacity :sender-capacity) (values -32003 "txpool is full"))
      (:nonce-too-low (values -32000 (format nil "nonce too low: next nonce ~D, tx nonce ~D"
                                             detail (tx:transaction-nonce tx))))
      (:nonce-overflow (values -32003 "nonce has max value"))
      (:invalid-signature (values +invalid-params+ "invalid transaction signature"))
      ((:unsupported-transaction-type :system-transaction) (values -32003 "transaction type not supported"))
      (:oversized-transaction (values -32000 (format nil "oversized data: transaction size ~D, limit ~D"
                                                     detail (* 128 1024))))
      (:subblock-nonce-key
       (values -32000 "Tempo Transaction with subblock nonce key prefix aren't supported in the pool"))
      (:too-many-calls
       (values -32000 (format nil "Too many calls in AA transaction: ~D exceeds maximum allowed 32" (length calls))))
      (:call-input-too-large
       (let ((i (position-if (lambda (c) (> (length (third c)) (* 128 1024))) calls)))
         (values -32000 (format nil "Call input size ~D exceeds maximum allowed ~D bytes (call index: ~D)"
                                (length (third (nth i calls))) (* 128 1024) i))))
      (:too-many-access-list-accounts
       (values -32000 (format nil "Too many access list accounts: ~D exceeds maximum allowed 256" (length access))))
      (:too-many-storage-keys-per-account
       (let ((i (position-if (lambda (a) (> (length (second a)) 256)) access)))
         (values -32000 (format nil "Too many storage keys in access list entry ~D: ~D exceeds maximum allowed 256"
                                i (length (second (nth i access)))))))
      (:too-many-storage-keys
       (values -32000 (format nil "Too many total storage keys in access list: ~D exceeds maximum allowed 2048"
                              (reduce #'+ access :key (lambda (a) (length (second a)))))))
      (:invalid-transaction
       (multiple-value-bind (code message data) (calls:invalid-request-error detail)
         (values code message data)))
      (t (values -32000 (princ-to-string condition))))))

(defun submit (data)
  "Decode DATA as reth does, then admit it into the backend's pool; the hash as octets."
  (let ((tx (decode-or-fail data)))
    (handler-case (chain:backend-submit-raw-transaction chain:*backend* data)
      (chain:no-transaction-pool () (internal-error "transaction pool unavailable"))
      (txpool:pool-error (e)
        (multiple-value-bind (code message data) (pool-error-response e tx)
          (rpc-fail code message data))))))

(defrpc "eth_sendRawTransaction" ((data bytes)) "decode and admit into the backend's pool"
  (hex32 (submit data)))

(defparameter *sync-timeout-ms* 30000 "reth RPC_DEFAULT_SEND_RAW_TX_SYNC_TIMEOUT_SECS")

(defun rust-duration (ms)
  "Rust's Debug of a Duration of MS milliseconds (\"30s\", \"1.5s\", \"500ms\")."
  (if (< ms 1000)
      (format nil "~Dms" ms)
      (multiple-value-bind (s rem) (floor ms 1000)
        (if (zerop rem)
            (format nil "~Ds" s)
            (format nil "~D.~As" s (string-right-trim "0" (format nil "~3,'0D" rem)))))))

(defun wait-for-receipt (hash timeout-ms)
  "Poll for HASH's block with the request's read hold released (blocks commit meanwhile);
the receipt JSON once a block includes it, or NIL after TIMEOUT-MS."
  (let ((deadline (+ (clempo.io:now-ms) timeout-ms)))
    (loop
      (multiple-value-bind (n i) (chain:backend-tx-location chain:*backend* hash)
        (when (and n (<= n chain:*head*)) (return (receipt-json (chain-block n) i))))
      (when (>= (clempo.io:now-ms) deadline) (return nil))
      (chain:call-outside-backend-read (lambda () (sleep 0.05))))))

(defparam timeout-ms (src) "Option<u64>: a JSON number of milliseconds"
  (clempo.serde:read-integer src 0 +u64-max+ "u64"))

(defrpc "eth_sendRawTransactionSync" ((data bytes) (timeout timeout-ms :optional t))
  "admit, then wait for the receipt (reth: a positive timeout is capped at the configured 30s)"
  (let* ((hash (submit data))
         (ms (if (and timeout (plusp timeout)) (min timeout *sync-timeout-ms*) *sync-timeout-ms*)))
    (or (wait-for-receipt hash ms)
        (rpc-fail 4 (format nil "Transaction ~A was added to the mempool but wasn't confirmed within ~A."
                            (hex32 hash) (rust-duration ms))))))

;;; Reading the pool

(defun pending-tx-json (transaction sender)
  "alloy's Transaction for a pool transaction (reth fill_pending): no block position or
timestamp, and an effective gas price without a base fee (the fee cap)."
  (let* ((head (chain-block chain:*head*))
         (b (make-cblock :number (cblock-number head) :hash (cblock-hash head) :header (cblock-header head)
                         :txs (vector transaction) :tx-hashes (vector (tx:transaction-hash transaction))
                         :senders (vector nil)))
         (object (tx-json b 0)))
    (invariant (= sender (cblock-sender b 0)) "pool sender ~X differs from the recovered ~X" sender (cblock-sender b 0))
    (cons :object
          (loop for (key . value) in (cdr object)
                unless (string= key "blockTimestamp")
                  collect (cons key (cond ((member key '("blockHash" "blockNumber" "transactionIndex") :test #'string=)
                                           :null)
                                          ((string= key "gasPrice") (q (effective-gas-price transaction nil)))
                                          (t value)))))))

(defun inspect-summary (transaction)
  "alloy TxpoolInspectSummary: \"<to>: <value> wei + <gas> gas × <max fee> wei\"; the value
of a Tempo transaction is the saturating sum of its calls' values."
  (let ((to (tx-to transaction))
        (value (if (= (tx:transaction-type transaction) #x76)
                   (min +u256-max+ (reduce #'+ (tx:transaction-calls transaction)
                                           :key (lambda (c) (octets->integer (second c)))))
                   (tx:transaction-value transaction)))
        (max-fee (if (member (tx:transaction-type transaction) '(0 1))
                     (tx:transaction-gas-price transaction)
                     (tx:transaction-max-fee-per-gas transaction))))
    (format nil "~A: ~D wei + ~D gas × ~D wei"
            (if to (format nil "~(~A~)" (addr to)) "contract creation")
            value (tx:transaction-gas-limit transaction) max-fee)))

(defun group-by-sender (entries render)
  "BTreeMap<Address, BTreeMap<String, _>>: senders in address order, each keyed by the
decimal nonce in string order; a later transaction with the same nonce replaces an earlier one."
  (let ((senders (make-hash-table)))
    (loop for (transaction sender) in entries
          do (let* ((nonces (or (gethash sender senders) (setf (gethash sender senders) (make-hash-table :test 'equal))))
                    (key (princ-to-string (tx:transaction-nonce transaction))))
               (setf (gethash key nonces) (funcall render transaction sender))))
    (cons :object
          (loop for sender in (sort (loop for s being the hash-keys of senders collect s) #'<)
                collect (cons (format nil "~(~A~)" (addr sender))
                              (nonce-map (gethash sender senders)))))))

(defun nonce-map (nonces)
  (cons :object (loop for key in (sort (loop for k being the hash-keys of nonces collect k) #'string<)
                      collect (cons key (gethash key nonces)))))

(defun pool-split (&optional sender)
  "(values pending queued) entries of the pool, those of SENDER only when given."
  (let ((entries (remove-if-not (lambda (e) (or (null sender) (= sender (second e))))
                                (chain:backend-pool-transactions chain:*backend*))))
    (values (remove-if-not #'third entries) (remove-if #'third entries))))

(defrpc "txpool_status" () "pending and queued transaction counts"
  (multiple-value-bind (pending queued) (pool-split)
    (json:jobject "pending" (q (length pending)) "queued" (q (length queued)))))

(defrpc "txpool_content" () "every pool transaction by sender and nonce"
  (multiple-value-bind (pending queued) (pool-split)
    (json:jobject "pending" (group-by-sender pending #'pending-tx-json)
                  "queued" (group-by-sender queued #'pending-tx-json))))

(defrpc "txpool_inspect" () "a one-line summary of every pool transaction"
  (flet ((summary (transaction sender) (declare (ignore sender)) (inspect-summary transaction)))
    (multiple-value-bind (pending queued) (pool-split)
      (json:jobject "pending" (group-by-sender pending #'summary)
                    "queued" (group-by-sender queued #'summary)))))

(defrpc "txpool_contentFrom" ((address address)) "one sender's pool transactions by nonce"
  (flet ((by-nonce (entries)
           (let ((nonces (make-hash-table :test 'equal)))
             (loop for (transaction sender) in entries
                   do (setf (gethash (princ-to-string (tx:transaction-nonce transaction)) nonces)
                            (pending-tx-json transaction sender)))
             (nonce-map nonces))))
    (multiple-value-bind (pending queued) (pool-split address)
      (json:jobject "pending" (by-nonce pending) "queued" (by-nonce queued)))))

(defrpc "eth_pendingTransactions" () "the pool's pending transactions"
  (map 'simple-vector (lambda (e) (pending-tx-json (first e) (second e))) (pool-split)))

(defrpc "admin_clearTxpool" () "remove every pool transaction; the number removed"
  (chain:backend-clear-pool chain:*backend*))
