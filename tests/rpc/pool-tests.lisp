(defpackage #:clempo.rpc.pool-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:render #:clempo.rpc.render) (#:archive #:clempo.archive)
                    (#:pool #:clempo.rpc.pool) (#:txpool #:clempo.txpool) (#:tx #:clempo.transactions)))
(in-package #:clempo.rpc.pool-tests)

;;; The pool methods read the backend's pool through CHAIN:BACKEND-POOL-TRANSACTIONS and
;;; submit through CHAIN:BACKEND-SUBMIT-RAW-TRANSACTION. A pool backend over the mainnet
;;; archive holds real mainnet transactions; the expected shapes follow reth's TxPoolApi
;;; (rpc/src/txpool.rs), fill_pending (rpc-convert transaction.rs) and alloy's
;;; TxpoolInspectSummary serializer.

(defclass pool-backend (chain:archive-backend)
  ((entries :initform nil :accessor entries)
   (submitted :initform nil :accessor submitted)
   (reject :initform nil :accessor reject)
   (fixed-hash :initform nil :accessor fixed-hash)))

(defmethod chain:backend-pool-transactions ((b pool-backend)) (entries b))
(defmethod chain:backend-clear-pool ((b pool-backend)) (prog1 (length (entries b)) (setf (entries b) nil)))
(defmethod chain:backend-submit-raw-transaction ((b pool-backend) raw)
  (when (reject b) (error 'txpool:pool-error :code (first (reject b)) :detail (second (reject b))))
  (push raw (submitted b))
  (or (fixed-hash b) (clempo.keccak:keccak256 raw)))

(defparameter *block* 3463 "A mainnet block with two transactions.")

(defun pool-backend ()
  (unless (archive:chunk-present-p 0 (archive:archive-dir))
    (skip-test "no mainnet archive under ~/cache/clempo/mainnet"))
  (chain:make-archive-backend :class 'pool-backend :head *block*))

(defun call (backend method &rest params)
  (json:parse-json
   (chain:with-backend-read (backend)
     (nth-value 1 (rpc:handle-body (json:to-json-string (json:jobject "jsonrpc" "2.0" "id" 1 "method" method
                                                               "params" (coerce params 'simple-vector))))))))

(defun block-entries (backend)
  "The transactions of *BLOCK* as pool entries: the first pending, the second queued."
  (chain:with-backend-read (backend)
    (let ((b (render:chain-block *block*)))
      (loop for i below (length (render:cblock-txs b))
            collect (list (svref (render:cblock-txs b) i) (render:cblock-sender b i) (zerop i))))))

(deftest archive-pool-is-empty ()
  ;; The archive backend has no pool: reth's answers for an empty pool, and a refusal to submit.
  (let ((b (chain:make-archive-backend :head 0)))
    (is= (json:parse-json "{\"pending\":\"0x0\",\"queued\":\"0x0\"}") (json:jget (call b "txpool_status") "result"))
    (is= (json:parse-json "{\"pending\":{},\"queued\":{}}") (json:jget (call b "txpool_content") "result"))
    (is= 0 (length (json:jget (call b "eth_pendingTransactions") "result")))
    (is= 0 (json:jget (call b "admin_clearTxpool") "result"))))

(deftest pool-methods-read-the-backend-pool ()
  (let* ((b (pool-backend)) (entries (block-entries b)))
    (setf (entries b) entries)
    (destructuring-bind ((tx0 sender0 p0) (tx1 sender1 p1)) entries
      (declare (ignore p0 p1))
      (is= (json:parse-json "{\"pending\":\"0x1\",\"queued\":\"0x1\"}") (json:jget (call b "txpool_status") "result"))
      ;; a pending transaction is the mined one without its block position and timestamp,
      ;; priced at its fee cap
      (let* ((mined (json:jget (call b "eth_getTransactionByHash" (octets->hex (tx:transaction-hash tx0))) "result"))
             (pending (json:jget (call b "eth_pendingTransactions") "result")))
        (is= 1 (length pending))
        (let ((p (svref pending 0)))
          (dolist (key '("blockHash" "blockNumber" "transactionIndex"))
            (is (eq :null (json:jget p key)) "~A is null" key))
          (is (null (assoc "blockTimestamp" (cdr p) :test #'string=)))
          (is= (render:q (render:effective-gas-price tx0 nil)) (json:jget p "gasPrice"))
          (dolist (key '("hash" "from" "nonce" "input" "type" "gas"))
            (is= (json:jget mined key) (json:jget p key) "~A" key))))
      (let* ((content (json:jget (call b "txpool_content") "result"))
             (from0 (format nil "~(~A~)" (render:addr sender0)))
             (from1 (format nil "~(~A~)" (render:addr sender1))))
        (is= (octets->hex (tx:transaction-hash tx0))
             (json:jget* content "pending" from0 (princ-to-string (tx:transaction-nonce tx0)) "hash"))
        (is= (octets->hex (tx:transaction-hash tx1))
             (json:jget* content "queued" from1 (princ-to-string (tx:transaction-nonce tx1)) "hash"))
        (let ((only (json:jget (call b "txpool_contentFrom" from1) "result")))
          (is= (json:jobject) (json:jget only "pending"))
          (is= (json:jget* content "queued" from1) (json:jget only "queued")))
        (is= (pool:inspect-summary tx0)
             (json:jget* (json:jget (call b "txpool_inspect") "result")
                         "pending" from0 (princ-to-string (tx:transaction-nonce tx0)))))
      (is= 2 (json:jget (call b "admin_clearTxpool") "result"))
      (is= (json:parse-json "{\"pending\":\"0x0\",\"queued\":\"0x0\"}") (json:jget (call b "txpool_status") "result")))))

(deftest inspect-summary-format ()
  ;; alloy: "<to:?>: <value> wei + <gas> gas × <gas_price> wei", Debug of an Address is
  ;; lowercase; a legacy transaction's fee cap is its gas price
  (let* ((b (pool-backend)) (tx0 (first (first (block-entries b)))))
    (is= (format nil "~(~A~): ~D wei + ~D gas × ~D wei" (render:addr (render:tx-to tx0))
                 (if (= (tx:transaction-type tx0) #x76) 0 (tx:transaction-value tx0))
                 (tx:transaction-gas-limit tx0)
                 (if (member (tx:transaction-type tx0) '(0 1)) (tx:transaction-gas-price tx0)
                     (tx:transaction-max-fee-per-gas tx0)))
         (pool:inspect-summary tx0))))

(deftest send-raw-transaction-submits-to-the-pool ()
  (let* ((b (pool-backend)) (tx0 (first (first (block-entries b))))
         (raw (clempo.transactions::transaction-raw tx0)))
    (is= (octets->hex (tx:transaction-hash tx0)) (json:jget (call b "eth_sendRawTransaction" (octets->hex raw)) "result"))
    (is-equalp raw (first (submitted b)))
    ;; reth's RpcPoolError texts and codes for the pool's rejections
    (loop for (code detail rpc-code message) in
          `((:already-imported nil -32000 "already known")
            (:replacement-underpriced nil -32000 "replacement transaction underpriced")
            (:sender-capacity nil -32003 "txpool is full")
            (:nonce-too-low 7 -32000 ,(format nil "nonce too low: next nonce 7, tx nonce ~D" (tx:transaction-nonce tx0))))
          do (setf (reject b) (list code detail))
             (let ((e (json:jget (call b "eth_sendRawTransaction" (octets->hex raw)) "error")))
               (is= rpc-code (json:jget e "code") "~A" code)
               (is= message (json:jget e "message") "~A" code)))
    ;; decoding comes first, with reth's invalid-params errors
    (is= "empty transaction data" (json:jget* (call b "eth_sendRawTransaction" "0x") "error" "message"))
    (is= "failed to decode signed transaction" (json:jget* (call b "eth_sendRawTransaction" "0x0102") "error" "message"))))

(deftest send-raw-transaction-without-pool ()
  (let ((b (pool-backend)))
    (change-class b 'chain:archive-backend)
    (let ((raw (clempo.transactions::transaction-raw (first (first (block-entries b))))))
      (is= "transaction pool unavailable"
           (json:jget* (call b "eth_sendRawTransaction" (octets->hex raw)) "error" "message")))))

;; reth's TransactionConfirmationTimeout (code 4) after the requested timeout, its Duration
;; printed with Rust's Debug; a transaction already in a block answers its receipt.
(deftest send-raw-transaction-sync ()
  (let* ((b (pool-backend))
         (tx0 (first (first (block-entries b))))
         (signed (octets->hex (clempo.transactions::transaction-raw tx0)))
         (never (make-array 32 :element-type (quote octet) :initial-element 7)))
    (is= (octets->hex (tx:transaction-hash tx0))
         (json:jget* (call b "eth_sendRawTransactionSync" signed 100) "result" "transactionHash"))
    (setf (fixed-hash b) never)
    (let ((e (json:jget (call b "eth_sendRawTransactionSync" signed 120) "error")))
      (is= 4 (json:jget e "code"))
      (is= (format nil "Transaction ~A was added to the mempool but wasn't confirmed within 120ms."
                   (octets->hex never))
           (json:jget e "message"))))
  (is= "30s" (pool:rust-duration 30000))
  (is= "1.5s" (pool:rust-duration 1500))
  (is= "1.25s" (pool:rust-duration 1250))
  (is= "500ms" (pool:rust-duration 500)))
