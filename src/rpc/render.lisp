(defpackage #:clempo.rpc.render
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rlp #:clempo.rlp) (#:tx #:clempo.transactions)
                    (#:h #:clempo.headers) (#:r #:clempo.receipts) (#:chain #:clempo.rpc.chain))
  (:documentation "Blocks, headers, transactions, receipts and logs as the reference node renders
them in JSON (alloy's serde shapes with Tempo's header, 0x76 transaction and receipt fields).
CHAIN-BLOCK decodes a block once per backend and caches it with its transaction hashes,
senders and receipts, which are filled in on first use.")
  (:export #:cblock #:chain-block #:cblock-number #:cblock-hash #:cblock-header #:cblock-raw
           #:cblock-txs #:cblock-tx-hashes #:cblock-size #:cblock-sender #:cblock-receipts
           #:cblock-tx-raw #:cblock-timestamp #:cblock-base-fee #:cblock-inner
           #:header-json #:block-json #:tx-json #:receipt-json #:receipt-jsons #:log-json
           #:block-logs #:q #:hex32 #:addr #:hexbytes #:effective-gas-price #:tx-to))

(in-package #:clempo.rpc.render)

;;; Scalars

(defun q (n) (quantity->hex n))
(defun hexbytes (o) (octets->hex o))
(defun hex32 (o) (octets->hex o))
(defun addr (n) (integer->hex n :bytes 20))
(defun opt (x f) (if x (funcall f x) :null))
(defun item-int (item) (rlp:rlp-decode-integer item))
(defun item-addr (item) (addr (octets->integer item)))

;;; Decoded blocks, cached per backend

(defstruct cblock number raw hash header txs tx-hashes tx-bounds (senders nil) (receipt-cache nil) size
  (lock (sb-thread:make-mutex :name "cblock")))

(defvar *caches* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defparameter *cache-limit* 4096)

(defun backend-cache (backend)
  (or (gethash backend *caches*)
      (setf (gethash backend *caches*) (make-hash-table :synchronized t))))

(defun chain-block (n &optional (backend chain:*backend*))
  "The decoded block N from BACKEND, or NIL when it has none."
  (let ((cache (backend-cache backend)))
    (or (gethash n cache)
        (let ((raw (chain:backend-block-raw backend n)))
          (when raw
            (let* ((b (clempo.blocks:decode-block raw))
                   (header (clempo.blocks:block-header b))
                   (txs (coerce (clempo.blocks:block-transactions b) 'simple-vector)))
              (when (>= (hash-table-count cache) *cache-limit*) (clrhash cache))
              (setf (gethash n cache)
                    (make-cblock :number n :raw raw :hash (h:header-hash header) :header header
                                 :txs txs
                                 :tx-hashes (map 'simple-vector #'tx:transaction-hash txs)
                                 :senders (make-array (length txs) :initial-element nil)
                                 :size (length raw)))))))))

(defun cblock-inner (b) (h:header-inner (cblock-header b)))
(defun cblock-timestamp (b) (h:ethereum-header-timestamp (cblock-inner b)))
(defun cblock-base-fee (b) (h:ethereum-header-base-fee-per-gas (cblock-inner b)))
(defun cblock-tx-raw (b i) (clempo.transactions::transaction-raw (svref (cblock-txs b) i)))

(defun cblock-sender (b i)
  "(values sender key-id) of transaction I, recovered once."
  (let ((cell (svref (cblock-senders b) i)))
    (unless cell
      (setf cell (multiple-value-list (tx:transaction-sender (svref (cblock-txs b) i)))
            (svref (cblock-senders b) i) cell))
    (values (first cell) (second cell))))

(defun cblock-receipts (b)
  "Block B's receipts as clempo.receipts structures, in transaction order."
  (or (cblock-receipt-cache b)
      (setf (cblock-receipt-cache b)
            (coerce (mapcar #'r:decode-receipt (chain:backend-receipts-raw chain:*backend* (cblock-number b)))
                    'simple-vector))))


;;; Headers and blocks

(defun header-json (header &optional (hash (h:header-hash header)))
  (let* ((i (h:header-inner header))
         (fields
           (list "hash" (hex32 hash)
                 "parentHash" (hex32 (h:ethereum-header-parent-hash i))
                 "sha3Uncles" (hex32 (h:ethereum-header-ommers-hash i))
                 "miner" (addr (h:ethereum-header-beneficiary i))
                 "stateRoot" (hex32 (h:ethereum-header-state-root i))
                 "transactionsRoot" (hex32 (h:ethereum-header-transactions-root i))
                 "receiptsRoot" (hex32 (h:ethereum-header-receipts-root i))
                 "logsBloom" (hexbytes (h:ethereum-header-logs-bloom i))
                 "difficulty" (q (h:ethereum-header-difficulty i))
                 "number" (q (h:ethereum-header-number i))
                 "gasLimit" (q (h:ethereum-header-gas-limit i))
                 "gasUsed" (q (h:ethereum-header-gas-used i))
                 "timestamp" (q (h:ethereum-header-timestamp i))
                 "extraData" (hexbytes (h:ethereum-header-extra-data i))
                 "mixHash" (hex32 (h:ethereum-header-mix-hash i))
                 "nonce" (hexbytes (h:ethereum-header-nonce i)))))
    (flet ((add (key value) (when value (setf fields (append fields (list key value))))))
      (add "baseFeePerGas" (let ((v (h:ethereum-header-base-fee-per-gas i))) (and v (q v))))
      (add "withdrawalsRoot" (let ((v (h:ethereum-header-withdrawals-root i))) (and v (hex32 v))))
      (add "blobGasUsed" (let ((v (h:ethereum-header-blob-gas-used i))) (and v (q v))))
      (add "excessBlobGas" (let ((v (h:ethereum-header-excess-blob-gas i))) (and v (q v))))
      (add "parentBeaconBlockRoot" (let ((v (h:ethereum-header-parent-beacon-block-root i))) (and v (hex32 v))))
      (add "requestsHash" (let ((v (h:ethereum-header-requests-hash i))) (and v (hex32 v))))
      (add "blockAccessListHash" (let ((v (h:ethereum-header-block-access-list-hash i))) (and v (hex32 v))))
      (add "slotNumber" (let ((v (h:ethereum-header-slot-number i))) (and v (q v))))
      (add "mainBlockGeneralGasLimit" (q (h:header-general-gas-limit header)))
      (add "sharedGasLimit" (q (h:header-shared-gas-limit header)))
      (add "timestampMillisPart" (q (h:header-timestamp-millis-part header)))
      (add "consensusContext"
           (let ((c (h:header-consensus-context header)))
             (and c (json:jobject "epoch" (h:consensus-context-epoch c)
                                  "view" (h:consensus-context-view c)
                                  "parentView" (h:consensus-context-parent-view c)
                                  "proposer" (hex32 (h:consensus-context-proposer c))))))
      (add "timestampMillis" (q (h:header-timestamp-millis header))))
    (apply #'json:jobject fields)))

(defun block-json (b full)
  (let ((j (header-json (cblock-header b) (cblock-hash b)))
        (blk (clempo.blocks:decode-block (cblock-raw b))))
    (append j
            (list (cons "size" (q (cblock-size b)))
                  (cons "uncles" (map 'simple-vector (lambda (o) (hex32 (h:header-hash o)))
                                      (clempo.blocks:block-ommers blk)))
                  (cons "transactions"
                        (if full
                            (coerce (loop for i from 0 below (length (cblock-txs b)) collect (tx-json b i)) 'simple-vector)
                            (map 'simple-vector #'hex32 (cblock-tx-hashes b)))))
            (when (clempo.blocks:block-withdrawals-present-p blk)
              (list (cons "withdrawals"
                          (map 'simple-vector
                               (lambda (w) (json:jobject "index" (q (item-int (first w)))
                                                         "validatorIndex" (q (item-int (second w)))
                                                         "address" (item-addr (third w))
                                                         "amount" (q (item-int (fourth w)))))
                               (clempo.blocks:block-withdrawals blk))))))))

;;; Transactions

(defun access-list-json (items)
  (map 'simple-vector
       (lambda (e) (json:jobject "address" (item-addr (first e))
                                 "storageKeys" (map 'simple-vector #'hex32 (second e))))
       items))

(defun ecdsa-json (parity r s)
  (json:jobject "r" (q r) "s" (q s) "yParity" (q parity) "v" (q parity)))

(defun primitive-signature-json (sig)
  "PrimitiveSignature serde: tagged by type."
  (let ((n (length sig)))
    (cond ((= n 65)
           (let ((v (aref sig 64)))
             (append (json:jobject "type" "secp256k1")
                     (cdr (ecdsa-json (if (>= v 27) (- v 27) v)
                                      (octets->integer sig :end 32) (octets->integer sig :start 32 :end 64))))))
          ((= (aref sig 0) 1)
           (json:jobject "type" "p256"
                         "r" (hexbytes (subseq sig 1 33)) "s" (hexbytes (subseq sig 33 65))
                         "pubKeyX" (hexbytes (subseq sig 65 97)) "pubKeyY" (hexbytes (subseq sig 97 129))
                         "preHash" (json:jbool (/= 0 (aref sig 129)))))
          (t
           (let ((k (- n 128)))
             (json:jobject "type" "webAuthn"
                           "r" (hexbytes (subseq sig k (+ k 32))) "s" (hexbytes (subseq sig (+ k 32) (+ k 64)))
                           "pubKeyX" (hexbytes (subseq sig (+ k 64) (+ k 96)))
                           "pubKeyY" (hexbytes (subseq sig (+ k 96) (+ k 128)))
                           "webauthnData" (hexbytes (subseq sig 1 k))))))))

(defun tempo-signature-json (sig &optional key-id)
  "TempoSignature serde: a primitive, or a keychain envelope with its version and key id."
  (if (and (/= (length sig) 65) (member (aref sig 0) '(3 4)))
      (json:jobject "userAddress" (addr (octets->integer sig :start 1 :end 21))
                    "signature" (primitive-signature-json (subseq sig 21))
                    "version" (if (= (aref sig 0) 3) "v1" "v2")
                    "keyId" (if key-id (addr key-id)
                                (addr (nth-value 1 (clempo.signatures:recover-signature sig (make-octets 32))))))
      (primitive-signature-json sig)))

(defun key-type-name (n) (case n (0 "secp256k1") (1 "p256") (2 "webAuthn") (3 "configurableAccount")))

(defun key-authorization-json (ka)
  (destructuring-bind (auth sig) ka
    (flet ((field (i) (let ((x (nth i auth))) (and x (not (and (typep x 'octets) (zerop (length x)))) x))))
      (json:jobject
       "chainId" (q (item-int (nth 0 auth)))
       "keyType" (key-type-name (item-int (nth 1 auth)))
       "keyId" (item-addr (nth 2 auth))
       "expiry" (opt (field 3) (lambda (x) (q (item-int x))))
       "limits" (opt (field 4)
                     (lambda (limits)
                       (map 'simple-vector
                            (lambda (l) (json:jobject "token" (item-addr (first l))
                                                      "limit" (q (item-int (second l)))
                                                      "period" (q (if (third l) (item-int (third l)) 0))))
                            limits)))
       "allowedCalls" (opt (field 5)
                           (lambda (scopes)
                             (map 'simple-vector
                                  (lambda (s)
                                    (apply #'json:jobject
                                           "target" (item-addr (first s))
                                           (when (second s)
                                             (list "selectorRules"
                                                   (map 'simple-vector
                                                        (lambda (rule)
                                                          (apply #'json:jobject
                                                                 "selector" (hexbytes (first rule))
                                                                 (when (second rule)
                                                                   (list "recipients" (map 'simple-vector #'item-addr (second rule))))))
                                                        (second s))))))
                                  scopes)))
       "witness" (opt (field 6) #'hex32)
       "isAdmin" (json:jbool (field 7))
       "account" (opt (field 8) #'item-addr)
       "signature" (primitive-signature-json sig)))))

(defun effective-gas-price (tx base-fee)
  (let ((type (tx:transaction-type tx)))
    (if (member type '(0 1))
        (tx:transaction-gas-price tx)
        (let ((max-fee (tx:transaction-max-fee-per-gas tx))
              (prio (tx:transaction-max-priority-fee-per-gas tx)))
          (if base-fee (min max-fee (+ base-fee prio)) max-fee)))))

(defun tx-to (tx)
  "The call target: the first call's for a Tempo transaction, NIL for a creation."
  (if (= (tx:transaction-type tx) #x76)
      (let ((to (first (first (tx:transaction-calls tx))))) (and (plusp (length to)) (octets->integer to)))
      (tx:transaction-to tx)))

(defun tx-json (b i)
  "Transaction I of block B as eth_getTransactionByHash renders it."
  (let* ((tx (svref (cblock-txs b) i))
         (type (tx:transaction-type tx))
         (fields (tx:transaction-fields tx)))
    (multiple-value-bind (sender key-id) (cblock-sender b i)
      (let ((body
              (ecase type
                (0 (let* ((v (item-int (nth 6 fields))) (chain-id (tx:transaction-chain-id tx)))
                     (append (list "type" "0x0")
                             (when chain-id (list "chainId" (q chain-id)))
                             (list "nonce" (q (tx:transaction-nonce tx)) "gasPrice" (q (tx:transaction-gas-price tx))
                                   "gas" (q (tx:transaction-gas-limit tx))
                                   "to" (opt (tx:transaction-to tx) #'addr) "value" (q (tx:transaction-value tx))
                                   "input" (hexbytes (tx:transaction-input tx))
                                   "r" (q (item-int (nth 7 fields))) "s" (q (item-int (nth 8 fields))) "v" (q v)))))
                (1 (list "type" "0x1" "chainId" (q (tx:transaction-chain-id tx)) "nonce" (q (tx:transaction-nonce tx))
                         "gasPrice" (q (tx:transaction-gas-price tx)) "gas" (q (tx:transaction-gas-limit tx))
                         "to" (opt (tx:transaction-to tx) #'addr) "value" (q (tx:transaction-value tx))
                         "accessList" (access-list-json (tx:transaction-access-list tx))
                         "input" (hexbytes (tx:transaction-input tx))
                         "r" (q (item-int (nth 9 fields))) "s" (q (item-int (nth 10 fields)))
                         "yParity" (q (item-int (nth 8 fields))) "v" (q (item-int (nth 8 fields)))))
                ((2 4)
                 (append
                  (list "type" (q type) "chainId" (q (tx:transaction-chain-id tx)) "nonce" (q (tx:transaction-nonce tx))
                        "gas" (q (tx:transaction-gas-limit tx))
                        "maxFeePerGas" (q (tx:transaction-max-fee-per-gas tx))
                        "maxPriorityFeePerGas" (q (tx:transaction-max-priority-fee-per-gas tx))
                        "to" (opt (tx:transaction-to tx) #'addr) "value" (q (tx:transaction-value tx))
                        "accessList" (access-list-json (tx:transaction-access-list tx)))
                  (when (= type 4)
                    (list "authorizationList"
                          (map 'simple-vector
                               (lambda (a) (json:jobject "chainId" (q (item-int (first a))) "address" (item-addr (second a))
                                                         "nonce" (q (item-int (third a))) "yParity" (q (item-int (fourth a)))
                                                         "r" (q (item-int (fifth a))) "s" (q (item-int (sixth a)))))
                               (tx:transaction-authorization-list tx))))
                  (let ((sig (last fields 3)))
                    (list "input" (hexbytes (tx:transaction-input tx))
                          "r" (q (item-int (second sig))) "s" (q (item-int (third sig)))
                          "yParity" (q (item-int (first sig))) "v" (q (item-int (first sig)))))
                  (list "gasPrice" (q (effective-gas-price tx (cblock-base-fee b))))))
                (#x76
                 (let ((sponsor (nth 11 fields)) (ka (and (= (length fields) 15) (nth 13 fields))))
                   (list "type" "0x76" "chainId" (q (tx:transaction-chain-id tx))
                         "feeToken" (opt (tx:transaction-fee-token tx) #'addr)
                         "maxPriorityFeePerGas" (q (tx:transaction-max-priority-fee-per-gas tx))
                         "maxFeePerGas" (q (tx:transaction-max-fee-per-gas tx))
                         "gas" (q (tx:transaction-gas-limit tx))
                         "calls" (map 'simple-vector
                                      (lambda (c) (json:jobject "to" (if (plusp (length (first c))) (item-addr (first c)) :null)
                                                                "value" (q (item-int (second c)))
                                                                "input" (hexbytes (third c)) "data" :null))
                                      (tx:transaction-calls tx))
                         "accessList" (access-list-json (tx:transaction-access-list tx))
                         "nonceKey" (q (tx:transaction-nonce-key tx))
                         "nonce" (q (tx:transaction-nonce tx))
                         "feePayerSignature" (if (consp sponsor)
                                                 (ecdsa-json (item-int (first sponsor)) (item-int (second sponsor))
                                                             (item-int (third sponsor)))
                                                 :null)
                         "validBefore" (opt (tx:transaction-valid-before tx) #'q)
                         "validAfter" (opt (tx:transaction-valid-after tx) #'q)
                         "keyAuthorization" (if (consp ka) (key-authorization-json ka) :null)
                         "aaAuthorizationList"
                         (map 'simple-vector
                              (lambda (a) (json:jobject "chainId" (q (item-int (first a))) "address" (item-addr (second a))
                                                        "nonce" (q (item-int (third a)))
                                                        "signature" (tempo-signature-json (fourth a))))
                              (tx:transaction-tempo-authorization-list tx))
                         "signature" (tempo-signature-json (car (last fields)) key-id)
                         "gasPrice" (q (effective-gas-price tx (cblock-base-fee b)))))))))
        (apply #'json:jobject
               (append body
                       (list "hash" (hex32 (svref (cblock-tx-hashes b) i))
                             "blockHash" (hex32 (cblock-hash b))
                             "blockNumber" (q (cblock-number b))
                             "transactionIndex" (q i)
                             "from" (addr sender))
                       (list "blockTimestamp" (q (cblock-timestamp b)))))))))

;;; Receipts and logs

(defun log-json (b i log-index tx-index log)
  (declare (ignore i))
  (json:jobject "address" (addr (r:event-log-address log))
                "topics" (map 'simple-vector #'hex32 (r:event-log-topics log))
                "data" (hexbytes (r:event-log-data log))
                "blockHash" (hex32 (cblock-hash b))
                "blockNumber" (q (cblock-number b))
                "blockTimestamp" (q (cblock-timestamp b))
                "transactionHash" (hex32 (svref (cblock-tx-hashes b) tx-index))
                "transactionIndex" (q tx-index)
                "logIndex" (q log-index)
                "removed" :false))

(defun create-address (sender nonce)
  (octets->integer (clempo.keccak:keccak256
                    (rlp:rlp-encode (list (integer->octets sender 20) (integer->octets-minimal nonce))))
                   :start 12))

(defun receipt-jsons (b)
  "Every receipt of block B, rendered."
  (let ((receipts (cblock-receipts b)) (log-index 0) (prev-gas 0))
    (coerce
     (loop for i from 0 below (length receipts)
           for rc = (svref receipts i)
           for tx = (svref (cblock-txs b) i)
           collect (let* ((sender (cblock-sender b i))
                          (gas-used (- (r:receipt-cumulative-gas-used rc) prev-gas))
                          (price (effective-gas-price tx (cblock-base-fee b)))
                          (to (tx-to tx))
                          (logs (loop for log in (r:receipt-logs rc)
                                      collect (prog1 (log-json b i log-index i log) (incf log-index))))
                          (fee-payer (if (= (tx:transaction-type tx) #x76) (tx:transaction-fee-payer tx) sender)))
                     (setf prev-gas (r:receipt-cumulative-gas-used rc))
                     (apply #'json:jobject
                            "type" (q (r:receipt-type rc))
                            "status" (q (r:receipt-status rc))
                            "cumulativeGasUsed" (q (r:receipt-cumulative-gas-used rc))
                            "logs" (coerce logs 'simple-vector)
                            "logsBloom" (hexbytes (or (r:receipt-bloom rc) (r:logs-bloom (r:receipt-logs rc))))
                            "transactionHash" (hex32 (svref (cblock-tx-hashes b) i))
                            "transactionIndex" (q i)
                            "blockHash" (hex32 (cblock-hash b))
                            "blockNumber" (q (cblock-number b))
                            "gasUsed" (q gas-used)
                            "effectiveGasPrice" (q price)
                            "from" (addr sender)
                            "to" (opt to #'addr)
                            "contractAddress" (if to :null
                                                  (addr (create-address sender (tx:transaction-nonce tx))))
                            (append
                             (when (and (plusp price) (plusp gas-used) (r:receipt-logs rc))
                               (list "feeToken" (addr (r:event-log-address (car (last (r:receipt-logs rc)))))))
                             (list "feePayer" (addr fee-payer))))))
     'simple-vector)))

(defun receipt-json (b i) (svref (receipt-jsons b) i))

(defun block-logs (b)
  "Every log of block B rendered, in order."
  (loop for rc across (receipt-jsons b)
        nconc (coerce (json:jget rc "logs") 'list)))
