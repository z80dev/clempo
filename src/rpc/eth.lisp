(defpackage #:clempo.rpc.eth
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:tx #:clempo.transactions)
                    (#:h #:clempo.headers) (#:r #:clempo.receipts))
  (:documentation "The web3, net and eth namespaces over a backend's blocks and receipts:
everything that needs no execution.")
  (:export #:resolve-block #:block-not-found #:*client-version* #:tag-number))

(in-package #:clempo.rpc.eth)

(defparameter *client-version* "clempo/v0.1.0/sbcl-2.6.8")
(defparameter +native-balance-placeholder+
  4242424242424242424242424242424242424242424242424242424242424242424242424242
  "Tempo has no native balance: eth_getBalance answers this constant for every account.")

;;; Block selection

(defun tag-number (tag)
  "The block number a tag or number selects, or NIL (pending has no block)."
  (case tag
    ((:latest :safe :finalized) chain:*head*)
    (:earliest 0)
    (:pending nil)
    (t tag)))

(defun hash-number (hash) (chain:backend-block-number chain:*backend* hash))

(defun resolve-block (id)
  "The cached block an id selects (number, tag or (:hash h ...)), or NIL."
  (let ((n (if (and (consp id) (eq (first id) :hash)) (hash-number (second id)) (tag-number id))))
    (and n (<= n chain:*head*) (chain-block n))))

(defun block-not-found (id)
  (rpc-fail -32001 (if (and (consp id) (eq (first id) :hash))
                       (format nil "block not found: hash ~A" (hex32 (second id)))
                       (format nil "block not found: ~A" (if (integerp id) (q id) (string-downcase (string id)))))))

(defun tx-location (hash)
  "(values block index) of a transaction hash at or below the head, or NIL."
  (multiple-value-bind (n i) (chain:backend-tx-location chain:*backend* hash)
    (when (and n (<= n chain:*head*)) (values (chain-block n) i))))

;;; web3, net, rpc

(defrpc "web3_clientVersion" () "client version string" *client-version*)
(defrpc "web3_sha3" ((data sha3-bytes)) "keccak256 of DATA" (hex32 (clempo.keccak:keccak256 data)))

(defparam sha3-bytes (src) "alloy Bytes as web3_sha3 reads them"
  (case (clempo.serde:peek-kind src)
    (:string
     (let* ((s (clempo.serde:read-string src)) (h (if (and (>= (length s) 2) (string-equal "0x" s :end2 2)) (subseq s 2) s)))
       (if (or (oddp (length h)) (notevery (lambda (c) (digit-char-p c 16)) h))
           (clempo.serde:serde-fail-here src "invalid value: string ~A, expected a valid hex string"
                                         (clempo.serde:rust-debug-string s))
           (hex->octets h))))
    (t (clempo.serde:invalid-type
        src "a variable number of bytes represented as a hex string, an array of u8, or raw bytes"))))

(defrpc "net_version" () "chain id in decimal" (format nil "~D" (chain:backend-chain-id chain:*backend*)))
(defrpc "net_listening" () "always true" :true)
(defrpc "net_peerCount" () "connected peers" "0x0")
(defrpc "rpc_modules" () "enabled namespaces"
  (json:jobject "reth" "1.0" "tempo" "1.0" "trace" "1.0" "rpc" "1.0" "txpool" "1.0" "debug" "1.0"
                "web3" "1.0" "admin" "1.0" "eth" "1.0" "net" "1.0"))

;;; eth: chain facts

(defrpc "eth_chainId" () "chain id" (q (chain:backend-chain-id chain:*backend*)))
(defrpc "eth_blockNumber" () "head block number" (q chain:*head*))
(defrpc "eth_protocolVersion" () "eth wire protocol version" "0x5")
(defrpc "eth_syncing" () "sync status" :false)
(defrpc "eth_hashrate" () "no mining" "0x0")
(defrpc "eth_accounts" () "no local accounts" #())
(defrpc "eth_coinbase" () "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "eth_mining" () "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "eth_getWork" () "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "eth_submitWork" ((nonce any) (pow-hash any) (mix any)) "unimplemented in the reference"
  (internal-error "unimplemented"))
(defrpc "eth_submitHashrate" ((rate any) (id any)) "no mining" :false)
(defrpc "eth_blobBaseFee" () "blob base fee" "0x1")
(defrpc "eth_maxPriorityFeePerGas" () "suggested tip" "0x0")
(defrpc "eth_gasPrice" () "suggested gas price: the head's base fee"
  (let ((b (chain-block chain:*head*))) (q (or (cblock-base-fee b) 0))))

(defrpc "eth_getBalance" ((address address) (block block-id :optional t))
  "Tempo's placeholder: native balances do not exist"
  (q +native-balance-placeholder+))

;;; eth: blocks

(defrpc "eth_getBlockByNumber" ((id block-number-or-tag) (full bool)) "block by number or tag"
  (let ((b (resolve-block id))) (if b (block-json b full) :null)))

(defrpc "eth_getBlockByHash" ((hash hash) (full bool)) "block by hash"
  (let ((b (resolve-block (list :hash hash)))) (if b (block-json b full) :null)))

(defrpc "eth_getHeaderByNumber" ((id block-number-or-tag)) "header by number or tag"
  (let ((b (resolve-block id))) (if b (header-json (cblock-header b) (cblock-hash b)) :null)))

(defrpc "eth_getHeaderByHash" ((hash hash)) "header by hash"
  (let ((b (resolve-block (list :hash hash)))) (if b (header-json (cblock-header b) (cblock-hash b)) :null)))

(defrpc "eth_getBlockTransactionCountByNumber" ((id block-number-or-tag)) "transaction count"
  (let ((b (resolve-block id))) (if b (q (length (cblock-txs b))) :null)))

(defrpc "eth_getBlockTransactionCountByHash" ((hash hash)) "transaction count"
  (let ((b (resolve-block (list :hash hash)))) (if b (q (length (cblock-txs b))) :null)))

(defrpc "eth_getUncleCountByBlockNumber" ((id block-number-or-tag)) "ommer count"
  (let ((b (resolve-block id))) (if b "0x0" :null)))

(defrpc "eth_getUncleCountByBlockHash" ((hash hash)) "ommer count"
  (let ((b (resolve-block (list :hash hash)))) (if b "0x0" :null)))

(defrpc "eth_getUncleByBlockNumberAndIndex" ((id block-number-or-tag) (index index)) "no ommers" :null)
(defrpc "eth_getUncleByBlockHashAndIndex" ((hash hash) (index index)) "no ommers" :null)

(defrpc "eth_getBlockReceipts" ((id block-id)) "every receipt of a block"
  (let ((b (resolve-block id))) (if b (receipt-jsons b) :null)))

;;; eth: transactions

(defun tx-at (b i) (if (and b (< i (length (cblock-txs b)))) (tx-json b i) :null))
(defun raw-tx-at (b i) (if (and b (< i (length (cblock-txs b)))) (hexbytes (cblock-tx-raw b i)) :null))

(defrpc "eth_getTransactionByHash" ((hash hash)) "transaction by hash"
  (multiple-value-bind (b i) (tx-location hash) (if b (tx-json b i) :null)))

(defrpc "eth_getRawTransactionByHash" ((hash hash)) "EIP-2718 bytes by hash"
  (multiple-value-bind (b i) (tx-location hash) (if b (hexbytes (cblock-tx-raw b i)) :null)))

(defrpc "eth_getTransactionReceipt" ((hash hash)) "receipt by transaction hash"
  (multiple-value-bind (b i) (tx-location hash) (if b (receipt-json b i) :null)))

(defrpc "eth_getTransactionByBlockNumberAndIndex" ((id block-number-or-tag) (index index)) "by position"
  (tx-at (resolve-block id) index))
(defrpc "eth_getTransactionByBlockHashAndIndex" ((hash hash) (index index)) "by position"
  (tx-at (resolve-block (list :hash hash)) index))
(defrpc "eth_getRawTransactionByBlockNumberAndIndex" ((id block-number-or-tag) (index index)) "raw by position"
  (raw-tx-at (resolve-block id) index))
(defrpc "eth_getRawTransactionByBlockHashAndIndex" ((hash hash) (index index)) "raw by position"
  (raw-tx-at (resolve-block (list :hash hash)) index))

;;; debug raw methods

(defun block-or-fail (id)
  (or (resolve-block id) (block-not-found id)))

(defrpc "debug_getRawHeader" ((id block-id)) "RLP header"
  (hexbytes (h:encode-header (cblock-header (block-or-fail id)))))

(defrpc "debug_getRawBlock" ((id block-id)) "RLP block"
  (hexbytes (cblock-raw (block-or-fail id))))

(defrpc "debug_getRawReceipts" ((id block-id)) "EIP-2718 receipts"
  (let ((b (block-or-fail id)))
    (map 'simple-vector #'hexbytes (chain:backend-receipts-raw chain:*backend* (cblock-number b)))))

(defrpc "debug_getRawTransactions" ((id block-id)) "EIP-2718 transactions; [] for an unknown block"
  (let ((b (resolve-block id)))
    (if b
        (coerce (loop for i from 0 below (length (cblock-txs b)) collect (hexbytes (cblock-tx-raw b i))) 'simple-vector)
        #())))

(defrpc "debug_getRawTransaction" ((hash hash)) "EIP-2718 bytes by hash"
  (multiple-value-bind (b i) (tx-location hash) (if b (hexbytes (cblock-tx-raw b i)) :null)))
