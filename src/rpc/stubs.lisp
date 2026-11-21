(defpackage #:clempo.rpc.stubs
  (:use #:cl #:clempo.base #:clempo.rpc)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:eth #:clempo.rpc.eth))
  (:documentation "Methods the reference answers without chain data: reth's debug no-ops, the
admin, txpool, signing and bundle endpoints of a node with no local accounts or pool, and the
error paths of methods whose success needs a transaction pool.")
  (:export))

(in-package #:clempo.rpc.stubs)

;;; Parameter types of the Rust signatures (plain integers are JSON numbers there)

(defmacro def-json-integer (name rust-name bits signed)
  `(defparam ,name (src) ,(format nil "Rust ~A: a JSON number" rust-name)
     (serde:read-integer src ,(if signed (- (ash 1 (1- bits))) 0)
                         ,(if signed (1- (ash 1 (1- bits))) (1- (ash 1 bits)))
                         ,rust-name)))

(def-json-integer json-u64 "u64" 64 nil)
(def-json-integer json-i32 "i32" 32 t)

(defparam hex-u64 (src) "ruint U64 from a string: an 8 byte hex string"
  (if (eq (serde:peek-kind src) :string)
      (let* ((s (serde:read-string src)) (h (if (and (>= (length s) 2) (string-equal "0x" s :end2 2)) (subseq s 2) s)))
        (if (and (plusp (length h)) (<= (length h) 16) (every (lambda (c) (digit-char-p c 16)) h))
            (parse-integer h :radix 16)
            (serde:serde-fail-here src "invalid value: string ~A, expected a 8 byte hex string" (serde:rust-debug-string s))))
      (serde:invalid-type src "a 8 byte hex string")))

(defparam pagination (src) "Tempo PaginationParams"
  (serde:do-object (key src :expected "struct PaginationParams")
    (if (string= key "limit")
        (serde:read-integer src 0 +u64-max+ "usize")
        (serde:skip-value src)))
  t)

(defparam enode-url (src) "reth NodeRecord: enode://<id>@<ipv4>:<port>"
  (let* ((s (if (eq (serde:peek-kind src) :string) (serde:read-string src) (serde:invalid-type src "a string")))
         (at (position #\@ s))
         (host (subseq s (if at (1+ at) (min (length s) 8)) (or (position #\: s :start (or at 8)) (length s)))))
    (unless (and (= 4 (length (split host #\.))) (every (lambda (p) (and (plusp (length p)) (every #'digit-char-p p))) (split host #\.)))
      (serde:serde-fail-here src "Failed to parse url: invalid IPv4 address syntax"))
    s))

(defparam any-node (src) "reth AnyNode: an enode URL or a peer id"
  (let* ((s (if (eq (serde:peek-kind src) :string) (serde:read-string src) (serde:invalid-type src "a string")))
         (body (if (and (> (length s) 8) (string= "enode://" s :end2 8)) (subseq s 8) s))
         (id (subseq body 0 (or (position #\@ body) (length body)))))
    (unless (and (= (length id) 128) (every (lambda (c) (digit-char-p c 16)) id))
      (serde:serde-fail-here src "invalid public key: ~A" id))
    s))

(defun split (s ch)
  (loop with start = 0
        for pos = (position ch s :start start)
        collect (subseq s start pos)
        while pos do (setf start (1+ pos))))

;;; reth's debug no-ops

(defmacro defnull (name (&rest params) doc)
  `(defrpc ,name ,params ,doc nil :null))

(defnull "debug_chaindbCompact" () "reth no-op")
(defnull "debug_dbAncients" () "reth no-op")
(defnull "debug_freeOSMemory" () "reth no-op")
(defnull "debug_gcStats" () "reth no-op")
(defnull "debug_memStats" () "reth no-op")
(defnull "debug_chaindbProperty" ((property string)) "reth no-op")
(defnull "debug_dbAncient" ((kind string) (number json-u64)) "reth no-op")
(defnull "debug_printBlock" ((number json-u64)) "reth no-op")
(defnull "debug_setGCPercent" ((percent json-i32)) "reth no-op")
(defnull "debug_setHead" ((number hex-u64)) "reth no-op")
(defnull "debug_setTrieFlushInterval" ((interval string)) "reth no-op")
(defnull "debug_getModifiedAccountsByNumber" ((start json-u64) (end json-u64)) "reth no-op")
(defnull "debug_getModifiedAccountsByHash" ((start hash) (end hash)) "reth no-op")
(defnull "debug_preimage" ((hash hash)) "reth no-op")
(defnull "debug_dumpBlock" ((block block-number-or-tag)) "reth no-op")
(defnull "debug_getAccessibleState" ((from block-number-or-tag) (to block-number-or-tag)) "reth no-op")
(defnull "debug_accountRange" ((block any) (start any) (max any) (no-code any) (no-storage any) (incompletes any))
  "reth no-op")
(defnull "debug_storageRangeAt" ((block-hash hash) (tx-index any) (address address) (start hash) (max any))
  "reth no-op")
(defnull "debug_standardTraceBlockToFile" ((hash hash) (options any :optional t)) "reth no-op")
(defnull "debug_standardTraceBadBlockToFile" ((hash hash) (options any :optional t)) "reth no-op")

(defrpc "debug_seedHash" ((number json-u64)) "reth: the zero hash"
  (integer->hex 0 :bytes 32))
(defrpc "debug_getBadBlocks" () "no bad blocks are kept" #())
(defrpc "debug_traceBadBlock" ((hash hash) (options any :optional t)) "no bad blocks are kept"
  (internal-error "bad block not found in cache"))

(defrpc "debug_dbGet" ((key bytes)) "reth: only 0x63-prefixed code keys"
  (cond ((/= (length key) 33) (rpc-fail +invalid-params+ (format nil "Key must be 33 bytes, got ~D" (length key))))
        ((/= (aref key 0) #x63) (rpc-fail +invalid-params+ "Key prefix must be 0x63"))
        (t (internal-error "code store unavailable"))))

;;; A node with no local accounts and no transaction pool

(defun no-pool () (internal-error "transaction pool unavailable"))

(defun decode-or-fail (data)
  (when (zerop (length data)) (rpc-fail +invalid-params+ "empty transaction data"))
  (handler-case (clempo.transactions:decode-transaction data)
    (error () (rpc-fail +invalid-params+ "failed to decode signed transaction"))))

(defrpc "eth_sendRawTransaction" ((data bytes)) "decoded and refused: no pool" (decode-or-fail data) (no-pool))
(defrpc "eth_sendRawTransactionSync" ((data bytes)) "decoded and refused: no pool" (decode-or-fail data) (no-pool))
(defrpc "trace_rawTransaction" ((data bytes) (types any)) "decode, then trace"
  (decode-or-fail data) (internal-error "tracing unavailable"))
(defrpc "eth_sign" ((address address) (data bytes)) "no local accounts" (rpc-fail +invalid-params+ "unknown account"))
(defrpc "eth_sendTransaction" ((request any)) "no local accounts" (rpc-fail +invalid-params+ "unknown account"))
(defrpc "eth_signTransaction" ((request any)) "no local accounts" (rpc-fail +invalid-params+ "unknown account"))

(defparam typed-data (src) "alloy StrOrVal<TypedData>"
  (case (serde:peek-kind src)
    (:string (let ((s (serde:read-string src)))
               (handler-case (json:parse-json s)
                 (error () (serde:serde-fail "expected value at line 1 column 1")))
               s))
    (t (serde:skip-value src) (serde:serde-fail "data did not match any variant of untagged enum StrOrVal"))))

(defrpc "eth_signTypedData" ((address address) (data typed-data)) "no local accounts"
  (rpc-fail +invalid-params+ "unknown account"))

(defrpc "eth_callBundle" ((bundle any)) "bundles need at least one transaction"
  (if (zerop (length (json:jget bundle "txs" #())))
      (rpc-fail +invalid-params+ "bundle missing txs")
      (internal-error "bundle simulation unavailable")))

(defrpc "eth_pendingTransactions" () "the pool is empty" #())
(defrpc "txpool_status" () "the pool is empty" (json:jobject "pending" "0x0" "queued" "0x0"))
(defrpc "txpool_content" () "the pool is empty" (json:jobject "pending" (json:jobject) "queued" (json:jobject)))
(defrpc "txpool_inspect" () "the pool is empty" (json:jobject "pending" (json:jobject) "queued" (json:jobject)))
(defrpc "txpool_contentFrom" ((address address)) "the pool is empty"
  (json:jobject "pending" (json:jobject) "queued" (json:jobject)))

;;; Tempo extensions the reference leaves unimplemented

(defrpc "eth_getTransactions" ((params pagination)) "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "token_getTokens" ((params pagination)) "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "token_getTokensByAddress" ((params pagination)) "unimplemented in the reference" (internal-error "unimplemented"))
(defrpc "token_getRoleHistory" ((params pagination)) "unimplemented in the reference" (internal-error "unimplemented"))

;;; admin: no peers; subscriptions need a websocket

(defrpc "admin_peers" () "no peers" #())
(defrpc "admin_validatorKey" () "not a validator" :null)
(defrpc "admin_addPeer" ((enode enode-url)) "accepted" :true)
(defrpc "admin_addTrustedPeer" ((enode enode-url)) "accepted" :true)
(defrpc "admin_removePeer" ((node any-node)) "accepted" :true)
(defrpc "admin_removeTrustedPeer" ((node any-node)) "accepted" :true)

(dolist (name '("admin_peerEvents" "reth_subscribeChainNotifications" "reth_subscribePersistedBlock"
                "eth_subscribe" "eth_unsubscribe"))
  (setf (gethash name *methods*)
        (clempo.rpc::make-rpc-method :name name :doc "a subscription: HTTP callers get Internal error"
                                     :function (lambda (src) (declare (ignore src)) (internal-error)))))
