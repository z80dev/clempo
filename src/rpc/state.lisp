(defpackage #:clempo.rpc.state
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:store #:clempo.store) (#:trie #:clempo.trie) (#:state #:clempo.state)
                    (#:history #:clempo.history))
  (:documentation "The eth and debug methods that read the world state at a block: code,
storage, nonces, accounts, account info and Merkle proofs (reth's EthState over a state
provider). A block id selects a state the way reth's state_by_block_id does; the state itself
comes from the backend (clempo.rpc.chain:backend-state): the head's state database or a
clempo.history view of an older block, both read through the clempo.state backend generics.
A state it does not keep is answered with an internal error naming the block, never a guess.")
  (:export #:*eth-proof-window* #:block-id-number #:block-id-text #:header-not-found
           #:state-at #:state-block #:state-unavailable #:ensure-within-proof-window
           #:replay-coverage #:storage-key #:storage-key-value #:landed-function))

(in-package #:clempo.rpc.state)

(defparameter *eth-proof-window* 0
  "reth's eth_proof_window (DEFAULT_ETH_PROOF_WINDOW): how many blocks below the head
eth_getProof, eth_getMultiProof and eth_getAccount still answer.")

(defconstant +max-proof-slots+ 1024 "reth DEFAULT_MAX_PROOF_SLOTS")
(defconstant +max-storage-values-slots+ 1024 "reth DEFAULT_MAX_STORAGE_VALUES_SLOTS")

(defun landed-function (package name what)
  "The function PACKAGE::NAME, which another layer is adding; until it lands, WHAT fails
with an internal error saying so."
  (let* ((p (find-package package))
         (s (and p (find-symbol name p))))
    (if (and s (fboundp s))
        (symbol-function s)
        (internal-error (format nil "~A is not available: ~A:~A has not landed yet"
                                what package (string-downcase name))))))

;;; Block ids (as clempo.rpc's BLOCK-ID reader returns them) and their states

(defun hash-id-p (id) (and (consp id) (eq (first id) :hash)))

(defun block-id-text (id)
  "alloy's Display of a BlockId, as reth's \"block not found\" error prints it."
  (cond ((hash-id-p id)
         (format nil "~:[~;canonical ~]hash ~A" (getf (cddr id) :require-canonical) (hex32 (second id))))
        ((integerp id) (q id))
        (t (string-downcase (string id)))))

(defun header-not-found (id)
  (rpc-fail -32001 (format nil "block not found: ~A" (block-id-text id))))

(defun block-id-number (id)
  "The block number ID selects (NIL for an unknown hash); tags follow clempo.rpc.eth, numbers
above the head are returned as they are."
  (cond ((null id) chain:*head*)
        ((hash-id-p id) (chain:backend-block-number chain:*backend* (second id)))
        ((integerp id) id)
        (t (case id
             ((:latest :safe :finalized :pending) chain:*head*)
             (:earliest 0)))))

(define-condition state-unavailable (rpc-error) ()
  (:documentation "The backend keeps no state for the requested block."))

(defun state-unavailable (number)
  (error 'state-unavailable
         :code +internal-error+
         :message (format nil "state at block ~D is not available: this node keeps only the state of its latest block"
                          number)))

;;; The first blocks' states, rebuilt from genesis. Early mainnet blocks are nearly empty, so a
;;; state the backend does not keep near genesis is recomputed by executing the archived blocks
;;; on the genesis allocation, each block checked against its header (state root included).

(defparameter *replay-history-limit* 1024
  "Blocks up to this number may be rebuilt from genesis when the backend keeps no state.")

(defvar *replayed-states* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "backend -> recently rebuilt (number . statedb), newest first; the databases are read-only.")
(defvar *replay-lock* (sb-thread:make-mutex :name "replayed states"))

(defun replay-chain ()
  "The executor chain whose genesis the current backend starts from, or NIL."
  (let ((chain clempo.executor:*chain*))
    (when (eql (chain:backend-chain-id chain:*backend*)
               (clempo.chainspec:chain-id (clempo.chainspec:load-chainspec chain)))
      chain)))

(defun replay-coverage ()
  "The last block whose state the current backend can rebuild from genesis (STATE-AT's
fallback for 0 up to it), or NIL when it cannot rebuild any."
  (when (and (replay-chain) (chain:backend-block-raw chain:*backend* 0))
    (min *replay-history-limit* chain:*head*)))

(defun rebuild-state (n chain)
  "A fresh state database after block N of CHAIN: genesis, then blocks 1..N from the backend."
  (let ((db (clempo.executor:genesis-db :chain chain)))
    (loop for k from 1 to n
          do (let* ((raw (or (chain:backend-block-raw chain:*backend* k) (return-from rebuild-state nil)))
                    (block (clempo.blocks:decode-block raw))
                    (header (clempo.blocks:block-header block))
                    (result (clempo.executor:execute-block db block :chain chain))
                    (bad (clempo.executor:block-mismatches result header)))
               (when bad
                 (store:db-revert-block db)
                 (internal-error (format nil "state at block ~D is not available: re-executing block ~D from genesis disagrees with its header (~(~A~))"
                                         n k (first (first bad)))))
               (store:db-commit-block db k (clempo.blocks:block-hash block))))
    db))

(defun replayed-state (n)
  "The state after block N rebuilt from genesis, or NIL when N is beyond the replay limit."
  (let ((chain (and (<= n *replay-history-limit*) (replay-chain))))
    (when chain
      (sb-thread:with-mutex (*replay-lock*)
        (let ((recent (gethash chain:*backend* *replayed-states*)))
          (or (cdr (assoc n recent))
              (let ((db (rebuild-state n chain)))
                (when db
                  (setf (gethash chain:*backend* *replayed-states*)
                        (subseq (cons (cons n db) recent) 0 (min 8 (1+ (length recent))))))
                db)))))))

(defun state-block (id)
  "The number of the block whose state ID selects; an unknown block fails with \"block not
found\" as reth's state_by_block_id does."
  (let ((n (block-id-number id)))
    (when (or (null n) (> n chain:*head*)) (header-not-found (or id :latest)))
    n))

(defun state-at (id)
  "The state after the block ID selects (see STATE-BLOCK), a clempo.state backend: the
head's state database or a historical view. A known block the backend keeps no state for, and
that is too far from genesis to rebuild, fails with STATE-UNAVAILABLE.
Returns (values state number)."
  (let ((n (state-block id)))
    (values (or (chain:backend-state chain:*backend* n) (replayed-state n) (state-unavailable n)) n)))

(defun ensure-within-proof-window (id)
  "reth ensure_within_proof_window: the block's distance below the head must not exceed the
proof window."
  (let ((n (block-id-number id)))
    (unless n (header-not-found id))
    (when (> (- chain:*head* (min n chain:*head*)) *eth-proof-window*)
      (rpc-fail 4444 "distance to target block exceeds maximum proof window"))))

;;; Reading a state: a state database or a historical view, through the backend generics.

(defun account-fields (state address)
  "(values exists nonce balance code-hash-octets storage-root-octets); the storage root only
from a state database (proofs and eth_getAccount answer within the proof window, at the head)."
  (multiple-value-bind (exists nonce balance code-hash) (state:backend-account state address)
    (values exists nonce balance (if exists code-hash trie:+empty-code-hash+)
            (cond ((not exists) trie:+empty-trie-root+)
                  ((typep state 'store:statedb) (store:account-storage-root (store:db-account state address)))
                  (t (internal-error "storage roots are kept only for the head state"))))))

(defun account-basics (state address)
  "(values exists nonce balance code-hash-octets)"
  (multiple-value-bind (exists nonce balance code-hash) (state:backend-account state address)
    (values exists nonce balance (if exists code-hash trie:+empty-code-hash+))))

(defun account-code (state address)
  (state:backend-code state (nth-value 3 (account-basics state address))))

(defun code-by-hash (state hash)
  "The code whose hash is the octets HASH, or NIL. Code is never removed from the store, as
reth's Bytecodes table, so the answer does not depend on the block."
  (cond ((octets= hash trie:+empty-code-hash+) (make-octets 0))
        ((typep state 'history:history-state)
         (history:history-code-by-hash (history:history-state-history state) hash))
        (t (handler-case (store:db-code state hash) (simple-error () nil)))))

;;; Storage keys: alloy-serde JsonStorageKey

(defstruct (storage-key (:constructor make-storage-key (value full-p)))
  "A slot as the caller wrote it: FULL-P (63 or 64 digits) echoes as 32 bytes, otherwise as a
quantity."
  (value 0 :type integer) full-p)

(defun storage-key-json (k)
  (if (storage-key-full-p k)
      (integer->hex (storage-key-value k) :bytes 32)
      (q (storage-key-value k))))

(defparameter +storage-key-expected+ "a hex string of at most 64 digits")

(defun parse-storage-key-string (src s)
  (let ((digits (if (and (>= (length s) 2) (char= (char s 0) #\0) (char-equal (char s 1) #\x)) (subseq s 2) s)))
    (cond ((> (length digits) 64)
           (serde:serde-fail-here src "invalid length ~D, expected ~A" (length digits) +storage-key-expected+))
          ((notevery (lambda (c) (digit-char-p c 16)) digits)
           (serde:serde-fail-here src "invalid value: string ~A, expected ~A"
                                  (serde:rust-debug-string s) +storage-key-expected+))
          (t (make-storage-key (if (zerop (length digits)) 0 (parse-integer digits :radix 16))
                               (>= (length digits) 63))))))

(defparam storage-key (src) "alloy-serde JsonStorageKey: up to 64 hex digits (0x optional) or a u64"
  (case (serde:peek-kind src)
    (:string (parse-storage-key-string src (serde:read-string src)))
    (:number (let ((n (serde:read-number src)))
               (if (and (integerp n) (<= 0 n +u64-max+))
                   (make-storage-key n nil)
                   (serde:serde-fail-here src "invalid type: ~A, expected ~A" (serde:number-unexpected n)
                                          +storage-key-expected+))))
    (t (serde:invalid-type src +storage-key-expected+))))

(defparam storage-keys (src) "Vec<JsonStorageKey>"
  (let ((keys '()))
    (serde:do-array (i src) (push (read-param 'storage-key src) keys))
    (nreverse keys)))

(defun parse-address-key (src s)
  "An Address map key (FixedBytes<20> from a str), failing after the key as serde_json does."
  (let ((h (if (and (>= (length s) 2) (char= (char s 0) #\0) (char-equal (char s 1) #\x)) (subseq s 2) s)))
    (cond ((oddp (length h)) (serde:serde-fail-after-key src "odd number of digits"))
          ((/= (length h) 40) (serde:serde-fail-after-key src "invalid string length"))
          (t (let ((bad (position-if-not (lambda (c) (digit-char-p c 16)) h)))
               (when bad
                 (serde:serde-fail-after-key src "invalid character '~A' at position ~D" (string (char h bad)) bad))
               (parse-integer h :radix 16))))))

(defparam storage-requests (src) "HashMap<Address, Vec<JsonStorageKey>>: ((address . keys) ...)"
  (let ((out '()))
    (serde:do-object (key src)
      (let ((a (parse-address-key src key)))
        (setf out (cons (cons a (read-param 'storage-keys src)) (remove a out :key #'car)))))
    (nreverse out)))

(defparam proof-targets (src) "Vec<(Address, Vec<B256>)>: ((address . slot-octets-list) ...)"
  (let ((out '()))
    (serde:do-array (i src)
      (unless (eq (serde:peek-kind src) :array) (serde:invalid-type src "a tuple of size 2"))
      (let ((address nil) (slots '()) (n 0))
        (serde:do-array (j src :expected "a tuple of size 2")
          (case j
            (0 (setf address (read-param 'address src)))
            (1 (serde:do-array (k src) (push (read-param 'hash src) slots)))
            (t (serde:peek src)
               (incf (serde:src-pos src))
               (serde:serde-fail-here src "trailing characters")))
          (setf n (1+ j)))
        (when (< n 2) (serde:serde-fail-here src "invalid length ~D, expected a tuple of size 2" n))
        (push (cons address (nreverse slots)) out)))
    (nreverse out)))

;;; Proofs: the store's proof readers over the committed tries. An archive backend's head
;;; state is a flat history view; its tries come from clempo.rpc.tries.

(defun call-with-trie-state-at (id fn)
  "Call FN with a state database with tries after block ID: the backend's own, the head's
tries, or the backend's trie cursor moved to an earlier block (checked against its header's
state root) and held for the call."
  (multiple-value-bind (state n) (state-at id)
    (cond ((typep state 'store:statedb) (funcall fn state))
          ((= n chain:*head*) (funcall fn (clempo.rpc.tries:head-trie-state)))
          (t (clempo.rpc.tries:call-with-trie-state n fn)))))

(defmacro with-trie-state-at ((db id) &body body)
  `(call-with-trie-state-at ,id (lambda (,db) ,@body)))

(defun proof-db (state)
  (if (typep state 'store:statedb)
      state
      (internal-error "Merkle proofs are served for the head state only")))

(defun account-proof (state address) (store:db-account-proof (proof-db state) address))

(defun storage-proofs (state address slots)
  (mapcar (lambda (slot) (store:db-storage-proof (proof-db state) address slot)) slots))

(defun proof-json (db address keys)
  "alloy EIP1186AccountProofResponse for ADDRESS with storage KEYS (storage-key structs)."
  (multiple-value-bind (exists nonce balance code-hash storage-root) (account-fields db address)
    ;; The reference returns zero code and storage hashes for an absent account.
    (let ((proofs (storage-proofs db address (mapcar #'storage-key-value keys)))
          (nodes (lambda (list) (map 'simple-vector #'hexbytes list))))
      (json:jobject "address" (addr address)
                    "balance" (q balance)
                    "codeHash" (if exists (hex32 code-hash) (hex32 (make-octets 32)))
                    "nonce" (q nonce)
                    "storageHash" (if exists (hex32 storage-root) (hex32 (make-octets 32)))
                    "accountProof" (funcall nodes (account-proof db address))
                    "storageProof"
                    (map 'simple-vector
                         (lambda (k proof)
                           (json:jobject "key" (storage-key-json k)
                                         "value" (q (state:backend-storage db address (storage-key-value k)))
                                         "proof" (funcall nodes proof)))
                         keys proofs)))))

(defun slot-hex (state address slot) (integer->hex (state:backend-storage state address slot) :bytes 32))

;;; Methods

(defrpc "eth_getCode" ((address address) (block block-id :optional t)) "code of an account"
  (hexbytes (account-code (state-at block) address)))

(defrpc "eth_getStorageAt" ((address address) (key storage-key) (block block-id :optional t))
  "one storage slot"
  (slot-hex (state-at block) address (storage-key-value key)))

(defrpc "eth_getTransactionCount" ((address address) (block block-id :optional t))
  "the account nonce (the pool is empty, so pending adds nothing)"
  (q (nth-value 1 (account-basics (state-at block) address))))

(defrpc "eth_getStorageValues" ((requests storage-requests) (block block-id :optional t))
  "several slots of several accounts"
  (when (null requests) (rpc-fail +invalid-params+ "empty request"))
  (let ((total (loop for r in requests sum (length (cdr r)))))
    (when (> total +max-storage-values-slots+)
      (rpc-fail +invalid-params+ (format nil "total slot count ~D exceeds limit ~D" total +max-storage-values-slots+))))
  (let ((db (state-at block)))
    (cons :object
          (loop for (address . keys) in requests
                collect (cons (addr address)
                              (map 'simple-vector (lambda (k) (slot-hex db address (storage-key-value k))) keys))))))

(defrpc "eth_getAccount" ((address address) (block block-id)) "nonce, balance, storage root and code hash"
  (ensure-within-proof-window block)
  (with-trie-state-at (db block)
    (multiple-value-bind (exists nonce balance code-hash storage-root) (account-fields db address)
      (if exists
          (json:jobject "nonce" (q nonce) "balance" (q balance)
                        "storageRoot" (hex32 storage-root) "codeHash" (hex32 code-hash))
          :null))))

(defrpc "eth_getAccountInfo" ((address address) (block block-id)) "balance, nonce and code"
  (let ((db (state-at block)))
    (multiple-value-bind (exists nonce balance) (account-basics db address)
      (declare (ignore exists))
      (json:jobject "balance" (q balance) "nonce" (q nonce) "code" (hexbytes (account-code db address))))))

(defrpc "eth_getProof" ((address address) (keys storage-keys) (block block-id :optional t))
  "EIP-1186 account and storage proofs"
  (when (> (length keys) +max-proof-slots+)
    (rpc-fail +invalid-params+ (format nil "storage key count ~D exceeds limit ~D" (length keys) +max-proof-slots+)))
  (ensure-within-proof-window block)
  (with-trie-state-at (db block) (proof-json db address keys)))

(defrpc "eth_getMultiProof" ((targets proof-targets) (block block-id :optional t))
  "EIP-1186 proofs for several accounts"
  (when (> (length targets) +max-proof-slots+)
    (rpc-fail +invalid-params+ (format nil "target count ~D exceeds limit ~D" (length targets) +max-proof-slots+)))
  (let ((total (loop for x in targets sum (length (cdr x)))))
    (when (> total +max-proof-slots+)
      (rpc-fail +invalid-params+ (format nil "total slot count ~D exceeds limit ~D" total +max-proof-slots+))))
  (ensure-within-proof-window block)
  ;; No targets: the answer is empty whatever the state holds.
  (if (null targets)
      (progn (state-block block) #())
      (with-trie-state-at (db block)
        (map 'simple-vector
             (lambda (target)
               (proof-json db (car target)
                           (mapcar (lambda (slot) (make-storage-key (octets->integer slot) t)) (cdr target))))
             targets))))

(defrpc "debug_codeByHash" ((hash hash) (block block-id :optional t)) "code by its hash"
  ;; The empty code's hash names the empty code whatever the state holds.
  (let ((code (if (octets= hash trie:+empty-code-hash+)
                  (progn (state-block block) (make-octets 0))
                  ;; Codes are never removed: the history's code table answers for any block.
                  ;; A code it lacks may have been deployed after the blocks it covers, so
                  ;; without a state for the block that is not an answer of null.
                  (let* ((n (state-block block))
                         (h (and (typep chain:*backend* 'chain:archive-backend)
                                 (chain:backend-history chain:*backend*)))
                         (state (chain:backend-state chain:*backend* n)))
                    (cond (state (code-by-hash state hash))
                          ((and h (history:history-code-by-hash h hash)))
                          (t (code-by-hash (state-at block) hash)))))))
    (if code (hexbytes code) :null)))

;;; This node: admin_nodeInfo describes clempo itself. Its devp2p identity is a secp256k1 key
;;; made for the process (as reth makes one for a datadir without a key file); the RPC server
;;; runs no devp2p listener, so it reports loopback with ports 0.

(defvar *node-key* nil "This process's devp2p secret key, made on first use.")
(defvar *node-key-lock* (sb-thread:make-mutex :name "node key"))

(defun node-key ()
  (sb-thread:with-mutex (*node-key-lock*)
    (or *node-key*
        (setf *node-key* (1+ (clempo.io:random-below (1- clempo.secp256k1:+secp256k1-n+)))))))

(defun compressed-key (public)
  (let ((y (octets->integer public :start 32 :end 64)))
    (octets-concat (octets-of (if (evenp y) 2 3)) (subseq public 0 32))))

(defun base64url (octets)
  (let ((s (clempo.websocket:base64-encode octets)))
    (substitute #\_ #\/ (substitute #\- #\+ (string-right-trim "=" s)))))

(defun node-enr (key public ip tcp udp)
  "EIP-778 record (sequence 1, identity scheme v4) signed with KEY."
  (let* ((pairs (list (string->octets "id") (string->octets "v4")
                      (string->octets "ip") ip
                      (string->octets "secp256k1") (compressed-key public)
                      (string->octets "tcp") tcp
                      (string->octets "udp") udp))
         (content (cons 1 pairs)))
    (multiple-value-bind (r s) (clempo.secp256k1:sign (clempo.keccak:keccak256 (clempo.rlp:rlp-encode content)) key)
      (concatenate 'string "enr:"
                   (base64url (clempo.rlp:rlp-encode
                               (cons (octets-concat (integer->octets r 32) (integer->octets s 32)) content)))))))

(defun chain-config-json ()
  "debug_chainConfig's answer, the chain config reth reports in the node's eth protocol info."
  (funcall (clempo.rpc::rpc-method-function (find-method* "debug_chainConfig")) nil))

(defrpc "admin_nodeInfo" () "clempo's own node record"
  (let* ((key (node-key))
         (public (clempo.secp256k1:private->public key))
         (ip "127.0.0.1")
         (genesis (chain-block 0))
         (head (chain-block chain:*head*)))
    (json:jobject "id" (octets->hex (clempo.keccak:keccak256 public) :prefix nil)
                  "name" clempo.rpc.eth:*client-version*
                  "enode" (format nil "enode://~A@~A:0" (octets->hex public :prefix nil) ip)
                  "enr" (node-enr key public (octets-of 127 0 0 1) 0 0)
                  "ip" ip
                  "ports" (json:jobject "discovery" 0 "listener" 0)
                  "listenAddr" (format nil "~A:0" ip)
                  "protocols" (json:jobject
                               "eth" (json:jobject "network" (chain:backend-chain-id chain:*backend*)
                                                   "difficulty" :null
                                                   "genesis" (if genesis (hex32 (cblock-hash genesis)) :null)
                                                   "config" (chain-config-json)
                                                   "head" (if head (hex32 (cblock-hash head)) :null))))))
