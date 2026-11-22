(defpackage #:clempo.store
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode #:rlp-decode)
  (:import-from #:clempo.trie #:make-secure-trie #:trie-clone #:trie-put #:trie-delete
                #:trie-root #:trie-proof #:encode-account #:decode-account
                #:+empty-trie-root+ #:+empty-code-hash+
                #:trie-from-items #:node-store-get #:node-store-put
                #:node-store-write-through-p #:trie-map-memory-nodes)
  (:documentation "The committed state database: accounts, storage slots and code keyed by
integers, with the account and storage tries beside them so a block's state root costs only
its dirty paths. A block's writes are applied between DB-BEGIN-BLOCK and DB-COMMIT-BLOCK (or
undone by DB-REVERT-BLOCK).

A state database is either in RAM (MAKE-STATEDB: every account, slot and trie node in Lisp
tables, the database checkpoints and tests use) or backed by an on-disk store (kv.lisp,
opened by OPEN-STORE in persist.lisp): then the tables are a bounded cache over the store's
flat index (an account is one key, a slot one key, code one key) and the tries load their
nodes from it on demand. A backed database keeps every write since the last flush in RAM and
hands it to the store as one batch; between flushes, after a flush, it evicts what the cache
limits say.")
  (:export
   ;; accounts
   #:account #:account-nonce #:account-balance #:account-code-hash #:account-storage-root
   #:account-slot-count #:account-commitment
   ;; the state database
   #:statedb #:make-statedb #:db-number #:db-hash #:db-root-hash #:db-backed-p
   #:db-account #:db-storage #:db-code #:db-account-count
   #:map-accounts #:map-account-storage #:map-codes
   #:db-block-hash #:db-note-block-hash #:map-recent-hashes
   ;; writes
   #:db-set-account #:db-delete-account #:db-set-storage #:db-wipe-storage
   #:db-put-code #:db-state-root #:db-account-proof #:db-storage-proof
   #:db-account-trie #:db-storage-trie #:db-hashed-storage-trie
   #:db-begin-block #:db-commit-block #:db-revert-block #:db-block-open-p
   #:with-speculative-blocks
   ;; block diffs
   #:block-diff #:block-diff-number #:block-diff-hash #:block-diff-root
   #:block-diff-accounts #:block-diff-codes
   #:account-diff #:account-diff-address #:account-diff-deleted-p #:account-diff-wiped-p
   #:account-diff-nonce #:account-diff-balance #:account-diff-code-hash #:account-diff-slots
   #:account-diff-commitment
   #:make-account-diff #:make-block-diff #:db-apply-diff
   ;; cache limits
   #:cache-limits #:make-cache-limits #:db-cache-stats
   ;; persistence
   #:store #:store-db #:store-dir #:store-interval #:open-store #:close-store
   #:store-commit-block #:store-checkpoint #:store-sync #:store-flush #:store-durable-number
   #:store-sync-policy #:open-datadir-snapshot
   #:store-root-mismatch #:store-root-mismatch-number #:store-root-mismatch-expected
   #:store-root-mismatch-actual #:store-format-error #:store-locked #:store-locked-dir
   #:store-corrupt-log #:store-corrupt-log-path #:store-corrupt-log-offset
   #:write-checkpoint #:read-checkpoint #:encode-block-diff #:decode-block-diff
   #:code-hash-key #:address-octets #:slot-octets))

(in-package #:clempo.store)

(deftype hash32 () '(simple-array (unsigned-byte 8) (32)))

(declaim (inline address-octets slot-octets code-hash-key))
(defun address-octets (address) (integer->octets address 20))
(defun slot-octets (slot) (integer->octets slot 32))
(defun code-hash-key (hash)
  "Code table key of a code hash given as 32 octets or an integer."
  (if (integerp hash) hash (octets->integer hash)))

(define-constant +empty-code-key+ (octets->integer +empty-code-hash+))

(defstruct (account (:constructor %make-account (address)) (:copier nil))
  (address 0 :type integer :read-only t)
  (nonce 0 :type (unsigned-byte 64))
  (balance 0 :type integer)
  (code-hash +empty-code-hash+ :type hash32)
  ;; T14 (TIP-1108) configuration commitment; nonzero adds the account leaf's fifth string
  (commitment 0 :type (unsigned-byte 256))
  ;; The root of the storage once its trie is clean. A checkpoint load or a store read
  ;; restores it without the trie, which is built (in RAM) or opened (backed) when needed.
  (storage-root +empty-trie-root+ :type hash32)
  (slot-count 0 :type (integer 0))         ; nonzero slots
  ;; slot -> value. In RAM: every nonzero slot and nothing else. Backed: a cache over the
  ;; store's slots (zeros included) holding at least every slot written since the last flush.
  (storage nil :type (or null hash-table))
  ;; STORAGE holds every nonzero slot (always in RAM; backed after a wipe or for an account
  ;; with no storage), so a miss reads as zero without the store.
  (complete-p t)
  (trie nil)                               ; storage trie, NIL until first needed
  (dirty-slots '() :type list)             ; slots written since the trie was last updated
  (used nil)                               ; cache clock bit (backed)
  (owner nil))                             ; the backed database whose store holds the rest

;;; One undo record per account touched in the open block: the account object and its
;;; fields as they were, the first old value of each slot written before any wipe, and the
;;; storage table and trie objects to put back.
(defstruct (touch (:constructor make-touch (address account)))
  (address 0 :type integer)
  account                               ; object before the block, or NIL if absent
  (nonce 0) (balance 0) code-hash (commitment 0) storage-root storage trie
  (slot-count 0) (complete-p t)
  (old-slots (make-hash-table) :type hash-table) ; slot -> value before the block
  (written (make-hash-table) :type hash-table)   ; every slot written this block
  (wiped-p nil))

(defstruct (speculative-block (:constructor make-speculative-block
                                  (touched saved-trie new-codes number hash root recent
                                   account-total)))
  touched saved-trie new-codes number hash root recent account-total)

(defstruct (cache-limits (:constructor make-cache-limits
                             (&key (accounts (ash 1 20)) (slots (ash 1 23)) (trie-nodes (ash 1 21))
                                   (code-bytes (ash 256 20)) (dirty (ash 1 22)))))
  "Bounds of a backed state database's RAM, checked after each flush: cached ACCOUNTS, cached
SLOTS, decoded TRIE-NODES loaded from the store, CODE-BYTES of cached code. DIRTY is the
number of writes (accounts, slots and trie nodes) kept in RAM before the store flushes."
  (accounts 0 :type (integer 1)) (slots 0 :type (integer 1)) (trie-nodes 0 :type (integer 1))
  (code-bytes 0 :type (integer 1)) (dirty 0 :type (integer 1)))

(defstruct (flush-mark (:constructor make-flush-mark ()))
  "What a backed database must write for one account at the next flush."
  (wiped-p nil)                                  ; storage cleared (or account deleted) since
  (slots (make-hash-table) :type hash-table))    ; slots written since

(defun make-table (backed size)
  (declare (ignore backed))
  (make-hash-table :size size))

;;; Caches and threads. Only the database's writer (the thread that last opened a block, or
;;; opened the database) fills its caches; another thread reading a backed database reads
;;; through to the store without inserting anything. Readers and the writer never run at
;;; once (the RPC backend's lock), and concurrent readers of tables nobody changes are safe,
;;; so the hot tables need no locks.

(defstruct (statedb (:constructor %make-statedb
                        (backing nodes limits
                         &aux (accounts (make-table backing 4096))
                              (codes (make-table backing 1024))
                              (trie (make-secure-trie :store nodes))
                              (writer sb-thread:*current-thread*)))
                    (:conc-name db-))
  (writer nil)                                             ; the thread that fills the caches
  (backing nil)                                            ; NIL in RAM, else the KV store
  (nodes nil)                                              ; the tries' node store (backed)
  (limits nil)
  (accounts (make-hash-table) :type hash-table)            ; address -> account (backed: or NIL, absent)
  (codes (make-hash-table) :type hash-table)               ; hash integer -> octets
  (trie nil)                                               ; account trie
  (dirty (make-hash-table) :type hash-table)               ; address -> t (account trie stale)
  (number -1 :type integer)                                ; last committed block
  (hash nil)
  (root-hash nil)                                          ; root at NUMBER
  (recent (make-hash-table) :type hash-table)              ; number -> hash, last 256
  ;; the open block
  (open-p nil)
  (touched (make-hash-table) :type hash-table)             ; address -> touch
  (saved-trie nil)
  (new-codes '() :type list)
  ;; speculation: committed blocks that WITH-SPECULATIVE-BLOCKS will undo, newest first
  (speculation-depth 0 :type fixnum)
  (speculative '() :type list)
  ;; serializes proof readers' lazy storage-trie builds (DB-STORAGE-PROOF)
  (proof-lock (sb-thread:make-mutex :name "statedb proofs"))
  ;; in RAM: keccak(address) integer -> address, built by DB-ADDRESS-OF-HASH's first call
  (preimages nil)
  ;; backed: writes since the last flush, and cache bookkeeping
  (account-total 0 :type integer)                          ; accounts in the state
  (flush-accounts (make-hash-table) :type hash-table)      ; address -> flush-mark
  (flush-codes '() :type list)                             ; code keys new since the flush
  (flushed-root +empty-trie-root+)                         ; state root in the store
  (dirty-weight 0 :type fixnum)                            ; writes since the flush
  (cached-slots 0 :type fixnum)                            ; slot entries cached since eviction
  (code-bytes 0 :type fixnum))                             ; bytes of cached code

(declaim (inline filling-p))
(defun filling-p (db)
  "Whether this thread may insert into DB's caches."
  (eq (db-writer db) sb-thread:*current-thread*))

(defun make-statedb ()
  "An empty state database held in RAM."
  (%make-statedb nil nil nil))

(defun db-backed-p (db) (and (db-backing db) t))
(defun db-block-open-p (db) (db-open-p db))

(defun db-account-count (db)
  (if (db-backing db) (db-account-total db) (hash-table-count (db-accounts db))))

;;; Account records in the store: nonce, balance, code hash, storage root, commitment and
;;; slot count.

(defun encode-account-record (a)
  (let ((sink (make-sink nil 96)))
    (put-int sink (account-nonce a))
    (put-int sink (account-balance a))
    (put-raw sink (account-code-hash a))
    (put-raw sink (account-storage-root a))
    (put-int sink (account-commitment a))
    (put-int sink (account-slot-count a))
    (sink-octets sink)))

(defun decode-account-record (db address octets)
  (let ((src (make-source octets))
        (a (%make-account address)))
    (setf (account-owner a) db)
    (setf (account-nonce a) (get-int src)
          (account-balance a) (get-int src)
          (account-code-hash a) (get-raw src 32)
          (account-storage-root a) (get-raw src 32)
          (account-commitment a) (get-int src)
          (account-slot-count a) (get-int src))
    (unless (source-exhausted-p src) (format-error "trailing bytes in account ~40,'0X" address))
    (setf (account-complete-p a) (octets= (account-storage-root a) +empty-trie-root+))
    a))

(defun encode-slot-value (v) (integer->octets v (max 1 (ceiling (integer-length v) 8))))

;;; Reads

(defun load-account (db address)
  "ADDRESS's account from the store (NIL when absent), cached when this thread fills."
  (let* ((v (kv-get (db-backing db) (key-account address)))
         (a (when v (decode-account-record db address v))))
    (when (filling-p db)
      (when a (setf (account-used a) t))
      (setf (gethash address (db-accounts db)) a))
    a))

(declaim (inline db-account))
(defun db-account (db address)
  (multiple-value-bind (a present) (gethash address (db-accounts db))
    (cond (a (when (db-backing db) (setf (account-used a) t)) a)
          ((or present (null (db-backing db))) nil)
          (t (load-account db address)))))

(defun account-slot (db a slot)
  "The value of SLOT in account A (whose storage table caches the store when backed)."
  (let ((s (account-storage a)))
    (multiple-value-bind (v present) (if s (gethash slot s) (values nil nil))
      (cond (present v)
            ((account-complete-p a) 0)
            (t (let* ((o (kv-get (db-backing db) (key-slot (account-address a) slot)))
                      (v (if o (octets->integer o) 0)))
                 (when (filling-p db)
                   (setf (gethash slot (or (account-storage a)
                                           (setf (account-storage a) (make-table t 16))))
                         v)
                   (incf (db-cached-slots db)))
                 v))))))

(defun db-storage (db address slot)
  (let ((a (db-account db address)))
    (if a (account-slot db a slot) 0)))

(defun db-code (db hash)
  "Code octets for a code hash (32 octets or integer); the empty vector for the empty hash."
  (let ((key (code-hash-key hash)))
    (or (gethash key (db-codes db))
        (when (= key +empty-code-key+) (load-time-value (make-octets 0)))
        (let ((code (and (db-backing db) (kv-get (db-backing db) (key-code key)))))
          (when code
            (when (filling-p db)
              (incf (db-code-bytes db) (length code))
              (setf (gethash key (db-codes db)) code))
            code))
        (error "store: no code for hash ~X" key))))

;;; Iteration. A backed database iterates the store and overlays what RAM holds newer than
;;; it: the writes since the last flush and those of the open and speculative blocks.

(defun unflushed-addresses (db)
  "Addresses whose RAM state may differ from the store's."
  (let ((out (make-hash-table)))
    (flet ((add-touched (table) (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k out) t)) table)))
      (add-touched (db-flush-accounts db))
      (add-touched (db-touched db))
      (dolist (s (db-speculative db)) (add-touched (speculative-block-touched s))))
    out))

(defun map-accounts (fn db)
  "Call FN with (address account) for every account."
  (if (null (db-backing db))
      (maphash fn (db-accounts db))
      (let ((overlay (unflushed-addresses db)))
        (multiple-value-bind (lo hi) (key-range-accounts)
          (kv-map-range (db-backing db) lo hi
                        (lambda (key value)
                          (let ((address (key-address key)))
                            (unless (gethash address overlay)
                              (multiple-value-bind (a present) (gethash address (db-accounts db))
                                (let ((a (if present a (decode-account-record db address value))))
                                  (when a (funcall fn address a)))))))))
        (maphash (lambda (address flag)
                   (declare (ignore flag))
                   (let ((a (db-account db address))) (when a (funcall fn address a))))
                 overlay))))

(defun map-account-storage (fn account)
  "Call FN with (slot value) for every nonzero slot of ACCOUNT."
  (let ((s (account-storage account)))
    (cond ((account-complete-p account)
           (when s (maphash (lambda (slot v) (unless (zerop v) (funcall fn slot v))) s)))
          (t
           (multiple-value-bind (lo hi) (key-range-slots (account-address account))
             (kv-map-range (db-backing (account-owner account)) lo hi
                           (lambda (key value)
                             (let ((slot (key-slot-number key)))
                               (unless (and s (nth-value 1 (gethash slot s)))
                                 (funcall fn slot (octets->integer value)))))))
           (when s (maphash (lambda (slot v) (unless (zerop v) (funcall fn slot v))) s))))))

(defun map-codes (fn db)
  (if (null (db-backing db))
      (maphash fn (db-codes db))
      (let ((new (make-hash-table)))
        (dolist (k (db-flush-codes db)) (setf (gethash k new) t))
        (dolist (k (db-new-codes db)) (setf (gethash k new) t))
        (dolist (s (db-speculative db)) (dolist (k (speculative-block-new-codes s)) (setf (gethash k new) t)))
        (multiple-value-bind (lo hi) (key-range-codes)
          (kv-map-range (db-backing db) lo hi
                        (lambda (key code)
                          (let ((k (key-hash key)))
                            (remhash k new)
                            (funcall fn k code)))))
        (maphash (lambda (k flag) (declare (ignore flag)) (funcall fn k (db-code db k))) new))))

(defun db-block-hash (db number)
  "Hash of a recent block (the last 256 committed), or NIL."
  (gethash number (db-recent db)))

(defun db-note-block-hash (db number hash)
  (let ((recent (db-recent db)))
    (setf (gethash number recent) hash)
    (remhash (- number 256) recent)
    hash))

(defun map-recent-hashes (fn db) (maphash fn (db-recent db)))

;;; Storage tries

(defun build-storage-trie (account)
  "ACCOUNT's storage trie built in bulk from its slots (in parallel for a large account when
the caller bound a trie-hashing parallel map)."
  (let ((items (make-array (account-slot-count account))) (i 0))
    (map-account-storage (lambda (slot value) (setf (svref items i) (cons slot value)) (incf i))
                         account)
    (trie-from-items items (lambda (e) (values (slot-octets (car e)) (rlp-encode (cdr e))))
                     :secure t)))

(defun ensure-storage-trie (db account)
  (or (account-trie account)
      (setf (account-trie account)
            (if (db-backing db)
                (make-secure-trie :store (db-nodes db) :root (account-storage-root account))
                (let ((trie (build-storage-trie account)))
                  (invariant (octets= (trie-root trie) (account-storage-root account))
                             "storage of ~40,'0X does not hash to its recorded root" (account-address account))
                  trie)))))

(defun flush-storage (account)
  "Bring the storage trie up to date and return the storage root."
  (let ((slots (account-dirty-slots account)))
    (when slots
      (let ((trie (account-trie account))
            (storage (account-storage account)))
        (dolist (slot slots)
          (let ((v (if storage (gethash slot storage 0) 0)))
            (if (zerop v)
                (trie-delete trie (slot-octets slot))
                (trie-put trie (slot-octets slot) (rlp-encode v)))))
        (setf (account-dirty-slots account) '()
              (account-storage-root account) (trie-root trie)))))
  (account-storage-root account))

(defun db-state-root (db)
  "The state root of the db as it is now (including an open block)."
  (let ((dirty (db-dirty db)))
    (when (plusp (hash-table-count dirty))
      (let ((trie (db-trie db)))
        (maphash (lambda (address flag)
                   (declare (ignore flag))
                   (let ((a (db-account db address)))
                     (if a
                         (trie-put trie (address-octets address)
                                   (encode-account (account-nonce a) (account-balance a)
                                                   (flush-storage a) (account-code-hash a)
                                                   (account-commitment a)))
                         (trie-delete trie (address-octets address)))))
                 dirty)
        ;; DB-ROOT-HASH stays the last committed root; an open block's root is only returned.
        (clrhash dirty)))
    (trie-root (db-trie db))))

;;; Merkle proofs (eth_getProof) of the db as it is now: node RLP octets, root first.
;;; Concurrent readers (RPC requests under the backend's shared read lock) may ask for proofs
;;; of a committed db: its tries are clean, and proofs over a clean trie write nothing shared
;;; (clempo.trie publishes its node memo atomically). The one lazy fill, a storage trie a
;;; checkpoint load left unbuilt, happens under the db's proof lock.

(defun db-account-proof (db address)
  "The account trie proof for ADDRESS (an absence proof when it has no account)."
  (db-state-root db)
  (trie-proof (db-trie db) (address-octets address)))

(defun db-storage-proof (db address slot)
  "The proof for SLOT in ADDRESS's storage trie; NIL when ADDRESS has no account (its storage
root is the empty root, which has no nodes)."
  (let ((a (db-account db address)))
    (when a
      (db-state-root db)
      (let ((trie (clempo.lock:with-lock ((db-proof-lock db)) (ensure-storage-trie db a))))
        (if (octets= (trie-root trie) +empty-trie-root+)
            '()
            (trie-proof trie (slot-octets slot)))))))

(defun db-account-trie (db)
  "The account trie of the db as it is now, fully hashed (for read-only introspection)."
  (db-state-root db)
  (db-trie db))

(defun db-storage-trie (db address)
  "ADDRESS's storage trie as it is now, fully hashed; NIL when ADDRESS has no account."
  (let ((a (db-account db address)))
    (when a
      (db-state-root db)
      (let ((trie (clempo.lock:with-lock ((db-proof-lock db)) (ensure-storage-trie db a))))
        (trie-root trie)
        trie))))

(defun hashed-snapshot (trie)
  "A snapshot of the secure TRIE addressed by hashed keys (the 32-octet paths of its leaves)."
  (let ((snapshot (trie-clone trie)))
    (clempo.trie::%make-trie (clempo.trie::trie-root-node snapshot) (clempo.trie::trie-store snapshot) nil)))

(defun db-address-of-hash (db hash)
  "The address of the account in RAM whose keccak is HASH (32 octets), or NIL. In RAM the
preimage index is built on the first call, one pass over the accounts, and then kept by
LIVE-ACCOUNT as accounts are created; backed, only the unflushed accounts are looked at
\(the store holds the rest, tries included)."
  (let ((key (octets->integer hash)))
    (if (db-backing db)
        (loop for address being the hash-keys of (unflushed-addresses db)
              when (= key (octets->integer (keccak256 (integer->octets address 20)))) return address)
        (clempo.lock:with-lock ((db-proof-lock db))
          (gethash key (or (db-preimages db)
                           (let ((index (make-hash-table :size (max 16 (hash-table-count (db-accounts db))))))
                             (loop for address being the hash-keys of (db-accounts db)
                                   do (setf (gethash (octets->integer (keccak256 (integer->octets address 20))) index)
                                            address))
                             (setf (db-preimages db) index))))))))

(defun db-hashed-storage-trie (db hash)
  "The storage trie, as it is now and addressed by hashed slots, of the account whose
hashed address is HASH; NIL when DB has none. No scan of the store: an account the store
holds unchanged opens from the storage root in its account leaf."
  (db-state-root db)
  (let ((address (db-address-of-hash db hash)))
    (cond (address (let ((trie (db-storage-trie db address))) (and trie (hashed-snapshot trie))))
          ((db-backing db)
           (let ((leaf (clempo.trie:trie-get (hashed-snapshot (db-trie db)) hash)))
             (and leaf (clempo.trie:make-trie :store (db-nodes db)
                                              :root (nth-value 2 (decode-account leaf)))))))))

;;; Writes. Every write happens inside an open block so it can be undone and logged.

(defun db-begin-block (db)
  (invariant (not (db-open-p db)) "a block is already open")
  (setf (db-writer db) sb-thread:*current-thread*)
  ;; Clean tries make O(1) snapshots that are exactly the pre-block state.
  (db-state-root db)
  (setf (db-open-p db) t
        (db-saved-trie db) (trie-clone (db-trie db))
        (db-new-codes db) '())
  db)

(defun touch (db address)
  "Record ADDRESS's pre-block state the first time the open block writes it."
  (invariant (db-open-p db) "write outside a block")
  (let ((touched (db-touched db)))
    (or (gethash address touched)
        (let* ((a (db-account db address))
               (tc (make-touch address a)))
          (when a
            (let ((trie (account-trie a)))
              (setf (touch-nonce tc) (account-nonce a)
                    (touch-balance tc) (account-balance a)
                    (touch-code-hash tc) (account-code-hash a)
                    (touch-commitment tc) (account-commitment a)
                    (touch-storage-root tc) (account-storage-root a)
                    (touch-storage tc) (account-storage a)
                    (touch-slot-count tc) (account-slot-count a)
                    (touch-complete-p tc) (account-complete-p a)
                    (touch-trie tc) (when trie (trie-clone trie)))))
          (setf (gethash address touched) tc)))))

(defun note-preimage (db address)
  "Keep DB's preimage index, once built, covering ADDRESS."
  (let ((index (db-preimages db)))
    (when index
      (clempo.lock:with-lock ((db-proof-lock db))
        (setf (gethash (octets->integer (keccak256 (integer->octets address 20))) index) address)))))

(defun live-account (db address)
  (or (db-account db address)
      (let ((a (%make-account address)))
        (when (db-backing db) (setf (account-owner a) db (account-used a) t))
        (note-preimage db address)
        (setf (gethash address (db-accounts db)) a))))

(defun db-set-account (db address nonce balance code-hash &optional (commitment 0))
  "Create or overwrite ADDRESS's nonce, balance, code hash (32 octets) and T14 configuration
commitment (0 when it has none)."
  (touch db address)
  (let ((a (live-account db address)))
    (setf (account-nonce a) nonce
          (account-balance a) balance
          (account-code-hash a) code-hash
          (account-commitment a) commitment
          (gethash address (db-dirty db)) t)
    a))

(defun db-delete-account (db address)
  "Remove ADDRESS with its storage."
  (when (db-account db address)
    ;; A later write recreates the account with empty storage: the diff says wiped.
    (setf (touch-wiped-p (touch db address)) t)
    (if (db-backing db)
        (setf (gethash address (db-accounts db)) nil)
        (remhash address (db-accounts db)))
    (setf (gethash address (db-dirty db)) t)))

(defun db-wipe-storage (db address)
  "Clear ADDRESS's storage (a CREATE over an account that had storage, or a destroy)."
  (let ((tc (touch db address))
        (a (db-account db address)))
    (setf (touch-wiped-p tc) t)
    (when a
      (setf (account-storage a) nil
            (account-complete-p a) t
            (account-slot-count a) 0
            (account-trie a) (make-secure-trie :store (db-nodes db))
            (account-dirty-slots a) '()
            (account-storage-root a) +empty-trie-root+
            (gethash address (db-dirty db)) t))))

(defun db-set-storage (db address slot value)
  (let* ((tc (touch db address))
         (a (live-account db address))
         (old (account-slot db a slot))
         (backed (db-backing db))
         (storage (or (account-storage a)
                      (setf (account-storage a) (make-table backed 16)))))
    (ensure-storage-trie db a)
    (unless (touch-wiped-p tc)
      (multiple-value-bind (prior present) (gethash slot (touch-old-slots tc))
        (declare (ignore prior))
        (unless present (setf (gethash slot (touch-old-slots tc)) old))))
    (setf (gethash slot (touch-written tc)) t)
    (cond ((and (zerop value) (not backed)) (remhash slot storage))
          (t (when backed (incf (db-cached-slots db)))
             (setf (gethash slot storage) value)))
    (incf (account-slot-count a) (- (if (zerop value) 0 1) (if (zerop old) 0 1)))
    (push slot (account-dirty-slots a))
    (setf (gethash address (db-dirty db)) t)
    value))

(defun db-put-code (db code)
  "Store CODE and return its hash (32 octets)."
  (let* ((hash (keccak256 code))
         (key (octets->integer hash)))
    (unless (gethash key (db-codes db))
      (setf (gethash key (db-codes db)) code)
      (incf (db-code-bytes db) (length code))
      (when (db-open-p db) (push key (db-new-codes db))))
    hash))

;;; Block diffs: what a committed block changed, the unit of the log.

(defstruct (account-diff (:constructor make-account-diff
                             (&key address deleted-p wiped-p nonce balance code-hash
                                   (commitment 0) slots)))
  (address 0 :type integer)
  deleted-p wiped-p
  (nonce 0) (balance 0) code-hash
  (commitment 0)
  (slots '() :type list))              ; ((slot . value) ...), value 0 = cleared

(defstruct (block-diff (:constructor make-block-diff (&key number hash root accounts codes)))
  (number 0 :type integer) hash root
  (accounts '() :type list)
  (codes '() :type list))              ; code octets new in this block

(defun block-diff-from-touches (db number hash root)
  (let ((accounts '()))
    (maphash (lambda (address tc)
               (let ((a (db-account db address)))
                 (push (if a
                           (make-account-diff
                            :address address :wiped-p (touch-wiped-p tc)
                            :nonce (account-nonce a) :balance (account-balance a)
                            :code-hash (account-code-hash a) :commitment (account-commitment a)
                            :slots (loop for slot being the hash-keys of (touch-written tc)
                                         collect (cons slot (account-slot db a slot))))
                           (make-account-diff :address address :deleted-p t))
                       accounts)))
             (db-touched db))
    (make-block-diff :number number :hash hash :root root
                     :accounts (sort accounts #'< :key #'account-diff-address)
                     :codes (mapcar (lambda (k) (gethash k (db-codes db)))
                                    (reverse (db-new-codes db))))))

(defun close-block (db &key keep-touched)
  "Close the open block; KEEP-TOUCHED when a speculative record holds the touch table."
  (if keep-touched
      (setf (db-touched db) (make-hash-table))
      (clrhash (db-touched db)))
  (setf (db-open-p db) nil
        (db-saved-trie db) nil
        (db-new-codes db) '()))

(defun forget-codes (db keys)
  "Remove the codes first stored by an undone block (KEYS, code table keys)."
  (let ((codes (db-codes db)))
    (dolist (k keys) (remhash k codes))))

(defun copy-table (table)
  (let ((copy (make-hash-table :size (max 16 (hash-table-count table)))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun note-block-writes (db)
  "Fold the open block's writes into what a backed database flushes next, and its count of
accounts."
  (let ((marks (db-flush-accounts db)) (weight 0))
    (declare (type fixnum weight))
    (maphash (lambda (address tc)
               (let ((mark (or (gethash address marks) (setf (gethash address marks) (make-flush-mark))))
                     (now (db-account db address)))
                 (cond ((and (touch-account tc) (null now)) (decf (db-account-total db)))
                       ((and (null (touch-account tc)) now) (incf (db-account-total db))))
                 (when (touch-wiped-p tc) (setf (flush-mark-wiped-p mark) t))
                 (let ((slots (flush-mark-slots mark)))
                   (maphash (lambda (slot flag) (declare (ignore flag)) (setf (gethash slot slots) t) (incf weight))
                            (touch-written tc)))
                 (incf weight)))
             (db-touched db))
    (setf (db-flush-codes db) (append (db-new-codes db) (db-flush-codes db)))
    (incf (db-dirty-weight db) weight)))

(defun count-speculative-accounts (db)
  "The account total after the open block (speculative blocks keep their own count)."
  (maphash (lambda (address tc)
             (let ((now (db-account db address)))
               (cond ((and (touch-account tc) (null now)) (decf (db-account-total db)))
                     ((and (null (touch-account tc)) now) (incf (db-account-total db))))))
           (db-touched db)))

(defun db-commit-block (db number hash)
  "Close the open block as block NUMBER with HASH: compute its state root, remember the
hash for BLOCKHASH, and return the block's diff. Inside WITH-SPECULATIVE-BLOCKS the block's
undo record is kept so the speculation can be unwound."
  (invariant (db-open-p db) "no open block")
  (let* ((root (db-state-root db))
         (diff (block-diff-from-touches db number hash root))
         (speculative (plusp (db-speculation-depth db))))
    (when speculative
      (push (make-speculative-block (db-touched db) (db-saved-trie db) (db-new-codes db)
                                    (db-number db) (db-hash db)
                                    (db-root-hash db) (copy-table (db-recent db))
                                    (db-account-total db))
            (db-speculative db)))
    (when (db-backing db)
      (if speculative (count-speculative-accounts db) (note-block-writes db)))
    (close-block db :keep-touched speculative)
    (setf (db-number db) number (db-hash db) hash (db-root-hash db) root)
    (db-note-block-hash db number hash)
    diff))

(defun undo-touches (db touched saved-trie)
  "Put every account in TOUCHED back as it was before its block, and the account trie."
  (let ((accounts (db-accounts db))
        (backed (db-backing db)))
    (maphash
     (lambda (address tc)
       (let ((a (touch-account tc)))
         (if (null a)
             ;; absent before the block (a backed store may still hold an older version)
             (if backed (setf (gethash address accounts) nil) (remhash address accounts))
             (progn
               (setf (account-nonce a) (touch-nonce tc)
                     (account-balance a) (touch-balance tc)
                     (account-code-hash a) (touch-code-hash tc)
                     (account-commitment a) (touch-commitment tc)
                     (account-storage-root a) (touch-storage-root tc)
                     (account-storage a) (touch-storage tc)
                     (account-slot-count a) (touch-slot-count tc)
                     (account-complete-p a) (touch-complete-p tc)
                     (account-trie a) (touch-trie tc)
                     (account-dirty-slots a) '())
               (let ((s (account-storage a)))
                 ;; No table before the block means every recorded old value is zero (or,
                 ;; backed, uncached: the store still holds it).
                 (when s
                   (maphash (lambda (slot old)
                              (if (and (zerop old) (not backed))
                                  (remhash slot s)
                                  (setf (gethash slot s) old)))
                            (touch-old-slots tc))))
               (note-preimage db address)
               (setf (gethash address accounts) a)))))
     touched)
    (setf (db-trie db) saved-trie)
    (clrhash (db-dirty db))))

(defun db-revert-block (db)
  "Undo every write of the open block and close it."
  (invariant (db-open-p db) "no open block")
  (undo-touches db (db-touched db) (db-saved-trie db))
  (forget-codes db (db-new-codes db))
  (close-block db)
  db)

(defun unwind-speculation (db depth)
  "Undo the open block and every speculative block committed since the list had DEPTH entries."
  (when (db-open-p db) (db-revert-block db))
  (loop while (> (length (db-speculative db)) depth)
        do (let ((s (pop (db-speculative db))))
             (undo-touches db (speculative-block-touched s) (speculative-block-saved-trie s))
             (forget-codes db (speculative-block-new-codes s))
             (setf (db-number db) (speculative-block-number s)
                   (db-hash db) (speculative-block-hash s)
                   (db-root-hash db) (speculative-block-root s)
                   (db-recent db) (speculative-block-recent s)
                   (db-account-total db) (speculative-block-account-total s)))))

(defmacro with-speculative-blocks ((db) &body body)
  "Run BODY, in which blocks may be executed and committed on DB one after another (each
building on the last), then undo all of them, on any exit. Nothing committed inside reaches
the block log or the store unless BODY logs it itself. May nest."
  (let ((d (gensym "DB")) (depth (gensym "DEPTH")))
    `(let* ((,d ,db)
            (,depth (length (db-speculative ,d))))
       (invariant (not (db-open-p ,d)) "speculation must start between blocks")
       (incf (db-speculation-depth ,d))
       (unwind-protect (progn ,@body)
         (unwind-speculation ,d ,depth)
         (decf (db-speculation-depth ,d))))))

(defun db-apply-diff (db diff)
  "Apply a logged block DIFF as block (block-diff-number DIFF) and check its root.
Returns the root."
  (db-begin-block db)
  (dolist (code (block-diff-codes diff)) (db-put-code db code))
  (dolist (d (block-diff-accounts diff))
    (let ((address (account-diff-address d)))
      (cond ((account-diff-deleted-p d) (db-delete-account db address))
            (t (when (account-diff-wiped-p d) (db-wipe-storage db address))
               (db-set-account db address (account-diff-nonce d) (account-diff-balance d)
                                  (account-diff-code-hash d) (account-diff-commitment d))
               (loop for (slot . value) in (account-diff-slots d)
                     do (db-set-storage db address slot value))))))
  (let ((root (db-state-root db)))
    (db-commit-block db (block-diff-number diff) (block-diff-hash diff))
    root))
