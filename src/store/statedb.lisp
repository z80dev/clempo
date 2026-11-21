(defpackage #:clempo.store
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode)
  (:import-from #:clempo.trie #:make-secure-trie #:trie-clone #:trie-put #:trie-delete
                #:trie-root #:trie-proof #:encode-account #:+empty-trie-root+ #:+empty-code-hash+
                #:trie-from-items)
  (:documentation "The committed state database: every account, storage slot and code blob
of the chain in RAM, keyed by integers, with the account and storage tries kept beside them so
a block's state root costs only its dirty paths. A block's writes are applied between
DB-BEGIN-BLOCK and DB-COMMIT-BLOCK (or undone by DB-REVERT-BLOCK); persist.lisp turns
committed blocks into a log and periodic checkpoints.")
  (:export
   ;; accounts
   #:account #:account-nonce #:account-balance #:account-code-hash #:account-storage-root
   #:account-slot-count
   ;; the state database
   #:statedb #:make-statedb #:db-number #:db-hash #:db-root-hash
   #:db-account #:db-storage #:db-code #:db-account-count
   #:map-accounts #:map-account-storage #:map-codes
   #:db-block-hash #:db-note-block-hash #:map-recent-hashes
   ;; writes
   #:db-set-account #:db-delete-account #:db-set-storage #:db-wipe-storage
   #:db-put-code #:db-state-root #:db-account-proof #:db-storage-proof
   #:db-account-trie #:db-storage-trie
   #:db-begin-block #:db-commit-block #:db-revert-block #:db-block-open-p
   #:with-speculative-blocks
   ;; block diffs
   #:block-diff #:block-diff-number #:block-diff-hash #:block-diff-root
   #:block-diff-accounts #:block-diff-codes
   #:account-diff #:account-diff-address #:account-diff-deleted-p #:account-diff-wiped-p
   #:account-diff-nonce #:account-diff-balance #:account-diff-code-hash #:account-diff-slots
   #:make-account-diff #:make-block-diff #:db-apply-diff
   ;; persistence
   #:store #:store-db #:store-dir #:store-interval #:open-store #:close-store
   #:store-commit-block #:store-checkpoint #:store-sync
   #:store-root-mismatch #:store-root-mismatch-number #:store-root-mismatch-expected
   #:store-root-mismatch-actual #:store-format-error
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

(defstruct (account (:constructor %make-account (address)) (:copier nil))
  (address 0 :type integer :read-only t)
  (nonce 0 :type (unsigned-byte 64))
  (balance 0 :type integer)
  (code-hash +empty-code-hash+ :type hash32)
  ;; The root of STORAGE once its trie is clean. A checkpoint load restores it without
  ;; building the trie; the trie is built from STORAGE on the first write and checked
  ;; against this root.
  (storage-root +empty-trie-root+ :type hash32)
  (storage nil :type (or null hash-table)) ; slot -> nonzero value
  (trie nil)                               ; storage trie, NIL until first needed
  (dirty-slots '() :type list))            ; slots written since the trie was last updated

(defun account-slot-count (account)
  (let ((s (account-storage account))) (if s (hash-table-count s) 0)))

;;; One undo record per account touched in the open block: the account object and its
;;; fields as they were, the first old value of each slot written before any wipe, and the
;;; storage table and trie objects to put back.
(defstruct (touch (:constructor make-touch (address account)))
  (address 0 :type integer)
  account                               ; object before the block, or NIL if absent
  (nonce 0) (balance 0) code-hash storage-root storage trie
  (old-slots (make-hash-table) :type hash-table) ; slot -> value before the block
  (written (make-hash-table) :type hash-table)   ; every slot written this block
  (wiped-p nil))

(defstruct (statedb (:constructor make-statedb ()) (:conc-name db-))
  (accounts (make-hash-table :size 4096) :type hash-table) ; address -> account
  (codes (make-hash-table :size 1024) :type hash-table)    ; hash integer -> octets
  (trie (make-secure-trie))                                ; account trie
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
  (proof-lock (sb-thread:make-mutex :name "statedb proofs")))

(defun db-block-open-p (db) (db-open-p db))
(defun db-account-count (db) (hash-table-count (db-accounts db)))

;;; Reads

(declaim (inline db-account))
(defun db-account (db address)
  (gethash address (db-accounts db)))

(defun db-storage (db address slot)
  (let ((a (gethash address (db-accounts db))))
    (if a
        (let ((s (account-storage a))) (if s (values (gethash slot s 0)) 0))
        0)))

(defun db-code (db hash)
  "Code octets for a code hash (32 octets or integer); the empty vector for the empty hash."
  (let ((key (code-hash-key hash)))
    (or (gethash key (db-codes db))
        (if (= key (load-time-value (octets->integer +empty-code-hash+)))
            (load-time-value (make-octets 0))
            (error "store: no code for hash ~X" key)))))

(defun map-accounts (fn db)
  "Call FN with (address account) for every account."
  (maphash fn (db-accounts db)))

(defun map-account-storage (fn account)
  (let ((s (account-storage account))) (when s (maphash fn s))))

(defun map-codes (fn db) (maphash fn (db-codes db)))

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

(defun ensure-storage-trie (account)
  (or (account-trie account)
      (let ((trie (build-storage-trie account)))
        (invariant (octets= (trie-root trie) (account-storage-root account))
                   "storage of ~40,'0X does not hash to its recorded root" (account-address account))
        (setf (account-trie account) trie))))

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
                                                   (flush-storage a) (account-code-hash a)))
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
      (let ((trie (sb-thread:with-mutex ((db-proof-lock db)) (ensure-storage-trie a))))
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
      (let ((trie (sb-thread:with-mutex ((db-proof-lock db)) (ensure-storage-trie a))))
        (trie-root trie)
        trie))))

;;; Writes. Every write happens inside an open block so it can be undone and logged.

(defun db-begin-block (db)
  (invariant (not (db-open-p db)) "a block is already open")
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
                    (touch-storage-root tc) (account-storage-root a)
                    (touch-storage tc) (account-storage a)
                    (touch-trie tc) (when trie (trie-clone trie)))))
          (setf (gethash address touched) tc)))))

(defun live-account (db address)
  (or (db-account db address)
      (setf (gethash address (db-accounts db)) (%make-account address))))

(defun db-set-account (db address nonce balance code-hash)
  "Create or overwrite ADDRESS's nonce, balance and code hash (32 octets)."
  (touch db address)
  (let ((a (live-account db address)))
    (setf (account-nonce a) nonce
          (account-balance a) balance
          (account-code-hash a) code-hash
          (gethash address (db-dirty db)) t)
    a))

(defun db-delete-account (db address)
  "Remove ADDRESS with its storage."
  (when (db-account db address)
    ;; A later write recreates the account with empty storage: the diff says wiped.
    (setf (touch-wiped-p (touch db address)) t)
    (remhash address (db-accounts db))
    (setf (gethash address (db-dirty db)) t)))

(defun db-wipe-storage (db address)
  "Clear ADDRESS's storage (a CREATE over an account that had storage, or a destroy)."
  (let ((tc (touch db address))
        (a (db-account db address)))
    (setf (touch-wiped-p tc) t)
    (when a
      (setf (account-storage a) nil
            (account-trie a) (make-secure-trie)
            (account-dirty-slots a) '()
            (account-storage-root a) +empty-trie-root+
            (gethash address (db-dirty db)) t))))

(defun db-set-storage (db address slot value)
  (let* ((tc (touch db address))
         (a (live-account db address))
         (storage (or (account-storage a)
                      (setf (account-storage a) (make-hash-table)))))
    (ensure-storage-trie a)
    (unless (touch-wiped-p tc)
      (multiple-value-bind (old present) (gethash slot (touch-old-slots tc))
        (declare (ignore old))
        (unless present (setf (gethash slot (touch-old-slots tc)) (gethash slot storage 0)))))
    (setf (gethash slot (touch-written tc)) t)
    (if (zerop value) (remhash slot storage) (setf (gethash slot storage) value))
    (push slot (account-dirty-slots a))
    (setf (gethash address (db-dirty db)) t)
    value))

(defun db-put-code (db code)
  "Store CODE and return its hash (32 octets)."
  (let* ((hash (keccak256 code))
         (key (octets->integer hash)))
    (unless (gethash key (db-codes db))
      (setf (gethash key (db-codes db)) code)
      (when (db-open-p db) (push key (db-new-codes db))))
    hash))

;;; Block diffs: what a committed block changed, the unit of the log.

(defstruct (account-diff (:constructor make-account-diff
                             (&key address deleted-p wiped-p nonce balance code-hash slots)))
  (address 0 :type integer)
  deleted-p wiped-p
  (nonce 0) (balance 0) code-hash
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
                            :code-hash (account-code-hash a)
                            :slots (loop for slot being the hash-keys of (touch-written tc)
                                         collect (cons slot (db-storage db address slot))))
                           (make-account-diff :address address :deleted-p t))
                       accounts)))
             (db-touched db))
    (make-block-diff :number number :hash hash :root root
                     :accounts (sort accounts #'< :key #'account-diff-address)
                     :codes (mapcar (lambda (k) (gethash k (db-codes db)))
                                    (reverse (db-new-codes db))))))

(defun close-block (db)
  (setf (db-touched db) (make-hash-table)
        (db-open-p db) nil
        (db-saved-trie db) nil
        (db-new-codes db) '()))

(defstruct (speculative-block (:constructor make-speculative-block
                                  (touched saved-trie number hash root recent)))
  touched saved-trie number hash root recent)

(defun copy-table (table)
  (let ((copy (make-hash-table :size (max 16 (hash-table-count table)))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun db-commit-block (db number hash)
  "Close the open block as block NUMBER with HASH: compute its state root, remember the
hash for BLOCKHASH, and return the block's diff. Inside WITH-SPECULATIVE-BLOCKS the block's
undo record is kept so the speculation can be unwound."
  (invariant (db-open-p db) "no open block")
  (let* ((root (db-state-root db))
         (diff (block-diff-from-touches db number hash root)))
    (when (plusp (db-speculation-depth db))
      (push (make-speculative-block (db-touched db) (db-saved-trie db) (db-number db) (db-hash db)
                                    (db-root-hash db) (copy-table (db-recent db)))
            (db-speculative db)))
    (close-block db)
    (setf (db-number db) number (db-hash db) hash (db-root-hash db) root)
    (db-note-block-hash db number hash)
    diff))

(defun undo-touches (db touched saved-trie)
  "Put every account in TOUCHED back as it was before its block, and the account trie."
  (let ((accounts (db-accounts db)))
    (maphash
     (lambda (address tc)
       (let ((a (touch-account tc)))
         (if (null a)
             (remhash address accounts)
             (progn
               (setf (account-nonce a) (touch-nonce tc)
                     (account-balance a) (touch-balance tc)
                     (account-code-hash a) (touch-code-hash tc)
                     (account-storage-root a) (touch-storage-root tc)
                     (account-storage a) (touch-storage tc)
                     (account-trie a) (touch-trie tc)
                     (account-dirty-slots a) '())
               (let ((s (account-storage a)))
                 ;; No table before the block means every recorded old value is zero.
                 (when s
                   (maphash (lambda (slot old)
                              (if (zerop old) (remhash slot s) (setf (gethash slot s) old)))
                            (touch-old-slots tc))))
               (setf (gethash address accounts) a)))))
     touched)
    (setf (db-trie db) saved-trie)
    (clrhash (db-dirty db))))

(defun db-revert-block (db)
  "Undo every write of the open block and close it."
  (invariant (db-open-p db) "no open block")
  (undo-touches db (db-touched db) (db-saved-trie db))
  (close-block db)
  db)

(defun unwind-speculation (db depth)
  "Undo the open block and every speculative block committed since the list had DEPTH entries."
  (when (db-open-p db) (db-revert-block db))
  (loop while (> (length (db-speculative db)) depth)
        do (let ((s (pop (db-speculative db))))
             (undo-touches db (speculative-block-touched s) (speculative-block-saved-trie s))
             (setf (db-number db) (speculative-block-number s)
                   (db-hash db) (speculative-block-hash s)
                   (db-root-hash db) (speculative-block-root s)
                   (db-recent db) (speculative-block-recent s)))))

(defmacro with-speculative-blocks ((db) &body body)
  "Run BODY, in which blocks may be executed and committed on DB one after another (each
building on the last), then undo all of them, on any exit. Nothing committed inside reaches
the block log unless BODY logs it itself. May nest."
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
                                  (account-diff-code-hash d))
               (loop for (slot . value) in (account-diff-slots d)
                     do (db-set-storage db address slot value))))))
  (let ((root (db-state-root db)))
    (db-commit-block db (block-diff-number diff) (block-diff-hash diff))
    root))
