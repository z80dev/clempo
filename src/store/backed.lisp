(in-package #:clempo.store)

;;; A backed state database: its tries' nodes in the KV store, its writes handed to the store
;;; as one batch per flush, and its caches trimmed after each flush.
;;;
;;; Trie nodes are kept by hash with a reference count: a node's value in the store is its
;;; count (varint) and its RLP. A node is referenced by each stored node that names it (a
;;; branch or extension child, or the storage root in an account leaf) and the state root by
;;; the head, so identical subtrees (equal storage tries of two contracts) are stored once.
;;; Between flushes the tries hash in RAM and every node they encode lands in PENDING; a flush
;;; walks from the new state root through the nodes not yet stored, counts their references,
;;; drops the reference the old root held and deletes every node whose count reaches zero,
;;; walking on into its children. Nodes hashed between flushes that the new root does not
;;; reach (intermediate states, speculative blocks) are never written. So the store holds
;;; exactly the nodes of the state at its head, and a hot path rewritten in every block is
;;; written once per flush.

(defstruct (node-store (:constructor make-node-store (kv)))
  kv
  (pending (make-hash-table :synchronized t) :type hash-table) ; hash integer -> RLP, since the flush
  (pending-bytes 0 :type fixnum)
  (loads 0 :type fixnum))                                       ; nodes read from the store

(defun node-value-split (value)
  "(values count rlp-start) of a stored node value."
  (let ((n 0) (shift 0) (i 0))
    (loop (when (>= i (length value)) (format-error "truncated node value"))
          (let ((b (aref value i)))
            (incf i)
            (setf n (logior n (ash (logand b 127) shift)))
            (when (< b 128) (return (values n i)))
            (incf shift 7)
            (when (> shift 56) (format-error "overlong node count"))))))

(defun node-value (count rlp)
  (let ((sink (make-sink nil (+ 4 (length rlp)))))
    (put-varint sink count)
    (put-raw sink rlp)
    (sink-octets sink)))

(defmethod node-store-get ((store node-store) hash)
  (let ((key (octets->integer hash)))
    (or (gethash key (node-store-pending store))
        (let ((v (kv-get (node-store-kv store) (key-node key))))
          (when v
            (incf (node-store-loads store))
            (subseq v (nth-value 1 (node-value-split v))))))))

(defmethod node-store-put ((store node-store) hash bytes)
  (let ((key (octets->integer hash)) (pending (node-store-pending store)))
    (unless (gethash key pending)
      (incf (node-store-pending-bytes store) (length bytes))
      (setf (gethash key pending) bytes))))

(defun node-references (rlp account-p)
  "The stored nodes RLP names: ((hash-integer . account-trie-p) ...). In the account trie a
leaf's account names its storage root (a storage-trie node)."
  (let ((items (rlp-decode rlp)))
    (flet ((ref-p (x) (and (typep x 'octets) (= (length x) 32))))
      (case (length items)
        (17 (loop for i below 16 for x = (nth i items)
                  when (ref-p x) collect (cons (octets->integer x) account-p)))
        (2 (destructuring-bind (path item) items
             (cond ((not (and (typep path 'octets) (plusp (length path)))) (format-error "bad trie node path"))
                   ((logbitp 5 (aref path 0))
                    (when account-p
                      (let ((root (nth-value 2 (decode-account item))))
                        (unless (octets= root +empty-trie-root+)
                          (list (cons (octets->integer root) nil))))))
                   ((ref-p item) (list (cons (octets->integer item) account-p))))))
        (t (format-error "trie node with ~D items" (length items)))))))

(defun write-nodes (store entries &key refs unrefs visits)
  "Fold the pending nodes reachable from REFS and VISITS into ENTRIES (key -> value, the
flush batch), with reference counts. REFS and UNREFS are ((hash . account-p) ...) roots
gaining and losing a reference; VISITS are roots to store without a reference (an import's
storage roots, which their accounts reference later)."
  (let* ((kv (node-store-kv store))
         (pending (node-store-pending store))
         (delta (make-hash-table))          ; hash -> change of its count
         (new (make-hash-table))            ; hash -> (rlp . account-p), stored by this flush
         (stored (make-hash-table))         ; hash -> (count . rlp) read from the store
         (dead (make-hash-table))
         (work '()))
    (labels ((stored-node (h)
               (multiple-value-bind (s found) (gethash h stored)
                 (if found s
                     (setf (gethash h stored)
                           (let ((v (or (let ((e (gethash (key-node h) entries))) (and e (plusp (length e)) e))
                                        (kv-get kv (key-node h)))))
                             (when v
                               (multiple-value-bind (count start) (node-value-split v)
                                 (cons count (subseq v start)))))))))
             (visit (h account-p)
               (unless (gethash h new)
                 (let ((rlp (gethash h pending)))
                   (cond ((and rlp (null (stored-node h)))
                          (setf (gethash h new) (cons rlp account-p))
                          (dolist (c (node-references rlp account-p)) (ref (car c) (cdr c))))
                         ((null (stored-node h))
                          (invariant nil "trie node ~64,'0X is neither pending nor stored" h))))))
             (ref (h account-p) (incf (gethash h delta 0)) (visit h account-p))
             (unref (h account-p) (decf (gethash h delta 0)) (push (cons h account-p) work)))
      (dolist (r visits) (visit (car r) (cdr r)))
      (dolist (r refs) (ref (car r) (cdr r)))
      (dolist (r unrefs) (unref (car r) (cdr r)))
      ;; Counts only fall from here: a node whose count reaches zero is deleted and gives
      ;; up its references.
      (loop while work
            do (destructuring-bind (h . account-p) (pop work)
                 (unless (gethash h dead)
                   (let* ((n (gethash h new))
                          (s (unless n (stored-node h)))
                          (count (+ (if n 0 (car s)) (gethash h delta 0))))
                     (invariant (or n s) "unreferencing trie node ~64,'0X that is not stored" h)
                     (invariant (>= count 0) "trie node ~64,'0X referenced below zero" h)
                     (when (zerop count)
                       (setf (gethash h dead) t)
                       (dolist (c (node-references (if n (car n) (cdr s)) account-p))
                         (unref (car c) (cdr c))))))))
      (maphash (lambda (h n)
                 (unless (gethash h dead)
                   (setf (gethash (key-node h) entries) (node-value (gethash h delta 0) (car n)))))
               new)
      (maphash (lambda (h d)
                 (unless (gethash h new)
                   (cond ((gethash h dead) (setf (gethash (key-node h) entries) (make-octets 0)))
                         ((/= d 0)
                          (let ((s (stored-node h)))
                            (setf (gethash (key-node h) entries) (node-value (+ (car s) d) (cdr s))))))))
               delta)
      (clrhash pending)
      (setf (node-store-pending-bytes store) 0)
      entries)))

;;; Flushing a backed database

(defun encode-head (db)
  (let ((sink (make-sink nil 9000)))
    (put-u64 sink (db-number db))
    (put-raw sink (db-hash db))
    (put-raw sink (db-root-hash db))
    (put-u64 sink (db-account-total db))
    (put-u32 sink (hash-table-count (db-recent db)))
    (map-recent-hashes (lambda (n h) (put-u64 sink n) (put-raw sink h)) db)
    (sink-octets sink)))

(defun decode-head (db octets)
  (let ((src (make-source octets)))
    (setf (db-number db) (get-u64 src)
          (db-hash db) (get-raw src 32)
          (db-root-hash db) (get-raw src 32)
          (db-account-total db) (get-u64 src))
    (loop repeat (get-u32 src) do (let ((n (get-u64 src))) (db-note-block-hash db n (get-raw src 32))))
    (unless (source-exhausted-p src) (format-error "trailing bytes in the head record"))
    db))

(defun db-flush-ready-p (db)
  (and (db-backing db) (not (db-open-p db)) (zerop (db-speculation-depth db))))

(defun db-flush-entries (db)
  "The batch that brings the store to DB's state: a vector of (key . value) in key order."
  (invariant (db-flush-ready-p db) "flush inside a block or a speculation")
  (let ((entries (make-hash-table :size (* 2 (+ 64 (db-dirty-weight db)))))
        (kv (db-backing db))
        (tombstone (make-octets 0)))
    (maphash
     (lambda (address mark)
       (let ((a (db-account db address)))
         (when (flush-mark-wiped-p mark)
           (multiple-value-bind (lo hi) (key-range-slots address)
             (kv-map-range kv lo hi (lambda (key value) (declare (ignore value))
                                      (setf (gethash key entries) tombstone)))))
         (cond (a (setf (gethash (key-account address) entries) (encode-account-record a))
                  (maphash (lambda (slot flag)
                             (declare (ignore flag))
                             (let ((v (account-slot db a slot)))
                               (setf (gethash (key-slot address slot) entries)
                                     (if (zerop v) tombstone (encode-slot-value v)))))
                           (flush-mark-slots mark)))
               (t (setf (gethash (key-account address) entries) tombstone)))))
     (db-flush-accounts db))
    (dolist (k (db-flush-codes db))
      (let ((code (gethash k (db-codes db))))
        (when code (setf (gethash (key-code k) entries) code))))
    (let ((old (db-flushed-root db)) (new (db-root-hash db)))
      (unless (octets= old new)
        (flet ((root (r) (unless (octets= r +empty-trie-root+) (list (cons (octets->integer r) t)))))
          (write-nodes (db-nodes db) entries :refs (root new) :unrefs (root old)))))
    (clrhash (node-store-pending (db-nodes db)))
    (setf (node-store-pending-bytes (db-nodes db)) 0)
    (setf (gethash (key-head) entries) (encode-head db))
    (sorted-entries entries)))

(defun sorted-entries (table)
  (let ((v (make-array (hash-table-count table))) (i 0))
    (maphash (lambda (k val) (setf (aref v i) (cons k val)) (incf i)) table)
    (sort v #'< :key #'car)))

(defun db-flush-weight (db)
  "How much a backed database holds in RAM for its next flush."
  (+ (db-dirty-weight db) (hash-table-count (node-store-pending (db-nodes db)))))

(defun db-after-flush (db)
  "The store now holds DB's state: forget the flushed writes and trim the caches."
  (clrhash (db-flush-accounts db))
  (setf (db-flush-codes db) '()
        (db-dirty-weight db) 0
        (db-flushed-root db) (db-root-hash db))
  (evict db))

(defun evict (db)
  "Trim a flushed database's caches to its limits. Every cached object is clean (the store
holds it), so anything may go; the clock bits keep what was used since the last trim."
  (let ((limits (db-limits db)) (accounts (db-accounts db)) (nodes (db-nodes db)))
    ;; trie nodes: reopen the tries from their roots
    (when (> (node-store-loads nodes) (cache-limits-trie-nodes limits))
      (setf (db-trie db) (make-secure-trie :store nodes :root (db-root-hash db)))
      (maphash (lambda (address a) (declare (ignore address)) (when a (setf (account-trie a) nil))) accounts)
      (setf (node-store-loads nodes) 0))
    ;; slots: drop every account's slot cache
    (when (> (db-cached-slots db) (cache-limits-slots limits))
      (maphash (lambda (address a)
                 (declare (ignore address))
                 (when (and a (account-storage a))
                   (setf (account-storage a) nil
                         (account-complete-p a) (octets= (account-storage-root a) +empty-trie-root+))))
               accounts)
      (setf (db-cached-slots db) 0))
    ;; accounts: keep those used since the last trim, or none
    (when (> (hash-table-count accounts) (cache-limits-accounts limits))
      (let ((drop '()))
        (maphash (lambda (address a)
                   (if (and a (account-used a))
                       (setf (account-used a) nil)
                       (push address drop)))
                 accounts)
        (dolist (address drop) (remhash address accounts)))
      (when (> (hash-table-count accounts) (cache-limits-accounts limits))
        (clrhash accounts)))
    (when (> (db-code-bytes db) (cache-limits-code-bytes limits))
      (clrhash (db-codes db))
      (setf (db-code-bytes db) 0))
    db))

(defun db-cache-stats (db)
  "A plist of what a backed database holds in RAM."
  (list :accounts (hash-table-count (db-accounts db))
        :cached-slots (db-cached-slots db)
        :code-bytes (db-code-bytes db)
        :trie-node-loads (if (db-nodes db) (node-store-loads (db-nodes db)) 0)
        :pending-nodes (if (db-nodes db) (hash-table-count (node-store-pending (db-nodes db))) 0)
        :unflushed-writes (db-dirty-weight db)))

(defun open-backed-statedb (kv limits)
  "The state database over KV's head (an empty one when KV holds none)."
  (let* ((nodes (make-node-store kv))
         (db (%make-statedb kv nodes limits))
         (head (kv-get kv (key-head))))
    (when head
      (decode-head db head)
      (setf (db-trie db) (make-secure-trie :store nodes :root (db-root-hash db))
            (db-flushed-root db) (db-root-hash db)))
    db))
