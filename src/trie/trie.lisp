(defpackage #:clempo.trie
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak #:keccak256 #:keccak256-into #:make-keccak)
  (:import-from #:clempo.rlp #:rlp-encode #:rlp-encoded-length #:rlp-encode-into
                            #:rlp-item-bounds #:rlp-decode #:rlp-decode-integer #:rlp-error)
  (:export #:trie #:make-trie #:make-secure-trie #:trie-clone
           #:trie-get #:trie-put #:trie-delete #:trie-root #:ordered-trie-root
           #:trie-proof #:verify-proof #:trie-node-at #:node-store-get #:node-store-put
           #:call-with-trie-hashing #:trie-from-items
           #:trie-node-error #:encode-account #:decode-account
           #:+empty-trie-root+ #:+empty-code-hash+))

(in-package #:clempo.trie)

(define-constant +empty-trie-root+ (keccak256 (rlp-encode 0)))
(defparameter *empty-bytes* (make-octets 0))
(define-constant +empty-code-hash+ (keccak256 (make-octets 0)))

;;; The largest state branch is a 3-byte list header, sixteen 33-byte child
;;; references, and a 1-byte empty value. Larger arbitrary values still work.
(defstruct hash-workspace
  (node-bytes (make-octets 544) :type octets)
  (key-bytes (make-octets 32) :type octets)
  (keccak (make-keccak)))

(defvar *hash-workspace* nil)
(defvar *parallel-map* nil)
(defvar *parallel-threshold* 512)

(defun call-with-trie-hashing (function &key (parallel-map *parallel-map*)
                                             (parallel-threshold *parallel-threshold*))
  "Call FUNCTION with reusable hashing workspace for a sequence of trie operations.
For a large dirty root without a node store, PARALLEL-MAP, if supplied, receives
(job-function jobs-list). It must invoke every job and join them before returning.
Each job hashes an independent subtree; its return value is ignored. Serialized
and parallel calls produce the same root."
  (check-type parallel-map (or null function))
  (check-type parallel-threshold (integer 0 *))
  (let ((*hash-workspace* (or *hash-workspace* (make-hash-workspace)))
        (*parallel-map* parallel-map)
        (*parallel-threshold* parallel-threshold))
    (funcall function)))

(declaim (inline hash-octets))
(defun hash-octets (bytes end)
  (declare #.clempo.base:*hot* (type octets bytes) (type fixnum end))
  (keccak256-into bytes 0 end (make-octets 32) 0
                  (when *hash-workspace* (hash-workspace-keccak *hash-workspace*))))

;;; Immutable paths use Ethereum's hex-prefix representation directly. The
;;; first nibble is flags (leaf, odd length), not part of the key.
(declaim (inline nibble path-start path-length leaf-path-p))
(defun nibble (bytes i)
  (declare #.clempo.base:*hot* (type octets bytes) (type fixnum i))
  (let ((b (aref bytes (ash i -1))))
    (if (logbitp 0 i) (logand b 15) (ash b -4))))

(defun path-start (path)
  (declare #.clempo.base:*hot* (type octets path))
  (if (logbitp 4 (aref path 0)) 1 2))

(defun path-length (path)
  (declare #.clempo.base:*hot* (type octets path))
  (- (ash (length path) 1) (path-start path)))

(defun leaf-path-p (path)
  (declare #.clempo.base:*hot* (type octets path))
  (logbitp 5 (aref path 0)))

(defun allocate-path (count leaf-p)
  (declare #.clempo.base:*hot* (type fixnum count))
  (let ((path (make-octets (1+ (ash count -1)))))
    (setf (aref path 0) (logior (if (oddp count) #x10 0) (if leaf-p #x20 0)))
    path))

(defun copy-nibbles (dst at src start count)
  (declare #.clempo.base:*hot* (type octets dst src) (type fixnum at start count))
  (loop repeat count
        for i fixnum from at for j fixnum from start
        for p fixnum = (ash i -1) for n = (nibble src j)
        do (setf (aref dst p)
                 (if (oddp i) (logior (aref dst p) n) (ash n 4))))
  dst)

(defun make-path (bytes start count leaf-p)
  (declare #.clempo.base:*hot* (type octets bytes) (type fixnum start count))
  (let ((path (allocate-path count leaf-p)))
    (copy-nibbles path (path-start path) bytes start count)))

(defun common-length (path key at end)
  (declare #.clempo.base:*hot* (type octets path key) (type fixnum at end))
  (let* ((start (path-start path))
         (limit (min (- end at) (- (ash (length path) 1) start))))
    (declare (type fixnum limit))
    (loop for n fixnum from 0 below limit
          unless (= (nibble path (+ n start)) (nibble key (+ n at))) return n
          finally (return limit))))

(defstruct (node (:constructor nil) (:copier nil))
  ;; Ownership permits in-place edits until a snapshot shares the root. A ref
  ;; is an inline node RLP (<32 bytes) or the 32-byte hash of a larger node.
  owner
  (ref nil :type (or null octets)))

(defstruct (short-node (:include node) (:constructor make-short (owner path item)))
  (path *empty-bytes* :type octets)
  item) ; leaf value or extension child
(defstruct (branch-node (:include node) (:constructor make-branch (owner children &optional value)))
  (children #() :type simple-vector)
  (value nil :type (or null octets)))

(defstruct (stored-node (:constructor make-stored-node (hash &optional root-p)))
  (hash *empty-bytes* :type octets :read-only t)
  (root-p nil :read-only t)
  loaded)

(defstruct (trie (:constructor %make-trie (root-node store secure-p)) (:copier nil))
  root-node
  (store nil :read-only t)
  (secure-p nil :read-only t)
  (owner (cons nil nil))
  root-hash
  (dirty-updates 0 :type fixnum))

(defgeneric node-store-get (store hash)
  (:documentation "Read immutable encoded node bytes by HASH, or return NIL if absent."))

(defgeneric node-store-put (store hash bytes)
  (:documentation "Persist immutable BYTES by HASH. Writes must be idempotent; signal on failure."))

(define-condition trie-node-error (error)
  ((reason :initarg :reason :reader trie-node-error-reason))
  (:report (lambda (c s) (format s "Trie node: ~A" (trie-node-error-reason c)))))

(defun bad-node (reason)
  (error 'trie-node-error :reason reason))

(defun make-trie (&key secure store root)
  "Create an empty trie, or open ROOT lazily from STORE. SECURE hashes each raw key.
Keys may be reused after a call. Stored values, roots and proof bytes are shared
immutable octet vectors; callers must not mutate them."
  (when root (check-type root (simple-array (unsigned-byte 8) (32))))
  (let ((nonempty (and root (not (octets= root +empty-trie-root+)))))
    (when nonempty (check-type store (not null)))
    (%make-trie (when nonempty (make-stored-node root t)) store secure)))

(defun make-secure-trie (&key store root)
  (make-trie :secure t :store store :root root))

(defun trie-clone (trie)
  "Take an O(1) snapshot. Both handles may be edited independently; subsequent
writes copy only shared paths. Do not edit a handle concurrently."
  (let ((copy (%make-trie (trie-root-node trie) (trie-store trie) (trie-secure-p trie))))
    (setf (trie-owner trie) (cons nil nil)
          (trie-root-hash copy) (trie-root-hash trie)
          (trie-dirty-updates copy) (trie-dirty-updates trie))
    copy))

(defun trie-key (trie key)
  (cond ((not (trie-secure-p trie)) key)
        ((and *hash-workspace* (null (trie-store trie)))
         ;; In-memory traversal cannot invoke a node-store method that reenters
         ;; the trie while this scratch key is still being read.
         (keccak256-into key 0 (length key) (hash-workspace-key-bytes *hash-workspace*) 0
                         (hash-workspace-keccak *hash-workspace*)))
        (t (keccak256 key))))

(defun new-leaf (owner key at end value)
  (declare #.clempo.base:*hot* (type octets key) (type fixnum at end))
  (make-short owner (make-path key at (- end at) t) value))

(defun short-with (node owner item)
  (if (eq owner (node-owner node))
      (progn (setf (short-node-item node) item (node-ref node) nil) node)
      (make-short owner (short-node-path node) item)))

(defun edit-branch (node owner)
  (if (eq owner (node-owner node))
      (progn (setf (node-ref node) nil) node)
      (make-branch owner (copy-seq (branch-node-children node)) (branch-node-value node))))

(defun insert-node (node owner key at end value store)
  (declare #.clempo.base:*hot* (type octets key value) (type fixnum at end))
  (let ((opened (open-node node store)))
    (etypecase opened
      (null (values (new-leaf owner key at end value) t))
      (branch-node
       (let ((index (and (< at end) (nibble key at))))
         (if index
             (multiple-value-bind (child changed)
                 (insert-node (svref (branch-node-children opened) index)
                              owner key (1+ at) end value store)
               (if changed
                   (let ((branch (edit-branch opened owner)))
                     (setf (svref (branch-node-children branch) index) child)
                     (values branch t))
                   (values node nil)))
             (if (and (branch-node-value opened) (octets= (branch-node-value opened) value))
                 (values node nil)
                 (let ((branch (edit-branch opened owner)))
                   (setf (branch-node-value branch) value)
                   (values branch t))))))
      (short-node
       (let* ((path (short-node-path opened))
              (plen (path-length path))
              (leaf-p (leaf-path-p path))
              (matched (common-length path key at end))
              (split (+ at matched))
              (item (short-node-item opened)))
         (declare (type fixnum split))
         (cond
           ((and (= matched plen) (not leaf-p))
            (multiple-value-bind (child changed) (insert-node item owner key split end value store)
              (if changed (values (short-with opened owner child) t) (values node nil))))
           ((and (= matched plen) (= split end))
            (if (octets= item value) (values node nil)
                (values (short-with opened owner value) t)))
           (t
            (let* ((children (make-array 16 :initial-element nil))
                   (branch (make-branch owner children))
                   (p (+ (path-start path) matched)))
              (declare (type fixnum p))
              (if (= matched plen)
                  (setf (branch-node-value branch) item)
                  (setf (svref children (nibble path p))
                        (if (and (not leaf-p) (= (1+ matched) plen)) item
                            (make-short owner
                                        (make-path path (1+ p) (the fixnum (- plen matched 1)) leaf-p)
                                        item))))
              (if (= split end)
                  (setf (branch-node-value branch) value)
                  (setf (svref children (nibble key split))
                        (new-leaf owner key (1+ split) end value)))
              (values (if (zerop matched) branch
                          (make-short owner (make-path path (path-start path) matched nil) branch))
                      t)))))))))

(defun prepend-path (owner bytes start count child store)
  ;; Consecutive short nodes are never canonical.
  (let ((opened (open-node child store)))
    (if (short-node-p opened)
        (let* ((suffix (short-node-path opened))
               (path (allocate-path (+ count (path-length suffix)) (leaf-path-p suffix)))
               (at (path-start path)))
          (copy-nibbles path at bytes start count)
          (copy-nibbles path (+ at count) suffix (path-start suffix) (path-length suffix))
          (make-short owner path (short-node-item opened)))
        (make-short owner (make-path bytes start count nil) child))))

(defun compact-branch (branch owner store)
  (let ((children (branch-node-children branch))
        (value (branch-node-value branch))
        (only nil))
    (dotimes (i 16)
      (when (svref children i)
        (when (or only value) (return-from compact-branch branch))
        (setf only i)))
    (cond (only (prepend-path owner (octets-of only) 1 1 (svref children only) store))
          (value (make-short owner (octets-of #x20) value))
          (t nil))))

(defun delete-node (node owner key at end store)
  (declare #.clempo.base:*hot* (type octets key) (type fixnum at end))
  (let ((opened (open-node node store)))
    (etypecase opened
      (null (values nil nil))
      (branch-node
       (cond
         ((< at end)
          (let ((i (nibble key at)))
            (multiple-value-bind (child changed)
                (delete-node (svref (branch-node-children opened) i) owner key (1+ at) end store)
              (if changed
                  (let ((branch (edit-branch opened owner)))
                    (setf (svref (branch-node-children branch) i) child)
                    (values (compact-branch branch owner store) t))
                  (values node nil)))))
         ((branch-node-value opened)
          (let ((branch (edit-branch opened owner)))
            (setf (branch-node-value branch) nil)
            (values (compact-branch branch owner store) t)))
         (t (values node nil))))
      (short-node
       (let* ((path (short-node-path opened))
              (len (path-length path))
              (next (+ at len)))
         (declare (type fixnum next))
         (cond
           ((/= len (common-length path key at end)) (values node nil))
           ((leaf-path-p path) (if (= next end) (values nil t) (values node nil)))
           (t (multiple-value-bind (child changed)
                  (delete-node (short-node-item opened) owner key next end store)
                (cond ((not changed) (values node nil))
                      ((null child) (values nil t))
                      ((short-node-p child)
                       (values (prepend-path owner path (path-start path) len child store) t))
                      (t (values (short-with opened owner child) t)))))))))))

(declaim (inline ref-length write-ref))
(defun ref-length (ref)
  (declare #.clempo.base:*hot* (type (or null octets) ref))
  (cond ((null ref) 1) ((= (length ref) 32) 33) (t (length ref))))

(defun write-ref (ref bytes pos)
  (declare #.clempo.base:*hot* (type (or null octets) ref) (type octets bytes) (type fixnum pos))
  (cond ((null ref) (setf (aref bytes pos) #x80) (1+ pos))
        ((= (length ref) 32)
         (setf (aref bytes pos) #xa0)
         (replace bytes ref :start1 (1+ pos))
         (+ pos 33))
        (t (replace bytes ref :start1 pos) (+ pos (length ref)))))

(defun rlp-list-buffer (payload &optional scratch)
  "Write the list header; return bytes, payload offset and total encoded length.
Short inline nodes and persisted or proof nodes need their own exact arrays."
  (declare #.clempo.base:*hot* (type fixnum payload) (type (or null octets) scratch))
  (let* ((width (if (<= payload 55) 0 (ceiling (integer-length payload) 8)))
         (start (1+ width))
         (end (+ start payload))
         (bytes (if (and scratch (<= 32 end (length scratch))) scratch (make-octets end))))
    (declare (type fixnum width start end) (type octets bytes))
    (if (zerop width)
        (setf (aref bytes 0) (+ #xc0 payload))
        (progn
          (setf (aref bytes 0) (+ #xf7 width))
          (loop for i fixnum from 1 below start
                do (setf (aref bytes i) (ldb (byte 8 (ash (- width i) 3)) payload)))))
    (values bytes start end)))

(declaim (ftype (function (t t &optional (or null octets)) (values octets fixnum &optional)) encode-node))

(defun node-reference (node store)
  (declare #.clempo.base:*hot*)
  (typecase node
    (null nil)
    (stored-node (stored-node-hash node))
    (t (or (node-ref node)
           (multiple-value-bind (bytes end)
               (encode-node node store (when (and *hash-workspace* (null store))
                                         (hash-workspace-node-bytes *hash-workspace*)))
             (let ((ref (if (< end 32) bytes (hash-octets bytes end))))
               (when (and store (>= end 32)) (node-store-put store ref bytes))
               (setf (node-ref node) ref)))))))

(defun encode-node (node store &optional scratch)
  (declare #.clempo.base:*hot*)
  (etypecase node
    (short-node
     (let* ((path (short-node-path node))
            (item (short-node-item node))
            (leaf-p (leaf-path-p path))
            (ref (unless leaf-p (node-reference item store))))
       (multiple-value-bind (out pos end)
           (rlp-list-buffer (+ (the fixnum (rlp-encoded-length path))
                               (if leaf-p (the fixnum (rlp-encoded-length item)) (ref-length ref)))
                            scratch)
         (let ((p (rlp-encode-into path out pos)))
           (if leaf-p (rlp-encode-into item out p) (write-ref ref out p)))
         (values out end))))
    (branch-node
     (let* ((children (branch-node-children node))
            (value (or (branch-node-value node) *empty-bytes*))
            (payload (rlp-encoded-length value)))
       (declare (type fixnum payload))
       (dotimes (i 16) (incf payload (ref-length (node-reference (svref children i) store))))
       (multiple-value-bind (out pos end) (rlp-list-buffer payload scratch)
         (declare (type fixnum pos))
         (dotimes (i 16)
           (setf pos (write-ref (node-reference (svref children i) store) out pos)))
         (rlp-encode-into value out pos)
         (values out end))))))

(defun decode-node-at (bytes at limit)
  (multiple-value-bind (pos end list-p) (rlp-item-bounds bytes at limit)
    (unless list-p (bad-node "node must be an RLP list"))
    (let ((start pos))
      (labels ((item (kind)
                 (multiple-value-bind (first last nested) (rlp-item-bounds bytes pos end)
                   (let ((next last) (len (- last first)))
                     (prog1 (ecase kind
                              (:string
                               (when nested (bad-node "expected a string"))
                               (subseq bytes first last))
                              (:child
                               (cond (nested
                                      (unless (< (- last pos) 32)
                                        (bad-node "oversized inline child"))
                                      (decode-node-at bytes pos last))
                                     ((= len 32) (make-stored-node (subseq bytes first last)))
                                     ((not (zerop len)) (bad-node "invalid child reference")))))
                       (setf pos next))))))
        ;; Count only top-level items; then decode the two permitted shapes.
        (let* ((count (loop for p = start then next while (< p end)
                            for next = (nth-value 1 (rlp-item-bounds bytes p end))
                            count t))
               (node
                 (case count
                   (2 (let ((path (item :string)))
                        (unless (and (plusp (length path)) (< (aref path 0) #x40)
                                     (or (= (path-start path) 1) (zerop (nibble path 1))))
                          (bad-node "malformed compact path"))
                        (let* ((leaf-p (leaf-path-p path))
                               (value (item (if leaf-p :string :child))))
                          (unless (if leaf-p (plusp (length value))
                                      (and (plusp (path-length path)) value
                                           (not (short-node-p value))))
                            (bad-node "empty leaf, empty extension, or consecutive short nodes"))
                          (make-short nil path value))))
                   (17 (let ((children (make-array 16))
                             (populated 0))
                         (dotimes (i 16)
                           (let ((child (item :child)))
                             (when child (incf populated))
                             (setf (svref children i) child)))
                         (let* ((value (item :string))
                                (value (unless (zerop (length value)) value)))
                           (when (< (+ populated (if value 1 0)) 2)
                             (bad-node "uncompressed branch"))
                           (make-branch nil children value))))
                   (otherwise (bad-node "node has neither 2 nor 17 items")))))
          ;; An inline child already is its own reference. The caller sets the
          ;; checked hash for nodes read from a store.
          (when (< (- end at) 32)
            (setf (node-ref node) (subseq bytes at end)))
          (values node end))))))

(defun open-node (node store)
  (if (not (stored-node-p node))
      node
      (or (stored-node-loaded node)
          (let* ((hash (stored-node-hash node))
                 (bytes (node-store-get store hash)))
            (unless (and (typep bytes 'octets) (plusp (length bytes))
                         (or (stored-node-root-p node) (>= (length bytes) 32))
                         (octets= hash (keccak256 bytes)))
              (bad-node "missing, unreadable or mismatched node hash"))
            (multiple-value-bind (opened end) (decode-node-at bytes 0 (length bytes))
              (unless (= end (length bytes)) (bad-node "trailing data"))
              (when (>= end 32) (setf (node-ref opened) hash))
              ;; The memo is the only write a read makes. CAS publishes the node
              ;; fully initialized; concurrent readers of a clean trie that decoded
              ;; it too adopt the first copy.
              (or (sb-ext:compare-and-swap (stored-node-loaded node) nil opened) opened))))))

(defun walk-key (node key store &optional visit)
  (declare #.clempo.base:*hot* (type octets key))
  (let ((at 0) (end (* 2 (length key))))
    (declare (type fixnum at end))
    (loop
      (unless node (return nil))
      (setf node (open-node node store))
      (when visit (funcall visit node))
      (etypecase node
        (branch-node
         (when (= at end) (return (branch-node-value node)))
         (setf node (svref (branch-node-children node) (nibble key at)))
         (incf at))
        (short-node
         (let* ((path (short-node-path node))
                (len (path-length path)))
           (unless (= len (common-length path key at end)) (return nil))
           (incf at len)
           (if (leaf-path-p path)
               (return (when (= at end) (short-node-item node)))
               (setf node (short-node-item node)))))))))

(defun trie-get (trie key)
  "Return the value at KEY or NIL; do not mutate returned bytes."
  (check-type key octets)
  (walk-key (trie-root-node trie) (trie-key trie key) (trie-store trie)))

(defun update-trie (trie key value)
  (let* ((key (trie-key trie key))
         (root (trie-root-node trie))
         (owner (trie-owner trie))
         (store (trie-store trie))
         (end (* 2 (length key))))
    (multiple-value-bind (node changed)
        (if value (insert-node root owner key 0 end value store)
            (delete-node root owner key 0 end store))
      (when changed
        (setf (trie-root-node trie) node (trie-root-hash trie) nil)
        (incf (trie-dirty-updates trie)))
      changed)))

(defun trie-put (trie key value)
  "Put immutable VALUE at KEY; an empty value deletes KEY. Return VALUE."
  (check-type key octets)
  (check-type value octets)
  (update-trie trie key (unless (zerop (length value)) value))
  value)

(defun trie-delete (trie key)
  "Delete KEY; return true if it was present."
  (check-type key octets)
  (update-trie trie key nil))

;;; Bulk construction: the canonical trie of a whole key set built top down by splitting
;;; on nibbles, with no per-key insertion (no path splitting, copying or re-walking). With a
;;; parallel map bound by CALL-WITH-TRIE-HASHING and enough keys, keys are hashed and the
;;; root's sixteen subtrees built and hashed in parallel.

(defvar *bulk-parallel-threshold* 4096 "Keys from which TRIE-FROM-ITEMS works in parallel.")

(defun build-subtrie (items depth owner)
  "The node holding ITEMS, a nonempty list of (key . value) with distinct keys of one
length, below the first DEPTH nibbles they share."
  (declare #.clempo.base:*hot* (type fixnum depth) (type list items))
  (let* ((first-key (car (first items)))
         (end (* 2 (length (the octets first-key)))))
    (declare (type octets first-key) (type fixnum end))
    (if (null (rest items))
        (make-short owner (make-path first-key depth (- end depth) t) (cdr (first items)))
        (let ((split depth))
          (declare (type fixnum split))
          ;; nibbles every key shares become an extension over the branch that splits them
          (loop while (let ((n (nibble first-key split)))
                        (every (lambda (item) (= n (nibble (car item) split))) (rest items)))
                do (incf split))
          (let ((children (make-array 16 :initial-element nil))
                (buckets (make-array 16 :initial-element nil)))
            (declare (dynamic-extent buckets))
            (dolist (item items) (push item (svref buckets (nibble (car item) split))))
            (dotimes (i 16)
              (when (svref buckets i)
                (setf (svref children i) (build-subtrie (svref buckets i) (1+ split) owner))))
            (let ((branch (make-branch owner children)))
              (if (= split depth)
                  branch
                  (make-short owner (make-path first-key depth (- split depth) nil) branch))))))))

(defun trie-from-items (items item-fn &key secure)
  "A fresh trie holding, for each element of the simple-vector ITEMS, the key and value
ITEM-FN returns as two values (octets; empty values are skipped). Keys must be distinct
and, in a plain trie, of one length. ITEM-FN may be called from several threads."
  (check-type items simple-vector)
  (let* ((trie (%make-trie nil nil secure))
         (owner (trie-owner trie))
         (n (length items)))
    (flet ((pair (item)
             (multiple-value-bind (key value) (funcall item-fn item)
               (unless (zerop (length value))
                 (cons (if secure (keccak256 key) key) value)))))
      (if (and *parallel-map* (>= n *bulk-parallel-threshold*))
          (let* ((jobs 16)
                 (chunks (make-array jobs))       ; chunk -> 16 buckets by first nibble
                 (children (make-array 16 :initial-element nil)))
            (funcall *parallel-map*
                     (lambda (j)
                       (let ((buckets (make-array 16 :initial-element nil)))
                         (loop for i from (floor (* j n) jobs) below (floor (* (1+ j) n) jobs)
                               for p = (pair (svref items i))
                               when p do (push p (svref buckets (nibble (car p) 0))))
                         (setf (svref chunks j) buckets)))
                     (loop for j below jobs collect j))
            (funcall *parallel-map*
                     (lambda (nib)
                       (let ((bucket (loop for j below jobs append (svref (svref chunks j) nib))))
                         (when bucket
                           (let ((child (build-subtrie bucket 1 owner)))
                             (call-with-trie-hashing (lambda () (node-reference child nil))
                                                     :parallel-map nil)
                             (setf (svref children nib) child)))))
                     (loop for nib below 16 collect nib))
            (setf (trie-root-node trie)
                  (if (> (count-if #'identity children) 1)
                      (make-branch owner children)
                      ;; one occupied nibble: rebuild serially for the extension above it
                      (let ((all (loop for c across chunks
                                       append (loop for b across c append b))))
                        (when all (build-subtrie all 0 owner))))))
          (let ((all (loop for item across items for p = (pair item) when p collect p)))
            (when all (setf (trie-root-node trie) (build-subtrie all 0 owner))))))
    trie))

(defun prehash-root-children (node)
  ;; An extension may lead to the first branch. Every child of that branch
  ;; owns disjoint mutable nodes; no job writes the branch's own cached ref.
  (loop while (and (short-node-p node) (not (leaf-path-p (short-node-path node))))
        do (setf node (short-node-item node)))
  (when (branch-node-p node)
    (let ((jobs (loop for child across (branch-node-children node)
                      when (and (node-p child) (not (node-ref child))) collect child)))
      (when (cdr jobs)
        (funcall *parallel-map*
                 (lambda (child)
                   (call-with-trie-hashing (lambda () (node-reference child nil)) :parallel-map nil))
                 jobs)))))

(defun compute-root (trie node)
  (let ((store (trie-store trie)))
    (when (and *parallel-map* (null store) (>= (trie-dirty-updates trie) *parallel-threshold*))
      (prehash-root-children node))
    (let ((ref (node-reference node store)))
      (if (= (length ref) 32) ref
          (let ((hash (hash-octets ref (length ref))))
            (when store (node-store-put store hash ref))
            hash)))))

(defun trie-root (trie)
  "Return the canonical 32-byte root; with a store, persist dirty hashed nodes."
  (or (trie-root-hash trie)
      (let* ((node (trie-root-node trie))
             (hash (typecase node
                     (null +empty-trie-root+)
                     (stored-node (stored-node-hash node))
                     (t (if *hash-workspace*
                            (compute-root trie node)
                            (call-with-trie-hashing (lambda () (compute-root trie node))))))))
        (setf (trie-dirty-updates trie) 0 (trie-root-hash trie) hash))))

(defun trie-proof (trie key)
  "Return the RLP octets in eth_getProof order (encode each as 0x hex for RPC).
Includes the root even if short; inline children travel inside their parents."
  (check-type key octets)
  (trie-root trie)
  (let ((store (trie-store trie))
        (first t)
        (proof nil))
    (walk-key (trie-root-node trie) (trie-key trie key) store
              (lambda (node)
                (let ((ref (node-ref node)))
                  (when (or first (= (length ref) 32))
                    (push (if (= (length ref) 32) (encode-node node store) ref) proof))
                  (setf first nil))))
    (nreverse proof)))

;;; Node introspection (reth's TrieUpdates describe branch nodes by their nibble path).

(defun node-kind (node)
  (etypecase node
    (branch-node :branch)
    (short-node (if (leaf-path-p (short-node-path node)) :leaf :extension))))

(defun node-cached-ref (node)
  (if (stored-node-p node) (stored-node-hash node) (node-ref node)))

(defun trie-node-at (trie nibbles)
  "The node of TRIE whose path starts exactly at NIBBLES (a sequence of 0..15 in hashed-key
space): (values :branch children) with CHILDREN a 16-vector of NIL or (kind . ref), kind
:branch, :extension or :leaf and ref the child's 32-byte hash or inline RLP;
(values :extension ext-nibbles child-kind) for an extension (EXT-NIBBLES a list);
(values :leaf key-nibbles) for a leaf (the rest of its key); NIL when no node starts there. Reads cached refs only: the trie
must be fully hashed (TRIE-ROOT); returned refs are shared and must not be mutated. Opening
a stored node publishes its decoded memo (safe for concurrent clean readers)."
  (let ((store (trie-store trie))
        (node (trie-root-node trie))
        (want (coerce nibbles 'simple-vector))
        (at 0))
    (loop
      (unless node (return nil))
      (setf node (open-node node store))
      (when (= at (length want))
        (return
          (etypecase node
            (branch-node
             (values :branch
                     (map 'simple-vector
                          (lambda (child)
                            (when child
                              (cons (node-kind (open-node child store)) (node-cached-ref child))))
                          (branch-node-children node))))
            (short-node
             (let* ((path (short-node-path node))
                    (nibbles (loop for i from (path-start path) below (+ (path-start path) (path-length path))
                                   collect (nibble path i))))
               (if (leaf-path-p path)
                   (values :leaf nibbles)
                   (values :extension nibbles (node-kind (open-node (short-node-item node) store)))))))))
      (etypecase node
        (branch-node
         (setf node (svref (branch-node-children node) (svref want at)))
         (incf at))
        (short-node
         (let* ((path (short-node-path node))
                (len (path-length path))
                (start (path-start path)))
           (when (or (leaf-path-p path) (> (+ at len) (length want))
                     (loop for i below len thereis (/= (nibble path (+ start i)) (svref want (+ at i)))))
             (return nil))
           (incf at len)
           (setf node (short-node-item node))))))))

(defstruct (proof-store (:constructor make-proof-store (nodes)))
  nodes)

(defmethod node-store-get ((store proof-store) hash)
  (declare (ignore hash))
  (pop (proof-store-nodes store)))

(defun verify-proof (root key proof &key secure)
  "Check ordered RLP node octets against ROOT. Return (values value valid-p).
With SECURE, hash KEY as for eth_getProof. A valid absence returns NIL, T;
a malformed, incomplete or trailing proof returns NIL, NIL."
  (check-type root (simple-array (unsigned-byte 8) (32)))
  (check-type key octets)
  (let ((nodes (coerce proof 'list)))
    (if (octets= root +empty-trie-root+)
        (values nil (or (null nodes)
                        (and (null (cdr nodes)) (typep (car nodes) 'octets)
                             (octets= (car nodes) (octets-of #x80)))))
        (let ((store (make-proof-store nodes)))
          (handler-case
              (let ((value (trie-get (make-trie :secure secure :store store :root root) key)))
                (if (proof-store-nodes store) (values nil nil) (values value t)))
            ((or trie-node-error rlp-error) () (values nil nil)))))))

(defun encode-account (nonce balance storage-root code-hash &optional (commitment 0))
  "Encode the Ethereum state account RLP. A nonzero T14 configuration COMMITMENT (TIP-1108)
adds the account extension as a fifth string, 0x00 || commitment (alloy-trie TrieAccount with
account-ext, tempo encode_config_commitment)."
  (check-type nonce (unsigned-byte 64))
  (check-type balance (unsigned-byte 256))
  (check-type storage-root (simple-array (unsigned-byte 8) (32)))
  (check-type code-hash (simple-array (unsigned-byte 8) (32)))
  (check-type commitment (unsigned-byte 256))
  (multiple-value-bind (out pos)
      (rlp-list-buffer (+ 66 (the fixnum (rlp-encoded-length nonce))
                             (the fixnum (rlp-encoded-length balance))
                          (if (zerop commitment) 0 34)))
    (setf pos (rlp-encode-into nonce out pos)
          pos (rlp-encode-into balance out pos)
          pos (write-ref storage-root out pos)
          pos (write-ref code-hash out pos))
    (unless (zerop commitment)
      (setf (aref out pos) #xa1 (aref out (1+ pos)) 0)
      (integer-into-octets commitment out (+ pos 2) (+ pos 34)))
    out))

(defun decode-account (bytes)
  "Return (values nonce balance storage-root code-hash commitment) from account RLP; the
commitment is 0 without the T14 extension (decode_config_commitment)."
  (destructuring-bind (nonce balance storage-root code-hash &optional (extension nil ext-p))
      (rlp-decode bytes)
    (check-type storage-root (simple-array (unsigned-byte 8) (32)))
    (check-type code-hash (simple-array (unsigned-byte 8) (32)))
    (when ext-p
      (unless (and (typep extension 'octets) (= (length extension) 33) (zerop (aref extension 0))
                   (find-if-not #'zerop extension :start 1))
        (error 'rlp-error :message "invalid account commitment extension")))
    (values (rlp-decode-integer nonce :max-bytes 8) (rlp-decode-integer balance)
            storage-root code-hash
            (if ext-p (octets->integer extension :start 1) 0))))

(defun ordered-trie-root (encoded-items)
  "Root of an Ethereum indexed list. The keys are RLP(index), starting at zero.
ENCODED-ITEMS is a list of the exact transaction, receipt or withdrawal bytes."
  (let ((trie (make-trie)))
    (loop for item in encoded-items for index from 0
          do (trie-put trie (rlp-encode index) item))
    (trie-root trie)))
