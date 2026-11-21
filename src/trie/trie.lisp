(defpackage #:clempo.trie
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode #:rlp-encoded-length #:rlp-encode-into)
  (:export #:ordered-trie-root #:+empty-trie-root+))

(in-package #:clempo.trie)

(define-constant +empty-trie-root+ (keccak256 (rlp-encode 0)))
(defparameter *empty-bytes* (make-octets 0))

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

(defstruct (trie (:constructor make-trie ()) (:copier nil))
  root-node
  (owner (cons nil nil)))

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

(defun insert-node (node owner key at end value)
  (declare #.clempo.base:*hot* (type octets key value) (type fixnum at end))
  (etypecase node
    (null (new-leaf owner key at end value))
    (branch-node
     (let* ((index (and (< at end) (nibble key at)))
            (new (if index
                     (insert-node (svref (branch-node-children node) index)
                                  owner key (1+ at) end value)
                     value))
            (branch (edit-branch node owner)))
       (if index (setf (svref (branch-node-children branch) index) new)
           (setf (branch-node-value branch) new))
       branch))
    (short-node
     (let* ((path (short-node-path node))
            (plen (path-length path))
            (leaf-p (leaf-path-p path))
            (matched (common-length path key at end))
            (split (+ at matched))
            (item (short-node-item node)))
       (declare (type fixnum split))
       (if (and (= matched plen) (or (not leaf-p) (= split end)))
           (short-with node owner (if leaf-p value (insert-node item owner key split end value)))
           (let* ((children (make-array 16 :initial-element nil))
                  (branch (make-branch owner children))
                  (p (+ (path-start path) matched)))
             (if (= matched plen)
                 (setf (branch-node-value branch) item)
                 (setf (svref children (nibble path p))
                       (if (and (not leaf-p) (= (1+ matched) plen)) item
                           (make-short owner (make-path path (1+ p) (- plen matched 1) leaf-p) item))))
             (if (= split end)
                 (setf (branch-node-value branch) value)
                 (setf (svref children (nibble key split))
                       (new-leaf owner key (1+ split) end value)))
             (if (zerop matched) branch
                 (make-short owner (make-path path (path-start path) matched nil) branch))))))))

(declaim (inline ref-length write-ref))
(defun ref-length (ref)
  (declare #.clempo.base:*hot* (type (or null octets) ref))
  (cond ((null ref) 1) ((= (length ref) 32) 33) (t (length ref))))

(defun write-ref (ref bytes pos)
  (declare #.clempo.base:*hot* (type (or null octets) ref) (type octets bytes) (type fixnum pos))
  (cond ((null ref) (setf (aref bytes pos) #x80) (1+ pos))
        ((= (length ref) 32) (rlp-encode-into ref bytes pos))
        (t (replace bytes ref :start1 pos) (+ pos (length ref)))))

(defun rlp-list-buffer (payload)
  "Allocate the finished node RLP and write its list header."
  (declare #.clempo.base:*hot* (type fixnum payload))
  (if (<= payload 55)
      (let ((bytes (make-octets (1+ payload))))
        (setf (aref bytes 0) (+ #xc0 payload))
        (values bytes 1))
      (let* ((width (ceiling (integer-length payload) 8))
             (start (1+ width))
             (bytes (make-octets (+ start payload))))
        (setf (aref bytes 0) (+ #xf7 width))
        (loop for i fixnum from 1 below start
              do (setf (aref bytes i) (ldb (byte 8 (ash (- width i) 3)) payload)))
        (values bytes start))))

(defun node-reference (node)
  (when node
    (or (node-ref node)
        (let ((bytes (encode-node node)))
          (setf (node-ref node) (if (< (length bytes) 32) bytes (keccak256 bytes)))))))

(defun encode-node (node)
  (declare #.clempo.base:*hot*)
  (etypecase node
    (short-node
     (let* ((path (short-node-path node))
            (item (short-node-item node))
            (leaf-p (leaf-path-p path))
            (ref (unless leaf-p (node-reference item))))
       (multiple-value-bind (out pos)
           (rlp-list-buffer (+ (rlp-encoded-length path)
                               (if leaf-p (rlp-encoded-length item) (ref-length ref))))
         (let ((p (rlp-encode-into path out pos)))
           (if leaf-p (rlp-encode-into item out p) (write-ref ref out p)))
         out)))
    (branch-node
     (let* ((children (branch-node-children node))
            (value (or (branch-node-value node) *empty-bytes*))
            (payload (rlp-encoded-length value)))
       (declare (type fixnum payload))
       (dotimes (i 16) (incf payload (ref-length (node-reference (svref children i)))))
       (multiple-value-bind (out pos) (rlp-list-buffer payload)
         (declare (type fixnum pos))
         (dotimes (i 16) (setf pos (write-ref (node-reference (svref children i)) out pos)))
         (rlp-encode-into value out pos)
         out)))))

(defun trie-put (trie key value)
  (declare #.clempo.base:*hot* (type octets key value))
  (setf (trie-root-node trie)
        (insert-node (trie-root-node trie) (trie-owner trie) key 0 (* 2 (length key)) value))
  value)

(defun trie-root (trie)
  (let ((ref (node-reference (trie-root-node trie))))
    (cond ((null ref) +empty-trie-root+)
          ((= (length ref) 32) ref)
          (t (keccak256 ref)))))

(defun ordered-trie-root (encoded-items)
  "Root of an Ethereum indexed list. The keys are RLP(index), starting at zero.
ENCODED-ITEMS is a list of the exact transaction, receipt or withdrawal bytes."
  (let ((trie (make-trie)))
    (loop for item in encoded-items for index from 0
          do (trie-put trie (rlp-encode index) item))
    (trie-root trie)))
