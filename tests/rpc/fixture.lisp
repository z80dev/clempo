(defpackage #:clempo.rpc.fixture
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain))
  (:export #:*block* #:*template* #:test-backend #:make-test-backend #:backend-head-number
           #:put-block #:renumbered))
(in-package #:clempo.rpc.fixture)

;;; A backend over recorded mainnet blocks (data/ws-blocks.jsonl): blocks 3,462 and 3,463
;;; with their receipts. Every other block up to the head is 3,462 renumbered (its
;;; transaction and receipt reused), so heads can advance through thousands of blocks
;;; without the block archive.

(defparameter *block* 3463 "A mainnet block with two transactions and three logs.")
(defparameter *template* 3462 "The recorded block other heights are copies of.")

(defclass test-backend (chain:backend)
  ((head :initarg :head :accessor backend-head-number)
   (state :initarg :state :initform nil)                              ; the head's state database
   (blocks :initform (make-hash-table) :reader fixture-blocks)        ; number -> raw
   (receipts :initform (make-hash-table) :reader fixture-receipts)    ; number -> envelopes
   (lock :initform (sb-thread:make-mutex :name "fixture blocks") :reader fixture-lock))
  (:documentation "Recorded blocks with a head the test moves, and optionally a state
database served as the head's state."))

(defun recorded-blocks ()
  (with-open-file (in (asdf:system-relative-pathname "clempo-rpc" "../../tests/rpc/data/ws-blocks.jsonl"))
    (loop for line = (read-line in nil) while line
          collect (let ((j (json:parse-json line)))
                    (list (json:jget j "number") (hex->octets (json:jget j "block"))
                          (map 'list #'hex->octets (json:jget j "receipts")))))))

(defun renumbered (raw n)
  "The block RAW with its header's number set to N."
  (let* ((items (clempo.rlp:rlp-decode raw))
         (inner (fourth (first items))))
    (setf (nth 8 inner) (integer->octets-minimal n))
    (clempo.rlp:rlp-encode items)))

(defun put-block (b n raw &optional (receipts nil receipts-p))
  "Serve RAW as block N, with RECEIPTS when given (else its receipts are missing)."
  (sb-thread:with-mutex ((fixture-lock b))
    (setf (gethash n (fixture-blocks b)) raw)
    (if receipts-p
        (setf (gethash n (fixture-receipts b)) receipts)
        (remhash n (fixture-receipts b)))))

(defmethod chain:backend-head ((b test-backend)) (backend-head-number b))

(defmethod chain:backend-state ((b test-backend) number)
  (when (= number (backend-head-number b)) (slot-value b 'state)))

(defmethod chain:backend-block-raw ((b test-backend) n)
  (when (and (integerp n) (<= 0 n (backend-head-number b)))
    (or (sb-thread:with-mutex ((fixture-lock b)) (gethash n (fixture-blocks b)))
        (let ((template (sb-thread:with-mutex ((fixture-lock b))
                          (gethash *template* (fixture-blocks b)))))
          (put-block b n (renumbered template n)
                     (sb-thread:with-mutex ((fixture-lock b)) (gethash *template* (fixture-receipts b))))
          (chain:backend-block-raw b n)))))

(defun stored-blocks (b)
  (sb-thread:with-mutex ((fixture-lock b))
    (loop for n being the hash-keys of (fixture-blocks b) using (hash-value raw)
          when (<= n (backend-head-number b)) collect (cons n raw))))

(defmethod chain:backend-block-number ((b test-backend) hash)
  (loop for (n . raw) in (stored-blocks b)
        when (octets= hash (chain:scan-block-hashes raw)) return n))

(defmethod chain:backend-tx-location ((b test-backend) hash)
  (loop for (n . raw) in (stored-blocks b)
        do (let ((j (position hash (nth-value 1 (chain:scan-block-hashes raw)) :test #'octets=)))
             (when j (return (values n j))))))

(defmethod chain:backend-receipts-raw ((b test-backend) n)
  (multiple-value-bind (receipts present)
      (sb-thread:with-mutex ((fixture-lock b)) (gethash n (fixture-receipts b)))
    (if present receipts (error 'chain:missing-receipts :number n))))

(defun make-test-backend (&key (head (1- *block*)) state)
  "A backend over the recorded blocks, headed one below *BLOCK* unless HEAD is given; STATE,
a state database, is served as the head's."
  (let ((b (make-instance 'test-backend :head head :state state)))
    (loop for (n raw receipts) in (recorded-blocks) do (put-block b n raw receipts))
    b))
