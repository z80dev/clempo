(in-package #:clempo.state)

;;; The committed state a WORLD reads through. Addresses, slots and values are integers;
;;; code hashes are 32 octets. A backend never sees uncommitted changes: the world hands
;;; them over with DRAIN-CHANGES at the end of a block.

(define-constant +empty-code+ (make-octets 0))
(define-constant +empty-code-hash+ (keccak256 (make-octets 0)))

(defun code-hash (code)
  (declare (type octets code))
  (if (zerop (length code)) +empty-code-hash+ (keccak256 code)))

(defgeneric backend-account (backend address)
  (:documentation "(values found-p nonce balance code-hash commitment) for ADDRESS. COMMITMENT
is the T14 configuration commitment (TIP-1108), 0 when absent; a backend that returns only
four values has none."))

(defgeneric backend-storage (backend address slot)
  (:documentation "The committed value of SLOT of ADDRESS, 0 when absent."))

(defgeneric backend-code (backend code-hash)
  (:documentation "The code whose keccak is CODE-HASH (32 octets)."))

(defgeneric backend-storage-empty-p (backend address)
  (:documentation "True when ADDRESS has no committed storage (EIP-7610 collision check)."))

;;; In-memory backend: accounts keyed by address, code keyed by its hash as an integer.

(defstruct (mem-account (:constructor make-mem-account (&key (nonce 0) (balance 0)
                                                            (code-hash +empty-code-hash+)
                                                            (commitment 0))))
  (nonce 0 :type (unsigned-byte 64))
  (balance 0 :type u256)
  (code-hash +empty-code-hash+ :type octets)
  (commitment 0 :type u256)
  (storage (make-hash-table) :type hash-table))

(defclass memory-backend ()
  ((accounts :initform (make-hash-table) :reader memory-backend-accounts)
   (codes :initform (make-hash-table) :reader memory-backend-codes)))

(defun make-memory-backend () (make-instance 'memory-backend))

(defmethod backend-account ((b memory-backend) address)
  (let ((a (gethash address (memory-backend-accounts b))))
    (if a
        (values t (mem-account-nonce a) (mem-account-balance a) (mem-account-code-hash a)
                (mem-account-commitment a))
        (values nil 0 0 +empty-code-hash+ 0))))

(defmethod backend-storage ((b memory-backend) address slot)
  (let ((a (gethash address (memory-backend-accounts b))))
    (if a (values (gethash slot (mem-account-storage a) 0)) 0)))

(defmethod backend-code ((b memory-backend) code-hash)
  (if (octets= code-hash +empty-code-hash+)
      +empty-code+
      (or (gethash (octets->integer code-hash) (memory-backend-codes b))
          (unreachable "no code for hash ~A" (octets->hex code-hash)))))

(defmethod backend-storage-empty-p ((b memory-backend) address)
  (let ((a (gethash address (memory-backend-accounts b))))
    (or (null a) (zerop (hash-table-count (mem-account-storage a))))))

(defun memory-backend-put-account (b address &key (nonce 0) (balance 0) (code +empty-code+)
                                                  (commitment 0))
  "Create or replace ADDRESS (keeping its storage) in the in-memory backend."
  (let* ((hash (code-hash code))
         (a (or (gethash address (memory-backend-accounts b))
                (setf (gethash address (memory-backend-accounts b)) (make-mem-account)))))
    (setf (mem-account-nonce a) nonce
          (mem-account-balance a) balance
          (mem-account-code-hash a) hash
          (mem-account-commitment a) commitment)
    (when (plusp (length code))
      (setf (gethash (octets->integer hash) (memory-backend-codes b)) code))
    a))

(defun memory-backend-put-storage (b address slot value)
  (let ((a (or (gethash address (memory-backend-accounts b))
               (unreachable "storage for missing account ~X" address))))
    (if (zerop value)
        (remhash slot (mem-account-storage a))
        (setf (gethash slot (mem-account-storage a)) value))))
