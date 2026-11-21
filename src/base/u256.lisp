(in-package #:clempo.base)

;;; 256-bit words are plain Lisp integers in [0, 2^256). Small values stay fixnums and
;;; never allocate; wrapping is one LOGAND. Signed views use two's complement.

(define-constant +u256-modulus+ (ash 1 256))
(define-constant +u256-max+ (1- (ash 1 256)))
(define-constant +u64-max+ (1- (ash 1 64)))

(declaim (inline wrap256 u256->s256 s256->u256))
(defun wrap256 (n)
  (declare (type integer n))
  (ldb (byte 256 0) n))

(defun u256->s256 (n)
  (declare (type u256 n))
  (if (logbitp 255 n) (- n +u256-modulus+) n))

(defun s256->u256 (n)
  (declare (type integer n))
  (ldb (byte 256 0) n))
