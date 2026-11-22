(in-package #:clempo.evm)

;;; 256-bit word arithmetic on plain integers. Results stay in [0, 2^256). Fixnum
;;; operands take the inline fast path and never allocate unless the result is large.

(deftype word () '(integer 0 #.(1- (ash 1 256))))
(deftype gas () '(signed-byte 64))

(define-constant +mod+ (ash 1 256))
(define-constant +max+ (1- (ash 1 256)))
(define-constant +sign-bit+ (ash 1 255))

(defbound +word-bytes+ 32 "Bytes in a word's big-endian encoding."
  (= (* 8 +word-bytes+) (integer-length +max+)))

(defbound +word-limbs+ 4 "64-bit limbs WRITE-WORD splits a word into."
  (= (* 64 +word-limbs+) (* 8 +word-bytes+))
  ;; WRITE-WORD's fixnum path writes a word's low 8 bytes only.
  (< most-positive-fixnum (ash 1 64)))

;;; U*'s fast path multiplies two (UNSIGNED-BYTE 31) operands as fixnums.
(static-assert (typep (expt (1- (ash 1 31)) 2) 'fixnum))

(declaim (inline u+ u- u* to-signed from-signed u-lt u-gt s-lt s-gt
                 word-not to-address))

(defun u+ (a b)
  (declare (type word a b))
  (if (and (typep a 'fixnum) (typep b 'fixnum))
      (+ a b)
      (let ((r (+ a b))) (if (>= r +mod+) (- r +mod+) r))))

(defun u- (a b)
  (declare (type word a b))
  (if (and (typep a 'fixnum) (typep b 'fixnum) (>= a b))
      (- a b)
      (let ((r (- a b))) (if (minusp r) (+ r +mod+) r))))

(defun u* (a b)
  (declare (type word a b))
  (if (and (typep a '(unsigned-byte 31)) (typep b '(unsigned-byte 31)))
      (* a b)
      (let ((r (* a b))) (if (< r +mod+) r (logand r +max+)))))

(defun to-signed (a)
  (declare (type word a))
  (if (typep a 'fixnum) a (if (logbitp 255 a) (- a +mod+) a)))

(defun from-signed (a)
  (declare (type integer a))
  (if (minusp a) (+ a +mod+) a))

(defun word-not (a) (declare (type word a)) (logxor a +max+))

(defun to-address (a) (declare (type word a)) (ldb (byte 160 0) a))

(defun u/ (a b)
  (declare (type word a b))
  (if (zerop b) 0 (values (floor a b))))

(defun u-mod (a b)
  (declare (type word a b))
  (if (zerop b) 0 (mod a b)))

(defun s/ (a b)
  (declare (type word a b))
  (if (zerop b) 0 (from-signed (logand (truncate (to-signed a) (to-signed b)) +max+))))

(defun s-mod (a b)
  (declare (type word a b))
  (if (zerop b) 0 (from-signed (rem (to-signed a) (to-signed b)))))

(defun add-mod (a b n)
  (declare (type word a b n))
  (if (zerop n) 0 (mod (+ a b) n)))

(defun mul-mod (a b n)
  (declare (type word a b n))
  (if (zerop n) 0 (mod (* a b) n)))

(defun u-exp (base exponent)
  (declare (type word base exponent))
  (cond ((zerop exponent) 1)
        ((< base 2) base)
        ((= base 2) (if (< exponent 256) (ash 1 exponent) 0))
        (t (let ((result 1) (b base) (e exponent))
             (declare (type integer result b e))
             (loop while (plusp e)
                   do (when (oddp e) (setf result (logand (* result b) +max+)))
                      (setf e (ash e -1))
                      (when (plusp e) (setf b (logand (* b b) +max+))))
             result))))

(defun sign-extend (b x)
  (declare (type word b x))
  (if (>= b 31)
      x
      (let ((bit (+ (* 8 b) 7)))
        (if (logbitp bit x)
            (logior x (logxor +max+ (1- (ash 1 bit))))
            (ldb (byte (1+ bit) 0) x)))))

(defun word-byte (i x)
  (declare (type word i x))
  (if (< i 32) (ldb (byte 8 (* 8 (- 31 i))) x) 0))

(defun shl (shift x)
  (declare (type word shift x))
  (if (< shift 256) (ash (ldb (byte (- 256 shift) 0) x) shift) 0))

(defun shr (shift x)
  (declare (type word shift x))
  (if (< shift 256) (ash x (- shift)) 0))

(defun sar (shift x)
  (declare (type word shift x))
  (let ((s (to-signed x)))
    (if (< shift 256)
        (from-signed (ash s (- shift)))
        (if (minusp s) +max+ 0))))

(defun u-lt (a b) (declare (type word a b)) (if (< a b) 1 0))
(defun u-gt (a b) (declare (type word a b)) (if (> a b) 1 0))
(defun s-lt (a b) (declare (type word a b)) (if (< (to-signed a) (to-signed b)) 1 0))
(defun s-gt (a b) (declare (type word a b)) (if (> (to-signed a) (to-signed b)) 1 0))

;;; Words <-> memory bytes. A word is read and written as four 64-bit limbs so a large
;;; value is built with few intermediate bignums.

(defun read-word (bytes start)
  "The 32 bytes of BYTES at START as a word."
  (declare #.*hot* (type octets bytes) (type fixnum start))
  (flet ((limb (pos)
           (declare (type fixnum pos))
           (let ((v 0))
             (declare (type (unsigned-byte 64) v))
             (loop for i fixnum from pos below (+ pos 8)
                   do (setf v (logior (ldb (byte 64 0) (ash v 8)) (aref bytes i))))
             v)))
    (let ((l0 (limb start)) (l1 (limb (+ start 8))) (l2 (limb (+ start 16))) (l3 (limb (+ start 24))))
      (declare (type (unsigned-byte 64) l0 l1 l2 l3))
      (if (and (zerop l0) (zerop l1) (zerop l2) (< l3 most-positive-fixnum))
          l3
          (logior (ash l0 192) (ash l1 128) (ash l2 64) l3)))))

(defun read-word-padded (bytes start)
  "The 32 bytes of BYTES at START, zero-padded past its end."
  (declare (type octets bytes) (type integer start))
  (let ((len (length bytes)))
    (cond ((>= start len) 0)
          ((<= (+ start 32) len) (read-word bytes start))
          (t (let ((v 0))
               (loop for i from start below (+ start 32)
                     do (setf v (logior (ash v 8) (if (< i len) (aref bytes i) 0))))
               v)))))

(defun write-word (bytes start value)
  (declare #.*hot* (type octets bytes) (type fixnum start) (type word value))
  (if (typep value 'fixnum)
      (progn
        (fill bytes 0 :start start :end (+ start 24))
        (loop for i fixnum from (+ start 31) downto (+ start 24)
              for shift fixnum from 0 by 8
              do (setf (aref bytes i) (ldb (byte 8 shift) value))))
      (loop for limb fixnum from 0 below +word-limbs+
            for l of-type (unsigned-byte 64) = (ldb (byte 64 (* 64 limb)) value)
            do (loop for i fixnum from (- (+ start 31) (* 8 limb)) downto (- (+ start 24) (* 8 limb))
                     for shift fixnum from 0 by 8
                     do (setf (aref bytes i) (ldb (byte 8 shift) l)))))
  value)

(defun word->octets (value)
  (let ((o (make-octets 32))) (write-word o 0 value) o))

(defun octets->word (o) (octets->integer o))

(defun copy-padded (src src-start dst dst-start size)
  "Copy SIZE bytes from SRC at SRC-START (an integer, possibly past its end) into DST,
zero-filling what SRC lacks."
  (declare (type octets src dst) (type integer src-start) (type fixnum dst-start size))
  (let* ((len (length src))
         (avail (if (>= src-start len) 0 (min size (- len src-start)))))
    (declare (type fixnum avail))
    (when (plusp avail)
      (replace dst src :start1 dst-start :end1 (+ dst-start avail) :start2 src-start))
    (when (< avail size)
      (fill dst 0 :start (+ dst-start avail) :end (+ dst-start size)))
    dst))

(defun padded-slice (src start size)
  "SIZE bytes of SRC from START, zero-padded."
  (copy-padded src start (make-octets size) 0 size))
