(defpackage #:clempo.field25519
  (:use #:cl #:clempo.base)
  (:export #:+p+ #:+limbs+ #:+zero+ #:+one+ #:field #:from-integer #:to-integer #:f+ #:f- #:f* #:sq
           #:inverse #:pow2523 #:integer-le #:octets-le #:clamped-octets))
(in-package #:clempo.field25519)

(defconstant +p+ (- (ash 1 255) 19))
(defconstant +limb-bits+ 51)
(defconstant +limb-mask+ (1- (ash 1 +limb-bits+)))

(defbound +limbs+ 5
  "Radix 2^51 limbs of a field element."
  (>= (* +limbs+ +limb-bits+) (integer-length +p+))
  ;; 2^255 = 19 (mod p): a product's limbs i+j >= +LIMBS+ fold back scaled by 19.
  (= (mod (ash 1 (* +limbs+ +limb-bits+)) +p+) 19)
  ;; A doubled, 19-scaled limb is one multiplicand word, and a product coefficient (at
  ;; most +LIMBS+ such terms) fits the two-word accumulator.
  (< (* 2 19 (ash 1 +limb-bits+)) (ash 1 64))
  (< (* +limbs+ 2 19 (ash 1 (* 2 +limb-bits+))) (ash 1 128)))

(deftype field () `(simple-array (unsigned-byte 64) (,+limbs+)))

(defun from-integer (x)
  (let ((a (make-array +limbs+ :element-type '(unsigned-byte 64))))
    (dotimes (i +limbs+ a) (setf (aref a i) (ldb (byte +limb-bits+ (* i +limb-bits+)) x)))))

(defparameter +zero+ (from-integer 0))
(defparameter +one+ (from-integer 1))

(defun to-integer (a)
  (declare (type field a))
  (let ((x (logior (aref a 0) (ash (aref a 1) 51) (ash (aref a 2) 102)
                   (ash (aref a 3) 153) (ash (aref a 4) 204))))
    (if (>= x +p+) (- x +p+) x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun limb-vars () (loop repeat +limbs+ collect (gensym "LIMB")))
  (defun limb-load (vars a)
    (loop for x in vars for i below +limbs+ collect `(,x (the (unsigned-byte 51) (aref ,a ,i)))))
  (defun normalize-forms (vars)
    (destructuring-bind (a b c d e) vars
      (declare (ignore b c d))
      `(,@(loop repeat 2 append
                (append (loop for (x y) on vars while y append
                              `((setf ,y (ldb (byte 64 0) (+ ,y (ash ,x -51)))
                                      ,x (logand +limb-mask+ ,x))))
                        `((setf ,a (ldb (byte 64 0) (+ ,a (* 19 (ash ,e -51))))
                                ,e (logand +limb-mask+ ,e))))))))
  (defun store-limbs (vars)
    `(let ((out (make-array ,+limbs+ :element-type '(unsigned-byte 64))))
       ,@(loop for x in vars for i below +limbs+ collect `(setf (aref out ,i) ,x))
       out)))

(defmacro define-linear (name subtract)
  (let ((r (limb-vars)))
    `(defun ,name (a b)
       (declare #.clempo.base:*hot* (type field a b))
       (let (,@(loop for x in r for i below +limbs+ collect
                     `(,x ,(if subtract
                               `(- (+ (the (unsigned-byte 51) (aref a ,i))
                                      ,(* 2 (- +limb-mask+ (if (zerop i) 18 0))))
                                   (the (unsigned-byte 51) (aref b ,i)))
                               `(+ (the (unsigned-byte 51) (aref a ,i))
                                   (the (unsigned-byte 51) (aref b ,i)))))))
         (declare (type (unsigned-byte 64) ,@r))
         ,@(normalize-forms r)
         ,(store-limbs r)))))
(define-linear f+ nil)
(define-linear f- t)

(defmacro define-product (name square)
  ;; Let b=2^51. The coefficient of b^k combines terms with i+j=k and
  ;; 19*(terms with i+j=k+5). Each coefficient is at most 95*b^2, so two
  ;; machine words per accumulator suffice. Squares emit only i<=j terms.
  (let ((xs (limb-vars)) (ys (limb-vars)) (r (limb-vars)))
    `(defun ,name (a ,@(unless square '(b)))
       (declare #.clempo.base:*hot* (type field a ,@(unless square '(b))))
       (let (,@(limb-load xs 'a) ,@(unless square (limb-load ys 'b))
             ,@(loop for x in r collect `(,x 0)) (carry 0))
         (declare (type (unsigned-byte 64) ,@xs ,@(unless square ys) ,@r carry))
         ,@(loop for k below +limbs+ for out in r collect
                 `(let ((lo carry) (hi 0))
                    (declare (type (unsigned-byte 64) lo hi))
                    ,@(loop for i below +limbs+ for j = (mod (- k i) +limbs+)
                            unless (and square (> i j)) collect
                            (let ((x (nth i xs)) (y (nth j (if square xs ys)))
                                  (scale (* (if (> (+ i j) k) 19 1) (if (and square (/= i j)) 2 1))))
                              `(multiple-value-bind (upper lower)
                                   (sb-bignum:%multiply-and-add ,x ,(if (= scale 1) y `(* ,scale ,y)) lo)
                                 (declare (type (unsigned-byte 64) upper lower))
                                 (setf hi (ldb (byte 64 0) (+ hi upper)) lo lower))))
                    (setf ,out (logand +limb-mask+ lo)
                          carry (logior (ldb (byte 64 0) (ash hi 13)) (ash lo -51)))))
         (setf ,(first r) (ldb (byte 64 0) (+ ,(first r) (* 19 carry))))
         ,@(normalize-forms r)
         ,(store-limbs r)))))
(define-product f* nil)
(define-product sq t)

(defun squares (x n)
  (dotimes (i n x) (setf x (sq x))))

(defun pow2523 (x)
  "Exponentiate to 2^252 - 3, extending runs of one-bits by squaring and multiplying."
  (let* ((x2 (sq x)) (x9 (f* (squares x2 2) x)) (x11 (f* x9 x2))
         (x5 (f* (sq x11) x9))                 ; x^(2^5 - 1)
         (x10 (f* (squares x5 5) x5))
         (x20 (f* (squares x10 10) x10))
         (x40 (f* (squares x20 20) x20))
         (x50 (f* (squares x40 10) x10))
         (x100 (f* (squares x50 50) x50))
         (x200 (f* (squares x100 100) x100))
         (x250 (f* (squares x200 50) x50)))
    (f* (squares x250 2) x)))

(defun inverse (x) (f* (squares (pow2523 x) 3) (f* x (sq x))))

(defun integer-le (data &optional (start 0) (end (length data)))
  (declare (type octets data))
  (loop with result = 0 for i from (1- end) downto start do
    (setf result (logior (ash result 8) (aref data i))) finally (return result)))

(defun octets-le (x &optional (n 32))
  (let ((out (make-octets n)))
    (dotimes (i n out) (setf (aref out i) (ldb (byte 8 (* i 8)) x)))))

(defun clamped-octets (secret)
  "RFC 7748 clamping of a 32-octet little-endian scalar, as octets."
  (let ((k (subseq secret 0 32)))
    (setf (aref k 0) (logand (aref k 0) 248)
          (aref k 31) (logior 64 (logand (aref k 31) 127)))
    k))
