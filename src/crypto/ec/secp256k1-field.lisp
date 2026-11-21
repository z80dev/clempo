(defpackage #:clempo.secp256k1
  (:use #:cl)
  (:import-from #:clempo.base #:octets #:octets->integer #:integer->octets #:octets-concat)
  (:export #:ecrecover #:pubkey->address #:sign #:private->public #:ecdh
           #:+secp256k1-n+ #:+secp256k1-p+))
(in-package #:clempo.secp256k1)

(defconstant +secp256k1-p+ #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f)
(defconstant +secp256k1-n+ #xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)

(defmacro integer-limb (integer index)
  ;; Positive integers only. Read sign-extension words as zero, not past the object.
  `(if (typep ,integer 'fixnum)
       ,(if (zerop index) integer 0)
       (if (< ,index (sb-bignum:%bignum-length ,integer))
           (sb-bignum::%bignum-ref ,integer ,index) 0)))

(declaim (inline mul-add-word))
(defun mul-add-word (a b c d)
  (declare #.clempo.base:*hot* (type (unsigned-byte 64) a b c d))
  ;; Use the three-argument intrinsic: SBCL 2.6.8 arm64's four-argument
  ;; VOP permits its HI output to alias Y before MUL consumes Y.
  (multiple-value-bind (hi lo) (sb-bignum::%multiply-and-add a b c)
    (multiple-value-bind (low carry) (sb-bignum::%add-with-carry lo d 0)
      (values (ldb (byte 64 0) (+ hi carry)) low))))

(declaim (inline pack-field))
(defun pack-field (r0 r1 r2 r3)
  (declare #.clempo.base:*hot* (type (unsigned-byte 64) r0 r1 r2 r3))
  (when (and (zerop r3) (zerop r2) (zerop r1))
    (return-from pack-field r0))
  (let ((out (sb-bignum::%allocate-bignum 5)))
    (setf (sb-bignum::%bignum-ref out 0) r0
          (sb-bignum::%bignum-ref out 1) r1
          (sb-bignum::%bignum-ref out 2) r2
          (sb-bignum::%bignum-ref out 3) r3
          (sb-bignum::%bignum-ref out 4) 0)
    (if (plusp r3)
        (progn
          (unless (logbitp 63 r3) (sb-bignum::%bignum-set-length out 4))
          out)
        (sb-bignum::%normalize-bignum out 5))))

(defmacro fold-field-words (w0 w1 w2 w3 w4 w5 w6 w7)
  (let ((r0 (gensym)) (r1 (gensym)) (r2 (gensym)) (r3 (gensym))
        (hi (gensym)) (carry (gensym)))
    `(let ((,r0 0) (,r1 0) (,r2 0) (,r3 0) (,hi 0) (,carry 0))
       (declare (type (unsigned-byte 64) ,r0 ,r1 ,r2 ,r3 ,hi) (type bit ,carry))
       (setf (values ,hi ,r0) (mul-add-word ,w4 #x1000003d1 ,w0 0))
       (setf (values ,hi ,r1) (mul-add-word ,w5 #x1000003d1 ,w1 ,hi))
       (setf (values ,hi ,r2) (mul-add-word ,w6 #x1000003d1 ,w2 ,hi))
       (setf (values ,hi ,r3) (mul-add-word ,w7 #x1000003d1 ,w3 ,hi))
       ;; hi <= 2^32+977. The second fold overflows by at most one bit.
       (setf (values ,hi ,r0) (sb-bignum::%multiply-and-add ,hi #x1000003d1 ,r0))
       (setf (values ,r1 ,carry) (sb-bignum::%add-with-carry ,r1 ,hi 0))
       (setf (values ,r2 ,carry) (sb-bignum::%add-with-carry ,r2 0 ,carry))
       (setf (values ,r3 ,carry) (sb-bignum::%add-with-carry ,r3 0 ,carry))
       (when (= ,carry 1)
         (setf (values ,r0 ,carry) (sb-bignum::%add-with-carry ,r0 #x1000003d1 0))
         (setf (values ,r1 ,carry) (sb-bignum::%add-with-carry ,r1 0 ,carry))
         (setf (values ,r2 ,carry) (sb-bignum::%add-with-carry ,r2 0 ,carry))
         (setf ,r3 (sb-bignum::%add-with-carry ,r3 0 ,carry)))
       (when (and (= ,r3 #xffffffffffffffff) (= ,r2 #xffffffffffffffff)
                  (= ,r1 #xffffffffffffffff) (>= ,r0 #xfffffffefffffc2f))
         (setf ,r0 (ldb (byte 64 0) (+ ,r0 #x1000003d1)) ,r1 0 ,r2 0 ,r3 0))
       (pack-field ,r0 ,r1 ,r2 ,r3))))

(defun reduce-field (x)
  "Fold a 512-bit product using 2^256 = 2^32+977; allocate only the result."
  (declare #.clempo.base:*hot* (type integer x))
  (unless (typep x '(unsigned-byte 512))
    (return-from reduce-field (mod x +secp256k1-p+)))
  (when (< x +secp256k1-p+) (return-from reduce-field x))
  (fold-field-words (integer-limb x 0) (integer-limb x 1)
                    (integer-limb x 2) (integer-limb x 3)
                    (integer-limb x 4) (integer-limb x 5)
                    (integer-limb x 6) (integer-limb x 7)))

(defmacro define-field-multiply ()
  (let ((as (loop repeat 4 collect (gensym "A")))
        (bs (loop repeat 4 collect (gensym "B")))
        (rs (loop repeat 8 collect (gensym "R")))
        (carry (gensym "CARRY")))
    `(defun field-multiply (x y)
       (declare #.clempo.base:*hot*
                (type (integer 0 #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e) x y))
       (let (,@(loop for a in as for i from 0 collect `(,a (integer-limb x ,i)))
             ,@(loop for b in bs for i from 0 collect `(,b (integer-limb y ,i)))
             ,@(loop for r in rs collect `(,r 0)) (,carry 0))
         (declare (type (unsigned-byte 64) ,@as ,@bs ,@rs ,carry))
         ,@(loop for a in as for i from 0 append
             (append (list `(setf ,carry 0))
                     (loop for b in bs for j from 0
                           for r = (nth (+ i j) rs)
                           collect `(setf (values ,carry ,r) (mul-add-word ,a ,b ,r ,carry)))
                     (list `(setf ,(nth (+ i 4) rs) ,carry))))
         (fold-field-words ,@rs)))))

(define-field-multiply)

(defun field-scale (x scale)
  (declare #.clempo.base:*hot*
           (type (integer 0 #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e) x)
           (type (integer 0 8) scale))
  (when (= scale 1) (return-from field-scale x))
  (when (zerop scale) (return-from field-scale 0))
  (let ((r0 0) (r1 0) (r2 0) (r3 0) (hi 0))
    (declare (type (unsigned-byte 64) r0 r1 r2 r3 hi))
    (setf (values hi r0) (sb-bignum::%multiply-and-add (integer-limb x 0) scale hi))
    (setf (values hi r1) (sb-bignum::%multiply-and-add (integer-limb x 1) scale hi))
    (setf (values hi r2) (sb-bignum::%multiply-and-add (integer-limb x 2) scale hi))
    (setf (values hi r3) (sb-bignum::%multiply-and-add (integer-limb x 3) scale hi))
    (fold-field-words r0 r1 r2 r3 hi 0 0 0)))

(defmacro define-field-add-subtract ()
  `(progn
     ,@(loop for subtract in '(nil t)
             for name in '(field-add field-subtract)
             for operation = (if subtract 'sb-bignum::%subtract-with-borrow 'sb-bignum::%add-with-carry)
             for init = (if subtract 1 0)
             collect
       `(defun ,name (x y)
          (declare #.clempo.base:*hot*
                   (type (integer 0 #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e) x y))
          (let ((r0 0) (r1 0) (r2 0) (r3 0) (carry ,init))
            (declare (type (unsigned-byte 64) r0 r1 r2 r3) (type bit carry))
            ,@(loop for r in '(r0 r1 r2 r3) for i from 0
                    collect `(setf (values ,r carry)
                                   (,operation (integer-limb x ,i) (integer-limb y ,i) carry)))
            ,(if subtract
                 `(when (zerop carry)
                    ;; Underflow: add p, i.e. subtract c modulo 2^256.
                    (setf (values r0 carry) (sb-bignum::%subtract-with-borrow r0 #x1000003d1 1))
                    (setf (values r1 carry) (sb-bignum::%subtract-with-borrow r1 0 carry))
                    (setf (values r2 carry) (sb-bignum::%subtract-with-borrow r2 0 carry))
                    (setf r3 (sb-bignum::%subtract-with-borrow r3 0 carry)))
                 `(if (= carry 1)
                      (progn
                        (setf (values r0 carry) (sb-bignum::%add-with-carry r0 #x1000003d1 0))
                        (setf (values r1 carry) (sb-bignum::%add-with-carry r1 0 carry))
                        (setf (values r2 carry) (sb-bignum::%add-with-carry r2 0 carry))
                        (setf r3 (sb-bignum::%add-with-carry r3 0 carry)))
                      (when (and (= r3 #xffffffffffffffff) (= r2 #xffffffffffffffff)
                                 (= r1 #xffffffffffffffff) (>= r0 #xfffffffefffffc2f))
                        (setf r0 (ldb (byte 64 0) (+ r0 #x1000003d1)) r1 0 r2 0 r3 0))))
            (pack-field r0 r1 r2 r3))))))

(define-field-add-subtract)
