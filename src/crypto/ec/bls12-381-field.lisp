;;; Raw BLS12-381 Fp. Six little-endian 64-bit Montgomery limbs, R = 2^384.
;;; Operands are canonical residues (< p). Offsets count words, not bytes.
;;; Exact output/input aliases and disjoint regions are supported; partial
;;; overlaps are not. Caller scratch must be disjoint from all operands.
(in-package #:clempo.bls12-381)

(deftype fp-arena () '(simple-array (unsigned-byte 64) (*)))
(deftype fp-offset () `(integer 0 ,(- array-dimension-limit 6)))

(defconstant +fp-montgomery-factor+ #x89f3fffcfffcfffd)

;; Immutable conversion operands, never shared mutable scratch.
(defparameter *fp-r-squared*
  (make-array 6 :element-type '(unsigned-byte 64)
              :initial-contents '(#xf4df1f341c341746 #x0a76e6a609d104f1
                                  #x8de5476c4c95b6d5 #x67eb88a9939d83c0
                                  #x9a793e85b519952d #x11988fe592cae3aa)))
(defparameter *fp-integer-one*
  (make-array 6 :element-type '(unsigned-byte 64) :initial-contents '(1 0 0 0 0 0)))

(declaim (inline fp-multiply-add-word))
(defun fp-multiply-add-word (a b c d)
  (declare #.clempo.base:*hot* (type (unsigned-byte 64) a b c d))
  ;; SBCL 2.6.8 ARM64's four-argument intrinsic can overwrite Y with HI
  ;; before MUL reads Y. The shared three-argument word macro plus ADD avoids
  ;; that intrinsic, and selects portable arithmetic when native support is absent.
  (multiple-value-bind (hi lo) (clempo.ec.word:multiply-and-add a b c)
    (multiple-value-bind (low carry) (clempo.ec.word:add-with-carry lo d 0)
      (values (ldb (byte 64 0) (+ hi carry)) low))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fp-modulus-words ()
    (loop for i below 6 collect (ldb (byte 64 (* i 64)) +p+)))

  (defun fp-reduce-word-forms (words carry)
    ;; All callers produce values below 2p; 2p < R, so no seventh word.
    (let ((ps (fp-modulus-words)))
      (labels ((at-least (i)
                 (if (zerop i)
                     `(>= ,(nth i words) ,(nth i ps))
                     `(or (> ,(nth i words) ,(nth i ps))
                          (and (= ,(nth i words) ,(nth i ps))
                               ,(at-least (1- i)))))))
        `((when ,(at-least 5)
            (setf ,carry 1)
            ,@(loop for word in words for p in ps collect
                `(setf (values ,word ,carry)
                       (clempo.ec.word:subtract-with-borrow ,word ,p ,carry)))))))))

(defmacro define-fp-basic-operations ()
  `(progn
     (defun fp-zero! (out out-offset)
       (declare #.clempo.base:*hot* (type fp-arena out) (type fp-offset out-offset))
       ,@(loop for i below 6 collect `(setf (aref out (+ out-offset ,i)) 0))
       (values))
     (defun fp-one! (out out-offset)
       (declare #.clempo.base:*hot* (type fp-arena out) (type fp-offset out-offset))
       ,@(loop for i below 6
               for word = (ldb (byte 64 (* i 64)) (mod (ash 1 384) +p+))
               collect `(setf (aref out (+ out-offset ,i)) ,word))
       (values))
     (defun fp-copy! (out out-offset a a-offset)
       (declare #.clempo.base:*hot* (type fp-arena out a)
                (type fp-offset out-offset a-offset))
       ,@(loop for i below 6 collect
           `(setf (aref out (+ out-offset ,i)) (aref a (+ a-offset ,i))))
       (values))
     (defun fp-zero-p (a a-offset)
       (declare #.clempo.base:*hot* (type fp-arena a) (type fp-offset a-offset))
       (zerop (logior ,@(loop for i below 6 collect `(aref a (+ a-offset ,i))))))
     (defun fp-equal-p (a a-offset b b-offset)
       (declare #.clempo.base:*hot* (type fp-arena a b)
                (type fp-offset a-offset b-offset))
       (and ,@(loop for i below 6 collect
                `(= (aref a (+ a-offset ,i)) (aref b (+ b-offset ,i))))))))

(define-fp-basic-operations)

(defmacro define-fp-additive-operations ()
  `(progn
     ,@(loop for name in '(fp-add! fp-sub! fp-neg!)
             for addp = (eq name 'fp-add!)
             for negp = (eq name 'fp-neg!)
             for words = (loop repeat 6 collect (gensym "WORD"))
             for carry = (gensym "CARRY")
             collect
         `(defun ,name (out out-offset a a-offset ,@(unless negp '(b b-offset)))
            (declare #.clempo.base:*hot* (type fp-arena out a ,@(unless negp '(b)))
                     (type fp-offset out-offset a-offset ,@(unless negp '(b-offset))))
            (let (,@(loop for word in words collect `(,word 0))
                  (,carry ,(if addp 0 1)))
              (declare (type (unsigned-byte 64) ,@words) (type bit ,carry))
              ,@(loop for word in words for i from 0 collect
                  `(setf (values ,word ,carry)
                         (,(if addp 'clempo.ec.word:add-with-carry
                                    'clempo.ec.word:subtract-with-borrow)
                          ,(if negp 0 `(aref a (+ a-offset ,i)))
                          ,(if negp `(aref a (+ a-offset ,i)) `(aref b (+ b-offset ,i)))
                          ,carry)))
              ,@(if addp
                    (fp-reduce-word-forms words carry)
                    `((when (zerop ,carry)
                        (setf ,carry 0)
                        ,@(loop for word in words for p in (fp-modulus-words) collect
                            `(setf (values ,word ,carry)
                                   (clempo.ec.word:add-with-carry ,word ,p ,carry))))))
              ,@(loop for word in words for i from 0 collect
                  `(setf (aref out (+ out-offset ,i)) ,word)))
            (values)))))

(define-fp-additive-operations)

(defmacro define-fp-multiply ()
  ;; Coarsely integrated operand scanning. Only six accumulator words are
  ;; live between rounds. p < 2^381 leaves ample headroom for the top carries.
  (let ((as (loop repeat 6 collect (gensym "A")))
        (ts (loop repeat 6 collect (gensym "T")))
        (b (gensym "B")) (m (gensym "M"))
        (carry (gensym "CARRY")) (top (gensym "TOP")) (borrow (gensym "BORROW")))
    `(defun fp-mul! (out out-offset a a-offset b b-offset)
       (declare #.clempo.base:*hot* (type fp-arena out a b)
                (type fp-offset out-offset a-offset b-offset))
       (let (,@(loop for word in as for i from 0 collect `(,word (aref a (+ a-offset ,i))))
             ,@(loop for word in ts collect `(,word 0))
             (,b 0) (,m 0) (,carry 0) (,top 0) (,borrow 0))
         (declare (type (unsigned-byte 64) ,@as ,@ts ,b ,m ,carry ,top)
                  (type bit ,borrow))
         ,@(loop for i below 6 append
             (append
              `((setf ,b (aref b (+ b-offset ,i)) ,carry 0))
              (loop for a in as for word in ts collect
                  `(setf (values ,carry ,word)
                         (fp-multiply-add-word ,a ,b ,word ,carry)))
              `((setf ,top ,carry
                      ,m (ldb (byte 64 0) (* ,(first ts) +fp-montgomery-factor+))
                      ,carry 0))
              (loop for word in ts for p in (fp-modulus-words) collect
                  `(setf (values ,carry ,word)
                         (fp-multiply-add-word ,m ,p ,word ,carry)))
              ;; Low word is zero by construction. Divide by 2^64.
              (loop for word in ts for next in (rest ts) collect `(setf ,word ,next))
              `((setf ,(car (last ts)) (ldb (byte 64 0) (+ ,top ,carry))))))
         ,@(fp-reduce-word-forms ts borrow)
         ,@(loop for word in ts for i from 0 collect `(setf (aref out (+ out-offset ,i)) ,word)))
       (values))))

(define-fp-multiply)

(defun fp-square! (out out-offset a a-offset)
  (declare #.clempo.base:*hot* (type fp-arena out a)
           (type fp-offset out-offset a-offset))
  (fp-mul! out out-offset a a-offset a a-offset)
  (values))

(defmacro fp-positive-integer-word (integer index)
  ;; No LDB-created bignums when importing an already canonical integer.
  `(if (typep ,integer 'fixnum)
       ,(if (zerop index) integer 0)
       (if (< ,index (sb-bignum:%bignum-length ,integer))
           (sb-bignum::%bignum-ref ,integer ,index) 0)))

(defmacro define-fp-integer-import ()
  `(defun fp-from-integer! (out out-offset integer)
     "Import INTEGER, reducing modulo p at the integer boundary if necessary."
     (declare #.clempo.base:*hot* (type fp-arena out) (type fp-offset out-offset)
              (type integer integer))
     (let ((n (if (<= 0 integer (1- +p+)) integer (mod integer +p+))))
       ,@(loop for i below 6 collect
           `(setf (aref out (+ out-offset ,i)) (fp-positive-integer-word n ,i))))
     (fp-mul! out out-offset out out-offset *fp-r-squared* 0)
     (values)))

(define-fp-integer-import)

(defun fp-to-integer (a a-offset)
  "Export once at the boundary, without temporary wide bignums or division."
  (declare #.clempo.base:*hot* (type fp-arena a) (type fp-offset a-offset))
  (let ((words (make-array 6 :element-type '(unsigned-byte 64) :initial-element 0)))
    (declare (dynamic-extent words))
    (fp-mul! words 0 a a-offset *fp-integer-one* 0)
    (when (zerop (logior (aref words 1) (aref words 2) (aref words 3)
                        (aref words 4) (aref words 5)))
      (return-from fp-to-integer (aref words 0)))
    (let ((out (sb-bignum::%allocate-bignum 7)))
      (dotimes (i 6) (setf (sb-bignum::%bignum-ref out i) (aref words i)))
      (setf (sb-bignum::%bignum-ref out 6) 0)
      (sb-bignum::%normalize-bignum out 7))))

(defun fp-pow! (out out-offset a a-offset exponent work work-offset)
  "Raise A to a nonnegative integer EXPONENT. WORK has at least 24 disjoint words."
  (declare #.clempo.base:*hot* (type fp-arena out a work)
           (type fp-offset out-offset a-offset work-offset) (type integer exponent))
  (unless (typep exponent '(integer 0 *))
    (error "Negative Fp exponent: ~D" exponent))
  ;; Read exponent bits without shifting/allocating bignums on every bit.
  ;; Reserve 24 words for the common field/tower caller contract; use only 12.
  (let ((base work-offset) (acc (+ work-offset 6)))
    (declare (type fp-offset base acc))
    (fp-copy! work base a a-offset)
    (fp-one! work acc)
    (loop for bit of-type fixnum downfrom (1- (integer-length exponent)) to 0 do
      (fp-square! work acc work acc)
      (when (logbitp bit exponent)
        (fp-mul! work acc work acc work base)))
    (fp-copy! out out-offset work acc))
  (values))

(defun fp-inv! (out out-offset a a-offset work work-offset)
  "Invert nonzero A. WORK has at least 24 disjoint words. Zero is invalid."
  (declare #.clempo.base:*hot* (type fp-arena out a work)
           (type fp-offset out-offset a-offset work-offset))
  (when (fp-zero-p a a-offset) (error 'invalid-point))
  (fp-pow! out out-offset a a-offset #.(- +p+ 2) work work-offset)
  (values))
