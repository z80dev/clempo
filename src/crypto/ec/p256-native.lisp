;;; P256 verification arenas: canonical four-word Montgomery residues, R=2^256.
;;; Offsets count UB64 words. Exact aliases are supported, partial overlap is not.
;;; Scratch is caller-owned and disjoint. Arithmetic never exports boxed integers.
(in-package #:clempo.p256)

(deftype p256-arena () '(simple-array (unsigned-byte 64) (*)))

(defmacro define-p256-residue (prefix modulus)
  (let* ((p (symbol-value modulus))
         (ps (loop for i below 4 collect (ldb (byte 64 (* i 64)) p)))
         (factor (mod (- (clempo.ec:invert (first ps) (ash 1 64))) (ash 1 64))))
    (labels ((name (suffix) (intern (format nil "~A-~A" prefix suffix)))
             (ge (words i)
               (if (zerop i) `(>= ,(nth i words) ,(nth i ps))
                   `(or (> ,(nth i words) ,(nth i ps))
                        (and (= ,(nth i words) ,(nth i ps)) ,(ge words (1- i))))))
             (subtract (words carry)
               (loop for word in words for pword in ps collect
                 `(setf (values ,word ,carry)
                        (sb-bignum::%subtract-with-borrow ,word ,pword ,carry)))))
      `(progn
         (defparameter ,(name "R2")
           (make-array 4 :element-type '(unsigned-byte 64)
                       :initial-contents ',(loop for i below 4 collect
                                             (ldb (byte 64 (* i 64)) (mod (ash 1 512) p)))))
         (defun ,(name "ONE!") (out o)
           (declare #.clempo.base:*hot* (type p256-arena out) (type fixnum o))
           ,@(loop for i below 4 collect
               `(setf (aref out (+ o ,i)) ,(ldb (byte 64 (* i 64)) (mod (ash 1 256) p))))
           (values))
         ,@(loop for addp in '(t nil) collect
             (let ((words (loop repeat 4 collect (gensym "W"))))
               `(defun ,(name (if addp "ADD!" "SUB!")) (out o a ao b bo)
                  (declare #.clempo.base:*hot* (type p256-arena out a b)
                           (type fixnum o ao bo))
                  (let (,@(loop for word in words collect `(,word 0)) (carry ,(if addp 0 1)))
                    (declare (type (unsigned-byte 64) ,@words) (type bit carry))
                    ,@(loop for word in words for i from 0 collect
                        `(setf (values ,word carry)
                               (,(if addp 'sb-bignum::%add-with-carry 'sb-bignum::%subtract-with-borrow)
                                (aref a (+ ao ,i)) (aref b (+ bo ,i)) carry)))
                    ,(if addp
                         `(when (or (= carry 1) ,(ge words 3))
                            (setf carry 1) ,@(subtract words 'carry))
                         `(when (zerop carry)
                            ,@(loop for word in words for pword in ps collect
                                `(setf (values ,word carry)
                                       (sb-bignum::%add-with-carry ,word ,pword carry)))))
                    ,@(loop for word in words for i from 0 collect `(setf (aref out (+ o ,i)) ,word)))
                  (values))))
         ,(let ((as (loop repeat 4 collect (gensym "A")))
                (bs (loop repeat 4 collect (gensym "B")))
                (rs (loop repeat 9 collect (gensym "R"))))
            `(defun ,(name "MUL!") (out o a ao b bo)
               (declare #.clempo.base:*hot* (type p256-arena out a b)
                        (type fixnum o ao bo))
               (let (,@(loop for a in as for i from 0 collect `(,a (aref a (+ ao ,i))))
                     ,@(loop for b in bs for i from 0 collect `(,b (aref b (+ bo ,i))))
                     ,@(loop for r in rs collect `(,r 0)) (carry 0) (m 0) (bit 0))
                 (declare (type (unsigned-byte 64) ,@as ,@bs ,@rs carry m) (type bit bit))
                 ,@(loop for a in as for i from 0 append
                     `((setf carry 0)
                       ,@(loop for b in bs for j from 0 collect
                           `(setf (values carry ,(nth (+ i j) rs))
                                  (clempo.secp256k1::mul-add-word ,a ,b ,(nth (+ i j) rs) carry)))
                       (setf ,(nth (+ i 4) rs) carry)))
                 ;; REDC with an explicit ninth word: both moduli are close to R.
                 ,@(loop for i below 4 append
                     `((setf m (ldb (byte 64 0) (* ,(nth i rs) ,factor)) carry 0)
                       ,@(loop for pword in ps for j from 0 collect
                           `(setf (values carry ,(nth (+ i j) rs))
                                  (clempo.secp256k1::mul-add-word m ,pword ,(nth (+ i j) rs) carry)))
                       (setf (values ,(nth (+ i 4) rs) bit)
                             (sb-bignum::%add-with-carry ,(nth (+ i 4) rs) carry 0))
                       ,@(loop for j from (+ i 5) below 9 collect
                           `(setf (values ,(nth j rs) bit)
                                  (sb-bignum::%add-with-carry ,(nth j rs) 0 bit)))))
                 (when (or (plusp ,(nth 8 rs)) ,(ge (subseq rs 4 8) 3))
                   (setf bit 1) ,@(subtract (subseq rs 4 8) 'bit))
                 ,@(loop for r in (subseq rs 4 8) for i from 0 collect `(setf (aref out (+ o ,i)) ,r)))
               (values)))
         (defun ,(name "SQUARE!") (out o a ao)
           (declare #.clempo.base:*hot* (type p256-arena out a) (type fixnum o ao))
           (,(name "MUL!") out o a ao a ao))
         (defun ,(name "FROM-INTEGER!") (out o n)
           "Import a canonical integer without constructing shifted bignums."
           (declare #.clempo.base:*hot* (type p256-arena out) (type fixnum o)
                    (type (integer 0 ,(1- p)) n))
           ,@(loop for i below 4 collect
               `(setf (aref out (+ o ,i)) (clempo.secp256k1::integer-limb n ,i)))
           (,(name "MUL!") out o out o ,(name "R2") 0))
         (defun ,(name "INV!") (out o a ao work wo)
           "Invert a nonzero residue; eight disjoint scratch words. Zero maps to zero."
           (declare #.clempo.base:*hot* (type p256-arena out a work) (type fixnum o ao wo))
           (p256-copy! work wo a ao)
           (,(name "ONE!") work (+ wo 4))
           (loop for bit of-type fixnum downfrom 255 to 0 do
             (,(name "SQUARE!") work (+ wo 4) work (+ wo 4))
             (when (logbitp bit ,(- p 2))
               (,(name "MUL!") work (+ wo 4) work (+ wo 4) work wo)))
           (p256-copy! out o work (+ wo 4)))))))

(defun p256-zero! (out o)
  (declare #.clempo.base:*hot* (type p256-arena out) (type fixnum o))
  (dotimes (i 4) (setf (aref out (+ o i)) 0)) (values))
(defun p256-copy! (out o a ao)
  (declare #.clempo.base:*hot* (type p256-arena out a) (type fixnum o ao))
  (dotimes (i 4) (setf (aref out (+ o i)) (aref a (+ ao i)))) (values))
(defun p256-zero-p (a ao)
  (declare #.clempo.base:*hot* (type p256-arena a) (type fixnum ao))
  (zerop (logior (aref a ao) (aref a (+ ao 1)) (aref a (+ ao 2)) (aref a (+ ao 3)))))
(defun p256-equal-p (a ao b bo)
  (declare #.clempo.base:*hot* (type p256-arena a b) (type fixnum ao bo))
  (dotimes (i 4 t) (unless (= (aref a (+ ao i)) (aref b (+ bo i))) (return nil))))

(define-p256-residue pf +p256-p+)
(define-p256-residue ps +p256-n+)

(defparameter *p256-integer-one*
  (make-array 4 :element-type '(unsigned-byte 64) :initial-contents '(1 0 0 0)))

(defun p256-jzero! (out o)
  (declare #.clempo.base:*hot* (type p256-arena out) (type fixnum o))
  (p256-zero! out o) (pf-one! out (+ o 4)) (p256-zero! out (+ o 8)))
(defun p256-jcopy! (out o p po)
  (declare #.clempo.base:*hot* (type p256-arena out p) (type fixnum o po))
  (dotimes (i 12) (setf (aref out (+ o i)) (aref p (+ po i)))) (values))

(defun p256-jdouble! (out o p po work wo)
  "Jacobian doubling for a=-3; 32 disjoint scratch words."
  (declare #.clempo.base:*hot* (type p256-arena out p work) (type fixnum o po wo))
  (if (or (p256-zero-p p (+ po 8)) (p256-zero-p p (+ po 4)))
      (p256-jzero! out o)
      (let ((yy wo) (s (+ wo 4)) (m (+ wo 8)) (t0 (+ wo 12))
            (x (+ wo 16)) (y (+ wo 20)) (z (+ wo 24)) (t1 (+ wo 28)))
        (declare (type fixnum yy s m t0 x y z t1))
        (pf-square! work yy p (+ po 4))
        (pf-mul! work s p po work yy)
        (pf-add! work s work s work s) (pf-add! work s work s work s)
        (pf-square! work t0 p (+ po 8))
        (pf-sub! work m p po work t0) (pf-add! work t1 p po work t0)
        (pf-mul! work m work m work t1)
        (pf-add! work t0 work m work m) (pf-add! work m work m work t0)
        (pf-square! work x work m)
        (pf-sub! work x work x work s) (pf-sub! work x work x work s)
        (pf-sub! work y work s work x) (pf-mul! work y work y work m)
        (pf-square! work t0 work yy)
        (pf-add! work t0 work t0 work t0) (pf-add! work t0 work t0 work t0)
        (pf-add! work t0 work t0 work t0) (pf-sub! work y work y work t0)
        (pf-mul! work z p (+ po 4) p (+ po 8)) (pf-add! work z work z work z)
        (p256-copy! out o work x) (p256-copy! out (+ o 4) work y)
        (p256-copy! out (+ o 8) work z)))
  (values))

(defun p256-jadd-affine! (out o p po q qo work wo)
  "Mixed addition; Q has Z=1 or 0. Forty-four disjoint scratch words."
  (declare #.clempo.base:*hot* (type p256-arena out p q work) (type fixnum o po qo wo))
  (cond ((p256-zero-p q (+ qo 8)) (p256-jcopy! out o p po))
        ((p256-zero-p p (+ po 8)) (p256-jcopy! out o q qo))
        (t
         (let ((zz wo) (u (+ wo 4)) (v (+ wo 8)) (h (+ wo 12))
               (r (+ wo 16)) (hh (+ wo 20)) (hhh (+ wo 24)) (xh (+ wo 28))
               (x (+ wo 32)) (y (+ wo 36)) (z (+ wo 40)))
           (declare (type fixnum zz u v h r hh hhh xh x y z))
           (pf-square! work zz p (+ po 8)) (pf-mul! work u q qo work zz)
           (pf-mul! work v work zz p (+ po 8)) (pf-mul! work v work v q (+ qo 4))
           (pf-sub! work h work u p po) (pf-sub! work r work v p (+ po 4))
           (if (p256-zero-p work h)
               (if (p256-zero-p work r) (p256-jdouble! out o p po work wo) (p256-jzero! out o))
               (progn
                 (pf-square! work hh work h) (pf-mul! work hhh work h work hh)
                 (pf-mul! work xh p po work hh) (pf-square! work x work r)
                 (pf-sub! work x work x work hhh)
                 (pf-sub! work x work x work xh) (pf-sub! work x work x work xh)
                 (pf-sub! work y work xh work x) (pf-mul! work y work y work r)
                 (pf-mul! work v p (+ po 4) work hhh) (pf-sub! work y work y work v)
                 (pf-mul! work z p (+ po 8) work h)
                 (p256-copy! out o work x) (p256-copy! out (+ o 4) work y)
                 (p256-copy! out (+ o 8) work z))))))
  (values))

(defun p256-table! (table to q qo work wo)
  "Build 0..15 Q, affine, with one inversion; 112 scratch words.
Q is a nonzero affine prime-order point and must be disjoint from TABLE."
  (declare #.clempo.base:*hot* (type p256-arena table q work) (type fixnum to qo wo))
  (p256-jzero! table to) (p256-jcopy! table (+ to 12) q qo)
  (loop for i of-type fixnum from 2 below 16 do
    (p256-jadd-affine! table (+ to (* i 12)) table (+ to (* (1- i) 12)) q qo work (+ wo 64)))
  (pf-one! work wo)
  (loop for i of-type fixnum from 1 below 16 do
    (pf-mul! work (+ wo (* i 4)) work (+ wo (* (1- i) 4)) table (+ to (* i 12) 8)))
  (pf-inv! work (+ wo 64) work (+ wo 60) work (+ wo 68))
  (loop for i of-type fixnum downfrom 15 to 1
        for p of-type fixnum = (+ to (* i 12)) do
    (pf-mul! work (+ wo 68) work (+ wo 64) work (+ wo (* (1- i) 4)))
    (pf-mul! work (+ wo 64) work (+ wo 64) table (+ p 8))
    (pf-square! work (+ wo 72) work (+ wo 68))
    (pf-mul! table p table p work (+ wo 72))
    (pf-mul! work (+ wo 72) work (+ wo 72) work (+ wo 68))
    (pf-mul! table (+ p 4) table (+ p 4) work (+ wo 72))
    (pf-one! table (+ p 8)))
  (values))

(defparameter *p256-constants*
  (let ((a (make-array 20 :element-type '(unsigned-byte 64) :initial-element 0)))
    (pf-from-integer! a 0 #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296)
    (pf-from-integer! a 4 #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)
    (pf-one! a 8)
    (pf-from-integer! a 12 #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b)
    (pf-from-integer! a 16 +p256-n+)
    a))
(defparameter *p256-generator-table*
  (let ((table (make-array 192 :element-type '(unsigned-byte 64) :initial-element 0))
        (work (make-array 112 :element-type '(unsigned-byte 64) :initial-element 0)))
    (p256-table! table 0 *p256-constants* 0 work 0) table))

(defun p256-affine-on-curve-p (p po work wo)
  (declare #.clempo.base:*hot* (type p256-arena p work) (type fixnum po wo))
  (pf-square! work wo p po) (pf-mul! work wo work wo p po)
  (pf-sub! work wo work wo p po) (pf-sub! work wo work wo p po)
  (pf-sub! work wo work wo p po) (pf-add! work wo work wo *p256-constants* 12)
  (pf-square! work (+ wo 4) p (+ po 4))
  (p256-equal-p work wo work (+ wo 4)))

(defun p256-joint! (out o u uo v vo table to work wo)
  "Joint radix-16 multiplication; U,V are ordinary scalar limbs. Scratch: 44 words."
  (declare #.clempo.base:*hot* (type p256-arena out u v table work)
           (type fixnum o uo vo to wo))
  (p256-jzero! out o)
  (loop for i of-type fixnum downfrom 63 to 0 do
    (dotimes (j 4) (p256-jdouble! out o out o work wo))
    (let ((a (ldb (byte 4 (* 4 (logand i 15))) (aref u (+ uo (ash i -4)))))
          (b (ldb (byte 4 (* 4 (logand i 15))) (aref v (+ vo (ash i -4))))))
      (declare (type fixnum a b))
      (unless (zerop a)
        (p256-jadd-affine! out o out o *p256-generator-table* (* a 12) work wo))
      (unless (zerop b)
        (p256-jadd-affine! out o out o table (+ to (* b 12)) work wo))))
  (values))
