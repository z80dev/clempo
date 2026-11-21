(defpackage #:clempo.x25519
  (:use #:cl #:clempo.base)
  (:export #:x25519 #:x25519-public-key))
(defpackage #:clempo.ed25519
  (:use #:cl #:clempo.base)
  (:export #:ed25519-public-key #:ed25519-sign #:ed25519-verify))
(in-package #:clempo.x25519)

(defconstant +p+ (- (ash 1 255) 19))

(declaim (inline f+ f- reduce-product f* sq))
(defun f+ (x y) (let ((sum (+ x y))) (if (< sum +p+) sum (- sum +p+))))
(defun f- (x y) (let ((diff (- x y))) (if (minusp diff) (+ diff +p+) diff)))
(defun reduce-product (x)
  (let* ((r (+ (ldb (byte 255 0) x) (* 19 (ash x -255))))
         (s (+ (ldb (byte 255 0) r) (* 19 (ash r -255)))))
    (if (< s +p+) s (- s +p+))))
(defun f* (x y) (reduce-product (* x y)))
(defun sq (x) (f* x x))

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

(defun clamped-scalar (bytes)
  (logior (logand (integer-le bytes 0 32) (- (ash 1 254) 8)) (ash 1 254)))

(defun x25519 (secret peer-public)
  "RFC 7748 raw shared coordinate. Callers must reject an all-zero shared secret."
  (check-type secret octets)
  (check-type peer-public octets)
  (assert (= (length secret) (length peer-public) 32))
  (let ((scalar (clamped-scalar secret))
        (u (mod (ldb (byte 255 0) (integer-le peer-public)) +p+)))
    (let ((x2 1) (z2 0) (x3 u) (z3 1) (previous 0))
      (loop for i from 254 downto 0 for bit = (ldb (byte 1 i) scalar) do
        (unless (= bit previous) (rotatef x2 x3) (rotatef z2 z3))
        (setf previous bit)
        (let* ((a (f+ x2 z2)) (aa (sq a)) (b (f- x2 z2)) (bb (sq b)) (e (f- aa bb))
               (c (f+ x3 z3)) (d (f- x3 z3)) (da (f* d a)) (cb (f* c b)))
          (setf x3 (sq (f+ da cb)) z3 (f* u (sq (f- da cb)))
                x2 (f* aa bb) z2 (f* e (f+ aa (f* 121665 e))))))
      ;; Scalar clamping fixes the final bit to zero, hence the final swap is a no-op.
      (octets-le (f* x2 (inverse z2))))))

(defparameter *base-u* (octets-le 9))
(defun x25519-public-key (secret) (x25519 secret *base-u*))

(in-package #:clempo.ed25519)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clempo.x25519::f+ clempo.x25519::f- clempo.x25519::f* clempo.x25519::sq
            clempo.x25519::inverse clempo.x25519::pow2523 clempo.x25519::+p+
            clempo.x25519::octets-le clempo.x25519::integer-le clempo.x25519::clamped-scalar)))

(defconstant +d+ 37095705934669439343138083508754565189542113879843219016388785533085940283555)
(defconstant +2d+ 16295367250680780974490674513165176452449235426866156013048779062215315747161)
(defconstant +sqrt-minus-one+ 19681161376707505956807079304988542015446066515923890162744021073123829784752)
(defconstant +order+ (+ (ash 1 252) 27742317777372353535851937790883648493))

(defstruct (point (:constructor point (x y z xy)))
  (x 0 :type (integer 0) :read-only t)
  (y 1 :type (integer 0) :read-only t)
  (z 1 :type (integer 0) :read-only t)
  (xy 0 :type (integer 0) :read-only t))

(defstruct (cached (:constructor %cached (ymx ypx z dxy)))
  (ymx 1 :type (integer 0) :read-only t)
  (ypx 1 :type (integer 0) :read-only t)
  (z 1 :type (integer 0) :read-only t)
  (dxy 0 :type (integer 0) :read-only t))

(defparameter *identity* (point 0 1 1 0))
(defparameter *base*
  (let ((x 15112221349535400772501151409588531511454012693041857206046113283949847762202)
        (y 46316835694926478169428394003475163141307993866256225615783033603165251855960))
    (point x y 1 (f* x y))))

(defun cache (p)
  (%cached (f- (point-y p) (point-x p)) (f+ (point-y p) (point-x p))
           (point-z p) (f* +2d+ (point-xy p))))

(defun add (p q affine)
  (declare #.clempo.base:*hot* (type point p) (type cached q))
  (let* ((a (f* (f- (point-y p) (point-x p)) (cached-ymx q)))
         (b (f* (f+ (point-y p) (point-x p)) (cached-ypx q)))
         (c (f* (point-xy p) (cached-dxy q)))
         (z (if affine (point-z p) (f* (point-z p) (cached-z q))))
         (d (f+ z z)) (e (f- b a)) (f (f- d c)) (g (f+ d c)) (h (f+ b a)))
    (point (f* e f) (f* g h) (f* f g) (f* e h))))

(defun double-point (p)
  (declare #.clempo.base:*hot* (type point p))
  (let* ((a (sq (point-x p))) (b (sq (point-y p))) (z (sq (point-z p)))
         (c (f+ z z)) (d (f- 0 a)) (h (f- d b))
         (e (f- (sq (f+ (point-x p) (point-y p))) (f+ a b)))
         (g (f+ d b)) (f (f- g c)))
    (point (f* e f) (f* g h) (f* f g) (f* e h))))

(defun negate (p)
  (point (f- 0 (point-x p)) (point-y p) (point-z p) (f- 0 (point-xy p))))

(defun to-affine (p)
  (let* ((inv (inverse (point-z p))) (x (f* (point-x p) inv)) (y (f* (point-y p) inv)))
    (point x y 1 (f* x y))))

(defun encode-point (p)
  (let ((p (to-affine p)))
    (octets-le (logior (ash (logand (point-x p) 1) 255) (point-y p)))))

(defun decode-point (data &optional (start 0))
  ;; ZIP-215: the field coordinate may be noncanonical; the sign bit on x=0
  ;; does not invalidate the point. The original octets enter the challenge.
  (let* ((n (integer-le data start (+ start 32))) (y (mod (ldb (byte 255 0) n) +p+))
         (yy (sq y)) (u (f- yy 1)) (v (f+ 1 (f* +d+ yy)))
         (v3 (f* (sq v) v)) (v7 (f* v (sq v3)))
         (x (f* (f* u v3) (pow2523 (f* u v7))))
         (check (f* v (sq x))))
    (cond ((= check u))
          ((= check (f- 0 u)) (setf x (f* x +sqrt-minus-one+)))
          (t (return-from decode-point nil)))
    (unless (= (logand x 1) (ldb (byte 1 255) n)) (setf x (f- 0 x)))
    (point x y 1 (f* x y))))

(defun base-multiples ()
  ;; A comb: precompute j*16^i*B, so fixed-base multiplication has no doubling.
  (let ((table (make-array '(64 16))) (base *base*))
    (dotimes (i 64 table)
      (let ((base-cache (cache base)) (p *identity*))
        (dotimes (j 16)
          (setf (aref table i j) (cache (to-affine p)))
          (setf p (add p base-cache nil))))
      (dotimes (j 4) (setf base (double-point base))))))

(defparameter *fixed-base* (base-multiples))

(defun base-mul (scalar)
  (let ((p *identity*))
    (dotimes (i 64 p)
      (setf p (add p (aref *fixed-base* i (ldb (byte 4 (* i 4)) scalar)) t)))))

(defun multiply (scalar base)
  (let ((table (make-array 16)) (cached-base (cache base)) (p base))
    (loop for i from 1 below 16 do (setf (aref table i) (cache p) p (add p cached-base nil)))
    (setf p *identity*)
    (loop for offset from 252 downto 0 by 4 for nibble = (ldb (byte 4 offset) scalar) do
      (dotimes (i 4) (setf p (double-point p)))
      (unless (zerop nibble) (setf p (add p (aref table nibble) nil))))
    p))

(defun seed-hash (seed)
  (check-type seed octets)
  (assert (= (length seed) 32))
  (clempo.sha2:sha512 seed))

(defun ed25519-public-key (seed)
  (encode-point (base-mul (clamped-scalar (seed-hash seed)))))

(defun challenge (nonce-public start public-key message)
  (let ((ctx (clempo.sha2:make-sha512)))
    (clempo.sha2:sha512-update ctx nonce-public :start start :end (+ start 32))
    (clempo.sha2:sha512-update ctx public-key)
    (clempo.sha2:sha512-update ctx message)
    (mod (integer-le (clempo.sha2:sha512-final ctx)) +order+)))

(defun ed25519-sign (seed message)
  (check-type message octets)
  (let* ((digest (seed-hash seed)) (scalar (clamped-scalar digest))
         (public-key (encode-point (base-mul scalar)))
         (ctx (clempo.sha2:make-sha512)))
    (clempo.sha2:sha512-update ctx digest :start 32)
    (clempo.sha2:sha512-update ctx message)
    (let* ((r (mod (integer-le (clempo.sha2:sha512-final ctx)) +order+))
           (encoded-r (encode-point (base-mul r)))
           (k (challenge encoded-r 0 public-key message)))
      (octets-concat encoded-r (octets-le (mod (+ r (* k scalar)) +order+))))))

(defun ed25519-verify (public-key message signature)
  "Verify pure Ed25519 using Commonware's ZIP-215 (cofactored) rules."
  (check-type public-key octets)
  (check-type message octets)
  (check-type signature octets)
  (when (= (length public-key) 32)
    (when (= (length signature) 64)
      (let ((s (integer-le signature 32)))
        (when (< s +order+)
          (let ((a (decode-point public-key)) (r (decode-point signature)))
            (when (and a r)
              (let* ((k (challenge signature 0 public-key message))
                     (p (add (base-mul s) (cache (multiply k (negate a))) nil)))
                (setf p (add p (cache (negate r)) nil))
                (dotimes (i 3) (setf p (double-point p)))
                (and (zerop (point-x p)) (= (point-y p) (point-z p)))))))))))
