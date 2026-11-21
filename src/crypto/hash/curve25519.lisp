(defpackage #:clempo.x25519
  (:use #:cl #:clempo.base #:clempo.field25519)
  (:export #:x25519 #:x25519-public-key))
(defpackage #:clempo.ed25519
  (:use #:cl #:clempo.base #:clempo.field25519)
  (:export #:ed25519-public-key #:ed25519-sign #:ed25519-verify #:ed25519-public-key-valid-p))
(in-package #:clempo.x25519)

(defparameter *a24* (from-integer 121665))

(defun x25519 (secret peer-public)
  "RFC 7748 raw shared coordinate. Callers must reject an all-zero shared secret."
  (check-type secret octets)
  (check-type peer-public octets)
  (assert (= (length secret) (length peer-public) 32))
  (let ((scalar (clamped-scalar secret))
        (u (from-integer (ldb (byte 255 0) (integer-le peer-public)))))
    (let ((x2 +one+) (z2 +zero+) (x3 u) (z3 +one+) (previous 0))
      (loop for i from 254 downto 0 for bit = (ldb (byte 1 i) scalar) do
        (unless (= bit previous) (rotatef x2 x3) (rotatef z2 z3))
        (setf previous bit)
        (let* ((a (f+ x2 z2)) (aa (sq a)) (b (f- x2 z2)) (bb (sq b)) (e (f- aa bb))
               (c (f+ x3 z3)) (d (f- x3 z3)) (da (f* d a)) (cb (f* c b)))
          (setf x3 (sq (f+ da cb)) z3 (f* u (sq (f- da cb)))
                x2 (f* aa bb) z2 (f* e (f+ aa (f* *a24* e))))))
      ;; Scalar clamping fixes the final bit to zero, hence the final swap is a no-op.
      (octets-le (to-integer (f* x2 (inverse z2)))))))

(defparameter *base-u* (octets-le 9))
(defun x25519-public-key (secret) (x25519 secret *base-u*))

(in-package #:clempo.ed25519)

(defparameter *d* (from-integer 37095705934669439343138083508754565189542113879843219016388785533085940283555))
(defparameter *2d* (f+ *d* *d*))
(defparameter *sqrt-minus-one* (from-integer 19681161376707505956807079304988542015446066515923890162744021073123829784752))
(defconstant +order+ (+ (ash 1 252) 27742317777372353535851937790883648493))

(defstruct (point (:constructor point (x y z xy)))
  (x +zero+ :type field :read-only t)
  (y +one+ :type field :read-only t)
  (z +one+ :type field :read-only t)
  (xy +zero+ :type field :read-only t))

(defstruct (cached (:constructor %cached (ymx ypx z dxy)))
  (ymx +one+ :type field :read-only t)
  (ypx +one+ :type field :read-only t)
  (z +one+ :type field :read-only t)
  (dxy +zero+ :type field :read-only t))

(defparameter *identity* (point +zero+ +one+ +one+ +zero+))
(defparameter *base*
  (let ((x (from-integer 15112221349535400772501151409588531511454012693041857206046113283949847762202))
        (y (from-integer 46316835694926478169428394003475163141307993866256225615783033603165251855960)))
    (point x y +one+ (f* x y))))

(defun cache (p)
  (%cached (f- (point-y p) (point-x p)) (f+ (point-y p) (point-x p))
           (point-z p) (f* *2d* (point-xy p))))

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
         (c (f+ z z)) (d (f- +zero+ a)) (h (f- d b))
         (e (f- (sq (f+ (point-x p) (point-y p))) (f+ a b)))
         (g (f+ d b)) (f (f- g c)))
    (point (f* e f) (f* g h) (f* f g) (f* e h))))

(defun negate (p)
  (point (f- +zero+ (point-x p)) (point-y p) (point-z p) (f- +zero+ (point-xy p))))

(defun to-affine (p &optional (inv (inverse (point-z p))))
  (let ((x (f* (point-x p) inv)) (y (f* (point-y p) inv)))
    (point x y +one+ (f* x y))))

(defun encode-point (p)
  (let ((p (to-affine p)))
    (octets-le (logior (ash (logand (to-integer (point-x p)) 1) 255) (to-integer (point-y p))))))

(defun decode-point (data &optional (start 0))
  ;; ZIP-215: the field coordinate may be noncanonical; the sign bit on x=0
  ;; does not invalidate the point. The original octets enter the challenge.
  (let* ((n (integer-le data start (+ start 32))) (y (from-integer (ldb (byte 255 0) n)))
         (yy (sq y)) (u (f- yy +one+)) (v (f+ +one+ (f* *d* yy)))
         (v3 (f* (sq v) v)) (v7 (f* v (sq v3)))
         (x (f* (f* u v3) (pow2523 (f* u v7))))
         (check (to-integer (f* v (sq x))))
         (u-value (to-integer u)))
    (cond ((= check u-value))
          ((= check (- +p+ u-value)) (setf x (f* x *sqrt-minus-one*)))
          (t (return-from decode-point nil)))
    (unless (= (logand (to-integer x) 1) (ldb (byte 1 255) n)) (setf x (f- +zero+ x)))
    (point x y +one+ (f* x y))))

(defun ed25519-public-key-valid-p (data)
  "True when DATA represents an Edwards point under ZIP-215 decoding."
  (and (typep data 'octets) (= (length data) 32) (not (null (decode-point data)))))

(defun base-multiples ()
  ;; A comb: precompute j*16^i*B, so fixed-base multiplication has no doubling.
  (let ((table (make-array '(64 16))) (base *base*)
        (points (make-array 16)) (products (make-array 16)))
    (dotimes (i 64 table)
      (let ((base-cache (cache base)) (p *identity*) (product +one+))
        (dotimes (j 16)
          (setf (aref points j) p (aref products j) product)
          (setf product (f* product (point-z p)) p (add p base-cache nil)))
        ;; One inversion per row; the backwards sweep recovers each Z^-1.
        (let ((inv (inverse product)))
          (loop for j from 15 downto 0 for q = (aref points j) do
            (setf (aref table i j) (cache (to-affine q (f* inv (aref products j))))
                  inv (f* inv (point-z q))))))
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
                (and (zerop (to-integer (point-x p)))
                     (= (to-integer (point-y p)) (to-integer (point-z p))))))))))))
