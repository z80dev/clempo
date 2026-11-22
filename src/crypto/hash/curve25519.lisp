(defpackage #:clempo.x25519
  (:use #:cl #:clempo.base #:clempo.field25519)
  (:export #:x25519 #:x25519-public-key))
(defpackage #:clempo.ed25519
  (:use #:cl #:clempo.base #:clempo.field25519)
  (:export #:ed25519-public-key #:ed25519-sign #:ed25519-verify #:ed25519-public-key-valid-p))
(in-package #:clempo.x25519)

(defparameter *a24* (from-integer 121665))

(defun swap-if (swap a b)
  "A and B, exchanged when SWAP is 1, as fresh field elements. SWAP crosses the call as
a bit: an all-ones mask word would be boxed, and that allocation is visible in timing."
  (declare #.clempo.base:*hot* (type field a b) (type bit swap))
  (let ((x (make-array +limbs+ :element-type '(unsigned-byte 64)))
        (y (make-array +limbs+ :element-type '(unsigned-byte 64)))
        (mask (clempo.ct:bit-mask swap)))
    (declare (type (unsigned-byte 64) mask))
    (dotimes (i +limbs+ (values x y))
      (let ((d (logand mask (logxor (aref a i) (aref b i)))))
        (setf (aref x i) (logxor (aref a i) d) (aref y i) (logxor (aref b i) d))))))

(defun x25519 (secret peer-public)
  "RFC 7748 raw shared coordinate. Callers must reject an all-zero shared secret.
The Montgomery ladder swaps with masks, so no branch or index depends on SECRET."
  (clempo.ct:with-data-independent-timing
    (check-type secret octets)
    (check-type peer-public octets)
    (assert (= (length secret) (length peer-public) 32))
    (let ((scalar (clamped-octets secret))
          (u (from-integer (ldb (byte 255 0) (integer-le peer-public)))))
      (let ((x2 +one+) (z2 +zero+) (x3 u) (z3 +one+) (previous 0))
        (declare (type bit previous))
        (loop for i from 254 downto 0
              for bit of-type bit = (ldb (byte 1 (logand i 7)) (aref scalar (ash i -3))) do
          (let ((swap (logxor bit previous)))
            (setf (values x2 x3) (swap-if swap x2 x3)
                  (values z2 z3) (swap-if swap z2 z3)
                  previous bit))
          (let* ((a (f+ x2 z2)) (aa (sq a)) (b (f- x2 z2)) (bb (sq b)) (e (f- aa bb))
                 (c (f+ x3 z3)) (d (f- x3 z3)) (da (f* d a)) (cb (f* c b)))
            (setf x3 (sq (f+ da cb)) z3 (f* u (sq (f- da cb)))
                  x2 (f* aa bb) z2 (f* e (f+ aa (f* *a24* e))))))
        ;; Scalar clamping fixes the final bit to zero, hence the final swap is a no-op.
        (octets-le (to-integer (f* x2 (inverse z2))))))))

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
  ;; A comb of odd multiples, (2j+1)*16^i*B for i < 65 and j < 8: fixed-base
  ;; multiplication has no doublings, and with signed odd digits (CLEMPO.CT:RECODE-ODD!)
  ;; no entry it reads is the identity.
  (let ((table (make-array (list clempo.ct:+digits+ clempo.ct:+window-entries+))) (base *base*)
        (points (make-array clempo.ct:+window-entries+))
        (products (make-array clempo.ct:+window-entries+)))
    (dotimes (i clempo.ct:+digits+ table)
      (let ((twice (cache (double-point base))) (p base) (product +one+))
        (dotimes (j clempo.ct:+window-entries+)
          (setf (aref points j) p (aref products j) product)
          (setf product (f* product (point-z p)) p (add p twice nil)))
        ;; One inversion per row; the backwards sweep recovers each Z^-1.
        (let ((inv (inverse product)))
          (loop for j from (1- clempo.ct:+window-entries+) downto 0 for q = (aref points j) do
            (setf (aref table i j) (cache (to-affine q (f* inv (aref products j))))
                  inv (f* inv (point-z q))))))
      (dotimes (j clempo.ct:+window-bits+) (setf base (double-point base))))))

(defparameter *fixed-base* (base-multiples))

(defparameter *order-words*
  (let ((w (clempo.ct:make-words clempo.ct:+scalar-words+)))
    (dotimes (i clempo.ct:+scalar-words+ w) (setf (aref w i) (ldb (byte 64 (* 64 i)) +order+)))))

(defun select-base (row digit)
  "DIGIT*16^ROW*B for an odd DIGIT in [-15, 15]: all +WINDOW-ENTRIES+ entries of ROW read
under masks, then a masked negation (swap y-x with y+x, negate 2dxy)."
  (declare #.clempo.base:*hot* (type (integer 0 64) row) (type (integer -15 15) digit))
  (let* ((sign (logand 1 (ash digit -63)))
         (index (ash (+ (logxor digit (- sign)) sign) -1))
         (negate (clempo.ct:bit-mask sign))
         (ymx (make-array +limbs+ :element-type '(unsigned-byte 64) :initial-element 0))
         (ypx (make-array +limbs+ :element-type '(unsigned-byte 64) :initial-element 0))
         (z (make-array +limbs+ :element-type '(unsigned-byte 64) :initial-element 0))
         (dxy (make-array +limbs+ :element-type '(unsigned-byte 64) :initial-element 0)))
    (declare (type bit sign) (type (integer 0 7) index) (type (unsigned-byte 64) negate))
    (dotimes (j clempo.ct:+window-entries+)
      (let ((mask (clempo.ct:equal-mask j index)) (entry (aref *fixed-base* row j)))
        (declare (type cached entry) (type (unsigned-byte 64) mask))
        (macrolet ((take (out accessor)
                     `(let ((from (,accessor entry)))
                        (dotimes (i +limbs+)
                          (setf (aref ,out i) (logior (aref ,out i) (logand mask (aref from i))))))))
          (take ymx cached-ymx) (take ypx cached-ypx) (take z cached-z) (take dxy cached-dxy))))
    (let ((minus (f- +zero+ dxy)))
      (dotimes (i +limbs+)
        (let ((d (logand negate (logxor (aref ymx i) (aref ypx i)))))
          (setf (aref ymx i) (logxor (aref ymx i) d) (aref ypx i) (logxor (aref ypx i) d)
                (aref dxy i) (logior (logand negate (aref minus i)) (logandc1 negate (aref dxy i)))))))
    (%cached ymx ypx z dxy)))

(defun base-mul (scalar)
  "SCALAR*B for 32 little-endian SCALAR octets; constant-time in SCALAR."
  (declare (type octets scalar))
  (let ((digits (clempo.ct:make-digits)))
    (clempo.ct:recode-odd! digits (reverse scalar) *order-words*)
    (let ((p (add *identity* (select-base 0 (aref digits 0)) t)))
      (loop for i from 1 below clempo.ct:+digits+
            do (setf p (add p (select-base i (aref digits i)) t)))
      p)))

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

(clempo.ct:define-ct-field ct-scalar #.+order+)

(defun ed25519-public-key (seed)
  (clempo.ct:with-data-independent-timing
    (encode-point (base-mul (clamped-octets (seed-hash seed))))))

(defun challenge (nonce-public start public-key message)
  (let ((ctx (clempo.sha2:make-sha512)))
    (clempo.sha2:sha512-update ctx nonce-public :start start :end (+ start 32))
    (clempo.sha2:sha512-update ctx public-key)
    (clempo.sha2:sha512-update ctx message)
    (mod (integer-le (clempo.sha2:sha512-final ctx)) +order+)))

(defun ed25519-sign (seed message)
  "RFC 8032 Ed25519. Constant-time in the seed and nonce: the base comb reads every
entry under masks, and S = r + k a mod L is Montgomery arithmetic with masked
reductions. Only R, the challenge k and S, all public, become integers."
  (clempo.ct:with-data-independent-timing
    (check-type message octets)
    (let* ((digest (seed-hash seed)) (scalar (clamped-octets digest))
           (public-key (encode-point (base-mul scalar)))
           (ctx (clempo.sha2:make-sha512))
           (w (clempo.ct:make-words 16)) (r (make-octets 32)) (s (make-octets 32)))
      (clempo.sha2:sha512-update ctx digest :start 32)
      (clempo.sha2:sha512-update ctx message)
      (let ((nonce (clempo.sha2:sha512-final ctx)))
        (clempo.ct:words-from-octets! w 0 4 nonce 0 :little-endian t)
        (clempo.ct:words-from-octets! w 4 4 nonce 32 :little-endian t)
        (ct-scalar-from-wide-words! w 8 w 0 w 4)
        (ct-scalar-to-words! w 0 w 8)
        (clempo.ct:words-to-octets! r 0 w 0 4 :little-endian t))
      (let* ((encoded-r (encode-point (base-mul r)))
             (k (challenge encoded-r 0 public-key message)))
        (ct-scalar-from-integer! w 0 k)
        (clempo.ct:words-from-octets! w 4 4 scalar 0 :little-endian t)
        (ct-scalar-from-words! w 4 w 4)
        (ct-scalar-mul! w 0 w 0 w 4)
        (ct-scalar-add! w 0 w 0 w 8)
        (ct-scalar-to-words! w 0 w 0)
        (clempo.ct:words-to-octets! s 0 w 0 4 :little-endian t)
        (octets-concat encoded-r s)))))

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
                     (p (add (base-mul (octets-le s)) (cache (multiply k (negate a))) nil)))
                (setf p (add p (cache (negate r)) nil))
                (dotimes (i 3) (setf p (double-point p)))
                (and (zerop (to-integer (point-x p)))
                     (= (to-integer (point-y p)) (to-integer (point-z p))))))))))))
