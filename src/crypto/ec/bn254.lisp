;;; EIP-196/197. All arithmetic is local, exact Common Lisp integer arithmetic.
(defpackage #:clempo.bn254
  (:use #:cl)
  (:import-from #:clempo.base #:define-constant #:make-octets #:integer->octets
                #:octets->integer #:octets-concat)
  (:export #:+field-modulus+ #:+group-order+ #:bn254-add #:bn254-mul #:bn254-pairing
           #:decode-g1 #:encode-g1 #:decode-g2 #:g1-add #:g1-mul #:g2-in-subgroup-p))
(in-package #:clempo.bn254)

(define-constant +field-modulus+
  21888242871839275222246405745257275088696311157297823662689037894645226208583)
(define-constant +group-order+
  21888242871839275222246405745257275088548364400416034343698204186575808495617)
(define-constant +ate-loop+ 29793968203157093288) ; 6 * 4965661367192848881 + 2

(declaim (inline fp+ fp- fp* fp-square fp-neg fp-zero-p fp2-zero-p))
(defun fp+ (a b) (mod (+ a b) +field-modulus+))
(defun fp- (a b) (mod (- a b) +field-modulus+))
(defun fp* (a b) (mod (* a b) +field-modulus+))
(defun fp-square (a) (fp* a a))
(defun fp-neg (a) (if (zerop a) 0 (- +field-modulus+ a)))
(defun fp-zero-p (a) (zerop a))
(defun fp-inverse (a)
  ;; Euclid avoids hundreds of modular multiplications per affine Miller step.
  (clempo.base:invariant (not (zerop a)) "inverse of zero in Fp")
  (let ((r +field-modulus+) (s a) (u 0) (v 1))
    (loop until (zerop s)
          do (multiple-value-bind (q rem) (floor r s)
               (psetf r s s rem u v v (- u (* q v)))))
    (mod u +field-modulus+)))

;;; Fp2 = Fp[i]/(i^2+1), represented by (real . imaginary), always canonical.
(defun fp2+ (a b) (cons (fp+ (car a) (car b)) (fp+ (cdr a) (cdr b))))
(defun fp2- (a b) (cons (fp- (car a) (car b)) (fp- (cdr a) (cdr b))))
(defun fp2-neg (a) (cons (fp-neg (car a)) (fp-neg (cdr a))))
(defun fp2-zero-p (a) (and (zerop (car a)) (zerop (cdr a))))
(defun fp2* (a b)
  (declare #.clempo.base:*hot*)
  (let* ((ar (car a)) (ai (cdr a)) (br (car b)) (bi (cdr b))
         (rr (* ar br)) (ii (* ai bi)))
    (cons (mod (- rr ii) +field-modulus+)
          (mod (- (* (+ ar ai) (+ br bi)) rr ii) +field-modulus+))))
(defun fp2-square (a)
  (declare #.clempo.base:*hot*)
  (let ((r (car a)) (i (cdr a)))
    (cons (mod (* (+ r i) (- r i)) +field-modulus+)
          (mod (* 2 r i) +field-modulus+))))
(defun fp2-scale (a n) (cons (fp* (car a) n) (fp* (cdr a) n)))
(defun fp2-conjugate (a) (cons (car a) (fp-neg (cdr a))))
(defun fp2-inverse (a)
  (let ((d (fp-inverse (fp+ (fp-square (car a)) (fp-square (cdr a))))))
    (cons (fp* (car a) d) (fp-neg (fp* (cdr a) d)))))
(defun fp2-xi (a) ; multiply by xi = 9+i
  (cons (mod (- (* 9 (car a)) (cdr a)) +field-modulus+)
        (mod (+ (car a) (* 9 (cdr a))) +field-modulus+)))
(defun fp2-power (a n)
  (let ((out (cons 1 0)))
    (loop for bit downfrom (1- (integer-length n)) to 0
          do (setf out (fp2-square out))
             (when (logbitp bit n) (setf out (fp2* out a))))
    out))

;;; One Jacobian group law, specialised at macroexpansion for Fp and Fp2.
;;; Affine points are (x . y); NIL is infinity. Jacobian vectors are internal.
(defmacro define-jacobian-group (double add multiply to-affine
                                plus minus times square zero inverse one)
  `(progn
     (defun ,double (point)
       (when (and point (not (,zero (aref point 1))))
         (let* ((x (aref point 0)) (y (aref point 1)) (z (aref point 2))
                (a (,square x)) (b (,square y)) (c (,square b))
                (d0 (,minus (,minus (,square (,plus x b)) a) c))
                (d (,plus d0 d0)) (e (,plus a (,plus a a)))
                (f (,square e)) (nx (,minus f (,plus d d)))
                (c2 (,plus c c)) (c4 (,plus c2 c2)) (c8 (,plus c4 c4)))
           (vector nx (,minus (,times e (,minus d nx)) c8)
                   (,times (,plus y y) z)))))
     (defun ,add (left right)
       (cond ((null left) right) ((null right) left)
             (t (let* ((x1 (aref left 0)) (y1 (aref left 1)) (z1 (aref left 2))
                       (x2 (aref right 0)) (y2 (aref right 1)) (z2 (aref right 2))
                       (zz1 (,square z1)) (zz2 (,square z2))
                       (u1 (,times x1 zz2)) (u2 (,times x2 zz1))
                       (s1 (,times y1 (,times z2 zz2)))
                       (s2 (,times y2 (,times z1 zz1)))
                       (h (,minus u2 u1)) (r (,minus s2 s1)))
                  (if (,zero h)
                      (if (,zero r) (,double left) nil)
                      (let* ((hh (,square h)) (hhh (,times h hh))
                             (v (,times u1 hh))
                             (nx (,minus (,minus (,square r) hhh) (,plus v v))))
                        (vector nx (,minus (,times r (,minus v nx)) (,times s1 hhh))
                                (,times h (,times z1 z2)))))))))
     (defun ,multiply (point scalar)
       ;; Do not reduce SCALAR here: G2 subgroup validation uses [r]Q.
       (let ((out nil) (base (when point (vector (car point) (cdr point) ,one))))
         (loop for bit downfrom (1- (integer-length scalar)) to 0
               do (setf out (,double out))
                  (when (logbitp bit scalar) (setf out (,add out base))))
         out))
     (defun ,to-affine (point)
       (when point
         (let* ((zi (,inverse (aref point 2))) (zz (,square zi)))
           (cons (,times (aref point 0) zz)
                 (,times (aref point 1) (,times zi zz))))))))

(define-jacobian-group g1-double-j g1-add-j g1-mul-j g1-affine
  fp+ fp- fp* fp-square fp-zero-p fp-inverse 1)
(define-jacobian-group g2-double-j g2-add-j g2-mul-j g2-affine
  fp2+ fp2- fp2* fp2-square fp2-zero-p fp2-inverse (cons 1 0))

(defun g1-add (a b)
  "Add validated affine G1 points. NIL denotes infinity."
  (g1-affine (g1-add-j (when a (vector (car a) (cdr a) 1))
                       (when b (vector (car b) (cdr b) 1)))))
(defun g1-mul (point scalar)
  "Multiply a validated affine G1 point by a nonnegative integer."
  (g1-affine (g1-mul-j point (mod scalar +group-order+))))
(defun g2-in-subgroup-p (point)
  "Whether a validated on-twist affine point has order dividing r."
  (null (g2-mul-j point +group-order+)))

(defun input-word (input start)
  ;; EIP-196 right zero-padding is a byte convention, not integer left-padding.
  (let ((n 0) (end (length input)))
    (loop for i from start below (+ start 32)
          do (setf n (logior (ash n 8) (if (< i end) (aref input i) 0))))
    n))
(defun canonical-fp-p (x) (and (integerp x) (<= 0 x) (< x +field-modulus+)))
(defun decode-g1 (input &optional (start 0))
  "Decode EIP-196 G1, right-padding absent bytes. Return POINT, VALID-P."
  (let ((x (input-word input start)) (y (input-word input (+ start 32))))
    (cond ((not (and (canonical-fp-p x) (canonical-fp-p y))) (values nil nil))
          ((and (zerop x) (zerop y)) (values nil t))
          ((= (fp-square y) (fp+ (fp* (fp-square x) x) 3)) (values (cons x y) t))
          (t (values nil nil)))))
(defun encode-g1 (point)
  "Encode a validated affine G1 point as 64 octets; infinity is all zero."
  (if point
      (octets-concat (integer->octets (car point) 32) (integer->octets (cdr point) 32))
      (make-octets 64)))
(defun decode-g2 (input &optional (start 0))
  "Decode 128 EIP-197 bytes including canonical coordinates and subgroup check.
Unlike EIP-196, a truncated G2 encoding is invalid. Return POINT, VALID-P."
  (unless (<= (+ start 128) (length input)) (return-from decode-g2 (values nil nil)))
  (let* ((xi (input-word input start)) (xr (input-word input (+ start 32)))
         (yi (input-word input (+ start 64))) (yr (input-word input (+ start 96))))
    (unless (and (canonical-fp-p xr) (canonical-fp-p xi)
                 (canonical-fp-p yr) (canonical-fp-p yi))
      (return-from decode-g2 (values nil nil)))
    (let ((x (cons xr xi)) (y (cons yr yi)))
      (when (and (fp2-zero-p x) (fp2-zero-p y))
        (return-from decode-g2 (values nil t)))
      (let ((point (cons x y))
            (b (cons 19485874751759354771024239261021720505790618469301721065564631296452457478373
                     266929791119991161246907387137283842545076965332900288569378510910307636690)))
        (if (and (equal (fp2-square y) (fp2+ (fp2* (fp2-square x) x) b))
                 (g2-in-subgroup-p point))
            (values point t)
            (values nil nil))))))

;;; Fp6 = Fp2[v]/(v^3-xi): vectors of three Fp2 coefficients.
;;; Fp12 = Fp6[w]/(w^2-v): (even-coefficients . odd-coefficients).
;;; These are algebraic tower coefficients, not a wire representation.
(defun fp6+ (a b)
  (vector (fp2+ (svref a 0) (svref b 0)) (fp2+ (svref a 1) (svref b 1))
          (fp2+ (svref a 2) (svref b 2))))
(defun fp6- (a b)
  (vector (fp2- (svref a 0) (svref b 0)) (fp2- (svref a 1) (svref b 1))
          (fp2- (svref a 2) (svref b 2))))
(defun fp6-neg (a)
  (vector (fp2-neg (svref a 0)) (fp2-neg (svref a 1)) (fp2-neg (svref a 2))))
(defun fp6-v (a) (vector (fp2-xi (svref a 2)) (svref a 0) (svref a 1)))
(defun fp6* (a b)
  (let* ((a0 (svref a 0)) (a1 (svref a 1)) (a2 (svref a 2))
         (b0 (svref b 0)) (b1 (svref b 1)) (b2 (svref b 2))
         (t0 (fp2* a0 b0)) (t1 (fp2* a1 b1)) (t2 (fp2* a2 b2)))
    (vector
     (fp2+ t0 (fp2-xi (fp2- (fp2- (fp2* (fp2+ a1 a2) (fp2+ b1 b2)) t1) t2)))
     (fp2+ (fp2- (fp2- (fp2* (fp2+ a0 a1) (fp2+ b0 b1)) t0) t1) (fp2-xi t2))
     (fp2+ (fp2- (fp2- (fp2* (fp2+ a0 a2) (fp2+ b0 b2)) t0) t2) t1))))
(defun fp6-square (a)
  (let* ((a0 (svref a 0)) (a1 (svref a 1)) (a2 (svref a 2))
         (s0 (fp2-square a0)) (s1 (fp2-scale (fp2* a0 a1) 2))
         (s2 (fp2-square (fp2+ (fp2- a0 a1) a2)))
         (s3 (fp2-scale (fp2* a1 a2) 2)) (s4 (fp2-square a2)))
    (vector (fp2+ s0 (fp2-xi s3)) (fp2+ s1 (fp2-xi s4))
            (fp2- (fp2- (fp2+ (fp2+ s1 s2) s3) s0) s4))))
(defun fp6-inverse (a)
  (let* ((a0 (svref a 0)) (a1 (svref a 1)) (a2 (svref a 2))
         (t0 (fp2- (fp2-square a0) (fp2-xi (fp2* a1 a2))))
         (t1 (fp2- (fp2-xi (fp2-square a2)) (fp2* a0 a1)))
         (t2 (fp2- (fp2-square a1) (fp2* a0 a2)))
         (inv (fp2-inverse (fp2+ (fp2* a0 t0)
                                (fp2-xi (fp2+ (fp2* a2 t1) (fp2* a1 t2)))))))
    (vector (fp2* t0 inv) (fp2* t1 inv) (fp2* t2 inv))))
(defun fp12-one ()
  (cons (vector (cons 1 0) (cons 0 0) (cons 0 0))
        (vector (cons 0 0) (cons 0 0) (cons 0 0))))
(defun fp12* (a b)
  (let ((aa (fp6* (car a) (car b))) (bb (fp6* (cdr a) (cdr b))))
    (cons (fp6+ aa (fp6-v bb))
          (fp6- (fp6- (fp6* (fp6+ (car a) (cdr a)) (fp6+ (car b) (cdr b))) aa) bb))))
(defun fp12-square (a)
  (let* ((ab (fp6* (car a) (cdr a)))
         (other (fp6* (fp6+ (car a) (cdr a)) (fp6+ (car a) (fp6-v (cdr a))))))
    (cons (fp6- (fp6- other ab) (fp6-v ab)) (fp6+ ab ab))))
(defun fp12-inverse (a)
  (let ((den (fp6-inverse (fp6- (fp6-square (car a)) (fp6-v (fp6-square (cdr a)))))))
    (cons (fp6* (car a) den) (fp6-neg (fp6* (cdr a) den)))))
(defun fp12-power (a n)
  ;; Four-bit sliding windows keep the final exponent's multiplications small.
  (when (zerop n) (return-from fp12-power (fp12-one)))
  (let ((odd-powers (make-array 8)) (a2 (fp12-square a))
        (out (fp12-one)) (bit (1- (integer-length n))))
    (setf (svref odd-powers 0) a)
    (loop for k from 1 below 8
          do (setf (svref odd-powers k) (fp12* (svref odd-powers (1- k)) a2)))
    (loop while (>= bit 0)
          do (if (not (logbitp bit n))
                 (setf out (fp12-square out) bit (1- bit))
                 (let ((low (max 0 (- bit 3))))
                   (loop until (logbitp low n) do (incf low))
                   (let* ((width (1+ (- bit low))) (window (ldb (byte width low) n)))
                     (dotimes (k width) (setf out (fp12-square out)))
                     (setf out (fp12* out (svref odd-powers (ash window -1)))
                           bit (1- low))))))
    out))
(defparameter *fp12-frobenius2-coefficients*
  (let* ((c (fp2-power (cons 9 1) (/ (1- (* +field-modulus+ +field-modulus+)) 6)))
         (powers (make-array 6)))
    (setf (svref powers 0) (cons 1 0))
    (loop for k from 1 below 6 do (setf (svref powers k) (fp2* (svref powers (1- k)) c)))
    powers))
(defun fp12-frobenius2 (a)
  (flet ((part (v offset)
           (vector (fp2* (svref v 0) (svref *fp12-frobenius2-coefficients* offset))
                   (fp2* (svref v 1) (svref *fp12-frobenius2-coefficients* (+ offset 2)))
                   (fp2* (svref v 2) (svref *fp12-frobenius2-coefficients* (+ offset 4))))))
    (cons (part (car a) 0) (part (cdr a) 1))))
(define-constant +hard-exponent+
  (/ (+ (- (expt +field-modulus+ 4) (expt +field-modulus+ 2)) 1) +group-order+))
(defun final-exponentiation (f)
  ;; (p^12-1)/r = (p^6-1)(p^2+1)(p^4-p^2+1)/r.
  (let* ((easy (fp12* (cons (car f) (fp6-neg (cdr f))) (fp12-inverse f)))
         (cyclotomic (fp12* easy (fp12-frobenius2 easy))))
    (fp12-power cyclotomic +hard-exponent+)))

(defun g2-frobenius (point)
  (cons (fp2* (fp2-conjugate (car point))
              (cons 21575463638280843010398324269430826099269044274347216827212613867836435027261
                    10307601595873709700152284273816112264069230130616436755625194854815875713954))
        (fp2* (fp2-conjugate (cdr point))
              (cons 2821565182194536844548159561693502659359617185244120367078079554186484126554
                    3505843767911556378687030309984248845540243509899259641013678093033130930403))))
(defun fp6-scale (a n)
  (vector (fp2-scale (svref a 0) n) (fp2-scale (svref a 1) n)
          (fp2-scale (svref a 2) n)))
(defun fp6-mul-01 (a b0 b1)
  ;; The Miller line has only its first two odd tower coefficients.
  (let* ((a0 (svref a 0)) (a1 (svref a 1)) (a2 (svref a 2))
         (t0 (fp2* a0 b0)) (t1 (fp2* a1 b1)))
    (vector (fp2+ t0 (fp2-xi (fp2* a2 b1)))
            (fp2- (fp2- (fp2* (fp2+ a0 a1) (fp2+ b0 b1)) t0) t1)
            (fp2+ t1 (fp2* a2 b0)))))
(defun fp12-mul-line (a line)
  (let* ((n (car (svref (car line) 0)))
         (b0 (svref (cdr line) 0)) (b1 (svref (cdr line) 1)))
    (cons (fp6+ (fp6-scale (car a) n) (fp6-v (fp6-mul-01 (cdr a) b0 b1)))
          (fp6+ (fp6-scale (cdr a) n) (fp6-mul-01 (car a) b0 b1)))))
(defun miller-line (r q p)
  "Return R+Q and its line evaluated at P after the D-type untwist.
Vertical denominators lie in Fp6, so the final exponent eliminates them."
  (when (null r) (return-from miller-line (values q (fp12-one))))
  (when (null q) (return-from miller-line (values r (fp12-one))))
  (let* ((x (car r)) (y (cdr r)) (qx (car q)) (qy (cdr q))
         (dx (fp2- qx x)) (dy (fp2- qy y)))
    (when (and (fp2-zero-p dx) (or (not (fp2-zero-p dy)) (fp2-zero-p y)))
      ;; Vertical line x_P - x_R*w^2 (including the 2-torsion tangent).
      (return-from miller-line
        (values nil (cons (vector (cons (car p) 0) (fp2-neg x) (cons 0 0))
                          (vector (cons 0 0) (cons 0 0) (cons 0 0))) t)))
    (let* ((slope (if (fp2-zero-p dx)
                      (fp2* (fp2-scale (fp2-square x) 3) (fp2-inverse (fp2-scale y 2)))
                      (fp2* dy (fp2-inverse dx))))
           (nx (fp2- (fp2- (fp2-square slope) x) qx))
           (ny (fp2- (fp2* slope (fp2- x nx)) y)))
      (values (cons nx ny)
              (cons (vector (cons (cdr p) 0) (cons 0 0) (cons 0 0))
                    (vector (fp2-neg (fp2-scale slope (car p)))
                            (fp2- (fp2* slope x) y) (cons 0 0)))))))
(defun multi-miller (pairs)
  ;; A single accumulator shares the loop squarings across all input pairs.
  ;; State vectors hold P, Q, and the running multiple R, respectively.
  (let ((states (mapcar (lambda (pair) (vector (car pair) (cdr pair) (cdr pair))) pairs))
        (f (fp12-one)))
    (labels ((step-line (state q)
               (multiple-value-bind (r line vertical-p)
                   (miller-line (svref state 2) q (svref state 0))
                 (setf (svref state 2) r
                       f (if vertical-p (fp12* f line) (fp12-mul-line f line))))))
      (loop for bit downfrom (- (integer-length +ate-loop+) 2) to 0
            do (setf f (fp12-square f))
               (dolist (state states) (step-line state (svref state 2)))
               (when (logbitp bit +ate-loop+)
                 (dolist (state states) (step-line state (svref state 1)))))
      (dolist (state states)
        (let* ((q1 (g2-frobenius (svref state 1))) (q2 (g2-frobenius q1)))
          (step-line state q1)
          (step-line state (cons (car q2) (fp2-neg (cdr q2)))))))
    f))

(defun bn254-add (input)
  "EIP-196 addition: 64 output octets, or NIL for invalid coordinates.
Read the first 128 bytes, right-pad missing bytes with zero, ignore excess."
  (multiple-value-bind (a valid-a) (decode-g1 input)
    (multiple-value-bind (b valid-b) (decode-g1 input 64)
      (when (and valid-a valid-b) (encode-g1 (g1-add a b))))))
(defun bn254-mul (input)
  "EIP-196 multiplication: 64 octets, or NIL for an invalid G1 point.
Read the first 96 bytes, right-pad with zero, ignore excess; scalar is arbitrary."
  (multiple-value-bind (point valid) (decode-g1 input)
    (when valid (encode-g1 (g1-mul point (input-word input 64))))))
(defun bn254-pairing (input)
  "EIP-197 product-of-pairings check. Return BOOLEAN, VALID-P.
An invalid length, noncanonical coordinate, off-curve point or non-G2 twist
point returns NIL,NIL. A well-formed false equation returns NIL,T."
  (unless (zerop (mod (length input) 192))
    (return-from bn254-pairing (values nil nil)))
  (let ((pairs nil))
    (loop for start from 0 below (length input) by 192
          do (multiple-value-bind (p valid-p) (decode-g1 input start)
               (multiple-value-bind (q valid-q) (decode-g2 input (+ start 64))
                 ;; Validate BOTH points even if the other is infinity.
                 (unless (and valid-p valid-q)
                   (return-from bn254-pairing (values nil nil)))
                 (when (and p q) (push (cons p q) pairs)))))
    (values (or (null pairs)
                (equalp (final-exponentiation (multi-miller pairs)) (fp12-one)))
            t)))
