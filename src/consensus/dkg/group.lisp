;;; G2 (the MinSig public-key group) for the DKG: compressed octets at the boundary, points
;;; from clempo.bls12-381 inside. Decompression (with the subgroup check G2::read does) is
;;; memoized per octet vector, since the same commitments are read, checked and combined
;;; several times in a round.

(in-package #:clempo.dkg)

(defvar *g2-points* (make-hash-table :test 'equalp :weakness :key :synchronized t)
  "Compressed G2 octets -> decompressed point (a pure cache).")

(defun g2-of (octets)
  "The point OCTETS encode. Signals CW-ERROR unless OCTETS is a valid compressed point of
the prime-order subgroup other than the identity (G2::read)."
  (multiple-value-bind (point found) (gethash octets *g2-points*)
    (if found
        point
        (let ((point (handler-case (clempo.bls12-381:g2-decompress octets)
                       (clempo.bls12-381:invalid-point ()
                         (cw-fail "G2: invalid compressed point")))))
          (unless point (cw-fail "G2: infinity"))
          (setf (gethash (copy-seq octets) *g2-points*) point)))))

(defun g2-octets (point)
  "Compressed octets of POINT (NIL is the identity)."
  (clempo.bls12-381:g2-compress point))

(defun g2-base-mul (scalar)
  "g * SCALAR, constant-time in the (secret) SCALAR."
  (clempo.bls12-381:g2-mul-secret (clempo.bls12-381:g2-generator) scalar))

(defun g2-lincomb (points scalars)
  "sum_i scalars_i * points_i."
  (if (null points)
      nil
      (clempo.bls12-381:g2-msm points (mapcar (lambda (s) (mod s +group-order+)) scalars))))

(defun g2-sum (points)
  (reduce #'clempo.bls12-381:g2-add points :initial-value nil))

(defun scalar-powers (x n)
  "1, x, x^2, ... (N of them)."
  (loop repeat n for p = 1 then (fr* p x) collect p))

(defun commitment-points (coefficients)
  (mapcar #'g2-of coefficients))

(defun commitment-eval (coefficients x)
  "Poly::eval_msm over a polynomial of compressed G2 COEFFICIENTS at scalar X: a point."
  (g2-lincomb (commitment-points coefficients) (scalar-powers x (length coefficients))))

(defun commit-scalars (coefficients)
  "Poly::commit: g * c for each scalar coefficient, as compressed octets."
  (mapcar (lambda (c) (g2-octets (g2-base-mul c))) coefficients))

(defun commitment-degree (coefficients)
  "Poly::degree_exact over compressed coefficients: trailing identities do not count."
  (1- (let ((n (length coefficients)))
        (max 1 (- n (loop for c in (reverse coefficients)
                          while (compressed-identity-p c) count t))))))

(defvar *partial-publics* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Sharing -> vector of its computed partial public keys (Sharing's OnceLock evals).")

(defun sharing-partial-public (sharing index)
  "Sharing::partial_public: the public key share of participant INDEX (0-based) as G2
octets, computed once per sharing. Signals DKG-ERROR when INDEX >= the sharing's total."
  (let ((x (mode-scalar (sharing-mode sharing) (sharing-total sharing) index)))
    (unless x (dkg-fail :invalid-index "~D of ~D" index (sharing-total sharing)))
    (let ((evals (or (gethash sharing *partial-publics*)
                     (setf (gethash sharing *partial-publics*)
                           (make-array (sharing-total sharing) :initial-element nil)))))
      (or (aref evals index)
          (setf (aref evals index)
                (g2-octets (commitment-eval (sharing-coefficients sharing) x)))))))
