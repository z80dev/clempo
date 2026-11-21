;;; The BLS12-381 scalar field (integers mod r): evaluation points, Lagrange weights,
;;; polynomials over scalars.

(in-package #:clempo.dkg)

;; Shares and polynomial coefficients are secret: constant-time Montgomery arithmetic.
(declaim (inline fr+ fr- fr*))
(defun fr+ (a b) (clempo.bls12-381:fr-add a b))
(defun fr- (a b) (clempo.bls12-381:fr-sub a b))
(defun fr* (a b) (clempo.bls12-381:fr-mul a b))

(defun fr-expt (base exponent)
  (let ((result 1) (base (mod base +group-order+)))
    (loop while (plusp exponent)
          do (when (oddp exponent) (setf result (fr* result base)))
             (setf exponent (ash exponent -1)
                   base (fr* base base)))
    result))

(defun fr-inverse (a)
  (invariant (not (zerop (mod a +group-order+))) "inverse of zero")
  (fr-expt a (- +group-order+ 2)))

(defparameter *root-of-unity*
  ;; group.rs ROOT_OF_UNITY: a primitive 2^32-th root of unity, given there in Montgomery
  ;; form (limbs little-endian, times 2^-256).
  (fr* #x5bf3adda19e9b27b0af53ae352a31e645b1b4c801819d7ecb9b58d8c5f0e466a
       (fr-inverse (ash 1 256)))
  "A primitive 2^32-th root of unity mod r.")

(defun root-of-unity (lg)
  "Scalar::root_of_unity: a primitive 2^LG-th root of unity, LG <= 32."
  (when (<= lg 32)
    (let ((w *root-of-unity*))
      (loop repeat (- 32 lg) do (setf w (fr* w w)))
      w)))

(defun mode-scalar (mode total index)
  "Mode::scalar: the evaluation point of participant INDEX among TOTAL, or NIL when
INDEX >= TOTAL. NonZeroCounter gives index+1; RootsOfUnity gives w^index for a primitive root
of unity of order next_power_of_two(TOTAL)."
  (when (< index total)
    (ecase mode
      (:nonzero-counter (1+ index))
      (:roots-of-unity
       (let ((lg (integer-length (1- total))))
         (fr-expt (or (root-of-unity lg) (dkg-fail :domain-too-large)) index))))))

(defparameter *keygen-salt*
  (clempo.sha2:sha256 (string->octets "BLS-SIG-KEYGEN-SALT-")))

(defun scalar-from-ikm (ikm)
  "Scalar::from_ikm (blst_keygen, the IETF BLS KeyGen / EIP-2333 HKDF_mod_r): a non-zero
scalar from at least 32 bytes of input keying material."
  (invariant (>= (length ikm) 32) "keygen needs at least 32 bytes of IKM")
  (loop for salt = *keygen-salt* then (clempo.sha2:sha256 salt)
        for okm = (clempo.sha2:hkdf-sha256-expand
                   (clempo.sha2:hkdf-sha256-extract salt (octets-concat ikm (octets-of 0)))
                   (octets-of 0 48) 48)
        for sk = (clempo.bls12-381:fr-reduce-octets okm)
        unless (zerop sk) return sk))

(defun random-scalar (&optional (rng #'clempo.io:random-octets))
  "Scalar::random: Scalar::from_ikm over 64 bytes from RNG (a function of a byte count, by
default clempo.io:*entropy*'s random-octets)."
  (scalar-from-ikm (funcall rng 64)))

(defun lagrange-weights (points)
  "Interpolator::new: for distinct evaluation POINTS, the weights w_i with
f(0) = sum w_i f(points_i) for every polynomial of degree < (length POINTS)."
  (let ((zero-at (position 0 points)))
    (if zero-at
        (loop for i from 0 below (length points) collect (if (= i zero-at) 1 0))
        (loop for xi in points
              for i from 0
              collect (let ((num 1) (den 1))
                        (loop for xj in points
                              for j from 0
                              unless (= i j)
                                do (setf num (fr* num xj)
                                         den (fr* den (fr- xj xi))))
                        (fr* num (fr-inverse den)))))))

(defun scalar-poly-eval (coefficients x)
  "Horner evaluation of a scalar polynomial, constant first."
  (let ((acc 0))
    (dolist (c (reverse coefficients) acc)
      (setf acc (fr+ (fr* acc x) c)))))
