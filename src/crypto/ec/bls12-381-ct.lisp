;;; Constant-time BLS12-381 for secret scalars: signing, proofs of possession,
;;; public keys and DKG share arithmetic. Verification, aggregation and MSM keep
;;; the faster variable-time kernels; their scalars are public.
(in-package #:clempo.bls12-381)

(clempo.ct:define-ct-field ct-fp #.+p+)
(clempo.ct:define-ct-quadratic-field ct-fp2 ct-fp 6)
(clempo.ct:define-ct-field ct-fr #.+r+)
;; Points in the prime-order subgroups never meet the exceptional cases the complete
;; formulas exclude (sums with a 2-torsion difference), so they stay complete here.
(clempo.ct:define-ct-curve ct-g1 :field ct-fp :width 6 :a 0 :b 4 :order #.+r+)
(clempo.ct:define-ct-curve ct-g2 :field ct-fp2 :width 12 :a 0 :b (4 . 4) :order #.+r+)

(defun secret-scalar-octets (scalar)
  "32 big-endian octets of SCALAR mod r; secrets are already canonical."
  (integer->octets (if (< -1 scalar +r+) scalar (mod scalar +r+)) 32))

(defun g1-mul-secret (point scalar)
  "SCALAR*POINT, constant-time in SCALAR, for a public affine G1 POINT (or NIL)."
  (clempo.ct:with-data-independent-timing
    (when point
      (let ((w (clempo.ct:make-words 30)))
        (ct-g1-from-affine-integers! w 0 (car point) (cdr point))
        (ct-g1-mul! w 0 w 0 (secret-scalar-octets scalar))
        (when (zerop (ct-g1-affine! w 18 w 0))
          (cons (ct-fp-to-integer w 18) (ct-fp-to-integer w 24)))))))

(defun g2-mul-secret (point scalar)
  "SCALAR*POINT, constant-time in SCALAR, for a public affine G2 POINT (or NIL)."
  (clempo.ct:with-data-independent-timing
    (when point
      (let ((w (clempo.ct:make-words 60)))
        (ct-g2-from-affine-integers! w 0 (car point) (cdr point))
        (ct-g2-mul! w 0 w 0 (secret-scalar-octets scalar))
        (when (zerop (ct-g2-affine! w 36 w 0))
          (cons (ct-fp2-to-integer w 36) (ct-fp2-to-integer w 48)))))))

(macrolet ((define-fr (name op)
             `(defun ,name (a b)
                ,(format nil "a ~A b mod r in constant time for canonical integer inputs." op)
                (clempo.ct:with-data-independent-timing
                  (let ((w (clempo.ct:make-words 12)))
                    (ct-fr-from-integer! w 0 a) (ct-fr-from-integer! w 4 b)
                    (,(ecase op (+ 'ct-fr-add!) (- 'ct-fr-sub!) (* 'ct-fr-mul!)) w 8 w 0 w 4)
                    (ct-fr-to-integer w 8))))))
  (define-fr fr-add +)
  (define-fr fr-sub -)
  (define-fr fr-mul *))

(defun fr-reduce-octets (octets)
  "The big-endian OCTETS (at most 64) as an integer mod r, without a division."
  (clempo.ct:with-data-independent-timing
    (let ((wide (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
          (w (clempo.ct:make-words 12)))
      (assert (<= (length octets) 64))
      (replace wide octets :start1 (- 64 (length octets)))
      (clempo.ct:words-from-octets! w 0 4 wide 32)
      (clempo.ct:words-from-octets! w 4 4 wide 0)
      (ct-fr-from-wide-words! w 8 w 0 w 4)
      (ct-fr-to-integer w 8))))
