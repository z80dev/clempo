;;; Constant-time BLS12-381 for secret scalars: signing, proofs of possession,
;;; public keys and DKG share arithmetic. Verification, aggregation and MSM keep
;;; the faster variable-time kernels; their scalars are public.
;;;
;;; Secret scalars are 32 big-endian octets below r (Scalar's encoding), end to end: no
;;; secret scalar becomes a Lisp integer, whose bignum length would depend on its value.
;;; Arithmetic loads them into Montgomery words, works there and writes octets back,
;;; zeroing its word arenas on the way out.
(in-package #:clempo.bls12-381)

(clempo.ct:define-ct-field ct-fp #.+p+)
(clempo.ct:define-ct-quadratic-field ct-fp2 ct-fp 6)
(clempo.ct:define-ct-field ct-fr #.+r+)
;; Points in the prime-order subgroups never meet the exceptional cases the complete
;; formulas exclude (sums with a 2-torsion difference), so they stay complete here.
(clempo.ct:define-ct-curve ct-g1 :field ct-fp :width 6 :a 0 :b 4 :order #.+r+)
(clempo.ct:define-ct-curve ct-g2 :field ct-fp2 :width 12 :a 0 :b (4 . 4) :order #.+r+)

(deftype fr-octets () '(clempo.base:octets 32))

(defun fr-octets-p (x &key nonzero)
  "Whether X is an FR-OCTETS scalar: 32 big-endian octets below r, and not zero when
NONZERO. The checks are constant-time; only the single public verdict branches."
  (and (typep x 'fr-octets)
       (clempo.ct:with-data-independent-timing
         (clempo.ct:with-wiped ((w (clempo.ct:make-words 4)))
           (clempo.ct:words-from-octets! w 0 4 x 0)
           (= 1 (logand (ct-fr-less-bit w 0)
                        (if nonzero (logxor 1 (ct-fr-zero-bit w 0)) 1)))))))

(defun fr-octets-from-integer (integer)
  "The FR-OCTETS of a public INTEGER mod r (evaluation points, weights, test values)."
  (integer->octets (mod integer +r+) 32))

(defun g1-mul-secret (point scalar)
  "SCALAR*POINT, constant-time in the FR-OCTETS SCALAR, for a public affine G1 POINT
\(or NIL)."
  (check-type scalar fr-octets)
  (clempo.ct:with-data-independent-timing
    (when point
      (clempo.ct:with-wiped ((w (clempo.ct:make-words 30)))
        (ct-g1-from-affine-integers! w 0 (car point) (cdr point))
        (ct-g1-mul! w 0 w 0 scalar)
        (when (zerop (ct-g1-affine! w 18 w 0))
          (cons (ct-fp-to-integer w 18) (ct-fp-to-integer w 24)))))))

(defun g2-mul-secret (point scalar)
  "SCALAR*POINT, constant-time in the FR-OCTETS SCALAR, for a public affine G2 POINT
\(or NIL)."
  (check-type scalar fr-octets)
  (clempo.ct:with-data-independent-timing
    (when point
      (clempo.ct:with-wiped ((w (clempo.ct:make-words 60)))
        (ct-g2-from-affine-integers! w 0 (car point) (cdr point))
        (ct-g2-mul! w 0 w 0 scalar)
        (when (zerop (ct-g2-affine! w 36 w 0))
          (cons (ct-fp2-to-integer w 36) (ct-fp2-to-integer w 48)))))))

(defun fr-words-octets (w o)
  "Fresh FR-OCTETS of the Montgomery element at W[O..O+4), converted in place."
  (ct-fr-to-words! w o w o)
  (let ((out (make-octets 32)))
    (clempo.ct:words-to-octets! out 0 w o 4)
    out))

(macrolet ((define-fr (name op)
             `(defun ,name (a b)
                ,(format nil "a ~A b mod r in constant time, for FR-OCTETS A and B: fresh FR-OCTETS." op)
                (check-type a fr-octets)
                (check-type b fr-octets)
                (clempo.ct:with-data-independent-timing
                  (clempo.ct:with-wiped ((w (clempo.ct:make-words 12)))
                    (clempo.ct:words-from-octets! w 0 4 a 0)
                    (ct-fr-from-words! w 0 w 0)
                    (clempo.ct:words-from-octets! w 4 4 b 0)
                    (ct-fr-from-words! w 4 w 4)
                    (,(ecase op (+ 'ct-fr-add!) (* 'ct-fr-mul!)) w 8 w 0 w 4)
                    (fr-words-octets w 8))))))
  (define-fr fr-add +)
  (define-fr fr-mul *))

(defun fr-reduce-octets (octets)
  "The big-endian OCTETS (at most 64) mod r, as fresh FR-OCTETS, without a division."
  (assert (<= (length octets) 64))
  (clempo.ct:with-data-independent-timing
    (clempo.ct:with-wiped ((wide (make-octets 64)) (w (clempo.ct:make-words 12)))
      (replace wide octets :start1 (- 64 (length octets)))
      (clempo.ct:words-from-octets! w 0 4 wide 32)
      (clempo.ct:words-from-octets! w 4 4 wide 0)
      (ct-fr-from-wide-words! w 8 w 0 w 4)
      (fr-words-octets w 8))))
