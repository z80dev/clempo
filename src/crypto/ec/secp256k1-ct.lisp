;;; Constant-time secp256k1 for secret scalars: signing nonces, public keys and
;;; ECDH. Recovery and verification keep the faster variable-time kernels; their
;;; inputs are public.
(in-package #:clempo.secp256k1)

(clempo.ct:define-ct-field ct-fe #.+secp256k1-p+)
(clempo.ct:define-ct-field ct-scalar #.+secp256k1-n+)
(clempo.ct:define-ct-curve ct-point :field ct-fe :width 4 :a 0 :b 7 :order #.+secp256k1-n+)

(declaim (type clempo.ct:words *ct-generator-comb*))
(defparameter *ct-generator-comb* (ct-point-build-comb +sn-generator-x+ +sn-generator-y+)
  "(2j+1)*16^i*G for i < 65, j < 8: fixed-base multiplication without doublings.")

(defun ct-write-affine! (out start arena o)
  "Write the canonical big-endian x||y of the affine Montgomery pair at O."
  (ct-fe-to-words! arena o arena o)
  (ct-fe-to-words! arena (+ o 4) arena (+ o 4))
  (clempo.ct:words-to-octets! out start arena o 4)
  (clempo.ct:words-to-octets! out (+ start 32) arena (+ o 4) 4)
  out)

(defun ct-sign (hash32 secret nonce)
  "R = NONCE*G and s = NONCE^-1 (h + r d) mod n from 32-octet big-endian SECRET
and NONCE in [1, n). Return r, s and R's y parity and x overflow (both public)."
  (clempo.ct:with-wiped ((w (clempo.ct:make-words 48)))
    ;; 0 R (12 words), 12 affine x y, 20 k^-1, 24 d, 28 h, 32 r then s.
    (ct-point-mul-comb! w 0 *ct-generator-comb* nonce)
    (ct-point-affine! w 12 w 0)
    (let* ((x (ct-fe-to-integer w 12)) (y (ct-fe-to-integer w 16)) (r (mod x +secp256k1-n+)))
      (clempo.ct:words-from-octets! w 20 4 nonce 0)
      (ct-scalar-from-words! w 20 w 20)
      (ct-scalar-inv! w 20 w 20)
      (clempo.ct:words-from-octets! w 24 4 secret 0)
      (ct-scalar-from-words! w 24 w 24)
      (clempo.ct:words-from-octets! w 28 4 hash32 0)
      (ct-scalar-from-words! w 28 w 28)
      (ct-scalar-from-integer! w 32 r)
      (ct-scalar-mul! w 32 w 32 w 24)
      (ct-scalar-add! w 32 w 32 w 28)
      (ct-scalar-mul! w 32 w 32 w 20)
      (values r (ct-scalar-to-integer w 32) (logand y 1) (>= x +secp256k1-n+)))))

(defun ct-scalar-octets-valid-p (octets)
  "Whether the 32 big-endian OCTETS are a scalar in [1, n), without a data-dependent
branch before the single public answer."
  (clempo.ct:with-wiped ((w (clempo.ct:make-words 4)))
    (clempo.ct:words-from-octets! w 0 4 octets 0)
    (= 1 (logand (ct-scalar-less-bit w 0) (logxor 1 (ct-scalar-zero-bit w 0))))))
