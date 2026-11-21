(defpackage #:clempo.p384
  (:use #:cl)
  (:export #:p384-verify #:p384-valid-public-key-p #:+p384-p+ #:+p384-n+))
(in-package #:clempo.p384)

;; NIST P-384 / SEC 2 secp384r1 parameters.
(defconstant +p384-p+ #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff)
(defconstant +p384-n+ #xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973)

(clempo.ec:define-prime-curve p384
  :p #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
  :a -3
  :b #xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
  :n #xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
  :gx #xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7
  :gy #x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f)

(declaim (inline p384-valid-public-key-p))
(defun p384-valid-public-key-p (x y)
  "True only for canonical, non-infinite affine NIST P-384 public keys."
  (p384-point-p x y))

(defun p384-verify (hash48 r s qx qy)
  "Verify P-384 ECDSA over an already computed 48-byte SHA-384 digest.
Accept both low- and high-s, as Tempo's Nitro P384Verifier requires. Reject
noncanonical scalars/coordinates, off-curve keys and infinity results. The
caller parses SEC1/DER/P1363; this function does not parse encodings or hash."
  (unless (and (typep hash48 'clempo.base:octets) (= (length hash48) 48)
               (typep r `(integer 1 ,(1- +p384-n+)))
               (typep s `(integer 1 ,(1- +p384-n+)))
               (p384-valid-public-key-p qx qy))
    (return-from p384-verify nil))
  (let* ((si (clempo.ec:invert s +p384-n+))
         (u1 (mod (* (clempo.base:octets->integer hash48) si) +p384-n+))
         (u2 (mod (* r si) +p384-n+))
         (x (p384-joint u1 p384-generator u2 (p384-table qx qy) 7 5)))
    (and x (= (mod x +p384-n+) r))))
