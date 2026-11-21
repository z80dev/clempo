(in-package #:clempo.p256)

(defconstant +p256-n+ #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)

(clempo.ec:define-prime-curve p256
  :p #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  :multiply p256-field-multiply
  :a -3 :b #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
  :n #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  :gx #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
  :gy #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)

(defun p256-verify (hash32 r s qx qy)
  "EIP-7951 P256VERIFY semantics. Verifies an already computed 32-byte digest.
High-s signatures are valid here. Invalid scalar/coordinate ranges, infinity,
off-curve public keys and an infinity verification result return NIL."
  (unless (and (typep hash32 'clempo.base:octets) (= (length hash32) 32)
               (typep r `(integer 1 ,(1- +p256-n+)))
               (typep s `(integer 1 ,(1- +p256-n+)))
               (p256-point-p qx qy))
    (return-from p256-verify nil))
  (let* ((si (clempo.ec:invert s +p256-n+))
         (u1 (mod (* (clempo.base:octets->integer hash32) si) +p256-n+))
         (u2 (mod (* r si) +p256-n+)))
    (let ((x (p256-joint u1 p256-generator u2 (p256-table qx qy) 7 5)))
      ;; EIP7951 compares x mod n, not the un-reduced field element x.
      (and x (= (mod x +p256-n+) r)))))

(defun p256-verify-low-s (hash32 r s qx qy)
  "Tempo P256/WebAuthn transaction signature rule, including low-s validation.
The caller supplies the prehashed or WebAuthn digest, not the raw challenge."
  (and (typep s `(integer 1 ,(ash +p256-n+ -1)))
       (p256-verify hash32 r s qx qy)))
