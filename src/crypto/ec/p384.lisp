(in-package #:clempo.p384)

(declaim (inline p384-canonical-coordinate-p))
(defun p384-canonical-coordinate-p (coordinate)
  (typep coordinate '(integer 0 #.(1- +p384-p+))))

(defun p384-valid-public-key-p (x y &optional scratch)
  "True only for canonical, non-infinite affine NIST P-384 public keys."
  (and (p384-canonical-coordinate-p x)
       (p384-canonical-coordinate-p y)
       (p384-valid-native-public-key-p
        x y (or scratch *p384-scratch* (make-p384-scratch)))))

(defun p384-verify (hash48 r s qx qy &optional scratch)
  "Verify P-384 ECDSA over an already computed 48-byte SHA-384 digest.
Accept both low- and high-s, as Tempo's Nitro P384Verifier requires. Reject
noncanonical scalars/coordinates, off-curve keys and infinity results. The
caller parses SEC1/DER/P1363; this function does not parse encodings or hash.
SCRATCH, or a bound *P384-SCRATCH*, is private mutable nonreentrant storage."
  (and (typep hash48 'clempo.base:octets) (= (length hash48) 48)
       (typep r '(integer 1 #.(1- +p384-n+)))
       (typep s '(integer 1 #.(1- +p384-n+)))
       (p384-canonical-coordinate-p qx)
       (p384-canonical-coordinate-p qy)
       (let* ((context (or scratch *p384-scratch* (make-p384-scratch)))
              (cache (p384-scratch-cache context)))
         (or (and cache (p384-cache-hit-p cache hash48 r s qx qy))
             (when (p384-verify-native hash48 r s qx qy context)
               (when cache (p384-cache-store! cache hash48 r s qx qy))
               t)))))
