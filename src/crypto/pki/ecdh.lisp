(defpackage #:clempo.ecdh
  (:use #:cl #:clempo.base)
  (:documentation "Ephemeral ECDH on NIST P-256 (SEC 1, uncompressed points) for TLS key
shares. Constant-time in the secret scalar: complete projective formulas, masked table
reads and a Fermat inversion (clempo.ct).")
  (:export #:p256-ecdh-public-key #:p256-ecdh-shared))
(in-package #:clempo.ecdh)

(defconstant +p+ #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff)
(defconstant +b+ #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b)
(defconstant +n+ #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)

(clempo.ct:define-ct-field ct-fe #.+p+)
(clempo.ct:define-ct-curve ct-point :field ct-fe :width 4 :a -3 :b #.+b+ :order #.+n+)

(defparameter *generator-comb*
  (ct-point-build-comb #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
                       #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5))

(defun on-curve-p (x y)
  (and (< -1 x +p+) (< -1 y +p+)
       (= (mod (* y y) +p+) (mod (+ (* x x x) (* -3 x) +b+) +p+))))

(defun encode-affine (w o)
  "SEC 1 uncompressed encoding of the affine Montgomery pair at O."
  (let ((out (make-octets 65)))
    (setf (aref out 0) 4)
    (ct-fe-to-words! w o w o) (ct-fe-to-words! w (+ o 4) w (+ o 4))
    (clempo.ct:words-to-octets! out 1 w o 4)
    (clempo.ct:words-to-octets! out 33 w (+ o 4) 4)
    out))

(defun p256-ecdh-public-key (secret)
  "The 65-byte uncompressed public point for 32-byte SECRET (a scalar in [1, n-1])."
  (clempo.ct:with-data-independent-timing
    (assert (and (= (length secret) 32) (< 0 (octets->integer secret) +n+)))
    (let ((w (clempo.ct:make-words 20)))
      (ct-point-mul-comb! w 0 *generator-comb* secret)
      (ct-point-affine! w 12 w 0)
      (encode-affine w 12))))

(defun p256-ecdh-shared (secret peer)
  "The 32-byte x coordinate of SECRET*PEER, or NIL when PEER is not a valid point."
  (clempo.ct:with-data-independent-timing
    (when (and (= (length secret) 32) (= (length peer) 65) (= (aref peer 0) 4))
      (let ((x (octets->integer peer :start 1 :end 33)) (y (octets->integer peer :start 33)))
        (when (on-curve-p x y)
          (let ((w (clempo.ct:make-words 20)))
            (ct-point-from-affine-integers! w 0 x y)
            (ct-point-mul! w 0 w 0 secret)
            (when (zerop (ct-point-affine! w 12 w 0))
              (subseq (encode-affine w 12) 1 33))))))))
