(defpackage #:clempo.ecdh
  (:use #:cl #:clempo.base)
  (:documentation "Ephemeral ECDH on NIST P-256 (SEC 1, uncompressed points) for TLS key
shares. Built on clempo.ec's generic Jacobian arithmetic; variable-time, which an
ephemeral per-handshake key tolerates.")
  (:export #:p256-ecdh-public-key #:p256-ecdh-shared))
(in-package #:clempo.ecdh)

(clempo.ec:define-prime-curve ecdh256
  :p #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  :a #xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
  :b #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
  :n #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  :gx #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
  :gy #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)

(defconstant +n+ #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)

(defun encode (x y)
  (octets-concat (octets-of 4) (integer->octets x 32) (integer->octets y 32)))

(defun p256-ecdh-public-key (secret)
  "The 65-byte uncompressed public point for 32-byte SECRET (a scalar in [1, n-1])."
  (let ((k (octets->integer secret)))
    (assert (< 0 k +n+))
    (multiple-value-call #'encode (ecdh256-mul k))))

(defun p256-ecdh-shared (secret peer)
  "The 32-byte x coordinate of SECRET*PEER, or NIL when PEER is not a valid point."
  (when (and (= (length peer) 65) (= (aref peer 0) 4))
    (let ((x (octets->integer peer :start 1 :end 33)) (y (octets->integer peer :start 33)))
      (when (ecdh256-point-p x y)
        (multiple-value-bind (sx sy)
            (ecdh256-mul (octets->integer secret) (ecdh256-table x y 5) 5)
          (declare (ignore sy))
          (and sx (integer->octets sx 32)))))))
