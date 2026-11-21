(defpackage #:clempo.secp256k1
  (:use #:cl)
  (:import-from #:clempo.base #:octets #:octets->integer #:integer->octets #:octets-concat)
  (:export #:ecrecover #:pubkey->address #:sign #:private->public #:ecdh
           #:+secp256k1-n+ #:+secp256k1-p+))
(in-package #:clempo.secp256k1)

(defconstant +secp256k1-p+ #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f)
(defconstant +secp256k1-n+ #xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)


(clempo.ec:define-prime-curve secp
  :p #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
  :a 0 :b 7
  :n #xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
  :gx #x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
  :gy #x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)

(defun encode-public (x y)
  (when x (octets-concat (integer->octets x 32) (integer->octets y 32))))

(defun private->public (private-key)
  "Uncompressed public key x||y (64 octets), or NIL for an invalid private scalar."
  (when (typep private-key `(integer 1 ,(1- +secp256k1-n+)))
    (multiple-value-bind (x y) (secp-mul private-key) (encode-public x y))))

(defun pubkey->address (public-key)
  "Low 160 bits of Keccak256(x||y), as an integer."
  (check-type public-key octets)
  (assert (= (length public-key) 64))
  (ldb (byte 160 0) (octets->integer (clempo.keccak:keccak256 public-key))))

(defun ecrecover (hash32 r s recid)
  "Recover x||y or NIL. Accepts high s, as Ethereum's 0x01 precompile does.
RECID is 0..3 (0/1 for Ethereum), NOT the precompile v byte 27/28."
  (unless (and (typep hash32 'octets) (= (length hash32) 32)
               (typep r `(integer 1 ,(1- +secp256k1-n+)))
               (typep s `(integer 1 ,(1- +secp256k1-n+)))
               (typep recid '(integer 0 3)))
    (return-from ecrecover nil))
  (let ((rx (+ r (* (ash recid -1) +secp256k1-n+))))
    (when (>= rx +secp256k1-p+) (return-from ecrecover nil))
    (let* ((yy (secp-f (+ (* rx rx rx) 7)))
           (ry (clempo.modexp:modular-expt yy (ash (1+ +secp256k1-p+) -2)
                                         +secp256k1-p+)))
      (unless (= (secp-f* ry ry) yy) (return-from ecrecover nil))
      (unless (= (logand ry 1) (logand recid 1)) (setf ry (secp-f- 0 ry)))
      ;; The curve has prime order n (cofactor 1), so every non-infinity R is in G.
      (let* ((ri (clempo.ec:invert r +secp256k1-n+))
             (u (mod (* (- (octets->integer hash32)) ri) +secp256k1-n+))
             (v (mod (* s ri) +secp256k1-n+)))
        (multiple-value-bind (x y) (secp-joint u secp-generator v (secp-table rx ry) 7 5)
          (encode-public x y))))))

(defun ecdh (private-key peer-public-key)
  "ECDH raw shared x (32 octets), NOT the libsecp hashed ECDH convention.
Returns NIL for an invalid private scalar or non-curve peer x||y."
  (unless (and (typep private-key `(integer 1 ,(1- +secp256k1-n+)))
               (typep peer-public-key 'octets) (= (length peer-public-key) 64))
    (return-from ecdh nil))
  (let ((x (octets->integer peer-public-key :end 32))
        (y (octets->integer peer-public-key :start 32)))
    (when (secp-point-p x y)
      (let ((shared (secp-mul private-key (secp-table x y) 5)))
        (when shared (integer->octets shared 32))))))

(defun sign (hash32 private-key)
  "RFC6979/HMAC-SHA256 deterministic ECDSA. Returns r,s,recid, low-s.
This bignum implementation is variable-time; do not expose it to a hostile
co-resident timing observer when using a long-lived secret key."
  (check-type hash32 octets)
  (assert (= (length hash32) 32))
  (assert (typep private-key `(integer 1 ,(1- +secp256k1-n+))))
  (let* ((h (octets->integer hash32))
         (seed (octets-concat (integer->octets private-key 32)
                              (integer->octets (mod h +secp256k1-n+) 32)))
         (k (clempo.base:make-octets 32))
         (v (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1)))
    (flet ((mac (key data) (clempo.sha2:hmac-sha256 key data)))
      (setf k (mac k (octets-concat v (clempo.base:octets-of 0) seed))
            v (mac k v)
            k (mac k (octets-concat v (clempo.base:octets-of 1) seed))
            v (mac k v))
      (loop
        (setf v (mac k v))
        (let ((nonce (octets->integer v)))
          (when (< 0 nonce +secp256k1-n+)
            (multiple-value-bind (x y) (secp-mul nonce)
              (let* ((r (mod x +secp256k1-n+))
                     (s (mod (* (clempo.ec:invert nonce +secp256k1-n+)
                                (+ h (* r private-key))) +secp256k1-n+))
                     (recid (+ (if (>= x +secp256k1-n+) 2 0) (logand y 1))))
                (unless (or (zerop r) (zerop s))
                  (when (> s (ash +secp256k1-n+ -1))
                    (setf s (- +secp256k1-n+ s) recid (logxor recid 1)))
                  (return (values r s recid)))))))
        ;; RFC6979 section 3.2h: rejection updates K and V before trying again.
        (setf k (mac k (octets-concat v (clempo.base:octets-of 0)))
              v (mac k v))))))
