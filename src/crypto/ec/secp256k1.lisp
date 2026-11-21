(in-package #:clempo.secp256k1)

(defun private->public (private-key)
  "Uncompressed public key x||y, or NIL for an invalid private scalar."
  (when (and (integerp private-key) (< 0 private-key +secp256k1-n+))
    (let* ((context (or *recovery-scratch* (make-recovery-scratch)))
           (arena (recovery-scratch-words context)))
      (sf-from-integer! arena 0 +sn-generator-x+)
      (sf-from-integer! arena 4 +sn-generator-y+)
      (sf-from-integer! arena 24 private-key) (sf-zero! arena 28)
      (when (native-joint-affine! arena (recovery-scratch-digits context))
        (write-native-public! (clempo.base:make-octets 64) arena)))))

(defun pubkey->address (public-key)
  "Low 160 bits of Keccak256(x||y), as an integer."
  (check-type public-key octets)
  (assert (= (length public-key) 64))
  (let ((digest (if *recovery-scratch* (recovery-scratch-digest *recovery-scratch*)
                    (clempo.base:make-octets 32))))
    (clempo.keccak:keccak256-into public-key 0 64 digest 0)
    (digest-address digest)))

(defun ecrecover (hash32 r s recid &optional output (scratch *recovery-scratch*))
  "Recover x||y or NIL; high s is accepted. RECID is 0..3, not v=27/28.
OUTPUT, if non-NIL, must be a caller-owned 64-octet vector. With caller SCRATCH
and OUTPUT the complete call allocates nothing. Defaults return fresh results."
  (unless (valid-recovery-input-p hash32 r s recid) (return-from ecrecover nil))
  (check-type output (or null octets))
  (when output (assert (= (length output) 64)))
  (check-type scratch (or null recovery-scratch))
  (let* ((context (or scratch (make-recovery-scratch)))
         (arena (recovery-scratch-words context)))
    (when (recover-native! hash32 r s recid arena (recovery-scratch-digits context))
      (write-native-public! (or output (clempo.base:make-octets 64)) arena))))

(defun ecrecover-address (hash32 r s recid &optional (scratch *recovery-scratch*))
  "Recover the integer address, or NIL. With SCRATCH only the result allocates.
SCRATCH is private mutable workspace and must not be used concurrently."
  (unless (valid-recovery-input-p hash32 r s recid) (return-from ecrecover-address nil))
  (check-type scratch (or null recovery-scratch))
  (let* ((context (or scratch (make-recovery-scratch)))
         (arena (recovery-scratch-words context))
         (public (recovery-scratch-public context)) (digest (recovery-scratch-digest context)))
    (when (recover-native! hash32 r s recid arena (recovery-scratch-digits context))
      (write-native-public! public arena)
      (clempo.keccak:keccak256-into public 0 64 digest 0)
      (digest-address digest))))

(defun ecdh (private-key peer-public-key &key compressed)
  "Raw shared x32, or SEC1 compressed shared point33 with :COMPRESSED T.
Return NIL for an invalid private scalar or peer point."
  (unless (and (integerp private-key) (< 0 private-key +secp256k1-n+)
               (typep peer-public-key 'octets) (= (length peer-public-key) 64))
    (return-from ecdh nil))
  (let* ((context (or *recovery-scratch* (make-recovery-scratch)))
         (arena (recovery-scratch-words context)))
    (sw-read32! arena 0 peer-public-key 0) (sw-read32! arena 4 peer-public-key 32)
    (when (or (sw-ge-p arena 0 *sf-prime* 0) (sw-ge-p arena 4 *sf-prime* 0))
      (return-from ecdh nil))
    (sf-square! arena 32 arena 0) (sf-mul! arena 32 arena 32 arena 0)
    (sf-add! arena 32 arena 32 *sf-seven* 0) (sf-square! arena 36 arena 4)
    (unless (sf-equal-p arena 32 arena 36) (return-from ecdh nil))
    (sf-zero! arena 24) (sf-from-integer! arena 28 private-key)
    (when (native-joint-affine! arena (recovery-scratch-digits context))
      (let ((out (clempo.base:make-octets (if compressed 33 32))))
        (when compressed (setf (aref out 0) (+ 2 (logand (aref arena 4) 1))))
        (sw-write32! out (if compressed 1 0) arena 0) out))))

(defun sign (hash32 private-key)
  "RFC6979/HMAC-SHA256 deterministic ECDSA. Returns r,s,recid, low-s.
This implementation is variable-time; do not expose a long-lived secret key
to a hostile co-resident timing observer."
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
            (multiple-value-bind (x y) (native-generator-mul nonce)
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
