(in-package #:clempo.secp256k1)

;;; Private keys are 32 big-endian octets of a scalar in [1, n), end to end: no secret
;;; scalar becomes a Lisp integer, whose bignum length would depend on its value.

(deftype private-key () '(octets 32))

(defun private-key-p (key)
  "Whether KEY is a PRIVATE-KEY: 32 octets of a scalar in [1, n). The range check is
constant-time; only its single public verdict branches."
  (and (typep key '(octets 32)) (ct-scalar-octets-valid-p key)))

(defun random-private-key (&optional (rng #'clempo.io:random-octets))
  "A fresh PRIVATE-KEY from RNG (a function of an octet count, clempo.io:*entropy* by
default), by rejection: a draw is out of range with probability below 2^-127."
  (loop for key = (funcall rng 32)
        when (private-key-p key) return key
        do (clempo.ct:wipe key)))

(defun private->public (private-key)
  "Uncompressed public key x||y, or NIL unless PRIVATE-KEY is a PRIVATE-KEY.
Constant-time in the key: comb multiplication with masked table reads."
  (clempo.ct:with-data-independent-timing
    (when (private-key-p private-key)
      (clempo.ct:with-wiped ((w (clempo.ct:make-words 20)))
        (ct-point-mul-comb! w 0 *ct-generator-comb* private-key)
        (ct-point-affine! w 12 w 0)
        (ct-write-affine! (clempo.base:make-octets 64) 0 w 12)))))

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
Return NIL unless PRIVATE-KEY is a PRIVATE-KEY and the peer point is valid."
  (clempo.ct:with-data-independent-timing
    (unless (and (private-key-p private-key)
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
      ;; The peer point is public; the scalar multiplication is constant-time.
      (clempo.ct:with-wiped ((w (clempo.ct:make-words 32)))
        (ct-point-from-affine-integers! w 0 (octets->integer peer-public-key :end 32)
                                        (octets->integer peer-public-key :start 32))
        (ct-point-mul! w 0 w 0 private-key)
        (when (zerop (ct-point-affine! w 12 w 0))
          (clempo.ct:with-wiped ((point (ct-write-affine! (clempo.base:make-octets 64) 0 w 12)))
            (let ((out (clempo.base:make-octets (if compressed 33 32))))
              (when compressed (setf (aref out 0) (+ 2 (logand (aref point 63) 1))))
              (replace out point :start1 (if compressed 1 0) :end2 32))))))))

(defun sign (hash32 private-key)
  "RFC6979/HMAC-SHA256 deterministic ECDSA with a PRIVATE-KEY. Returns r,s,recid, low-s.
Constant-time in the private key and nonce: the nonce point comes from a comb
with masked table reads, k^-1 is a Fermat power with a fixed exponent, and no
branch depends on either secret. Only the public outputs (r, s, recid) are
integers; the HMAC state and nonce candidates are zeroed before returning."
  (clempo.ct:with-data-independent-timing
    (check-type hash32 octets)
    (assert (= (length hash32) 32))
    (assert (private-key-p private-key))
    (clempo.ct:with-wiped
        ((reduced (integer->octets (mod (octets->integer hash32) +secp256k1-n+) 32))
         (k (clempo.base:make-octets 32))
         (v (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1)))
      (flet ((mac! (place key &rest data)
               ;; PLACE := HMAC(KEY, DATA...), zeroing the fresh digest once copied.
               (let ((digest (apply #'clempo.sha2:hmac-sha256 key data)))
                 (replace place digest)
                 (clempo.ct:wipe digest))))
        (let ((zero (clempo.base:octets-of 0)) (one (clempo.base:octets-of 1)))
          (mac! k k v zero private-key reduced)
          (mac! v k v)
          (mac! k k v one private-key reduced)
          (mac! v k v)
          (loop
            (mac! v k v)
            ;; Rejection reveals only that a candidate was out of range (p < 2^-127).
            (when (ct-scalar-octets-valid-p v)
              (multiple-value-bind (r s parity overflow) (ct-sign hash32 private-key v)
                (let ((recid (+ (if overflow 2 0) parity)))
                  (unless (or (zerop r) (zerop s))
                    (when (> s (ash +secp256k1-n+ -1))
                      (setf s (- +secp256k1-n+ s) recid (logxor recid 1)))
                    (return (values r s recid))))))
            ;; RFC6979 section 3.2h: rejection updates K and V before trying again.
            (mac! k k v zero)
            (mac! v k v)))))))
