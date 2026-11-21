(defpackage #:clempo.ec.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.ec.tests)

(defun fixture (name)
  (asdf:system-relative-pathname "clempo-crypto-ec/tests"
                                 (concatenate 'string "../../../tests/crypto/ec/data/" name)))

(defun each-line (name fn)
  (with-open-file (in (fixture name))
    (loop for line = (read-line in nil) while line do
      (unless (or (zerop (length line)) (char= (char line 0) #\#))
        (funcall fn (uiop:split-string line :separator '(#\Space #\Tab)))))))

(defun recover-address (hash r s v)
  (let ((pub (clempo.secp256k1:ecrecover hash r s v)))
    (and pub (clempo.secp256k1:pubkey->address pub))))

(defun check-transaction (raw sender)
  (let* ((typed (< (aref raw 0) #x80))
         (fields (clempo.rlp:rlp-decode raw :start (if typed 1 0)))
         (n (length fields))
         (v (octets->integer (nth (- n 3) fields)))
         (r (octets->integer (nth (- n 2) fields)))
         (s (octets->integer (nth (1- n) fields)))
         (unsigned (subseq fields 0 (- n 3)))
         (recid (if typed v (if (>= v 35) (mod (- v 35) 2) (- v 27))))
         (payload (clempo.rlp:rlp-encode
                   (if (and (not typed) (>= v 35))
                       (append unsigned (list (floor (- v 35) 2) 0 0)) unsigned)))
         (hash (clempo.keccak:keccak256
                (if typed (octets-concat (octets-of (aref raw 0)) payload) payload))))
    (is= sender (recover-address hash r s recid))))

(deftest secp-sender-corpus ()
  (each-line "secp256k1/tempo_mainnet_senders.txt"
             (lambda (cols)
               (check-transaction (hex->octets (first cols)) (hex->integer (second cols)))))
  (each-line "secp256k1/eest_senders.txt"
             (lambda (cols)
               (if (string= (first cols) "tx")
                   (check-transaction (hex->octets (second cols)) (hex->integer (third cols)))
                   (destructuring-bind (kind chain address nonce v r s sender) cols
                     (declare (ignore kind))
                     (let ((hash (clempo.keccak:keccak256
                                  (octets-concat (octets-of 5)
                                   (clempo.rlp:rlp-encode
                                    (list (hex->integer chain) (hex->octets address)
                                          (hex->integer nonce)))))))
                       (is= (hex->integer sender)
                            (recover-address hash (hex->integer r) (hex->integer s)
                                             (hex->integer v)))))))))

(deftest secp-ecdh-corpus ()
  (each-line "secp256k1/ecdh.txt"
             (lambda (cols)
               (destructuring-bind (priv peer expected) cols
                 (is-equalp (hex->octets expected)
                            (clempo.secp256k1:ecdh (integer->octets (hex->integer priv) 32)
                                                   (hex->octets peer)))))))

(deftest secp-rfc6979 ()
  ;; RustCrypto k256 0.13.4 src/ecdsa.rs, ethereum_end_to_end_example.
  (let* ((priv (hex->octets "4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318"))
         (hash (clempo.keccak:keccak256
                (hex->octets "e9808504e3b29200831e848094f0109fc8df283027b6285cc889f5aa624eac1f55843b9aca0080018080"))))
    (multiple-value-bind (r s v) (clempo.secp256k1:sign hash priv)
      (is= #xc9cf86333bcb065d140032ecaab5d9281bde80f21b9687b3e94161de42d51895 r)
      (is= #x727a108a0b8d101465414033c3f705a9c7b826e596766046ee1183dbc8aeaa68 s)
      (is= 0 v)
      (is-equalp (clempo.secp256k1:private->public priv)
                 (clempo.secp256k1:ecrecover hash r s v))
      (is-equalp (clempo.secp256k1:private->public priv)
                 (clempo.secp256k1:ecrecover hash r (- clempo.secp256k1:+secp256k1-n+ s)
                                           (logxor v 1))))))

(deftest secp-invalid-inputs ()
  (let ((hash (make-octets 32)) (n clempo.secp256k1:+secp256k1-n+))
    (dolist (r (list -1 0 n (1+ n))) (is (null (clempo.secp256k1:ecrecover hash r 1 0))))
    (dolist (s (list -1 0 n (1+ n))) (is (null (clempo.secp256k1:ecrecover hash 1 s 0))))
    (dolist (v '(-1 4 27 28)) (is (null (clempo.secp256k1:ecrecover hash 1 1 v))))
    (is (null (clempo.secp256k1:ecrecover (make-octets 31) 1 1 0)))
    (is (null (clempo.secp256k1:ecrecover hash (1- n) 1 2)))
    (is (null (clempo.secp256k1:private->public (make-octets 32))))
    (is (null (clempo.secp256k1:private->public (integer->octets n 32))))
    (is (null (clempo.secp256k1:private->public (integer->octets (1+ n) 32))))
    (is (null (clempo.secp256k1:private->public 1)))   ; integers are not keys
    (is (null (clempo.secp256k1:private->public (subseq (integer->octets 1 32) 1))))
    (is (null (clempo.secp256k1:ecdh (integer->octets 1 32) (make-octets 64))))
    (signals error (clempo.secp256k1:sign hash (integer->octets n 32)))
    (is (clempo.secp256k1:private-key-p (integer->octets (1- n) 32)))
    (is (not (clempo.secp256k1:private-key-p (integer->octets 0 32))))))

(deftest p256-eip7951-corpus ()
  (each-line "p256/eest_p256verify.txt"
             (lambda (cols)
               (let* ((input (hex->octets (sixth cols)))
                      (valid (= (length input) 160))
                      (expected (string= (fourth cols) "32"))
                      (actual
                        (and valid
                             (clempo.p256:p256-verify
                              (subseq input 0 32)
                              (octets->integer input :start 32 :end 64)
                              (octets->integer input :start 64 :end 96)
                              (octets->integer input :start 96 :end 128)
                              (octets->integer input :start 128 :end 160)))))
                 (is= expected actual)
                 (when actual
                   (let* ((r (octets->integer input :start 32 :end 64))
                          (s (octets->integer input :start 64 :end 96))
                          (x (octets->integer input :start 96 :end 128))
                          (y (octets->integer input :start 128 :end 160))
                          (hash (subseq input 0 32)))
                     (is (clempo.p256:p256-verify hash r (- clempo.p256:+p256-n+ s) x y))
                     (is= (<= s (ash clempo.p256:+p256-n+ -1))
                          (clempo.p256:p256-verify-low-s hash r s x y))))))))

(defun padded-slice (data start size)
  (let ((out (make-octets size)))
    (when (< start (length data))
      (replace out data :start2 start :end2 (min (+ start size) (length data))))
    out))

(deftest modular-expt-small ()
  (loop for modulus from 1 to 33 do
    (loop for base from 0 to 13 do
      (loop for exponent from 0 to 18 do
        (is= (mod (expt base exponent) modulus)
             (clempo.modexp:modular-expt base exponent modulus)))))
  (is-equalp (make-octets 0) (clempo.modexp:modexp (make-octets 0) (make-octets 0) (make-octets 0)))
  (is-equalp (octets-of 0 0) (clempo.modexp:modexp (octets-of 7) (make-octets 0) (octets-of 0 0))))

(deftest modexp-precompile-corpus ()
  (dolist (file '("modexp/modexp_eip2565.json" "modexp/modexp_eip7883.json"))
    (with-open-file (in (fixture file))
      (let* ((text (make-string (file-length in)))
             (cases (progn (read-sequence text in) (clempo.json:parse-json text))))
        (loop for case across cases do
          (let* ((input (hex->octets (clempo.json:jget case "Input")))
                 (blen (octets->integer (padded-slice input 0 32)))
                 (elen (octets->integer (padded-slice input 32 32)))
                 (mlen (octets->integer (padded-slice input 64 32)))
                 (expected (hex->octets (clempo.json:jget case "Expected")))
                 (actual (if (zerop mlen) (make-octets 0)
                             (clempo.modexp:modexp
                              (padded-slice input 96 blen)
                              (padded-slice input (+ 96 blen) elen)
                              (padded-slice input (+ 96 blen elen) mlen)))))
            (is-equalp expected actual "~A" (clempo.json:jget case "Name"))))))))

;;; Integer adapters are test-only: the production field no longer constructs
;;; bignums per operation. Preserve the old boundary/random input datasets.
(defun native-field-reduce (integer)
  (let ((w (make-array 16 :element-type '(unsigned-byte 64) :initial-element 0))
        (magnitude (abs integer)))
    (setf (aref w 5) 1) ; 2^64, not a Montgomery operand.
    (loop for i downfrom (max 0 (floor (1- (integer-length magnitude)) 64)) to 0 do
      (clempo.secp256k1::sf-mul! w 0 w 0 w 4)
      (setf (aref w 8) (ldb (byte 64 (* i 64)) magnitude))
      (clempo.secp256k1::sf-add! w 0 w 0 w 8))
    (when (minusp integer) (clempo.secp256k1::sf-neg! w 0 w 0))
    (clempo.secp256k1::sf-to-integer w 0)))
(defun native-field-op (fn x y)
  (let ((w (make-array 12 :element-type '(unsigned-byte 64) :initial-element 0)))
    (clempo.secp256k1::sf-from-integer! w 0 x)
    (clempo.secp256k1::sf-from-integer! w 4 y)
    (funcall fn w 8 w 0 w 4)
    (clempo.secp256k1::sf-to-integer w 8)))
(defun native-scalar-parts (k)
  (let ((w (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
        (digits (make-array 520 :element-type '(signed-byte 8) :initial-element 0)))
    (clempo.secp256k1::sf-from-integer! w 0 k)
    (clempo.secp256k1::sg-decompose! w 0 digits 0 130 7 32)
    (flet ((part (start)
             (let ((value 0))
               (loop for i downfrom 129 to 0 do
                 (setf value (+ (* 2 value) (aref digits (+ start i))))) value)))
      (values (part 0) (part 130)))))

(deftest secp-native-field-reduction ()
  (let ((p clempo.secp256k1:+secp256k1-p+))
    (dolist (x (list -1 0 1 (1- p) p (1+ p) (1- (* p 2)) (* p 2)
                     (1- (* p p)) (* p p) (1- (ash 1 512)) (ash 1 512)))
      (is= (mod x p) (native-field-reduce x)))
    (let ((x 17) (mask (1- (ash 1 512))))
      (dotimes (i 10000)
        (setf x (logand (+ (* x #x5851f42d4c957f2d) #x14057b7ef767814f) mask))
        (is= (mod x p) (native-field-reduce x))))))

(deftest secp-glv-decomposition ()
  (let ((n clempo.secp256k1:+secp256k1-n+)
        (lambda #x5363ad4cc05c30e0a5261c028812645a122e22ea20816678df02967c1b23bd72))
    (loop for i from 0 below 12
          for k = (mod (+ (* i (ash n -4)) #x127bf3193d) n) do
      (multiple-value-bind (a b) (native-scalar-parts k)
        (is= k (mod (+ a (* b lambda)) n))
        (is (<= (integer-length (abs a)) 129))
        (is (<= (integer-length (abs b)) 129))
        (let ((expected (clempo.secp256k1.native-group-tests::oracle-mul
                         (clempo.secp256k1.native-group-tests::oracle-g) k)))
          (is-equalp (octets-concat (integer->octets (car expected) 32)
                                   (integer->octets (cdr expected) 32))
                     (clempo.secp256k1:private->public (integer->octets k 32))))))))

(deftest secp-native-field-multiply ()
  (let ((p clempo.secp256k1:+secp256k1-p+))
    (dolist (x (list 0 1 2 (1- (ash 1 64)) (ash 1 64) (ash 1 128) (ash 1 192) (1- p)))
      (dolist (y (list 0 1 2 (1- (ash 1 64)) (ash 1 64) (ash 1 128) (ash 1 192) (1- p)))
        (is= (mod (* x y) p) (native-field-op #'clempo.secp256k1::sf-mul! x y))))
    (let ((x 17) (y 23))
      (dotimes (i 10000)
        (setf x (mod (+ (* x #x5851f42d4c957f2d) #x14057b7ef767814f) p)
              y (mod (+ (* y #x14057b7ef767814f) #x5851f42d4c957f2d) p))
        (is= (mod (* x y) p) (native-field-op #'clempo.secp256k1::sf-mul! x y))))))

(deftest secp-native-field-add-subtract-scale ()
  (let ((p clempo.secp256k1:+secp256k1-p+))
    (flet ((check (x y)
             (is= (mod (+ x y) p) (native-field-op #'clempo.secp256k1::sf-add! x y))
             (is= (mod (- x y) p) (native-field-op #'clempo.secp256k1::sf-sub! x y))
             (dotimes (scale 9)
               (is= (mod (* x scale) p) (native-field-op #'clempo.secp256k1::sf-mul! x scale)))))
      (dolist (x (list 0 1 2 (1- (ash 1 64)) (ash 1 64) (ash 1 128) (ash 1 192) (1- p)))
        (dolist (y (list 0 1 2 (1- (ash 1 64)) (ash 1 64) (ash 1 128) (ash 1 192) (1- p)))
          (check x y)))
      (let ((x 17) (y 23))
        (dotimes (i 1000)
          (setf x (mod (+ (* x #x5851f42d4c957f2d) #x14057b7ef767814f) p)
                y (mod (+ (* y #x14057b7ef767814f) #x5851f42d4c957f2d) p))
          (check x y))))))
