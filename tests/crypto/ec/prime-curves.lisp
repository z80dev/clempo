(defpackage #:clempo.ec.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.ec.tests)

(defun fixture (name)
  (asdf:system-relative-pathname "clempo-crypto-ec/tests" (concatenate 'string "data/" name)))

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
                            (clempo.secp256k1:ecdh (hex->integer priv) (hex->octets peer)))))))

(deftest secp-rfc6979 ()
  ;; RustCrypto k256 0.13.4 src/ecdsa.rs, ethereum_end_to_end_example.
  (let* ((priv #x4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318)
         (hash (clempo.keccak:keccak256
                (hex->octets "e9808504e3b29200831e848094f0109fc8df283027b6285cc889f5aa624eac1f55843b9aca0080018080"))))
    (multiple-value-bind (r s v) (clempo.secp256k1:sign hash priv)
      (is= #xc9cf86333bcb065d140032ecaab5d9281bde80f21b9687b3e94161de42d518957 r)
      (is= #x27a108a0b8d101465414033c3f705a9c7b826e596766046ee1183dbc8aeaa68 s)
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
    (is (null (clempo.secp256k1:private->public 0)))
    (is (null (clempo.secp256k1:private->public n)))
    (is (null (clempo.secp256k1:ecdh 1 (make-octets 64))))))

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
  (is-equalp #() (clempo.modexp:modexp #() #() #()))
  (is-equalp (octets-of 0 0) (clempo.modexp:modexp (octets-of 7) #() (octets-of 0 0))))

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
