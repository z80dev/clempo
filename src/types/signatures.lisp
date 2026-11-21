(defpackage #:clempo.signatures
  (:use #:cl #:clempo.base)
  (:export #:recover-signature #:signature-kind #:signature-inner-kind
           #:key-authorization-signer #:authorization-signer #:signature-error
           #:parse-key-authorization #:key-authorization
           #:key-authorization-chain-id #:key-authorization-key-type
           #:key-authorization-key-id #:key-authorization-expiry
           #:key-authorization-limits #:key-authorization-allowed-calls
           #:key-authorization-witness #:key-authorization-is-admin
           #:key-authorization-account #:key-authorization-limit-count
           #:key-authorization-call-scope-count #:key-authorization-selector-count
           #:key-authorization-recipient-count
           #:key-authorization-limit #:key-authorization-limit-token
           #:key-authorization-limit-amount #:key-authorization-limit-period
           #:key-authorization-call-scope #:key-authorization-call-scope-target
           #:key-authorization-call-scope-selector-rules
           #:key-authorization-selector-rule #:key-authorization-selector-rule-selector
           #:key-authorization-selector-rule-recipients
           ;; T14 ConfigurableAccount signatures
           #:configurable-signature #:configurable-signature-p #:configurable-signature-account
           #:configurable-signature-salt #:configurable-signature-version
           #:configurable-signature-threshold #:configurable-signature-owners
           #:configurable-signature-signatures #:parse-configurable-signature
           #:configurable-commitment #:configurable-commitment-error
           #:configurable-account-address #:configurable-derivation-gas
           #:configurable-verification-gas #:verify-configurable-approvals
           #:configurable-config-error #:make-configurable-signature
           #:encode-configurable-signature #:signature-error-reason #:key-authorization-hash
           #:canonical-signature))

(in-package #:clempo.signatures)

(define-condition signature-error (error)
  ((reason :initarg :reason :reader signature-error-reason))
  (:report (lambda (c s) (format s "Invalid signature: ~A" (signature-error-reason c)))))

(defun reject-signature (reason)
  (error 'signature-error :reason reason))

(defun signature-shape (signature start)
  "Return outer and primitive kinds without copying a keychain's inner payload. A T14
ConfigurableAccount signature (0x05 || RLP) is fully decoded and shape-checked; its kinds
are :CONFIGURABLE-ACCOUNT."
  (let ((n (- (length signature) start)))
    (cond ((= n 65) (values :secp256k1 :secp256k1))
          ((< n 2) (reject-signature "missing signature payload"))
          (t (case (aref signature start)
               (1 (unless (= n 130) (reject-signature "P256 length"))
                  (values :p256 :p256))
               (2 (unless (<= 129 n 2049) (reject-signature "WebAuthn length"))
                  (values :webauthn :webauthn))
               ((3 4)
                (unless (> n 21) (reject-signature "keychain length"))
                (unless (zerop start) (reject-signature "nested keychain"))
                (multiple-value-bind (outer inner) (signature-shape signature (+ start 21))
                  (declare (ignore outer))
                  (when (and (= (aref signature start) 3) (eq inner :configurable-account))
                    (reject-signature "configurable account access keys require keychain V2"))
                  (values (if (= (aref signature start) 3) :keychain-v1 :keychain-v2)
                          inner)))
               (5 (parse-configurable-signature signature (1+ start))
                  (values :configurable-account :configurable-account))
               (otherwise (reject-signature "unknown signature type")))))))

(defun signature-kind (signature)
  "Validate the wire shape and identify the outer scheme. 65 bytes always means secp256k1."
  (unless (typep signature 'octets) (reject-signature "expected octets"))
  (nth-value 0 (signature-shape signature 0)))

(defun signature-inner-kind (signature)
  "Validate the same wire shape as SIGNATURE-KIND and return its primitive scheme."
  (unless (typep signature 'octets) (reject-signature "expected octets"))
  (nth-value 1 (signature-shape signature 0)))

(defun base64url-hash (hash)
  (let ((out (make-string 43))
        (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
    (dotimes (i 43 out)
      (let* ((bit (* i 6)) (byte (floor bit 8)) (shift (mod bit 8))
             (word (ash (aref hash byte) 8)))
        (when (< (1+ byte) 32) (setf word (logior word (aref hash (1+ byte)))))
        (setf (char out i) (char alphabet (logand 63 (ash word (- shift 10)))))))))

(defun webauthn-client-data (data)
  "Strict JSON validation; known fields, like Rust's borrowed strings, may not be escaped."
  (let* ((text (handler-case (octets->string data)
                 (error () (reject-signature "invalid clientData UTF-8"))))
         (n (length text)) (p 0) (kind nil) (challenge nil))
    (labels ((bad () (reject-signature "invalid clientData JSON"))
             (peek () (and (< p n) (char text p)))
             (space () (loop while (and (peek) (find (peek) '(#\Space #\Tab #\Newline #\Return))) do (incf p)))
             (take (c) (unless (eql (peek) c) (bad)) (incf p))
             (hex4 ()
               (let ((code 0))
                 (dotimes (i 4 code)
                   (let ((digit (and (peek) (digit-char-p (peek) 16))))
                     (unless digit (bad))
                     (setf code (+ (* code 16) digit))
                     (incf p)))))
             (str ()
               (let ((start p) (escaped nil))
                 (take #\")
                 (loop
                   (let ((c (peek)))
                     (unless c (bad))
                     (incf p)
                     (cond ((char= c #\") (return))
                           ((< (char-code c) 32) (bad))
                           ((char= c #\\)
                            (setf escaped t)
                            (let ((e (peek)))
                              (unless e (bad)) (incf p)
                              (cond ((char= e #\u)
                                     (let ((code (hex4)))
                                       (cond ((<= #xd800 code #xdbff)
                                              (take #\\) (take #\u)
                                              (unless (<= #xdc00 (hex4) #xdfff) (bad)))
                                             ((<= #xdc00 code #xdfff) (bad)))))
                                    ((not (find e "\"\\/bfnrt")) (bad))))))))
                 (values (clempo.json:parse-json (subseq text start p)) escaped)))
             (number-value ()
               (when (eql (peek) #\-) (incf p))
               (cond ((eql (peek) #\0) (incf p))
                     ((and (peek) (find (peek) "123456789"))
                      (loop while (and (peek) (digit-char-p (peek))) do (incf p)))
                     (t (bad)))
               (when (eql (peek) #\.)
                 (incf p)
                 (unless (and (peek) (digit-char-p (peek))) (bad))
                 (loop while (and (peek) (digit-char-p (peek))) do (incf p)))
               (when (and (peek) (find (peek) "eE"))
                 (incf p)
                 (when (and (peek) (find (peek) "+-")) (incf p))
                 (unless (and (peek) (digit-char-p (peek))) (bad))
                 (loop while (and (peek) (digit-char-p (peek))) do (incf p))))
             (literal (s)
               (unless (and (<= (+ p (length s)) n)
                            (string= s text :start2 p :end2 (+ p (length s)))) (bad))
               (incf p (length s)))
             (array-value (depth)
               (take #\[) (space)
               (unless (eql (peek) #\])
                 (loop (value depth) (space)
                       (unless (eql (peek) #\,) (return))
                       (incf p) (space)))
               (take #\]))
             (object-value (depth root)
               (take #\{) (space)
               (unless (eql (peek) #\})
                 (loop
                   (let ((key (str)))
                     (space) (take #\:) (space)
                     (if (and root (member key '("type" "challenge") :test #'string=))
                         (multiple-value-bind (v escaped) (str)
                           (when escaped (bad))
                           (if (string= key "type")
                               (progn (when kind (bad)) (setf kind v))
                               (progn (when challenge (bad)) (setf challenge v))))
                         (value depth)))
                   (space)
                   (unless (eql (peek) #\,) (return))
                   (incf p) (space)))
               (take #\}))
             (value (depth)
               (when (> depth 128) (bad))
               (space)
               (case (peek)
                 (#\{ (object-value (1+ depth) nil))
                 (#\[ (array-value (1+ depth)))
                 (#\" (str))
                 (#\t (literal "true")) (#\f (literal "false")) (#\n (literal "null"))
                 (otherwise (number-value)))))
      (space) (object-value 1 t) (space)
      (unless (= p n) (bad))
      (unless (and (equal kind "webauthn.get") challenge) (bad))
      challenge)))

(defun webauthn-hash (signature hash field-start)
  (let ((data-length (1- field-start)))
    (unless (>= data-length 69) (reject-signature "WebAuthn data too short"))
    (let ((flags (aref signature 33)))
      (unless (and (not (zerop (logand flags 5))) (zerop (logand flags #xc0)))
        (reject-signature "WebAuthn assertion flags")))
    (let ((json (subseq signature 38 field-start)))
      (unless (string= (webauthn-client-data json) (base64url-hash hash))
        (reject-signature "WebAuthn challenge mismatch"))
      (clempo.sha2:sha256
       (octets-concat (subseq signature 1 38) (clempo.sha2:sha256 json))))))

(defun secp256k1-parity (v)
  "alloy normalize_v: 0/27 even, 1/28 odd, EIP-155 values from 35."
  (case v ((0 1) v) ((27 28) (- v 27))
        (otherwise (if (>= v 35) (mod (- v 35) 2) (reject-signature "invalid recovery parity")))))

(defun recover-primitive (signature hash kind)
  (if (eq kind :secp256k1)
      (let* ((r (octets->integer signature :end 32))
             (s (octets->integer signature :start 32 :end 64))
             (recid (secp256k1-parity (aref signature 64))))
        (unless (<= 1 s #x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a0)
          (reject-signature "secp256k1 high or zero s"))
        (or (clempo.secp256k1:ecrecover-address hash r s recid)
            (reject-signature "secp256k1 recovery failed")))
      (let* ((start (if (eq kind :p256) 1 (- (length signature) 128)))
             (r (octets->integer signature :start start :end (+ start 32)))
             (s (octets->integer signature :start (+ start 32) :end (+ start 64)))
             (x (octets->integer signature :start (+ start 64) :end (+ start 96)))
             (y (octets->integer signature :start (+ start 96) :end (+ start 128)))
             (message (if (eq kind :webauthn)
                          (webauthn-hash signature hash start)
                          (if (zerop (aref signature 129)) hash (clempo.sha2:sha256 hash)))))
        (unless (clempo.p256:p256-verify-low-s message r s x y)
          (reject-signature "P256 verification failed"))
        (octets->integer (clempo.keccak:keccak256 (subseq signature (+ start 64) (+ start 128)))
                         :start 12))))

;;; ConfigurableAccount signatures (T14, TIP-1107; tempo_primitives
;;; transaction/configurable_account): 0x05 || rlp([account, [salt, version, threshold,
;;; [[owner, weight] ...]], [owner-signature ...]]). The owners' primitive signatures approve
;;; keccak(domain || inner digest || account || version) with weights reaching the threshold.

(defconstant +max-configurable-owners+ 48)
(defconstant +max-configurable-signatures+ 8)
(defconstant +max-owner-signature-bytes+ 2049 "1 + MAX_WEBAUTHN_SIGNATURE_LENGTH")
(define-constant +configurable-account-domain+ (string->octets "tempo:configurable-account:account"))
(define-constant +configurable-config-domain+ (string->octets "tempo:configurable-account:config"))
(define-constant +configurable-signature-domain+
  (string->octets "tempo:configurable-account:signature"))

(defstruct (configurable-signature (:constructor %make-configurable-signature))
  "A decoded ConfigurableAccountSignature. OWNERS are ((address weight) ...), strictly
ascending; SIGNATURES the owners' primitive signature octets, in approval order."
  (account 0 :read-only t) (salt 0 :read-only t) (version 0 :read-only t)
  (threshold 0 :read-only t) (owners nil :read-only t) (signatures nil :read-only t))

(defun configurable-config-error (account version threshold owners)
  "ConfigurableAccountConfig::validate_for_account: NIL, or the error keyword."
  (cond ((null owners) :empty-owners)
        ((> (length owners) +max-configurable-owners+) :too-many-owners)
        ((zerop threshold) :zero-threshold)
        ((loop for (owner) in owners
               thereis (cond ((zerop owner) :zero-owner)
                             ((and (zerop version) account (= account owner)) :account-is-owner))))
        ((some (lambda (o) (zerop (second o))) owners) :zero-weight)
        ((loop for (a b) on owners while b thereis (>= (first a) (first b)))
         (if (/= (length (remove-duplicates owners :key #'first)) (length owners))
             :duplicate-owner
             :owners-not-ascending))
        ((> (reduce #'+ owners :key #'second) 255) :total-weight-exceeds-max)
        ((> threshold (reduce #'+ (subseq (sort (mapcar #'second owners) #'>) 0
                                          (min +max-configurable-signatures+ (length owners)))))
         :threshold-exceeds-weight)))

(defun signature-shape-primitive (signature)
  "The kind of a PrimitiveSignature's octets, or NIL when they are not one (a secp256k1
parity outside alloy's normalize_v does not decode)."
  (let ((n (length signature)))
    (cond ((= n 65) (let ((v (aref signature 64)))
                      (and (or (<= v 1) (= v 27) (= v 28) (>= v 35)) :secp256k1)))
          ((< n 2) nil)
          ((= (aref signature 0) 1) (and (= n 130) :p256))
          ((= (aref signature 0) 2) (and (<= 129 n 2049) :webauthn)))))

(defun parse-configurable-signature (data start)
  "decode_exact::<ConfigurableAccountSignature>(DATA[START..]) with try_new's shape checks."
  (flet ((bad () (reject-signature "Invalid configurable account signature RLP"))
         (fixed (x n) (and (typep x 'octets) (= (length x) n))))
    (let ((item (handler-case (clempo.rlp:rlp-decode data :start start)
                  (clempo.rlp:rlp-error () (bad)))))
      (unless (and (listp item) (= (length item) 3)) (bad))
      (destructuring-bind (account config signatures) item
        (unless (and (fixed account 20) (listp config) (= (length config) 4) (listp signatures))
          (bad))
        (destructuring-bind (salt version threshold owners) config
          (unless (and (fixed salt 32) (listp owners)) (bad))
          (flet ((int (x bytes) (handler-case (clempo.rlp:rlp-decode-integer x :max-bytes bytes)
                                  (clempo.rlp:rlp-error () (bad)))))
            (let ((account (octets->integer account))
                  (version (int version 8))
                  (threshold (int threshold 1))
                  (owners (mapcar (lambda (o)
                                    (unless (and (listp o) (= (length o) 2) (fixed (first o) 20))
                                      (bad))
                                    (list (octets->integer (first o)) (int (second o) 1)))
                                  owners)))
              (dolist (s signatures)
                ;; PrimitiveSignature::from_bytes: no keychain or configurable owner approvals
                (unless (and (typep s 'octets) (signature-shape-primitive s)) (bad)))
              (when (or (zerop account)
                        (configurable-config-error account version threshold owners)
                        (null signatures)
                        (> (length signatures) +max-configurable-signatures+)
                        (some (lambda (s) (> (length s) +max-owner-signature-bytes+)) signatures))
                (reject-signature "invalid configurable account signature"))
              (%make-configurable-signature :account account :salt (octets->integer salt)
                                            :version version :threshold threshold
                                            :owners owners :signatures signatures))))))))

(defun configurable-signature (signature)
  "The CONFIGURABLE-SIGNATURE a transaction signature names: its own (0x05) or the delegate
of a V2 keychain wrapping one; NIL for every other signature."
  (unless (typep signature 'octets) (reject-signature "expected octets"))
  (let ((n (length signature)))
    (cond ((or (= n 65) (< n 2)) nil)
          ((= (aref signature 0) 5) (parse-configurable-signature signature 1))
          ((and (= (aref signature 0) 4) (> n 22) (/= (- n 21) 65) (= (aref signature 21) 5))
           (parse-configurable-signature signature 22)))))

(defun make-configurable-signature (&key (account 0) (salt 0) (version 0) threshold owners
                                         signatures)
  "A CONFIGURABLE-SIGNATURE from its parts (OWNERS ((address weight) ...), SIGNATURES the
owners' primitive signature octets), unchecked: ENCODE-CONFIGURABLE-SIGNATURE and parse it to
validate."
  (%make-configurable-signature :account account :salt salt :version version
                                :threshold threshold :owners owners :signatures signatures))

(defun encode-configurable-signature (signature)
  "The wire octets 0x05 || rlp([account, [salt, version, threshold, owners], signatures])."
  (octets-concat
   (octets-of 5)
   (clempo.rlp:rlp-encode
    (list (integer->octets (configurable-signature-account signature) 20)
          (list (integer->octets (configurable-signature-salt signature) 32)
                (configurable-signature-version signature)
                (configurable-signature-threshold signature)
                (mapcar (lambda (o) (list (integer->octets (first o) 20) (second o)))
                        (configurable-signature-owners signature)))
          (configurable-signature-signatures signature)))))

(defun configurable-owner-set-octets (threshold owners)
  (apply #'octets-concat (octets-of threshold (length owners))
         (loop for (owner weight) in owners
               collect (integer->octets owner 20) collect (octets-of weight))))

(defun configurable-account-address (signature)
  "The account a version-zero configuration derives: keccak(account domain || salt ||
owner set), last 20 bytes."
  (octets->integer
   (clempo.keccak:keccak256
    (octets-concat +configurable-account-domain+
                   (integer->octets (configurable-signature-salt signature) 32)
                   (configurable-owner-set-octets (configurable-signature-threshold signature)
                                                  (configurable-signature-owners signature))))
   :start 12))

(defun configurable-commitment-preimage-length (signature)
  (+ (length +configurable-config-domain+) 32 8 2
     (* 21 (length (configurable-signature-owners signature)))))

(defun configurable-account-preimage-length (signature)
  (+ (length +configurable-account-domain+) 32 2
     (* 21 (length (configurable-signature-owners signature)))))

(defun configurable-commitment (signature)
  "config_commitment: keccak(config domain || salt || version || owner set) as an integer."
  (octets->integer
   (clempo.keccak:keccak256
    (octets-concat +configurable-config-domain+
                   (integer->octets (configurable-signature-salt signature) 32)
                   (integer->octets (configurable-signature-version signature) 8)
                   (configurable-owner-set-octets (configurable-signature-threshold signature)
                                                  (configurable-signature-owners signature))))))

(defun configurable-commitment-error (signature stored)
  "validate_account_commitment against the STORED commitment (0: unregistered): NIL, or
:commitment-mismatch, :unregistered-version or :initial-account-mismatch."
  (let ((actual (configurable-commitment signature)))
    (cond ((or (zerop actual) (and (/= stored 0) (/= stored actual))) :commitment-mismatch)
          ((/= stored 0) nil)
          ((/= (configurable-signature-version signature) 0) :unregistered-version)
          ((/= (configurable-account-address signature) (configurable-signature-account signature))
           :initial-account-mismatch))))

(defun keccak-word-gas (length) (+ 30 (* 6 (ceiling length 32))))

(defun configurable-derivation-gas (signature)
  "ConfigurableAccountConfig::derivation_gas: keccak of the account preimage."
  (keccak-word-gas (configurable-account-preimage-length signature)))

(defun primitive-verification-gas (signature)
  "PrimitiveSignature::verification_gas: 3000 secp256k1, 8000 P256, 8000 WebAuthn plus the
calldata cost of its assertion data."
  (ecase (signature-shape-primitive signature)
    (:secp256k1 3000)
    (:p256 8000)
    (:webauthn (+ 8000 (* 4 (loop for i from 1 below (- (length signature) 128)
                                  sum (if (zerop (aref signature i)) 1 4)))))))

(defun configurable-verification-gas (signature)
  "ConfigurableAccountSignature::verification_gas: every owner approval, the witness
(account and configuration RLP) as calldata, the commitment and signature-digest hashes."
  (let ((witness (octets-concat
                  (clempo.rlp:rlp-encode (integer->octets (configurable-signature-account signature) 20))
                  (clempo.rlp:rlp-encode
                   (list (integer->octets (configurable-signature-salt signature) 32)
                         (configurable-signature-version signature)
                         (configurable-signature-threshold signature)
                         (mapcar (lambda (o) (list (integer->octets (first o) 20) (second o)))
                                 (configurable-signature-owners signature)))))))
    (+ (reduce #'+ (configurable-signature-signatures signature) :key #'primitive-verification-gas)
       (* 4 (loop for b across witness sum (if (zerop b) 1 4)))
       (keccak-word-gas (configurable-commitment-preimage-length signature))
       (keccak-word-gas (+ (length +configurable-signature-domain+) 32 20 8)))))

(defun configurable-digest (signature inner-digest)
  "configurable_account_digest: what the owners sign for INNER-DIGEST (32 octets)."
  (clempo.keccak:keccak256
   (octets-concat +configurable-signature-domain+ inner-digest
                  (integer->octets (configurable-signature-account signature) 20)
                  (integer->octets (configurable-signature-version signature) 8))))

(defun verify-configurable-approvals (signature inner-digest)
  "verify_approvals with ConfigurableAccountWeightAccumulator: the owners' signatures over
INNER-DIGEST, recovered owners strictly ascending, reaching the threshold with the last
approval and without one to spare. Returns the account or signals SIGNATURE-ERROR."
  (let* ((digest (configurable-digest signature inner-digest))
         (threshold (configurable-signature-threshold signature))
         (owners (configurable-signature-owners signature))
         (count 0) (weight 0) (min-weight nil) (previous nil))
    (when (zerop threshold) (reject-signature "configurable account threshold cannot be zero"))
    (loop for (approval . more) on (configurable-signature-signatures signature)
          do (let* ((owner (handler-case
                               (recover-primitive approval digest (signature-shape-primitive approval))
                             (signature-error ()
                               (reject-signature "invalid configurable account owner signature"))))
                    (owner-weight (or (second (assoc owner owners))
                                      (reject-signature "configurable account signer is not an owner"))))
               (when (and (plusp count) (>= weight threshold))
                 (reject-signature "excess configurable account owner signatures"))
               (incf count)
               (when (> count +max-configurable-signatures+)
                 (reject-signature "too many configurable account signatures"))
               (when (and previous (>= previous owner))
                 (reject-signature "configurable account recovered owners must be strictly ascending"))
               (setf previous owner)
               (incf weight owner-weight)
               (setf min-weight (if min-weight (min min-weight owner-weight) owner-weight))
               (when (and (>= weight threshold) more)
                 (reject-signature "excess configurable account owner signatures"))))
    (cond ((zerop count) (reject-signature "configurable account signatures cannot be empty"))
          ((< weight threshold) (reject-signature "configurable account signature weight below threshold"))
          ((>= weight (+ threshold min-weight))
           (reject-signature "excess configurable account owner signatures")))
    (configurable-signature-account signature)))

(defun canonical-signature (signature)
  "SIGNATURE (a TempoSignature's or AccountSignature's octets) as the reference re-encodes it
after decoding: alloy's Signature::as_bytes writes a secp256k1 parity as 27/28, wherever the
signature sits (direct, an owner approval inside 0x05, a keychain's inner signature). Signing
hashes cover this re-encoding, not the received octets. Octets that do not decode are
returned unchanged (decoding rejects them elsewhere)."
  (handler-case
      (let ((n (length signature)))
        (cond ((= n 65)
               (let ((parity (secp256k1-parity (aref signature 64))))
                 (if (= (aref signature 64) (+ 27 parity))
                     signature
                     (let ((out (copy-seq signature)))
                       (setf (aref out 64) (+ 27 parity))
                       out))))
              ((< n 2) signature)
              ((= (aref signature 0) 5)
               (let ((c (parse-configurable-signature signature 1)))
                 (encode-configurable-signature
                  (%make-configurable-signature
                   :account (configurable-signature-account c) :salt (configurable-signature-salt c)
                   :version (configurable-signature-version c)
                   :threshold (configurable-signature-threshold c)
                   :owners (configurable-signature-owners c)
                   :signatures (mapcar #'canonical-signature (configurable-signature-signatures c))))))
              ((and (member (aref signature 0) '(3 4)) (> n 21))
               (octets-concat (subseq signature 0 21) (canonical-signature (subseq signature 21))))
              (t signature)))
    (signature-error () signature)))

(defun recover-account (signature start hash)
  "AccountSignature::recover_account of SIGNATURE[START..] over HASH (32 octets)."
  (if (and (/= (- (length signature) start) 65) (> (- (length signature) start) 1)
           (= (aref signature start) 5))
      (verify-configurable-approvals (parse-configurable-signature signature (1+ start)) hash)
      (let ((inner (if (zerop start) signature (subseq signature start))))
        (recover-primitive inner hash (nth-value 1 (signature-shape inner 0))))))

(defun recover-signature (signature hash)
  "Return sender and, for keychain envelopes, verified access-key ID. A ConfigurableAccount
signature (or a V2 keychain delegate wrapping one) recovers the account once its owner
quorum verifies. Keychain authorization, commitments in on-chain state and fork-version
gating remain the caller's responsibility."
  (unless (and (typep hash 'octets) (= (length hash) 32))
    (reject-signature "expected 32-byte signing hash"))
  (let ((kind (signature-kind signature)))
    (if (member kind '(:keychain-v1 :keychain-v2))
        (let* ((account (octets->integer signature :start 1 :end 21))
               (effective (if (eq kind :keychain-v2)
                              (clempo.keccak:keccak256
                               (octets-concat (octets-of 4) hash (subseq signature 1 21))) hash)))
          (values account (recover-account signature 21 effective)))
        (values (recover-account signature 0 hash) nil))))

(defun wire-integer (value bytes)
  (if (integerp value)
      (progn (unless (typep value `(unsigned-byte ,(* bytes 8)))
               (reject-signature "integer out of range")) value)
      (clempo.rlp:rlp-decode-integer value :max-bytes bytes)))

(defun wire-address (value)
  (if (integerp value) (integer->octets (wire-integer value 20) 20)
      (progn (unless (and (typep value 'octets) (= (length value) 20))
               (reject-signature "invalid address")) value)))

(defun absent-option-p (value)
  (and (typep value 'octets) (zerop (length value))))

(defun trim-absent-options (fields required)
  (let ((last-present (loop for x in fields for i from 0
                            unless (and (>= i required) (absent-option-p x))
                              maximize (1+ i))))
    (subseq fields 0 (max required (or last-present 0)))))

(defun key-authorization-fields (authorization)
  (labels ((shape (x low high)
             (unless (and (listp x) (<= low (length x) high))
               (reject-signature "key authorization field shape")))
           (fixed (x bytes)
             (unless (and (typep x 'octets) (= (length x) bytes))
               (reject-signature "key authorization fixed bytes")) x)
           (limit (x)
             (shape x 2 3)
             (let ((out (list (wire-address (first x)) (wire-integer (second x) 32))))
               (when (and (third x) (not (absent-option-p (third x))))
                 (let ((period (wire-integer (third x) 8)))
                   (unless (plusp period) (reject-signature "zero token limit period"))
                   (setf out (append out (list period)))))
               out))
           (rule (x)
             (shape x 2 2)
             (shape (second x) 0 most-positive-fixnum)
             (list (fixed (first x) 4) (mapcar #'wire-address (second x))))
           (scope (x)
             (shape x 2 2)
             (shape (second x) 0 most-positive-fixnum)
             (list (wire-address (first x)) (mapcar #'rule (second x)))))
    (shape authorization 3 9)
    (let ((fields
            (loop for value in authorization for i from 0 collect
              (case i
                (0 (wire-integer value 8))
                (1 (let ((kind (wire-integer value 1)))
                     ;; 3: a T14 configurable account granted as an access key
                     (unless (<= kind 3) (reject-signature "invalid key type")) kind))
                (2 (wire-address value))
                (3 (if (absent-option-p value) value
                       (let ((expiry (wire-integer value 8)))
                         (unless (plusp expiry) (reject-signature "zero expiry")) expiry)))
                ((4 5)
                 (if (absent-option-p value) value
                     (progn (shape value 0 most-positive-fixnum)
                            (mapcar (if (= i 4) #'limit #'scope) value))))
                (6 (if (absent-option-p value) value (fixed value 32)))
                (7 (if (absent-option-p value) value
                       (progn (unless (= 1 (wire-integer value 8))
                                (reject-signature "invalid admin marker")) 1)))
                (8 (if (absent-option-p value) value (wire-address value)))))))
      (trim-absent-options fields 3))))

;;; Parsed executor view. All scalar wire values, including selectors, witnesses
;;; and addresses, are integers. Read-only slots and borrowed read-only lists
;;; avoid accessor allocation; do not mutate lists returned by the parsed view.
(defstruct (key-authorization-limit (:constructor %make-key-authorization-limit))
  (token 0 :read-only t) (amount 0 :read-only t) (period 0 :read-only t))

(defstruct (key-authorization-selector-rule
             (:constructor %make-key-authorization-selector-rule)
             (:conc-name %key-authorization-selector-rule-))
  (selector 0 :read-only t) (recipients nil :read-only t))

(defun key-authorization-selector-rule-selector (rule)
  (%key-authorization-selector-rule-selector rule))

(defun key-authorization-selector-rule-recipients (rule)
  (%key-authorization-selector-rule-recipients rule))

(defstruct (key-authorization-call-scope
             (:constructor %make-key-authorization-call-scope)
             (:conc-name %key-authorization-call-scope-))
  (target 0 :read-only t) (selector-rules nil :read-only t))

(defun key-authorization-call-scope-target (scope)
  (%key-authorization-call-scope-target scope))

(defun key-authorization-call-scope-selector-rules (scope)
  (%key-authorization-call-scope-selector-rules scope))

(defstruct (key-authorization (:constructor %make-key-authorization)
                              (:conc-name %key-authorization-))
  (chain-id 0 :read-only t) (key-type 0 :read-only t) (key-id 0 :read-only t)
  (expiry nil :read-only t) (limits nil :read-only t)
  (allowed-calls nil :read-only t) (witness nil :read-only t)
  (is-admin nil :read-only t) (account nil :read-only t)
  (limit-count 0 :read-only t) (call-scope-count 0 :read-only t)
  (selector-count 0 :read-only t) (recipient-count 0 :read-only t))

(macrolet ((readers (&rest slots)
             `(progn ,@(loop for slot in slots
                             for public = (intern (format nil "KEY-AUTHORIZATION-~A" slot))
                             for private = (intern (format nil "%KEY-AUTHORIZATION-~A" slot))
                             collect `(defun ,public (view) (,private view))))))
  (readers chain-id key-type key-id expiry witness is-admin account
           limit-count call-scope-count selector-count recipient-count))

(defun key-authorization-limits (view)
  (%key-authorization-limits view))

(defun key-authorization-allowed-calls (view)
  (%key-authorization-allowed-calls view))

(defun parse-key-authorization (key-auth)
  "Parse decoded SignedKeyAuthorization (authorization signature), without recovery.
Uses the signing normalization and rejects keychain approval signatures.
KEY-TYPE is 0/1/2 (secp256k1/P256/WebAuthn). EXPIRY, WITNESS and
ACCOUNT are NIL when absent; integer zero is a present witness/account.
LIMITS and ALLOWED-CALLS are :ABSENT when unrestricted, NIL when explicitly
empty, otherwise borrowed read-only lists of immutable records. A limit PERIOD
of zero means one-time. Empty selector rules allow any selector; empty recipients
allow any recipient. Do not mutate returned lists. IS-ADMIN is a boolean.
Counts are zero for absent lists; reading them or lists does not allocate.
This validates shape only, not signatures, chain/fork policy or state."
  (unless (and (listp key-auth) (= (length key-auth) 2))
    (reject-signature "key authorization shape"))
  (let ((fields (key-authorization-fields (first key-auth)))
        (selector-count 0) (recipient-count 0))
    (when (member (signature-kind (second key-auth)) '(:keychain-v1 :keychain-v2))
      (reject-signature "keychain cannot authorize a key"))
    (labels ((option (i)
               (let ((value (nth i fields)))
                 (if (or (>= i (length fields)) (absent-option-p value))
                     :absent value)))
             (scalar (i)
               (let ((value (option i))) (unless (eq value :absent) value)))
             (address (value) (octets->integer value)))
      (let* ((limits (option 4))
             (calls (option 5))
             (parsed-limits
               (if (eq limits :absent) :absent
                   (mapcar (lambda (limit)
                             (%make-key-authorization-limit
                              :token (address (first limit)) :amount (second limit)
                              :period (or (third limit) 0))) limits)))
             (parsed-calls
               (if (eq calls :absent) :absent
                   (mapcar
                    (lambda (scope)
                      (%make-key-authorization-call-scope
                       :target (address (first scope))
                       :selector-rules
                       (mapcar (lambda (rule)
                                 (incf selector-count)
                                 (incf recipient-count (length (second rule)))
                                 (%make-key-authorization-selector-rule
                                  :selector (octets->integer (first rule))
                                  :recipients (mapcar #'address (second rule))))
                               (second scope))))
                    calls))))
        (%make-key-authorization
         :chain-id (first fields) :key-type (second fields)
         :key-id (address (third fields)) :expiry (scalar 3)
         :limits parsed-limits :allowed-calls parsed-calls
         :witness (let ((value (scalar 6))) (and value (octets->integer value)))
         :is-admin (not (null (scalar 7)))
         :account (let ((value (scalar 8))) (and value (address value)))
         :limit-count (if (eq limits :absent) 0 (length limits))
         :call-scope-count (if (eq calls :absent) 0 (length calls))
         :selector-count selector-count :recipient-count recipient-count)))))

(defun key-authorization-hash (key-auth)
  "The signature hash of a SignedKeyAuthorization's authorization: keccak(rlp(fields))."
  (clempo.keccak:keccak256 (clempo.rlp:rlp-encode (key-authorization-fields (first key-auth)))))

(defun key-authorization-signer (key-auth)
  "SignedKeyAuthorization::authorizing_account of (authorization signature): the recovered
primitive signer, or the account a ConfigurableAccount signature names. The configurable
owner quorum is not checked here (verify it with VERIFY-CONFIGURABLE-APPROVALS over
KEY-AUTHORIZATION-HASH), nor the account's commitment and code."
  (unless (and (listp key-auth) (= (length key-auth) 2))
    (reject-signature "key authorization shape"))
  (destructuring-bind (authorization signature) key-auth
    (key-authorization-fields authorization)
    (case (signature-kind signature)
      ((:keychain-v1 :keychain-v2) (reject-signature "keychain cannot authorize a key"))
      (:configurable-account
       (configurable-signature-account (configurable-signature signature)))
      (t (recover-signature signature (key-authorization-hash key-auth))))))

(defun authorization-signer (auth)
  "Recover a Tempo four-field or EIP-7702 six-field decoded authorization."
  (unless (and (listp auth) (member (length auth) '(4 6)))
    (reject-signature "authorization shape"))
  (let* ((fields (list (wire-integer (first auth) 32) (wire-address (second auth))
                       (wire-integer (third auth) 8)))
         (hash (clempo.keccak:keccak256
                (octets-concat (octets-of 5) (clempo.rlp:rlp-encode fields))))
         (signature (if (= (length auth) 4) (fourth auth)
                        (let ((v (wire-integer (fourth auth) 1)))
                          (unless (<= v 1) (reject-signature "authorization parity"))
                          (octets-concat (integer->octets (wire-integer (fifth auth) 32) 32)
                                         (integer->octets (wire-integer (sixth auth) 32) 32)
                                         (octets-of v))))))
    (recover-signature signature hash)))
