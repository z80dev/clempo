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
           #:key-authorization-selector-rule-recipients))

(in-package #:clempo.signatures)

(define-condition signature-error (error)
  ((reason :initarg :reason :reader signature-error-reason))
  (:report (lambda (c s) (format s "Invalid signature: ~A" (signature-error-reason c)))))

(defun reject-signature (reason)
  (error 'signature-error :reason reason))

(defun signature-shape (signature start)
  "Return outer and primitive kinds without copying a keychain's inner payload."
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
                  (values (if (= (aref signature start) 3) :keychain-v1 :keychain-v2)
                          inner)))
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

(defun recover-primitive (signature hash kind)
  (if (eq kind :secp256k1)
      (let* ((r (octets->integer signature :end 32))
             (s (octets->integer signature :start 32 :end 64))
             (v (aref signature 64))
             (recid (case v ((0 1) v) ((27 28) (- v 27))
                          (otherwise (reject-signature "invalid recovery parity")))))
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

(defun recover-signature (signature hash)
  "Return sender and, for keychain envelopes, verified access-key ID.
Keychain authorization in on-chain state and fork-version gating remain the caller's responsibility."
  (unless (and (typep hash 'octets) (= (length hash) 32))
    (reject-signature "expected 32-byte signing hash"))
  (let ((kind (signature-kind signature)))
    (if (member kind '(:keychain-v1 :keychain-v2))
        (let* ((account (octets->integer signature :start 1 :end 21))
               (inner (subseq signature 21))
               (effective (if (eq kind :keychain-v2)
                              (clempo.keccak:keccak256
                               (octets-concat (octets-of 4) hash (subseq signature 1 21))) hash)))
          (values account (recover-primitive inner effective (signature-kind inner))))
        (values (recover-primitive signature hash kind) nil))))

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
                     (unless (<= kind 2) (reject-signature "invalid key type")) kind))
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

(defun key-authorization-signer (key-auth)
  "Recover a SignedKeyAuthorization decoded as (authorization signature)."
  (unless (and (listp key-auth) (= (length key-auth) 2))
    (reject-signature "key authorization shape"))
  (destructuring-bind (authorization signature) key-auth
    (let ((fields (key-authorization-fields authorization)))
      (when (member (signature-kind signature) '(:keychain-v1 :keychain-v2))
        (reject-signature "keychain cannot authorize a key"))
      (recover-signature signature (clempo.keccak:keccak256 (clempo.rlp:rlp-encode fields))))))

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
