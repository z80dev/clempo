(in-package #:clempo.signature-verifier)

;;; SignatureVerifier (crates/precompiles/src/signature_verifier, T3): recover and verify
;;; Tempo signatures (tempo_primitives::transaction::tt_signature): secp256k1 (65 bytes),
;;; P-256 (0x01), WebAuthn (0x02), keychain envelopes (0x03 V1, 0x04 V2) and T14
;;; configurable account signatures (0x05 + RLP).

(defcontract signature-verifier (:address +signature-verifier-address+))

(deferror invalid-format "InvalidFormat()")
(deferror invalid-signature "InvalidSignature()")

(defconstant +self+ +signature-verifier-address+)
(defconstant +max-webauthn-signature-length+ 2048)
(defconstant +secp256k1-verify-gas+ 3000)
(defconstant +p256-verify-gas+ 8000)
(defconstant +webauthn-verify-gas+ 8000)
(define-constant +secp256k1-half-n+ (ash clempo.secp256k1:+secp256k1-n+ -1))
(defconstant +max-calldata-length+
  (+ 4 (* 32 4) (* 32 (ceiling (1+ +max-webauthn-signature-length+) 32))))

(define-condition malformed-signature (error) ()
  (:documentation "TempoSignature::from_bytes failed."))

(defun malformed () (error 'malformed-signature))

;;; Primitive signatures: (kind . octets), the octets being the full encoding.

(defun secp-v-valid-p (v) (or (<= v 1) (= v 27) (= v 28) (>= v 35)))

(defun parse-primitive (data)
  "PrimitiveSignature::from_bytes: (kind . data) or MALFORMED."
  (let ((n (length data)))
    (cond ((zerop n) (malformed))
          ((= n 65) (unless (secp-v-valid-p (aref data 64)) (malformed)) (cons :secp256k1 data))
          ((< n 2) (malformed))
          (t (case (aref data 0)
               (1 (unless (= n 130) (malformed)) (cons :p256 data))
               (2 (unless (<= 128 (1- n) +max-webauthn-signature-length+) (malformed))
                (cons :webauthn data))
               (t (malformed)))))))

(defun primitive-type (sig)
  "SignatureType: 0 secp256k1, 1 P256, 2 WebAuthn."
  (ecase (car sig) (:secp256k1 0) (:p256 1) (:webauthn 2)))

(defun tokens-in-calldata (data &key (start 0) (end (length data)))
  (loop for i from start below end sum (if (zerop (aref data i)) 1 4)))

(defun primitive-base-gas (sig)
  (ecase (car sig)
    (:secp256k1 +secp256k1-verify-gas+) (:p256 +p256-verify-gas+) (:webauthn +webauthn-verify-gas+)))

(defun primitive-verification-gas (sig)
  (if (eq (car sig) :webauthn)
      (let ((data (cdr sig)))
        (+ +webauthn-verify-gas+ (* 4 (tokens-in-calldata data :start 1 :end (- (length data) 128)))))
      (primitive-base-gas sig)))

(defun recover-primitive-signer (sig hash)
  "PrimitiveSignature::recover_signer over the integer HASH: the address, or NIL."
  (let ((data (cdr sig)))
    (if (eq (car sig) :secp256k1)
        (let* ((r (octets->integer data :end 32))
               (s (octets->integer data :start 32 :end 64))
               (v (aref data 64))
               (parity (cond ((<= v 1) v) ((< v 35) (- v 27)) (t (mod (- v 35) 2)))))
          (when (and (< 0 r clempo.secp256k1:+secp256k1-n+) (< 0 s) (<= s +secp256k1-half-n+))
            (let ((key (ignore-errors
                        (clempo.secp256k1:ecrecover (integer->octets hash 32) r s parity))))
              (and key (clempo.secp256k1:pubkey->address key)))))
        (handler-case (values (clempo.signatures:recover-signature data (integer->octets hash 32)))
          (clempo.signatures:signature-error () nil)))))

;;; Configurable account signatures (configurable_account/mod.rs).

(defstruct (ca-signature (:constructor make-ca-signature (account config signatures)))
  account config signatures)

(defun rlp-address (item)
  (unless (and (typep item 'octets) (= (length item) 20)) (malformed))
  (octets->integer item))

(defun rlp-int (item bytes)
  (handler-case (clempo.rlp:rlp-decode-integer item :max-bytes bytes)
    (error () (malformed))))

(defun parse-configurable (data start)
  "decode_exact::<ConfigurableAccountSignature>(DATA[START..]) and its shape checks."
  (let ((item (handler-case (clempo.rlp:rlp-decode data :start start)
                (error () (malformed)))))
    (unless (and (listp item) (= (length item) 3)) (malformed))
    (destructuring-bind (account config signatures) item
      (unless (and (listp config) (= (length config) 4) (listp signatures)) (malformed))
      (destructuring-bind (salt version threshold owners) config
        (unless (and (typep salt 'octets) (= (length salt) 32) (listp owners)) (malformed))
        (let* ((account (rlp-address account))
               (config (clempo.configurable-account::make-config
                        (octets->integer salt) (rlp-int version 8) (rlp-int threshold 1)
                        (mapcar (lambda (o)
                                  (unless (and (listp o) (= (length o) 2)) (malformed))
                                  (list (rlp-address (first o)) (rlp-int (second o) 1)))
                                owners)))
               (signatures (mapcar (lambda (s)
                                     (unless (typep s 'octets) (malformed))
                                     (parse-primitive s))
                                   signatures)))
          (when (or (zerop account)
                    (clempo.configurable-account::config-error config account)
                    (null signatures)
                    (> (length signatures) clempo.configurable-account::+max-signatures+)
                    (some (lambda (s) (> (length (cdr s)) (1+ +max-webauthn-signature-length+)))
                          signatures))
            (malformed))
          (make-ca-signature account config signatures))))))

(defun minimal-octets (n)
  (if (zerop n) (make-octets 0) (integer->octets n (ceiling (integer-length n) 8))))

(defun witness-octets (sig)
  "alloy_rlp::encode(account) ++ config.encode()."
  (let ((config (ca-signature-config sig)))
    (octets-concat
     (clempo.rlp:rlp-encode (integer->octets (ca-signature-account sig) 20))
     (clempo.rlp:rlp-encode
      (list (integer->octets (clempo.configurable-account::config-salt config) 32)
            (minimal-octets (clempo.configurable-account::config-version config))
            (minimal-octets (clempo.configurable-account::config-threshold config))
            (mapcar (lambda (o) (list (integer->octets (first o) 20) (minimal-octets (second o))))
                    (clempo.configurable-account::config-owners config)))))))

(defun ca-verification-gas (sig)
  (let ((witness (witness-octets sig)))
    (+ (reduce #'+ (ca-signature-signatures sig) :key #'primitive-verification-gas)
       (* 4 (tokens-in-calldata witness))
       (keccak-gas (clempo.configurable-account::commitment-preimage-length (ca-signature-config sig)))
       (keccak-gas (+ (length clempo.configurable-account::+signature-domain+) 32 20 8)))))

(defun commitment-valid-p (sig stored)
  "validate_account_commitment."
  (let* ((config (ca-signature-config sig))
         (actual (clempo.configurable-account::config-commitment-of config)))
    (and (/= actual 0)
         (or (zerop stored) (= stored actual))
         (or (/= stored 0)
             (and (zerop (clempo.configurable-account::config-version config))
                  (= (clempo.configurable-account::config-account-address config)
                     (ca-signature-account sig)))))))

(defun approvals-valid-p (sig inner-digest)
  "verify_approvals and the weight accumulator."
  (let* ((config (ca-signature-config sig))
         (threshold (clempo.configurable-account::config-threshold config))
         (digest (clempo.configurable-account::configurable-account-digest
                  inner-digest (ca-signature-account sig)
                  (clempo.configurable-account::config-version config)))
         (signatures (ca-signature-signatures sig))
         (count 0) (weight 0) (min-weight nil) (previous nil))
    (when (zerop threshold) (return-from approvals-valid-p nil))
    (loop for (approval . more) on signatures
          do (let* ((owner (or (recover-primitive-signer approval digest)
                               (return-from approvals-valid-p nil)))
                    (owner-weight (or (clempo.configurable-account::config-owner-weight config owner)
                                      (return-from approvals-valid-p nil))))
               (when (and (plusp count) (>= weight threshold)) (return-from approvals-valid-p nil))
               (when (zerop owner-weight) (return-from approvals-valid-p nil))
               (incf count)
               (when (> count clempo.configurable-account::+max-signatures+)
                 (return-from approvals-valid-p nil))
               (when (and previous (>= previous owner)) (return-from approvals-valid-p nil))
               (setf previous owner)
               (incf weight owner-weight)
               (setf min-weight (if min-weight (min min-weight owner-weight) owner-weight))
               (when (and (>= weight threshold) more) (return-from approvals-valid-p nil))))
    (and (plusp count) (>= weight threshold)
         (not (and min-weight (>= weight (+ threshold min-weight)))))))

;;; TempoSignature::from_bytes: (:primitive sig), (:keychain account inner version) with
;;; INNER a primitive (kind . octets) or a CA-SIGNATURE, or (:configurable ca-signature).

(defun parse-account-signature (data start)
  (let ((n (- (length data) start)))
    (if (and (/= n 65) (>= n 2))
        (case (aref data start)
          ((3 4) (malformed))
          (5 (parse-configurable data (1+ start)))
          (t (parse-primitive (subseq data start))))
        (parse-primitive (subseq data start)))))

(defun parse-tempo-signature (data)
  (when (zerop (length data)) (malformed))
  (if (and (/= (length data) 65) (>= (length data) 2) (member (aref data 0) '(3 4)))
      (let ((version (if (= (aref data 0) 3) :v1 :v2)))
        (when (< (length data) 21) (malformed))
        (let ((inner (parse-account-signature data 21)))
          (when (and (eq version :v1) (ca-signature-p inner)) (malformed))
          (list :keychain (octets->integer data :start 1 :end 21) inner version)))
      (let ((sig (parse-account-signature data 0)))
        (if (ca-signature-p sig) (list :configurable sig) (list :primitive sig)))))

(defun parse-or-invalid-format (data)
  (handler-case (parse-tempo-signature data)
    (malformed-signature () (revert 'invalid-format))))

;;; The verifier.

(defun recover-checked (hash sig)
  "recover_primitive: charge the base gas, then recover or InvalidSignature."
  (deduct-gas (primitive-base-gas sig))
  (or (recover-primitive-signer sig hash) (revert 'invalid-signature)))

(defun recover (hash signature)
  "SignatureVerifier::recover: the signer of HASH (integer) under SIGNATURE (octets, a
primitive signature). Signals precompile-revert InvalidFormat / InvalidSignature."
  (let ((sig (handler-case (parse-primitive signature)
               (malformed-signature () (revert 'invalid-format)))))
    (recover-checked hash sig)))

(defun verify (signer hash signature)
  "SignatureVerifier verify(): whether SIGNER signed HASH (reverts like RECOVER)."
  (= (recover hash signature) signer))

(defun keychain-signing-hash (hash account)
  (clempo.keccak:keccak256-integer
   (octets-concat (octets-of 4) (integer->octets hash 32) (integer->octets account 20))))

(defun active-key (account key-id signature-type)
  (handler-case
      (clempo.keychain:validate-keychain-authorization
       account key-id (min (block-timestamp) (1- (ash 1 64)))
       (and signature-type (spec>= :t14) signature-type))
    (precompile-revert (c)
      ;; Err(e) if e.is_system_error(): a panic propagates.
      (if (eq (precompile-revert-name c) 'panic) (error c) nil))))

(defun recover-keychain-key (hash parsed)
  "(values account key-id signature-type) of a V2 keychain signature over a primitive."
  (unless (eq (first parsed) :keychain) (revert 'invalid-format))
  (destructuring-bind (account inner version) (rest parsed)
    (when (or (eq version :v1) (ca-signature-p inner)) (revert 'invalid-format))
    (values account (recover-checked (keychain-signing-hash hash account) inner)
            (primitive-type inner))))

(defun verify-configurable-signature (account hash sig)
  "The account's commitment when SIG verifies for ACCOUNT, else NIL."
  (let* ((info (account account))
         (commitment (if info (account-info-commitment info) 0))
         (code-hash (if info (account-info-code-hash info) +keccak-empty+)))
    (and (verify-configurable-signature-against
          account hash sig commitment
          (clempo.configurable-account:valid-account-code-p code-hash commitment))
         commitment)))

(defun verify-configurable-signature-against (account hash sig commitment code-valid)
  (deduct-gas (+ (ca-verification-gas sig)
                 (if (zerop commitment)
                     (clempo.configurable-account::derivation-gas (ca-signature-config sig))
                     0)))
  (when (or (/= (ca-signature-account sig) account)
            (not (clempo.configurable-account:valid-account-p account))
            (not code-valid)
            (not (commitment-valid-p sig commitment)))
    (return-from verify-configurable-signature-against nil))
  (unless (approvals-valid-p sig hash) (revert 'invalid-signature))
  t)

(defun verify-configurable-account (account hash signature)
  "SignatureVerifier::verify_configurable_account: whether SIGNATURE (octets) is a valid
configurable account signature of ACCOUNT over HASH against current state."
  (let ((parsed (parse-or-invalid-format signature)))
    (unless (eq (first parsed) :configurable) (revert 'invalid-format))
    (and (verify-configurable-signature account hash (second parsed)) t)))

(defun verify-keychain (account hash signature)
  (let ((parsed (parse-or-invalid-format signature)))
    (when (and (spec>= :t14) (eq (first parsed) :keychain) (ca-signature-p (third parsed)))
      (destructuring-bind (user delegate version) (rest parsed)
        (declare (ignore version))
        (when (/= user account) (return-from verify-keychain nil))
        (let ((commitment (verify-configurable-signature
                           (ca-signature-account delegate) (keychain-signing-hash hash account)
                           delegate)))
          (unless commitment (return-from verify-keychain nil))
          (let ((key (active-key account (ca-signature-account delegate) nil)))
            (return-from verify-keychain
              (and key
                   (clempo.keychain::accepts-signature-type-p
                    key 3 (lambda () (/= commitment 0)))))))))
    (multiple-value-bind (user key-id type) (recover-keychain-key hash parsed)
      (when (or (/= user account) (retired-root-p key-id)) (return-from verify-keychain nil))
      (and (active-key account key-id type) t))))

(defun verify-keychain-admin (account hash signature)
  (let ((parsed (parse-or-invalid-format signature)))
    (multiple-value-bind (user key-id type) (recover-keychain-key hash parsed)
      (when (or (/= user account) (retired-root-p key-id)) (return-from verify-keychain-admin nil))
      (when (= key-id account) (return-from verify-keychain-admin t))
      (let ((key (active-key account key-id type)))
        (and key (clempo.keychain:authorized-key-is-admin key) t)))))

(defcall signature-verifier recover* (hash signature)
    (:sol "recover(bytes32 hash, bytes signature)" :returns "address")
  (recover hash signature))

(defcall signature-verifier verify* (signer hash signature)
    (:sol "verify(address signer, bytes32 hash, bytes signature)" :returns "bool")
  (verify signer hash signature))

(defcall signature-verifier verify-keychain* (account hash signature)
    (:sol "verifyKeychain(address account, bytes32 hash, bytes signature)" :returns "bool"
     :since :t6)
  (verify-keychain account hash signature))

(defcall signature-verifier verify-keychain-admin* (account hash signature)
    (:sol "verifyKeychainAdmin(address account, bytes32 hash, bytes signature)" :returns "bool"
     :since :t6)
  (verify-keychain-admin account hash signature))

(defcall signature-verifier verify-configurable-account* (account hash signature)
    (:sol "verifyConfigurableAccount(address account, bytes32 hash, bytes signature)"
     :returns "bool" :since :t14)
  (verify-configurable-account account hash signature))

(define-constant +unbounded-selectors+
  (list (clempo.abi:abi-selector "verifyConfigurableAccount(address,bytes32,bytes)")
        (clempo.abi:abi-selector "verifyKeychain(address,bytes32,bytes)")))

(defdispatch signature-verifier (self input sender)
  (charge-input-cost input)
  (when (and (> (length input) +max-calldata-length+)
             (not (and (fork-if (:since :t14) t nil)
                       (member (octets->integer input :end 4) +unbounded-selectors+))))
    (revert 'invalid-format))
  (dispatch-calls))

(register-precompile +signature-verifier-address+ 'signature-verifier :t3)
