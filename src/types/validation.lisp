(in-package #:clempo.transactions)

(defun calldata-tokens (data &key (start 0) (end (length data)))
  "Count EIP-7623 calldata tokens (zero = 1, nonzero = 4), without copying a slice."
  (declare (type octets data) (type fixnum start end))
  (invariant (<= 0 start end (length data)) "Invalid calldata slice")
  (loop for i fixnum from start below end
        sum (if (zerop (aref data i)) 1 4)))

(defun tempo-signature-verification-gas (signature)
  "Extra intrinsic gas above the secp256k1 verification included in 21000.
Primitive P256/WebAuthn add 5000; WebAuthn also pays its assertion calldata cost.
Keychain envelopes add 3000 for validation, independently of their inner scheme."
  (let* ((kind (clempo.signatures:signature-inner-kind signature))
         (keychain (and (/= (length signature) 65)
                        (member (aref signature 0) '(3 4))))
         (start (if keychain 21 0)))
    (+ (if keychain 3000 0)
       (ecase kind
         (:secp256k1 0)
         (:p256 5000)
         (:webauthn (+ 5000 (* 4 (calldata-tokens signature :start (1+ start)
                                                 :end (- (length signature) 128)))))))))


(defun validate-signature-version (signature t1c-p)
  "Gate keychain V1/V2 at T1C. Shape errors signal SIGNATURE-ERROR."
  (case (clempo.signatures:signature-kind signature)
    (:keychain-v1 (if t1c-p :legacy-keychain-post-t1c t))
    (:keychain-v2 (if t1c-p t :keychain-v2-before-t1c))
    (otherwise t)))

(defun tempo-authorization-fields (item)
  "Return CHAIN-ID, ADDRESS, NONCE, SIGNATURE for a decoded four-field authorization.
The signature bytes are borrowed. Recover its authority with
CLEMPO.SIGNATURES:AUTHORIZATION-SIGNER; state/code and chain checks belong to execution."
  (require-count item 4)
  (validate-field (second item) :address)
  (validate-field (fourth item) :bytes)
  (values (field-integer (first item)) (octets->integer (second item))
          (field-integer (third item) 8) (fourth item)))

(defun validation-fork-at-least-p (fork target)
  (let* ((forks '(:t0 :t1 :t1a :t1b :t1c :t2 :t3 :t4 :t5 :t6 :t7 :t8
                  :t9 :t10 :t11 :t12 :t13 :t14 :t15))
         (index (position fork forks)) (minimum (position target forks)))
    (invariant (and index minimum) "Unknown validation fork ~S / ~S" fork target)
    (>= index minimum)))

(defun validate-key-authorization (key-auth sender access-key-id chain-id fork
                                  &key transaction-signature)
  "Stateless grant checks from Tempo handler.validate_key_authorization; T or keyword.
KEY-AUTH is the decoded (authorization signature), or NIL for no grant. SENDER and
ACCESS-KEY-ID must already be recovered from the transaction. Supply TRANSACTION-
SIGNATURE whenever ACCESS-KEY-ID is non-NIL, for the inner-type binding checks.
This verifies the grant signature, not its on-chain permission/expiry/spending state."
  (unless key-auth (return-from validate-key-authorization t))
  (when (and access-key-id (null transaction-signature))
    (return-from validate-key-authorization :missing-transaction-signature))
  (handler-case
      (let* ((view (clempo.signatures:parse-key-authorization key-auth))
             (t6-p (validation-fork-at-least-p fork :t6))
             (t3-p (validation-fork-at-least-p fork :t3))
             (same-key (and access-key-id
                            (= access-key-id (clempo.signatures:key-authorization-key-id view))))
             (inner-kind (and access-key-id
                              (clempo.signatures:signature-inner-kind transaction-signature))))
        (with-accessors ((grant-chain clempo.signatures:key-authorization-chain-id)
                         (key-type clempo.signatures:key-authorization-key-type)
                         (expiry clempo.signatures:key-authorization-expiry)
                         (limits clempo.signatures:key-authorization-limits)
                         (calls clempo.signatures:key-authorization-allowed-calls)
                         (witness clempo.signatures:key-authorization-witness)
                         (admin clempo.signatures:key-authorization-is-admin)
                         (account clempo.signatures:key-authorization-account)) view
          (cond
            ((and access-key-id (not same-key) (not t6-p))
             :access-key-cannot-authorize-other-keys)
            ((and same-key t3-p
                  (not (eq inner-kind (ecase key-type
                                        (0 :secp256k1) (1 :p256) (2 :webauthn)))))
             :key-authorization-signature-type-mismatch)
            ((and (or admin account) (not t6-p)) :admin-key-before-t6)
            ((and t6-p account (/= account sender)) :key-authorization-account-mismatch)
            ((and admin (or expiry (not (eq limits :absent)) (not (eq calls :absent))))
             :admin-key-has-restrictions)
            (t
             (let ((signer (handler-case (clempo.signatures:key-authorization-signer key-auth)
                             (clempo.signatures:signature-error ()
                               (return-from validate-key-authorization
                                 :key-authorization-signature-recovery-failed)))))
               (cond
                 ((and (not t6-p) (/= signer sender)) :key-authorization-not-signed-by-root)
                 ((not (or (= grant-chain chain-id)
                           (and (not (validation-fork-at-least-p fork :t1c))
                                (zerop grant-chain))))
                  :key-authorization-chain-id-mismatch)
                 ((and witness (not (validation-fork-at-least-p fork :t5)))
                  :key-authorization-witness-before-t5)
                 ((and (not t3-p) (not (eq limits :absent))
                       (some (lambda (limit)
                               (plusp (clempo.signatures:key-authorization-limit-period limit)))
                             limits))
                  :periodic-key-authorization-before-t3)
                 ((and (not t3-p) (not (eq calls :absent))) :key-call-scopes-before-t3)
                 ((and t6-p (/= signer sender) (null account))
                  :key-authorization-account-required)
                 ((and t6-p (= signer sender) access-key-id (not same-key))
                  :root-grant-requires-root-transaction)
                 ((and t6-p (/= signer sender)
                       (or (null access-key-id) (/= signer access-key-id)))
                  :admin-grant-requires-admin-transaction)
                 ((and t6-p (/= signer sender)
                       (not (eq inner-kind (clempo.signatures:signature-kind (second key-auth)))))
                  :admin-grant-signature-type-mismatch)
                 (t t)))))))
    (clempo.signatures:signature-error () :invalid-key-authorization)
    (clempo.rlp:rlp-error () :invalid-key-authorization)))
