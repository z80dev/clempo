(in-package #:clempo.keychain)

;;; AccountKeychain (crates/precompiles/src/account_keychain): access keys with a signature
;;; type, an expiry, per-token spending limits (T3: periodic) and call scopes (TIP-1011, T3),
;;; T5 witnesses (TIP-1053), T6 admin keys (retired by TIP-1137 at T14) and T14
;;; ConfigurableAccount grants (TIP-1111/1113). The transaction key and tx.origin live in
;;; transient storage, seeded by the executor.

(defenum stored-signature-type secp256k1 p256 webauthn configurable-account)

(defstorable authorized-key
  (signature-type stored-signature-type)
  (expiry u64)
  (enforce-limits bool)
  (is-revoked bool)
  (is-admin bool))

(defstorable spending-limit-state
  (remaining u256)
  (max u128)
  (period u64)
  (period-end u64))

(defstorable selector-scope
  (recipients (set address)))

(defstorable target-scope
  (selectors (set b4))
  (selector-scopes (mapping b4 selector-scope)))

(defstorable key-scope
  (is-scoped bool)
  (targets (set address))
  (target-scopes (mapping address target-scope)))

(defcontract account-keychain (:address +account-keychain-address+)
  (keys (mapping address (mapping address authorized-key)))
  (spending-limits (mapping b256 (mapping address spending-limit-state)))
  (key-scopes (mapping b256 key-scope))
  (key-authorization-witnesses (mapping address (mapping b256 bool)))
  (transaction-key address :doc "Transient.")
  (tx-origin address :doc "Transient."))

(deferror unauthorized-caller "UnauthorizedCaller()")
(deferror key-already-exists "KeyAlreadyExists()")
(deferror key-not-found "KeyNotFound()")
(deferror key-expired "KeyExpired()")
(deferror spending-limit-exceeded "SpendingLimitExceeded()")
(deferror invalid-spending-limit "InvalidSpendingLimit()")
(deferror invalid-signature-type "InvalidSignatureType()")
(deferror zero-public-key "ZeroPublicKey()")
(deferror expiry-in-past "ExpiryInPast()")
(deferror key-already-revoked "KeyAlreadyRevoked()")
(deferror signature-type-mismatch "SignatureTypeMismatch(uint8 expected, uint8 actual)")
(deferror call-not-allowed "CallNotAllowed()")
(deferror invalid-call-scope "InvalidCallScope()")
(deferror invalid-key-id "InvalidKeyId()")
(deferror invalid-key-authorization-witness "InvalidKeyAuthorizationWitness()")
(deferror key-authorization-witness-already-burned "KeyAuthorizationWitnessAlreadyBurned()")
(deferror legacy-authorize-key-selector-changed "LegacyAuthorizeKeySelectorChanged(bytes4 newSelector)")
(deferror static-call-not-allowed "StaticCallNotAllowed()")

(defevent key-authorized "KeyAuthorized(address indexed account, address indexed publicKey, uint8 signatureType, uint64 expiry)")
(defevent admin-key-authorized "AdminKeyAuthorized(address indexed account, address indexed publicKey)")
(defevent key-revoked "KeyRevoked(address indexed account, address indexed publicKey)")
(defevent spending-limit-updated "SpendingLimitUpdated(address indexed account, address indexed publicKey, address indexed token, uint256 newLimit)")
(defevent access-key-spend "AccessKeySpend(address indexed account, address indexed publicKey, address indexed token, uint256 amount, uint256 remainingLimit)")
(defevent key-authorization-witness "KeyAuthorizationWitness(address indexed account, bytes32 indexed witness)")
(defevent key-authorization-witness-burned "KeyAuthorizationWitnessBurned(address indexed account, bytes32 indexed witness)")

(defconstant +self+ +account-keychain-address+)
(defconstant +configurable-account-type+ 3)
(defconstant +invalid-signature-type+ 255 "alloy's __Invalid enum variant, as the decoder yields it.")
(define-constant +zero-periodic-remaining-sentinel+ (1- (ash 1 256))
  "(T7+) Alias for a zero periodic remaining amount, so the slot is never cleared.")
(define-constant +u64-max+ (1- (ash 1 64)))

(define-constant +tip20-transfer-selector+ (clempo.abi:abi-selector "transfer(address,uint256)"))
(define-constant +tip20-approve-selector+ (clempo.abi:abi-selector "approve(address,uint256)"))
(define-constant +tip20-transfer-with-memo-selector+
  (clempo.abi:abi-selector "transferWithMemo(address,uint256,bytes32)"))

(defun constrained-tip20-selector-p (selector)
  (or (= selector +tip20-transfer-selector+) (= selector +tip20-approve-selector+)
      (= selector +tip20-transfer-with-memo-selector+)))

(defun now ()
  "The block timestamp saturated to u64."
  (min (block-timestamp) +u64-max+))

(defun sat-add64 (a b) (min +u64-max+ (+ a b)))
(defun sat-add256 (a b) (min (1- (ash 1 256)) (+ a b)))

(defun spending-limit-key (account key-id)
  "AccountKeychain::spending_limit_key: keccak256(account || keyId), no gas."
  (let ((data (make-octets 40)))
    (replace data (integer->octets account 20))
    (replace data (integer->octets key-id 20) :start1 20)
    (clempo.keccak:keccak256-integer data)))

(defun t3-spending-limit-cap (limit)
  (when (> limit u128-max) (revert 'invalid-spending-limit))
  limit)

;;; Places of the call-scope tree.

(defun scope-place (key-hash) (key-scopes +self+ key-hash))
(defun scoped-place (key-hash) (field (scope-place key-hash) 'is-scoped))
(defun targets-place (key-hash) (field (scope-place key-hash) 'targets))
(defun selectors-place (key-hash target)
  (field (at (field (scope-place key-hash) 'target-scopes) target) 'selectors))
(defun recipients-place (key-hash target selector)
  (field (at (field (at (field (scope-place key-hash) 'target-scopes) target) 'selector-scopes)
             selector)
         'recipients))

;;; Key loading and authorization predicates.

(defun read-key (account key-id)
  "keys[account][key_id].read(): a stored signature type past the enum is Panic(0x21)."
  (let ((key (read (keys +self+ account key-id))))
    (when (> (authorized-key-signature-type key) +configurable-account-type+)
      (panic +panic-enum-conversion+))
    key))

(defun load-active-key (account key-id now)
  "load_active_key: the stored key, or KeyAlreadyRevoked / KeyNotFound / KeyExpired."
  (let ((key (read-key account key-id)))
    (when (authorized-key-is-revoked key) (revert 'key-already-revoked))
    ;; TIP-1137: keys stored as admin act as regular access keys from T14.
    (fork-if (:since :t14) (setf (authorized-key-is-admin key) nil))
    (when (zerop (authorized-key-expiry key)) (revert 'key-not-found))
    (when (>= now (authorized-key-expiry key)) (revert 'key-expired))
    key))

(defmacro unless-reverted (form &optional default)
  "FORM's value, or DEFAULT when it reverts (Err(e) if !e.is_system_error())."
  `(handler-case ,form
     (precompile-revert (c)
       ;; A panic is a system error: it propagates.
       (if (eq (precompile-revert-name c) 'panic) (error c) ,default))))

(defun admin-key-p (account key-id)
  "is_admin_key: the root (unless retired, T14) or an active admin access key."
  (if (and (= key-id account) (not (retired-root-p account)))
      t
      (let ((key (unless-reverted (load-active-key account key-id (now)))))
        (and key (authorized-key-is-admin key)))))

(defun ensure-admin-caller (sender)
  (let ((transaction-key (t-read (transaction-key +self+))))
    (when (and (/= 0 transaction-key)
               (fork-if (:since :t6) (not (admin-key-p sender transaction-key)) t))
      (revert 'unauthorized-caller)))
  (fork-if (:since :t2)
    (let ((origin (t-read (tx-origin +self+))))
      (when (or (zerop origin) (/= origin sender))
        (revert 'unauthorized-caller)))))

(defun accepts-signature-type-p (key signature-type retired-root-thunk)
  "accepts_signature_type: the stored type, or (T14) a ConfigurableAccount signature for a
primitive key whose key ID retired its root."
  (let ((stored (authorized-key-signature-type key)))
    (cond ((= stored signature-type) t)
          ((or (= stored +configurable-account-type+)
               (/= signature-type +configurable-account-type+))
           nil)
          (t (funcall retired-root-thunk)))))

(defun validate-keychain-authorization (account key-id timestamp expected-signature-type)
  "AccountKeychain::validate_keychain_authorization: the active AUTHORIZED-KEY of ACCOUNT's
KEY-ID at TIMESTAMP, checking EXPECTED-SIGNATURE-TYPE (0..3) unless NIL (pre-T1). Signals
precompile-revert KeyAlreadyRevoked / KeyNotFound / KeyExpired / SignatureTypeMismatch."
  (let ((key (load-active-key account key-id timestamp)))
    (when (and expected-signature-type
               (not (accepts-signature-type-p key expected-signature-type
                                              (lambda () (retired-root-p key-id)))))
      (revert 'signature-type-mismatch (authorized-key-signature-type key) expected-signature-type))
    key))

(defun get-key (account key-id)
  "The stored AUTHORIZED-KEY of ACCOUNT's KEY-ID (one SLOAD), whatever its state."
  (read-key account key-id))

;;; Call scopes.

(defun validate-selector-rules (target rules)
  (let ((sort-selectors (spec>= :t11))
        (tip20 :unknown))
    (flet ((tip20-p ()
             (when (eq tip20 :unknown)
               (setf tip20 (fork-if (:before :t4)
                             (clempo.tip20-factory:tip20-p target)
                             (tip20-address-p target))))
             tip20))
      (when (and sort-selectors (has-duplicates-metered (mapcar #'first rules)))
        (revert 'invalid-call-scope))
      (let ((seen (make-hash-table)))
        (loop for (selector recipients) in rules
              do (unless sort-selectors
                   (when (gethash selector seen) (revert 'invalid-call-scope))
                   (setf (gethash selector seen) t))
                 (when recipients
                   (unless (and (constrained-tip20-selector-p selector) (tip20-p))
                     (revert 'invalid-call-scope))
                   (when (or (member 0 recipients)
                             (has-duplicates-metered recipients))
                     (revert 'invalid-call-scope))))))))

(defun validate-one-call-scope (scope)
  (destructuring-bind (target rules) scope
    (when (zerop target) (revert 'invalid-call-scope))
    (when rules (validate-selector-rules target rules))))

(defun validate-call-scopes (scopes)
  (fork-if (:since :t11)
    (progn
      (when (has-duplicates-metered (mapcar #'first scopes)) (revert 'invalid-call-scope))
      (mapc #'validate-one-call-scope scopes))
    (let ((seen (make-hash-table)))
      (dolist (scope scopes)
        (when (gethash (first scope) seen) (revert 'invalid-call-scope))
        (setf (gethash (first scope) seen) t)
        (fork-if (:since :t4) (validate-one-call-scope scope))))))

(defun clear-target-selectors (key-hash target)
  (let ((selectors (read (selectors-place key-hash target))))
    (dolist (selector selectors)
      (delete (recipients-place key-hash target selector)))
    (delete (selectors-place key-hash target))))

(defun clear-all-target-scopes (key-hash)
  (dolist (target (read (targets-place key-hash)))
    (clear-target-selectors key-hash target))
  (delete (targets-place key-hash)))

(defun remove-target-scope (key-hash target)
  (when (set-remove (targets-place key-hash) target)
    (clear-target-selectors key-hash target)))

(defun upsert-target-scope (key-hash scope)
  (destructuring-bind (target rules) scope
    (fork-if (:before :t4) (validate-one-call-scope scope))
    (set-insert (targets-place key-hash) target)
    (clear-target-selectors key-hash target)
    (loop for (selector recipients) in rules
          do (set-insert (selectors-place key-hash target) selector)
             (if recipients
                 (write (recipients-place key-hash target selector) recipients)
                 ;; Pre-T4 kept the empty-set delete for its storage-touch pattern.
                 (fork-if (:before :t4) (delete (recipients-place key-hash target selector)))))))

(defun replace-allowed-calls (key-hash allowed-calls)
  "ALLOWED-CALLS is :ANY (unrestricted) or a list of scopes (NIL: scoped deny-all)."
  (clear-all-target-scopes key-hash)
  (if (eq allowed-calls :any)
      (write (scoped-place key-hash) nil)
      (progn
        (write (scoped-place key-hash) t)
        (when allowed-calls
          (validate-call-scopes allowed-calls)
          (dolist (scope allowed-calls) (upsert-target-scope key-hash scope))))))

(defun validate-call-scope (caller key-id to input)
  "validate_call_scope_for_transaction: signal CallNotAllowed unless CALLER's KEY-ID may
call TO (an address, or NIL for a CREATE) with INPUT (octets)."
  (when (or (zerop key-id) (spec< :t3))
    (return-from validate-call-scope nil))
  (unless to (revert 'call-not-allowed))
  (let ((key-hash (spending-limit-key caller key-id)))
    (unless (read (scoped-place key-hash)) (return-from validate-call-scope nil))
    (unless (set-contains (targets-place key-hash) to) (revert 'call-not-allowed))
    (when (zerop (set-length (selectors-place key-hash to)))
      (return-from validate-call-scope nil))
    (when (< (length input) 4) (revert 'call-not-allowed))
    (let ((selector (octets->integer input :end 4)))
      (unless (set-contains (selectors-place key-hash to) selector) (revert 'call-not-allowed))
      (let ((recipients (recipients-place key-hash to selector)))
        (when (zerop (set-length recipients)) (return-from validate-call-scope nil))
        (when (< (length input) 36) (revert 'call-not-allowed))
        (unless (every #'zerop (subseq input 4 16)) (revert 'call-not-allowed))
        (unless (set-contains recipients (octets->integer input :start 16 :end 36))
          (revert 'call-not-allowed))
        nil))))

;;; Authorization.

(defun apply-key-authorization-restrictions (account key-id limits allowed-calls)
  (let ((limit-key (spending-limit-key account key-id))
        (now (now)))
    (loop for (token amount period) in limits
          do (fork-if (:since :t3)
               (write (spending-limits +self+ limit-key token)
                      (make-spending-limit-state
                       :remaining amount :max (t3-spending-limit-cap amount) :period period
                       :period-end (if (zerop period) 0 (sat-add64 now period))))
               (write (field (spending-limits +self+ limit-key token) 'remaining) amount)))
    (fork-if (:since :t3) (replace-allowed-calls limit-key allowed-calls))))

(defun ensure-witness-not-burned (account witness)
  (when (read (key-authorization-witnesses +self+ account witness))
    (revert 'key-authorization-witness-already-burned)))

(defun authorize-key-internal (sender key-id signature-type expiry enforce-limits limits
                               allow-any-calls allowed-calls witness admin)
  (ensure-admin-caller sender)
  (fork-if (:since :t14)
    (let* ((info (warm-caller-account sender))
           (commitment (if info (account-info-commitment info) 0)))
      (when (and (/= 0 commitment)
                 (not (clempo.configurable-account:valid-account-code-p
                       (account-info-code-hash info) commitment)))
        (revert 'invalid-key-id))))
  (when (zerop key-id) (revert 'zero-public-key))
  (let ((delegate-grant (and (= signature-type +configurable-account-type+)
                             (fork-if (:since :t14) t nil))))
    (when (and (or admin delegate-grant) (= key-id sender)) (revert 'invalid-key-id))
    (when delegate-grant
      (when (or (not (clempo.configurable-account:valid-account-p key-id))
                (let ((info (account key-id)))
                  (and info
                       (not (clempo.configurable-account:valid-account-code-p
                             (account-info-code-hash info) (account-info-commitment info))))))
        (revert 'invalid-key-id)))
    (fork-if (:since :t0)
      (when (<= expiry (now)) (revert 'expiry-in-past)))
    (let ((existing (read-key sender key-id)))
      (when (> (authorized-key-expiry existing) 0) (revert 'key-already-exists))
      (when (authorized-key-is-revoked existing) (revert 'key-already-revoked)))
    ;; TIP-1111: a migrated delegate only takes ConfigurableAccount grants.
    (when (and (not delegate-grant) (retired-root-p key-id))
      (revert 'invalid-signature-type))
    (when (> signature-type +configurable-account-type+) (revert 'invalid-signature-type))
    (when (and (= signature-type +configurable-account-type+) (spec< :t14))
      (revert 'invalid-signature-type))
    (let ((allowed
            (fork-if (:since :t3)
              (progn
                (when enforce-limits
                  (let ((seen (make-hash-table)))
                    (dolist (limit limits)
                      (when (gethash (first limit) seen) (revert 'invalid-spending-limit))
                      (setf (gethash (first limit) seen) t))))
                (if allow-any-calls
                    (progn
                      (fork-if (:since :t5)
                        (when allowed-calls (revert 'invalid-call-scope)))
                      :any)
                    allowed-calls))
              (progn
                (when (some (lambda (limit) (/= 0 (third limit))) limits)
                  (revert 'invalid-spending-limit))
                (when (or (not allow-any-calls) allowed-calls) (revert 'invalid-call-scope))
                :any))))
      (when witness (ensure-witness-not-burned sender witness))
      (write (keys +self+ sender key-id)
             (make-authorized-key :signature-type signature-type :expiry expiry
                                  :enforce-limits enforce-limits :is-revoked nil :is-admin admin))
      (unless admin
        (apply-key-authorization-restrictions sender key-id (and enforce-limits limits) allowed))
      (when witness (emit +self+ 'key-authorization-witness sender witness))
      (emit +self+ 'key-authorized sender key-id signature-type expiry))))

(defun authorize-key (account key-id signature-type expiry enforce-limits limits
                      allow-any-calls allowed-calls witness)
  "AccountKeychain::authorize_key for ACCOUNT (msg.sender). LIMITS: list of (token amount
period); ALLOWED-CALLS: list of (target ((selector (recipient...))...)); WITNESS: a b256 or NIL."
  (authorize-key-internal account key-id signature-type expiry enforce-limits limits
                          allow-any-calls allowed-calls witness nil))

(defun authorize-admin-key (account key-id signature-type witness)
  "AccountKeychain::authorize_admin_key (T6..T13)."
  (fork-if (:since :t14) (revert 'invalid-key-id))
  (authorize-key-internal account key-id signature-type +u64-max+ nil '() t '() witness t)
  (emit +self+ 'admin-key-authorized account key-id))

(defun revoke-key (account key-id)
  (ensure-admin-caller account)
  (when (zerop (authorized-key-expiry (read-key account key-id)))
    (revert 'key-not-found))
  (write (keys +self+ account key-id) (make-authorized-key :is-revoked t))
  (emit +self+ 'key-revoked account key-id))

(defun update-spending-limit (account key-id token new-limit)
  (ensure-admin-caller account)
  (let ((key (load-active-key account key-id (now))))
    (when (authorized-key-is-admin key) (revert 'invalid-key-id))
    (unless (authorized-key-enforce-limits key)
      (setf (authorized-key-enforce-limits key) t)
      (write (keys +self+ account key-id) key)))
  (let ((place (spending-limits +self+ (spending-limit-key account key-id) token)))
    (fork-if (:since :t3)
      (let ((state (read place)))
        (setf (spending-limit-state-remaining state) new-limit
              (spending-limit-state-max state) (t3-spending-limit-cap new-limit))
        (write place state))
      (write (field place 'remaining) new-limit)))
  (emit +self+ 'spending-limit-updated account key-id token new-limit))

;;; Spending limits.

(defun next-period-end (period period-end now)
  (sat-add64 period-end (min +u64-max+ (* period (min +u64-max+ (1+ (floor (max 0 (- now period-end)) period)))))))

(defun effective-limit-state-with-key (account key-id token now key)
  (when (or (authorized-key-is-revoked key) (zerop (authorized-key-expiry key)))
    (return-from effective-limit-state-with-key (values 0 0)))
  (when (and (>= now (authorized-key-expiry key)) (spec>= :t3))
    (return-from effective-limit-state-with-key (values 0 0)))
  (let* ((place (spending-limits +self+ (spending-limit-key account key-id) token))
         (remaining (read (field place 'remaining))))
    (when (spec< :t3) (return-from effective-limit-state-with-key (values remaining 0)))
    (let ((period (read (field place 'period))))
      (when (zerop period) (return-from effective-limit-state-with-key (values remaining 0)))
      (when (and (spec>= :t7) (= remaining +zero-periodic-remaining-sentinel+))
        (setf remaining 0))
      (let ((period-end (read (field place 'period-end))))
        (if (< now period-end)
            (values remaining period-end)
            (let ((next-end (next-period-end period period-end now)))
              (values (read (field place 'max)) next-end)))))))

(defun effective-limit-state (account key-id token now)
  (if (and (zerop key-id) (spec>= :t3))
      (values 0 0)
      (effective-limit-state-with-key account key-id token now
                                      (read-key account key-id))))

(defun verify-and-update-spending (account key-id token amount)
  (when (zerop key-id) (return-from verify-and-update-spending nil))
  (let* ((now (now))
         (key (load-active-key account key-id now)))
    (unless (authorized-key-enforce-limits key) (return-from verify-and-update-spending nil))
    (let ((place (spending-limits +self+ (spending-limit-key account key-id) token)))
      (fork-if (:before :t3)
        (let ((remaining (read (field place 'remaining))))
          (when (> amount remaining) (revert 'spending-limit-exceeded))
          (write (field place 'remaining) (- remaining amount)))
        (let* ((state (read place))
               (remaining (spending-limit-state-remaining state))
               (periodic (/= 0 (spending-limit-state-period state))))
          (when periodic
            (when (and (spec>= :t7) (= remaining +zero-periodic-remaining-sentinel+))
              (setf remaining 0))
            (when (>= now (spending-limit-state-period-end state))
              (setf remaining (spending-limit-state-max state)
                    (spending-limit-state-remaining state) remaining
                    (spending-limit-state-period-end state)
                    (next-period-end (spending-limit-state-period state)
                                     (spending-limit-state-period-end state) now))))
          (when (> amount remaining) (revert 'spending-limit-exceeded))
          (let ((new-remaining (- remaining amount)))
            (if periodic
                (progn
                  (setf (spending-limit-state-remaining state)
                        (if (and (spec>= :t7) (zerop new-remaining))
                            +zero-periodic-remaining-sentinel+
                            new-remaining))
                  (write place state))
                (write (field place 'remaining) new-remaining))
            (emit +self+ 'access-key-spend account key-id token amount new-remaining)))))
    nil))

(defun transaction-key-for (account)
  "The transaction's access key when ACCOUNT is tx.origin, else NIL (root key or other caller)."
  (let ((transaction-key (t-read (transaction-key +self+))))
    (unless (zerop transaction-key)
      (when (= account (t-read (tx-origin +self+)))
        transaction-key))))

(defun authorize-transfer (account token amount)
  "AccountKeychain::authorize_transfer: charge AMOUNT of TOKEN to the transaction's access key."
  (let ((key-id (transaction-key-for account)))
    (when key-id (verify-and-update-spending account key-id token amount))
    nil))

(defun authorize-approve (account token old-approval new-approval)
  "AccountKeychain::authorize_approve: charge an approval increase to the access key."
  (let ((key-id (transaction-key-for account)))
    (when key-id
      (let ((increase (max 0 (- new-approval old-approval))))
        (unless (zerop increase)
          (verify-and-update-spending account key-id token increase))))
    nil))

(defun refund-spending-limit (account token amount)
  "AccountKeychain::refund_spending_limit: restore AMOUNT after a fee refund."
  (let ((key-id (transaction-key-for account)))
    (unless key-id (return-from refund-spending-limit nil))
    (let ((key (unless-reverted (load-active-key account key-id (now)))))
      (unless (and key (authorized-key-enforce-limits key))
        (return-from refund-spending-limit nil)))
    (let ((place (spending-limits +self+ (spending-limit-key account key-id) token)))
      (fork-if (:before :t3)
        (write (field place 'remaining) (sat-add256 (read (field place 'remaining)) amount))
        (let* ((state (read place))
               (remaining (spending-limit-state-remaining state))
               (max (spending-limit-state-max state))
               (refunded (if (and (spec>= :t7) (/= 0 (spending-limit-state-period state))
                                  (= remaining +zero-periodic-remaining-sentinel+))
                             amount
                             (sat-add256 remaining amount))))
          (setf (spending-limit-state-remaining state) (if (zerop max) refunded (min refunded max)))
          (write place state))))
    nil))

;;; Transient context (executor).

(defun set-transaction-key (key-id)
  "Seed the transaction's access key (0 = root key) before execution."
  (t-write (transaction-key +self+) key-id))

(defun set-tx-origin (origin)
  "Seed tx.origin before execution."
  (t-write (tx-origin +self+) origin))

;;; ABI.

(defun sender-guard-legacy-static ()
  (when (static-p) (revert 'static-call-not-allowed)))

(defcall account-keychain authorize-key-legacy (key-id signature-type expiry enforce-limits limits)
    (:sol "authorizeKey(address keyId, enum:4 signatureType, uint64 expiry, bool enforceLimits, (address,uint256)[] limits)")
  (fork-if (:since :t3)
    (revert 'legacy-authorize-key-selector-changed
            (clempo.abi:abi-selector
             "authorizeKey(address,uint8,(uint64,bool,(address,uint256,uint64)[],bool,(address,(bytes4,address[])[])[]))"))
    (progn
      (sender-guard-legacy-static)
      (authorize-key sender key-id signature-type expiry enforce-limits
                     (mapcar (lambda (l) (list (first l) (second l) 0)) limits)
                     t '() nil)
      (values))))

(defun authorize-with-config (sender key-id signature-type config witness)
  (destructuring-bind (expiry enforce-limits limits allow-any-calls allowed-calls) config
    (authorize-key sender key-id signature-type expiry enforce-limits limits allow-any-calls
                   allowed-calls witness)))

(defcall account-keychain authorize-key* (key-id signature-type config)
    (:sol "authorizeKey(address keyId, enum:4 signatureType, (uint64,bool,(address,uint256,uint64)[],bool,(address,(bytes4,address[])[])[]) config)"
     :mutates t :since :t3)
  (authorize-with-config sender key-id signature-type config nil)
  (values))

(defcall account-keychain authorize-key-with-witness (key-id signature-type config witness)
    (:sol "authorizeKey(address keyId, enum:4 signatureType, (uint64,bool,(address,uint256,uint64)[],bool,(address,(bytes4,address[])[])[]) config, bytes32 witness)"
     :mutates t :since :t5)
  (authorize-with-config sender key-id signature-type config witness)
  (values))

(defcall account-keychain authorize-admin-key* (key-id signature-type witness)
    (:sol "authorizeAdminKey(address keyId, enum:4 signatureType, bytes32 witness)"
     :mutates t :since :t6 :until :t14)
  (authorize-admin-key sender key-id signature-type witness)
  (values))

(defcall account-keychain burn-key-authorization-witness (witness)
    (:sol "burnKeyAuthorizationWitness(bytes32 witness)" :mutates t :since :t5)
  (ensure-admin-caller sender)
  (ensure-witness-not-burned sender witness)
  (write (key-authorization-witnesses +self+ sender witness) t)
  (emit +self+ 'key-authorization-witness-burned sender witness)
  (values))

(defcall account-keychain revoke-key* (key-id)
    (:sol "revokeKey(address keyId)" :mutates t)
  (revoke-key sender key-id)
  (values))

(defcall account-keychain update-spending-limit* (key-id token new-limit)
    (:sol "updateSpendingLimit(address keyId, address token, uint256 newLimit)" :mutates t)
  (update-spending-limit sender key-id token new-limit)
  (values))

(defcall account-keychain set-allowed-calls (key-id scopes)
    (:sol "setAllowedCalls(address keyId, (address,(bytes4,address[])[])[] scopes)"
     :mutates t :since :t3)
  (when (spec< :t3) (revert 'invalid-call-scope))
  (ensure-admin-caller sender)
  (when (authorized-key-is-admin (load-active-key sender key-id (now)))
    (revert 'invalid-key-id))
  (let ((key-hash (spending-limit-key sender key-id)))
    (unless scopes (revert 'invalid-call-scope))
    (validate-call-scopes scopes)
    (dolist (scope scopes) (upsert-target-scope key-hash scope))
    (write (scoped-place key-hash) t))
  (values))

(defcall account-keychain remove-allowed-calls (key-id target)
    (:sol "removeAllowedCalls(address keyId, address target)" :mutates t :since :t3)
  (ensure-admin-caller sender)
  (when (authorized-key-is-admin (load-active-key sender key-id (now)))
    (revert 'invalid-key-id))
  (let ((key-hash (spending-limit-key sender key-id)))
    (when (read (scoped-place key-hash))
      (remove-target-scope key-hash target)))
  (values))

(defcall account-keychain get-key* (account key-id)
    (:sol "getKey(address account, address keyId)" :returns "(uint8,address,uint64,bool,bool)")
  (let ((key (read-key account key-id)))
    (if (or (zerop (authorized-key-expiry key)) (authorized-key-is-revoked key))
        (list 0 0 0 nil (authorized-key-is-revoked key))
        (list (authorized-key-signature-type key) key-id (authorized-key-expiry key)
              (authorized-key-enforce-limits key) (authorized-key-is-revoked key)))))

(defcall account-keychain get-remaining-limit (account key-id token)
    (:sol "getRemainingLimit(address account, address keyId, address token)" :returns "uint256"
     :until :t3)
  (fork-if (:before :t2)
    (read (field (spending-limits +self+ (spending-limit-key account key-id) token) 'remaining))
    (values (effective-limit-state account key-id token (now)))))

(defcall account-keychain get-remaining-limit-with-period (account key-id token)
    (:sol "getRemainingLimitWithPeriod(address account, address keyId, address token)"
     :returns ("uint256" "uint64") :since :t3)
  (effective-limit-state account key-id token (now)))

(defcall account-keychain get-allowed-calls (account key-id)
    (:sol "getAllowedCalls(address account, address keyId)"
     :returns ("bool" "(address,(bytes4,address[])[])[]") :since :t3)
  (when (zerop key-id) (return-from account-keychain/get-allowed-calls (values nil '())))
  (let ((key (read-key account key-id)))
    (when (or (zerop (authorized-key-expiry key)) (authorized-key-is-revoked key)
              (>= (now) (authorized-key-expiry key)))
      (return-from account-keychain/get-allowed-calls (values t '()))))
  (let ((key-hash (spending-limit-key account key-id)))
    (unless (read (scoped-place key-hash))
      (return-from account-keychain/get-allowed-calls (values nil '())))
    (values t
            (loop for target in (read (targets-place key-hash))
                  collect (list target
                                (loop for selector in (read (selectors-place key-hash target))
                                      collect (list selector
                                                    (read (recipients-place key-hash target selector)))))))))

(defcall account-keychain is-key-authorization-witness-burned (account witness)
    (:sol "isKeyAuthorizationWitnessBurned(address account, bytes32 witness)" :returns "bool"
     :since :t5)
  (read (key-authorization-witnesses +self+ account witness)))

(defcall account-keychain is-admin-key (account key-id)
    (:sol "isAdminKey(address account, address keyId)" :returns "bool" :since :t6)
  (admin-key-p account key-id))

(defcall account-keychain get-transaction-key ()
    (:sol "getTransactionKey()" :returns "address")
  (t-read (transaction-key +self+)))

(define-constant +authorize-selectors+
  (list (clempo.abi:abi-selector "authorizeKey(address,uint8,uint64,bool,(address,uint256)[])")
        (clempo.abi:abi-selector "authorizeKey(address,uint8,(uint64,bool,(address,uint256,uint64)[],bool,(address,(bytes4,address[])[])[]))")
        (clempo.abi:abi-selector "authorizeKey(address,uint8,(uint64,bool,(address,uint256,uint64)[],bool,(address,(bytes4,address[])[])[]),bytes32)")
        (clempo.abi:abi-selector "authorizeAdminKey(address,uint8,bytes32)")))

(defun rejects-configurable-account-type-p (input)
  "T11..T13: an authorize call naming signature type 3 reverts like the strict decoder."
  (and (fork-if (:since :t11) (fork-if (:since :t14) nil t) nil)
       (>= (length input) 68)
       (member (octets->integer input :end 4) +authorize-selectors+)
       (= (octets->integer input :start 36 :end 68) +configurable-account-type+)))

(defdispatch account-keychain (self input sender)
  (charge-input-cost input)
  (when (rejects-configurable-account-type-p input)
    (revert-data (make-octets 0)))
  (dispatch-calls))

(register-precompile +account-keychain-address+ 'account-keychain :genesis)
