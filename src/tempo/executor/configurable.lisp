(in-package #:clempo.executor)

;;; Configurable account transaction authority (T14; TIP-1107, TIP-1108, TIP-1113): tempo
;;; crates/evm/src/handler/configurable_account/mod.rs. The owner quorums themselves were
;;; verified when the transaction's sender, access key and key-authorization signer were
;;; recovered (clempo.signatures); this is the state they depend on.

(defun txv-simulation-p (v)
  "ExecutionContext::Simulation: an unsigned request (eth_call, eth_estimateGas, ...)."
  (null (txv-tx v)))

(defun configurable-roles (v)
  "TempoAaTx::configurable_roles: the outer role (a direct ConfigurableAccount signature or a
V2 keychain delegating to one), then the inline key authorization's, as parsed
clempo.signatures:configurable-signature structures (absent roles left out)."
  (when (txv-aa-p v)
    (let ((outer (clempo.signatures:configurable-signature (txv-signature v)))
          (raw (txv-key-authorization v)))
      (remove nil (list outer (and raw (clempo.signatures:configurable-signature (second raw))))))))

(defun signed-by-primitive-root-p (v)
  "Every non-AA transaction, and an AA transaction with a bare primitive signature."
  (or (not (txv-aa-p v))
      (member (clempo.signatures:signature-kind (txv-signature v)) '(:secp256k1 :p256 :webauthn))))

(defun account-access-gas (cold)
  "A warm read, plus the cold surcharge when the account had not been accessed."
  (let ((gp (evm:spec-gas evm:*spec*)))
    (if cold (evm:gas-params-cold-account gp) (evm:gas-params-warm-read gp))))

(defun load-checked-account (address)
  "load_account: warm ADDRESS and return (values was-cold invalid-code empty nonce commitment)."
  (let* ((world evm:*world*)
         (cold (not (state:warm-address world address)))
         (account (state:get-account world address))
         (commitment (state:account-commitment account)))
    (values cold
            (not (clempo.configurable-account:valid-account-code-p
                  (octets->integer (state:account-code-hash account)) commitment))
            (state:account-dead-p account)
            (state:account-nonce account)
            commitment)))

(defun registration-event-gas (role)
  "LOG plus the cost of ConfigurableAccountConfigUpdated(address indexed, bytes32, uint64,
uint8, (address,uint8)[]): two topics, four head words, the array length and two words per
owner."
  (+ forks:+log-gas+ (* 2 375)
     (* 8 (+ 160 (* 64 (length (clempo.signatures:configurable-signature-owners role)))))))

(defun validate-configurable-roles (v)
  "validate_configurable_roles: returns (values extra-gas accounts) with ACCOUNTS the role
accounts loaded, in order."
  (let* ((caller (txv-sender v))
         (roles (configurable-roles v))
         (gp (evm:spec-gas evm:*spec*))
         (accounts '())
         (extra 0))
    (when (and roles (forks:spec< :t14))
      (invalid "configurable account: unsupported context"))
    (when (and (forks:spec>= :t14) (txv-aa-p v) (keychain-signature-p (txv-signature v)))
      (multiple-value-bind (cold invalid-code empty nonce commitment) (load-checked-account caller)
        (declare (ignore cold empty nonce))
        (when (and (/= commitment 0) invalid-code)
          (invalid "configurable account: invalid account ~X" caller))))
    (let ((direct (and (txv-aa-p v)
                       (eq (clempo.signatures:signature-kind (txv-signature v)) :configurable-account)
                       (first roles))))
      (when (and direct (/= (clempo.signatures:configurable-signature-account direct) caller))
        (invalid "configurable account: account mismatch, expected ~X, got ~X"
                 caller (clempo.signatures:configurable-signature-account direct))))
    (dolist (role roles)
      (let ((address (clempo.signatures:configurable-signature-account role)))
        (unless (clempo.configurable-account:valid-account-p address)
          (invalid "configurable account: invalid account ~X" address))
        (multiple-value-bind (cold invalid-code empty nonce commitment) (load-checked-account address)
          (when invalid-code (invalid "configurable account: invalid account ~X" address))
          (let ((first (not (member address accounts))))
            (when (and first (/= address caller)) (incf extra (account-access-gas cold)))
            (setf accounts (append accounts (list address)))
            (case (clempo.signatures:configurable-commitment-error role commitment)
              ((nil))
              (:commitment-mismatch
               (invalid "configurable account: configuration commitment mismatch"))
              (t (invalid "configurable account: invalid account ~X" address)))
            (when (zerop commitment)
              (incf extra (clempo.signatures:configurable-derivation-gas role))
              (when first
                ;; TIP-1108 prices the commitment write like account creation, and the
                ;; registration event emitted without metering
                (incf extra (+ (evm:gas-params-new-account gp) (registration-event-gas role)))
                ;; the sender's creation is free only when another TIP-1000 charge pays it
                (let ((paid (and (= address caller)
                                 (or (and (zerop (txv-nonce-key v)) (zerop (txv-nonce v)))
                                     (and (/= 0 (txv-nonce-key v))
                                          (null (first (first (txv-calls v))))
                                          (zerop nonce))))))
                  (when (and empty (not paid))
                    (incf extra (evm:gas-params-new-account gp))))))))))
    (values extra accounts)))

(defun validate-root-keys (v accounts)
  "validate_root_keys (TIP-1113): bare primitive authority of an account with a commitment is
retired, checked for sender, key-authorization signer, fee payer and access key in that order.
Each distinct non-caller account pays one access. Returns (values extra-gas accounts)."
  (let ((caller (txv-sender v)) (extra 0))
    (flet ((check (account role)
             (multiple-value-bind (cold invalid-code empty nonce commitment)
                 (load-checked-account account)
               (declare (ignore invalid-code empty nonce))
               (when (and (/= account caller) (not (member account accounts)))
                 (incf extra (account-access-gas cold)))
               (setf accounts (append accounts (list account)))
               (unless (zerop commitment)
                 (error 'invalid-transaction :tx-index *tx-index* :kind :root-key-retired
                                             :reason (format nil "root key retired: ~(~A~) ~X"
                                                             role account))))))
      (when (and (not (txv-simulation-p v)) (signed-by-primitive-root-p v))
        (check caller :sender))
      (when (txv-aa-p v)
        (let ((raw (txv-key-authorization v)))
          (when (and raw (not (eq (clempo.signatures:signature-kind (second raw))
                                  :configurable-account)))
            (check (handler-case (clempo.signatures:key-authorization-signer raw)
                     (clempo.signatures:signature-error ()
                       (invalid "key authorization signature recovery failed")))
                   :key-authorization-signer)))
        (when (txv-sponsored-p v)
          (check (txv-fee-payer v) :fee-payer))
        (when (and (keychain-signature-p (txv-signature v))
                   (not (eq (clempo.signatures:signature-inner-kind (txv-signature v))
                            :configurable-account))
                   (txv-key-id v))
          (check (txv-key-id v) :access-key))))
    (values extra accounts)))

(defun granted-key-access-gas (v accounts)
  "granted_key_access_gas: the inline grant's key, unless the caller or a role already paid."
  (let ((raw (txv-key-authorization v)))
    (if raw
        (let ((granted (clempo.signatures:key-authorization-key-id
                        (clempo.signatures:parse-key-authorization raw))))
          (if (or (= granted (txv-sender v)) (member granted accounts))
              0
              (account-access-gas (load-checked-account granted))))
        0)))

(defun validate-configurable-state (v)
  "configurable_account::validate_state: the extra intrinsic gas the transaction's
configurable roles and (from T14) retired root keys cost; signals INVALID-TRANSACTION.
Transaction warmth, not the beneficiary's, must be in place first. Tempo L1 reads no zone
commitments."
  (multiple-value-bind (extra accounts) (validate-configurable-roles v)
    (when (forks:spec>= :t14)
      (multiple-value-bind (root-gas accounts) (validate-root-keys v accounts)
        (incf extra root-gas)
        (when (txv-aa-p v) (incf extra (granted-key-access-gas v accounts)))))
    extra))

(defun seed-upgrade-authority (v)
  "TempoTxEnv::seed_upgrade_authority: the transaction authority ConfigurableAccount's
upgradeAccount and updateConfig read. Eligibility 0: an authorization list sits beside it;
1: not signed by a primitive root; 2: signed by one. The directly authorized account is a
direct configurable signer, or in simulation a registered caller signing without a key."
  (let* ((caller (txv-sender v))
         (eligibility (cond ((txv-authorizations v) 0)
                            ((not (signed-by-primitive-root-p v)) 1)
                            (t 2)))
         (direct (cond ((and (txv-aa-p v)
                             (eq (clempo.signatures:signature-kind (txv-signature v))
                                 :configurable-account))
                        (clempo.signatures:configurable-signature-account
                         (clempo.signatures:configurable-signature (txv-signature v))))
                       ((and (txv-simulation-p v)
                             (not (and (txv-aa-p v) (keychain-signature-p (txv-signature v))))
                             (/= 0 (state:account-commitment (state:get-account evm:*world* caller))))
                        caller)
                       (t 0))))
    (clempo.configurable-account:set-authority caller direct eligibility)))

(defun verify-configurable-grant (v)
  "configurable_account::verify for the inline grant's role: a key authorization signed by a
configurable account names its account without recovery (authorizing_account), so its owner
quorum is checked here over the authorization's signature hash. The outer role's quorum was
verified when the sender (or keychain key) was recovered. Simulation skips it."
  (let ((raw (txv-key-authorization v)))
    (when (and raw (not (txv-simulation-p v))
               (eq (clempo.signatures:signature-kind (second raw)) :configurable-account))
      (handler-case
          (clempo.signatures:verify-configurable-approvals
           (clempo.signatures:configurable-signature (second raw))
           (clempo.signatures:key-authorization-hash raw))
        (clempo.signatures:signature-error (c)
          (invalid "configurable account quorum: ~A" (clempo.signatures:signature-error-reason c)))))))

(defun register-commitments (v)
  "register_commitments: each signing configurable account without a commitment is registered
at its initial configuration (ConfigurableAccountConfigUpdated, version 0), unmetered and
outside the batch checkpoint, so it persists when the calls fail."
  (let ((roles (configurable-roles v)))
    (when roles
      (with-protocol-storage ()
        (dolist (role roles)
          (clempo.configurable-account:register
           (clempo.signatures:configurable-signature-account role)
           (clempo.signatures:configurable-signature-salt role)
           (clempo.signatures:configurable-signature-version role)
           (clempo.signatures:configurable-signature-threshold role)
           (clempo.signatures:configurable-signature-owners role)
           (clempo.signatures:configurable-commitment role)))))))

(defun configurable-sender-p (v account)
  "apply_nonce from T14: a registered configurable account carries the 0xef marker and may
still send owner-signed, keychain-signed or simulated transactions."
  (and (forks:spec>= :t14)
       (= (octets->integer (state:account-code-hash account))
          (load-time-value (octets->integer (clempo.keccak:keccak256 (octets-of #xef)))))
       (or (member (clempo.signatures:signature-kind (txv-signature v))
                   '(:configurable-account :keychain-v1 :keychain-v2))
           (txv-simulation-p v))
       (/= 0 (state:account-commitment account))))
