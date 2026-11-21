(in-package #:clempo.tip403)

;;; TIP403Registry (crates/precompiles/src/tip403_registry): whitelist, blacklist and
;;; compound (TIP-1015) transfer policies, TIP-1028 receive policies and TIP-1092
;;; token-to-policy bindings.

(defconstant +reject-all-policy-id+ 0)
(defconstant +allow-all-policy-id+ 1)

;;; PolicyType: WHITELIST 0, BLACKLIST 1, COMPOUND 2; __Invalid (255) for anything else.
(defconstant +whitelist+ 0)
(defconstant +blacklist+ 1)
(defconstant +compound+ 2)
(defconstant +invalid-policy-type+ 255)

;;; BlockedReason.
(defconstant +blocked-none+ 0)
(defconstant +blocked-token-filter+ 1)
(defconstant +blocked-receive-policy+ 2)

;;; RecoveryMode (receive_policy_guard): Originator 0, Receiver 1, ThirdParty 2.
(defenum recovery-mode originator receiver third-party)

(defconstant +recovery-originator+ 0 "RECOVERY_ORIGINATOR: the zero address.")

(defstorable policy-data
  (policy-type u8)
  (admin address))

(defstorable compound-policy-data
  (sender-policy-id u64)
  (recipient-policy-id u64)
  (mint-recipient-policy-id u64))

(defstorable policy-record
  (base policy-data)
  (compound compound-policy-data))

(defstorable receive-policy-config
  (has-receive-policy bool)
  (sender-policy-id u64)
  (sender-policy-type u8)
  (token-filter-id u64)
  (token-filter-type u8)
  (recovery-mode recovery-mode))

(defstorable receive-policy
  (config receive-policy-config)
  (recovery-address address))

(defstorable token-transfer-policy
  (policy-id u64)
  (is-set bool))

(defcontract tip403-registry (:address +tip403-registry-address+)
  (policy-id-counter u64)
  (policy-records (mapping u64 policy-record))
  (policy-set (mapping u64 (mapping address bool)))
  (receive-policies (mapping address receive-policy))
  (token-transfer-policies (mapping address token-transfer-policy)))

(deferror unauthorized "Unauthorized()")
(deferror policy-not-found "PolicyNotFound()")
(deferror policy-not-simple "PolicyNotSimple()")
(deferror invalid-policy-type "InvalidPolicyType()")
(deferror incompatible-policy-type "IncompatiblePolicyType()")
(deferror virtual-address-not-allowed "VirtualAddressNotAllowed()")
(deferror invalid-receive-policy-type "InvalidReceivePolicyType()")
(deferror invalid-recovery-authority "InvalidRecoveryAuthority()")
(deferror invalid-token "InvalidToken()")

(defevent policy-admin-updated "PolicyAdminUpdated(uint64 indexed policyId, address indexed updater, address indexed admin)")
(defevent policy-created "PolicyCreated(uint64 indexed policyId, address indexed updater, uint8 policyType)")
(defevent whitelist-updated "WhitelistUpdated(uint64 indexed policyId, address indexed updater, address indexed account, bool allowed)")
(defevent blacklist-updated "BlacklistUpdated(uint64 indexed policyId, address indexed updater, address indexed account, bool restricted)")
(defevent compound-policy-created "CompoundPolicyCreated(uint64 indexed policyId, address indexed creator, uint64 senderPolicyId, uint64 recipientPolicyId, uint64 mintRecipientPolicyId)")
(defevent receive-policy-updated "ReceivePolicyUpdated(address indexed account, uint64 senderPolicyId, uint64 tokenFilterId, address recoveryAuthority)")

(define-symbol-macro +self+ +tip403-registry-address+)

;;; Helpers.

(defun virtual-address-p (address)
  "TempoAddressExt::is_virtual: bytes 4..14 are all 0xFD."
  (= (ldb (byte 80 48) address) #xfdfdfdfdfdfdfdfdfdfd))

(defun policy-type-of (raw)
  "PolicyType::try_from(u8) as stored: the variant, or NIL."
  (and (<= raw +compound+) raw))

(defun decode-policy-type (data)
  "PolicyData::policy_type."
  (let ((ty (policy-type-of (policy-data-policy-type data))))
    (fork-if (:since :t2)
        (or ty (revert 'invalid-policy-type))
        (if (and ty (/= ty +compound+)) ty (panic +panic-under-overflow+)))))

(defun simple-data-p (data)
  (<= (policy-data-policy-type data) +blacklist+))

(defun compound-data-p (data)
  (= (policy-data-policy-type data) +compound+))

(defun default-data-p (data)
  (and (zerop (policy-data-policy-type data)) (zerop (policy-data-admin data))))

(defun builtin-authorization (policy-id)
  "(values authorized builtin-p)."
  (case policy-id
    (1 (values t t))
    (0 (values nil t))
    (t (values nil nil))))

(defun builtin-p (policy-id) (<= policy-id +allow-all-policy-id+))

(defun read-policy-id-counter ()
  "policy_id_counter: the stored counter, at least 2."
  (max 2 (read (policy-id-counter +self+))))

(defun base-place (policy-id) (field (policy-records +self+ policy-id) 'base))
(defun compound-place (policy-id) (field (policy-records +self+ policy-id) 'compound))
(defun config-place (account) (field (receive-policies +self+ account) 'config))
(defun recovery-place (account) (field (receive-policies +self+ account) 'recovery-address))

(defun get-policy-data (policy-id)
  "get_policy_data: from T2, PolicyNotFound for an unset record past the counter."
  (let ((data (read (base-place policy-id))))
    (when (and (fork-if (:since :t2) t nil)
               (default-data-p data)
               (>= policy-id (read-policy-id-counter)))
      (revert 'policy-not-found))
    data))

(defun next-policy-id ()
  "Reads the counter, writes counter+1, returns the new id."
  (let ((id (read-policy-id-counter)))
    (write (policy-id-counter +self+)
           (if (>= id u64-max) (panic +panic-under-overflow+) (1+ id)))
    id))

(defun ensure-simple-type (policy-type)
  "PolicyTypeExt::ensure_is_simple."
  (cond ((<= policy-type +blacklist+) policy-type)
        (t (fork-if (:since :t2)
               (revert 'incompatible-policy-type)
               +invalid-policy-type+))))

(defun event-policy-type (raw)
  "policy_type.try_into().unwrap_or(__Invalid) as the event's uint8."
  (or (policy-type-of raw) +invalid-policy-type+))

;;; Reads.

(defun policy-exists-p (policy-id)
  "TIP403Registry::policy_exists."
  (or (builtin-p policy-id)
      (< policy-id (read-policy-id-counter))))

(defun registered-token-transfer-policy-id (token)
  "The registry binding of TOKEN's transfer policy, or NIL when unset."
  (let ((binding (read (token-transfer-policies +self+ token))))
    (and (token-transfer-policy-is-set binding)
         (token-transfer-policy-policy-id binding))))

(defun set-token-transfer-policy (token policy-id)
  "Bind TOKEN to POLICY-ID (unchecked)."
  (write (token-transfer-policies +self+ token)
         (make-token-transfer-policy :policy-id policy-id :is-set t)))

(defun token-transfer-policy-id (token)
  "(values set-p policy-id)."
  (unless (funcall 'clempo.tip20-factory:tip20-p token) (revert 'invalid-token))
  (let ((registered (registered-token-transfer-policy-id token)))
    (values (and registered t)
            (or registered (funcall 'clempo.tip20:legacy-transfer-policy-id token)))))

(defun migrate-transfer-policy-ids (tokens)
  (let ((migrated 0))
    (dolist (token tokens migrated)
      (unless (or (not (funcall 'clempo.tip20-factory:tip20-p token))
                  (registered-token-transfer-policy-id token))
        (let ((policy-id (funcall 'clempo.tip20:legacy-transfer-policy-id token)))
          (set-token-transfer-policy token policy-id)
          (funcall 'clempo.tip20:delete-legacy-transfer-policy-id token)
          (incf migrated))))))

(defun policy-data* (policy-id)
  "policyData: (values type admin)."
  (fork-if (:since :t2)
      (when (builtin-p policy-id)
        (return-from policy-data* (values policy-id 0)))
      (unless (policy-exists-p policy-id) (revert 'policy-not-found)))
  (let ((data (get-policy-data policy-id)))
    (values (decode-policy-type data) (policy-data-admin data))))

(defun compound-policy-data* (policy-id)
  (let ((data (get-policy-data policy-id)))
    (unless (compound-data-p data)
      (if (policy-exists-p policy-id)
          (revert 'incompatible-policy-type)
          (revert 'policy-not-found)))
    (let ((c (read (compound-place policy-id))))
      (values (compound-policy-data-sender-policy-id c)
              (compound-policy-data-recipient-policy-id c)
              (compound-policy-data-mint-recipient-policy-id c)))))

(defun receive-policy-type (raw)
  (or (policy-type-of raw) (revert 'invalid-receive-policy-type)))

(defun receive-policy-recovery (account mode)
  (ecase mode
    (0 +recovery-originator+)
    (1 account)
    (2 (read (recovery-place account)))))

(defun receive-policy* (account)
  (let ((c (read (config-place account))))
    (values (receive-policy-config-has-receive-policy c)
            (receive-policy-config-sender-policy-id c)
            (receive-policy-type (receive-policy-config-sender-policy-type c))
            (receive-policy-config-token-filter-id c)
            (receive-policy-type (receive-policy-config-token-filter-type c))
            (receive-policy-recovery account (receive-policy-config-recovery-mode c)))))

;;; Authorization.

(defun authorized-simple-inner (policy-id user data)
  ;; policy_set is read before the type check (pre-T1 gas order).
  (let ((in-set (read (at (policy-set +self+ policy-id) user))))
    (ecase (decode-policy-type data)
      (0 in-set)
      (1 (not in-set))
      (2 (revert 'incompatible-policy-type)))))

(defun authorized-simple-p (policy-id user &optional cache)
  (multiple-value-bind (auth builtin) (builtin-authorization policy-id)
    (if builtin
        auth
        (authorized-simple-inner policy-id user (or cache (get-policy-data policy-id))))))

(defun auth-role (role)
  "AuthRole::sender()/recipient()/mint_recipient(): directional roles are :TRANSFER before T2."
  (if (eq role :transfer) role (fork-if (:since :t2) role :transfer)))

(defun authorized-as-p (policy-id user role)
  "TIP403Registry::is_authorized_as with the hardfork-aware ROLE (:transfer :sender
:recipient :mint-recipient)."
  (let ((role (auth-role role)))
    (when (and (fork-if (:since :t6) t nil) (= user +receive-policy-guard-address+))
      (return-from authorized-as-p t))
    (multiple-value-bind (auth builtin) (builtin-authorization policy-id)
      (when builtin (return-from authorized-as-p auth)))
    (let ((data (get-policy-data policy-id)))
      (if (compound-data-p data)
          (let ((c (read (compound-place policy-id))))
            (ecase role
              (:sender (authorized-simple-p (compound-policy-data-sender-policy-id c) user))
              (:recipient (authorized-simple-p (compound-policy-data-recipient-policy-id c) user))
              (:mint-recipient
               (authorized-simple-p (compound-policy-data-mint-recipient-policy-id c) user))
              (:transfer
               (let ((sender-auth (authorized-simple-p (compound-policy-data-sender-policy-id c) user)))
                 (if (and (not sender-auth) (fork-if (:since :t2) t nil))
                     nil
                     (let ((recipient-auth
                             (authorized-simple-p (compound-policy-data-recipient-policy-id c) user)))
                       (and sender-auth recipient-auth)))))))
          (authorized-simple-inner policy-id user data)))))

(defun authorized-p (policy-id user)
  "isAuthorized: the symmetric :TRANSFER role."
  (authorized-as-p policy-id user :transfer))

(defun policy-lookup-error-p (condition)
  "is_policy_lookup_error: InvalidPolicyType/PolicyNotFound from T2, Panic(0x11) before."
  (and (typep condition 'precompile-revert)
       (let ((data (precompile-revert-data condition)))
         (fork-if (:since :t2)
             (or (equalp data (error-data 'invalid-policy-type))
                 (equalp data (error-data 'policy-not-found)))
             (equalp data (clempo.abi:panic-data +panic-under-overflow+))))))

(defun check-receive-policy (token sender receiver)
  "check_receive_policy: NIL when authorized, else (values blocked-reason recovery-address)."
  (let ((c (read (config-place receiver))))
    (cond ((not (receive-policy-config-has-receive-policy c)) nil)
          ((not (authorized-simple-p
                 (receive-policy-config-token-filter-id c) token
                 (make-policy-data :policy-type (receive-policy-config-token-filter-type c))))
           (values +blocked-token-filter+
                   (receive-policy-recovery receiver (receive-policy-config-recovery-mode c))))
          ((not (authorized-simple-p
                 (receive-policy-config-sender-policy-id c) sender
                 (make-policy-data :policy-type (receive-policy-config-sender-policy-type c))))
           (values +blocked-receive-policy+
                   (receive-policy-recovery receiver (receive-policy-config-recovery-mode c))))
          (t nil))))

(defun validate-receive-policy (token sender receiver)
  "(values authorized blocked-reason)."
  (let ((reason (or (check-receive-policy token sender receiver) +blocked-none+)))
    (values (= reason +blocked-none+) reason)))

;;; Mutations.

(defun create-policy (sender policy-type admin)
  "createPolicy: returns the new policy id."
  (let* ((ty (ensure-simple-type policy-type))
         (id (next-policy-id)))
    (write (base-place id) (make-policy-data :policy-type ty :admin admin))
    (emit +self+ 'policy-created id sender (event-policy-type ty))
    (emit +self+ 'policy-admin-updated id sender admin)
    id))

(defun create-policy-with-accounts (sender admin policy-type accounts)
  (let ((ty (ensure-simple-type policy-type)))
    (when (fork-if (:since :t3) t nil)
      (dolist (a accounts)
        (when (virtual-address-p a) (revert 'virtual-address-not-allowed))))
    (let ((id (next-policy-id)))
      (write (base-place id) (make-policy-data :policy-type ty :admin admin))
      (dolist (a accounts)
        (write (at (policy-set +self+ id) a) t)
        (case policy-type
          (0 (emit +self+ 'whitelist-updated id sender a t))
          (1 (emit +self+ 'blacklist-updated id sender a t))
          (t (revert 'incompatible-policy-type))))
      (emit +self+ 'policy-created id sender (event-policy-type ty))
      (emit +self+ 'policy-admin-updated id sender admin)
      id)))

(defun set-policy-admin (sender policy-id admin)
  (let ((data (get-policy-data policy-id)))
    (unless (= (policy-data-admin data) sender) (revert 'unauthorized))
    (write (base-place policy-id)
           (make-policy-data :policy-type (policy-data-policy-type data) :admin admin))
    (emit +self+ 'policy-admin-updated policy-id sender admin)
    (values)))

(defun modify-policy-list (sender policy-id account value want event)
  (when (and (fork-if (:since :t3) t nil) (virtual-address-p account))
    (revert 'virtual-address-not-allowed))
  (let ((data (get-policy-data policy-id)))
    (unless (= (policy-data-admin data) sender) (revert 'unauthorized))
    (unless (= (decode-policy-type data) want) (revert 'incompatible-policy-type))
    (write (at (policy-set +self+ policy-id) account) value)
    (emit +self+ event policy-id sender account value)
    (values)))

(defun modify-policy-whitelist (sender policy-id account allowed)
  (modify-policy-list sender policy-id account allowed +whitelist+ 'whitelist-updated))

(defun modify-policy-blacklist (sender policy-id account restricted)
  "modifyPolicyBlacklist."
  (modify-policy-list sender policy-id account restricted +blacklist+ 'blacklist-updated))

(defun validate-simple-policy (policy-id)
  (unless (builtin-p policy-id)
    (when (>= policy-id (read-policy-id-counter)) (revert 'policy-not-found))
    (unless (simple-data-p (get-policy-data policy-id)) (revert 'policy-not-simple))))

(defun create-compound-policy (sender sender-id recipient-id mint-recipient-id)
  (validate-simple-policy sender-id)
  (validate-simple-policy recipient-id)
  (validate-simple-policy mint-recipient-id)
  (let ((id (next-policy-id)))
    (write (policy-records +self+ id)
           (make-policy-record
            :base (make-policy-data :policy-type +compound+ :admin 0)
            :compound (make-compound-policy-data :sender-policy-id sender-id
                                                 :recipient-policy-id recipient-id
                                                 :mint-recipient-policy-id mint-recipient-id)))
    (emit +self+ 'compound-policy-created id sender sender-id recipient-id mint-recipient-id)
    id))

(defun validate-receive-policy-id (policy-id)
  "The policy type to cache for a receive policy."
  (cond ((builtin-p policy-id) policy-id)
        ((>= policy-id (read-policy-id-counter)) (revert 'policy-not-found))
        (t (let ((data (get-policy-data policy-id)))
             (unless (simple-data-p data) (revert 'invalid-receive-policy-type))
             (policy-data-policy-type data)))))

(defun set-receive-policy (sender sender-policy-id token-filter-id recovery-authority)
  "setReceivePolicy (TIP-1028) for SENDER."
  (when (virtual-address-p sender) (revert 'virtual-address-not-allowed))
  (multiple-value-bind (mode stored)
      (cond ((= recovery-authority +recovery-originator+) (values 0 0))
            ((= recovery-authority sender) (values 1 0))
            (t (values 2 recovery-authority)))
    (when (or (precompile-address-p recovery-authority) (virtual-address-p recovery-authority))
      (revert 'invalid-recovery-authority))
    (let ((sender-type (validate-receive-policy-id sender-policy-id))
          (filter-type (validate-receive-policy-id token-filter-id)))
      (write (config-place sender)
             (make-receive-policy-config :has-receive-policy t
                                         :sender-policy-id sender-policy-id
                                         :sender-policy-type sender-type
                                         :token-filter-id token-filter-id
                                         :token-filter-type filter-type
                                         :recovery-mode mode))
      (write (recovery-place sender) stored)
      (emit +self+ 'receive-policy-updated sender sender-policy-id token-filter-id
            recovery-authority)
      (values))))

;;; ABI.

(defcall tip403-registry policy-id-counter* ()
    (:sol "policyIdCounter()" :returns "uint64")
  (read-policy-id-counter))

(defcall tip403-registry policy-exists* (policy-id)
    (:sol "policyExists(uint64 policyId)" :returns "bool")
  (policy-exists-p policy-id))

(defcall tip403-registry token-transfer-policy-id* (token)
    (:sol "tokenTransferPolicyId(address token)" :returns ("bool" "uint64") :since :t9)
  (token-transfer-policy-id token))

(defcall tip403-registry policy-data** (policy-id)
    (:sol "policyData(uint64 policyId)" :returns ("enum:3" "address"))
  (policy-data* policy-id))

(defcall tip403-registry is-authorized (policy-id user)
    (:sol "isAuthorized(uint64 policyId, address user)" :returns "bool")
  (authorized-as-p policy-id user :transfer))

(defcall tip403-registry is-authorized-sender (policy-id user)
    (:sol "isAuthorizedSender(uint64 policyId, address user)" :returns "bool" :since :t2)
  (authorized-as-p policy-id user :sender))

(defcall tip403-registry is-authorized-recipient (policy-id user)
    (:sol "isAuthorizedRecipient(uint64 policyId, address user)" :returns "bool" :since :t2)
  (authorized-as-p policy-id user :recipient))

(defcall tip403-registry is-authorized-mint-recipient (policy-id user)
    (:sol "isAuthorizedMintRecipient(uint64 policyId, address user)" :returns "bool" :since :t2)
  (authorized-as-p policy-id user :mint-recipient))

(defcall tip403-registry compound-policy-data** (policy-id)
    (:sol "compoundPolicyData(uint64 policyId)" :returns ("uint64" "uint64" "uint64") :since :t2)
  (compound-policy-data* policy-id))

(defcall tip403-registry receive-policy** (account)
    (:sol "receivePolicy(address account)"
     :returns ("bool" "uint64" "enum:3" "uint64" "enum:3" "address") :since :t6)
  (receive-policy* account))

(defcall tip403-registry validate-receive-policy* (token from receiver)
    (:sol "validateReceivePolicy(address token, address sender, address receiver)"
     :returns ("bool" "enum:3") :since :t6)
  (validate-receive-policy token from receiver))

(defcall tip403-registry set-receive-policy* (sender-policy-id token-filter-id recovery-authority)
    (:sol "setReceivePolicy(uint64 senderPolicyId, uint64 tokenFilterId, address recoveryAuthority)"
     :mutates t :since :t6)
  (set-receive-policy sender sender-policy-id token-filter-id recovery-authority))

(defcall tip403-registry migrate-transfer-policy-ids* (tokens)
    (:sol "migrateTransferPolicyIds(address[] tokens)" :returns "uint256" :mutates t :since :t9)
  (migrate-transfer-policy-ids tokens))

(defcall tip403-registry create-policy* (admin policy-type)
    (:sol "createPolicy(address admin, enum:3 policyType)" :returns "uint64" :mutates t)
  (create-policy sender policy-type admin))

(defcall tip403-registry create-policy-with-accounts* (admin policy-type accounts)
    (:sol "createPolicyWithAccounts(address admin, enum:3 policyType, address[] accounts)"
     :returns "uint64" :mutates t)
  (create-policy-with-accounts sender admin policy-type accounts))

(defcall tip403-registry set-policy-admin* (policy-id admin)
    (:sol "setPolicyAdmin(uint64 policyId, address admin)" :mutates t)
  (set-policy-admin sender policy-id admin))

(defcall tip403-registry modify-policy-whitelist* (policy-id account allowed)
    (:sol "modifyPolicyWhitelist(uint64 policyId, address account, bool allowed)" :mutates t)
  (modify-policy-whitelist sender policy-id account allowed))

(defcall tip403-registry modify-policy-blacklist* (policy-id account restricted)
    (:sol "modifyPolicyBlacklist(uint64 policyId, address account, bool restricted)" :mutates t)
  (modify-policy-blacklist sender policy-id account restricted))

(defcall tip403-registry create-compound-policy* (sender-id recipient-id mint-recipient-id)
    (:sol "createCompoundPolicy(uint64 senderPolicyId, uint64 recipientPolicyId, uint64 mintRecipientPolicyId)"
     :returns "uint64" :mutates t :since :t2)
  (create-compound-policy sender sender-id recipient-id mint-recipient-id))

(defdispatch tip403-registry (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +tip403-registry-address+ 'tip403-registry :genesis)
