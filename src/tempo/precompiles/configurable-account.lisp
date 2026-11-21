(in-package #:clempo.configurable-account)

;;; ConfigurableAccount (crates/precompiles/src/configurable_account, T14; TIP-1109..1114):
;;; owner/threshold configurations committed in the account's configuration commitment, the
;;; deriveAccount/updateConfig/upgradeAccount calls, and the stateless marker-code shim.
;;; The configuration rules (tempo_primitives::transaction::configurable_account) live here
;;; too: the signature verifier and the keychain use them.

(defcontract configurable-account (:address +configurable-account-address+)
  (tx-origin address :doc "Transient.")
  (directly-authorized-account address :doc "Transient.")
  (upgrade-eligibility u8 :doc "Transient UpgradeEligibility: 0 HasAuthorizationList,
1 NotPrimitiveRoot, 2 PrimitiveRoot."))

(deferror invalid-account "InvalidAccount()")
(deferror invalid-config "InvalidConfig()")
(deferror invalid-threshold "InvalidThreshold()")
(deferror invalid-configurable-account-owner "InvalidConfigurableAccountOwner()")
(deferror invalid-weight "InvalidWeight()")
(deferror too-many-owners "TooManyOwners()")
(deferror duplicate-owner "DuplicateOwner()")
(deferror invalid-owner-order "InvalidOwnerOrder()")
(deferror unauthorized-configurable-account-caller "UnauthorizedConfigurableAccountCaller()")
(deferror primitive-root-required "PrimitiveRootRequired()")
(deferror upgrade-must-be-direct-call "UpgradeMustBeDirectCall()")
(deferror account-already-configurable "AccountAlreadyConfigurable()")
(deferror account-has-code "AccountHasCode()")

(defevent configurable-account-config-updated
  "ConfigurableAccountConfigUpdated(address indexed account, bytes32 salt, uint64 version, uint8 threshold, (address,uint8)[] owners)")
(defevent account-upgraded
  "AccountUpgraded(address indexed account, bytes32 indexed commitment, bytes32 salt, uint8 threshold, (address,uint8)[] owners)")

(defconstant +self+ +configurable-account-address+)
(defconstant +max-owners+ 48)
(defconstant +max-signatures+ 8)
(define-constant +account-domain+ (string->octets "tempo:configurable-account:account"))
(define-constant +config-domain+ (string->octets "tempo:configurable-account:config"))
(define-constant +upgrade-domain+ (string->octets "tempo:configurable-account:upgrade"))
(define-constant +account-code-hash+ (clempo.keccak:keccak256-integer (octets-of #xef))
  "CONFIGURABLE_ACCOUNT_CODE_HASH: keccak256(0xef), the protocol-installed marker.")

;;; Namespace and code rules.

(defun virtual-address-p (address)
  "TIP-1022: bytes 4..14 are ten 0xFD bytes."
  (= (ldb (byte 80 48) address) #xfdfdfdfdfdfdfdfdfdfd))

(defun zone-portal-p (address)
  (and (= (ash address -64) #x5ad000000000000000000000) (/= 0 (ldb (byte 64 0) address))))

(defun ethereum-precompile-p (address)
  "The Osaka precompile set: 0x01..0x11 and P256VERIFY at 0x100."
  (or (<= 1 address #x11) (= address #x100)))

(defun valid-account-p (address)
  "is_valid_native_account: not zero, virtual, a Tempo or Ethereum precompile, or a zone portal."
  (not (or (zerop address) (virtual-address-p address) (precompile-address-p address)
           (ethereum-precompile-p address) (zone-portal-p address))))

(defun valid-account-code-p (code-hash commitment)
  "valid_account_code: no code without a commitment, the marker with one."
  (= code-hash (if (zerop commitment) +keccak-empty+ +account-code-hash+)))

;;; Configurations: SALT (b256), VERSION (u64), THRESHOLD (u8), OWNERS ((owner weight)...).

(defstruct (config (:constructor make-config (salt version threshold owners)))
  salt version threshold owners)

(defun config-error (config &optional account)
  "ConfigurableAccountConfig::validate_inner: NIL, or the error keyword."
  (let ((owners (config-owners config)))
    (cond ((null owners) :empty-owners)
          ((> (length owners) +max-owners+) :too-many-owners)
          ((zerop (config-threshold config)) :zero-threshold)
          ((loop for (owner) in owners
                 thereis (cond ((zerop owner) :zero-owner)
                               ((and (zerop (config-version config)) account (= account owner))
                                :account-is-owner))))
          ((some (lambda (o) (zerop (second o))) owners) :zero-weight)
          ((loop for (a b) on owners while b thereis (>= (first a) (first b)))
           (if (/= (length (remove-duplicates owners :key #'first)) (length owners))
               :duplicate-owner
               :owners-not-ascending))
          ((> (reduce #'+ owners :key #'second) 255) :total-weight-exceeds-max)
          ((> (config-threshold config)
              (reduce #'+ (subseq (sort (mapcar #'second owners) #'>) 0
                                  (min +max-signatures+ (length owners)))))
           :threshold-exceeds-weight))))

(defun config-error-revert (error)
  "From<ConfigurableAccountConfigError> for TempoPrecompileError."
  (revert (ecase error
            ((:empty-owners :zero-owner :account-is-owner) 'invalid-configurable-account-owner)
            (:too-many-owners 'too-many-owners)
            ((:zero-threshold :threshold-exceeds-weight) 'invalid-threshold)
            ((:zero-weight :total-weight-exceeds-max) 'invalid-weight)
            (:duplicate-owner 'duplicate-owner)
            (:owners-not-ascending 'invalid-owner-order)
            (:derived-account-zero 'invalid-account))))

(defun validate-config (config &optional account)
  (let ((error (config-error config account)))
    (when error (config-error-revert error))))

(defun owner-set-octets (config)
  (let ((owners (config-owners config)))
    (apply #'octets-concat
           (octets-of (config-threshold config) (length owners))
           (loop for (owner weight) in owners
                 collect (integer->octets owner 20) collect (octets-of weight)))))

(defun account-preimage (config)
  (octets-concat +account-domain+ (integer->octets (config-salt config) 32)
                 (owner-set-octets config)))

(defun commitment-preimage (config)
  "Fails with InvalidConfig when the owner count cannot be encoded."
  (when (> (length (config-owners config)) +max-owners+) (revert 'invalid-config))
  (octets-concat +config-domain+ (integer->octets (config-salt config) 32)
                 (integer->octets (config-version config) 8) (owner-set-octets config)))

(defun derivation-gas (config) (keccak-gas (length (account-preimage config))))

(defun config-account-address (config)
  (ldb (byte 160 0) (clempo.keccak:keccak256-integer (account-preimage config))))

(defun config-owner-weight (config owner)
  (second (assoc owner (config-owners config))))

(defun abi-owners (owners) (mapcar (lambda (o) (list (first o) (second o))) owners))

;;; The precompile.

(defun set-authority (origin directly-authorized upgrade-eligibility)
  "ConfigurableAccount::set_authority (executor): seed the transaction's authority.
UPGRADE-ELIGIBILITY: 0 HasAuthorizationList, 1 NotPrimitiveRoot, 2 PrimitiveRoot."
  (t-write (tx-origin +self+) origin)
  (t-write (directly-authorized-account +self+) directly-authorized)
  (t-write (upgrade-eligibility +self+) upgrade-eligibility))

(defun register (account salt version threshold owners commitment)
  "ConfigurableAccount::register (executor, intrinsic gas): first-use registration."
  (unless (zerop (config-commitment account)) (return-from register nil))
  (set-config-commitment account commitment :intrinsic)
  (emit +self+ 'configurable-account-config-updated account salt version threshold
        (abi-owners owners)))

(defcall configurable-account derive-account (salt threshold owners)
    (:sol "deriveAccount(bytes32 salt, uint8 threshold, (address,uint8)[] owners)" :returns "address")
  (let ((config (make-config salt 0 threshold owners)))
    (validate-config config)
    (deduct-gas (derivation-gas config))
    (let ((account (config-account-address config)))
      (when (zerop account) (config-error-revert :derived-account-zero))
      (when (config-owner-weight config account) (config-error-revert :account-is-owner))
      (unless (valid-account-p account) (revert 'invalid-account))
      account)))

(defcall configurable-account get-config-commitment (account)
    (:sol "getConfigCommitment(address account)" :returns "bytes32")
  (config-commitment account))

(defcall configurable-account update-config (current threshold owners)
    (:sol "updateConfig((bytes32,uint64,uint8,(address,uint8)[]) current, uint8 threshold, (address,uint8)[] owners)"
     :mutates t)
  (when (or (zerop sender) (/= (t-read (directly-authorized-account +self+)) sender))
    (revert 'unauthorized-configurable-account-caller))
  (destructuring-bind (salt version current-threshold current-owners) current
    (let* ((current (make-config salt version current-threshold current-owners))
           (stored (config-commitment sender))
           (hash (keccak (commitment-preimage current))))
      (unless (= stored hash) (revert 'invalid-config))
      (when (= version u64-max) (revert 'invalid-config))
      (let ((next (make-config salt (1+ version) threshold owners)))
        (validate-config next sender)
        (let ((hash (keccak (commitment-preimage next))))
          (when (zerop hash) (revert 'invalid-config))
          (set-config-commitment sender hash :precompile)
          (emit +self+ 'configurable-account-config-updated sender salt (1+ version) threshold
                (abi-owners owners))))))
  (values))

(defcall configurable-account upgrade-account (threshold owners)
    (:sol "upgradeAccount(uint8 threshold, (address,uint8)[] owners)" :returns "bytes32"
     :mutates t)
  (unless (= (t-read (tx-origin +self+)) sender) (revert 'upgrade-must-be-direct-call))
  (case (t-read (upgrade-eligibility +self+))
    (0 (revert 'upgrade-must-be-direct-call))
    (1 (revert 'primitive-root-required)))
  (let* ((info (account sender))
         (commitment (if info (account-info-commitment info) 0)))
    (when (and (account-has-code-p info)
               (not (valid-account-code-p (account-info-code-hash info) commitment)))
      (revert 'account-has-code))
    (unless (zerop commitment) (revert 'account-already-configurable)))
  (let* ((salt (keccak (octets-concat +upgrade-domain+ (integer->octets sender 20))))
         (upgraded (make-config salt 1 threshold owners)))
    (when (config-error upgraded sender) (revert 'invalid-config))
    (let ((hash (keccak (handler-case (commitment-preimage upgraded)
                          (precompile-revert () (revert 'invalid-config))))))
      (when (zerop hash) (revert 'invalid-config))
      (set-config-commitment sender hash :precompile)
      (emit +self+ 'account-upgraded sender hash salt threshold (abi-owners owners))
      hash)))

(defdispatch configurable-account (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +configurable-account-address+ 'configurable-account :t14)

;;; The marker-code shim (configurable_account/shim.rs): what a call to an account carrying
;;; the 0xef marker runs.

(define-constant +is-valid-signature-selector+ (clempo.abi:abi-selector "isValidSignature(bytes32,bytes)"))
(define-constant +on-erc721-received-selector+
  (clempo.abi:abi-selector "onERC721Received(address,address,uint256,bytes)"))

(defun shim-call (account input)
  "shim::call: the output octets of a call to configurable ACCOUNT with INPUT; reverts
signal precompile-revert."
  (charge-input-cost input)
  (when (zerop (length input)) (return-from shim-call (make-octets 0)))
  (flet ((decode (types)
           (handler-case (clempo.abi:abi-decode-call types input :validate t)
             (clempo.abi:abi-decode-error () (revert-data (make-octets 0))))))
    (let ((selector (and (>= (length input) 4) (octets->integer input :end 4))))
      (cond
        ((eql selector +is-valid-signature-selector+)
         (destructuring-bind (hash signature) (decode '((:bytes-n 32) :bytes))
           (deduct-gas (input-cost *spec* 32))
           (let ((valid (clempo.signature-verifier:verify-configurable-account account hash signature)))
             (clempo.abi:abi-encode '((:bytes-n 4))
                                    (list (if valid +is-valid-signature-selector+ #xffffffff))))))
        ((eql selector +on-erc721-received-selector+)
         (decode '(:address :address (:uint 256) :bytes))
         (clempo.abi:abi-encode '((:bytes-n 4)) (list +on-erc721-received-selector+)))
        (t (revert-data (make-octets 0)))))))
