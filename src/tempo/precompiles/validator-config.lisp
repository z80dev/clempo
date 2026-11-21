(in-package #:clempo.validator-config)

;;; ValidatorConfig V1 (crates/precompiles/src/validator_config): the owner-managed
;;; consensus validator registry used before the V2 migration. Every selector is scheduled
;;; until T15 (V1 then answers UnknownFunctionSelector).

(defstorable validator
  (public-key b256)                     ; zero => the validator does not exist
  (active bool)
  (index u64)
  (validator-address address)
  (inbound-address string)
  (outbound-address string))

(defcontract validator-config (:address +validator-config-address+ :prefix "S")
  (owner address)
  (validators-array (vec address))
  (validators (mapping address validator))
  (next-dkg-ceremony u64))

(deferror unauthorized "Unauthorized()")
(deferror validator-already-exists "ValidatorAlreadyExists()")
(deferror validator-not-found "ValidatorNotFound()")
(deferror invalid-public-key "InvalidPublicKey()")
(deferror not-host-port "NotHostPort(string field, string input, string backtrace)")
(deferror not-ip-port "NotIpPort(string field, string input, string backtrace)")

(defconstant +self+ +validator-config-address+)

(defun validator-abi (v &optional (address (validator-validator-address v)))
  "IValidatorConfig::Validator as an ABI tuple list."
  (list (validator-public-key v) (validator-active v) (validator-index v) address
        (validator-inbound-address v) (validator-outbound-address v)))

;;; Reads.

(defun initialize (owner)
  "ValidatorConfig::initialize: mark the account initialised (code 0xef), then set OWNER."
  (set-code +self+ (octets-of #xef))
  (write (s-owner +self+) owner)
  nil)

(defun owner ()
  (read (s-owner +self+)))

(defun check-owner (caller)
  (unless (= (owner) caller) (revert 'unauthorized)))

(defun validator-count ()
  "The number of registered validators (one SLOAD)."
  (vec-length (s-validators-array +self+)))

(defun validators-array (index)
  "The validator address at INDEX; Panic(ArrayOutOfBounds) past the end."
  (let ((elem (vec-at (s-validators-array +self+) index)))
    (unless elem (panic +panic-array-oob+))
    (read elem)))

(defun validators (address)
  "The record of ADDRESS as (public-key active index validator-address inbound-address
outbound-address); all zero when absent."
  (validator-abi (read (s-validators +self+ address))))

(defun validator-exists-p (address)
  (/= 0 (validator-public-key (read (s-validators +self+ address)))))

(defun get-validators ()
  "Every validator in index order, each as VALIDATORS returns it; the address comes from
the array."
  (let ((count (validator-count)))
    (loop for i from 0 below count
          collect (let ((address (read (vec-element (s-validators-array +self+) i))))
                    (validator-abi (read (s-validators +self+ address)) address)))))

(defun get-next-full-dkg-ceremony ()
  "The epoch whose fresh DKG ceremony feeds epoch N+1's polynomial."
  (read (s-next-dkg-ceremony +self+)))

;;; Mutations.

(defun check-ip-ports (inbound outbound)
  "ensure_address_is_ip_port on both addresses; the error detail is the Display text from T2
and the legacy Debug text before."
  (let ((detail (fork-if (:since :t2)
                  "input was not of the form `<ip>:<port>`"
                  "Parse(AddrParseError(Socket))")))
    (unless (clempo.validator-config-v2::parse-ip-port inbound)
      (revert 'not-host-port "inboundAddress" inbound detail))
    (unless (clempo.validator-config-v2::parse-ip-port outbound)
      (revert 'not-ip-port "outboundAddress" outbound detail))))

(defun change-owner (sender new-owner)
  (check-owner sender)
  (write (s-owner +self+) new-owner)
  nil)

(defun add-validator (sender new-validator-address public-key active inbound-address outbound-address)
  "ValidatorConfig::add_validator (owner only)."
  (when (zerop public-key) (revert 'invalid-public-key))
  (check-owner sender)
  (when (validator-exists-p new-validator-address) (revert 'validator-already-exists))
  (check-ip-ports inbound-address outbound-address)
  (let ((count (validator-count)))
    (write (s-validators +self+ new-validator-address)
           (make-validator :public-key public-key :active active :index count
                           :validator-address new-validator-address
                           :inbound-address inbound-address :outbound-address outbound-address)))
  (vec-push (s-validators-array +self+) new-validator-address)
  nil)

(defun update-validator (sender new-validator-address public-key inbound-address outbound-address)
  "ValidatorConfig::update_validator: SENDER (a validator) rewrites its record, optionally
rotating it to NEW-VALIDATOR-ADDRESS."
  (when (zerop public-key) (revert 'invalid-public-key))
  (unless (validator-exists-p sender) (revert 'validator-not-found))
  (let ((old (read (s-validators +self+ sender))))
    (unless (= new-validator-address sender)
      (when (validator-exists-p new-validator-address) (revert 'validator-already-exists))
      (write (vec-element (s-validators-array +self+) (validator-index old)) new-validator-address)
      (delete (s-validators +self+ sender)))
    (check-ip-ports inbound-address outbound-address)
    (write (s-validators +self+ new-validator-address)
           (make-validator :public-key public-key :active (validator-active old)
                           :index (validator-index old)
                           :validator-address new-validator-address
                           :inbound-address inbound-address :outbound-address outbound-address)))
  nil)

(defun set-validator-active (address active)
  (let ((v (read (s-validators +self+ address))))
    (setf (validator-active v) active)
    (write (s-validators +self+ address) v)))

(defun change-validator-status (sender validator active)
  "ValidatorConfig::change_validator_status (owner only)."
  (check-owner sender)
  (unless (validator-exists-p validator) (revert 'validator-not-found))
  (set-validator-active validator active)
  nil)

(defun change-validator-status-by-index (sender index active)
  "ValidatorConfig::change_validator_status_by_index (owner only, T1+)."
  (check-owner sender)
  (let ((elem (vec-at (s-validators-array +self+) index)))
    (unless elem (revert 'validator-not-found))
    (set-validator-active (read elem) active))
  nil)

(defun set-next-full-dkg-ceremony (sender epoch)
  (check-owner sender)
  (write (s-next-dkg-ceremony +self+) epoch)
  nil)

;;; Dispatch (validator_config/dispatch.rs).

(defcall validator-config owner* ()
    (:sol "owner()" :returns "address" :until :t15)
  (owner))

(defcall validator-config get-validators* ()
    (:sol "getValidators()" :returns "(bytes32,bool,uint64,address,string,string)[]" :until :t15)
  (get-validators))

(defcall validator-config get-next-full-dkg-ceremony* ()
    (:sol "getNextFullDkgCeremony()" :returns "uint64" :until :t15)
  (get-next-full-dkg-ceremony))

(defcall validator-config validators-array* (index)
    (:sol "validatorsArray(uint256 index)" :returns "address" :until :t15)
  (when (> index u64-max) (panic +panic-array-oob+))
  (validators-array index))

(defcall validator-config validators* (validator)
    (:sol "validators(address validator)" :returns "(bytes32,bool,uint64,address,string,string)"
     :until :t15)
  (validators validator))

(defcall validator-config validator-count* ()
    (:sol "validatorCount()" :returns "uint64" :until :t15)
  (validator-count))

(defcall validator-config add-validator* (new-validator-address public-key active inbound outbound)
    (:sol "addValidator(address newValidatorAddress, bytes32 publicKey, bool active, string inboundAddress, string outboundAddress)"
     :mutates t :until :t15)
  (add-validator sender new-validator-address public-key active inbound outbound)
  (values))

(defcall validator-config update-validator* (new-validator-address public-key inbound outbound)
    (:sol "updateValidator(address newValidatorAddress, bytes32 publicKey, string inboundAddress, string outboundAddress)"
     :mutates t :until :t15)
  (update-validator sender new-validator-address public-key inbound outbound)
  (values))

(defcall validator-config change-validator-status* (validator active)
    (:sol "changeValidatorStatus(address validator, bool active)" :mutates t :until :t15)
  (change-validator-status sender validator active)
  (values))

(defcall validator-config change-validator-status-by-index* (index active)
    (:sol "changeValidatorStatusByIndex(uint64 index, bool active)" :mutates t :since :t1 :until :t15)
  (change-validator-status-by-index sender index active)
  (values))

(defcall validator-config change-owner* (new-owner)
    (:sol "changeOwner(address newOwner)" :mutates t :until :t15)
  (change-owner sender new-owner)
  (values))

(defcall validator-config set-next-full-dkg-ceremony* (epoch)
    (:sol "setNextFullDkgCeremony(uint64 epoch)" :mutates t :until :t15)
  (set-next-full-dkg-ceremony sender epoch)
  (values))

(defdispatch validator-config (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +validator-config-address+ 'validator-config :genesis)
