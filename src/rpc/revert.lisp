(defpackage #:clempo.rpc.revert
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:abi #:clempo.abi) (#:serde #:clempo.serde))
  (:documentation "Tempo's revert decoding for RPC errors (node/src/rpc/error.rs FromRevert):
revert data whose selector is a Tempo precompile error decodes into the text of
tempo_precompiles::error::TempoPrecompileError's Display, \"<label>: <Variant>(<Rust Debug>)\".")
  (:export #:precompile-error-text))

(in-package #:clempo.rpc.revert)

;;; Transcribed from the reference's sol! interfaces (crates/contracts/src/precompiles/*.rs),
;;; in the order tempo_precompiles::error::error_decoder_registry registers them; a later
;;; interface's selector replaces an earlier one's, as the registry's HashMap insert does.
(defparameter +precompile-error-interfaces+
  '(
    ("Stablecoin DEX error"
     "Unauthorized()"
     "PairDoesNotExist()"
     "PairAlreadyExists()"
     "OrderDoesNotExist()"
     "IdenticalTokens()"
     "InvalidToken()"
     "TickOutOfBounds(int16 tick)"
     "InvalidTick()"
     "InvalidFlipTick()"
     "InsufficientBalance()"
     "InsufficientLiquidity()"
     "InsufficientOutput()"
     "MaxInputExceeded()"
     "BelowMinimumOrderSize(uint128 amount)"
     "InvalidBaseToken()"
     "OrderNotStale()"
     "IndexAlreadySet()"
     )
    ("TIP20 token error"
     "InsufficientBalance(uint256 available, uint256 required, address token)"
     "InsufficientAllowance()"
     "SupplyCapExceeded()"
     "InvalidSupplyCap()"
     "InvalidPayload()"
     "PolicyForbids()"
     "InvalidRecipient()"
     "ContractPaused()"
     "InvalidCurrency()"
     "InvalidQuoteToken()"
     "InvalidAmount()"
     "NoOptedInSupply()"
     "Unauthorized()"
     "ProtectedAddress()"
     "InvalidToken()"
     "Uninitialized()"
     "InvalidTransferPolicyId()"
     "PermitExpired()"
     "InvalidSignature()"
     "LogoURITooLong()"
     "InvalidLogoURI()"
     "RootKeyRetired(address account)"
     "UnsupportedAddressVersion(uint8 version)"
     "ZoneUnregistered(uint32 zoneId)"
     )
    ("TIP20 factory error"
     "AddressReserved()"
     "AddressNotReserved()"
     "InvalidQuoteToken()"
     "TokenAlreadyExists(address token)"
     )
    ("TIP20 channel reserve error"
     "ChannelAlreadyExists()"
     "ChannelNotFound()"
     "NotPayer()"
     "NotPayeeOrOperator()"
     "InvalidPayee()"
     "ZeroDeposit()"
     "ExpiringNonceHashNotSet()"
     "InvalidSignature()"
     "AmountExceedsDeposit()"
     "AmountNotIncreasing()"
     "CaptureAmountInvalid()"
     "CloseNotReady()"
     "DepositOverflow()"
     "RootKeyRetired(address account)"
     "SignerConfigChanged(address signer)"
     "SaltNotSignerCommitment(address signer, bytes32 commitment)"
     )
    ("Roles auth error"
     "Unauthorized()"
     )
    ("TIP20 registry error"
     "MasterIdCollision(address master)"
     "InvalidMasterAddress()"
     "ProofOfWorkFailed()"
     "VirtualAddressUnregistered()"
     )
    ("TIP403 registry error"
     "Unauthorized()"
     "PolicyNotFound()"
     "PolicyNotSimple()"
     "InvalidPolicyType()"
     "IncompatiblePolicyType()"
     "VirtualAddressNotAllowed()"
     "InvalidReceivePolicyType()"
     "InvalidRecoveryAuthority()"
     )
    ("TIP fee manager error"
     "InvalidToken()"
     "InsufficientFeeTokenBalance()"
     "CannotChangeWithinBlock()"
     )
    ("TIP fee AMM error"
     "IdenticalAddresses()"
     "InvalidToken()"
     "InsufficientLiquidity()"
     "InsufficientReserves()"
     "InvalidAmount()"
     "DivisionByZero()"
     "InvalidSwapCalculation()"
     )
    ("Tempo Transaction nonce error"
     "ProtocolNonceNotSupported()"
     "InvalidNonceKey()"
     "NonceOverflow()"
     "ExpiringNonceReplay()"
     "ExpiringNonceSetFull()"
     "InvalidExpiringNonceExpiry()"
     )
    ("Validator config error"
     "Unauthorized()"
     "ValidatorAlreadyExists()"
     "ValidatorNotFound()"
     "InvalidPublicKey()"
     "NotHostPort(string field, string input, string backtrace)"
     "NotIpPort(string field, string input, string backtrace)"
     )
    ("Validator config v2 error"
     "AlreadyInitialized()"
     "IngressAlreadyExists(string ingress)"
     "EmptyV1ValidatorSet()"
     "InvalidMigrationIndex()"
     "InvalidIp(string input, string reason)"
     "InvalidOwner()"
     "InvalidPublicKey()"
     "InvalidSignature()"
     "InvalidSignatureFormat()"
     "InvalidValidatorAddress()"
     "MigrationNotComplete()"
     "NotInitialized()"
     "NotIp(string input, string backtrace)"
     "NotIpPort(string input, string backtrace)"
     "PublicKeyAlreadyExists()"
     "Unauthorized()"
     "AddressAlreadyHasValidator()"
     "ValidatorAlreadyDeactivated()"
     "ValidatorNotFound()"
     )
    ("Account keychain error"
     "UnauthorizedCaller()"
     "KeyAlreadyExists()"
     "KeyNotFound()"
     "KeyExpired()"
     "SpendingLimitExceeded()"
     "InvalidSpendingLimit()"
     "InvalidSignatureType()"
     "ZeroPublicKey()"
     "ExpiryInPast()"
     "KeyAlreadyRevoked()"
     "SignatureTypeMismatch(uint8 expected, uint8 actual)"
     "CallNotAllowed()"
     "InvalidCallScope()"
     "InvalidKeyId()"
     "InvalidKeyAuthorizationWitness()"
     "KeyAuthorizationWitnessAlreadyBurned()"
     "LegacyAuthorizeKeySelectorChanged(bytes4 newSelector)"
     )
    ("Signature verifier error"
     "InvalidFormat()"
     "InvalidSignature()"
     )
    ("TIP1028 blocked transfers error"
     "InvalidReceipt()"
     "InvalidClaimAddress()"
     "UnauthorizedClaimer()"
     "AddressReserved()"
     )
    ("TIP1060 storage credits error"
     "InvalidMode()"
     "OnlyDirectCall()"
     )
    ("Current committee error"
     "Unauthorized()"
     )
    ("ZoneFactory error"
     "InvalidToken()"
     "TokenTransferPolicyNotSet()"
     "InvalidClosedLoopConfig()"
     "NotOwner()"
     "InvalidAdmin()"
     "InvalidSequencerSet()"
     "AlreadyInitialized()"
     "TokenMetadataTooLong()"
     )
    ("configurable account error"
     "InvalidAccount()"
     "InvalidConfig()"
     "InvalidThreshold()"
     "InvalidConfigurableAccountOwner()"
     "InvalidWeight()"
     "TooManyOwners()"
     "DuplicateOwner()"
     "InvalidOwnerOrder()"
     "UnauthorizedConfigurableAccountCaller()"
     "PrimitiveRootRequired()"
     "UpgradeMustBeDirectCall()"
     "AccountAlreadyConfigurable()"
     "AccountHasCode()"
     "UnsupportedOnZone()"
     )
    )
  "The Tempo precompile error interfaces in tempo_precompiles::error::error_decoder_registry order: (label signature ...).")

(defun signature-fields (signature)
  "\"Name(uint256 a, address b)\" -> (values name types field-names)."
  (multiple-value-bind (name types) (abi:parse-signature signature)
    (let* ((open (position #\( signature))
           (inner (string-trim " " (subseq signature (1+ open) (position #\) signature :from-end t)))))
      (values name types
              (if (zerop (length inner))
                  '()
                  (loop for part in (split-sequence #\, inner)
                        collect (let ((p (string-trim " " part)))
                                  (subseq p (1+ (position #\Space p :from-end t))))))))))

(defun split-sequence (char string)
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start end)
        while end))

(defparameter *registry*
  (let ((table (make-hash-table)))
    (loop for (label . signatures) in +precompile-error-interfaces+
          do (dolist (s signatures)
               (multiple-value-bind (name types fields) (signature-fields s)
                 (setf (gethash (abi:abi-selector (abi:canonical-signature name types)) table)
                       (list label name types fields)))))
    table)
  "selector -> (label name types field-names)")

(defun debug-value (type value)
  "Rust's Debug of an alloy sol value."
  (cond ((eq type :address) (integer->hex value :bytes 20))
        ((eq type :string) (serde:rust-debug-string value))
        ((and (consp type) (eq (first type) :bytes-n))
         (integer->hex value :bytes (second type)))
        (t (format nil "~D" value))))

(defun precompile-error-text (data)
  "The Display text of the Tempo precompile error DATA encodes, or NIL."
  (when (>= (length data) 4)
    (let ((entry (gethash (octets->integer data :end 4) *registry*)))
      (when entry
        (destructuring-bind (label name types fields) entry
          (let ((values (handler-case (abi:abi-decode types data :start 4 :validate t)
                          (abi:abi-error () (return-from precompile-error-text nil)))))
            (format nil "~A: ~A(~A~@[ { ~{~A~^, ~} }~])" label name name
                    (and fields
                         (loop for f in fields for type in types for v in values
                               collect (format nil "~A: ~A" f (debug-value type v)))))))))))
