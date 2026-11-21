(in-package #:clempo.precompiles)

;;; Addresses (crates/contracts/src/precompiles/mod.rs, crates/hardfork/src/constants.rs).

(defconstant +path-usd-address+ #x20c0000000000000000000000000000000000000)
(defconstant +ousd-address+ #x20c0000000000000000000006a37da5c996874be)
(defconstant +default-fee-token+ +path-usd-address+)
(defconstant +fee-manager-address+ #xfeec000000000000000000000000000000000000)
(defconstant +tip403-registry-address+ #x403c000000000000000000000000000000000000)
(defconstant +tip20-factory-address+ #x20fc000000000000000000000000000000000000)
(defconstant +stablecoin-dex-address+ #xdec0000000000000000000000000000000000000)
(defconstant +nonce-address+ #x4e4f4e4345000000000000000000000000000000)
(defconstant +validator-config-address+ #xcccccccc00000000000000000000000000000000)
(defconstant +validator-config-v2-address+ #xcccccccc00000000000000000000000000000001)
(defconstant +account-keychain-address+ #xaaaaaaaa00000000000000000000000000000000)
(defconstant +address-registry-address+ #xfdc0000000000000000000000000000000000000)
(defconstant +signature-verifier-address+ #x5165300000000000000000000000000000000000)
(defconstant +receive-policy-guard-address+ #xb10c000000000000000000000000000000000000)
(defconstant +current-committee-address+ #xc077e00000000000000000000000000000000000)
(defconstant +configurable-account-address+ #xaacc000000000000000000000000000000000000)
(defconstant +channel-reserve-address+ #x4d50500000000000000000000000000000000000)
(defconstant +zone-factory-address+ #x5af2000000000000000000000000000000000000)
(defconstant +zone-verifier-address+ #x5a56000000000000000000000000000000000000)
(defconstant +zone-portal-impl-address+ #x5ad1000000000000000000000000000000000000)
(defconstant +zone-messenger-address+ #x5a4d000000000000000000000000000000000000)

(defun fallback-fee-tokens (fork)
  "TempoHardfork::fallback_fee_tokens."
  (if (fork>= fork :t13)
      (list +path-usd-address+ +ousd-address+)
      (list +path-usd-address+)))

;;; TIP-20 addresses: the 12-byte prefix 20C000000000000000000000 then a u64 id.

(declaim (inline tip20-address-p))
(defun tip20-address-p (address)
  (= (ash address -64) #x20c000000000000000000000))

(defun tip20-id (address) (and (tip20-address-p address) (ldb (byte 64 0) address)))
(defun tip20-address (id) (logior (ash #x20c000000000000000000000 64) id))

;;; The set. SYSTEM_PRECOMPILES lists fixed addresses with their activation fork; TIP-20
;;; tokens are every address with the TIP-20 prefix.

(defvar *system-precompiles* (make-hash-table) "address -> (contract . fork)")

(defun %register-precompile (address contract fork source)
  (setf (gethash address *system-precompiles*) (cons contract fork))
  (record-fork-site fork :precompile (list contract address) nil source)
  contract)

(defmacro register-precompile (address contract fork &environment env)
  "Make CONTRACT the precompile at ADDRESS from FORK, recording this declaration's source."
  `(%register-precompile ,address ,contract ,fork
                         ',(fork-source-position env nil :since)))

(defvar *tip20-contract* nil
  "The contract behind every TIP-20-prefixed address (set by the TIP-20 implementation).")

(defun register-tip20-precompile (contract)
  (setf *tip20-contract* contract))

(defun precompile-contract (address &optional (fork *spec*))
  "The contract implementing the precompile at ADDRESS under FORK, or NIL."
  (if (tip20-address-p address)
      *tip20-contract*
      (let ((entry (gethash address *system-precompiles*)))
        (and entry (fork>= fork (cdr entry)) (car entry)))))

(defun precompile-address-p (address &optional (fork *spec*))
  "TempoAddressExt::is_precompile."
  (or (tip20-address-p address)
      (let ((entry (gethash address *system-precompiles*)))
        (and entry (fork>= fork (cdr entry)) t))))

(defun call-precompile (target input caller &key (code-address target))
  "Run the Tempo precompile at CODE-ADDRESS for a call to TARGET (they differ for a
DELEGATECALL) from CALLER with calldata INPUT, under *STORAGE* (whose gas limit and static
flag are the frame's) and *SPEC*. Returns (values status output gas-used refund) with
status :success, :revert, :halt or :fatal (output is then the reason), or NIL when
CODE-ADDRESS is not a precompile."
  (let ((contract (precompile-contract code-address)))
    (when contract
      (flet ((finish (status output)
               (return-from call-precompile
                 (values status output (gas-used) (gas-refunded)))))
        (when (/= target code-address)
          (finish :revert (clempo.abi:abi-encode-error "DelegateCallNotAllowed()")))
        (when (< (gas-remaining) (input-cost *spec* (length input)))
          (finish :halt :out-of-gas))
        (handler-case
            (finish :success (funcall (contract-dispatcher contract) code-address input caller))
          (precompile-revert (c) (finish :revert (precompile-revert-data c)))
          (precompile-halt (c) (finish :halt (precompile-halt-reason c)))
          (precompile-fatal (c) (finish :fatal (precompile-fatal-message c))))))))
