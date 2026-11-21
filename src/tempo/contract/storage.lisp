(in-package #:clempo.contract)

;;; Outcomes. A precompile either returns, reverts with ABI data, halts (out of gas, a state
;;; change in a static call, ...) or fails fatally (the reference's PrecompileError::Fatal,
;;; which aborts the block). TempoPrecompileError's variants map onto these conditions.

(define-condition precompile-condition (error) ())

(define-condition precompile-revert (precompile-condition)
  ((data :initarg :data :reader precompile-revert-data)
   (name :initarg :name :initform nil :reader precompile-revert-name)
   (args :initarg :args :initform nil :reader precompile-revert-args))
  (:report (lambda (c s)
             (format s "precompile revert~@[ ~S~]~@[ ~S~] ~A" (precompile-revert-name c)
                     (precompile-revert-args c) (octets->hex (precompile-revert-data c))))))

(define-condition precompile-halt (precompile-condition)
  ((reason :initarg :reason :reader precompile-halt-reason))
  (:report (lambda (c s) (format s "precompile halt: ~A" (precompile-halt-reason c)))))

(define-condition precompile-fatal (precompile-condition)
  ((message :initarg :message :reader precompile-fatal-message))
  (:report (lambda (c s) (format s "precompile fatal: ~A" (precompile-fatal-message c)))))

(defun revert-data (data)
  "Revert with raw DATA (octets)."
  (error 'precompile-revert :data data))

(defun halt (reason)
  "Halt the frame: REASON is :out-of-gas, :static-call, or a string."
  (error 'precompile-halt :reason reason))

(defun out-of-gas () (halt :out-of-gas))

(defun fatal (control &rest args)
  (error 'precompile-fatal :message (apply #'format nil control args)))

;;; Solidity errors. DEFERROR registers NAME with its signature; REVERT signals it.

(defmacro deferror (name signature)
  "Register the Solidity error NAME, e.g. (deferror insufficient-balance
\"InsufficientBalance(uint256 available, uint256 required, address token)\")."
  (multiple-value-bind (sol types) (clempo.abi:parse-signature signature)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'error-signature)
             (list ,(clempo.abi:abi-selector (clempo.abi:canonical-signature sol types)) ',types
                   ,(clempo.abi:canonical-signature sol types)))
       ',name)))

(defun error-data (name &rest args)
  "The ABI revert data of the registered error NAME with ARGS."
  (destructuring-bind (selector types canonical) (or (get name 'error-signature)
                                                     (error "unknown precompile error ~S" name))
    (declare (ignore canonical))
    (invariant (= (length types) (length args)) "~S takes ~D arguments" name (length types))
    (clempo.abi:abi-encode-call selector types args)))

(defun revert (name &rest args)
  "Revert with the registered Solidity error NAME."
  (error 'precompile-revert :data (apply #'error-data name args) :name name :args args))

(defconstant +panic-assert+ #x01)
(defconstant +panic-under-overflow+ #x11)
(defconstant +panic-division-by-zero+ #x12)
(defconstant +panic-enum-conversion+ #x21)
(defconstant +panic-array-oob+ #x32)

(defun panic (code)
  "Revert with Panic(uint256 CODE)."
  (error 'precompile-revert :data (clempo.abi:panic-data code) :name 'panic :args (list code)))

;;; Checked arithmetic: Rust's checked_* with under_overflow (Panic 0x11) on failure.

(defconstant u64-max (1- (ash 1 64)))
(defconstant u128-max (1- (ash 1 128)))

(declaim (inline checked+ checked- checked*))
(defun checked+ (a b &optional (bits 256))
  (let ((r (+ a b)))
    (if (>= r (ash 1 bits)) (panic +panic-under-overflow+) r)))

(defun checked- (a b)
  (let ((r (- a b)))
    (if (minusp r) (panic +panic-under-overflow+) r)))

(defun checked* (a b &optional (bits 256))
  (let ((r (* a b)))
    (if (>= r (ash 1 bits)) (panic +panic-under-overflow+) r)))

(defun checked-div (a b)
  (if (zerop b) (panic +panic-under-overflow+) (floor a b)))

(defun checked-mul-div (a b c)
  "a*b/c with the u256 product checked, as U256::checked_mul then checked_div."
  (checked-div (checked* a b) c))

;;; Accounts as the storage layer sees them.

(define-constant +keccak-empty-octets+ (clempo.keccak:keccak256 (make-octets 0)))
(define-constant +keccak-empty+ (octets->integer +keccak-empty-octets+))

(defstruct (account-info (:constructor make-account-info
                             (&key (nonce 0) (balance 0) (code (make-octets 0))
                                   (code-hash (if (zerop (length code))
                                                  +keccak-empty+
                                                  (clempo.keccak:keccak256-integer code)))
                                   (commitment 0))))
  (nonce 0 :type (integer 0))
  (balance 0 :type (integer 0))
  (code (make-octets 0) :type octets)
  (code-hash +keccak-empty+ :type (integer 0))
  (commitment 0 :type (integer 0)))

(defun account-empty-p (info)
  "AccountInfo::is_empty: no nonce, no balance, no code. NIL INFO (no account) is empty."
  (or (null info)
      (and (zerop (account-info-nonce info)) (zerop (account-info-balance info))
           (= (account-info-code-hash info) +keccak-empty+))))

(defun account-has-code-p (info)
  "Not AccountInfo::is_empty_code_hash."
  (and info (/= (account-info-code-hash info) +keccak-empty+)))

(defstruct (block-info (:constructor make-block-info
                           (&key (number 0) (timestamp 0) (beneficiary 0) (chain-id 4217))))
  (number 0 :type (integer 0))
  (timestamp 0 :type (integer 0))
  (beneficiary 0 :type (integer 0))
  (chain-id 4217 :type (integer 0)))

;;; The backend protocol. *STORAGE* is Rust's thread-local StorageCtx; everything that
;;; touches state or gas goes through it.

(defvar *storage* nil "The active storage backend.")

(defclass storage-backend () ()
  (:documentation "Base class of storage backends."))

(defgeneric backend-sload (backend address slot))
(defgeneric backend-sstore (backend address slot value))
(defgeneric backend-tload (backend address slot))
(defgeneric backend-tstore (backend address slot value))
(defgeneric backend-emit-log (backend address topics data))
(defgeneric backend-logs (backend) (:documentation "Logs emitted so far, oldest first."))
(defgeneric backend-deduct-gas (backend gas))
(defgeneric backend-refund-gas (backend gas))
(defgeneric backend-gas-limit (backend))
(defgeneric backend-gas-used (backend))
(defgeneric backend-gas-refunded (backend))
(defgeneric backend-account (backend address)
  (:documentation "Load ADDRESS's account, charging the account access (with_account_info).
Returns an ACCOUNT-INFO or NIL when the account does not exist."))
(defgeneric backend-warm-caller-account (backend address)
  (:documentation "The account of an already-warm caller, uncharged (with_warm_caller_info)."))
(defgeneric backend-set-code (backend address code))
(defgeneric backend-static-p (backend))
(defgeneric backend-block (backend) (:documentation "The BLOCK-INFO of the executing block."))
(defgeneric backend-checkpoint (backend))
(defgeneric backend-commit (backend checkpoint))
(defgeneric backend-revert (backend checkpoint))
(defgeneric backend-config-commitment (backend address))
(defgeneric backend-set-config-commitment (backend address commitment gas-kind))
(defgeneric backend-call (backend target input gas-limit static-p)
  (:method (backend target input gas-limit static-p)
    (declare (ignore backend target input gas-limit static-p))
    ;; NestedCallUnavailable reverts with empty data.
    (revert-data (make-octets 0))))
(defgeneric backend-set-tip1060-credits (backend enabled) (:method (b e) (declare (ignore b e))))
(defgeneric backend-set-tip1060-credit-minting (backend enabled) (:method (b e) (declare (ignore b e))))
(defgeneric backend-kind (backend) (:method (b) (declare (ignore b)) :l1))

(defmacro with-storage ((backend) &body body)
  `(let ((*storage* ,backend)) ,@body))

(declaim (inline sload sstore tload tstore deduct-gas))
(defun sload (address slot) (backend-sload *storage* address slot))
(defun sstore (address slot value) (backend-sstore *storage* address slot value))
(defun tload (address slot) (backend-tload *storage* address slot))
(defun tstore (address slot value) (backend-tstore *storage* address slot value))
(defun deduct-gas (gas) (backend-deduct-gas *storage* gas))
(defun refund-gas (gas) (backend-refund-gas *storage* gas))
(defun gas-limit () (backend-gas-limit *storage*))
(defun gas-used () (backend-gas-used *storage*))
(defun gas-remaining () (- (backend-gas-limit *storage*) (backend-gas-used *storage*)))
(defun gas-refunded () (backend-gas-refunded *storage*))
(defun static-p () (backend-static-p *storage*))
(defun ensure-not-static () (when (static-p) (halt :static-call)))

(defun emit-log (address topics data)
  "Emit a log from ADDRESS: TOPICS is a list of integers, DATA octets."
  (backend-emit-log *storage* address topics data))

(defun account (address) (backend-account *storage* address))
(defun warm-caller-account (address) (backend-warm-caller-account *storage* address))
(defun initialized-p (address)
  "ContractStorage::is_initialized: the account at ADDRESS has code."
  (account-has-code-p (account address)))
(defun set-code (address code) (backend-set-code *storage* address code))

(defun block-env () (backend-block *storage*))
(defun block-timestamp () (block-info-timestamp (backend-block *storage*)))
(defun block-number () (block-info-number (backend-block *storage*)))
(defun block-beneficiary () (block-info-beneficiary (backend-block *storage*)))
(defun chain-id () (block-info-chain-id (backend-block *storage*)))
(defun evm-kind () (backend-kind *storage*))

(defun keccak-octets (data)
  "keccak256 of DATA, charging its gas (PrecompileStorageProvider::keccak256)."
  (deduct-gas (keccak-gas (length data)))
  (clempo.keccak:keccak256 data))

(defun keccak (data)
  "keccak256 of DATA as an integer, charging its gas."
  (octets->integer (keccak-octets data)))

(defun recover-signer (digest v r s)
  "ecrecover with its 3,000 gas: the signer's address, or NIL. DIGEST, R, S are integers.
alloy's secp256k1::recover_signer, so s above n/2 (EIP-2) recovers nothing."
  (deduct-gas +ecrecover-gas+)
  (when (and (member v '(27 28)) (<= s clempo.secp256k1:+secp256k1-half-n+))
    (let ((pub (ignore-errors
                (clempo.secp256k1:ecrecover (integer->octets digest 32) r s (- v 27)))))
      (when pub
        (let ((addr (clempo.secp256k1:pubkey->address pub)))
          (unless (zerop addr) addr))))))

(defmacro with-checkpoint ((&optional (commit-var (gensym "COMMIT"))) &body body)
  "StorageCtx::checkpoint (T1C+): state changes in BODY are rolled back unless BODY returns
normally; within BODY, (COMMIT-VAR) may be called early. Before T1C no checkpoint is taken."
  (let ((cp (gensym "CP")) (done (gensym "DONE")))
    `(let ((,cp (when (spec>= :t1c) (backend-checkpoint *storage*)))
           (,done nil))
       (flet ((,commit-var ()
                (when (and ,cp (not ,done))
                  (setf ,done t)
                  (backend-commit *storage* ,cp))))
         (declare (ignorable #',commit-var))
         (unwind-protect (multiple-value-prog1 (progn ,@body) (,commit-var))
           (when (and ,cp (not ,done))
             (setf ,done t)
             (backend-revert *storage* ,cp)))))))

(defun config-commitment (address) (backend-config-commitment *storage* address))
(defun set-config-commitment (address commitment &optional (gas-kind :precompile))
  (backend-set-config-commitment *storage* address commitment gas-kind))
(defun retired-root-p (address)
  "StorageCtx::is_retired_root: T14+ and ADDRESS has a configuration commitment."
  (and (spec>= :t14) (/= 0 (config-commitment address))))

(defun nested-call (target input gas-limit static-p)
  "Call TARGET from the running precompile. Returns (values success-p output)."
  (backend-call *storage* target input gas-limit static-p))

(defun set-tip1060-credits (enabled) (backend-set-tip1060-credits *storage* enabled))
(defun set-tip1060-credit-minting (enabled) (backend-set-tip1060-credit-minting *storage* enabled))
