(in-package #:clempo.contract)

;;; The host: what the metered backend needs from the world state. The executor implements
;;; these generic functions over clempo.state; MEMORY-HOST below implements them in memory
;;; (tests, the oracle). Warmth follows EIP-2929; everything a host changes is journaled so a
;;; checkpoint can roll it back (warmth included, as in revm/evm2).

(define-condition cold-load-skipped (precompile-halt) ()
  (:default-initargs :reason :out-of-gas)
  (:documentation "A cold load refused for lack of gas (LoadError::ColdLoadSkipped)."))

(defgeneric host-sload (host address slot skip-cold)
  (:documentation "Warm ADDRESS and SLOT and return (values present-value cold-p). When the
slot is cold and SKIP-COLD is true, signal COLD-LOAD-SKIPPED without warming."))
(defgeneric host-sstore (host address slot value skip-cold)
  (:documentation "Like HOST-SLOAD, then write VALUE. Returns (values original present cold-p):
ORIGINAL is the slot's value at the start of the transaction, PRESENT before this write."))
(defgeneric host-tload (host address slot))
(defgeneric host-tstore (host address slot value))
(defgeneric host-account (host address skip-cold)
  (:documentation "Warm and load ADDRESS: (values account-info-or-nil cold-p). A cold account
with SKIP-COLD signals COLD-LOAD-SKIPPED."))
(defgeneric host-peek-account (host address)
  (:documentation "(values account-info-or-nil warm-p), changing nothing."))
(defgeneric host-set-code (host address code)
  (:documentation "Warm ADDRESS and set its code. Returns whether the account was empty."))
(defgeneric host-set-config-commitment (host address commitment)
  (:documentation "Set ADDRESS's configuration commitment, giving it the configurable-account
code when it had none. Returns (values previous-commitment account-was-empty-p)."))
(defgeneric host-log (host address topics data))
(defgeneric host-logs (host) (:documentation "Logs so far, oldest first: (address topics data)."))
(defgeneric host-checkpoint (host))
(defgeneric host-commit (host checkpoint))
(defgeneric host-revert (host checkpoint))
(defgeneric host-block (host))
(defgeneric host-call (host target input gas-limit static-p)
  (:method (host target input gas-limit static-p)
    (declare (ignore host target input gas-limit static-p))
    (revert-data (make-octets 0))))

;;; METERED-STORAGE: crates/precompiles/src/storage/evm.rs, gas for gas.

(define-constant +storage-credits-address+ #x1060000000000000000000000000000000000000)

;;; A structure, not a class: one is made per precompile call and per protocol call, and its
;;; slots are read on every SLOAD and charge.
(defstruct (metered-storage (:constructor %make-metered-storage
                                (host limit static tip1060 non-creditable))
                            (:copier nil))
  "The backend precompiles run on inside the EVM: every SLOAD, SSTORE, account load, log and
code write is charged exactly as EvmPrecompileStorageProvider charges it, including T4's
pre-charging, T12's stipend guard and TIP-1060 storage credits."
  host
  (limit (1- (ash 1 64)))
  (spent 0)
  (refunded 0)
  (static nil :read-only t)
  (tip1060 :default)
  (minting t)
  ;; NIL or a function (owner slot) -> true for a slot whose clear must not mint a storage
  ;; credit (NonCreditableSlots).
  (non-creditable nil))

(declaim (inline metered-static metered-tip1060 (setf metered-tip1060) metered-minting
                 (setf metered-minting) non-creditable-slots (setf non-creditable-slots)))
(defun metered-static (b) (metered-storage-static b))
(defun metered-tip1060 (b) (metered-storage-tip1060 b))
(defun (setf metered-tip1060) (v b) (setf (metered-storage-tip1060 b) v))
(defun metered-minting (b) (metered-storage-minting b))
(defun (setf metered-minting) (v b) (setf (metered-storage-minting b) v))
(defun non-creditable-slots (b) (metered-storage-non-creditable b))
(defun (setf non-creditable-slots) (v b) (setf (metered-storage-non-creditable b) v))

(defun make-metered-storage (host &key (gas-limit (1- (ash 1 64))) static (tip1060 t) non-creditable)
  "A metered backend over HOST with GAS-LIMIT gas. TIP1060 NIL is
enter_evm_without_tip1060_accounting."
  (%make-metered-storage host gas-limit static tip1060 non-creditable))

(defun tip1060-enabled-p (b)
  (and (metered-tip1060 b) (spec>= :t7)))

;;; The limit is often 2^64 - 1, a bignum: compare against SPENT + GAS (a fixnum) rather than
;;; subtracting, which would cons a bignum on every charge.
(defun charge (b gas)
  (let ((spent (+ (metered-storage-spent b) gas)))
    (if (> spent (metered-storage-limit b))
        (out-of-gas)
        (setf (metered-storage-spent b) spent))))

(declaim (inline short-of-p))
(defun short-of-p (b gas)
  "True when B has less than GAS left."
  (> (+ (metered-storage-spent b) gas) (metered-storage-limit b)))

(defmethod backend-deduct-gas ((b metered-storage) gas) (charge b gas))
(defmethod backend-refund-gas ((b metered-storage) gas) (incf (metered-storage-refunded b) gas))
(defmethod backend-gas-limit ((b metered-storage)) (metered-storage-limit b))
(defmethod backend-gas-used ((b metered-storage)) (metered-storage-spent b))
(defmethod backend-gas-refunded ((b metered-storage)) (metered-storage-refunded b))
(defmethod backend-static-p ((b metered-storage)) (metered-static b))
(defmethod backend-block ((b metered-storage)) (host-block (metered-storage-host b)))
(defmethod backend-logs ((b metered-storage)) (host-logs (metered-storage-host b)))
(defmethod backend-set-tip1060-credits ((b metered-storage) enabled) (setf (metered-tip1060 b) enabled))
(defmethod backend-set-tip1060-credit-minting ((b metered-storage) enabled) (setf (metered-minting b) enabled))

(defmethod backend-sload ((b metered-storage) address slot)
  (let ((additional (gas :cold-storage-additional-cost))
        (skip nil))
    (when (spec>= :t4)
      (charge b (gas :warm-storage-read-cost))
      (setf skip (short-of-p b additional)))
    (multiple-value-bind (value cold) (host-sload (metered-storage-host b) address slot skip)
      (when (spec< :t4) (charge b (gas :warm-storage-read-cost)))
      (when cold (charge b additional))
      value)))

(defun sstore-dynamic-gas (original present new cold)
  "GasParams::sstore_dynamic_gas under EIP-2200."
  (+ (if cold (gas :cold-storage-cost) 0)
     (if (and (/= new present) (= original present))
         (if (zerop original)
             (gas :sstore-set-without-load-cost)
             (gas :sstore-reset-without-cold-load-cost))
         0)))

(defun sstore-refund (original present new)
  "GasParams::sstore_refund under EIP-2200."
  (let ((clearing (gas :sstore-clearing-slot-refund)))
    (cond ((= new present) 0)
          ((and (= original present) (zerop new)) clearing)
          (t (let ((refund 0))
               (unless (zerop original)
                 (cond ((zerop present) (decf refund clearing))
                       ((zerop new) (incf refund clearing))))
               (when (= original new)
                 (incf refund (if (zerop original)
                                  (gas :sstore-set-refund)
                                  (gas :sstore-reset-refund))))
               refund)))))

(defmethod backend-sstore ((b metered-storage) address slot value)
  (when (metered-static b) (halt :static-call))
  (when (and (spec>= :t12) (short-of-p b (1+ (gas :call-stipend))))
    (out-of-gas))
  (let ((skip nil))
    (when (spec>= :t4)
      (charge b (gas :sstore-static))
      (setf skip (short-of-p b (gas :cold-storage-additional-cost))))
    (multiple-value-bind (original present cold)
        (host-sstore (metered-storage-host b) address slot value skip)
      (when (spec< :t4) (charge b (gas :sstore-static)))
      (when (tip1060-enabled-p b)
        (sstore-storage-credits b address slot present value))
      (charge b (sstore-dynamic-gas original present value cold))
      (when (spec>= :t4)
        (incf (metered-storage-refunded b) (sstore-refund original present value)))
      value)))

;;; TIP-1060 storage credits (storage_credits/accounting.rs). The credit balance of OWNER
;;; is the u64 at slot OWNER of the storage-credits precompile; its transaction-local state
;;; is the transient word at the same slot: mode in the low byte, budget in bits 64..127,
;;; pending refunds in bits 192..255.

(defun sstore-storage-credits (b owner slot present new)
  (let ((host (metered-storage-host b)))
    (when (and (not (eq (zerop present) (zerop new)))
               (/= owner +storage-credits-address+))
      (charge b (gas :warm-storage-read-cost))
      (let* ((account-slot owner)
             (additional (gas :cold-storage-additional-cost))
             (skip (short-of-p b additional)))
        (multiple-value-bind (raw cold) (host-sload host +storage-credits-address+ account-slot skip)
          (when cold (charge b additional))
          (when (> raw u64-max) (fatal "invalid storage credits state"))
          (let ((credit raw) (changed nil))
            (cond
              ((zerop new)
               ;; x -> 0 mints a credit, except on the fee bookkeeping slots
               (when (and (non-creditable-slots b) (funcall (non-creditable-slots b) owner slot))
                 (return-from sstore-storage-credits nil))
               (when (metered-minting b)
                 (setf credit (min u64-max (1+ credit)) changed t)))
              (t
               ;; 0 -> x
               (let* ((state (host-tload host +storage-credits-address+ account-slot))
                      (mode (ldb (byte 8 0) state))
                      (budget (ldb (byte 64 64) state))
                      (pending (ldb (byte 64 192) state)))
                 (when (> mode 2) (fatal "invalid storage credits state"))
                 (flet ((store-state ()
                          (when (metered-static b) (halt :static-call))
                          (host-tstore host +storage-credits-address+ account-slot
                                       (logior mode (ash budget 64) (ash pending 192)))))
                   (cond ((and (= mode 2) (plusp credit) (plusp budget))
                          (decf credit)
                          (setf changed t)
                          (unless (= budget u64-max)
                            (decf budget)
                            (store-state)))
                         ((member mode '(1 2))
                          (charge b +storage-credit-value+))
                         (t ; refund mode
                          (charge b +storage-credit-value+)
                          (setf pending (min u64-max (1+ pending)))
                          (store-state)))))))
            (when changed
              (when (metered-static b) (halt :static-call))
              (multiple-value-bind (original2 present2)
                  (host-sstore host +storage-credits-address+ account-slot credit nil)
                (when (and (/= present2 credit) (= original2 present2))
                  (charge b (gas :sstore-reset-without-cold-load-cost)))))))))))

(defmethod backend-tload ((b metered-storage) address slot)
  (charge b (gas :warm-storage-read-cost))
  (host-tload (metered-storage-host b) address slot))

(defmethod backend-tstore ((b metered-storage) address slot value)
  (when (metered-static b) (halt :static-call))
  (charge b (gas :warm-storage-read-cost))
  (host-tstore (metered-storage-host b) address slot value))

(defmethod backend-emit-log ((b metered-storage) address topics data)
  (when (metered-static b) (halt :static-call))
  (charge b (+ +log-gas+ (* (gas :logdata) (length data)) (* (gas :logtopic) (length topics))))
  (host-log (metered-storage-host b) address topics data))

(defmethod backend-account ((b metered-storage) address)
  (let ((additional (gas :cold-account-additional-cost))
        (skip nil))
    (when (spec>= :t4)
      (charge b (gas :warm-storage-read-cost))
      (setf skip (short-of-p b additional)))
    (multiple-value-bind (info cold) (host-account (metered-storage-host b) address skip)
      (when (spec< :t4) (charge b (gas :warm-storage-read-cost)))
      (when cold (charge b additional))
      (when (and info (/= 0 (account-info-commitment info)) (spec< :t14))
        (fatal "account commitment before T14"))
      info)))

(defmethod backend-warm-caller-account ((b metered-storage) address)
  (multiple-value-bind (info warm) (host-peek-account (metered-storage-host b) address)
    (unless warm (fatal "caller account must already be warm"))
    info))

(defmethod backend-set-code ((b metered-storage) address code)
  (when (metered-static b) (halt :static-call))
  (charge b (* (gas :code-deposit-cost) (length code)))
  (host-set-code (metered-storage-host b) address code)
  nil)

(defmethod backend-config-commitment ((b metered-storage) address)
  (let ((info (backend-account b address)))
    (if info (account-info-commitment info) 0)))

(defmethod backend-set-config-commitment ((b metered-storage) address commitment gas-kind)
  (when (or (spec< :t14) (metered-static b) (zerop commitment))
    (revert-data (make-octets 0)))
  (when (eq gas-kind :precompile) (charge b 5000))
  (let* ((host (metered-storage-host b))
         (info (host-peek-account host address))
         (previous (if info (account-info-commitment info) 0))
         (creates (account-empty-p info)))
    (charge b (ecase gas-kind
                (:intrinsic (if (zerop previous) 0 (revert-data (make-octets 0))))
                (:precompile (if (zerop previous) (max 0 (- (gas :new-account-cost) 5000)) 0))))
    (when (and creates (eq gas-kind :precompile))
      (charge b (gas :new-account-cost)))
    (host-set-config-commitment host address commitment)
    nil))

(defmethod backend-checkpoint ((b metered-storage)) (host-checkpoint (metered-storage-host b)))
(defmethod backend-commit ((b metered-storage) cp) (host-commit (metered-storage-host b) cp))
(defmethod backend-revert ((b metered-storage) cp) (host-revert (metered-storage-host b) cp))
(defmethod backend-call ((b metered-storage) target input gas-limit static-p)
  (host-call (metered-storage-host b) target input gas-limit static-p))

;;; MEMORY-HOST: a journaled in-memory world for tests and the oracle.

(defclass memory-host ()
  ((accounts :initform (make-hash-table) :reader memory-host-accounts) ; address -> account-info
   (storage :initform (make-hash-table) :reader memory-host-storage)   ; address -> (slot -> value)
   (original :initform (make-hash-table :test 'equal) :reader memory-host-original) ; (a . s) -> v
   (transient :initform (make-hash-table :test 'equal) :accessor memory-host-transient)
   (warm-addresses :initform (make-hash-table) :reader warm-addresses)
   (warm-slots :initform (make-hash-table :test 'equal) :reader warm-slots)
   (logs :initform '() :accessor memory-host-log-list)
   (journal :initform '() :accessor memory-host-journal)
   (depth :initform 0 :accessor memory-host-depth)
   (block :initarg :block :initform (make-block-info) :accessor memory-host-block)))

(defun make-memory-host (&key (block (make-block-info))) (make-instance 'memory-host :block block))

(defun journal (host undo) (push undo (memory-host-journal host)))

(defun slot-table (host address)
  (or (gethash address (memory-host-storage host))
      (setf (gethash address (memory-host-storage host)) (make-hash-table))))

(defun memory-host-put-account (host address &key (nonce 0) (balance 0) code (commitment 0))
  "Seed an account (not journaled)."
  (setf (gethash address (memory-host-accounts host))
        (make-account-info :nonce nonce :balance balance :code (or code (make-octets 0))
                           :commitment commitment)))

(defun memory-host-account (host address) (gethash address (memory-host-accounts host)))

(defun memory-host-put-storage (host address slot value)
  "Seed a storage slot (not journaled; becomes the slot's original value)."
  (setf (gethash slot (slot-table host address)) value))

(defun memory-host-put-transient (host address slot value)
  (setf (gethash (cons address slot) (memory-host-transient host)) value))

(defun memory-host-warm-address (host address)
  (setf (gethash address (warm-addresses host)) t))

(defun memory-host-commitments (host)
  "address -> commitment of every account with a non-zero commitment."
  (loop for a being the hash-keys of (memory-host-accounts host) using (hash-value info)
        when (/= 0 (account-info-commitment info)) collect (cons a (account-info-commitment info))))

(defun warm-address (host address)
  (unless (gethash address (warm-addresses host))
    (setf (gethash address (warm-addresses host)) t)
    (journal host (lambda () (remhash address (warm-addresses host))))
    t))

(defun warm-slot (host address slot skip-cold)
  "Warm SLOT; return T when it was cold."
  (let ((key (cons address slot)))
    (cond ((gethash key (warm-slots host)) nil)
          (skip-cold (error 'cold-load-skipped))
          (t (setf (gethash key (warm-slots host)) t)
             (journal host (lambda () (remhash key (warm-slots host))))
             t))))

(defmethod host-sload ((h memory-host) address slot skip-cold)
  (let ((cold (warm-slot h address slot skip-cold)))
    (warm-address h address)
    (values (gethash slot (slot-table h address) 0) cold)))

(defmethod host-sstore ((h memory-host) address slot value skip-cold)
  (let ((cold (warm-slot h address slot skip-cold)))
    (warm-address h address)
    (let* ((table (slot-table h address))
           (present (gethash slot table 0))
           (key (cons address slot))
           (original (multiple-value-bind (v found) (gethash key (memory-host-original h))
                       (if found v (setf (gethash key (memory-host-original h)) present)))))
      (setf (gethash slot table) value)
      (journal h (lambda () (setf (gethash slot table) present)))
      (values original present cold))))

(defmethod host-tload ((h memory-host) address slot)
  (gethash (cons address slot) (memory-host-transient h) 0))

(defmethod host-tstore ((h memory-host) address slot value)
  (let* ((key (cons address slot))
         (old (gethash key (memory-host-transient h) 0)))
    (setf (gethash key (memory-host-transient h)) value)
    (journal h (lambda () (setf (gethash key (memory-host-transient h)) old)))))

(defmethod host-account ((h memory-host) address skip-cold)
  (let ((cold (not (gethash address (warm-addresses h)))))
    (when (and cold skip-cold) (error 'cold-load-skipped))
    (warm-address h address)
    (values (gethash address (memory-host-accounts h)) cold)))

(defmethod host-peek-account ((h memory-host) address)
  (values (gethash address (memory-host-accounts h))
          (and (gethash address (warm-addresses h)) t)))

(defun replace-account (h address new)
  (let ((old (gethash address (memory-host-accounts h))))
    (setf (gethash address (memory-host-accounts h)) new)
    (journal h (lambda ()
                 (if old
                     (setf (gethash address (memory-host-accounts h)) old)
                     (remhash address (memory-host-accounts h)))))
    old))

(defmethod host-set-code ((h memory-host) address code)
  (warm-address h address)
  (let ((old (gethash address (memory-host-accounts h))))
    (replace-account h address
                     (make-account-info :nonce (if old (account-info-nonce old) 0)
                                        :balance (if old (account-info-balance old) 0)
                                        :code code
                                        :commitment (if old (account-info-commitment old) 0)))
    (account-empty-p old)))

(defmethod host-set-config-commitment ((h memory-host) address commitment)
  (let* ((old (gethash address (memory-host-accounts h)))
         (previous (if old (account-info-commitment old) 0)))
    (replace-account h address
                     (make-account-info :nonce (if old (account-info-nonce old) 0)
                                        :balance (if old (account-info-balance old) 0)
                                        :code (if (or (zerop previous) (null old))
                                                  (octets-of #xef)
                                                  (account-info-code old))
                                        :commitment commitment))
    (values previous (account-empty-p old))))

(defmethod host-log ((h memory-host) address topics data)
  (push (list address topics data) (memory-host-log-list h))
  (journal h (lambda () (pop (memory-host-log-list h)))))

(defmethod host-logs ((h memory-host)) (reverse (memory-host-log-list h)))

(defmethod host-checkpoint ((h memory-host))
  (incf (memory-host-depth h))
  (length (memory-host-journal h)))

(defmethod host-commit ((h memory-host) cp)
  (declare (ignore cp))
  (decf (memory-host-depth h)))

(defmethod host-revert ((h memory-host) cp)
  (decf (memory-host-depth h))
  (loop while (> (length (memory-host-journal h)) cp)
        do (funcall (pop (memory-host-journal h)))))

(defmethod host-block ((h memory-host)) (memory-host-block h))

;;; HASHMAP-STORAGE: unmetered, for unit tests (storage/hashmap.rs).

(defclass hashmap-storage (storage-backend)
  ((slots :initform (make-hash-table :test 'equal) :reader hashmap-storage-slots)
   (transient :initform (make-hash-table :test 'equal) :reader hashmap-transient)
   (accounts :initform (make-hash-table) :reader hashmap-accounts)
   (logs :initform '() :accessor hashmap-storage-logs)
   (block :initarg :block :initform (make-block-info) :accessor hashmap-block)
   (static :initarg :static :initform nil :accessor hashmap-static)
   (used :initform 0 :accessor hashmap-used)))

(defun make-hashmap-storage (&key (block (make-block-info)) static)
  (make-instance 'hashmap-storage :block block :static static))

(defmethod backend-sload ((b hashmap-storage) address slot)
  (gethash (cons address slot) (hashmap-storage-slots b) 0))
(defmethod backend-sstore ((b hashmap-storage) address slot value)
  (when (hashmap-static b) (halt :static-call))
  (setf (gethash (cons address slot) (hashmap-storage-slots b)) value))
(defmethod backend-tload ((b hashmap-storage) address slot)
  (gethash (cons address slot) (hashmap-transient b) 0))
(defmethod backend-tstore ((b hashmap-storage) address slot value)
  (setf (gethash (cons address slot) (hashmap-transient b)) value))
(defmethod backend-emit-log ((b hashmap-storage) address topics data)
  (push (list address topics data) (hashmap-storage-logs b)))
(defmethod backend-logs ((b hashmap-storage)) (reverse (hashmap-storage-logs b)))
(defmethod backend-deduct-gas ((b hashmap-storage) gas) (incf (hashmap-used b) gas))
(defmethod backend-refund-gas ((b hashmap-storage) gas) (declare (ignore gas)))
(defmethod backend-gas-limit ((b hashmap-storage)) (1- (ash 1 64)))
(defmethod backend-gas-used ((b hashmap-storage)) (hashmap-used b))
(defmethod backend-gas-refunded ((b hashmap-storage)) 0)
(defmethod backend-account ((b hashmap-storage) address) (gethash address (hashmap-accounts b)))
(defmethod backend-warm-caller-account ((b hashmap-storage) address) (gethash address (hashmap-accounts b)))
(defmethod backend-set-code ((b hashmap-storage) address code)
  (setf (gethash address (hashmap-accounts b)) (make-account-info :code code)))
(defmethod backend-static-p ((b hashmap-storage)) (hashmap-static b))
(defmethod backend-block ((b hashmap-storage)) (hashmap-block b))
(defmethod backend-checkpoint ((b hashmap-storage)) nil)
(defmethod backend-commit ((b hashmap-storage) cp) (declare (ignore cp)))
(defmethod backend-revert ((b hashmap-storage) cp) (declare (ignore cp)))
(defmethod backend-config-commitment ((b hashmap-storage) address)
  (let ((info (gethash address (hashmap-accounts b)))) (if info (account-info-commitment info) 0)))
(defmethod backend-set-config-commitment ((b hashmap-storage) address commitment gas-kind)
  (declare (ignore gas-kind))
  (setf (gethash address (hashmap-accounts b))
        (make-account-info :code (octets-of #xef) :commitment commitment)))

;;; RECORDING-STORAGE: forwards to an inner backend and keeps the operation trace.

(defclass recording-storage (storage-backend)
  ((inner :initarg :inner :reader recording-inner)
   (trace :initform '() :accessor recording-trace-list)))

(defun make-recording-storage (inner) (make-instance 'recording-storage :inner inner))
(defun recording-storage-trace (b) (reverse (recording-trace-list b)))

(defmacro define-recorded (name (&rest args) &optional (record t))
  `(defmethod ,name ((b recording-storage) ,@args)
     (let ((result (,name (recording-inner b) ,@args)))
       ,@(when record
           `((push (list ',name ,@args result) (recording-trace-list b))))
       result)))

(define-recorded backend-sload (address slot))
(define-recorded backend-sstore (address slot value))
(define-recorded backend-tload (address slot))
(define-recorded backend-tstore (address slot value))
(define-recorded backend-emit-log (address topics data))
(define-recorded backend-deduct-gas (gas))
(define-recorded backend-refund-gas (gas))
(define-recorded backend-account (address))
(define-recorded backend-set-code (address code))
(define-recorded backend-config-commitment (address))
(define-recorded backend-set-config-commitment (address commitment gas-kind))
(define-recorded backend-logs () nil)
(define-recorded backend-gas-limit () nil)
(define-recorded backend-gas-used () nil)
(define-recorded backend-gas-refunded () nil)
(define-recorded backend-warm-caller-account (address) nil)
(define-recorded backend-static-p () nil)
(define-recorded backend-block () nil)
(define-recorded backend-checkpoint ())
(define-recorded backend-commit (cp))
(define-recorded backend-revert (cp))
