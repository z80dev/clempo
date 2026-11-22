(in-package #:clempo.evm)

;;; Message calls and contract creation: the frames below the interpreter.

(define-constant +max-nonce+ (1- (ash 1 64)))

(declaim (inline all-but-64th success-p))
(defun all-but-64th (gas)
  (declare (type gas gas))
  (- gas (floor gas 64)))

(defun success-p (status) (or (eq status :stop) (eq status :return)))

(defun create-address (sender nonce)
  (octets->integer (keccak256 (rlp-encode (list (integer->octets sender 20) nonce))) :start 12))

(defun create2-address (sender salt initcode)
  (octets->integer (keccak256 (octets-concat (octets-of #xff) (integer->octets sender 20)
                                             (integer->octets salt 32) (keccak256 initcode)))
                   :start 12))

(defun transfer (world from to value)
  "Move VALUE wei from FROM to TO (accounts or addresses); the caller checked the balance."
  (sub-balance world from value)
  (add-balance world to value))

(defun run-code-frame (frame)
  "Run FRAME (code or precompile). Returns (values status gas-left output)."
  (multiple-value-bind (status output) (run-frame frame)
    (values status (frame-gas frame) output)))

(defun call-message (kind caller target code-address value input gas depth static-p
                     &key (code nil code-p) disable-precompiles)
  "Run a message call frame: KIND is :call, :callcode, :delegatecall or :staticcall,
TARGET the storage context, CODE-ADDRESS where the code (or precompile) is. CODE, when
given, replaces CODE-ADDRESS's code (EIP-7702 delegation). Value moves only for :call.
Returns (values status gas-left output); state changes are rolled back unless the
status is :stop or :return."
  (declare (type gas gas) (type fixnum depth))
  (when (zerop depth) (check-evm-thread))
  (let* ((world *world*)
         (tracer *tracer*)
         (cp (checkpoint world))
         (target-account (get-account world target))
         (precompile (and (not disable-precompiles)
                          (find-precompile (spec-precompiles *spec*) code-address)))
         (frame (%make-frame :kind kind :address target :code-address code-address
                             :caller caller :value value :input input :gas gas :depth depth
                             :static-p static-p :account target-account
                             :code (cond (precompile +empty-code+)
                                         (code-p code)
                                         (t (get-code world code-address))))))
    (when tracer (trace-call-start tracer frame))
    (touch-account world target-account)
    (when (and (eq kind :call) (/= value 0))
      (transfer world caller target-account value))
    (multiple-value-bind (status gas-left output)
        (cond (precompile
               (multiple-value-bind (status output) (funcall (precompile-fn precompile) frame)
                 (values status (if (member status +success-statuses+) (frame-gas frame) 0)
                         output)))
              ((zerop (length (frame-code frame))) (values :stop gas +empty-code+))
              (t (run-code-frame frame)))
      (unless (success-p status) (rollback world cp))
      (when tracer (trace-call-end tracer frame status gas-left output))
      (values status gas-left output))))

(defun create-message (caller address value initcode gas depth &optional (kind :create))
  "Create a contract at ADDRESS by running INITCODE (the caller already bumped its nonce
and checked collision). KIND is :create or :create2. Returns (values status gas-left
output); output is the revert data on :revert, empty otherwise."
  (declare (type gas gas) (type fixnum depth))
  (when (zerop depth) (check-evm-thread))
  (let* ((world *world*)
         (spec *spec*)
         (gp (spec-gas spec))
         (tracer *tracer*)
         (cp (checkpoint world))
         (account (get-account world address))
         (frame (%make-frame :kind kind :address address :code-address address :caller caller
                             :value value :input +empty-code+ :gas gas :depth depth
                             :code initcode :account account)))
    (when tracer (trace-call-start tracer frame))
    (mark-created world account)
    (set-nonce world account 1)
    (touch-account world account)
    (when (/= value 0) (transfer world caller account value))
    (multiple-value-bind (status gas-left output)
        (if (zerop (length initcode))
            (values :stop gas +empty-code+)
            (run-code-frame frame))
      (declare (type gas gas-left))
      (multiple-value-bind (status gas-left output)
          (if (success-p status)
              (let ((deposit (* (gas-params-code-deposit gp) (length output))))
                (cond ((and (fork>= :london) (plusp (length output)) (= (aref output 0) #xef))
                       (values :invalid-code-prefix 0 +empty-code+))
                      ((> deposit gas-left) (values :out-of-gas 0 +empty-code+))
                      ((> (length output) (gas-params-max-code-size gp))
                       (values :code-too-large 0 +empty-code+))
                      (t (set-code world account output)
                         (values :return (- gas-left deposit) output))))
              (values status gas-left (if (eq status :revert) output +empty-code+)))
        (unless (success-p status) (rollback world cp))
        (when tracer (trace-call-end tracer frame status gas-left output))
        (values status gas-left output)))))

(defun collision-p (world account)
  "EIP-684/7610: creating over an account with a nonce, code or storage fails."
  (or (/= (account-nonce account) 0)
      (not (eq (account-code-hash account) +empty-code-hash+))
      (storage-nonempty-p world account)))

(defun call-roles (frame kind to value)
  "(values caller target value) of a KIND call to TO made by FRAME."
  (let ((self (frame-address frame)))
    (ecase kind
      (:call (values self to value))
      (:callcode (values self self value))
      (:delegatecall (values (frame-caller frame) self (frame-value frame)))
      (:staticcall (values self to 0)))))

(defun trace-unstarted (kind caller address code-address value input code gas depth
                        status gas-left)
  "Tell *TRACER* about a message that failed its prechecks (depth, balance, nonce,
collision) and so never ran: a frame that starts and ends with STATUS."
  (let ((tracer *tracer*)
        (frame (%make-frame :kind kind :caller caller :address address
                            :code-address code-address :value value :input input :code code
                            :gas gas :depth depth)))
    (trace-call-start tracer frame)
    (trace-call-end tracer frame status gas-left +empty-code+)))

;;; The CALL family. Static gas (the warm access) is paid, memory is already expanded and
;;; paid; FRAME-GAS holds the gas. Returns the word to push (1 or 0) or NIL for an
;;; exceptional halt.

(defun op-call (frame kind requested to value in-offset in-size out-offset out-size)
  (declare (type frame frame))
  (let* ((world *world*)
         (spec *spec*)
         (gp (spec-gas spec))
         (gas (frame-gas frame))
         (code-address to)
         (code nil)
         (delegated nil)
         (transfers (and (/= value 0) (or (eq kind :call) (eq kind :callcode))))
         (cost (cold-account-surcharge to)))
    (declare (type gas gas) (type fixnum cost))
    (when (fork>= :prague)
      (let ((target (delegation-target (get-code world to))))
        (when target
          (setf delegated t code-address target code (get-code world target))
          (incf cost (if (warm-address world target)
                         (gas-params-warm-read gp)
                         (gas-params-cold-account gp))))))
    (when transfers (incf cost (gas-params-call-value gp)))
    (when (and (eq kind :call) (/= value 0) (account-dead-p (get-account world to)))
      (incf cost (gas-params-new-account gp)))
    (when (< gas cost) (return-from op-call nil))
    (let ((child-gas (min requested (all-but-64th (- gas cost)))))
      (declare (type gas child-gas))
      (decf gas (+ cost child-gas))
      (let ((sub-gas (+ child-gas (if transfers (gas-params-call-stipend gp) 0)))
            (depth (frame-depth frame)))
        (declare (type gas sub-gas))
        (setf (frame-return-data frame) +empty-code+)
        (when (or (>= depth +max-depth+)
                  (and transfers (< (account-balance (frame-account frame)) value)))
          (setf (frame-gas frame) gas)
          (when *tracer*
            (multiple-value-bind (caller target value) (call-roles frame kind to value)
              (trace-unstarted kind caller target code-address value
                               (memory-slice frame in-offset in-size)
                               (or code (get-code world code-address)) sub-gas (1+ depth)
                               (if (>= depth +max-depth+) :call-too-deep :out-of-funds)
                               sub-gas)))
          (setf (frame-gas frame) (+ gas sub-gas))
          (return-from op-call 0))
        (let ((input (memory-slice frame in-offset in-size)))
          (setf (frame-gas frame) gas)    ; what the step left, seen by tracers
          (multiple-value-bind (status gas-left output)
              (multiple-value-bind (caller target value) (call-roles frame kind to value)
                (call-message kind caller target code-address value input sub-gas (1+ depth)
                              (or (frame-static-p frame) (eq kind :staticcall))
                              :code (or code (get-code world code-address))
                              :disable-precompiles delegated))
            (declare (type gas gas-left))
            (setf (frame-gas frame) (+ gas gas-left)
                  (frame-return-data frame) output)
            (incf (frame-child-gas frame) (- child-gas gas-left))
            (let ((n (min out-size (length output))))
              (when (plusp n)
                (replace (frame-memory frame) output :start1 out-offset :end2 n)))
            (if (success-p status) 1 0)))))))

(defun op-create (frame kind value offset size salt)
  "CREATE/CREATE2 once the opcode paid its initcode, memory and creation gas. Returns the
address (or 0) to push."
  (declare (type frame frame))
  (let* ((world *world*)
         (gas (frame-gas frame)))
    (declare (type gas gas))
    (let ((child-gas (all-but-64th gas))
          (account (frame-account frame))
          (self (frame-address frame))
          (depth (frame-depth frame)))
      (declare (type gas child-gas))
      (decf gas child-gas)
      (setf (frame-return-data frame) +empty-code+)
      (when (or (< (account-balance account) value)
                (= (account-nonce account) +max-nonce+)
                (>= depth +max-depth+))
        (setf (frame-gas frame) gas)
        (when *tracer*
          (let* ((initcode (memory-slice frame offset size))
                 (address (if (eq kind :create)
                              (create-address self (account-nonce account))
                              (create2-address self salt initcode))))
            (trace-unstarted kind self address address value +empty-code+ initcode child-gas
                             (1+ depth)
                             (cond ((>= depth +max-depth+) :call-too-deep)
                                   ((< (account-balance account) value) :out-of-funds)
                                   (t :nonce-overflow))
                             child-gas)))
        (setf (frame-gas frame) (+ gas child-gas))
        (return-from op-create 0))
      (let* ((initcode (memory-slice frame offset size))
             (address (if (eq kind :create)
                          (create-address self (account-nonce account))
                          (create2-address self salt initcode))))
        (when (fork>= :berlin) (warm-address world address))
        (increment-nonce world account)
        (when (collision-p world (get-account world address))
          (setf (frame-gas frame) gas)
          (incf (frame-child-gas frame) child-gas)
          (when *tracer*
            (trace-unstarted kind self address address value +empty-code+ initcode child-gas
                             (1+ depth) :create-collision 0))
          (return-from op-create 0))
        (setf (frame-gas frame) gas)
        (multiple-value-bind (status gas-left output)
            (create-message self address value initcode child-gas (1+ depth) kind)
          (declare (type gas gas-left))
          (setf (frame-gas frame) (+ gas gas-left))
          (incf (frame-child-gas frame) (- child-gas gas-left))
          (cond ((success-p status) address)
                (t (setf (frame-return-data frame) output) 0)))))))

(defun op-selfdestruct (frame beneficiary)
  "SELFDESTRUCT (EIP-6780 from Cancun) once the opcode paid its static gas and passed the
static-context check. Returns NIL for an exceptional halt."
  (declare (type frame frame))
  (let* ((world *world*)
         (spec *spec*)
         (gp (spec-gas spec))
         (gas (frame-gas frame))
         (account (frame-account frame))
         (balance (account-balance account))
         (cost 0))
    (declare (type gas gas) (type fixnum cost))
    (when (and (fork>= :berlin) (not (warm-address world beneficiary)))
      (incf cost (gas-params-cold-account gp)))
    (when (and (/= balance 0) (account-dead-p (get-account world beneficiary)))
      (incf cost (gas-params-new-account-selfdestruct gp)))
    (decf gas cost)
    (when (< gas 0) (return-from op-selfdestruct nil))
    (setf (frame-gas frame) gas)
    (when *tracer* (trace-selfdestruct *tracer* frame beneficiary balance))
    (cond ((fork>= :cancun)
           (transfer world account (get-account world beneficiary) balance)
           (when (created-p account)
             (set-balance world account 0)
             (mark-destructed world account)))
          (t
           (let ((refund (gas-params-selfdestruct-refund gp)))
             (when (and (mark-destructed world account) (plusp refund))
               (add-refund world refund)))
           (add-balance world beneficiary balance)
           (set-balance world account 0)))
    t))

(defun sstore-cost (world gp fork frame key new)
  "EIP-2200/2929/3529 SSTORE gas; applies the refund changes as a side effect.
Returns (values cost cold-p)."
  (declare (type gas-params gp) (type fixnum fork) (type frame frame))
  (let* ((account (frame-account frame))
         (original (account-storage-original world account key))
         (current (account-sload world account key))
         (berlin (>= fork (fork-index :berlin)))
         (warm (if berlin (gas-params-warm-read gp) (gas-params-sload gp)))
         (set (gas-params-sstore-set gp))
         (reset (gas-params-sstore-reset gp))
         (clear (gas-params-sstore-clear-refund gp))
         (cold nil)
         (cost 0))
    (declare (type fixnum cost warm set reset clear))
    (when (and berlin (not (warm-slot world (frame-address frame) key)))
      (setf cold t)
      (incf cost (gas-params-cold-sload gp)))
    (incf cost (if (and (= original current) (/= current new))
                   (if (zerop original) set reset)
                   warm))
    (when (/= current new)
      (when (and (/= original 0) (/= current 0) (= new 0)) (add-refund world clear))
      (when (and (/= original 0) (= current 0)) (add-refund world (- clear)))
      (when (= original new)
        (add-refund world (if (zerop original)
                              (gas-params-sstore-set-refund gp)
                              (gas-params-sstore-reset-refund gp)))))
    (values cost cold)))
