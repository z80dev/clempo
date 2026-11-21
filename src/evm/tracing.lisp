(defpackage #:clempo.tracing
  (:use #:cl #:clempo.base #:clempo.state #:clempo.evm)
  (:import-from #:clempo.json #:jobject #:jbool #:jget #:jobject-p)
  (:documentation "EVM tracers: geth's debug_trace* tracers and parity's trace_* family, as
the reference node (evm2-inspectors' TracingInspector and its geth and parity builders)
produces them.

A tracer records an arena of call nodes (and, when asked, opcode steps, memory, storage
accesses and the accounts the transaction loaded) from the CLEMPO.EVM hook generics, then
formats it as CLEMPO.JSON values. One tracer traces one transaction.

Call sequence:

  (let ((tracer (make-geth-tracer options)))     ; options: debug_trace*'s JSON object
    (with-tracer (tracer world)                   ; binds *TRACER*, installs the observer
      (apply-transaction handler world tx))       ; or any handler calling the hooks
    (geth-result tracer :block-number n :block-hash h :tx-hash x :tx-index i))

  (let ((tracer (make-parity-tracer '(:trace :state-diff :vm-trace))))
    (with-tracer (tracer world) (apply-transaction handler world tx))
    (parity-result tracer))                       ; trace_replayTransaction's TraceResults
  (parity-transaction-traces tracer :block-number n ...)  ; trace_transaction's list

  (let ((tracer (make-opcode-gas-tracer)))
    (with-tracer (tracer world) (apply-transaction handler world tx))
    (opcode-gas-result tracer))                   ; trace_transactionOpcodeGas's opcodeGas

The handler must call TRACE-TX-START before buying gas and TRACE-TX-END after
FINALIZE-TX (ETHEREUM-HANDLER does); the transaction's gas limit, sender, gas used and
output come from them unless GETH-RESULT is given :GAS-LIMIT :CALLER :GAS-USED :OUTPUT.
Tracing a block: one tracer per transaction; call logs are indexed from 0 within the
transaction, as the reference's are. Bad options signal TRACER-ERROR.")
  (:export
   #:make-geth-tracer #:make-parity-tracer #:make-opcode-gas-tracer
   #:attach-tracer #:detach-tracer #:with-tracer
   #:geth-result #:parity-result #:parity-transaction-traces #:opcode-gas-result
   #:tracer-error #:tracer-error-message #:tracer-error-kind
   #:revert-reason #:opcode-name))

(in-package #:clempo.tracing)

(define-condition tracer-error (error)
  ((kind :initarg :kind :reader tracer-error-kind)
   (message :initarg :message :reader tracer-error-message))
  (:report (lambda (c s) (write-string (tracer-error-message c) s)))
  (:documentation "Unusable tracer options. KIND is :unsupported-tracer, :invalid-config or
:js-tracer; MESSAGE is the reference's error text."))

(defun tracer-fail (kind fmt &rest args)
  (error 'tracer-error :kind kind :message (apply #'format nil fmt args)))

;;; Opcode names and stack outputs as evm2's opcode table has them (DIFFICULTY at 0x44,
;;; and the opcodes evm2 knows that clempo does not run yet).

(defparameter *op-names*
  (let ((v (make-array 256 :initial-element nil)))
    (dotimes (i 256)
      (let ((info (op-info i)))
        (when info (setf (svref v i) (string-upcase (symbol-name (op-info-name info)))))))
    (loop for (op name) in '((#x44 "DIFFICULTY") (#x4b "SLOTNUM") (#xe6 "DUPN")
                             (#xe7 "SWAPN") (#xe8 "EXCHANGE"))
          do (setf (svref v op) name))
    v))

(defun opcode-name (op)
  "evm2's name of opcode byte OP, or NIL when it has none."
  (svref *op-names* op))

(defun op-outputs (op)
  "The stack words OP leaves for parity's vmTrace `push` (evm2's stack outputs)."
  (cond ((<= #x80 op #x9f) (+ 2 (logand op 15)))          ; DUPn and SWAPn: n+1
        ((or (null (opcode-name op))
             (member op '(#x00 #x37 #x39 #x3c #x3e #x50 #x52 #x53 #x55 #x56 #x57 #x5b #x5d #x5e
                          #xe7 #xe8 #xf3 #xfd #xfe #xff))
             (<= #xa0 op #xa4))
         0)
        (t 1)))

(defun vm-op-name (op)
  (or (opcode-name op) (format nil "UNKNOWN(0x~2,'0X)" op)))

(defun struct-log-op-name (op)
  (or (opcode-name op) (format nil "opcode 0x~(~x~) not defined" op)))

(defun call-op-p (op) (member op '(#xf0 #xf1 #xf2 #xf4 #xf5 #xfa)))

(defun memory-op-p (op)
  "evm2's OpCode::modifies_memory: a later step may reuse this one's memory otherwise."
  (or (member op '(#x3c #x51 #x52 #x53 #x5e #x20 #x39 #x37 #x3e #xf1 #xf2 #xf4 #xfa
                   #xf3 #xfd #xf0 #xf5))
      (<= #xa0 op #xa4)))

(defun static-gas (op)
  "evm2's static gas of OP, charged before its stack is checked."
  (let ((gp (spec-gas *spec*)))
    (case op
      ((#x31 #x3b #x3c #x3f #xf1 #xf2 #xf4 #xfa)
       (if (fork>= :berlin) 100 (gas-params-account-read gp)))
      (#x54 (if (fork>= :berlin) 100 (gas-params-sload gp)))
      (#xff (gas-params-selfdestruct gp))
      (#x20 (gas-params-keccak gp))
      (#x40 (gas-params-blockhash gp))
      ((#xa0 #xa1 #xa2 #xa3 #xa4) (gas-params-log gp))
      ((#xf0 #xf5) (gas-params-create gp))
      (t (let* ((info (op-info op)) (gas (and info (op-info-gas info))))
           (if (integerp gas) gas 0))))))

;;; Halt statuses. Clempo's status keywords map onto evm2's InstrStop; each stop has its
;;; Debug name (struct logs print "Some(Name)"), geth and parity messages and a class.
;;; NIL is a frame without a status (evm2's placeholder root around a Tempo AA batch): not
;;; successful, yet no error.

(defparameter *stops*
  ;; stop debug-name geth parity class
  '((nil "None" nil nil :none)
    (:stop "Stop" nil nil :success)
    (:return "Return" nil nil :success)
    (:selfdestruct "SelfDestruct" nil nil :success)
    (:revert "Revert" "execution reverted" "Reverted" :revert)
    (:call-too-deep "CallTooDeep" "max call depth exceeded" "Max call depth exceeded" :revert)
    (:out-of-funds "OutOfFunds" "insufficient balance for transfer"
     "Insufficient balance for transfer" :revert)
    (:nonce-overflow "NonceOverflow" "nonce uint64 overflow" "Nonce overflow" :revert)
    (:out-of-gas "OutOfGas" "out of gas" "Out of gas" :oog)
    (:memory-oog "MemoryOOG" "out of gas: out of memory" "Out of gas" :oog)
    (:precompile-oog "PrecompileOOG" "out of gas" "Out of gas" :oog)
    (:reentrancy-sentry-oog "ReentrancySentryOOG" "out of gas: not enough gas for reentrancy sentry"
     "Out of gas" :oog)
    (:call-not-allowed-inside-static "CallNotAllowedInsideStatic" "write protection"
     "Mutable Call In Static Context" :halt)
    (:state-change-during-static-call "StateChangeDuringStaticCall" "write protection"
     "Mutable Call In Static Context" :halt)
    (:opcode-not-found "OpcodeNotFound" "invalid opcode" "Bad instruction" :halt)
    (:invalid-fe-opcode "InvalidFEOpcode" "invalid opcode: INVALID" "Bad instruction" :halt)
    (:not-activated "NotActivated" "invalid opcode" "Bad instruction" :halt)
    (:invalid-jump "InvalidJump" "invalid jump destination" "Bad jump destination" :halt)
    (:stack-underflow "StackUnderflow" "stack underflow" "Stack underflow" :halt)
    (:stack-overflow "StackOverflow" "Out of stack" "Out of stack" :halt)
    (:out-of-offset "OutOfOffset" "return data out of bounds" "Out of bounds" :halt)
    (:create-collision "CreateCollision" "contract address collision"
     "Contract address collision" :halt)
    (:precompile-error "PrecompileError" "precompiled failed" "Built-in failed" :halt)
    (:create-contract-size-limit "CreateContractSizeLimit" "max code size exceeded"
     "Out of gas" :halt)
    (:create-contract-starting-with-ef "CreateContractStartingWithEF"
     "invalid code: must not begin with 0xef" "Invalid code" :halt)))

(defun stop-entry (stop)
  (or (assoc stop *stops*)
      ;; An unknown halt prints as its Debug name, like the reference's fallback.
      (let ((name (remove #\- (string-capitalize (symbol-name stop)))))
        (list stop name name name :halt))))

(defun stop-class (stop) (fifth (stop-entry stop)))
(defun stop-success-p (stop) (eq (stop-class stop) :success))
(defun stop-halt-p (stop) (member (stop-class stop) '(:halt :oog)))
(defun stop-message (stop style)
  (unless (stop-success-p stop)
    (if (eq style :parity) (fourth (stop-entry stop)) (third (stop-entry stop)))))

(defun evm2-stop (status op &key precompile)
  "The evm2 stop for clempo's STATUS ending the opcode OP (or a precompile frame)."
  (case status
    (:out-of-gas (if precompile :precompile-oog :out-of-gas))
    (:memory-out-of-gas :memory-oog)
    (:precompile-failure :precompile-error)
    (:invalid-opcode (cond ((eql op #xfe) :invalid-fe-opcode)
                           ((and op (opcode-name op)) :not-activated)
                           (t :opcode-not-found)))
    (:static-violation (if (member op '(#xf1 #xf2))
                           :call-not-allowed-inside-static
                           :state-change-during-static-call))
    (:return-data-out-of-bounds :out-of-offset)
    (:invalid-code-prefix :create-contract-starting-with-ef)
    (:code-too-large :create-contract-size-limit)
    (t status)))

;;; Revert reasons, as geth's UnpackRevert: Error(string) and Panic(uint256).

(defparameter *panic-messages*
  '((#x00 . "generic panic") (#x01 . "assert(false)")
    (#x11 . "arithmetic underflow or overflow") (#x12 . "division or modulo by zero")
    (#x21 . "enum overflow") (#x22 . "invalid encoded storage byte array accessed")
    (#x31 . "out-of-bounds array access; popping on an empty array")
    (#x32 . "out-of-bounds access of an array or bytesN") (#x41 . "out of memory")
    (#x51 . "uninitialized function")))

(defun utf8-lossy (octets start end)
  "Decode OCTETS[START, END) as UTF-8, replacing malformed sequences with U+FFFD."
  (with-output-to-string (s)
    (let ((i start))
      (flet ((cont (k) (and (< k end) (= (logand (aref octets k) #xc0) #x80))))
        (loop while (< i end)
              do (let ((b (aref octets i)))
                   (multiple-value-bind (n min init)
                       (cond ((< b #x80) (values 1 0 b))
                             ((<= #xc2 b #xdf) (values 2 #x80 (logand b #x1f)))
                             ((<= #xe0 b #xef) (values 3 #x800 (logand b #x0f)))
                             ((<= #xf0 b #xf4) (values 4 #x10000 (logand b #x07)))
                             (t (values 0 0 0)))
                     (let ((cp init) (ok (plusp n)) (used 1))
                       (when ok
                         (loop for k from 1 below n
                               do (if (cont (+ i k))
                                      (setf cp (logior (ash cp 6) (logand (aref octets (+ i k)) #x3f))
                                            used (1+ k))
                                      (progn (setf ok nil) (return))))
                         (when (and ok (or (< cp min) (> cp #x10ffff) (<= #xd800 cp #xdfff)))
                           (setf ok nil used 1)))
                       (cond (ok (write-char (code-char cp) s) (incf i n))
                             (t (write-char (code-char #xfffd) s) (incf i (max 1 used))))))))))))

(defun revert-reason (output)
  "The revert reason geth's callTracer shows for OUTPUT, or NIL."
  (declare (type octets output))
  (let ((n (length output)))
    (when (>= n 36)
      (let ((selector (octets->integer output :end 4))
            (word (octets->integer output :start 4 :end 36)))
        (case selector
          (#x08c379a0
           (let ((body (- n 4)))
             (when (<= (+ word 32) body)
               (let ((len (octets->integer output :start (+ 4 word) :end (+ 36 word))))
                 (when (<= (+ word 32 len) body)
                   (let ((reason (utf8-lossy output (+ 36 word) (+ 36 word len))))
                     (unless (every (lambda (c) (char= c (code-char 0))) reason)
                       reason)))))))
          (#x4e487b71
           (or (cdr (assoc word *panic-messages*))
               (format nil "unknown panic code: 0x~(~x~)" word))))))))

;;; JSON helpers

(defun obj (&rest kvs)
  "A JSON object of KVS, leaving out pairs whose value is NIL."
  (cons :object (loop for (k v) on kvs by #'cddr when v collect (cons k v))))

(defun hex-address (a) (integer->hex a :bytes 20))
(defun hex-word (w) (integer->hex w :bytes 32))
(defun hex-bytes (o) (octets->hex o))
(defun hex-qty (n) (quantity->hex n))

(defun vec (list) (coerce list 'simple-vector))

(defun adjustable () (make-array 4 :adjustable t :fill-pointer 0))

;;; The arena

(defstruct (call-log (:constructor make-call-log (address topics data position index)))
  address topics data position index)

(defstruct (node (:constructor make-node))
  (idx 0 :type fixnum)
  (parent nil)
  (children (adjustable))
  (depth 0 :type fixnum)
  (kind :call)
  (caller 0)
  (address 0)
  (storage-address 0)
  (value 0)
  (input +empty-code+)
  (bytecode +empty-code+)
  (gas-limit 0)
  (gas-used 0)
  (output +empty-code+)
  (stop nil)
  (success nil)
  (precompile-p nil)
  (sd-address nil)
  (sd-target nil)
  (sd-value nil)
  (logs (adjustable))
  (ordering (adjustable))                ; (:step . i) (:call . child-position) (:log . i)
  (steps (adjustable))
  (refund-base 0)
  (log-mark 0)
  (halt nil))                            ; the evm2 stop of the step that halted the frame

(defstruct (step-record (:conc-name step-) (:constructor make-step))
  pc op gas refund stack push-stack memory return-data
  storage                                ; (key . value) of an SLOAD or SSTORE
  (cost 0) stop gas-after
  write-range                            ; vmTrace: (start . end) of memory to report
  mem store)                             ; vmTrace: (off . data), (key . val)

(defstruct (pending (:constructor make-pending (node step before op)))
  node step before after op (child-gas 0))

(defstruct (account-record (:constructor make-account-record (account)))
  account
  pre-exists pre-nonce pre-balance pre-code-hash pre-code
  post-exists post-nonce post-balance post-code-hash post-code
  (slots (make-hash-table))              ; slot -> value at transaction start
  (post-slots (make-hash-table)))

(defclass tracer ()
  ((steps-p :initarg :steps :initform nil)
   (stack-mode :initarg :stack :initform :none)   ; :none :full :pushes
   (memory-p :initarg :memory :initform nil)
   (storage-p :initarg :storage :initform nil)
   (return-data-p :initarg :return-data :initform nil)
   (deltas-p :initarg :deltas :initform nil)
   (state-p :initarg :state :initform nil)
   (four-byte-p :initarg :four-byte :initform nil)
   (opcode-gas-p :initarg :opcode-gas :initform nil)
   (nodes :initform (adjustable) :reader tracer-nodes)
   (active :initform '())                         ; ((node . frame) ...), innermost first
   (pending :initform '())
   (log-count :initform 0)
   (world :initform nil)
   (previous-observer :initform nil)
   (precompiles :initform nil)
   (accounts :initform (make-hash-table))
   (post-captured :initform nil)
   (four-byte :initform (make-hash-table :test 'equal))
   (opcode-counts :initform (make-hash-table))
   (opcode-gas :initform (make-hash-table))
   (tx-gas-limit :initform nil)
   (tx-sender :initform nil)
   (tx-gas-used :initform nil)
   (tx-output :initform nil))
  (:documentation "Records one transaction for the tracer outputs below."))

(defclass geth-tracer (tracer)
  ((kind :initarg :kind :reader geth-tracer-kind)   ; :default :call :flat-call :prestate ...
   (config :initarg :config :reader geth-tracer-config)
   (children :initarg :children :initform '())))   ; mux: ((name kind config) ...)

(defclass parity-tracer (tracer)
  ((types :initarg :types :reader parity-tracer-types)))

(defclass opcode-gas-tracer (tracer) ())

;;; Attaching

(defun attach-tracer (tracer world)
  "Prepare TRACER to trace a transaction on WORLD: install the account observer it needs.
Bind *TRACER* to it around the transaction (WITH-TRACER does both)."
  (with-slots (state-p previous-observer) tracer
    (setf (slot-value tracer 'world) world)
    (when state-p
      (setf previous-observer (world-observer world)
            (world-observer world) (make-observer tracer world)))
    tracer))

(defun detach-tracer (tracer world)
  "Remove what ATTACH-TRACER installed."
  (with-slots (state-p previous-observer) tracer
    (when state-p
      (capture-post-state tracer)
      (setf (world-observer world) previous-observer)))
  tracer)

(defmacro with-tracer ((tracer world) &body body)
  "Run BODY with TRACER attached to WORLD and bound to *TRACER*."
  (let ((tr (gensym "TRACER")) (w (gensym "WORLD")))
    `(let ((,tr ,tracer) (,w ,world))
       (attach-tracer ,tr ,w)
       (unwind-protect (let ((*tracer* ,tr)) ,@body)
         (detach-tracer ,tr ,w)))))

(defun make-observer (tracer world)
  (let ((accounts (slot-value tracer 'accounts)))
    (lambda (account slot value)
      (let ((rec (or (gethash (account-address account) accounts)
                     (setf (gethash (account-address account) accounts)
                           (snapshot-pre world account)))))
        (when slot
          (let ((slots (account-record-slots rec)))
            (unless (nth-value 1 (gethash slot slots))
              (setf (gethash slot slots) value))))))))

(defun snapshot-pre (world account)
  (let ((rec (make-account-record account)))
    (setf (account-record-pre-exists rec) (account-exists account)
          (account-record-pre-nonce rec) (account-nonce account)
          (account-record-pre-balance rec) (account-balance account)
          (account-record-pre-code-hash rec) (account-code-hash account)
          (account-record-pre-code rec) (account-code world account))
    rec))

(defun capture-post-state (tracer)
  "Snapshot every loaded account as the finished transaction left it."
  (with-slots (world accounts post-captured) tracer
    (unless (or post-captured (null world))
      (setf post-captured t)
      (let ((observer (world-observer world)))
        (setf (world-observer world) nil)
        (unwind-protect
             (loop for rec being the hash-values of accounts
                   for account = (account-record-account rec)
                   do (setf (account-record-post-exists rec) (account-exists account)
                            (account-record-post-nonce rec) (account-nonce account)
                            (account-record-post-balance rec) (account-balance account)
                            (account-record-post-code-hash rec) (account-code-hash account)
                            (account-record-post-code rec) (account-code world account))
                      (loop for slot being the hash-keys of (account-record-slots rec)
                            do (setf (gethash slot (account-record-post-slots rec))
                                     (account-sload world account slot))))
          (setf (world-observer world) observer))))))

;;; Recording

(defun current-node (tracer) (car (first (slot-value tracer 'active))))

(defun precompile-address-p (tracer address)
  (with-slots (precompiles) tracer
    (unless precompiles
      (setf precompiles (make-hash-table))
      (dolist (a (precompile-addresses (spec-precompiles *spec*)))
        (setf (gethash a precompiles) t)))
    (values (gethash address precompiles))))

(defun create-kind-p (kind) (or (eq kind :create) (eq kind :create2)))
(defun delegate-kind-p (kind) (or (eq kind :delegatecall) (eq kind :callcode)))

(defmethod trace-tx-start ((tracer tracer) world tx)
  (declare (ignore world))
  (with-slots (tx-gas-limit tx-sender) tracer
    (setf tx-gas-limit (evm-tx-gas-limit tx)
          tx-sender (evm-tx-sender tx))))

(defmethod trace-tx-end ((tracer tracer) world tx result)
  (declare (ignore world tx))
  (with-slots (tx-gas-used tx-output state-p) tracer
    (setf tx-gas-used (tx-result-gas-used result)
          tx-output (or (tx-result-output result) +empty-code+))
    (when state-p (capture-post-state tracer))))

(defmethod trace-call-start ((tracer tracer) frame)
  (with-slots (nodes active pending log-count four-byte-p four-byte world) tracer
    (let* ((kind (frame-kind frame))
           (parent-entry (first active))
           (createp (create-kind-p kind)))
      ;; The step that started this message has charged everything by now.
      (let ((p (first pending)))
        (when (and p parent-entry (eq (pending-node p) (car parent-entry))
                   (null (pending-after p)))
          (setf (pending-after p) (+ (frame-gas (cdr parent-entry))
                                     (if createp (frame-gas frame) 0)))))
      (multiple-value-bind (from to)
          (if (delegate-kind-p kind)
              (values (frame-address frame) (frame-code-address frame))
              (values (frame-caller frame) (frame-address frame)))
        (let* ((input (if createp (frame-code frame) (frame-input frame)))
               (parent (and parent-entry (car parent-entry)))
               (node (make-node :idx (fill-pointer nodes)
                                :parent (and parent (node-idx parent))
                                ;; one below its parent: a Tempo AA batch's calls run at
                                ;; depth 0 under a wrapper frame that is not an EVM message
                                :depth (if parent (1+ (node-depth parent)) (frame-depth frame))
                                :kind kind
                                :caller from :address to
                                :storage-address (frame-address frame)
                                :value (frame-value frame) :input input
                                :bytecode (frame-code frame)
                                :gas-limit (frame-gas frame)
                                :precompile-p (and (not createp)
                                                   (precompile-address-p tracer to))
                                :refund-base (world-refund *world*)
                                :log-mark log-count)))
          (vector-push-extend node nodes)
          (when parent
            (vector-push-extend (cons :call (fill-pointer (node-children parent)))
                                (node-ordering parent))
            (vector-push-extend (node-idx node) (node-children parent)))
          (when (and four-byte-p (not createp) (>= (length input) 4)
                     (not (precompile-address-p tracer (frame-code-address frame))))
            (let ((key (cons (octets->integer input :end 4) (- (length input) 4))))
              (incf (gethash key (slot-value tracer 'four-byte) 0))))
          (push (cons node frame) active))))))

(defmethod trace-call-end ((tracer tracer) frame status gas-left output)
  (with-slots (active pending log-count opcode-gas-p) tracer
    (let* ((node (car (pop active)))
           (stop (or (and (node-halt node) (not (member status '(:stop :return :revert)))
                          (node-halt node))
                     (evm2-stop status nil :precompile (node-precompile-p node)))))
      (setf (node-gas-used node) (max 0 (- (node-gas-limit node) gas-left))
            (node-stop node) stop
            (node-success node) (stop-success-p stop)
            (node-output node) (or output +empty-code+))
      (when (node-error-p node) (setf log-count (node-log-mark node)))
      ;; trace_transactionOpcodeGas leaves the child's gas out of its CALL or CREATE.
      (when (and opcode-gas-p (plusp (frame-depth frame)))
        (let ((p (first pending))
              (op (ecase (frame-kind frame)
                    (:call #xf1) (:callcode #xf2) (:delegatecall #xf4) (:staticcall #xfa)
                    (:create #xf0) (:create2 #xf5))))
          (when (and p (eql (pending-op p) op))
            (incf (pending-child-gas p) (node-gas-used node))))))))

(defun stack-top (stack sp k)
  "The word K below the top of STACK[0..SP), or NIL."
  (and (< k sp) (svref stack (- sp k 1))))

(defun memory-write-range (op stack sp)
  "Parity vmTrace: the memory range OP writes (or MLOAD reads), from its operands."
  (flet ((back (k) (let ((v (stack-top stack sp k))) (and v (< v most-positive-fixnum) v))))
    (multiple-value-bind (offset size)
        (case op
          ((#x51 #x52) (values (back 0) 32))
          (#x53 (values (back 0) 1))
          ((#x37 #x39 #x3e #x5e) (values (back 0) (back 2)))
          (#x3c (values (back 1) (back 3)))
          ((#xf1 #xf2) (values (back 5) (back 6)))
          ((#xf4 #xfa) (values (back 4) (back 5))))
      (when (and offset size (/= size 0))
        (cons offset (+ offset size))))))

(defmethod trace-step ((tracer tracer) frame pc op gas sp stack)
  (with-slots (steps-p stack-mode memory-p storage-p return-data-p deltas-p world pending)
      tracer
    (let ((node (current-node tracer)) (step nil))
      (when steps-p
        (let ((steps (node-steps node)))
          (setf step (make-step :pc pc :op op :gas gas
                                :refund (let ((r (- (world-refund *world*) (node-refund-base node))))
                                          (ldb (byte 64 0) r))
                                :stack (when (eq stack-mode :full) (subseq stack 0 sp))
                                :return-data (if return-data-p (frame-return-data frame) +empty-code+)))
          (when memory-p
            (let ((prev (and (plusp (fill-pointer steps)) (aref steps (1- (fill-pointer steps))))))
              (setf (step-memory step)
                    (if (and prev (not (memory-op-p (step-op prev))) (step-memory prev))
                        (step-memory prev)
                        (subseq (frame-memory frame) 0 (frame-mem-size frame))))))
          (when (and storage-p (or (= op #x54) (= op #x55)) (plusp sp))
            (let ((key (svref stack (1- sp))))
              (setf (step-storage step)
                    (if (= op #x55)
                        (and (>= sp 2) (cons key (svref stack (- sp 2))))
                        (cons key (let ((observer (world-observer world)))
                                    (setf (world-observer world) nil)
                                    (unwind-protect (account-sload world (frame-account frame) key)
                                      (setf (world-observer world) observer))))))))
          (when deltas-p
            (setf (step-write-range step) (memory-write-range op stack sp))
            (when (and (= op #x55) (>= sp 2))
              (setf (step-store step) (cons (svref stack (1- sp)) (svref stack (- sp 2))))))
          (vector-push-extend (cons :step (fill-pointer steps)) (node-ordering node))
          (vector-push-extend step steps)))
      (push (make-pending node step gas op) pending))))

(defun step-stop-and-cost (status op before gas-end after &optional static-p)
  "evm2's stop and gas cost for a step: BEFORE and GAS-END are the gas around it, AFTER the
gas once a CALL or CREATE charged everything (NIL otherwise). STATIC-P: the frame is static."
  (let ((stop (and status (evm2-stop status op))))
    (cond ((member stop '(:stack-underflow :stack-overflow))
           ;; evm2 charges static gas before it checks the stack.
           (let ((static (static-gas op)))
             (if (< before static)
                 (values :out-of-gas before)
                 (values stop static))))
          ((and stop (= op #x55) (not (stop-success-p stop)))
           ;; evm2's SSTORE checks the static context, then the EIP-2200 sentry, before
           ;; charging anything.
           (cond (static-p (values :state-change-during-static-call 0))
                 ((and (fork>= :berlin) (<= before (gas-params-call-stipend (spec-gas *spec*))))
                  (values :reentrancy-sentry-oog 0))
                 ((eq (stop-class stop) :oog) (values stop before))
                 (t (values stop (max 0 (- before gas-end))))))
          ((and stop (eq (stop-class stop) :oog)) (values stop before))
          (after (values stop (max 0 (- before after))))
          (t (values stop (max 0 (- before gas-end)))))))

(defmethod trace-step-end ((tracer tracer) frame pc op gas sp stack status)
  (declare (ignore pc))
  (with-slots (pending stack-mode deltas-p opcode-gas-p opcode-counts opcode-gas) tracer
    (let ((p (pop pending)))
      (multiple-value-bind (stop cost)
          (step-stop-and-cost status op (pending-before p) gas nil (frame-static-p frame))
        (when (and stop (stop-halt-p stop))
          (setf (node-halt (pending-node p)) stop))
        (when (and opcode-gas-p (opcode-name op))
          (incf (gethash op opcode-counts 0))
          (incf (gethash op opcode-gas 0) (max 0 (- cost (pending-child-gas p)))))
        (let ((step (pending-step p)))
          (when step
            (setf (step-stop step) stop
                  (step-cost step) (if (pending-after p)
                                       (max 0 (- (pending-before p) (pending-after p)))
                                       cost)
                  (step-gas-after step) gas)
            (when (eq stack-mode :pushes)
              (let ((n (if (opcode-name op) (op-outputs op) 0)))
                (setf (step-push-stack step) (subseq stack (max 0 (- sp n)) sp))))
            (when (and deltas-p (not (and stop (stop-halt-p stop))))
              (let ((range (step-write-range step)))
                (when range
                  (when (call-op-p op)
                    (setf range (cons (car range)
                                      (+ (car range)
                                         (min (- (cdr range) (car range))
                                              (length (frame-return-data frame)))))))
                  (when (and (< (car range) (cdr range))
                             (<= (cdr range) (frame-mem-size frame)))
                    (setf (step-mem step)
                          (cons (car range) (subseq (frame-memory frame) (car range) (cdr range))))))))))))))

(defmethod trace-log ((tracer tracer) frame log)
  (declare (ignore frame))
  ;; A log outside any frame (a Tempo key authorization before the first call) takes an
  ;; index but has no trace to attach to, as in evm2's tracing inspector.
  (with-slots (log-count) tracer
    (let ((node (current-node tracer)))
      (when node
        (vector-push-extend (cons :log (fill-pointer (node-logs node))) (node-ordering node))
        (vector-push-extend (make-call-log (log-entry-address log) (log-entry-topics log)
                                           (log-entry-data log)
                                           (fill-pointer (node-children node)) log-count)
                            (node-logs node)))
      (incf log-count))))

(defmethod trace-selfdestruct ((tracer tracer) frame beneficiary balance)
  (let ((node (current-node tracer)))
    (when node
      (setf (node-sd-address node) (frame-address frame)
            (node-sd-target node) beneficiary
            (node-sd-value node) balance))))

;;; Shared views of the arena

(defun nodes-list (tracer) (coerce (tracer-nodes tracer) 'list))
(defun node-at (tracer i) (aref (tracer-nodes tracer) i))
(defun node-error-p (node) (not (member (stop-class (node-stop node)) '(:success :none))))
(defun node-revert-p (node) (eq (node-stop node) :revert))
(defun node-selfdestruct-p (node) (and (node-sd-target node) t))

(defun effective-p (tracer node)
  "NODE and all its callers ended without an error, so its effects stand."
  (loop for n = node then (node-at tracer (node-parent n))
        never (node-error-p n)
        while (node-parent n)))

(defun root-overrides (tracer &key gas-limit caller)
  "The root's gas limit and caller as geth's tracers show them: the transaction's."
  (with-slots (tx-gas-limit tx-sender) tracer
    (values (or gas-limit tx-gas-limit) (or caller tx-sender))))

;;; geth: callTracer

(defun call-frame-json (node &key logs gas caller gas-used calls)
  (let* ((createp (create-kind-p (node-kind node)))
         (success (node-success node))
         (output (node-output node)))
    (obj "from" (hex-address (or caller (node-caller node)))
         "gas" (hex-qty (or gas (node-gas-limit node)))
         "gasUsed" (hex-qty (or gas-used (node-gas-used node)))
         "to" (unless (and createp (not success)) (hex-address (node-address node)))
         "input" (hex-bytes (node-input node))
         "output" (when (plusp (length output)) (hex-bytes output))
         "error" (unless success (stop-message (node-stop node) :geth))
         "revertReason" (unless success (revert-reason output))
         "calls" (when calls (vec calls))
         "logs" (when (and logs (plusp (fill-pointer (node-logs node))))
                  (map 'simple-vector
                       (lambda (l)
                         (obj "address" (hex-address (call-log-address l))
                              "topics" (map 'simple-vector #'hex-word (call-log-topics l))
                              "data" (hex-bytes (call-log-data l))
                              "position" (hex-qty (if (eq logs :top) 0 (call-log-position l)))
                              "index" (hex-qty (call-log-index l))))
                       (node-logs node)))
         "value" (unless (eq (node-kind node) :staticcall) (hex-qty (node-value node)))
         "type" (string-upcase (symbol-name (node-kind node))))))

(defun selfdestruct-frame-json (node)
  (obj "from" (hex-address (node-sd-address node)) "gas" "0x0" "gasUsed" "0x0"
       "to" (hex-address (node-sd-target node)) "input" "0x"
       "value" (hex-qty (node-sd-value node)) "type" "SELFDESTRUCT"))

(defun call-tracer-json (tracer config &key gas-limit caller gas-used)
  (multiple-value-bind (gas from) (root-overrides tracer :gas-limit gas-limit :caller caller)
    (let* ((with-log (eq (jget config "withLog") :true))
           (gas-used (or gas-used (slot-value tracer 'tx-gas-used)))
           (root (node-at tracer 0)))
      (if (eq (jget config "onlyTopCall") :true)
          (call-frame-json root :logs (and with-log (not (node-error-p root)) :top)
                                :gas gas :caller from :gas-used gas-used)
          (labels ((build (node visible root-p)
                     (let* ((visible (and visible (not (node-error-p node))))
                            (calls (append (loop for i across (node-children node)
                                                 collect (build (node-at tracer i) visible nil))
                                           (when (node-selfdestruct-p node)
                                             (list (selfdestruct-frame-json node))))))
                       (if root-p
                           (call-frame-json node :logs visible :calls calls :gas gas
                                                 :caller from :gas-used gas-used)
                           (call-frame-json node :logs visible :calls calls)))))
            (build root with-log t))))))

;;; geth: default struct logger

(defun option-true-p (options key) (eq (jget options key) :true))

(defun option-enabled-p (options enable disable)
  "geth's enableX / disableX pair: enableX wins, then not disableX, default off."
  (let ((e (jget options enable)) (d (jget options disable)))
    (cond ((member e '(:true :false)) (eq e :true))
          ((member d '(:true :false)) (eq d :false))
          (t nil))))

(defun memory-chunks (memory)
  (let ((n (ceiling (length memory) 32)))
    (when (plusp n)
      (let ((padded (make-octets (* 32 n))))
        (replace padded memory)
        (vec (loop for i below n collect (octets->hex padded :start (* 32 i) :end (* 32 (1+ i)))))))))

(defun struct-logs (tracer options)
  (let ((stack-p (not (option-true-p options "disableStack")))
        (storage-p (not (option-true-p options "disableStorage")))
        (memory-p (option-enabled-p options "enableMemory" "disableMemory"))
        (return-data-p (option-enabled-p options "enableReturnData" "disableReturnData"))
        (limit (let ((l (jget options "limit"))) (and (integerp l) (plusp l) l)))
        (storages (make-hash-table))
        (json-len 0)
        (logs '()))
    (block done
      (labels ((walk (node)
                 (let ((ordering (node-ordering node)) (i 0) (n (fill-pointer (node-ordering node))))
                   (loop while (< i n)
                         do (let ((entry (aref ordering i)))
                              (incf i)
                              (when (eq (car entry) :step)
                                (when (and limit (> json-len limit)) (return-from done))
                                (let* ((step (aref (node-steps node) (cdr entry)))
                                       (log (step-json step node stack-p storage-p memory-p
                                                       return-data-p storages)))
                                  (when limit
                                    (incf json-len (length (clempo.json:to-json-string log))))
                                  (push log logs)
                                  ;; the child a call step started follows it in the ordering
                                  (when (and (call-op-p (step-op step)) (< i n)
                                             (eq (car (aref ordering i)) :call))
                                    (let ((child (aref (node-children node) (cdr (aref ordering i)))))
                                      (incf i)
                                      (walk (node-at tracer child)))))))))))
        (walk (node-at tracer 0))))
    (vec (nreverse logs))))

(defun step-json (step node stack-p storage-p memory-p return-data-p storages)
  (let* ((stop (step-stop step))
         (error-p (and stop (stop-halt-p stop)))
         (storage
           (when (and storage-p (not error-p) (step-storage step))
             (let ((table (or (gethash (node-storage-address node) storages)
                              (setf (gethash (node-storage-address node) storages)
                                    (make-hash-table)))))
               (setf (gethash (car (step-storage step)) table) (cdr (step-storage step)))
               (cons :object
                     (loop for k in (sort (loop for k being the hash-keys of table collect k) #'<)
                           collect (cons (hex-word k) (hex-word (gethash k table)))))))))
    (obj "pc" (step-pc step)
         "op" (struct-log-op-name (step-op step))
         "gas" (step-gas step)
         "gasCost" (step-cost step)
         "depth" (1+ (node-depth node))
         "error" (when error-p (format nil "Some(~A)" (second (stop-entry stop))))
         "stack" (when stack-p (map 'simple-vector #'hex-qty (or (step-stack step) #())))
         "returnData" (when (and return-data-p (plusp (length (step-return-data step))))
                        (hex-bytes (step-return-data step)))
         "memory" (when (and memory-p (step-memory step)) (memory-chunks (step-memory step)))
         "storage" storage
         "refund" (when (plusp (step-refund step)) (step-refund step)))))

(defun default-frame-json (tracer options &key gas-used output)
  (with-slots (tx-gas-used tx-output nodes) tracer
    (if (zerop (fill-pointer nodes))
        (jobject "failed" :false "gas" 0 "returnValue" "0x" "structLogs" #())
        (jobject "failed" (jbool (not (node-success (node-at tracer 0))))
                 "gas" (or gas-used tx-gas-used 0)
                 "returnValue" (hex-bytes (or output tx-output +empty-code+))
                 "structLogs" (struct-logs tracer options)))))

;;; geth: prestateTracer

(defun empty-account-p (nonce balance code-hash)
  (and (zerop nonce) (zerop balance) (equalp code-hash +empty-code-hash+)))

(defstruct (astate (:constructor make-astate (balance code nonce storage)))
  balance code nonce storage)            ; storage: sorted alist

(defun account-state (nonce balance code)
  (make-astate balance (and code (plusp (length code)) code) (and (/= nonce 0) nonce) '()))

(defun astate-json (s)
  (obj "balance" (and (astate-balance s) (hex-qty (astate-balance s)))
       "code" (and (astate-code s) (hex-bytes (astate-code s)))
       "nonce" (astate-nonce s)
       "storage" (when (astate-storage s)
                   (cons :object (loop for (k . v) in (astate-storage s)
                                       collect (cons (hex-word k) (hex-word v)))))))

(defun astate-equal (a b)
  (and (eql (astate-balance a) (astate-balance b))
       (equalp (astate-code a) (astate-code b))
       (eql (astate-nonce a) (astate-nonce b))
       (equal (astate-storage a) (astate-storage b))))

(defun sorted-records (tracer)
  (sort (loop for r being the hash-values of (slot-value tracer 'accounts) collect r)
        #'< :key (lambda (r) (account-address (account-record-account r)))))

(defun record-address (r) (account-address (account-record-account r)))

(defun sorted-slots (table &optional (key #'identity))
  (sort (loop for k being the hash-keys of table using (hash-value v)
              collect (cons k (funcall key k v)))
        #'< :key #'car))

(defun selfdestructed-addresses (tracer)
  "Accounts a standing SELFDESTRUCT removed."
  (let ((set (make-hash-table)))
    (loop for node across (tracer-nodes tracer)
          when (and (node-sd-address node) (effective-p tracer node))
            do (setf (gethash (node-sd-address node) set) t))
    set))

(defun created-addresses (tracer)
  (let ((set (make-hash-table)))
    (loop for node across (tracer-nodes tracer)
          when (and (create-kind-p (node-kind node)) (effective-p tracer node))
            do (setf (gethash (node-address node) set) t))
    set))

(defun pre-code (r code-p)
  (and code-p (not (equalp (account-record-pre-code-hash r) +empty-code-hash+))
       (account-record-pre-code r)))

(defun prestate-json (tracer config)
  (capture-post-state tracer)
  (let ((code-p (not (eq (jget config "disableCode") :true)))
        (storage-p (not (eq (jget config "disableStorage") :true))))
    (if (eq (jget config "diffMode") :true)
        (prestate-diff-json tracer code-p storage-p)
        (cons :object
              (loop for r in (sorted-records tracer)
                    unless (empty-account-p (account-record-pre-nonce r)
                                            (account-record-pre-balance r)
                                            (account-record-pre-code-hash r))
                      collect (let ((s (account-state (account-record-pre-nonce r)
                                                      (account-record-pre-balance r)
                                                      (pre-code r code-p))))
                                (when storage-p
                                  (setf (astate-storage s)
                                        (sorted-slots (account-record-slots r)
                                                      (lambda (k v) (declare (ignore k)) v))))
                                (cons (hex-address (record-address r)) (astate-json s))))))))

(defun prestate-diff-json (tracer code-p storage-p)
  (let ((destructed (selfdestructed-addresses tracer))
        (pre '()) (post '()) (empty-before '()))
    (dolist (r (sorted-records tracer))
      (let* ((address (record-address r))
             (selfdestructed (and (gethash address destructed)
                                  (not (account-record-post-exists r))))
             (deleted (not (account-record-post-exists r)))
             (pc (pre-code r code-p))
             (post-hash (if deleted +empty-code-hash+ (account-record-post-code-hash r)))
             (post-state (account-state
                          (if deleted 0 (account-record-post-nonce r))
                          (if deleted 0 (account-record-post-balance r))
                          (and code-p
                               (if (equalp post-hash (account-record-pre-code-hash r))
                                   pc
                                   (and (not deleted) (account-record-post-code r))))))
             (pre-state (account-state (account-record-pre-nonce r) (account-record-pre-balance r) pc)))
        (when (and (astate-code pre-state) (null (astate-code post-state)))
          (setf (astate-code post-state) +empty-code+))
        (when storage-p
          (if selfdestructed
              (setf (astate-storage pre-state)
                    (sorted-slots (account-record-slots r) (lambda (k v) (declare (ignore k)) v)))
              (loop for (slot . original) in (sorted-slots (account-record-slots r)
                                                           (lambda (k v) (declare (ignore k)) v))
                    for current = (gethash slot (account-record-post-slots r) 0)
                    unless (= original current)
                      do (unless (zerop original)
                           (push (cons slot original) (astate-storage pre-state)))
                         (unless (zerop current)
                           (push (cons slot current) (astate-storage post-state)))))
          (setf (astate-storage pre-state) (sort (astate-storage pre-state) #'< :key #'car)
                (astate-storage post-state) (sort (astate-storage post-state) #'< :key #'car)))
        (push (cons address pre-state) pre)
        (when (empty-account-p (account-record-pre-nonce r) (account-record-pre-balance r)
                               (account-record-pre-code-hash r))
          (push address empty-before))
        (unless selfdestructed (push (cons address post-state) post))))
    ;; Keep the changed accounts, then the changed fields.
    (setf pre (nreverse pre) post (nreverse post))
    (let ((unchanged (loop for (a . p) in pre
                           for q = (cdr (assoc a post))
                           when (and q (astate-equal p q)) collect a)))
      (setf pre (remove-if (lambda (e) (member (car e) unchanged)) pre)
            post (remove-if (lambda (e) (member (car e) unchanged)) post)))
    (loop for (a . q) in post
          for p = (cdr (assoc a pre))
          when p
            do (when (eql (astate-balance q) (astate-balance p)) (setf (astate-balance q) nil))
               (when (eql (astate-nonce q) (astate-nonce p)) (setf (astate-nonce q) nil))
               (when (equalp (astate-code q) (astate-code p)) (setf (astate-code q) nil)))
    (setf pre (remove-if (lambda (e) (member (car e) empty-before)) pre))
    (flet ((m (entries) (cons :object (loop for (a . s) in entries
                                            collect (cons (hex-address a) (astate-json s))))))
      (jobject "post" (m post) "pre" (m pre)))))

;;; geth: 4byteTracer, noopTracer

(defun four-byte-json (tracer)
  (cons :object
        (sort (loop for (selector . size) being the hash-keys of (slot-value tracer 'four-byte)
                      using (hash-value count)
                    collect (cons (format nil "0x~(~8,'0x~)-~D" selector size) count))
              #'string< :key #'car)))

;;; parity: traces

(defun excluded-nodes (tracer exclusion)
  "A vector marking the precompile calls (and their callees) EXCLUSION leaves out:
:zero-value (parity), :call-and-staticcall (flatCallTracer) or :none."
  (let* ((nodes (tracer-nodes tracer))
         (excluded (make-array (length nodes) :initial-element nil)))
    (loop for node across nodes
          for i from 0
          do (setf (aref excluded i)
                   (or (and (node-parent node) (node-precompile-p node)
                            (ecase exclusion
                              (:none nil)
                              (:zero-value (zerop (node-value node)))
                              (:call-and-staticcall (member (node-kind node) '(:call :staticcall)))))
                       (and (node-parent node) (aref excluded (node-parent node))))))
    excluded))

(defun kept-children (tracer node excluded)
  (declare (ignore tracer))
  (remove-if (lambda (c) (aref excluded c)) (coerce (node-children node) 'list)))

(defun trace-addresses (tracer excluded)
  (let* ((nodes (tracer-nodes tracer))
         (addresses (make-array (length nodes) :initial-element '())))
    (loop for node across nodes
          for i from 0
          do (loop for child in (kept-children tracer node excluded)
                   for k from 0
                   do (setf (aref addresses child) (append (aref addresses i) (list k)))))
    addresses))

(defun parity-action-json (node &key gas caller)
  (if (create-kind-p (node-kind node))
      (jobject "from" (hex-address (or caller (node-caller node)))
               "gas" (hex-qty (or gas (node-gas-limit node)))
               "init" (hex-bytes (node-input node))
               "value" (hex-qty (node-value node))
               "creationMethod" (string-downcase (symbol-name (node-kind node))))
      (jobject "from" (hex-address (or caller (node-caller node)))
               "callType" (string-downcase (symbol-name (node-kind node)))
               "gas" (hex-qty (or gas (node-gas-limit node)))
               "input" (hex-bytes (node-input node))
               "to" (hex-address (node-address node))
               "value" (hex-qty (node-value node)))))

(defun parity-result-json (node &key gas-used)
  (let ((gas-used (hex-qty (or gas-used (node-gas-used node)))))
    (cond ((and (node-error-p node) (not (node-revert-p node))) :null)
          ((and (create-kind-p (node-kind node)) (not (node-revert-p node)))
           (jobject "address" (hex-address (node-address node))
                    "code" (hex-bytes (node-output node)) "gasUsed" gas-used))
          (t (jobject "gasUsed" gas-used "output" (hex-bytes (node-output node)))))))

;;; A parity trace before serialisation: the JSON pieces plus its trace address.
(defstruct (ptrace (:constructor make-ptrace (type action error result subtraces address)))
  type action error result subtraces address)

(defun node-ptrace (node address subtraces &key gas caller gas-used)
  (make-ptrace (if (create-kind-p (node-kind node)) "create" "call")
               (parity-action-json node :gas gas :caller caller)
               (stop-message (node-stop node) :parity)
               (parity-result-json node :gas-used gas-used)
               subtraces address))

(defun selfdestruct-ptrace (node address)
  (make-ptrace "suicide"
               (jobject "address" (hex-address (node-sd-address node))
                        "balance" (hex-qty (node-sd-value node))
                        "refundAddress" (hex-address (node-sd-target node)))
               nil :null 0 address))

(defun address< (a b)
  (loop for x in a for y in b
        do (cond ((< x y) (return t)) ((> x y) (return nil)))
        finally (return (< (length a) (length b)))))

(defun parity-ptraces (tracer exclusion &key localized gas caller gas-used)
  "The traces in the order trace_replayTransaction (LOCALIZED nil) or trace_transaction
and flatCallTracer (LOCALIZED t) list them, selfdestructs included."
  (let* ((excluded (excluded-nodes tracer exclusion))
         (addresses (trace-addresses tracer excluded))
         (out '())
         (pending-sd '())
         (sorting nil))
    (loop for node across (tracer-nodes tracer)
          for i from 0
          unless (aref excluded i)
            do (let* ((root (zerop i))
                      (pt (node-ptrace node (aref addresses i)
                                       (length (kept-children tracer node excluded))
                                       :gas (and root gas) :caller (and root caller)
                                       :gas-used (and root gas-used))))
                 (when localized
                   ;; emit the selfdestructs that sort before this trace
                   (loop for sd = (find-if (lambda (s) (address< (ptrace-address s) (ptrace-address pt)))
                                           pending-sd :from-end t)
                         while sd
                         do (push sd out) (setf pending-sd (remove sd pending-sd))))
                 (push pt out)
                 (when (node-selfdestruct-p node)
                   (let ((sd (selfdestruct-ptrace node (append (ptrace-address pt)
                                                               (list (ptrace-subtraces pt))))))
                     (incf (ptrace-subtraces pt))
                     (if localized
                         (setf pending-sd (append pending-sd (list sd)))
                         (progn (push sd out) (setf sorting t)))))))
    (setf out (nreverse out))
    (when localized
      (setf out (append out (reverse pending-sd))))
    (if sorting (stable-sort out #'address< :key #'ptrace-address) out)))

(defun ptrace-json (pt)
  (obj "type" (ptrace-type pt) "action" (ptrace-action pt) "error" (ptrace-error pt)
       "result" (ptrace-result pt) "subtraces" (ptrace-subtraces pt)
       "traceAddress" (vec (ptrace-address pt))))

(defun localized-json (pt &key block-hash block-number tx-hash tx-index)
  (obj "action" (ptrace-action pt)
       "blockHash" (and block-hash (hex-word block-hash))
       "blockNumber" block-number
       "error" (ptrace-error pt)
       "result" (ptrace-result pt)
       "subtraces" (ptrace-subtraces pt)
       "traceAddress" (vec (ptrace-address pt))
       "transactionHash" (if tx-hash (hex-word tx-hash) :null)
       "transactionPosition" (or tx-index :null)
       "type" (ptrace-type pt)))

;;; parity: vmTrace

(defun began-executing-p (step bytecode)
  (and (not (member (step-stop step) '(:opcode-not-found :invalid-fe-opcode :not-activated
                                       :stack-underflow :stack-overflow)))
       (< (step-pc step) (length bytecode))))

(defun entered-frame-p (node)
  (not (member (node-stop node) '(:call-too-deep :out-of-funds :nonce-overflow))))

(defun vm-trace (tracer node excluded)
  (let ((precompiles (loop for c across (node-children node)
                           when (aref excluded c) collect (node-at tracer c)))
        (ordering (node-ordering node))
        (ops '()))
    (let ((i 0) (n (fill-pointer ordering)))
      (loop while (< i n)
            do (let ((entry (aref ordering i)))
                 (incf i)
                 (when (eq (car entry) :step)
                   (let ((step (aref (node-steps node) (cdr entry))))
                     ;; a call's child follows its step in the ordering
                     (let ((sub :null))
                       (when (and (call-op-p (step-op step))
                                  (not (and (step-stop step) (stop-halt-p (step-stop step)))))
                         (if (and (< i n) (eq (car (aref ordering i)) :call))
                             (let ((child (node-at tracer (aref (node-children node)
                                                                (cdr (aref ordering i))))))
                               (incf i)
                               (if (aref excluded (node-idx child))
                                   (progn (setf precompiles (remove child precompiles))
                                          (when (entered-frame-p child)
                                            (setf sub (jobject "code" "0x" "ops" #()))))
                                   (when (entered-frame-p child)
                                     (setf sub (vm-trace tracer child excluded)))))
                             (let ((p (pop precompiles)))
                               (when (and p (entered-frame-p p))
                                 (setf sub (jobject "code" "0x" "ops" #()))))))
                       (when (began-executing-p step (node-bytecode node))
                         (push (vm-instruction step sub) ops))))))))
    (jobject "code" (hex-bytes (node-bytecode node)) "ops" (vec (nreverse ops)))))

(defun vm-instruction (step sub)
  (let ((halted (and (step-stop step) (stop-halt-p (step-stop step)))))
    (jobject "cost" (step-cost step)
             "ex" (if halted
                      :null
                      (jobject "used" (step-gas-after step)
                               "push" (map 'simple-vector #'hex-qty (or (step-push-stack step) #()))
                               "mem" (let ((m (step-mem step)))
                                       (if m (jobject "off" (car m) "data" (hex-bytes (cdr m))) :null))
                               "store" (let ((s (step-store step)))
                                         (if s (jobject "key" (hex-qty (car s)) "val" (hex-qty (cdr s)))
                                             :null))))
             "pc" (step-pc step)
             "sub" sub
             "op" (vm-op-name (step-op step)))))

;;; parity: stateDiff

(defun delta-json (from to &key (added nil) (removed nil) (fmt #'hex-qty))
  (cond (added (jobject "+" (funcall fmt to)))
        (removed (jobject "-" (funcall fmt from)))
        ((equalp from to) "=")
        (t (jobject "*" (jobject "from" (funcall fmt from) "to" (funcall fmt to))))))

(defun state-diff-json (tracer)
  (capture-post-state tracer)
  (let ((created (created-addresses tracer))
        (destructed (selfdestructed-addresses tracer))
        (entries '()))
    (dolist (r (sorted-records tracer))
      (let* ((address (record-address r))
             (existed (account-record-pre-exists r))
             (exists (account-record-post-exists r))
             (pre-code (if (equalp (account-record-pre-code-hash r) +empty-code-hash+)
                           +empty-code+ (account-record-pre-code r)))
             (changed-slots (loop for (slot . original) in (sorted-slots (account-record-slots r)
                                                                         (lambda (k v) (declare (ignore k)) v))
                                  for current = (gethash slot (account-record-post-slots r) 0)
                                  unless (= original current) collect (list slot original current)))
             (diff
               (cond ((and (not exists) (not existed)) nil)
                     ((not exists)
                      (jobject "balance" (delta-json (account-record-pre-balance r) nil :removed t)
                               "code" (delta-json pre-code nil :removed t :fmt #'hex-bytes)
                               "nonce" (delta-json (account-record-pre-nonce r) nil :removed t)
                               "storage" (jobject)))
                     ((and (not existed)
                           (or (gethash address created)
                               (not (empty-account-p (account-record-post-nonce r)
                                                     (account-record-post-balance r)
                                                     (account-record-post-code-hash r)))))
                      (jobject "balance" (delta-json nil (account-record-post-balance r) :added t)
                               "code" (delta-json nil (account-record-post-code r) :added t
                                                      :fmt #'hex-bytes)
                               "nonce" (delta-json nil (account-record-post-nonce r) :added t)
                               "storage" (cons :object
                                               (loop for (slot nil current) in changed-slots
                                                     collect (cons (hex-word slot)
                                                                   (jobject "+" (hex-word current)))))))
                     (t
                      (let ((pre-nonce (if existed (account-record-pre-nonce r) 0))
                            (pre-balance (if existed (account-record-pre-balance r) 0))
                            (pre-hash (if existed (account-record-pre-code-hash r) +empty-code-hash+)))
                        (unless (and (null changed-slots)
                                     (= pre-nonce (account-record-post-nonce r))
                                     (= pre-balance (account-record-post-balance r))
                                     (equalp pre-hash (account-record-post-code-hash r))
                                     (not (gethash address destructed)))
                          (jobject "balance" (delta-json pre-balance (account-record-post-balance r))
                                   "code" (if (equalp pre-hash (account-record-post-code-hash r))
                                              "="
                                              (delta-json pre-code (account-record-post-code r)
                                                          :fmt #'hex-bytes))
                                   "nonce" (delta-json pre-nonce (account-record-post-nonce r))
                                   "storage" (cons :object
                                                   (loop for (slot original current) in changed-slots
                                                         collect (cons (hex-word slot)
                                                                       (jobject "*" (jobject "from" (hex-word original)
                                                                                             "to" (hex-word current)))))))))))))
        (when diff (push (cons (hex-address address) diff) entries))))
    (cons :object (nreverse entries))))

;;; Opcode gas

(defun opcode-gas-json (tracer)
  (with-slots (opcode-counts opcode-gas) tracer
    (vec (loop for op in (sort (loop for k being the hash-keys of opcode-counts collect k) #'<)
               collect (jobject "opcode" (opcode-name op)
                                "count" (gethash op opcode-counts)
                                "gasUsed" (gethash op opcode-gas 0))))))

;;; Constructors

(defun config-object (config name)
  "CONFIG as a JSON object (NIL and null are {}), or signal TRACER-ERROR."
  (cond ((or (null config) (eq config :null)) (jobject))
        ((jobject-p config)
         (loop for (k . v) in (cdr config)
               unless (or (member v '(:true :false :null)) (and (string= k "limit") (integerp v)))
                 do (tracer-fail :invalid-config
                                 "invalid tracer config: invalid type for ~A field `~A`" name k))
         config)
        (t (tracer-fail :invalid-config "invalid tracer config: invalid type: expected struct ~A" name))))

(defparameter *builtin-tracers*
  '(("callTracer" . :call) ("flatCallTracer" . :flat-call) ("prestateTracer" . :prestate)
    ("4byteTracer" . :four-byte) ("noopTracer" . :noop) ("muxTracer" . :mux)))

(defun tracer-flags (kind config)
  "Initargs of the recording a geth tracer KIND needs."
  (ecase kind
    (:default (list :steps t
                    :stack (if (option-true-p config "disableStack") :none :full)
                    :memory (option-enabled-p config "enableMemory" "disableMemory")
                    :storage (not (option-true-p config "disableStorage"))
                    :return-data (option-enabled-p config "enableReturnData" "disableReturnData")))
    ((:call :flat-call :noop) '())
    (:prestate (list :state t))
    (:four-byte (list :four-byte t))))

(defun merge-flags (a b)
  (loop for (k v) on b by #'cddr
        do (setf (getf a k) (or (getf a k) v)))
  a)

(defun make-geth-tracer (options)
  "A tracer for debug_trace* with OPTIONS, the GethDebugTracingOptions JSON object
\(tracer, tracerConfig and the struct logger's flags)."
  (let* ((options (if (jobject-p options) options (jobject)))
         (name (jget options "tracer"))
         (name (if (and (stringp name) (plusp (length name))) name nil))
         (config (jget options "tracerConfig")))
    (if (null name)
        (apply #'make-instance 'geth-tracer :kind :default :config options
               (tracer-flags :default options))
        (let ((kind (cdr (assoc name *builtin-tracers* :test #'string=))))
          (case kind
            ((nil) (if (member name '("stateGasTracer" "erc7562Tracer") :test #'string=)
                       (tracer-fail :unsupported-tracer "unsupported tracer")
                       (tracer-fail :js-tracer "JS Tracer is not enabled")))
            (:mux
             (let ((config (cond ((or (null config) (eq config :null)) (jobject))
                                 ((jobject-p config) config)
                                 (t (tracer-fail :invalid-config "invalid tracer config: invalid type: expected a map"))))
                   (children '()) (flags '()))
               (loop for (child-name . child-config) in (cdr config)
                     for child-kind = (cdr (assoc child-name *builtin-tracers* :test #'string=))
                     do (unless (and child-kind (not (eq child-kind :mux)))
                          (tracer-fail :unsupported-tracer "unsupported tracer type: '~A'" child-name))
                        (let ((c (config-object child-config child-name)))
                          (push (list child-name child-kind c) children)
                          (setf flags (merge-flags flags (tracer-flags child-kind c)))))
               (apply #'make-instance 'geth-tracer :kind :mux :config config
                      :children (nreverse children) flags)))
            (t (let ((c (config-object config name)))
                 (apply #'make-instance 'geth-tracer :kind kind :config c
                        (tracer-flags kind c)))))))))

(defun make-parity-tracer (types)
  "A tracer for trace_* with TYPES, a list of :trace :vm-trace :state-diff."
  (let ((vm (member :vm-trace types)))
    (make-instance 'parity-tracer :types types
                                  :steps (and vm t) :stack (if vm :pushes :none) :deltas (and vm t)
                                  :state (and (member :state-diff types) t))))

(defun make-opcode-gas-tracer ()
  "A tracer for trace_transactionOpcodeGas."
  (make-instance 'opcode-gas-tracer :opcode-gas t))

;;; Results

(defun geth-kind-json (tracer kind config &key gas-limit caller gas-used output
                                              block-hash block-number tx-hash tx-index)
  (ecase kind
    (:default (default-frame-json tracer config :gas-used gas-used :output output))
    (:call (call-tracer-json tracer config :gas-limit gas-limit :caller caller :gas-used gas-used))
    (:flat-call
     (multiple-value-bind (gas from) (root-overrides tracer :gas-limit gas-limit :caller caller)
       (map 'simple-vector
            (lambda (pt) (localized-json pt :block-hash block-hash :block-number block-number
                                            :tx-hash tx-hash :tx-index tx-index))
            (parity-ptraces tracer (if (eq (jget config "includePrecompiles") :true)
                                       :none :call-and-staticcall)
                            :localized t :gas gas :caller from
                            :gas-used (or gas-used (slot-value tracer 'tx-gas-used))))))
    (:prestate (prestate-json tracer config))
    (:four-byte (four-byte-json tracer))
    (:noop (jobject))))

(defgeneric geth-result (tracer &key gas-limit caller gas-used output
                                  block-hash block-number tx-hash tx-index)
  (:documentation "The debug_trace* result of the traced transaction as a CLEMPO.JSON value.
GAS-LIMIT, CALLER, GAS-USED and OUTPUT override what TRACE-TX-START/-END recorded;
the block and transaction keys localize flatCallTracer's traces."))

(defmethod geth-result ((tracer geth-tracer) &rest keys
                        &key gas-limit caller gas-used output block-hash block-number
                          tx-hash tx-index)
  (declare (ignore gas-limit caller gas-used output block-hash block-number tx-hash tx-index))
  (let ((kind (geth-tracer-kind tracer)))
    (if (eq kind :mux)
        (cons :object (loop for (name child-kind config) in (slot-value tracer 'children)
                            collect (cons name (apply #'geth-kind-json tracer child-kind config keys))))
        (apply #'geth-kind-json tracer kind (geth-tracer-config tracer) keys))))

(defun parity-result (tracer &key output)
  "trace_replayTransaction's TraceResults: {output, stateDiff, trace, vmTrace}."
  (let* ((types (parity-tracer-types tracer))
         (nodes-p (plusp (fill-pointer (tracer-nodes tracer))))
         (excluded (and nodes-p (excluded-nodes tracer :zero-value))))
    (jobject "output" (hex-bytes (or output (slot-value tracer 'tx-output) +empty-code+))
             "stateDiff" (if (member :state-diff types) (state-diff-json tracer) :null)
             "trace" (if (and nodes-p (member :trace types))
                         (map 'simple-vector #'ptrace-json (parity-ptraces tracer :zero-value))
                         #())
             "vmTrace" (if (and nodes-p (member :vm-trace types))
                           (vm-trace tracer (node-at tracer 0) excluded)
                           :null))))

(defun parity-transaction-traces (tracer &key block-hash block-number tx-hash tx-index)
  "trace_transaction's localized traces (also trace_block's per transaction)."
  (map 'simple-vector
       (lambda (pt) (localized-json pt :block-hash block-hash :block-number block-number
                                       :tx-hash tx-hash :tx-index tx-index))
       (parity-ptraces tracer :zero-value :localized t)))

(defun opcode-gas-result (tracer)
  "trace_transactionOpcodeGas's opcodeGas list, ordered by opcode."
  (opcode-gas-json tracer))
