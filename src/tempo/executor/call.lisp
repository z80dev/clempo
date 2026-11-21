(in-package #:clempo.executor)

;;; Calls on any state: the overlay blocks of handler.lisp (BEGIN-CALL-BLOCK, EXECUTE-REQUEST,
;;; APPLY-TRANSACTION on the context) over a state database or a clempo.history view, and the
;;; procedures reth's RPC builds from them: replaying a block's prefix for a trace, gas
;;; estimation (reth rpc-eth-api estimate.rs with Tempo's EstimateCall) and access lists
;;; (call.rs create_access_list_with with evm2's AccessListInspector). Nothing here writes a
;;; store; the JSON is the RPC layer's.

;;; Historical states as execution backends: a view answers the world's reads exactly as the
;;; state database did after its block.

(defmethod state:backend-account ((view clempo.history:history-state) address)
  (clempo.history:history-account view address))

(defmethod state:backend-storage ((view clempo.history:history-state) address slot)
  (clempo.history:history-storage view address slot))

(defmethod state:backend-code ((view clempo.history:history-state) code-hash)
  (clempo.history:history-code view code-hash))

(defmethod state:backend-storage-empty-p ((view clempo.history:history-state) address)
  (clempo.history:history-storage-empty-p view address))

;;; Replaying a block's prefix (debug_traceTransaction, trace_replayTransaction, ...)

(defun block-prefix-context (backend block index &key block-hash-fn (chain *chain*))
  "An overlay context for BLOCK on BACKEND (the state after its parent) holding the block's
pre-execution changes and its first INDEX transactions; the next APPLY-TRANSACTION on it
runs transaction INDEX as the block ran it."
  (let* ((*chain* chain)
         (ctx (begin-call-block backend (header-block-env (blocks:block-header block) :chain chain)
                                :block-hash-fn block-hash-fn)))
    (loop for tx in (blocks:block-transactions block)
          for i below index
          do (apply-transaction ctx tx (tx:transaction-sender tx)))
    ctx))

;;; Request execution as reth's prepare_call_env sets it up: no fee charge or balance check,
;;; no nonce, base fee or EIP-3607 check, no gas caps.

(defparameter *call-flags*
  '(:skip-nonce-check t :skip-base-fee-check t :skip-gas-caps t :skip-fee-charge t :skip-eip3607 t)
  "EXECUTE-REQUEST flags for eth_call, eth_estimateGas and eth_createAccessList.")

(defun call-request (ctx request)
  "Run REQUEST in CTX as eth_call does: a TX-OUTCOME, or INVALID-TRANSACTION."
  (apply #'execute-request ctx request *call-flags*))

(defun caller-gas-allowance (ctx request)
  "Tempo's caller_gas_allowance (crates/node/src/rpc/mod.rs) for REQUEST in CTX: the gas the
fee payer's fee token balance buys at the request's max fee per gas. When no fallback token
covers the request's gas limit the largest candidate balance stands in, as Tempo takes the
InsufficientBalance error's available amount. Every read is rolled back (no slot stays warm)."
  (let* ((*tx-index* (ctx-index ctx)) (*ctx* ctx)
         (world (ctx-world ctx))
         (mark (state:checkpoint world)))
    (unwind-protect
         (with-block-context (ctx)
           (let* ((v (request-txv request *tx-index*))
                  (payer (txv-fee-payer v))
                  (price (txv-max-fee-per-gas v))
                  (balance
                    (flet ((balance (token)
                             (with-protocol-storage (:static t) (clempo.tip20:balance-of token payer))))
                      (handler-case
                          (balance (resolve-fee-token v payer (calc-gas-balance-spending (txv-gas-limit v) price)))
                        (invalid-transaction ()
                          (reduce #'max (mapcar #'balance (pc:fallback-fee-tokens forks:*spec*))
                                  :initial-value 0))))))
             (if (zerop price)
                 0
                 (min +max-u64+ (floor (* balance +gas-price-scaling+) price)))))
      (state:rollback world mark))))

;;; eth_estimateGas

(defconstant +min-transaction-gas+ 21000)
(defconstant +call-stipend+ 2300)
(defparameter *estimate-error-ratio* 0.015d0 "reth ESTIMATE_GAS_ERROR_RATIO")

(define-condition estimate-failure (error)
  ((kind :initarg :kind :reader estimate-failure-kind)
   (gas-limit :initarg :gas-limit :reader estimate-failure-gas-limit)
   (outcome :initarg :outcome :initform nil :reader estimate-failure-outcome))
  (:report (lambda (c s)
             (format s "gas estimation failed: ~(~A~) at ~D" (estimate-failure-kind c)
                     (estimate-failure-gas-limit c))))
  (:documentation "How reth's estimate_gas_with ends without an estimate. KIND:
:gas-required-exceeds-allowance (\"gas required exceeds allowance (GAS-LIMIT)\"),
:basic-out-of-gas (\"out of gas: gas required exceeds: GAS-LIMIT\"), :revert (the OUTCOME's
revert output) or :halt (the OUTCOME's halt at GAS-LIMIT). Invalid requests other than a gas
limit that is too low or too high signal INVALID-TRANSACTION as they are."))

(defun estimate-fail (kind gas-limit &optional outcome)
  (error 'estimate-failure :kind kind :gas-limit gas-limit :outcome outcome))

(defun gas-limit-error (condition)
  "evm2's is_gas_too_low / is_gas_too_high for an invalid request: :too-low, :too-high or NIL."
  (let ((reason (and (typep condition 'execution-failure) (execution-failure-reason condition))))
    (when (stringp reason)
      (cond ((or (search "intrinsic gas too low" reason) (search "below the calldata floor" reason)) :too-low)
            ((or (search "gas limit above" reason)) :too-high)))))

(defun outcome-success-p (outcome) (tx-outcome-status outcome))
(defun outcome-revert-p (outcome)
  (and (not (tx-outcome-status outcome)) (null (tx-outcome-halt outcome))))

(defun out-of-gas-result (outcome requested)
  "reth map_out_of_gas_result."
  (cond ((outcome-success-p outcome) (estimate-fail :basic-out-of-gas requested outcome))
        ((outcome-revert-p outcome) (estimate-fail :revert requested outcome))
        (t (estimate-fail :halt requested outcome))))

(defun estimate-gas (run &key max-gas-limit request-gas price-p basic-transfer-p allowance)
  "reth estimate_gas_with: binary-search the gas limit at which RUN succeeds. RUN takes a gas
limit and returns a TX-OUTCOME (each on a fresh context, see CALL-REQUEST) or signals
INVALID-TRANSACTION. MAX-GAS-LIMIT: min(the transaction gas cap, the block gas limit, the
RPC gas cap). REQUEST-GAS: the request's gas limit, if any. PRICE-P: the request set a gas
price. BASIC-TRANSFER-P: no input and a target without code. ALLOWANCE: what the caller can
afford when the request has a nonzero price. Returns the estimate or signals ESTIMATE-FAILURE."
  (let ((highest (min (or request-gas max-gas-limit) max-gas-limit))
        (limited (or request-gas price-p)))
    (when allowance (setf highest (min highest allowance)))
    (when basic-transfer-p
      (let ((res (handler-case (funcall run (min +min-transaction-gas+ max-gas-limit))
                   (execution-failure () nil))))
        (when (and res (outcome-success-p res))
          (return-from estimate-gas (tx-outcome-gas-used res)))))
    (let* ((res (handler-case (funcall run highest)
                  (execution-failure (c)
                    (case (gas-limit-error c)
                      (:too-high (if limited
                                     (out-of-gas-result (funcall run max-gas-limit) highest)
                                     (error c)))
                      (:too-low (estimate-fail :gas-required-exceeds-allowance highest))
                      (t (error c))))))
           (refund (cond ((outcome-success-p res) (tx-outcome-refunded res))
                         ((outcome-revert-p res)
                          (if limited
                              (out-of-gas-result (funcall run max-gas-limit) highest)
                              (estimate-fail :revert highest res)))
                         (t (estimate-fail :halt highest res))))
           (gas-used (tx-outcome-gas-used res))
           (lowest (max 0 (1- gas-used)))
           (optimistic (floor (* (+ gas-used refund +call-stipend+) 64) 63)))
      (flet ((update (outcome limit)
               (if (outcome-success-p outcome) (setf highest limit) (setf lowest limit))))
        (when (< optimistic highest)
          (let ((r (funcall run optimistic)))
            (setf gas-used (tx-outcome-gas-used r))
            (update r optimistic)))
        (let ((mid (min (* gas-used 3) (floor (+ highest lowest) 2))))
          (loop while (< (1+ lowest) highest)
                do (when (< (/ (float (- highest lowest) 1d0) highest) *estimate-error-ratio*)
                     (loop-finish))
                   (handler-case (update (funcall run mid) mid)
                     (execution-failure (c)
                       (case (gas-limit-error c)
                         (:too-high (setf highest mid))
                         (:too-low (setf lowest mid))
                         (t (error c)))))
                   (setf mid (floor (+ highest lowest) 2))))
        highest))))

;;; eth_createAccessList: evm2's AccessListInspector as a clempo.evm tracer.

(defclass access-list-tracer ()
  ((slots :initform (make-hash-table) :reader access-list-slots) ; address -> hash-table of slots
   (excluded :initform (make-hash-table) :reader access-list-excluded)
   (precompiles :initform nil)
   (seeds-pruned :initform nil))
  (:documentation "Collects the accounts and slots a transaction's code touches."))

(defun make-access-list-tracer (initial &key excluded)
  "A tracer seeded with INITIAL ((address slot ...) ...); EXCLUDED addresses (EIP-7702
authorities) are never listed unless their storage is read or written."
  (let ((tr (make-instance 'access-list-tracer)))
    (loop for (address . slots) in initial
          do (let ((set (access-list-entry tr address)))
               (dolist (s slots) (setf (gethash s set) t))))
    (dolist (a excluded) (setf (gethash a (access-list-excluded tr)) t))
    tr))

(defun access-list-entry (tracer address)
  (let ((table (access-list-slots tracer)))
    (or (gethash address table) (setf (gethash address table) (make-hash-table)))))

(defun access-list-excluded-p (tracer address)
  (or (gethash address (access-list-excluded tracer))
      (gethash address (slot-value tracer 'precompiles))))

(defmethod evm:trace-call-start ((tr access-list-tracer) frame)
  (when (zerop (evm:frame-depth frame))
    (setf (gethash (evm:frame-caller frame) (access-list-excluded tr)) t
          (gethash (evm:frame-address frame) (access-list-excluded tr)) t)
    (let ((p (make-hash-table)))
      (dolist (a (evm:precompile-addresses (evm:spec-precompiles evm:*spec*))) (setf (gethash a p) t))
      (setf (slot-value tr 'precompiles) p))
    (unless (slot-value tr 'seeds-pruned)
      (setf (slot-value tr 'seeds-pruned) t)
      (let ((table (access-list-slots tr)))
        (loop for address in (loop for a being the hash-keys of table collect a)
              when (access-list-excluded-p tr address) do (remhash address table))))))

(defmethod evm:trace-step ((tr access-list-tracer) frame pc op gas sp stack)
  (declare (ignore pc gas))
  (flet ((word (k) (and (< k sp) (svref stack (- sp k 1)))))
    (case op
      ((#x54 #x55)                      ; SLOAD SSTORE
       (let ((slot (word 0)))
         (when slot (setf (gethash slot (access-list-entry tr (evm:frame-address frame))) t))))
      ((#x3c #x3f #x3b #x31 #xff)       ; EXTCODECOPY EXTCODEHASH EXTCODESIZE BALANCE SELFDESTRUCT
       (let ((w (word 0)))
         (when w
           (let ((a (ldb (byte 160 0) w)))
             (unless (access-list-excluded-p tr a) (access-list-entry tr a))))))
      ((#xf4 #xf1 #xfa #xf2)            ; DELEGATECALL CALL STATICCALL CALLCODE
       (when (>= sp 5)
         (let ((a (ldb (byte 160 0) (word 1))))
           (unless (access-list-excluded-p tr a) (access-list-entry tr a))))))))

(defun access-list-result (tracer)
  "The collected list ((address slot ...) ...), sorted as alloy's AccessList::sort (by
address, slots ascending)."
  (sort (loop for address being the hash-keys of (access-list-slots tracer) using (hash-value set)
              collect (cons address (sort (loop for s being the hash-keys of set collect s) #'<)))
        #'< :key #'car))

(defun create-access-list (run request &key excluded)
  "reth create_access_list_with. RUN takes a request plist and returns its TX-OUTCOME on a
fresh context (or signals INVALID-TRANSACTION). Runs REQUEST under the inspector seeded with
its :access-list, then, if it succeeded, once more with the list found to get its gas.
Returns (values access-list gas-used outcome)."
  (let* ((tracer (make-access-list-tracer (getf request :access-list) :excluded excluded))
         (first (let ((evm:*tracer* tracer)) (funcall run request)))
         (list (access-list-result tracer)))
    (if (not (outcome-success-p first))
        (values list (tx-outcome-gas-used first) first)
        (let ((again (funcall run (list* :access-list list request))))
          (values list (tx-outcome-gas-used again) again)))))
