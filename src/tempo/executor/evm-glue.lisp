;;; NOT YET IN clempo-tempo-executor.asd: written against EvmAgent's and PrecompilesAgent's
;;; working trees (clempo.state backend/world, clempo.evm spec/precompile provider/SSTORE hook,
;;; clempo.contract host protocol); add it to the system once clempo.evm and clempo.state are
;;; pushed. Compiled cleanly in a scratch tree combining those working trees (2026-11-21).
(in-package #:clempo.executor)

;;; How Tempo configures the EVM (tempo crates/evm/src/handler/config.rs, instructions.rs,
;;; crates/hardfork/src/gas_params.rs) and how the precompiles reach the world state
;;; (crates/precompiles/src/lib.rs execute_in_frame, storage/evm.rs).

;;; The committed state as the world's backend.

(defmethod state:backend-account ((db store:statedb) address)
  (let ((a (store:db-account db address)))
    (if a
        (values t (store:account-nonce a) (store:account-balance a) (store:account-code-hash a))
        (values nil 0 0 state:+empty-code-hash+))))

(defmethod state:backend-storage ((db store:statedb) address slot)
  (store:db-storage db address slot))

(defmethod state:backend-code ((db store:statedb) code-hash)
  (store:db-code db code-hash))

(defmethod state:backend-storage-empty-p ((db store:statedb) address)
  (let ((a (store:db-account db address)))
    (or (null a) (zerop (store:account-slot-count a)))))

(defun write-changes (db changes)
  "Write the world's drained ACCOUNT-CHANGEs into the open block of DB."
  (dolist (c changes)
    (let ((address (state:account-change-address c)))
      (cond ((state:account-change-deleted-p c) (store:db-delete-account db address))
            (t
             (when (state:account-change-storage-wiped-p c) (store:db-wipe-storage db address))
             (let ((code (state:account-change-code c)))
               (when (and code (plusp (length code))) (store:db-put-code db code)))
             (store:db-set-account db address (state:account-change-nonce c)
                                   (state:account-change-balance c) (state:account-change-code-hash c))
             (loop for (slot . value) in (state:account-change-storage c)
                   do (store:db-set-storage db address slot value)))))))

;;; Gas schedule: Osaka with Tempo's overrides (gas_params.rs configure_version, with
;;; EIP-8037 off as tempo_execution_config builds it) and the transaction gas cap
;;; (TempoHardfork::tx_gas_limit_cap).

(defun tempo-gas-params (fork)
  "evm2's SstoreSetWithoutLoadCost excludes the 100-gas warm read its SSTORE charges first;
clempo.evm's SSTORE-SET is the whole zero-to-nonzero cost, so Tempo's values gain that 100."
  (let* ((gp (evm:copy-gas-params (evm:gas-params-for-fork :osaka)))
         (warm (evm:gas-params-warm-read gp)))
    (when (forks:fork>= fork :t1)          ; apply_t1: TIP-1000
      (setf (evm:gas-params-sstore-set gp) (+ warm forks:+sstore-create-cost+)
            (evm:gas-params-tx-create gp) 500000
            (evm:gas-params-create gp) 500000
            (evm:gas-params-new-account gp) 250000
            (evm:gas-params-new-account-selfdestruct gp) 250000
            (evm:gas-params-code-deposit gp) 1000
            (evm:gas-params-auth-empty-account gp) 12500
            (evm:gas-params-auth-refund gp) 0))
    (when (forks:fork>= fork :t7)          ; apply_t7: TIP-1060
      (setf (evm:gas-params-sstore-set gp) (+ warm forks:+sstore-set-cost+)
            (evm:gas-params-sstore-set-refund gp) forks:+sstore-set-cost+
            (evm:gas-params-sstore-clear-refund gp) 0
            (evm:gas-params-refund-quotient gp) 1))
    (setf (evm:gas-params-tx-gas-cap gp) (forks:tx-gas-limit-cap fork))
    gp))

;;; The precompile provider: Ethereum's Osaka set (warm at transaction start) and Tempo's
;;; (cold, dispatched by address and fork).

(defstruct (tempo-precompiles (:constructor make-tempo-precompiles (fork base)))
  fork base
  ;; lazily filled and shared by every thread running this fork's cached spec (RPC calls)
  (entries (make-hash-table :synchronized t) :type hash-table))

(defmethod evm:find-precompile ((p tempo-precompiles) address)
  (or (evm:find-precompile (tempo-precompiles-base p) address)
      (when (pc:precompile-address-p address (tempo-precompiles-fork p))
        (let ((entries (tempo-precompiles-entries p)))
          (or (gethash address entries)
              (setf (gethash address entries)
                    (evm:make-precompile 'tempo-precompile address #'run-tempo-precompile)))))))

(defmethod evm:precompile-addresses ((p tempo-precompiles))
  (evm:precompile-addresses (tempo-precompiles-base p)))

(defvar *host* nil "The WORLD-HOST precompiles of the running block run on.")
(defvar *non-creditable* nil
  "NIL or a function (owner slot) naming slots whose clear mints no storage credit (TIP-1060
NonCreditableSlots, set up by fee collection).")

(defun run-tempo-precompile (frame)
  "TempoPrecompiles::execute: run the precompile at FRAME's code address in a metered
storage context with the frame's gas and static flag."
  (let ((storage (contract:make-metered-storage *host* :gas-limit (evm:frame-gas frame)
                                                       :static (evm:frame-static-p frame)
                                                       :non-creditable *non-creditable*)))
    (multiple-value-bind (status output used refund)
        (let ((contract:*storage* storage))
          (pc:call-precompile (evm:frame-address frame) (evm:frame-input frame) (evm:frame-caller frame)
                              :code-address (evm:frame-code-address frame)))
      (ecase status
        (:success (decf (evm:frame-gas frame) used)
                  (unless (zerop refund) (state:add-refund evm:*world* refund))
                  (values :return output))
        (:revert (decf (evm:frame-gas frame) used)
                 (values :revert output))
        (:halt (setf (evm:frame-gas frame) 0)
               (values (if (eq output :out-of-gas) :out-of-gas :precompile-failure) state:+empty-code+))
        (:fatal (error "Tempo precompile ~40,'0X failed fatally: ~A" (evm:frame-code-address frame) output))))))

;;; The host the metered storage runs on: the world of the running block.

(defclass world-host ()
  ((block-info :initarg :block-info :reader world-host-block-info))
  (:documentation "clempo.contract's HOST protocol over clempo.state's *WORLD*, with
evm2's journaled semantics (crates/precompiles/src/storage/evm.rs sload_journal ...)."))

(defun warm-slot-or-skip (world address slot skip-cold)
  "evm2 into_slot_with_skip_and_warm: T when the slot was cold."
  (if (state:slot-warm-p world address slot)
      nil
      (if skip-cold
          (error 'contract:cold-load-skipped)
          (progn (state:warm-slot world address slot) t))))

(defmethod contract:host-sload ((h world-host) address slot skip-cold)
  (let ((world evm:*world*))
    (state:warm-address world address)
    (let ((cold (warm-slot-or-skip world address slot skip-cold)))
      (values (state:sload world address slot) cold))))

(defmethod contract:host-sstore ((h world-host) address slot value skip-cold)
  (let ((world evm:*world*))
    (state:warm-address world address)
    (let ((cold (warm-slot-or-skip world address slot skip-cold)))
      (multiple-value-bind (original present) (state:sstore world address slot value)
        (values original present cold)))))

(defmethod contract:host-tload ((h world-host) address slot)
  (state:tload evm:*world* address slot))

(defmethod contract:host-tstore ((h world-host) address slot value)
  (state:tstore evm:*world* address slot value))

(defun account-info (world account)
  (when (state:account-exists account)
    (contract:make-account-info :nonce (state:account-nonce account)
                                :balance (state:account-balance account)
                                :code (state:account-code world account))))

(defmethod contract:host-account ((h world-host) address skip-cold)
  (let* ((world evm:*world*)
         (cold (not (state:address-warm-p world address))))
    (when (and cold skip-cold) (error 'contract:cold-load-skipped))
    (state:warm-address world address)
    (values (account-info world (state:get-account world address)) cold)))

(defmethod contract:host-peek-account ((h world-host) address)
  (let ((world evm:*world*))
    (values (account-info world (state:get-account world address))
            (state:address-warm-p world address))))

(defmethod contract:host-set-code ((h world-host) address code)
  (let* ((world evm:*world*)
         (account (state:get-account world address))
         (was-empty (state:account-dead-p account)))
    (state:warm-address world address)
    (state:set-code world account code)
    was-empty))

(defmethod contract:host-log ((h world-host) address topics data)
  (let ((entry (state:make-log-entry address topics data)))
    (state:add-log evm:*world* entry)
    ;; a precompile's logs reach the tracer like LOGn's (the frame is the precompile's own)
    (when evm:*tracer* (evm:trace-log evm:*tracer* nil entry))))

(defmethod contract:host-logs ((h world-host))
  (mapcar (lambda (e) (list (state:log-entry-address e) (state:log-entry-topics e) (state:log-entry-data e)))
          (state:logs-since evm:*world*)))

(defmethod contract:host-checkpoint ((h world-host)) (state:checkpoint evm:*world*))
(defmethod contract:host-commit ((h world-host) cp) (declare (ignore cp)) nil)
(defmethod contract:host-revert ((h world-host) cp) (state:rollback evm:*world* cp))
(defmethod contract:host-block ((h world-host)) (world-host-block-info h))

;;; Opcodes Tempo adds (instructions.rs) and the TIP-1060 SSTORE hook.

(defun millis-timestamp-op (frame stack sp)
  "0x4f before T1C: the block timestamp in milliseconds, 2 gas."
  (declare (type simple-vector stack) (type fixnum sp))
  (cond ((< (evm:frame-gas frame) 2) (values sp :out-of-gas))
        ((>= sp 1024) (values sp :stack-overflow))
        (t (decf (evm:frame-gas frame) 2)
           (let ((env (evm:block-env-ext evm:*block*)))
             (setf (svref stack sp) (+ (* 1000 (block-env-timestamp env))
                                       (block-env-timestamp-millis-part env))))
           (values (1+ sp) nil))))

(defvar *evm-specs* (make-hash-table :synchronized t) "fork -> clempo.evm spec")

(defun storage-credit-sstore-hook (frame key original present new cold)
  "instructions.rs sstore from T7: TIP-1060 storage credits after the slot write, metered on the
frame's gas (accounting.rs sstore_storage_credits without non-creditable slots: EVM writes
always mint). Returns NIL or the halt status."
  (declare (ignore original cold))
  (unless (eq (zerop present) (zerop new))
    (let ((b (contract:make-metered-storage *host* :gas-limit (evm:frame-gas frame))))
      (handler-case
          (progn (contract:sstore-storage-credits b (evm:frame-address frame) key present new)
                 (decf (evm:frame-gas frame) (contract:metered-storage-spent b))
                 nil)
        (contract:precompile-halt (c)
          (setf (evm:frame-gas frame) 0)
          (let ((reason (contract:precompile-halt-reason c)))
            (if (keywordp reason) reason :out-of-gas)))))))

(defun tempo-evm-spec (fork)
  "The EVM spec of Tempo hardfork FORK (all Tempo forks run on Osaka)."
  (or (gethash fork *evm-specs*)
      (setf (gethash fork *evm-specs*)
            (let ((spec (evm:make-spec :osaka :gas (tempo-gas-params fork)
                                              :precompiles (make-tempo-precompiles
                                                            fork (evm:ethereum-precompiles :osaka))
                                              :ext fork)))
              (when (forks:fork< fork :t1c)
                (setf (svref (evm:spec-extra-opcodes spec) #x4f) #'millis-timestamp-op))
              (when (forks:fork>= fork :t7)
                (setf (evm:spec-sstore-hook spec) #'storage-credit-sstore-hook))
              spec))))

(defun evm-block-env (backend env &optional block-hash-fn)
  "The EVM's block environment for ENV. BLOCKHASH asks BLOCK-HASH-FN (block number -> 32-byte
hash or NIL); by default a state database's last 256 committed hashes."
  (let ((hash-fn (or block-hash-fn
                     (if (typep backend 'store:statedb)
                         (lambda (n) (store:db-block-hash backend n))
                         (lambda (n) (error "BLOCKHASH ~D: no block-hash function for this backend" n))))))
    (evm:make-block-env :number (block-env-number env)
                        :timestamp (block-env-timestamp env)
                        :coinbase (block-env-beneficiary env)
                        :gas-limit (block-env-gas-limit env)
                        :base-fee (block-env-base-fee env)
                        :prevrandao (octets->integer (block-env-prevrandao env))
                        :difficulty 0
                        :chain-id (block-env-chain-id env)
                        :excess-blob-gas 0
                        :blob-base-fee 1
                        :block-hash-fn (lambda (n)
                                         (let ((h (funcall hash-fn n)))
                                           (if h (octets->integer h) 0)))
                        :ext env)))
