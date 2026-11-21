(in-package #:clempo.executor)

;;; One block: tempo crates/evm/src/block.rs (TempoBlockExecutor). The block is opened on
;;; the state database, executed, and left open: the caller compares the result with the
;;; header and commits (store:db-commit-block) or undoes it (store:db-revert-block).

(defstruct block-env
  (number 0 :type (unsigned-byte 64))
  (timestamp 0 :type (unsigned-byte 64))
  (timestamp-millis-part 0 :type (unsigned-byte 64))
  (beneficiary 0 :type (unsigned-byte 160))
  (gas-limit 0 :type (unsigned-byte 64))
  (base-fee 0 :type (unsigned-byte 64))
  (prevrandao (make-octets 32) :type octets)
  (parent-hash (make-octets 32) :type octets)
  (extra-data (make-octets 0) :type octets)
  (chain-id 0 :type (unsigned-byte 64))
  (spec :genesis :type keyword)
  (general-gas-limit 0 :type (unsigned-byte 64))
  (shared-gas-limit 0 :type (unsigned-byte 64))
  (proposer nil)
  (parent-beacon-block-root (make-octets 32) :type octets))

(defun header-block-env (header &key (chain *chain*))
  (let* ((inner (headers:header-inner header))
         (timestamp (headers:ethereum-header-timestamp inner))
         (context (headers:header-consensus-context header)))
    (make-block-env :number (headers:ethereum-header-number inner)
                    :timestamp timestamp
                    :timestamp-millis-part (headers:header-timestamp-millis-part header)
                    :beneficiary (headers:ethereum-header-beneficiary inner)
                    :gas-limit (headers:ethereum-header-gas-limit inner)
                    :base-fee (or (headers:ethereum-header-base-fee-per-gas inner) 0)
                    :prevrandao (headers:ethereum-header-mix-hash inner)
                    :parent-hash (headers:ethereum-header-parent-hash inner)
                    :extra-data (headers:ethereum-header-extra-data inner)
                    :chain-id (chainspec:chain-id (chainspec:load-chainspec chain))
                    :spec (chainspec:fork-at timestamp chain)
                    :general-gas-limit (headers:header-general-gas-limit header)
                    :shared-gas-limit (headers:header-shared-gas-limit header)
                    :proposer (when context (headers:consensus-context-proposer context))
                    :parent-beacon-block-root (or (headers:ethereum-header-parent-beacon-block-root inner)
                                                  (make-octets 32)))))

(defstruct block-result
  (number 0 :type integer)
  (gas-used 0 :type integer)
  (receipts '() :type list)
  receipts-root bloom state-root
  (tx-count 0 :type integer))

;;; Transactions

(defun system-transaction-p (tx)
  "Tempo's system transactions are legacy transactions with the zero signature (r = s = 0,
even parity): TEMPO_SYSTEM_TX_SIGNATURE."
  (and (zerop (tx:transaction-type tx))
       (zerop (tx:transaction-r tx)) (zerop (tx:transaction-s tx))
       (let ((v (tx:transaction-v tx)))
         (if (>= v 35) (evenp (- v 35)) (= v 27)))))

(defun rlp-list-p (data end)
  "Whether DATA below END is exactly one well-formed RLP list: what (LISTP (RLP-DECODE DATA
:END END)) accepts, with RLP-ITEM-BOUNDS's canonical-form checks and RLP:*RLP-MAX-DEPTH*'s
nesting bound, but checked in place without building the items (every block from genesis
carries one such list)."
  (declare (type octets data) (type fixnum end) #.*hot*)
  (invariant (<= end (length data)) "end ~D beyond ~D octets" end (length data))
  (labels ((long-length (pos ll end)
             ;; the payload length after a long-form header at POS, or -1
             (declare (type fixnum pos ll end))
             (if (or (> (+ pos 1 ll) end) (zerop (aref data (1+ pos))))
                 -1
                 (let ((n 0))
                   (declare (type fixnum n))
                   (loop for i fixnum from (+ pos 1) below (+ pos 1 ll)
                         do (setf n (logior (ash n 8) (aref data i)))
                            (when (> n end) (return-from long-length -1)))
                   (if (<= n 55) -1 n))))
           (children (start stop depth)
             ;; a list's items, or -1; DEPTH lists may still nest below this one
             (declare (type fixnum start stop depth))
             (let ((p start))
               (declare (type fixnum p))
               (when (minusp depth) (return-from children -1))
               (loop while (< p stop)
                     do (setf p (walk p stop depth))
                        (when (minusp p) (return-from children -1)))
               stop))
           (walk (pos end depth)
             ;; the position after the item at POS, or -1 when it is malformed
             (declare (type fixnum pos end depth))
             (if (>= pos end)
                 -1
                 (let ((b (aref data pos)))
                   (cond ((< b #x80) (1+ pos))
                         ((<= b #xb7)
                          (let ((stop (+ pos 1 (- b #x80))))
                            (if (or (> stop end)
                                    (and (= b #x81) (< (1+ pos) end) (< (aref data (1+ pos)) #x80)))
                                -1
                                stop)))
                         ((< b #xc0)
                          (let* ((ll (- b #xb7)) (n (long-length pos ll end)))
                            (if (or (minusp n) (> (+ pos 1 ll n) end)) -1 (+ pos 1 ll n))))
                         ((<= b #xf7)
                          (let ((stop (+ pos 1 (- b #xc0))))
                            (if (> stop end) -1 (children (1+ pos) stop (1- depth)))))
                         (t
                          (let* ((ll (- b #xf7)) (n (long-length pos ll end)))
                            (if (or (minusp n) (> (+ pos 1 ll n) end))
                                -1
                                (children (+ pos 1 ll) (+ pos 1 ll n) (1- depth))))))))))
    (let ((depth rlp:*rlp-max-depth*))
      (declare (type fixnum depth))
      (and (plusp end) (>= (aref data 0) #xc0) (= (walk 0 end depth) end)))))

(defun validate-system-transaction (env tx index)
  "block.rs validate_system_tx: only the end-of-block subblocks metadata transaction, a call
to the zero address whose input is RLP(Vec<SubBlockMetadata>) ++ be32(block number); none
from T4."
  (let ((to (tx:transaction-to tx))
        (input (tx:transaction-input tx)))
    (unless (and to (zerop to)) (fail-tx 'invalid-transaction index "invalid system transaction"))
    (when (forks:fork>= (block-env-spec env) :t4)
      (fail-tx 'invalid-transaction index "subblocks are disabled in T4+"))
    (let ((n (length input)))
      (unless (and (>= n 32)
                   (let ((number (block-env-number env)))
                     (loop for i from 0 below 32
                           always (= (aref input (- n 1 i)) (ldb (byte 8 (* 8 i)) number))))
                   (rlp-list-p input (- n 32)))
        (fail-tx 'invalid-transaction index "invalid subblocks metadata system transaction")))
    (unless (and (zerop (tx:transaction-gas-limit tx)) (zerop (tx:transaction-gas-price tx))
                 (zerop (tx:transaction-value tx)) (zerop (tx:transaction-nonce tx))
                 (eql (tx:transaction-chain-id tx) (block-env-chain-id env)))
      (fail-tx 'invalid-transaction index "system transaction fields must be zero"))))

(define-constant +system-call-gas-limit+ 250000000 "SYSTEM_CALL_GAS_LIMIT")

(defun execute-system-transaction (env tx index)
  "config.rs execute_legacy, System: a system call from the zero address to the zero address
with SYSTEM_CALL_GAS_LIMIT; its receipt reports success and no gas. The zero address carries
no code on Tempo chains, so the call runs no instruction and changes no state; a tracer sees
one empty CALL frame."
  (validate-system-transaction env tx index)
  (when (and (not (and (null (state:world-observer evm:*world*))
                       (state:account-absent-p evm:*world* 0)))
             (plusp (state:code-size evm:*world* 0)))
    (fail-tx 'unsupported-transaction index "system call into code at the zero address"))
  (when evm:*tracer*
    (evm:trace-unstarted :call 0 0 0 0 (tx:transaction-input tx) state:+empty-code+
                         +system-call-gas-limit+ 0 :stop +system-call-gas-limit+))
  (make-tx-outcome t 0 0 '()))

;;; Pre- and post-execution system changes

(defun fork-marker-addresses (spec)
  "block.rs apply_pre_execution_changes: precompiles that receive the 0xEF marker code when
their fork is active (once; an account that already has code is left alone)."
  (append (when (forks:fork>= spec :t2) '(#xCCCCCCCC00000000000000000000000000000001)) ; ValidatorConfigV2
          (when (forks:fork>= spec :t3) '(#x5165300000000000000000000000000000000000   ; SignatureVerifier
                                          #xFDC0000000000000000000000000000000000000)) ; AddressRegistry
          (when (forks:fork>= spec :t5) '(#x4D50500000000000000000000000000000000000)) ; TIP20ChannelReserve
          (when (forks:fork>= spec :t6) '(#xB10C000000000000000000000000000000000000)) ; ReceivePolicyGuard
          (when (forks:fork>= spec :t7) '(#x1060000000000000000000000000000000000000)) ; StorageCredits
          (when (forks:fork>= spec :t8) '(#xC077E00000000000000000000000000000000000)) ; CurrentCommittee
          (when (forks:fork>= spec :t14) '(#xAACC000000000000000000000000000000000000)))) ; ConfigurableAccount

(defun deploy-marker (db address)
  "block.rs deploy_precompile_at_boundary with no storage."
  (let ((a (store:db-account db address)))
    (unless (and a (not (octets= (store:account-code-hash a) clempo.trie:+empty-code-hash+)))
      (store:db-set-account db address (if a (store:account-nonce a) 0) (if a (store:account-balance a) 0)
                            (store:db-put-code db (octets-of #xef))))))

(defun apply-pre-execution-changes (db env block)
  (when (and block (blocks:block-withdrawals block))
    (fail-tx 'invalid-transaction nil "withdrawals are not permitted"))
  (dolist (address (fork-marker-addresses (block-env-spec env)))
    (deploy-marker db address)))

(defmacro untracked ((world) &body body)
  "Run BODY with WORLD's observer off: evm2's account_info_untracked / storage_slot_untracked,
reads a block access list does not record (block.rs reads the boundary installations' targets
so)."
  (let ((w (gensym "WORLD")) (observer (gensym "OBSERVER")))
    `(let* ((,w ,world) (,observer (state:world-observer ,w)))
       (setf (state:world-observer ,w) nil)
       (unwind-protect (progn ,@body)
         (setf (state:world-observer ,w) ,observer)))))

(defun deploy-markers-in-world (world env)
  "APPLY-PRE-EXECUTION-CHANGES's markers written into WORLD (an overlay block: no database)."
  (dolist (address (fork-marker-addresses (block-env-spec env)))
    (when (zerop (untracked (world) (state:code-size world address)))
      (state:set-code world address (octets-of #xef))))
  (state:finalize-tx world))

;;; The block

(defun execute-block (db block &key senders (chain *chain*) world)
  "Execute BLOCK on DB (opening a block on it) and return a BLOCK-RESULT. SENDERS, when
given, is the list of recovered senders in transaction order. WORLD, when given, is a
clempo.state world over DB that the block may reuse; it is reset first."
  (let* ((*chain* chain)
         ;; a replay binds one cache for its whole range
         (contract:*mapping-slot-cache* (or contract:*mapping-slot-cache*
                                            (contract:make-mapping-slot-cache)))
         (env (header-block-env (blocks:block-header block) :chain chain))
         (txs (blocks:block-transactions block))
         (senders (or senders (mapcar #'tx:transaction-sender txs)))
         (ctx (begin-block db env :block block :world (and world (state:reset-world world)))))
    (loop for tx in txs for sender in senders do (apply-transaction ctx tx sender))
    (finish-block ctx)))

(defun block-mismatches (result header)
  "Header fields RESULT disagrees with, in checking order, as (field expected actual)."
  (let ((inner (headers:header-inner header))
        (out '()))
    (flet ((check (field expected actual test)
             (unless (funcall test expected actual) (push (list field expected actual) out))))
      (check :gas-used (headers:ethereum-header-gas-used inner) (block-result-gas-used result) #'eql)
      (check :receipts-root (headers:ethereum-header-receipts-root inner) (block-result-receipts-root result) #'octets=)
      (check :logs-bloom (headers:ethereum-header-logs-bloom inner) (block-result-bloom result) #'octets=)
      (check :state-root (headers:ethereum-header-state-root inner) (block-result-state-root result) #'octets=))
    (nreverse out)))
