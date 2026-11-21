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
  (proposer nil))

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
                    :proposer (when context (headers:consensus-context-proposer context)))))

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
                   (octets= (subseq input (- n 32)) (integer->octets (block-env-number env) 32))
                   (handler-case (listp (rlp:rlp-decode input :end (- n 32))) (error () nil)))
        (fail-tx 'invalid-transaction index "invalid subblocks metadata system transaction")))
    (unless (and (zerop (tx:transaction-gas-limit tx)) (zerop (tx:transaction-gas-price tx))
                 (zerop (tx:transaction-value tx)) (zerop (tx:transaction-nonce tx))
                 (eql (tx:transaction-chain-id tx) (block-env-chain-id env)))
      (fail-tx 'invalid-transaction index "system transaction fields must be zero"))))

(defstruct (tx-outcome (:constructor make-tx-outcome (status gas-used block-gas logs)))
  status gas-used block-gas (logs '() :type list))

(defun execute-system-transaction (db env tx index)
  "config.rs execute_legacy, System: a system call from the zero address to the zero address
with SYSTEM_CALL_GAS_LIMIT; its receipt reports success and no gas. The zero address carries
no code on Tempo chains, so the call runs no instruction and changes no state."
  (validate-system-transaction env tx index)
  (let ((target (store:db-account db 0)))
    (when (and target (not (octets= (store:account-code-hash target) clempo.trie:+empty-code-hash+)))
      (fail-tx 'unsupported-transaction index "system call into code at the zero address")))
  (make-tx-outcome t 0 0 '()))

(defun execute-transaction (db env tx sender index)
  (declare (ignore db env sender))
  (fail-tx 'unsupported-transaction index "type ~D user transaction: the EVM is not wired in yet"
           (tx:transaction-type tx)))

;;; Pre- and post-execution system changes

(defparameter *fork-markers*
  '((:t2 . #xCCCCCCCC00000000000000000000000000000001)  ; ValidatorConfigV2
    (:t3 . #x5165300000000000000000000000000000000000)  ; SignatureVerifier
    (:t3 . #xFDC0000000000000000000000000000000000000)  ; AddressRegistry
    (:t5 . #x4D50500000000000000000000000000000000000)  ; TIP20ChannelReserve
    (:t6 . #xB10C000000000000000000000000000000000000)  ; ReceivePolicyGuard
    (:t7 . #x1060000000000000000000000000000000000000)  ; StorageCredits
    (:t8 . #xC077E00000000000000000000000000000000000)  ; CurrentCommittee
    (:t14 . #xAACC000000000000000000000000000000000000)); ConfigurableAccount
  "block.rs apply_pre_execution_changes: precompiles that receive the 0xEF marker code when
their fork is active (once; an account that already has code is left alone).")

(defun deploy-marker (db address)
  "block.rs deploy_precompile_at_boundary with no storage."
  (let ((a (store:db-account db address)))
    (unless (and a (not (octets= (store:account-code-hash a) clempo.trie:+empty-code-hash+)))
      (store:db-set-account db address (if a (store:account-nonce a) 0) (if a (store:account-balance a) 0)
                            (store:db-put-code db (octets-of #xef))))))

(defun apply-pre-execution-changes (db env block)
  (let ((spec (block-env-spec env)))
    (when (blocks:block-withdrawals block)
      (fail-tx 'invalid-transaction nil "withdrawals are not permitted"))
    (when (forks:fork>= spec :t10)
      (fail-tx 'unsupported-transaction nil "T10 zone factory installation is not implemented"))
    (dolist (m *fork-markers*)
      (when (forks:fork>= spec (car m)) (deploy-marker db (cdr m))))))

;;; The block

(defun execute-block (db block &key senders (chain *chain*))
  "Execute BLOCK on DB (opening a block on it) and return a BLOCK-RESULT. SENDERS, when
given, is the list of recovered senders in transaction order."
  (let* ((header (blocks:block-header block))
         (env (header-block-env header :chain chain))
         (txs (blocks:block-transactions block))
         (senders (or senders (mapcar #'tx:transaction-sender txs)))
         (cumulative 0) (block-gas 0) (receipts '()) (seen-system nil))
    (forks:with-spec ((block-env-spec env))
      (store:db-begin-block db)
      (apply-pre-execution-changes db env block)
      (loop for tx in txs for sender in senders for index from 0
            do (let ((outcome
                       (cond ((system-transaction-p tx)
                              (when seen-system
                                (fail-tx 'invalid-transaction index
                                         "duplicate subblocks metadata system transaction"))
                              (setf seen-system t)
                              (execute-system-transaction db env tx index))
                             (seen-system
                              (fail-tx 'invalid-transaction index
                                       "regular transaction can't follow system transaction"))
                             (t (execute-transaction db env tx sender index)))))
                 (incf cumulative (tx-outcome-gas-used outcome))
                 (incf block-gas (tx-outcome-block-gas outcome))
                 (push (receipts:make-receipt :type (tx:transaction-type tx)
                                              :status (if (tx-outcome-status outcome) 1 0)
                                              :cumulative-gas-used cumulative
                                              :logs (tx-outcome-logs outcome))
                       receipts)))
      (setf receipts (nreverse receipts))
      (make-block-result :number (block-env-number env)
                         :gas-used block-gas
                         :receipts receipts
                         :receipts-root (receipts:receipts-root receipts)
                         :bloom (receipts:receipts-bloom receipts)
                         :state-root (store:db-state-root db)
                         :tx-count (length txs)))))

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
