(in-package #:clempo.executor)

;;; The block's system paths: calls the protocol makes outside any user transaction, and the
;;; state it installs at fork boundaries. tempo crates/evm/src/block.rs
;;; (apply_pre_execution_changes, apply_current_committee_system_call,
;;; deploy_zone_factory_at_boundary, upgrade_zone_runtimes, install_zone_runtimes,
;;; zone_runtime_fork), evm2 crates/evm2/src/evm/system.rs (SystemTx, execute_system_call)
;;; and evm2 crates/evm2/src/block/ethereum/execution.rs
;;; (pre_execution_system_call_state_changes, the inner executor before T13).

(define-constant +system-address+ #xfffffffffffffffffffffffffffffffffffffffe
  "evm2 SYSTEM_ADDRESS: the default caller of a system call.")
(define-constant +evm-system-call-gas-limit+ 30000000
  "evm2 SYSTEM_CALL_GAS_LIMIT, the default SystemTx gas (Tempo's own system calls use
+SYSTEM-CALL-GAS-LIMIT+, 250,000,000).")
(define-constant +history-storage-address+ #x0000F90827F1C53a10cb7A02335B175320002935
  "EIP-2935. On mainnet its code arrives in block 1,507,667; before that the call is a no-op.")
(define-constant +beacon-roots-address+ #x000F3df6D732807Ef1319fB7B8bB8522d0Beac02
  "EIP-4788. Never deployed on Tempo mainnet, so its call is a no-op there.")

;;; One system call (evm2 Evm::system_call).

(defun run-system-call (ctx caller target input gas-limit)
  "evm2 Evm::system_call: one top-level CALL from CALLER to TARGET with INPUT, zero value and
GAS-LIMIT gas, outside any user transaction: no validation, nonce, fee or refund. Only
TARGET starts warm (not the caller, the beneficiary or the precompiles); ORIGIN is CALLER
and GASPRICE 0; a 7702-delegated TARGET runs its delegate's code; a TARGET without code
succeeds with no effect. The tracer is off, as evm2 runs system calls without the
inspector. A failed call has its state rolled back by the frame. Either way the call is
then finalized into CTX's world (state:finalize-tx), which carries it into the block.
Returns (values success-p output)."
  (with-block-context (ctx)
    (let ((world evm:*world*)
          (done nil))
      (invariant (zerop (state:checkpoint world)) "system call inside an open transaction")
      (unwind-protect
           (let ((evm:*tracer* nil)
                 (evm:*tx-env* (evm:make-tx-env :origin caller :gas-price 0))
                 (*non-creditable* nil)
                 (*tx-index* nil))
             (state:warm-address world target)
             (multiple-value-bind (status gas-left output)
                 (run-initial-frame caller 0 target input 0 gas-limit)
               (declare (ignore gas-left))
               (state:finalize-tx world)
               (setf done t)
               (values (evm:success-p status) output)))
        (unless done (state:discard-tx world))))))

;;; EIP-4788 and EIP-2935, every block.

(defun ethereum-fork-active-p (key timestamp)
  "Whether the Ethereum fork whose genesis config key is KEY (\"cancunTime\", \"pragueTime\")
is active at TIMESTAMP on *CHAIN* (reth is_*_active_at_timestamp)."
  (let ((at (clempo.json:jget (chainspec:chain-config *chain*) key)))
    (and at (not (eq at :null))
         (<= (etypecase at
               (integer at)
               (string (if (and (> (length at) 2) (string-equal "0x" at :end2 2))
                           (hex->integer at)
                           (parse-integer at))))
             timestamp))))

(defun apply-history-and-beacon-calls
    (ctx &key (parent-beacon-block-root (block-env-parent-beacon-block-root (ctx-env ctx))))
  "The pre-transaction system calls. From T13 (block.rs apply_pre_execution_changes): the
EIP-2935 call with the parent hash when Prague is active and the block is not genesis.
Before T13 (the inner Ethereum executor, pre_execution_system_call_state_changes): under
Cancun the EIP-4788 call with PARENT-BEACON-BLOCK-ROOT (the block env's, from the header;
Tempo's assembler always writes zero, assemble.rs), then the EIP-2935 call. Both go from
+SYSTEM-ADDRESS+ with +EVM-SYSTEM-CALL-GAS-LIMIT+ gas; a revert or halt is accepted. A Cancun
block without a parent beacon block root (NIL), or a genesis block with a non-zero one, is
invalid."
  (let* ((env (ctx-env ctx))
         (number (block-env-number env))
         (timestamp (block-env-timestamp env))
         (prague (ethereum-fork-active-p "pragueTime" timestamp)))
    (flet ((history ()
             (when (and prague (plusp number))
               (run-system-call ctx +system-address+ +history-storage-address+
                                (block-env-parent-hash env) +evm-system-call-gas-limit+))))
      (if (forks:fork>= (block-env-spec env) :t13)
          (history)
          (progn
            (when (ethereum-fork-active-p "cancunTime" timestamp)
              (unless parent-beacon-block-root
                (fail-tx 'invalid-transaction nil "missing parent beacon block root"))
              (if (zerop number)
                  (unless (every #'zerop parent-beacon-block-root)
                    (fail-tx 'invalid-transaction nil
                             "Cancun genesis parent beacon block root ~A is not zero"
                             (octets->hex parent-beacon-block-root)))
                  (run-system-call ctx +system-address+ +beacon-roots-address+
                                   parent-beacon-block-root +evm-system-call-gas-limit+)))
            (history))))))

;;; The CurrentCommittee call at an epoch's last block (T8).
;;; OnchainDkgOutcome (crates/dkg-onchain-artifacts) in the commonware codec, read with
;;; MAX_VALIDATORS and ModeVersion v0; only the epoch and the players are used here.

(define-constant +dkg-max-validators+ 65535 "OnchainDkgOutcome's MAX_VALIDATORS (u16::MAX).")

(defun write-dkg-public-key (w key) (clempo.cwcodec:write-raw w key))
(defun read-dkg-public-key (r)
  "ed25519 PublicKey::read: 32 bytes that decode to a curve point."
  (let ((key (clempo.cwcodec:read-raw r 32)))
    (unless (clempo.ed25519:ed25519-public-key-valid-p key)
      (clempo.cwcodec:cw-fail "ed25519: invalid public key"))
    key))
(clempo.cwcodec:defcwtype dkg-public-key :write write-dkg-public-key :read read-dkg-public-key)

(defun write-dkg-g2 (w point) (clempo.cwcodec:write-raw w point))
(defun read-dkg-g2 (r)
  "MinSig G2::read: a compressed point of the prime-order subgroup, not the identity."
  (let ((octets (clempo.cwcodec:read-raw r 96)))
    (unless (handler-case (clempo.bls12-381:g2-decompress octets)
              (clempo.bls12-381:invalid-point () nil))
      (clempo.cwcodec:cw-fail "G2: invalid compressed point"))
    octets))
(clempo.cwcodec:defcwtype dkg-g2 :write write-dkg-g2 :read read-dkg-g2)

(defun write-dkg-sharing-mode (w mode) (declare (ignore mode)) (clempo.cwcodec:write-u8 w 0))
(defun read-dkg-sharing-mode (r)
  "Mode under ModeVersion v0: NonZeroCounter (tag 0) only."
  (let ((tag (clempo.cwcodec:read-u8 r)))
    (unless (zerop tag) (clempo.cwcodec:cw-fail "Mode: tag ~D unsupported in version 0" tag))
    :nonzero-counter))
(clempo.cwcodec:defcwtype dkg-sharing-mode :write write-dkg-sharing-mode :read read-dkg-sharing-mode)

(defun write-dkg-sharing-total (w n) (clempo.cwcodec:write-uint w n 4))
(defun read-dkg-sharing-total (r)
  (let ((n (clempo.cwcodec:read-uint r 4)))
    (unless (<= 1 n +dkg-max-validators+) (clempo.cwcodec:cw-fail "Sharing: total ~D not in range" n))
    n))
(clempo.cwcodec:defcwtype dkg-sharing-total :write write-dkg-sharing-total :read read-dkg-sharing-total)

(clempo.cwcodec:defcodec dkg-sharing
  (mode dkg-sharing-mode)
  (total dkg-sharing-total)
  (coefficients (:vec dkg-g2 :min 1 :max +dkg-max-validators+)))

(clempo.cwcodec:defcodec dkg-output
  (summary (:array 32))
  (public dkg-sharing)
  (dealers (:set dkg-public-key :min 1 :max +dkg-max-validators+))
  (players (:set dkg-public-key :min 1 :max +dkg-max-validators+))
  (revealed (:set dkg-public-key :min 0 :max +dkg-max-validators+)))

(clempo.cwcodec:defcodec dkg-outcome
  (epoch :varint)
  (output dkg-output)
  (next-players (:set dkg-public-key :min 1 :max +dkg-max-validators+))
  (is-next-full-dkg :bool))

(defun decode-boundary-outcome (extra-data)
  "OnchainDkgOutcome::read of a boundary block's EXTRA-DATA. Like the committee call, trailing
bytes are ignored. Signals INVALID-TRANSACTION (block-level) when it does not decode."
  (handler-case (values (clempo.cwcodec:cw-decode 'dkg-outcome extra-data :partial t))
    (clempo.cwcodec:cw-error (e)
      (fail-tx 'invalid-transaction nil
               "failed decoding boundary block extra data as DKG outcome: ~A" e))))

(defun set-committee-members-calldata (epoch public-keys)
  "ICurrentCommittee.setCommitteeMembers(uint64 epoch, bytes32[] publicKeys)."
  (clempo.abi:abi-encode-call (clempo.abi:abi-selector "setCommitteeMembers(uint64,bytes32[])")
                              '((:uint 64) (:array (:bytes-n 32)))
                              (list epoch (mapcar #'octets->integer public-keys))))

(defun epoch-boundary-p (number)
  "The block before an epoch starts: (NUMBER + 1) mod the chain's epoch length (1 when the
chain spec has none, as TempoBlockExt.epoch_length) is zero."
  (zerop (mod (1+ number) (or (chainspec:chain-epoch-length (chainspec:load-chainspec *chain*)) 1))))

(defun apply-committee-call (ctx)
  "block.rs apply_current_committee_system_call: from T8, at an epoch's last block, write
the DKG outcome's epoch and players (from the header's extra data) into CurrentCommittee
with setCommitteeMembers, a system call from the zero address with +SYSTEM-CALL-GAS-LIMIT+
gas. An undecodable outcome or a failed call makes the block invalid."
  (let ((env (ctx-env ctx)))
    (when (and (forks:fork>= (block-env-spec env) :t8)
               (epoch-boundary-p (block-env-number env)))
      (let* ((outcome (decode-boundary-outcome (block-env-extra-data env)))
             (calldata (set-committee-members-calldata
                        (dkg-outcome-epoch outcome)
                        (dkg-output-players (dkg-outcome-output outcome)))))
        (unless (run-system-call ctx 0 pc:+current-committee-address+ calldata
                                 +system-call-gas-limit+)
          (fail-tx 'invalid-transaction nil "current committee system call failed"))))))

;;; The zone factory and the shared zone runtimes (TIP-1091, T10; upgraded at T13, T14, T15).
;;; The runtime bytecode is read from data/ when this file is compiled (data/README.md has
;;; its provenance; tests pin every runtime by its code hash).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun runtime-file-octets (name)
    (hex->octets (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (clempo.io:read-file-string
                               (merge-pathnames (concatenate 'string "data/" name)
                                                (or *compile-file-truename* *load-truename*)))))))

(defmacro zone-runtime (name)
  "The octets of data/NAME, as a literal."
  (runtime-file-octets name))

(define-constant +initial-factory-owner+ #xaF571FD4B3AD43a5807A5E58bFb25ea1aB327A14
  "INITIAL_FACTORY_OWNER, installed by the T10 activation.")

(defun initial-zone-factory-config (owner)
  "initial_zone_factory_config: initialized flag in bit 0, the owner from bit 32."
  (logior 1 (ash owner 32)))

(defparameter *zone-runtimes*
  (list (list :t10 (zone-runtime "zone-portal-t10.hex") (zone-runtime "zone-verifier-t10.hex")
              (zone-runtime "zone-messenger-t10.hex"))
        (list :t13 (zone-runtime "zone-portal-t13.hex") (zone-runtime "zone-verifier-t13.hex")
              (zone-runtime "zone-messenger-t13.hex"))
        (list :t14 (zone-runtime "zone-portal-t14.hex") (zone-runtime "zone-verifier-t13.hex")
              (zone-runtime "zone-messenger-t13.hex"))
        (list :t15 (zone-runtime "zone-portal-t15.hex") (zone-runtime "zone-verifier-t13.hex")
              (zone-runtime "zone-messenger-t13.hex")))
  "(fork portal verifier messenger): initial_zone_factory_state and t13/t14/t15_zone_factory_state.
T14 and T15 replace only the portal.")

(defun zone-runtimes (fork)
  "((address . code) ...) of the shared runtimes FORK installs, portal, verifier, messenger."
  (destructuring-bind (portal verifier messenger) (rest (assoc fork *zone-runtimes*))
    (list (cons pc:+zone-portal-impl-address+ portal)
          (cons pc:+zone-verifier-address+ verifier)
          (cons pc:+zone-messenger-address+ messenger))))

(defun zone-runtime-fork (spec)
  "zone_runtime_fork: the fork whose shared runtimes are live under SPEC, NIL before T13."
  (cond ((forks:fork>= spec :t15) :t15)
        ((forks:fork>= spec :t14) :t14)
        ((forks:fork>= spec :t13) :t13)))

(defun has-code-p (account)
  "!info.is_empty_code_hash() of an account that exists."
  (not (octets= (state:account-code-hash account) state:+empty-code-hash+)))

(defun install-zone-runtimes (world runtimes)
  "install_zone_runtimes: give each (address . code) its code, keeping nonce, balance and
storage; an account that already has that code is left alone."
  (loop for (address . code) in runtimes
        for account = (state:get-account world address)
        unless (octets= (state:account-code-hash account) (state:code-hash code))
          do (state:set-code world account code)))

(defun deploy-zone-factory (world)
  "deploy_zone_factory_at_boundary: unless the factory already has code (a genesis allocation
or an earlier installation), give it the 0xEF marker and its initial configuration in slot
0, and install the initial (T10) runtimes."
  (let ((factory (state:get-account world pc:+zone-factory-address+)))
    (unless (has-code-p factory)
      (state:set-code world factory (octets-of #xef))
      (state:sstore world pc:+zone-factory-address+ 0
                    (initial-zone-factory-config +initial-factory-owner+))
      (install-zone-runtimes world (zone-runtimes :t10)))))

(defun apply-zone-factory (ctx)
  "block.rs apply_pre_execution_changes, zone part: from T10 the factory installation, then,
when the runtime set live under the block's fork (ZONE-RUNTIME-FORK) differs from the one
live under the genesis fork, that set's runtimes (a chain starting at a runtime fork keeps
its genesis allocation). The writes go through CTX's world and are finalized into the block."
  (with-block-context (ctx)
    (let ((world evm:*world*)
          (spec forks:*spec*))
      (invariant (zerop (state:checkpoint world)) "zone installation inside an open transaction")
      (when (forks:fork>= spec :t10)
        (deploy-zone-factory world))
      (let ((runtime-fork (zone-runtime-fork spec)))
        (when (and runtime-fork
                   (not (eq runtime-fork
                            (zone-runtime-fork (chainspec:fork-at (chainspec:genesis-timestamp *chain*)
                                                                  *chain*)))))
          (install-zone-runtimes world (zone-runtimes runtime-fork))))
      (state:finalize-tx world))))
