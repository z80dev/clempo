(in-package #:clempo.executor)

;;; Payment-lane classification, the T14 proposer fee recipient, and the storage writes of
;;; the running transaction.

;;; Payment lane (tempo crates/evm/src/block.rs TempoBlockExecutor::is_payment)

(defun payment-transaction-p (tx fork)
  "Whether decoded TX qualifies for the payment lane under FORK: TIP-1045 with bounded
trailing calldata from T15, TIP-1045 from T5, the legacy TIP-20 prefix check before."
  (tx:transaction-payment-p tx (cond ((forks:fork>= fork :t15) :v3)
                                     ((forks:fork>= fork :t5) :v2)
                                     (t :v1))))

;;; Proposer fee recipient (tempo crates/evm/src/common.rs proposer_fee_recipient)
;;;
;;; ValidatorConfigV2 layout (crates/precompiles/src/validator_config_v2/mod.rs):
;;;   slot 0  Config: owner (bytes 0-19), is_init (20), init_at_height u64 (21-28), ...
;;;   slot 1  validators: Vec<ValidatorRecord>, length here, elements from keccak(1)
;;;   slot 3  pubkey_to_index: Mapping<B256, u64>, 1-indexed (0 = absent)
;;; A ValidatorRecord takes 6 slots: public_key, validator_address, ingress, egress,
;;; fee_recipient (bytes 0-19) + index (20-27), active_idx + added + deactivated.

(define-constant +vc2-validators-slot+ 1)
(define-constant +vc2-pubkey-to-index-slot+ 3)
(define-constant +vc2-record-slots+ 6)
(define-constant +vc2-fee-recipient-offset+ 4)

(defun vc2-word (n) (integer->octets n 32))

(defun proposer-fee-recipient (db env proposer-key)
  "The fee recipient ValidatorConfigV2 records for the validator with Ed25519 PROPOSER-KEY
\(32 octets or an integer), as the committed state DB (the parent block's state) holds it;
NIL before T2, when V2 was not initialized as of the parent block, or when the key (NIL
when the header has no consensus context) has no record. Reads DB directly, outside any
world journal."
  (let ((spec (block-env-spec env))
        (parent (max 0 (1- (block-env-number env))))
        (addr pc:+validator-config-v2-address+))
    (when (and proposer-key (forks:fork>= spec :t2))
      (let ((config (store:db-storage db addr 0)))
        (when (and (logbitp 160 config)
                   (<= (ldb (byte 64 168) config) parent))
          (let* ((key (if (integerp proposer-key) (vc2-word proposer-key) proposer-key))
                 (idx1 (ldb (byte 64 0)
                            (store:db-storage
                             db addr (octets->integer
                                      (clempo.keccak:keccak256-concat
                                       key (vc2-word +vc2-pubkey-to-index-slot+)))))))
            (unless (zerop idx1)
              (let ((base (octets->integer
                           (clempo.keccak:keccak256 (vc2-word +vc2-validators-slot+)))))
                (ldb (byte 160 0)
                     (store:db-storage db addr (ldb (byte 256 0)
                                                    (+ base (* (1- idx1) +vc2-record-slots+)
                                                       +vc2-fee-recipient-offset+))))))))))))

;;; Storage writes of the running transaction

(defun tip20-prefix-p (address)
  "Whether ADDRESS starts with the TIP-20 prefix 0x20c000000000000000000000."
  (= (ash address -64) #x20c000000000000000000000))

(defun collect-storage-changes (world &key (address-filter #'tip20-prefix-p))
  "Every slot the running transaction of WORLD wrote at an address ADDRESS-FILTER accepts,
as a list of (address slot original current): ORIGINAL is the value at the start of the
transaction, CURRENT the present one. Call before STATE:FINALIZE-TX."
  (let ((out '()))
    (dolist (account (state::world-tx-accounts world) out)
      (let ((present (state::account-present account))
            (address (state:account-address account)))
        (when (and present (plusp (hash-table-count present))
                   (or (null address-filter) (funcall address-filter address)))
          (maphash (lambda (slot current)
                     (push (list address slot
                                 (state:account-storage-original world account slot)
                                 current)
                           out))
                   present))))))
