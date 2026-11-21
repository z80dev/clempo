(defpackage #:clempo.builder.tests
  (:use #:cl #:clempo.base #:clempo.builder)
  (:local-nicknames (#:ex #:clempo.executor) (#:pool #:clempo.txpool) (#:store #:clempo.store)
                    (#:tx #:clempo.transactions) (#:receipts #:clempo.receipts)
                    (#:headers #:clempo.headers) (#:blocks #:clempo.blocks)
                    (#:rlp #:clempo.rlp) (#:trie #:clempo.trie)))

(in-package #:clempo.builder.tests)

;;; Mainnet blocks 3211..3874 (data/README.md): every block of 1..3874 with a user transaction,
;;; with its parent header. Each holds one user transaction and the reference's pre-T4
;;; subblocks metadata system transaction; the blocks between them change no state.

(defparameter *fixture*
  (asdf:system-relative-pathname
   "clempo-tempo-builder" "../../../tests/tempo/builder/data/mainnet-user-blocks-3211-3874.sexp"))

(defvar *entries* nil)

(defun entries ()
  "((number parent-header block) ...), oldest first."
  (or *entries*
      (setf *entries*
            (with-input-from-string (in (clempo.io:read-file-string *fixture*))
              (let ((*read-eval* nil))
                (loop for entry = (read in nil) while entry
                      collect (list (getf entry :number)
                                    (headers:decode-header (hex->octets (getf entry :parent-header)))
                                    (blocks:decode-block (hex->octets (getf entry :block))))))))))

(defun entry (number) (or (assoc number (entries)) (error "no fixture block ~D" number)))

(defun system-transaction-p (tx)
  (and (zerop (tx:transaction-type tx)) (zerop (tx:transaction-r tx)) (zerop (tx:transaction-s tx))))

(defun user-transactions (block) (remove-if #'system-transaction-p (blocks:block-transactions block)))

(defun recorded-attributes (header)
  "The attributes the recorded HEADER was proposed with."
  (let ((inner (headers:header-inner header)))
    (make-payload-attributes
     :timestamp (headers:ethereum-header-timestamp inner)
     :timestamp-millis-part (headers:header-timestamp-millis-part header)
     :suggested-fee-recipient (headers:ethereum-header-beneficiary inner)
     :prev-randao (headers:ethereum-header-mix-hash inner)
     :parent-beacon-block-root (headers:ethereum-header-parent-beacon-block-root inner)
     :extra-data (headers:ethereum-header-extra-data inner)
     :consensus-context (headers:header-consensus-context header)
     :target-gas-limit (headers:ethereum-header-gas-limit inner))))

(defun pool-with (db parent attributes transactions)
  "A fresh pool holding TRANSACTIONS, admitted against DB for the block after PARENT. Every
header of mainnet blocks 1..3874 has beneficiary 0 (data/README.md), so the validator tokens
of the last ten headers are address 0's."
  (let* ((env (next-block-env db parent attributes :chain :mainnet))
         (reader (pool:make-state-reader
                  db env :recent-validator-tokens (list (pool:validator-fee-token db env 0))))
         (p (pool:make-pool)))
    (dolist (tx transactions p)
      (pool:submit-transaction p tx reader :timestamp (ex:block-env-timestamp env)
                                           :fork (ex:block-env-spec env)))))

(defun inner (block) (headers:header-inner (blocks:block-header block)))

(defun hashes (transactions) (mapcar #'tx:transaction-hash transactions))

;;; Pure pieces

(deftest gas-limit-follows-parent-bound ()
  ;; alloy calculate_block_gas_limit: within parent +- (parent / 1024 - 1).
  (is= 30000000 (next-block-gas-limit 30000000 30000000))
  (is= 30029295 (next-block-gas-limit 30000000 40000000))
  (is= 29970705 (next-block-gas-limit 30000000 20000000))
  (is= 30010000 (next-block-gas-limit 30000000 30010000))
  (is= 500000000 (next-block-gas-limit 500000000 500000000))
  (is= 1000 (next-block-gas-limit 1000 5000)))

(deftest transaction-fit-limits ()
  (let ((cap 30000000))
    (is (null (transaction-fit 21000 cap 0 30000000 nil 0 30000000)))
    (is (null (transaction-fit 50 cap 950 1000 t 0 0)) "exactly fills the block")
    (is (eq :exceeds-block-gas-limit (transaction-fit 51 cap 950 1000 t 0 0)))
    (is (null (transaction-fit 100 cap 0 1000 t 900 950)) "payments ignore the general limit")
    (is (eq :exceeds-general-gas-limit (transaction-fit 100 cap 0 1000 nil 900 950)))
    (is (null (transaction-fit 40000000 cap 0 30000000 t 0 0)) "charged at most the tx cap")
    (is (eq :exceeds-block-gas-limit (transaction-fit 40000000 cap 1 30000000 t 0 0)))))

(deftest fee-token-addresses ()
  ;; tempo primitives address.rs is_tip20_prefix; hardfork lib.rs fallback_fee_tokens.
  (is (clempo.builder::tip20-address-p #x20c0000000000000000000000000000000000000))
  (is (clempo.builder::tip20-address-p #x20c0000000000000000000006a37da5c996874be))
  (is (not (clempo.builder::tip20-address-p #x20c0000000000000000000010000000000000000)))
  (is (not (clempo.builder::tip20-address-p #xfeec000000000000000000000000000000000000)))
  (is-equalp '(#x20c0000000000000000000000000000000000000)
             (clempo.builder::fallback-fee-tokens :t12))
  (is-equalp '(#x20c0000000000000000000000000000000000000
               #x20c0000000000000000000006a37da5c996874be)
             (clempo.builder::fallback-fee-tokens :t13)))

(deftest header-constants-match-mainnet ()
  (let ((h (inner (third (entry 3211)))))
    (is-equalp (hex->octets "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")
               +empty-ommers-hash+)
    (is-equalp (headers:ethereum-header-ommers-hash h) +empty-ommers-hash+)
    (is-equalp (headers:ethereum-header-requests-hash h) +empty-requests-hash+)
    (is-equalp (headers:ethereum-header-withdrawals-root h) trie:+empty-trie-root+)))

(deftest block-transaction-lengths-sum-to-body ()
  ;; The recorded body's transaction list is exactly the per-transaction lengths plus its
  ;; list header (encode.rs block_transaction_length).
  (dolist (e (entries))
    (let* ((block (third e))
           (items (second (rlp:rlp-decode (blocks:block-raw block))))
           (sum (reduce #'+ (mapcar #'block-transaction-length (blocks:block-transactions block)))))
      (is= (rlp:rlp-encoded-length items) (+ sum (rlp:header-length sum)) "block ~D" (first e)))))

;;; Building on the executor (mainnet genesis = state of blocks 1..3210)

(deftest next-env-from-mainnet-parent ()
  (destructuring-bind (number parent block) (entry 3211)
    (let* ((db (ex:genesis-db :chain :mainnet))
           (h (inner block))
           (env (next-block-env db parent (recorded-attributes (blocks:block-header block))
                                :chain :mainnet)))
      (is= number (ex:block-env-number env))
      (is= (headers:ethereum-header-timestamp h) (ex:block-env-timestamp env))
      (is= (headers:ethereum-header-base-fee-per-gas h) (ex:block-env-base-fee env))
      (is= (headers:ethereum-header-gas-limit h) (ex:block-env-gas-limit env))
      (is= (headers:ethereum-header-beneficiary h) (ex:block-env-beneficiary env))
      (is= 4217 (ex:block-env-chain-id env))
      (is (eq :t0 (ex:block-env-spec env))))))

(deftest empty-payload-on-mainnet-parent ()
  (destructuring-bind (number parent block) (entry 3211)
    (let* ((db (ex:genesis-db :chain :mainnet))
           (root (store:db-root-hash db))
           (recorded (inner block))
           (built (build-payload db parent (recorded-attributes (blocks:block-header block))
                                 :chain :mainnet))
           (b (built-payload-block built))
           (header (blocks:block-header b))
           (h (headers:header-inner header)))
      (is (null (blocks:block-transactions b)))
      (is (eq :pool-empty (built-payload-stop-reason built)))
      (is (not (store:db-block-open-p db)))
      (is-equalp root (store:db-root-hash db) "the state is left as it was")
      (is-equalp root (headers:ethereum-header-state-root h) "T0 empty block changes nothing")
      (is= 0 (headers:ethereum-header-gas-used h))
      (is= number (headers:ethereum-header-number h))
      (is-equalp (headers:ethereum-header-parent-hash recorded) (headers:ethereum-header-parent-hash h))
      (is= (headers:ethereum-header-timestamp recorded) (headers:ethereum-header-timestamp h))
      (is= (headers:header-timestamp-millis (blocks:block-header block)) (headers:header-timestamp-millis header))
      (is= (headers:ethereum-header-gas-limit recorded) (headers:ethereum-header-gas-limit h))
      (is= (headers:ethereum-header-base-fee-per-gas recorded) (headers:ethereum-header-base-fee-per-gas h))
      ;; The reference head: pre-T1 general = gas limit / 2 with no shared (subblock) gas.
      (is= 250000000 (headers:header-general-gas-limit header))
      (is= 0 (headers:header-shared-gas-limit header))
      (dolist (field '(headers:ethereum-header-ommers-hash headers:ethereum-header-mix-hash
                       headers:ethereum-header-nonce headers:ethereum-header-withdrawals-root
                       headers:ethereum-header-parent-beacon-block-root
                       headers:ethereum-header-requests-hash headers:ethereum-header-blob-gas-used
                       headers:ethereum-header-excess-blob-gas headers:ethereum-header-extra-data
                       headers:ethereum-header-beneficiary headers:ethereum-header-difficulty
                       headers:ethereum-header-slot-number
                       headers:ethereum-header-block-access-list-hash))
        (is-equalp (funcall field recorded) (funcall field h) "~A" field))
      (is-equalp trie:+empty-trie-root+ (headers:ethereum-header-transactions-root h))
      (is-equalp trie:+empty-trie-root+ (headers:ethereum-header-receipts-root h))
      (is-equalp (make-octets 256) (headers:ethereum-header-logs-bloom h))
      (is (null (blocks:block-body-mismatches b)))
      ;; Encoded and decoded, the block is the same block.
      (is-equalp (blocks:block-hash b)
                 (blocks:block-hash (blocks:decode-block (blocks:encode-block b))))
      ;; The executor, importing the built block, reaches the same commitments.
      (let ((result (ex:execute-block db b :chain :mainnet)))
        (is (null (ex:block-mismatches result header)))
        (store:db-revert-block db)))))

(deftest payload-not-better-is-aborted ()
  (destructuring-bind (number parent block) (entry 3211)
    (declare (ignore number))
    (let* ((db (ex:genesis-db :chain :mainnet))
           (root (store:db-root-hash db)))
      (multiple-value-bind (outcome fees)
          (build-payload db parent (recorded-attributes (blocks:block-header block))
                         :chain :mainnet :best-fees 0)
        (is (eq :aborted outcome))
        (is= 0 fees))
      (is (not (store:db-block-open-p db)))
      (is-equalp root (store:db-root-hash db)))))

(deftest stale-state-root-is-refused ()
  (destructuring-bind (number parent block) (entry 3463)
    (declare (ignore number))
    ;; Block 3463's parent follows 3211's user transaction; genesis is not its state.
    (let ((db (ex:genesis-db :chain :mainnet)))
      (signals payload-build-error
               (build-payload db parent (recorded-attributes (blocks:block-header block))
                              :chain :mainnet)))))

(defun rebuild-recorded (db entry)
  "Build ENTRY's block from a pool holding its user transaction, commit it, and check it
against the recorded block: same transactions, gas, state root and bloom, and the recorded
transactions and receipts roots once the reference's subblocks metadata transaction (status 1,
no gas, no logs) is appended. Returns the BUILT-PAYLOAD."
  (destructuring-bind (number parent block) entry
    (let* ((recorded (inner block))
           (attributes (recorded-attributes (blocks:block-header block)))
           (user (user-transactions block))
           (system (find-if #'system-transaction-p (blocks:block-transactions block)))
           (built (progn
                    (is-equalp (headers:header-state-root parent) (store:db-root-hash db)
                               "block ~D parent state" number)
                    (build-payload db parent attributes :chain :mainnet :commit t
                                   :pool (pool-with db parent attributes user))))
           (b (built-payload-block built))
           (h (inner b))
           (gas (headers:ethereum-header-gas-used h)))
      (is-equalp (hashes user) (hashes (blocks:block-transactions b)) "block ~D txs" number)
      (is (null (built-payload-skipped built)) "block ~D skipped ~S" number (built-payload-skipped built))
      (is= (headers:ethereum-header-gas-used recorded) gas "block ~D gas" number)
      (is-equalp (headers:ethereum-header-state-root recorded) (headers:ethereum-header-state-root h)
                 "block ~D state root" number)
      (is-equalp (headers:ethereum-header-logs-bloom recorded) (headers:ethereum-header-logs-bloom h)
                 "block ~D bloom" number)
      (is-equalp (headers:ethereum-header-transactions-root recorded)
                 (blocks:transactions-root (append (blocks:block-transactions b) (list system)))
                 "block ~D transactions root" number)
      (is-equalp (headers:ethereum-header-receipts-root recorded)
                 (receipts:receipts-root
                  (append (built-payload-receipts built)
                          (list (receipts:make-receipt :type 0 :status 1 :cumulative-gas-used gas))))
                 "block ~D receipts root" number)
      (is-equalp (headers:ethereum-header-state-root h) (store:db-root-hash db) "block ~D committed" number)
      built)))

(deftest rebuilds-recorded-mainnet-blocks ()
  (let ((db (ex:genesis-db :chain :mainnet)))
    (dolist (e (entries))
      (rebuild-recorded db e))))

(deftest pooled-block-skips-spent-nonce-and-reexecutes ()
  (let* ((db (ex:genesis-db :chain :mainnet))
         (later (member 3851 (entries) :key #'first))
         (first-later (first later)))
    (dolist (e (ldiff (entries) later))
      (rebuild-recorded db e))
    (destructuring-bind (number parent block) first-later
      (declare (ignore number))
      ;; Only these funding transactions can be admitted against this parent.
      ;; Other recorded transactions spend balances/keys created by later blocks.
      (let* ((stale (pool-with db parent (recorded-attributes (blocks:block-header block))
                               (loop for (number nil b) in later
                                     when (member number '(3851 3853 3856))
                                       append (user-transactions b))))
             (spent (first (user-transactions block)))
             ;; Block 3851 is built and committed from its own pool; STALE still holds its tx.
             (parent (blocks:block-header (built-payload-block (rebuild-recorded db first-later))))
             (root (store:db-root-hash db))
             (attributes (recorded-attributes
                          (blocks:block-header (third (car (last later))))))
             (built (build-payload db parent attributes :pool stale :chain :mainnet))
             (b (built-payload-block built))
             (included (hashes (blocks:block-transactions b))))
        (is (not (member (tx:transaction-hash spent) included :test #'equalp)))
        (is (eq :nonce-too-low
                (cdr (assoc (tx:transaction-hash spent) (built-payload-skipped built) :test #'equalp)))
            "the spent transaction is rolled back and skipped: ~S" (built-payload-skipped built))
        (is (>= (length included) 2) "included ~D" (length included))
        (is= (length included) (length (built-payload-senders built)))
        (is (not (store:db-block-open-p db)))
        (is-equalp root (store:db-root-hash db) "building left the state as it was")
        (is (null (blocks:block-body-mismatches b)))
        (let ((result (ex:execute-block db b :chain :mainnet :senders (built-payload-senders built))))
          (is (null (ex:block-mismatches result (blocks:block-header b))))
          (is= (headers:ethereum-header-gas-used (inner b)) (ex:block-result-gas-used result))
          (is-equalp (headers:ethereum-header-state-root (inner b)) (ex:block-result-state-root result))
          (store:db-revert-block db))))))

(deftest spent-fee-balance-shortcut-uses-real-state ()
  (let* ((reader (clempo.txpool.fixtures:make-fixture-reader))
         (db (pool:state-reader-db reader))
         (p (pool:make-pool))
         (tx (clempo.txpool.fixtures:fixture-transaction :type 2))
         (decreased (make-hash-table :test 'equal)))
    (pool:submit-transaction p tx reader)
    (destructuring-bind (token . slot) (pool:pool-transaction-fee-balance-slot p tx)
      (let ((cost (pool:pool-transaction-fee-cost p tx)))
        (store:db-begin-block db)
        (unwind-protect
             (progn
               (store:db-set-storage db token slot (1- cost))
               ;; A slot only enters the shortcut after a write lowered it in this block.
               (is (not (clempo.builder::fee-balance-insufficient-p db decreased p tx nil)))
               (setf (gethash (cons token slot) decreased) t)
               (is (clempo.builder::fee-balance-insufficient-p db decreased p tx nil))
               ;; A later credit is observed from current state, not the initial decrease.
               (store:db-set-storage db token slot cost)
               (is (not (clempo.builder::fee-balance-insufficient-p db decreased p tx nil))))
          (store:db-revert-block db))))))

(deftest spent-fallback-fee-balance-keeps-alternatives ()
  (let* ((reader (clempo.txpool.fixtures:make-fixture-reader))
         (db (pool:state-reader-db reader))
         (sender (clempo.txpool.fixtures:fixture-address 1))
         (path #x20c0000000000000000000000000000000000000)
         (ousd #x20c0000000000000000000006a37da5c996874be)
         (slot (clempo.txpool.fixtures::mapping-slot sender 9))
         (p (pool:make-pool))
         (tx (clempo.txpool.fixtures:fixture-transaction :fee-token nil))
         (decreased (make-hash-table :test 'equal)))
    (clempo.txpool.fixtures:fixture-change-state
     reader
     (lambda (db)
       (store:db-set-account db path 0 0 (store:db-put-code db (octets-of #xef)))
       (store:db-set-storage db path 7 (ash 1 160))
       (store:db-set-storage db path 4
                             (store:db-storage db clempo.txpool.fixtures:+fixture-token+ 4))
       (store:db-set-storage db path slot 1000000000000000000)
       (store:db-set-storage db clempo.txpool.fixtures::+fee-manager+
                             (clempo.txpool.fixtures::mapping-slot sender 1) 0)
       (store:db-set-storage db clempo.txpool.fixtures::+fee-manager+
                             (clempo.txpool.fixtures::mapping-slot 1 0) path)))
    (pool:submit-transaction p tx
                            (pool:make-state-reader db (pool:state-reader-env reader)
                                                    :recent-validator-tokens (list path)))
    (is (pool:pool-transaction-fallback-fee-token-p p tx))
    (let ((cost (pool:pool-transaction-fee-cost p tx)))
      (store:db-begin-block db)
      (unwind-protect
           (progn
             (store:db-set-storage db path slot 0)
             (setf (gethash (cons path slot) decreased) t)
             (is (clempo.builder::fee-balance-insufficient-p
                  db decreased p tx (clempo.builder::fallback-fee-tokens :t12)))
             ;; T13's untracked OUSD is an execution candidate, not assumed insolvent.
             (is (not (clempo.builder::fee-balance-insufficient-p
                       db decreased p tx (clempo.builder::fallback-fee-tokens :t13))))
             (setf (gethash (cons ousd slot) decreased) t)
             (is (clempo.builder::fee-balance-insufficient-p
                  db decreased p tx (clempo.builder::fallback-fee-tokens :t13)))
             (store:db-set-storage db ousd slot cost)
             (is (not (clempo.builder::fee-balance-insufficient-p
                       db decreased p tx (clempo.builder::fallback-fee-tokens :t13)))))
        (store:db-revert-block db)))))

;;; lib.rs maybe_override_fee_recipient: a proposer with a ValidatorConfigV2 record gets its
;;; fee recipient as the beneficiary even when that is the zero address (T14 executors reject
;;; any other). On the T15 devnet fixture (tests/tempo/executor/data/t15-devnet-*), after
;;; block 10 with the proposer's recorded recipient zeroed.

(deftest zero-fee-recipient-overrides-suggested-beneficiary ()
  (let* ((dir (asdf:system-relative-pathname "clempo-tempo-builder" "../../../tests/tempo/executor/data/"))
         (chain (clempo.chainspec:load-chainspec (merge-pathnames "t15-devnet-genesis.json" dir)))
         (ex:*chain* chain)
         (blocks (with-input-from-string (in (clempo.io:read-file-string
                                              (merge-pathnames "t15-devnet-blocks.txt" dir)))
                   (loop for line = (read-line in nil) while line
                         unless (string= line "") collect (blocks:decode-block (hex->octets line)))))
         (db (ex:genesis-db :chain chain)))
    (loop for block in (subseq blocks 0 10) for n from 1
          do (ex:execute-block db block :chain chain)
             (store:db-commit-block db n (blocks:block-hash block)))
    (let* ((next (blocks:block-header (nth 10 blocks)))
           (key (headers:consensus-context-proposer (headers:header-consensus-context next)))
           (parent (blocks:block-header (nth 9 blocks)))
           (attributes (make-payload-attributes
                        :timestamp (1+ (headers:header-timestamp parent))
                        :suggested-fee-recipient #x1234 :proposer-public-key key
                        :consensus-context (headers:header-consensus-context next)))
           (vc2 clempo.precompiles:+validator-config-v2-address+)
           (index (store:db-storage db vc2 (octets->integer
                                            (clempo.keccak:keccak256-concat key (integer->octets 3 32)))))
           (slot (+ (octets->integer (clempo.keccak:keccak256 (integer->octets 1 32)))
                    (* 6 (1- index)) 4)))
      (is (plusp index))
      (is (plusp (ldb (byte 160 0) (store:db-storage db vc2 slot))) "devnet recipients are set")
      (store:db-begin-block db)
      (store:db-set-storage db vc2 slot (logandc2 (store:db-storage db vc2 slot) (1- (ash 1 160))))
      (store:db-commit-block db 10 (blocks:block-hash (nth 9 blocks)))
      (is= 0 (ex:block-env-beneficiary (next-block-env db parent attributes :chain chain))))))
