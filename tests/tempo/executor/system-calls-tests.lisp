(in-package #:clempo.executor.tests)

;;; The system paths of system-calls.lisp against mainnet (data/system-paths.sexp, recorded
;;; from the reference node) and the T13-genesis devnet (data/devnet-r6/).

(defun system-fixture (name) (merge-pathnames name *data*))

(defun system-block (n)
  (blocks:decode-block
   (hex->octets (string-trim '(#\Newline #\Space)
                             (clempo.io:read-file-string (system-fixture (format nil "block-~D.hex" n)))))))

(defun system-cases ()
  (let ((text (clempo.io:read-file-string (system-fixture "system-paths.sexp")))
        (*read-eval* nil)
        (*package* (find-package '#:clempo.executor.tests)))
    (loop with start = 0
          for (case end) = (multiple-value-list (read-from-string text nil nil :start start))
          while case collect case do (setf start end))))

(defun system-ctx (backend env)
  "A block context over the in-memory BACKEND (no state database) for ENV."
  (clempo.executor::make-block-context
   :env env
   :block-env-evm (clempo.executor::evm-block-env nil env)
   :world (clempo.state:make-world backend)
   :host (make-instance 'clempo.executor::world-host
                        :block-info (clempo.contract:make-block-info
                                     :number (clempo.executor::block-env-number env)
                                     :timestamp (block-env-timestamp env)
                                     :beneficiary (block-env-beneficiary env)
                                     :chain-id (block-env-chain-id env)))))

(defun run-system-paths (ctx)
  "The system paths of one block in the reference's order: the pre-transaction calls (with
the header's parent beacon block root from the block env), the zone installation, then the
end-of-block committee call. Returns the drained changes."
  (clempo.executor::apply-history-and-beacon-calls ctx)
  (clempo.executor::apply-zone-factory ctx)
  (clempo.executor::apply-committee-call ctx)
  (clempo.state:drain-changes (clempo.executor::ctx-world ctx)))

(defun runtime-with-hash (hash)
  (loop for (nil . runtimes) in clempo.executor::*zone-runtimes*
        do (loop for (nil code) in runtimes
                 when (octets= hash (clempo.keccak:keccak256 code))
                   do (return-from runtime-with-hash code))))

;;; The runtime bytecode, pinned: every hash is the reference node's (web3_sha3 of the bytes;
;;; for T10 and T13 the bytes are eth_getCode's, for T14 and T15 the crates/contracts
;;; constants of the node's commit, which mainnet has not activated).

(deftest zone-runtimes-pinned-by-code-hash ()
  (loop for (fork portal verifier messenger)
          in '((:t10 "109c27a83ea58d7b91efcc7bb415a04abef5a52d8fdc5a246c96de965fa47b32"
                     "f3ab166b6d8637bb2f15ac5037b4546611339fc88e4b03b00b89559f7f2822a2"
                     "1ea2e80521c58fd7966e1fd370f84ac297261c7197f097ffc879de1890e0a1a0")
               (:t13 "bb0c0e14d5c2e15ca4aa1aaeb8d5c7bfdcf35fdadac5380ff93d38f962e4c41e"
                     "cf7b19d3c186e4fd235c94907d10bba5c1ce21d6e815676b00aaf298b456de14"
                     "526c5bb045b6229e5a693d70feee066cc3848b5f91c62949ced54818045214ee")
               (:t14 "044c134910e8ecbf1132c0b50e3b124888f389918f59e6764af7baba697642e6"
                     "cf7b19d3c186e4fd235c94907d10bba5c1ce21d6e815676b00aaf298b456de14"
                     "526c5bb045b6229e5a693d70feee066cc3848b5f91c62949ced54818045214ee")
               (:t15 "c07055d023c14eb7c5f092caf2be26cf80d33aed7f757eae087d6d2f01a02f0e"
                     "cf7b19d3c186e4fd235c94907d10bba5c1ce21d6e815676b00aaf298b456de14"
                     "526c5bb045b6229e5a693d70feee066cc3848b5f91c62949ced54818045214ee"))
        do (loop for (address code hash) in (clempo.executor::zone-runtimes fork)
                 for expected in (list portal verifier messenger)
                 do (is-equalp (hex->octets expected) (clempo.keccak:keccak256 code)
                               "~(~A~) runtime at ~X" fork address)
                    (is-equalp (hex->octets expected) hash "~(~A~) runtime hash at ~X" fork address)))
  ;; The factory's slot 0 after the T10 activation (eth_getStorageAt 0x5af2.. 0 35816049).
  (is= #xAF571FD4B3AD43A5807A5E58BFB25EA1AB327A1400000001
       (clempo.executor::initial-zone-factory-config clempo.executor::+initial-factory-owner+)))

;;; Mainnet blocks: the minimal pre-state of the accounts the paths touch, then every account
;;; and slot the node reports afterwards, and nothing else changed.

(defun seed-backend (case)
  (let ((backend (clempo.state:make-memory-backend)))
    (dolist (a (getf case :pre))
      (destructuring-bind (address &key nonce code-hash code) a
        (let* ((hash (hex->octets code-hash))
               (code (if code (hex->octets code) (runtime-with-hash hash))))
          (is code "no runtime with the pre-state code hash ~A of ~X" code-hash address)
          (when code
            (is-equalp hash (clempo.state:code-hash code))
            (when (or (plusp nonce) (plusp (length code)))
              (clempo.state:memory-backend-put-account backend address :nonce nonce :code code))))))
    (loop for (address slot value) in (getf case :storage-before)
          unless (zerop value) do (clempo.state:memory-backend-put-storage backend address slot value))
    backend))

(defun expected-changed-addresses (case)
  (sort (remove-duplicates
         (append (loop for (address . pre) in (getf case :pre)
                       for post = (rest (assoc address (getf case :post)))
                       unless (and (equalp (getf pre :code-hash) (getf post :code-hash))
                                   (eql (getf pre :nonce) (getf post :nonce)))
                         collect address)
                 (loop for (address slot after) in (getf case :storage-after)
                       for before = (or (third (find-if (lambda (e) (and (eql (first e) address)
                                                                         (eql (second e) slot)))
                                                        (getf case :storage-before)))
                                        0)
                       unless (eql before after) collect address)))
        #'<))

(deftest system-paths-match-mainnet ()
  (let ((*chain* :mainnet))
    (dolist (case (system-cases))
      (let* ((name (getf case :name))
             (block (system-block (getf case :block)))
             (env (header-block-env (blocks:block-header block) :chain :mainnet))
             (backend (seed-backend case))
             (changes (run-system-paths (system-ctx backend env))))
        (clempo.state:apply-changes backend changes)
        (is-equalp (expected-changed-addresses case)
                   (sort (mapcar #'clempo.state:account-change-address changes) #'<)
                   "accounts changed by ~A" name)
        (dolist (p (getf case :post))
          (destructuring-bind (address &key nonce code-hash) p
            (multiple-value-bind (found n balance hash) (clempo.state:backend-account backend address)
              (declare (ignore found balance))
              (is= nonce n "~A: nonce of ~X" name address)
              (is-equalp (hex->octets code-hash) hash "~A: code hash of ~X" name address))))
        (loop for (address slot value) in (getf case :storage-after)
              do (is= value (clempo.state:backend-storage backend address slot)
                      "~A: slot ~X of ~X" name slot address))))))

(deftest t8-boundary-outcome-and-calldata ()
  ;; Mainnet 32443199 is the first T8 epoch boundary (epoch length 21600).
  (let* ((*chain* :mainnet)
         (env (header-block-env (blocks:block-header (system-block 32443199)) :chain :mainnet))
         (outcome (clempo.executor::decode-boundary-outcome (clempo.executor::block-env-extra-data env)))
         (players (clempo.executor::dkg-output-players (clempo.executor::dkg-outcome-output outcome)))
         (calldata (clempo.executor::set-committee-members-calldata
                    (clempo.executor::dkg-outcome-epoch outcome) players)))
    (is (eq :t8 (block-env-spec env)))
    (is (clempo.executor::epoch-boundary-p 32443199))
    (is (not (clempo.executor::epoch-boundary-p 32443198)))
    (is= 1502 (clempo.executor::dkg-outcome-epoch outcome))
    (is= 15 (length players))
    ;; keccak("setCommitteeMembers(uint64,bytes32[])")[0..4], the node's web3_sha3
    (is-equalp (octets-of #x22 #x9b #xdd #x9c) (subseq calldata 0 4))
    (is= (+ 4 (* 32 (+ 3 15))) (length calldata))
    ;; Trailing bytes are ignored, as OnchainDkgOutcome::read.
    (is= 1502 (clempo.executor::dkg-outcome-epoch
               (clempo.executor::decode-boundary-outcome
                (octets-concat (clempo.executor::block-env-extra-data env) (octets-of 1 2 3)))))))

(deftest committee-call-failures-invalidate-the-block ()
  (let* ((*chain* :mainnet)
         (block (system-block 32443199))
         (env (header-block-env (blocks:block-header block) :chain :mainnet))
         (backend (clempo.state:make-memory-backend)))
    (clempo.state:memory-backend-put-account backend clempo.precompiles:+current-committee-address+
                                             :code (octets-of #xef))
    (flet ((env-with (accessor value)
             (let ((copy (clempo.executor::copy-block-env env)))
               (funcall (fdefinition (list 'setf accessor)) value copy)
               copy)))
      ;; extra data that is not an outcome
      (signals invalid-transaction
               (clempo.executor::apply-committee-call
                (system-ctx backend (env-with 'clempo.executor::block-env-extra-data (octets-of 1 2 3)))))
      ;; no call before T8, nor away from a boundary
      (is (null (clempo.executor::apply-committee-call
                 (system-ctx backend (env-with 'clempo.executor::block-env-spec :t7)))))
      (is (null (clempo.executor::apply-committee-call
                 (system-ctx backend (env-with 'clempo.executor::block-env-number 32443198))))))
    ;; setCommitteeMembers from anyone but the zero address reverts with Unauthorized() and
    ;; leaves the committee as it was.
    (let ((ctx (system-ctx backend env)))
      (multiple-value-bind (ok output)
          (clempo.executor::run-system-call
           ctx clempo.executor::+system-address+ clempo.precompiles:+current-committee-address+
           (clempo.executor::set-committee-members-calldata 7 (list (make-octets 32)))
           clempo.executor::+system-call-gas-limit+)
        (is (not ok))
        (is-equalp (clempo.abi:abi-selector-octets "Unauthorized()") output))
      (is= '() (remove-if-not #'clempo.state:account-change-storage
                              (clempo.state:drain-changes (clempo.executor::ctx-world ctx)))))))

;;; evm2 system.rs: caller and origin, no fee or nonce accounting, missing code, reverts.

(deftest run-system-call-semantics ()
  (let* ((*chain* :mainnet)
         (env (header-block-env (blocks:block-header (system-block 32443199)) :chain :mainnet))
         (target #x4242424242424242424242424242424242424242)
         (caller #x1111111111111111111111111111111111111111)
         (backend (clempo.state:make-memory-backend)))
    ;; CALLER -> slot 0, ORIGIN -> slot 1
    (clempo.state:memory-backend-put-account backend target
                                             :code (octets-of #x33 #x60 0 #x55 #x32 #x60 1 #x55 0))
    (let ((ctx (system-ctx backend env)))
      (is (clempo.executor::run-system-call ctx caller target (make-octets 0) 30000000))
      (let ((changes (clempo.state:drain-changes (clempo.executor::ctx-world ctx))))
        (is= (list target) (mapcar #'clempo.state:account-change-address changes) "only the target changes")
        (is= (list (cons 0 caller) (cons 1 caller))
             (sort (copy-list (clempo.state:account-change-storage (first changes))) #'< :key #'car))
        (is= 0 (clempo.state:account-change-nonce (first changes)))))
    ;; no code: success, nothing changes (the caller is not created)
    (let ((ctx (system-ctx backend env)))
      (is (clempo.executor::run-system-call ctx caller #x99 (make-octets 0) 30000000))
      (is= '() (clempo.state:drain-changes (clempo.executor::ctx-world ctx))))
    ;; SSTORE then REVERT: failure, nothing changes
    (clempo.state:memory-backend-put-account
     backend target :code (octets-of #x60 1 #x60 0 #x55 #x60 0 #x60 0 #xfd))
    (let ((ctx (system-ctx backend env)))
      (is (not (clempo.executor::run-system-call ctx caller target (make-octets 0) 30000000)))
      (is= '() (remove-if-not #'clempo.state:account-change-storage
                              (clempo.state:drain-changes (clempo.executor::ctx-world ctx)))))))

(deftest zone-runtime-upgrade-follows-the-genesis-fork ()
  ;; A T13 block over a factory with T10 runtimes: mainnet (genesis T0) upgrades them, a chain
  ;; whose genesis is already T13 keeps its allocation (block.rs zone_runtime_fork).
  (flet ((run (chain)
           (let* ((*chain* chain)
                  (backend (clempo.state:make-memory-backend))
                  (env (header-block-env (blocks:block-header (system-block 47250842)) :chain :mainnet)))
             (clempo.state:memory-backend-put-account backend clempo.precompiles:+zone-factory-address+
                                                      :code (octets-of #xef))
             (loop for (address code) in (clempo.executor::zone-runtimes :t10)
                   do (clempo.state:memory-backend-put-account backend address :code code))
             (let ((ctx (system-ctx backend env)))
               (clempo.executor::apply-zone-factory ctx)
               (mapcar #'clempo.state:account-change-address
                       (clempo.state:drain-changes (clempo.executor::ctx-world ctx)))))))
    (is= (list clempo.precompiles:+zone-messenger-address+ clempo.precompiles:+zone-verifier-address+
               clempo.precompiles:+zone-portal-impl-address+)
         (sort (run :mainnet) #'<))
    (is= '() (run (clempo.chainspec:load-chainspec (system-fixture "devnet-r6/genesis.json"))))))

;;; The T13-genesis devnet (P2PAgent's devnet-r6): genesis allocates the factory, the T13
;;; runtimes and the EIP-2935 contract; every block carries the 2935 call and blocks 59 and
;;; 119 the committee call (epoch length 60). Replayed through BEGIN-BLOCK / FINISH-BLOCK,
;;; every state root matches.

(deftest devnet-t13-genesis-replays ()
  (let* ((chain (clempo.chainspec:load-chainspec (system-fixture "devnet-r6/genesis.json")))
         (*chain* chain)
         (text (clempo.io:read-file-string (system-fixture "devnet-r6/blocks-1-60.txt")))
         (blocks (coerce (loop for start = 0 then (1+ end)
                               for end = (or (position #\Newline text :start start) (length text))
                               for line = (string-trim '(#\Space #\Return) (subseq text start end))
                               unless (string= line "") collect (blocks:decode-block (hex->octets line))
                               while (< end (length text)))
                         'vector))
         (db (genesis-db :chain chain))
         (result (replay-blocks db 1 60 :chain chain :block-fn (lambda (n) (aref blocks (1- n))))))
    (is= 60 (replay-result-matched-through result))
    (is (null (replay-result-stopped result)))
    (is= 1 (store:db-storage db clempo.precompiles:+current-committee-address+ 0) "committee epoch")
    (is= 4 (store:db-storage db clempo.precompiles:+current-committee-address+ 1) "committee size")))

;;; The subblocks metadata check (block.lisp RLP-LIST-P) accepts exactly what decoding it
;;; accepts: random well-formed lists, their mutations and truncations, and nesting depths.

(deftest subblocks-rlp-check-agrees-with-decoding ()
  (let ((random (sb-ext:seed-random-state 1016))
        (cases 0))
    (labels ((item (depth)
               (if (and (plusp depth) (< (random 3 random) 2))
                   (loop repeat (random 5 random) collect (item (1- depth)))
                   (let ((o (make-octets (case (random 4 random) (0 0) (1 1) (2 (random 56 random)) (t (+ 56 (random 80 random)))))))
                     (dotimes (i (length o) o) (setf (aref o i) (random 256 random))))))
             (decodes-to-list-p (data end)
               (handler-case (listp (clempo.rlp:rlp-decode data :end end)) (error () nil)))
             (check (data)
               (incf cases)
               (is (eq (decodes-to-list-p data (length data))
                       (clempo.executor::rlp-list-p data (length data)))
                   "~A" (octets->hex data))))
      (dotimes (k 300)
        (let ((data (clempo.rlp:rlp-encode (loop repeat (1+ (random 4 random)) collect (item 3)))))
          (check data)
          (dotimes (j 10)
            (let ((d (copy-seq data)))
              (setf (aref d (random (length d) random)) (random 256 random))
              (check d)
              (check (subseq d 0 (random (1+ (length d)) random)))))))
      (dotimes (k 2000)
        (let ((d (make-octets (random 10 random))))
          (dotimes (i (length d)) (setf (aref d i) (random 256 random)))
          (check d)))
      (dolist (depth '(30 31 32 33))
        (let ((d (make-octets (1+ depth))))
          (dotimes (i depth) (setf (aref d i) (+ #xc0 (- depth i))))
          (setf (aref d depth) #xc0)
          (check d)))
      (is (< 8000 cases)))))

;;; A T15-genesis reference devnet (data/README.md, t15-devnet-*): blocks carry a proposer,
;;; so from T14 the beneficiary must be the proposer's ValidatorConfigV2 fee recipient, and
;;; from T15 the boundary block 59's DKG outcome must be the one V2 in its parent state implies.

(deftest t15-devnet-proposer-and-boundary-checks ()
  (let* ((chain (clempo.chainspec:load-chainspec (system-fixture "t15-devnet-genesis.json")))
         (*chain* chain)
         (text (clempo.io:read-file-string (system-fixture "t15-devnet-blocks.txt")))
         (blocks (coerce (loop for start = 0 then (1+ end)
                               for end = (or (position #\Newline text :start start) (length text))
                               for line = (string-trim '(#\Space #\Return) (subseq text start end))
                               unless (string= line "") collect (blocks:decode-block (hex->octets line))
                               while (< end (length text)))
                         'vector))
         (db (genesis-db :chain chain))
         (result (replay-blocks db 1 58 :chain chain :block-fn (lambda (n) (aref blocks (1- n)))))
         (boundary (aref blocks 58)))
    (is= 58 (replay-result-matched-through result))
    (flet ((begin (mutate)
             (let ((env (clempo.executor::header-block-env (blocks:block-header boundary) :chain chain)))
               (funcall mutate env)
               (unwind-protect (progn (clempo.executor:begin-block db env :block boundary) :ok)
                 (store:db-revert-block db)))))
      (is (eq :ok (begin #'identity)))
      (is (clempo.executor::block-env-proposer
           (clempo.executor::header-block-env (blocks:block-header boundary) :chain chain)))
      ;; a beneficiary other than the proposer's fee recipient
      (signals invalid-transaction
               (begin (lambda (env) (incf (clempo.executor::block-env-beneficiary env)))))
      ;; a proposer V2 does not know
      (signals invalid-transaction
               (begin (lambda (env) (setf (clempo.executor::block-env-proposer env) (integer->octets 7 32)))))
      ;; trailing bytes after the outcome (the committee call alone would accept them)
      (signals invalid-transaction
               (begin (lambda (env) (setf (clempo.executor::block-env-extra-data env)
                                          (octets-concat (clempo.executor::block-env-extra-data env) (octets-of 0))))))
      ;; the full-DKG flag (the outcome's last byte) flipped
      (signals invalid-transaction
               (begin (lambda (env)
                        (let ((extra (copy-seq (clempo.executor::block-env-extra-data env))))
                          (setf (aref extra (1- (length extra))) (- 1 (aref extra (1- (length extra)))))
                          (setf (clempo.executor::block-env-extra-data env) extra))))))
    (is (null (replay-result-stopped (replay-blocks db 59 60 :chain chain
                                                    :block-fn (lambda (n) (aref blocks (1- n)))))))))
