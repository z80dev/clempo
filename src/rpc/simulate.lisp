(defpackage #:clempo.rpc.simulate
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state #:clempo.rpc.calls)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:h #:clempo.headers)
                    (#:executor #:clempo.executor) (#:chainspec #:clempo.chainspec)
                    (#:blocks #:clempo.blocks) (#:tx #:clempo.transactions)
                    (#:r #:clempo.receipts) (#:rlp #:clempo.rlp) (#:state #:clempo.state))
  (:documentation "eth_simulateV1 and tempo_simulateV1: requests executed in simulated blocks on
top of a block's state (reth's simulate.rs over the executor's overlay call blocks), rendered
as reth renders a SimulatedBlock.

Each simulated block is assembled as Tempo's block builder assembles it (EthBlockAssembler
plus the Tempo header fields copied from the parent by TempoNextBlockEnvAttributes): the
requests become transactions with mock signatures (alloy's zero signature, Tempo's (1, 1)
for legacy), their receipts give the receipts root and bloom, and the state root stays zero
(reth does not compute it for eth_simulateV1). The block is then rendered by
clempo.rpc.render from an in-memory CBLOCK."))

(in-package #:clempo.rpc.simulate)

(defparameter *max-simulate-blocks* 256 "reth max_simulate_blocks default")
(defparameter *timestamp-increment* 1
  "Seconds between defaulted simulated timestamps: alloy-chains' sub-second block time hint
for Tempo, rounded up (reth sanitize_chain).")
(define-constant +empty-ommers-hash+ (clempo.keccak:keccak256 (rlp:rlp-encode '())))
(define-constant +empty-requests-hash+
  (hex->octets "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

(defun sim-fail (code fmt &rest args) (rpc-fail code (apply #'format nil fmt args)))

;;; sanitize_chain: explicit numbers and times, gaps filled with empty blocks

(defun sanitize-chain (blocks parent-number parent-time)
  (let ((out '()) (prev-number parent-number) (prev-time parent-time))
    (dolist (blk blocks (nreverse out))
      (let* ((overrides (copy-list (getf blk :block-overrides)))
             (target (or (getf overrides :number) (1+ prev-number))))
        (when (<= target prev-number)
          (sim-fail -38020 "block numbers must be in order: ~D <= ~D" target prev-number))
        (when (> (- target parent-number) *max-simulate-blocks*) (rpc-fail -38026 "too many blocks"))
        (loop for n from (1+ prev-number) below target
              do (incf prev-time *timestamp-increment*)
                 (push (list :block-overrides (list :number n :time prev-time) :calls '()) out))
        (setf prev-number target)
        (let ((time (getf overrides :time)))
          (cond ((null time) (setf time (+ prev-time *timestamp-increment*)))
                ((<= time prev-time) (sim-fail -38021 "block timestamps must be in order: ~D <= ~D" time prev-time)))
          (setf (getf overrides :number) target (getf overrides :time) time prev-time time))
        (let ((b (copy-list blk)))
          (setf (getf b :block-overrides) overrides)
          (push b out))))))

;;; The simulated block's environment (next_evm_env over build_pending_env, then overrides)

(defun next-env (parent overrides validation)
  "(values block-env header-template-plist) of the block after header PARENT."
  (let* ((i (h:header-inner parent))
         (chain executor:*chain*)
         (time (getf overrides :time))
         (number (getf overrides :number))
         (base-fee (cond ((getf overrides :base-fee) (min (getf overrides :base-fee) +u64-max+))
                         ((not validation) 0)
                         (t (chainspec:next-block-base-fee time chain (or (h:ethereum-header-base-fee-per-gas i) 0)
                                                           (h:ethereum-header-gas-used i)))))
         (beacon (and (h:ethereum-header-parent-beacon-block-root i)
                      (or (getf overrides :beacon-root) (make-octets 32))))
         (random (or (getf overrides :random) (make-octets 32))))
    (when (or (getf overrides :withdrawals) (getf overrides :blob-base-fee) (getf overrides :difficulty))
      (internal-error "eth_simulateV1: withdrawals, blob base fee and difficulty block overrides are not supported yet"))
    (values
     (executor:make-block-env :number number :timestamp time
                              :timestamp-millis-part (h:header-timestamp-millis-part parent)
                              :beneficiary (or (getf overrides :coinbase) (h:ethereum-header-beneficiary i))
                              :gas-limit (or (getf overrides :gas-limit) (h:ethereum-header-gas-limit i))
                              :base-fee base-fee :prevrandao random
                              :parent-hash (h:header-hash parent)
                              :extra-data (h:ethereum-header-extra-data i)
                              :chain-id (chainspec:chain-id (chainspec:load-chainspec chain))
                              :spec (chainspec:fork-at time chain)
                              :general-gas-limit (h:header-general-gas-limit parent)
                              :shared-gas-limit (h:header-shared-gas-limit parent)
                              :parent-beacon-block-root (or beacon (make-octets 32)))
     (list :beacon beacon :random random
           :withdrawals-p (and (h:ethereum-header-withdrawals-root i) t)
           :blob-p (and (h:ethereum-header-excess-blob-gas i) t)
           :requests-p (and (h:ethereum-header-requests-hash i) t)))))

;;; Simulated transactions (TryIntoSimTx with Tempo's legacy signature)

(defun octets-or-empty (address) (if address (integer->octets address 20) (make-octets 0)))

(defun access-list-items (plist)
  (mapcar (lambda (entry) (list (integer->octets (first entry) 20)
                                (mapcar (lambda (k) (integer->octets k 32)) (rest entry))))
          plist))

(defun sim-transaction (p chain-id)
  "The signed envelope of the executor request plist P."
  (destructuring-bind (&key type nonce gas-price max-fee max-priority gas to value input access-list
                       &allow-other-keys)
      p
    (let ((to (octets-or-empty to)) (al (access-list-items access-list)))
      (tx:decode-transaction
       (ecase type
         (0 (rlp:rlp-encode (list nonce gas-price gas to value input (+ 35 (* 2 chain-id)) 1 1)))
         (1 (octets-concat (octets-of 1)
                           (rlp:rlp-encode (list chain-id nonce gas-price gas to value input al 0 0 0))))
         (2 (octets-concat (octets-of 2)
                           (rlp:rlp-encode (list chain-id nonce max-priority max-fee gas to value input al 0 0 0)))))))))

;;; Executing one block's calls (execute_transactions)

(defstruct sim-call request tx outcome gas sender)

(defun invalid-call (e request env)
  (multiple-value-bind (code message data)
      (invalid-request-error (executor:execution-failure-reason e) :simulate t :request request :env env)
    (rpc-fail code message data)))

(defun run-calls (ctx env shared calls validation remaining-cap)
  "Execute CALLS in CTX as Tempo's block executor does (its non-shared and non-payment gas
lanes included); returns (values sim-calls remaining-cap)."
  (let* ((world (executor:call-block-world ctx))
         (gas-limit (executor:block-env-gas-limit env))
         (chain-id (executor:block-env-chain-id env))
         (non-shared-left (- gas-limit shared))
         ;; reth's block executor recomputes the general lane from the chainspec
         (non-payment-left (chainspec:general-gas-limit-at (executor:block-env-timestamp env)
                                                           executor:*chain* gas-limit shared))
         (cumulative 0) (out '()))
    (dolist (r calls)
      (let* ((block-left (max 0 (- gas-limit cumulative)))
             (gas (let ((g (req-gas r)))
                    (cond ((and g (> g block-left))
                           (rpc-fail -38015 "Block gas limit exceeded by the block's transactions"))
                          (g (min g remaining-cap))
                          (t (min block-left remaining-cap)))))
             (from (or (req-from r) 0))
             (state-nonce (state:get-nonce world from))
             (nonce (or (req-nonce r) state-nonce)))
        (request-input r)
        (when (and (= state-nonce +u64-max+) (or (not validation) (eql nonce +u64-max+)))
          (rpc-fail +internal-error+ "nonce has max value"))
        (when (and (not validation) (= nonce +u64-max+)) (setf nonce 0))
        (multiple-value-bind (p tx)
            (if (aa-request-p r)
                (aa-sim-transaction r gas nonce (or (req-chain-id r) chain-id))
                (let* ((p (request-plist r world gas 0))
                       (p (list* :nonce nonce :chain-id (or (req-chain-id r) chain-id) p))
                       (tx (sim-transaction p chain-id)))
                  (values (list* :hash (octets->integer (tx:transaction-hash tx))
                                 :unique-id (octets->integer (tx:transaction-unique-identifier tx from))
                                 p)
                          tx)))
        (let* ((outcome (handler-case
                            (executor:execute-request ctx p :skip-nonce-check (not validation)
                                                            :skip-base-fee-check (not validation)
                                                            :skip-gas-caps t
                                                            :skip-eip3607 t)
                          (executor:invalid-transaction (e) (invalid-call e r env))))
               (used (executor:tx-outcome-gas-used outcome))
               (lane-gas (executor:tx-outcome-block-gas outcome))
               (payment-p (executor:payment-transaction-p tx (executor:block-env-spec env))))
          (when (or (> lane-gas non-shared-left) (and (not payment-p) (> lane-gas non-payment-left)))
            (internal-error "transaction exceeds its gas lane"))
          (decf non-shared-left lane-gas)
          (unless payment-p (decf non-payment-left lane-gas))
          (when (> used remaining-cap) (rpc-fail -38026 "Client adjustable limit reached"))
          (decf remaining-cap used)
          (incf cumulative used)
          (push (make-sim-call :request r :tx tx :outcome outcome :gas gas :sender from) out)))))
    (values (nreverse out) remaining-cap)))

;;; Assembling and rendering a simulated block

(defun assemble (env template parent sims)
  "The CBLOCK of the simulated block."
  (let* ((receipts (loop with cumulative = 0
                         for s in sims
                         for o = (sim-call-outcome s)
                         do (incf cumulative (executor:tx-outcome-gas-used o))
                         collect (r:make-receipt :type (tx:transaction-type (sim-call-tx s))
                                                 :status (if (executor:tx-outcome-status o) 1 0)
                                                 :cumulative-gas-used cumulative
                                                 :logs (executor:tx-outcome-logs o))))
         (txs (mapcar #'sim-call-tx sims))
         (inner (h:make-ethereum-header
                 :parent-hash (h:header-hash parent) :ommers-hash +empty-ommers-hash+
                 :beneficiary (executor:block-env-beneficiary env) :state-root (make-octets 32)
                 :transactions-root (blocks:transactions-root txs)
                 :receipts-root (r:receipts-root receipts)
                 :logs-bloom (r:receipts-bloom receipts)
                 :difficulty 0 :number (executor:block-env-number env)
                 :gas-limit (executor:block-env-gas-limit env)
                 :gas-used (if receipts (r:receipt-cumulative-gas-used (car (last receipts))) 0)
                 :timestamp (executor:block-env-timestamp env)
                 :extra-data (h:ethereum-header-extra-data (h:header-inner parent))
                 :mix-hash (getf template :random) :nonce (make-octets 8)
                 :base-fee-per-gas (executor:block-env-base-fee env)
                 :withdrawals-root (and (getf template :withdrawals-p) clempo.trie:+empty-trie-root+)
                 :blob-gas-used (and (getf template :blob-p) 0)
                 :excess-blob-gas (and (getf template :blob-p) 0)
                 :parent-beacon-block-root (getf template :beacon)
                 :requests-hash (and (getf template :requests-p) +empty-requests-hash+)))
         (header (h:make-header :general-gas-limit (h:header-general-gas-limit parent)
                                :shared-gas-limit (h:header-shared-gas-limit parent)
                                :timestamp-millis-part (h:header-timestamp-millis-part parent)
                                :inner inner))
         (raw (blocks:encode-block (blocks:make-block :header header :transactions txs
                                                      :withdrawals-present-p (getf template :withdrawals-p))))
         (decoded (blocks:decode-block raw))
         (txs (coerce (blocks:block-transactions decoded) 'simple-vector)))
    (make-cblock :number (executor:block-env-number env) :raw raw :hash (h:header-hash header)
                 :header header :txs txs :tx-hashes (map 'simple-vector #'tx:transaction-hash txs)
                 :senders (map 'simple-vector (lambda (s) (list (sim-call-sender s) nil)) sims)
                 :size (length raw))))

(defun call-json (b index log-start s)
  "SimCallResult of call INDEX; returns (values json next-log-index)."
  (let* ((o (sim-call-outcome s))
         (used (executor:tx-outcome-gas-used o))
         (max-used (+ used (executor:tx-outcome-refunded o))))
    (if (executor:tx-outcome-status o)
        (let ((logs (loop for log in (executor:tx-outcome-logs o)
                          for k from log-start
                          collect (log-json b index k index log))))
          (values (json:jobject "returnData" (hexbytes (executor:tx-outcome-output o))
                                "logs" (coerce logs 'simple-vector)
                                "gasUsed" (q used) "maxUsedGas" (q max-used) "status" "0x1")
                  (+ log-start (length logs))))
        (let ((e (handler-case (progn (call-result o (sim-call-gas s) "eth_simulateV1") nil)
                   (rpc-error (e) e))))
          (values (json:jobject "returnData" "0x" "logs" #()
                                "gasUsed" (q used) "maxUsedGas" (q max-used) "status" "0x0"
                                "error" (apply #'json:jobject
                                               "code" (if (eql (rpc-error-code e) 3) 3 -32015)
                                               "message" (error-text e)
                                               (when (rpc-error-data e) (list "data" (rpc-error-data e)))))
                  log-start)))))

(defun simulated-block-json (b sims full)
  (let ((log-index 0))
    (append (block-json b full)
            (list (cons "calls"
                        (coerce (loop for s in sims for i from 0
                                      collect (multiple-value-bind (j next) (call-json b i log-index s)
                                                (setf log-index next) j))
                                'simple-vector))))))

;;; simulate_v1

(defun simulated-block-hash-fn (table)
  (let ((archive (block-hash-fn)))
    (lambda (n) (or (gethash n table) (funcall archive n)))))

(defun simulate-blocks (payload b state)
  "reth simulate_v1 from the base block B on STATE: (values json-blocks sims-per-block)."
  (let* ((validation (getf payload :validation))
         (blocks (sanitize-chain (getf payload :block-state-calls) (cblock-number b) (cblock-timestamp b)))
         (hashes (make-hash-table))
         (hash-fn (simulated-block-hash-fn hashes))
         (parent (cblock-header b))
         (remaining-cap *rpc-gas-cap*)
         (ctx nil) (out '()) (all-sims '()))
    (dolist (blk blocks (values (nreverse out) (nreverse all-sims)))
      (let ((overrides (getf blk :block-overrides)))
        (let ((g (getf overrides :gas-limit)))
          (when (and g (> g (h:ethereum-header-gas-limit (h:header-inner parent))) (> g *rpc-gas-cap*))
            (rpc-fail -38026 "Client adjustable limit reached")))
        (loop for (n . hash) in (getf overrides :block-hash)
              do (setf (gethash n hashes) hash))
        (multiple-value-bind (env template) (next-env parent overrides validation)
          (check-state-overrides (getf blk :state-overrides))
          (setf ctx (if ctx
                        (executor:next-call-block ctx env :block-hash-fn hash-fn)
                        (executor:begin-call-block state env :block-hash-fn hash-fn)))
          (apply-state-overrides (executor:call-block-world ctx) (getf blk :state-overrides))
          (multiple-value-bind (sims cap) (run-calls ctx env (h:header-shared-gas-limit parent) (getf blk :calls)
                                                     validation remaining-cap)
            (setf remaining-cap cap)
            (let ((cb (assemble env template parent sims)))
              (setf (gethash (cblock-number cb) hashes) (cblock-hash cb)
                    parent (cblock-header cb))
              (push (simulated-block-json cb sims (getf payload :return-full-transactions)) out)
              (push sims all-sims))))))))

(defun check-payload (payload)
  (when (> (length (getf payload :block-state-calls)) *max-simulate-blocks*)
    (rpc-fail -38026 "too many blocks"))
  (when (null (getf payload :block-state-calls)) (rpc-fail +invalid-params+ "calls are empty.")))

(defun base-block (block not-found)
  (let* ((id (or block :latest))
         (n (block-id-number id)))
    (or (and n (<= n chain:*head*) (chain-block n))
        (funcall not-found id))))

(defrpc "eth_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "execute calls in simulated blocks on top of BLOCK"
  (check-payload payload)
  (let ((b (base-block block (lambda (id) (sim-fail -32000 "block not found: ~A" (block-id-text id))))))
    (coerce (simulate-blocks payload b (state-at (list :hash (cblock-hash b)))) 'simple-vector)))

;;; tempo_simulateV1: TIP-20 metadata of the tokens called, paying fees or emitting Transfer

(define-constant +transfer-topic+
  (hex->octets "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"))

(defun involved-tokens (payload all-sims)
  (let ((tokens '()))
    (flet ((add (a) (when (and a (integerp a) (executor:tip20-prefix-p a)) (pushnew a tokens))))
      (dolist (blk (getf payload :block-state-calls))
        (dolist (r (getf blk :calls))
          (add (req-to r))
          (dolist (c (req-calls r)) (add (first c)))
          (add (req-fee-token r))))
      (dolist (sims all-sims)
        (dolist (s sims)
          (dolist (log (executor:tx-outcome-logs (sim-call-outcome s)))
            (when (and (r:event-log-topics log) (octets= (first (r:event-log-topics log)) +transfer-topic+))
              (add (r:event-log-address log)))))))
    (sort tokens #'<)))

(defun token-metadata (tokens state b)
  (executor:with-state-storage (state (executor:header-block-env (cblock-header b)))
    (loop for token in tokens
          for code-hash = (nth-value 3 (state:backend-account state token))
          when (and code-hash (not (octets= code-hash clempo.trie:+empty-code-hash+)))
            collect (cons (addr token)
                          (json:jobject "name" (clempo.tip20:name token)
                                        "symbol" (clempo.tip20:symbol token)
                                        "currency" (clempo.tip20:currency token))))))

(defrpc "tempo_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "eth_simulateV1 with the metadata of the TIP-20 tokens involved"
  (let ((b (base-block block #'header-not-found)))
    (check-payload payload)
    (let ((state (state-at (list :hash (cblock-hash b)))))
      (multiple-value-bind (blocks all-sims) (simulate-blocks payload b state)
        (json:jobject "blocks" (coerce blocks 'simple-vector)
                      "tokenMetadata" (cons :object (token-metadata (involved-tokens payload all-sims) state b)))))))
