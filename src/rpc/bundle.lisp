(defpackage #:clempo.rpc.bundle
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:executor #:clempo.executor) (#:state #:clempo.state) (#:tx #:clempo.transactions)
                    (#:calls #:clempo.rpc.calls) (#:chainspec #:clempo.chainspec) (#:h #:clempo.headers))
  (:documentation "Signed transactions simulated on a block's state: eth_callBundle (reth
EthBundle::call_bundle, rpc/src/eth/bundle.rs) and trace_rawTransaction (reth
TraceApi::trace_raw_transaction). Both decode and recover the raw transactions as
eth_sendRawTransaction does, then apply them on the executor's overlay block
\(clempo.executor begin-call-block / apply-transaction).")
  (:export #:effective-tip #:recover-raw))

(in-package #:clempo.rpc.bundle)

;;; Decoding

(defun recover-raw (data)
  "reth recover_raw_transaction: (values transaction sender) of the EIP-2718 octets DATA."
  (when (zerop (length data)) (rpc-fail +invalid-params+ "empty transaction data"))
  (let ((transaction (handler-case (tx:decode-transaction data)
                       (error () (rpc-fail +invalid-params+ "failed to decode signed transaction")))))
    (values transaction
            (handler-case (tx:transaction-sender transaction)
              (error () (rpc-fail +invalid-params+ "invalid transaction signature"))))))

(defun invalid-transaction-error (e transaction sender world env)
  "Fail with reth's RpcInvalidTransactionError for the executor's rejection E of TRANSACTION:
the protocol nonce errors quote the account's nonce (NonceTooLow's Display), the rest map as
calls' requests do."
  (case (executor:invalid-transaction-kind e)
    (:nonce-too-low
     (if (and (= (tx:transaction-type transaction) #x76) (plusp (tx:transaction-nonce-key transaction)))
         (calls::fail-invalid e nil nil env)
         (rpc-fail -32000 (format nil "nonce too low: next nonce ~D, tx nonce ~D"
                                  (state:account-nonce (state:get-account world sender))
                                  (tx:transaction-nonce transaction)))))
    (:nonce-too-high
     (if (and (= (tx:transaction-type transaction) #x76) (plusp (tx:transaction-nonce-key transaction)))
         (calls::fail-invalid e nil nil env)
         (rpc-fail -32000 "nonce too high")))
    (t (calls::fail-invalid e nil nil env))))

(defun apply-signed (ctx transaction sender env &optional tracer)
  "Apply the signed TRANSACTION in CTX (under TRACER) as reth's transact_commit does; a
rejection fails with reth's error."
  (let ((world (executor:call-block-world ctx)))
    (handler-case
        (if tracer
            (clempo.tracing:with-tracer (tracer world) (executor:apply-transaction ctx transaction sender))
            (executor:apply-transaction ctx transaction sender))
      (executor:invalid-transaction (e) (invalid-transaction-error e transaction sender world env))
      (executor:execution-failure (e) (internal-error (executor:execution-failure-reason e))))))

;;; reth's transact_commit runs one transaction in the EVM: the block executor's gas lanes
;;; (Tempo's shared and general gas limits) do not apply to it.

(defun without-lanes (env)
  (setf (executor::block-env-shared-gas-limit env) 0
        (executor::block-env-general-gas-limit env) +u64-max+)
  env)

;;; eth_callBundle

(calls:defvec raw-tx-list calls::hex-data)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter +call-bundle-fields+
  '((:txs "txs" raw-tx-list :required t)
    (:block-number "blockNumber" calls:q64 :required t)
    (:state-block-number "stateBlockNumber" clempo.rpc.logs::block-number-or-tag-positioned :required t)
    (:transaction-index "transactionIndex" calls::transaction-index :optional t)
    (:coinbase "coinbase" address :optional t)
    (:timestamp "timestamp" calls:q64 :optional t)
    (:timeout "timeout" calls:q64 :optional t)
    (:gas-limit "gasLimit" calls:q64 :optional t)
    (:difficulty "difficulty" calls::q256 :optional t)
    (:base-fee "baseFee" calls::q128 :optional t))
  "alloy EthCallBundle's fields in declaration order (a sequence gives them positionally)."))

(macrolet ((define-object ()
             `(calls:defobject call-bundle-object (:expected "struct EthCallBundle") ,@+call-bundle-fields+)))
  (define-object))

(defparam eth-call-bundle (src) "alloy EthCallBundle: an object, or serde's positional sequence
\(the fields with a default may be left off the end)"
  (if (eq (serde:peek-kind src) :array)
      (let ((out '()) (n 0))
        (serde:do-array (j src)
          (let ((field (nth j +call-bundle-fields+)))
            (unless field
              (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "trailing characters"))
            (destructuring-bind (kw json type &key optional required) field
              (declare (ignore json required))
              (unless (and optional (serde:read-null-p src))
                (setf (getf out kw) (read-param type src)))))
          (setf n (1+ j)))
        (when (< n 3)
          (serde:serde-fail-here src "invalid length ~D, expected struct EthCallBundle with 10 elements" n))
        out)
      (read-param 'call-bundle-object src)))

(defun effective-tip (transaction base-fee)
  "alloy effective_tip_per_gas: the fee per gas above BASE-FEE the producer receives, or NIL
when the fee cap is below the base fee."
  (if (member (tx:transaction-type transaction) '(0 1))
      (let ((price (tx:transaction-gas-price transaction)))
        (and (>= price base-fee) (- price base-fee)))
      (let ((max-fee (tx:transaction-max-fee-per-gas transaction)))
        (and (>= max-fee base-fee)
             (min (- max-fee base-fee) (tx:transaction-max-priority-fee-per-gas transaction))))))

(defun bundle-fail (format &rest args)
  (rpc-fail +invalid-params+ (apply #'format nil format args)))

(defun balance (world address) (state:get-balance world address))

(defun call-bundle (bundle)
  (destructuring-bind (&key txs block-number state-block-number coinbase timestamp gas-limit base-fee
                       &allow-other-keys)
      bundle
    (when (null txs) (bundle-fail "bundle missing txs"))
    (when (zerop block-number) (bundle-fail "bundle missing blockNumber"))
    (when (and gas-limit (> gas-limit calls:*rpc-gas-cap*)) (rpc-fail -32000 "intrinsic gas too high"))
    (let* ((recovered (mapcar (lambda (raw) (multiple-value-list (recover-raw raw))) txs))
           ;; the state block is the parent of the simulated one; pending simulates on the head
           (parent-number (or (block-id-number state-block-number) chain:*head*))
           (parent (if (<= parent-number chain:*head*)
                       (chain-block parent-number)
                       (header-not-found state-block-number)))
           (env (without-lanes (executor:header-block-env (cblock-header parent))))
           (limit (or gas-limit calls:*rpc-gas-cap*)))
      (when coinbase (setf (executor:block-env-beneficiary env) coinbase))
      (setf (executor:block-env-timestamp env) (or timestamp (+ (executor:block-env-timestamp env) 12)))
      (setf (executor:block-env-gas-limit env) limit)
      (setf (executor:block-env-base-fee env)
            (if base-fee
                (min base-fee +u64-max+)
                (let ((inner (cblock-inner parent)))
                  (chainspec:next-block-base-fee (executor:block-env-timestamp env) executor:*chain*
                                                 (or (h:ethereum-header-base-fee-per-gas inner) 0)
                                                 (h:ethereum-header-gas-used inner)))))
      (setf (executor:block-env-number env) block-number)
      (let* ((state (state-at parent-number))
             (ctx (executor:begin-call-block state env :block-hash-fn (calls::block-hash-fn)))
             (world (executor:call-block-world ctx))
             (beneficiary (executor:block-env-beneficiary env))
             (base (executor:block-env-base-fee env))
             (cap (calls::tx-gas-cap env))
             (initial (balance world beneficiary))
             (before initial) (after initial)
             (total-fees 0) (gas-used 0)
             (hashes '())
             (results '()))
        (loop for (transaction sender) in recovered
              do (let ((gas (tx:transaction-gas-limit transaction)))
                   (unless (<= (if cap (min gas cap) gas) (max 0 (- limit gas-used)))
                     (bundle-fail "bundle gas exceeds the block gas limit of ~D" limit)))
                 (let* ((hash (tx:transaction-hash transaction))
                        (price (or (effective-tip transaction base)
                                   (rpc-fail -32000 "max fee per gas less than block base fee")))
                        (outcome (apply-signed ctx transaction sender env))
                        (used (executor:tx-outcome-gas-used outcome))
                        (fees (* used price)))
                   (push hash hashes)
                   (incf gas-used used)
                   (incf total-fees fees)
                   (setf after (balance world beneficiary))
                   (let ((diff (max 0 (- after before))))
                     (setf before after)
                     (push (apply #'json:jobject
                                  "coinbaseDiff" (princ-to-string diff)
                                  "ethSentToCoinbase" (princ-to-string (max 0 (- diff fees)))
                                  "fromAddress" (addr sender)
                                  "gasFees" (princ-to-string fees)
                                  "gasPrice" (princ-to-string price)
                                  "gasUsed" used
                                  "toAddress" (let ((to (tx-to transaction))) (if to (addr to) :null))
                                  "txHash" (hex32 hash)
                                  (list (if (executor:tx-outcome-status outcome) "value" "revert")
                                        (hexbytes (executor:tx-outcome-output outcome))))
                           results))))
        (let ((diff (max 0 (- after initial))))
          (json:jobject "bundleHash" (hex32 (clempo.keccak:keccak256 (apply #'octets-concat (reverse hashes))))
                        "bundleGasPrice" (princ-to-string (if (zerop gas-used) 0 (floor diff gas-used)))
                        "coinbaseDiff" (princ-to-string diff)
                        "ethSentToCoinbase" (princ-to-string (max 0 (- diff total-fees)))
                        "gasFees" (princ-to-string total-fees)
                        "results" (coerce (nreverse results) 'simple-vector)
                        "stateBlockNumber" parent-number
                        "totalGasUsed" gas-used))))))

(defrpc "eth_callBundle" ((bundle eth-call-bundle))
  "simulate signed transactions on top of a block's state"
  (call-bundle bundle))

;;; trace_rawTransaction

(defrpc "trace_rawTransaction" ((data bytes) (types calls:trace-types) (block block-id :optional t))
  "parity traces of a signed transaction on the state after BLOCK, in that block's environment"
  (multiple-value-bind (transaction sender) (recover-raw data)
    (let* ((b (calls::call-block block))
           (env (without-lanes (executor:header-block-env (cblock-header b))))
           (ctx (executor:begin-call-block (state-at (list :hash (cblock-hash b))) env
                                           :block-hash-fn (calls::block-hash-fn)))
           (tracer (clempo.tracing:make-parity-tracer types))
           (outcome (apply-signed ctx transaction sender env tracer)))
      (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome)))))
