(defpackage #:clempo.evm
  (:use #:cl #:clempo.base #:clempo.state)
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode)
  (:documentation "The EVM: opcodes declared with DEFOP and compiled into one dispatch loop,
fork-parameterised gas schedules (GAS-PARAMS), message calls and creation, Ethereum
precompiles behind a provider protocol, and transaction processing for Istanbul..Osaka.
Context lives in special variables: *WORLD* *SPEC* *BLOCK* *TX-ENV* *TRACER*.")
  (:export
   ;; forks and spec
   #:*forks* #:fork-index #:fork-name #:parse-fork #:fork>= #:fork-if #:current-fork
   #:gas-params #:make-gas-params #:copy-gas-params #:gas-params-for-fork #:*gas-param-names*
   #:spec #:make-spec #:spec-fork #:spec-gas #:spec-precompiles #:spec-extra-opcodes
   #:spec-sstore-hook #:spec-blob-target #:spec-blob-max #:spec-blob-update-fraction #:spec-ext
   #:block-env #:make-block-env #:block-env-number #:block-env-timestamp #:block-env-coinbase
   #:block-env-gas-limit #:block-env-base-fee #:block-env-prevrandao #:block-env-difficulty
   #:block-env-chain-id #:block-env-excess-blob-gas #:block-env-blob-base-fee
   #:block-env-block-hash-fn #:block-env-ext
   #:tx-env #:make-tx-env #:tx-env-origin #:tx-env-gas-price #:tx-env-blob-hashes
   #:*world* #:*spec* #:*block* #:*tx-env* #:*tracer*
   ;; tracing
   #:trace-tx-start #:trace-tx-end #:trace-call-start #:trace-call-end #:trace-step
   #:trace-step-end #:trace-log #:trace-selfdestruct #:trace-unstarted
   ;; opcodes
   #:defop #:op-info #:op-info-name #:op-info-opcode #:op-info-gas #:op-info-since #:ops-since
   ;; frames and execution
   #:frame #:frame-code #:frame-address #:frame-code-address #:frame-caller #:frame-value
   #:frame-input #:frame-gas #:frame-depth #:frame-static-p #:frame-memory #:frame-mem-size
   #:frame-return-data #:frame-account #:frame-kind
   #:run-frame #:call-message #:create-message #:create-address #:create2-address
   #:success-p #:collision-p #:with-evm-thread
   ;; precompiles
   #:precompile #:make-precompile #:precompile-name #:precompile-address #:precompile-fn
   #:find-precompile #:precompile-addresses #:ethereum-precompiles #:defprecompile
   ;; transactions
   #:evm-tx #:make-evm-tx #:evm-tx-type #:evm-tx-nonce #:evm-tx-gas-limit #:evm-tx-gas-price
   #:evm-tx-max-fee #:evm-tx-max-priority-fee #:evm-tx-to #:evm-tx-value #:evm-tx-data
   #:evm-tx-access-list #:evm-tx-blob-hashes #:evm-tx-max-fee-per-blob-gas
   #:evm-tx-authorizations #:evm-tx-sender
   #:authorization #:make-authorization #:authorization-hash #:authority-of
   #:tx-result #:tx-result-status #:tx-result-gas-used #:tx-result-logs #:tx-result-output
   #:tx-result-contract-address
   #:invalid-transaction #:invalid-transaction-reason #:reject-tx
   #:intrinsic-gas #:effective-gas-price #:validate-tx #:buy-gas #:warm-tx-access
   #:apply-authorizations #:execute-tx-message #:settle-tx
   #:ethereum-handler #:apply-transaction
   #:blob-base-fee #:fake-exponential #:+gas-per-blob+))
