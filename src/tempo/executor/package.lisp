(defpackage #:clempo.executor
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:store #:clempo.store) (#:chainspec #:clempo.chainspec)
                    (#:forks #:clempo.forks) (#:tx #:clempo.transactions)
                    (#:receipts #:clempo.receipts) (#:headers #:clempo.headers)
                    (#:blocks #:clempo.blocks) (#:rlp #:clempo.rlp)
                    (#:state #:clempo.state) (#:evm #:clempo.evm)
                    (#:contract #:clempo.contract) (#:pc #:clempo.precompiles))
  (:documentation "Tempo's block executor (tempo crates/evm/src/block.rs and handler.rs) over
the committed state in clempo.store, and REPLAY-BLOCKS, which executes a range of blocks,
checks every block against its header and stops at a mismatch in a BLOCK-MISMATCH condition
with RETRY-BLOCK, HEAL-FROM-REFERENCE, SKIP-BLOCK and ABORT-REPLAY restarts.")
  (:export
   ;; context
   #:*chain*
   ;; genesis
   #:load-genesis #:genesis-db
   ;; blocks
   #:block-env #:make-block-env #:block-env-number #:block-env-timestamp #:block-env-spec
   #:block-env-beneficiary #:block-env-gas-limit #:block-env-base-fee #:block-env-chain-id
   #:header-block-env
   #:block-result #:block-result-number #:block-result-gas-used #:block-result-receipts
   #:block-result-receipts-root #:block-result-bloom #:block-result-state-root
   #:block-result-tx-count
   #:execute-block #:block-mismatches
   ;; conditions
   #:block-mismatch #:block-mismatch-number #:block-mismatch-tx-index #:block-mismatch-field
   #:block-mismatch-expected #:block-mismatch-actual #:block-mismatch-detail
   #:block-mismatch-fields #:block-mismatch-result #:heal-block #:apply-prestate-diff
   #:execution-failure #:execution-failure-tx-index #:execution-failure-reason
   #:invalid-transaction #:unsupported-transaction #:invalid-transaction-kind
   ;; incremental execution (builder, pool, validators)
   #:begin-block #:apply-transaction #:finish-block #:check-transaction #:block-context
   #:with-state-storage #:tx-outcome-storage-changes #:payment-transaction-p
   #:proposer-fee-recipient #:collect-storage-changes #:tip20-prefix-p #:block-env-proposer
   #:tx-outcome #:tx-outcome-status #:tx-outcome-gas-used #:tx-outcome-block-gas
   #:tx-outcome-logs #:tx-outcome-fee-token #:tx-outcome-fee-payer #:tx-outcome-key-expiry
   #:tx-outcome-validator-fee #:tx-outcome-fallback-fee-token-p #:tempo-evm-spec
   ;; overlay blocks (RPC requests and trace replays)
   #:begin-call-block #:call-block-world #:execute-request #:next-call-block #:finish-call-block
   #:tx-outcome-output #:tx-outcome-halt #:tx-outcome-refunded
   ;; call.lisp (HistoryAgent): calls on any state view
   #:block-prefix-context #:call-request #:*call-flags*
   #:estimate-gas #:estimate-failure #:estimate-failure-kind #:estimate-failure-gas-limit
   #:estimate-failure-outcome
   #:create-access-list #:make-access-list-tracer #:access-list-result
   ;; replay
   #:replay-blocks #:retry-block #:heal-from-reference #:skip-block #:abort-replay
   #:replay-result #:make-replay-result #:replay-result-from #:replay-result-to
   #:replay-result-last #:replay-result-matched-through #:replay-result-heals
   #:replay-result-skips #:replay-result-blocks #:replay-result-txs #:replay-result-gas
   #:replay-result-stopped #:replay-result-stop-reason))
