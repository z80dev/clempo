(defpackage #:clempo.executor
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:store #:clempo.store) (#:chainspec #:clempo.chainspec)
                    (#:forks #:clempo.forks) (#:tx #:clempo.transactions)
                    (#:receipts #:clempo.receipts) (#:headers #:clempo.headers)
                    (#:blocks #:clempo.blocks) (#:rlp #:clempo.rlp))
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
   #:invalid-transaction #:unsupported-transaction
   ;; replay
   #:replay-blocks #:retry-block #:heal-from-reference #:skip-block #:abort-replay
   #:replay-result #:make-replay-result #:replay-result-from #:replay-result-to
   #:replay-result-last #:replay-result-matched-through #:replay-result-heals
   #:replay-result-skips #:replay-result-blocks #:replay-result-txs #:replay-result-gas
   #:replay-result-stopped #:replay-result-stop-reason))
