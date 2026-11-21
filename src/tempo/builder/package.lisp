(defpackage #:clempo.builder
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:ex #:clempo.executor) (#:pool #:clempo.txpool)
                    (#:store #:clempo.store) (#:chainspec #:clempo.chainspec)
                    (#:forks #:clempo.forks) (#:tx #:clempo.transactions)
                    (#:receipts #:clempo.receipts) (#:headers #:clempo.headers)
                    (#:blocks #:clempo.blocks) (#:rlp #:clempo.rlp))
  (:documentation "Tempo's payload builder (tempo crates/payload/builder/src/lib.rs): fills a
block on top of a parent from the transaction pool's best transactions, executing each on the
block executor, skipping what does not fit (block and general gas limits, the Osaka RLP size
limit, the transaction count) or fails validation (rolled back by the executor, the
transaction's nonce lane skipped), and assembles the header commitments from the execution
result. As in the reference head (tempo #7449 removed the subblock machinery), it adds no
system transaction and reserves no subblock (shared) gas on any fork.")
  (:export
   ;; attributes
   #:payload-attributes #:make-payload-attributes
   #:payload-attributes-timestamp #:payload-attributes-timestamp-millis-part
   #:payload-attributes-timestamp-millis
   #:payload-attributes-suggested-fee-recipient #:payload-attributes-proposer-public-key
   #:payload-attributes-consensus-context #:payload-attributes-extra-data
   #:payload-attributes-prev-randao #:payload-attributes-parent-beacon-block-root
   #:payload-attributes-target-gas-limit
   ;; the next block
   #:next-block-gas-limit #:next-block-env #:+max-block-transactions+ #:+max-rlp-block-size+
   #:+non-transaction-size-estimate+ #:block-transaction-length #:transaction-fit
   #:+empty-ommers-hash+ #:+empty-requests-hash+
   ;; building
   #:build-payload #:built-payload #:built-payload-block #:built-payload-senders
   #:built-payload-receipts #:built-payload-result #:built-payload-fees
   #:built-payload-stop-reason #:built-payload-skipped #:built-payload-env
   #:built-payload-estimated-size
   #:payload-build-error #:payload-build-error-reason))
