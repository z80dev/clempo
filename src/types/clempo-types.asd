;;; Owner: the types agent. Headers, transactions, receipts, blocks.
(defsystem "clempo-types"
  :description "Declarative Tempo headers, transaction envelopes, signatures, receipts and blocks"
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-trie")
  :serial t
  :components ((:file "defrlp") (:file "headers") (:file "signatures")
               (:file "transactions") (:file "receipts") (:file "blocks")))

(defsystem "clempo-types/tests"
  :depends-on ("clempo-types" "clempo-io")
  :pathname "../../tests/types/"
  :serial t
  :components ((:file "signatures") (:file "transactions") (:file "blocks")))
