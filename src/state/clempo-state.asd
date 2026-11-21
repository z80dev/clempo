;;; Owner: the evm agent. Journaled world state.
(defsystem "clempo-state"
  :description "Owner: the evm agent. Journaled world state."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-types" "clempo-trie")
  :serial t
  :components ())
