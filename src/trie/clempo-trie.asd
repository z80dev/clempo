;;; Owner: the trie agent. Merkle Patricia trie, roots, proofs.
(defsystem "clempo-trie"
  :description "Owner: the trie agent. Merkle Patricia trie, roots, proofs."
  :depends-on ("clempo-base" "clempo-crypto-keccak" "clempo-codec")
  :serial t
  :components ((:file "trie")))

(defsystem "clempo-trie/tests"
  :depends-on ("clempo-trie" "clempo-io")
  :pathname "../../tests/trie/"
  :components ((:file "trie-tests")))
