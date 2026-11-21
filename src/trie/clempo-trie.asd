;;; Merkle Patricia tries, roots and proofs.
(defsystem "clempo-trie"
  :description "Merkle Patricia tries, roots and proofs."
  :depends-on ("clempo-base" "clempo-crypto-keccak" "clempo-codec")
  :serial t
  :components ((:file "trie") (:file "witness")))

(defsystem "clempo-trie/tests"
  :depends-on ("clempo-trie" "clempo-io")
  :pathname "../../tests/trie/"
  :components ((:file "trie-tests")))
