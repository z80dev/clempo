;;; Chain specs, genesis and fork schedules.
(defsystem "clempo-tempo-chainspec"
  :description "Chain specs, genesis and fork schedules."
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto-keccak" "clempo-codec"
               "clempo-trie" "clempo-tempo-forks")
  :serial t
  :components ((:file "chainspec")
               (:static-file "data/presto.json")
               (:static-file "data/moderato.json")))

(defsystem "clempo-tempo-chainspec/tests"
  :depends-on ("clempo-tempo-chainspec")
  :pathname "../../../tests/tempo/chainspec/"
  :components ((:file "chainspec-tests")))
