;;; Tempo payload building over the pool and executor.
(defsystem "clempo-tempo-builder"
  :description "Builds Tempo blocks from pooled transactions
on the block executor (tempo crates/payload/builder)."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-trie" "clempo-types"
               "clempo-store" "clempo-tempo-forks" "clempo-tempo-chainspec"
               "clempo-tempo-executor" "clempo-tempo-txpool")
  :serial t
  :components ((:file "package")
               (:file "builder")))

(defsystem "clempo-tempo-builder/tests"
  :depends-on ("clempo-tempo-builder" "clempo-tempo-txpool/fixtures" "clempo-io")
  :pathname "../../../tests/tempo/builder/"
  :serial t
  :components ((:file "builder-tests")
               (:static-file "data/mainnet-user-blocks-3211-3874.sexp")))
