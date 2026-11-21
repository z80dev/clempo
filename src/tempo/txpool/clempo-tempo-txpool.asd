(defsystem "clempo-tempo-txpool"
  :description "State-validated Tempo transaction admission, nonce lanes and tip ordering"
  :depends-on ("clempo-types" "clempo-io" "clempo-tempo-forks"
               "clempo-tempo-precompiles" "clempo-tempo-executor")
  :serial t
  :components ((:file "package") (:file "liquidity") (:file "validation") (:file "pool")))

(defsystem "clempo-tempo-txpool/fixtures"
  :description "Recorded pool prestate and deterministic signed fixture scenarios"
  :depends-on ("clempo-tempo-txpool")
  :serial t
  :components ((:file "fixtures-package")
               (:file "../../../tests/tempo/txpool/fixtures")))

(defsystem "clempo-tempo-txpool/tests"
  :depends-on ("clempo-tempo-txpool/fixtures")
  :serial t
  :components ((:file "../../../tests/tempo/txpool/pool")))
