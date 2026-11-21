;;; The tempo layer: chain rules, precompiles, execution, pool and payload building.
(defsystem "clempo-tempo"
  :description "Tempo protocol: chain specs, hardforks, precompiles, executor, pool and builder"
  :depends-on ("clempo-tempo-chainspec" "clempo-tempo-forks" "clempo-tempo-contract"
               "clempo-tempo-precompiles" "clempo-tempo-executor"
               "clempo-tempo-txpool" "clempo-tempo-builder"))
