;;; The tempo layer: chainspec/, forks/, contract/, precompiles/, executor/.
(defsystem "clempo-tempo"
  :description "Tempo protocol: chain specs, hardforks, precompiles, executor"
  :depends-on ("clempo-tempo-chainspec" "clempo-tempo-forks" "clempo-tempo-contract"
               "clempo-tempo-precompiles" "clempo-tempo-executor"))
