;;; Owner: the precompiles agent. Tempo hardforks, fork-if, gas parameters.
(defsystem "clempo-tempo-forks"
  :description "Owner: the precompiles agent. Tempo hardforks, fork-if, gas parameters."
  :depends-on ("clempo-base")
  :serial t
  :components ((:file "forks")))

(defsystem "clempo-tempo-forks/tests"
  :depends-on ("clempo-tempo-forks")
  :pathname "../../../tests/tempo/forks/"
  :components ((:file "forks-tests")))
