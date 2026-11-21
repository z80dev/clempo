;;; Tempo hardforks, fork-if and the fork-site registry, gas parameters.
(defsystem "clempo-tempo-forks"
  :description "Tempo hardforks, fork-if and the fork-site registry, gas parameters."
  :depends-on ("clempo-base")
  :serial t
  :components ((:file "forks")))

(defsystem "clempo-tempo-forks/tests"
  :depends-on ("clempo-tempo-forks")
  :pathname "../../../tests/tempo/forks/"
  :components ((:file "forks-tests")))
