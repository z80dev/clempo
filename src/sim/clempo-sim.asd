;;; Owner: the consensus agent. The seeded validator simulation (R6).
(defsystem "clempo-sim"
  :description "N simplex+DKG validators on a seeded simulated network"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec"
               "clempo-consensus" "clempo-consensus-dkg" "clempo-consensus-simplex")
  :serial t
  :components ((:file "network")
               (:file "sim")
               (:file "run")
               (:file "scenarios")))

(defsystem "clempo-sim/tests"
  :depends-on ("clempo-sim")
  :pathname "../../tests/sim/"
  :components ((:file "sim-tests")))
