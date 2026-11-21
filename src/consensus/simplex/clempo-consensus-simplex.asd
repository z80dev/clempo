;;; Owner: the simplex agent. commonware simplex's voter (with its batcher's vote assembly
;;; and its resolver's repair requests) as a pure, deterministic state machine.
(defsystem "clempo-consensus-simplex"
  :description "Simplex voter and leader election (commonware-consensus 2026.10.1 as Tempo configures it)"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-consensus")
  :serial t
  :components ((:file "package")
               (:file "elector")
               (:file "round")
               (:file "voter")))

(defsystem "clempo-consensus-simplex/tests"
  :depends-on ("clempo-consensus-simplex" "clempo-consensus-dkg" "clempo-types")
  :pathname "../../../tests/consensus/simplex/"
  :serial t
  :components ((:file "elector-tests")
               (:file "voter-tests")))
