;;; The consensus layer (owner: the consensus agent). Subsystems live in their own systems
;;; under src/consensus/*/ (dkg/: clempo-consensus-dkg). This placeholder makes the layer
;;; present for scripts/build.lisp until the consensus agent adds its own components.
(defsystem "clempo-consensus"
  :description "Finalization certificates, Simplex, DKG"
  :depends-on ("clempo-consensus-dkg")
  :components ())
