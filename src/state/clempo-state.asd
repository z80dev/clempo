;;; Journaled world state.
(defsystem "clempo-state"
  :description "Journaled world state."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-types" "clempo-trie")
  :serial t
  :components ((:file "package")
               (:file "backend")
               (:file "world")
               (:file "root")))

(defsystem "clempo-state/tests"
  :depends-on ("clempo-state")
  :pathname "../../tests/state/"
  :components ((:file "state-tests")))
