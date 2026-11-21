;;; Owner: the executor agent. The world state in RAM and its on-disk checkpoints and log.
(defsystem "clempo-store"
  :description "Committed world state (accounts, storage, code, tries) with checkpoints and a block log"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-trie"
               (:require "sb-posix"))
  :serial t
  :components ((:file "statedb")
               (:file "binary")
               (:file "persist")))

(defsystem "clempo-store/tests"
  :depends-on ("clempo-store")
  :pathname "../../tests/store/"
  :components ((:file "store-tests")))
