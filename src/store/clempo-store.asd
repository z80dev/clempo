;;; The world state (in RAM, or backed by the on-disk KV store),
;;; its datadir with a write-ahead block log, and checkpoints.
;;; history*.lisp implements the historical state reader and writer.
(defsystem "clempo-store"
  :description "Committed world state (accounts, storage, code, tries) in RAM or over an on-disk store, with a block log"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-trie"
               (:require "sb-posix"))
  :serial t
  :components ((:file "statedb")
               (:file "binary")
               (:file "kv")
               (:file "backed")
               (:file "persist")
               (:file "history")
               (:file "history-write")))

(defsystem "clempo-store/tests"
  :depends-on ("clempo-store")
  :pathname "../../tests/store/"
  :components ((:file "store-tests")
               (:file "datadir-tests")
               (:file "history-tests")))
