(defsystem "clempo-node"
  :description "Execute, archive and serve the live Tempo chain"
  :depends-on ("sb-bsd-sockets" "sb-posix" "clempo-sync" "clempo-rpc"
               "clempo-store" "clempo-tempo-executor")
  :serial t
  :components ((:file "repl") (:file "workers") (:file "node")))

(defsystem "clempo-node/tests"
  :depends-on ("clempo-node")
  :pathname "../../tests/node/"
  :components ((:file "repl-tests")))
