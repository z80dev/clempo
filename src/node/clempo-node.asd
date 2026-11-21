(defsystem "clempo-node"
  :description "Execute, archive and serve the live Tempo chain"
  :depends-on ("sb-bsd-sockets" "sb-posix" "clempo-sync" "clempo-rpc"
               "clempo-store" "clempo-tempo-executor" "clempo-p2p" "clempo-validator")
  :serial t
  :components ((:file "repl") (:file "workers") (:file "node")))

(defsystem "clempo-node/tests"
  :depends-on ("clempo-node" "clempo-sync/tests" "clempo-tempo-txpool/fixtures")
  :pathname "../../tests/node/"
  :serial t
  :components ((:file "repl-tests") (:file "node-tests")))
