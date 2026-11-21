;;; Owner: the rpc agent. JSON-RPC server, method table, renderers, tracers.
(defsystem "clempo-rpc"
  :description "JSON-RPC server (HTTP and WebSocket) serving the reference node's method set"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-trie" "clempo-types"
               "clempo-sync" "clempo-tempo-chainspec" "clempo-store" "clempo-tempo-executor"
               (:require "sb-bsd-sockets"))
  :serial t
  :components ((:file "serde")
               (:file "defrpc")
               (:file "params")
               (:file "chain")
               (:file "websocket")
               (:file "server")
               (:file "render")
               (:file "eth")
               (:file "logs")
               (:file "config")
               (:file "stubs")
               (:file "sender")
               (:file "subscribe")
               (:file "state")
               (:file "revert")
               (:file "calls")
               (:file "simulate")))

(defsystem "clempo-rpc/tests"
  :depends-on ("clempo-rpc")
  :pathname "../../tests/rpc/"
  :serial t
  :components ((:file "rpc-tests")
               (:file "ws-tests")
               (:file "state-tests")
               (:file "serde-tests")))
