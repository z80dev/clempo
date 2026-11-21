(defsystem "clempo-sync"
  :description "Upstream JSON-RPC, raw block archive and authenticated follower"
  :depends-on ("clempo-base" "clempo-io" "clempo-net" "clempo-codec" "clempo-types"
               "clempo-consensus" "clempo-tempo-chainspec")
  :serial t
  :components ((:file "jsonrpc")
               (:file "archive")
               (:file "follow")
               (:file "finalizations")))

(defsystem "clempo-sync/tests"
  :depends-on ("clempo-sync")
  :pathname "../../tests/sync/"
  :components ((:file "archive-tests") (:file "follow-tests")))
