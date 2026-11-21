(defsystem "clempo-sync"
  :description "Upstream JSON-RPC, raw block archive, authenticated follower, chain-attached
transaction pool and the metrics registry"
  :depends-on ("sb-bsd-sockets" "clempo-base" "clempo-io" "clempo-codec" "clempo-types"
               "clempo-consensus" "clempo-tempo-chainspec" "clempo-tempo-executor"
               "clempo-tempo-txpool")
  :serial t
  :components ((:file "jsonrpc")
               (:file "archive")
               (:file "follow")
               (:file "finalizations")
               (:file "pool")
               (:file "metrics")))

(defsystem "clempo-sync/tests"
  :depends-on ("clempo-sync" "clempo-tempo-txpool/fixtures")
  :pathname "../../tests/sync/"
  :serial t
  :components ((:file "fake-upstream") (:file "archive-tests") (:file "follow-tests")
               (:file "certify-anchor-tests") (:file "pool-tests") (:file "metrics-tests")))
