(defsystem "clempo-sync"
  :description "Upstream JSON-RPC, raw block archive and authenticated follower"
  :depends-on ("clempo-base" "clempo-io" "clempo-codec" "clempo-types")
  :serial t
  :components ((:file "jsonrpc")
               (:file "archive")
               (:file "follow")))

(defsystem "clempo-sync/tests"
  :depends-on ("clempo-sync")
  :pathname "../../tests/sync/"
  :components ((:file "archive-tests") (:file "follow-tests")))
