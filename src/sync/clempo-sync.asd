(defsystem "clempo-sync"
  :description "Upstream JSON-RPC client, raw block archive and fetch"
  :depends-on ("clempo-base" "clempo-io" "clempo-codec")
  :serial t
  :components ((:file "jsonrpc")
               (:file "archive")))

(defsystem "clempo-sync/tests"
  :depends-on ("clempo-sync")
  :pathname "../../tests/sync/"
  :components ((:file "archive-tests")))
