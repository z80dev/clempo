(defsystem "clempo-sync"
  :description "Upstream block fetch, linking and the block archive"
  :depends-on ("clempo-base" "clempo-io" "clempo-codec")
  :serial t
  :components ((:file "jsonrpc")))
