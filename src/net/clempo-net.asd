(defsystem "clempo-net"
  :description "Native TLS 1.3 client, system trust anchors and the HTTP/1.1 client"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" (:require "sb-bsd-sockets"))
  :serial t
  :components ((:file "tls") (:file "trust") (:file "http")))

(defsystem "clempo-net/tests"
  :depends-on ("clempo-net")
  :pathname "../../tests/net/"
  :serial t
  :components ((:file "tls-tests") (:file "x509-chain-tests") (:file "http-tests")))
