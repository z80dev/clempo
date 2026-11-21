(defsystem "clempo-p2p"
  :description "Tempo devp2p and authenticated Commonware transports"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec"
               "clempo-tempo-chainspec" "clempo-types" "sb-bsd-sockets")
  :serial t
  :components ((:file "wire") (:file "discovery") (:file "snappy") (:file "rlpx") (:file "eth")
               (:file "eth-source") (:file "cw-stream") (:file "cw-limit") (:file "cwp2p")))

(defsystem "clempo-p2p/tests"
  :depends-on ("clempo-p2p")
  :pathname "../../tests/p2p/"
  :serial t
  :components ((:file "devp2p") (:file "discovery") (:file "cw-stream") (:file "cw-limit")
               (:file "cwp2p")))
