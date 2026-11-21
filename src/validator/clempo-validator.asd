(defsystem "clempo-validator"
  :description "Local Tempo validator process and authenticated peer supervision"
  :depends-on ("clempo-p2p" "clempo-io" "clempo-codec" "clempo-crypto-hash" "sb-bsd-sockets"
               "clempo-consensus" "clempo-types")
  :serial t
  :components ((:file "manifest") (:file "network") (:file "wire")))

(defsystem "clempo-validator/tests"
  :depends-on ("clempo-validator")
  :pathname "../../tests/validator/"
  :serial t
  :components ((:file "network") (:file "wire")))
