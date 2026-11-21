(defsystem "clempo-base"
  :description "clempo foundations: policy, octets, hex, u256, tests"
  :serial t
  :components ((:file "package")
               (:file "policy")
               (:file "octets")
               (:file "u256")
               (:file "test"))
  :in-order-to ((test-op (test-op "clempo-base/tests"))))

(defsystem "clempo-base/tests"
  :depends-on ("clempo-base")
  :pathname "../../tests/base/"
  :components ((:file "base-tests")))
