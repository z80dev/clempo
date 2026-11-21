;;; Interpreter, gas and Ethereum precompiles.
(defsystem "clempo-evm"
  :description "Interpreter, gas and Ethereum precompiles."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-types" "clempo-state")
  :serial t
  :components ((:file "package")
               (:file "spec")
               (:file "words")
               (:file "frame")
               (:file "defop")
               (:file "ops")
               (:file "interpreter")
               (:file "call")
               (:file "precompiles")
               (:file "precompiles-curves")
               (:file "tx")
               (:file "tracing")
               (:file "compile")
               (:file "compile-cache")))

(defsystem "clempo-evm/tests"
  :depends-on ("clempo-evm")
  :pathname "../../tests/evm/"
  :components ((:file "evm-tests")
               (:file "tracing-tests")
               (:file "compile-tests")))
