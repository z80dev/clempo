(defsystem "clempo-crypto"
  :description "Hashes, curves and signatures, all in Lisp"
  :depends-on ("clempo-base")
  :components ((:file "keccak")))

(defsystem "clempo-crypto/tests"
  :depends-on ("clempo-crypto")
  :pathname "../../tests/crypto/"
  :components ((:file "keccak-tests")))
