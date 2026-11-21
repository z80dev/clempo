;;; The crypto layer: keccak here, the rest in hash/ and ec/.
(defsystem "clempo-crypto"
  :description "Hashes, curves and signatures, all in Lisp"
  :depends-on ("clempo-base" "clempo-crypto-hash" "clempo-crypto-ec")
  :components ((:file "keccak")))

(defsystem "clempo-crypto/tests"
  :depends-on ("clempo-crypto")
  :pathname "../../tests/crypto/"
  :components ((:file "keccak-tests")))
