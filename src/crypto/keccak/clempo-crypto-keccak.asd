(defsystem "clempo-crypto-keccak"
  :description "Keccak-256"
  :depends-on ("clempo-base")
  :components ((:file "keccak")))

(defsystem "clempo-crypto-keccak/tests"
  :depends-on ("clempo-crypto-keccak")
  :pathname "../../../tests/crypto/keccak/"
  :components ((:file "keccak-tests")))
