(defsystem "clempo-crypto-keccak"
  :description "Keccak-256"
  :depends-on ("clempo-base" "clempo-io")       ; clempo.io:cpu-feature-p picks the instructions
  :serial t
  :components ((:file "keccak-arm64") (:file "keccak")))

(defsystem "clempo-crypto-keccak/tests"
  :depends-on ("clempo-crypto-keccak")
  :pathname "../../../tests/crypto/keccak/"
  :components ((:file "keccak-tests")))
