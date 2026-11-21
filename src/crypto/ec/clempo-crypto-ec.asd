;;; Owner: the curves agent. Prime-field curves and pairings.
(defsystem "clempo-crypto-ec"
  :description "secp256k1, p256, bn254, bls12-381, kzg, modexp"
  :depends-on ("clempo-base" "clempo-crypto-keccak" "clempo-crypto-hash")
  :serial t
  :components ((:file "modexp")
               (:file "prime-curve")
               (:file "secp256k1")
               (:file "p256")))

(defsystem "clempo-crypto-ec/tests"
  :depends-on ("clempo-crypto-ec" "clempo-codec")
  :serial t
  :pathname "../../../tests/crypto/ec/"
  :components ((:file "prime-curves")))
