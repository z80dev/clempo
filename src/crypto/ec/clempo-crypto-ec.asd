;;; Owner: the curves agent. Prime-field curves and pairings.
(defsystem "clempo-crypto-ec"
  :description "secp256k1, p256, p384, bn254, bls12-381, kzg, modexp"
  :depends-on ("clempo-base" "clempo-crypto-keccak" "clempo-crypto-hash")
  :serial t
  :components ((:file "modexp")
               (:file "prime-curve")
               (:file "secp256k1-field")
               (:file "secp256k1")
               (:file "secp256k1-glv")
               (:file "p256-field")
               (:file "p256")
               (:file "p384")
               (:file "bn254")
               (:file "bls12-381")
               (:file "bls12-381-hash")
               (:file "bls12-381-signatures")
               (:file "kzg")))

(defsystem "clempo-crypto-ec/tests"
  :depends-on ("clempo-crypto-ec" "clempo-codec")
  :serial t
  :pathname "../../../tests/crypto/ec/"
  :components ((:file "prime-curves")
               (:file "p256-field")
               (:file "p384/p384-tests")
               (:file "bn254/bn254-tests")
               (:file "bls12-381/precompile-tests")
               (:file "bls12-381/hash-tests")
               (:file "bls12-381/signature-tests")
               (:file "bls12-381/signature-min-sig-vectors")
               (:file "bls12-381/signature-min-pk-vectors")
               (:file "kzg/kzg-tests")))
