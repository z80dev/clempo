;;; The crypto layer. Subsystems: keccak/ (lead), hash/ (hash agent), ec/ (curves agent), pki/ (TLS agent).
(defsystem "clempo-crypto"
  :description "Hashes, curves and signatures, all in Lisp"
  :depends-on ("clempo-crypto-keccak" "clempo-crypto-hash" "clempo-crypto-ec" "clempo-crypto-pki"))
