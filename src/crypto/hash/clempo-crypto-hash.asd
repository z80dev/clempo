;;; Owner: the hash agent. Hashes, MACs, KDFs, ciphers and the 25519 curves.
(defsystem "clempo-crypto-hash"
  :description "sha2, ripemd160, blake2f, blake3, hmac, hkdf, aes, chacha20-poly1305, x25519, ed25519"
  :depends-on ("clempo-base")
  :serial t
  :components ())
