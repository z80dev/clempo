;;; Owner: the hash agent. Hashes, MACs, KDFs, ciphers and the 25519 curves.
(defsystem "clempo-crypto-hash"
  :description "sha2, ripemd160, blake2f, blake3, hmac, hkdf, aes, chacha20-poly1305, x25519, ed25519"
  :depends-on ("clempo-base")
  :serial t
  :components ((:file "sha256-arm64") (:file "sha2") (:file "blake") (:file "ripemd160") (:file "aes")
               (:file "chacha") (:file "field25519") (:file "curve25519")))

(defsystem "clempo-crypto-hash/tests"
  :depends-on ("clempo-crypto-hash")
  :pathname "../../../tests/crypto/hash/"
  :components ((:file "sha2") (:file "digests") (:file "aes") (:file "chacha") (:file "curve25519")))
