(defsystem "clempo-validator"
  :description "The Tempo validator process: consensus, DKG, catch-up, peers and JSON-RPC"
  :depends-on ("clempo-p2p" "clempo-io" "clempo-codec" "clempo-crypto-hash" "sb-bsd-sockets"
               "sb-posix" "clempo-consensus" "clempo-consensus-simplex" "clempo-consensus-dkg" "clempo-types"
               "clempo-tempo-builder" "clempo-tempo-precompiles" "clempo-crypto-ec" "clempo-crypto-keccak"
               "clempo-sync" "clempo-rpc" "clempo-store" "clempo-tempo-executor")
  :serial t
  :components ((:file "manifest") (:file "network") (:file "wire") (:file "journal")
               (:file "keys-age") (:file "keys") (:file "chain") (:file "engine") (:file "process")))

(defsystem "clempo-validator/tests"
  :depends-on ("clempo-validator")
  :pathname "../../tests/validator/"
  :serial t
  :components ((:file "network") (:file "wire") (:file "journal") (:file "keys") (:file "memory-net")
               (:file "engine") (:file "cluster")))
