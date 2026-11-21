(defsystem "clempo-validator"
  :description "Local Tempo validator process and authenticated peer supervision"
  :depends-on ("clempo-p2p" "clempo-io" "clempo-codec" "clempo-crypto-hash" "sb-bsd-sockets"
               "sb-posix" "clempo-consensus" "clempo-consensus-simplex" "clempo-consensus-dkg" "clempo-types"
               "clempo-tempo-builder" "clempo-tempo-precompiles" "clempo-crypto-ec" "clempo-crypto-keccak"
               "clempo-sync")
  :serial t
  :components ((:file "manifest") (:file "network") (:file "wire") (:file "journal")
               (:file "keys-age") (:file "keys") (:file "engine")))

(defsystem "clempo-validator/tests"
  :depends-on ("clempo-validator")
  :pathname "../../tests/validator/"
  :serial t
  :components ((:file "network") (:file "wire") (:file "journal") (:file "keys") (:file "engine")))
