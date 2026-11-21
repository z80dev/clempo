;;; Owner: the DKG agent. commonware's Feldman/Desmedt DKG and reshare, and Tempo's
;;; on-chain DKG artifacts (boundary outcomes, dealer logs in extraData).
(defsystem "clempo-consensus-dkg"
  :description "Feldman/Desmedt DKG and reshare over BLS12-381 (MinSig), byte-compatible with commonware 2026.10.1 and Tempo"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec")
  :serial t
  :components ((:file "package")
               (:file "codec")
               (:file "field")
               (:file "transcript")
               (:file "info")
               (:file "group")
               (:file "protocol")
               (:file "tempo")
               (:file "manager")))

(defsystem "clempo-consensus-dkg/tests"
  :depends-on ("clempo-consensus-dkg")
  :pathname "../../../tests/consensus/dkg/"
  :serial t
  :components ((:file "codec-tests")
               (:file "info-tests")
               (:file "protocol-tests")
               (:file "ceremony-tests")
               (:file "tempo-tests")
               (:file "manager-tests")))
