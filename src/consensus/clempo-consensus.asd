;;; Owner: the consensus agent. Simplex artifacts, the threshold scheme and finalization
;;; certificates. Subsystems: dkg/ (clempo-consensus-dkg), simplex/ (clempo-consensus-simplex).
(defsystem "clempo-consensus"
  :description "Tempo consensus: simplex wire types, bls12381 threshold VRF scheme, certificates"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-consensus-dkg")
  :serial t
  :components ((:file "wire")
               (:file "scheme")
               (:file "certify")))

(defsystem "clempo-consensus/tests"
  :depends-on ("clempo-consensus" "clempo-types")
  :pathname "../../tests/consensus/"
  :serial t
  :components ((:file "scheme-tests")
               (:file "certify-tests")))
