(defsystem "clempo-codec"
  :description "Wire encodings: RLP, JSON, ABI, commonware codec"
  :depends-on ("clempo-base" "clempo-crypto")
  :components ((:file "rlp")
               (:file "json")
               (:file "commonware")))

(defsystem "clempo-codec/tests"
  :depends-on ("clempo-codec")
  :pathname "../../tests/codec/"
  :components ((:file "rlp-tests")
               (:file "json-tests")
               (:file "commonware-tests")))
