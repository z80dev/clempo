;;; Owner: the TLS agent. What a TLS client needs beyond hash/ and ec/: AES-GCM, RSA
;;; signature verification, a DER reader and X.509 path validation.
(defsystem "clempo-crypto-pki"
  :description "aes-gcm, rsa verification, asn.1 der, x.509"
  :depends-on ("clempo-base" "clempo-crypto-hash" "clempo-crypto-ec")
  :serial t
  :components ((:file "gcm") (:file "rsa") (:file "ecdh") (:file "der") (:file "x509")))

(defsystem "clempo-crypto-pki/tests"
  :depends-on ("clempo-crypto-pki")
  :pathname "../../../tests/crypto/pki/"
  :serial t
  :components ((:file "gcm-tests") (:file "rsa-tests")))
