(defpackage #:clempo.signatures.tests
  (:use #:cl #:clempo.base #:clempo.signatures))

(in-package #:clempo.signatures.tests)

(defparameter *signature-fixture-directory*
  #.(merge-pathnames "data/" (or *compile-file-truename* *load-truename*)))

(defparameter *signature-fixtures*
  (loop for name in '("signature-mainnet-moderato.json" "signature-p256.json")
        append
        (with-open-file (in (merge-pathnames name *signature-fixture-directory*))
          (let ((text (make-string (file-length in))))
            (read-sequence text in)
            (coerce (clempo.json:parse-json text) 'list)))))

(defun fixture-by-label (label)
  (find label *signature-fixtures* :key (lambda (x) (clempo.json:jget x "label"))
        :test #'string=))

(defun fixture-signature (fixture)
  (hex->octets (clempo.json:jget fixture "signature")))

(defun fixture-hash (fixture)
  (clempo.keccak:keccak256 (hex->octets (clempo.json:jget fixture "preimage"))))

(deftest signature-real-network-senders ()
  (dolist (fixture *signature-fixtures*)
    (multiple-value-bind (sender key-id)
        (recover-signature (fixture-signature fixture) (fixture-hash fixture))
      (is= (hex->integer (clempo.json:jget fixture "sender")) sender)
      (let ((expected (clempo.json:jget fixture "keyId")))
        (if (zerop (length expected))
            (is (null key-id))
            (is= (hex->integer expected) key-id))))))

(deftest signature-wire-kinds ()
  (dolist (entry '(("tempo/secp256k1" :secp256k1)
                   ("standalone P256 from tempo/p256/keychain-v1" :p256)
                   ("tempo/webAuthn" :webauthn)
                   ("tempo/p256/keychain-v1" :keychain-v1)
                   ("tempo/secp256k1/keychain-v2" :keychain-v2)))
    (is (eq (second entry) (signature-kind (fixture-signature (fixture-by-label (first entry)))))))
  (dolist (entry '(("tempo/secp256k1" :secp256k1)
                   ("tempo/webAuthn" :webauthn)
                   ("tempo/p256/keychain-v1" :p256)
                   ("tempo/secp256k1/keychain-v2" :secp256k1)))
    (is (eq (second entry)
            (signature-inner-kind (fixture-signature (fixture-by-label (first entry)))))))
  ;; Length takes priority over any apparent type prefix, even keychain.
  (let ((sig (make-octets 65)))
    (setf (aref sig 0) 4)
    (is (eq :secp256k1 (signature-kind sig))))
  (dolist (bad (list (make-octets 0) (octets-of 1) (make-octets 66)
                    (octets-concat (octets-of 1) (make-octets 128))
                    (octets-concat (octets-of 2) (make-octets 2049))
                    (octets-concat (octets-of 3) (make-octets 20)
                                   (octets-of 4) (make-octets 20) (make-octets 65))))
    (signals signature-error (signature-kind bad))
    (signals signature-error (signature-inner-kind bad))))

(deftest signature-domain-and-corruption ()
  (dolist (label '("tempo/webAuthn" "standalone P256 from tempo/p256/keychain-v1"))
    (let* ((fixture (fixture-by-label label))
           (signature (fixture-signature fixture)) (hash (fixture-hash fixture)))
      (setf (aref hash 0) (logxor 1 (aref hash 0)))
      (signals signature-error (recover-signature signature hash))))
  (let* ((fixture (fixture-by-label "tempo/webAuthn"))
         (signature (fixture-signature fixture)) (hash (fixture-hash fixture)))
    ;; Assertion flags cannot contain AT/ED and must contain UP or UV.
    (dolist (flags '(0 64 128 193))
      (setf (aref signature 33) flags)
      (signals signature-error (recover-signature signature hash))))
  (let* ((fixture (fixture-by-label "standalone P256 from tempo/p256/keychain-v1"))
         (signature (fixture-signature fixture)))
    ;; P256's prehash is a boolean decoded as nonzero, not restricted to 1.
    (setf (aref signature 129) 255)
    (is= (hex->integer (clempo.json:jget fixture "sender"))
         (recover-signature signature (fixture-hash fixture)))
    (replace signature (integer->octets #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550 32)
             :start1 33)
    (signals signature-error (recover-signature signature (fixture-hash fixture))))
  (let* ((fixture (fixture-by-label "tempo/secp256k1/keychain-v2"))
         (signature (fixture-signature fixture))
         (hash (fixture-hash fixture)))
    ;; V2 binds the root address; changing it changes the recovered access key.
    (let ((original-key (nth-value 1 (recover-signature signature hash))))
      (setf (aref signature 1) (logxor 1 (aref signature 1)))
      (is (/= original-key (nth-value 1 (recover-signature signature hash)))))))

(deftest signature-real-key-authorizations ()
  ;; The root account's WebAuthn approval pins the exact key-authorization RLP hash:
  ;; any domain/field change fails its embedded transaction-hash challenge.
  (dolist (label '("tempo/p256/keychain-v1/keyauth-webAuthn"
                   "tempo/secp256k1/keychain-v2/sponsored/keyauth-webAuthn"
                   "tempo/secp256k1/keyauth-secp256k1"
                   "tempo/secp256k1/keychain-v2/keyauth-secp256k1"
                   "tempo/secp256k1/keychain-v2/sponsored/keyauth-secp256k1"))
    (let* ((fixture (fixture-by-label label))
           (auth (clempo.rlp:rlp-decode
                  (hex->octets (clempo.json:jget fixture "keyAuthorizationRlp")))))
      (let* ((view (parse-key-authorization auth))
             (fields (first auth)))
        (is= (clempo.rlp:rlp-decode-integer (first fields))
             (key-authorization-chain-id view))
        (is= (clempo.rlp:rlp-decode-integer (second fields))
             (key-authorization-key-type view))
        (is= (octets->integer (third fields)) (key-authorization-key-id view))
        (let ((expiry (fourth fields)))
          (if (and expiry (plusp (length expiry)))
              (is= (clempo.rlp:rlp-decode-integer expiry) (key-authorization-expiry view))
              (is (null (key-authorization-expiry view))))))
      (is= (hex->integer (clempo.json:jget fixture "sender"))
           (key-authorization-signer auth)))))

(deftest signature-real-tempo-authorization ()
  (let* ((fixture (fixture-by-label "tempo/secp256k1/aa-auth"))
         (auth (clempo.rlp:rlp-decode
                (hex->octets (aref (clempo.json:jget fixture "authorizationRlp") 0)))))
    ;; This authorization is a keychain V2 approval; the RPC fixture embeds its root.
    (is= (octets->integer (fourth auth) :start 1 :end 21)
         (authorization-signer auth))))

(deftest signature-client-json-validation ()
  (dolist (json '("{}" "{\"type\":\"webauthn.get\"}"
                  "{\"type\":\"webauthn.get\",\"challenge\":\"a\",\"challenge\":\"b\"}"
                  "{\"type\":\"web\\u0061uthn.get\",\"challenge\":\"a\"}"
                  "{\"type\":\"webauthn.get\",\"challenge\":\"a\",\"x\":01}"
                  "{\"type\":\"webauthn.get\",\"challenge\":\"a\",\"x\":[1,]}"))
    (signals signature-error (clempo.signatures::webauthn-client-data (string->octets json)))))

(deftest signature-p256-prehash-equivalence ()
  (let* ((fixture (fixture-by-label "standalone P256 from tempo/p256/keychain-v1"))
         (signature (fixture-signature fixture)))
    (setf (aref signature 129) 0)
    (is= (hex->integer (clempo.json:jget fixture "sender"))
         (recover-signature signature (clempo.sha2:sha256 (fixture-hash fixture))))))

(deftest signature-real-eip7702-wire-equivalence ()
  (with-open-file (in (merge-pathnames "signature-eip7702.json" *signature-fixture-directory*))
    (let ((text (make-string (file-length in))))
      (read-sequence text in)
      (loop for encoded across (clempo.json:jget (clempo.json:parse-json text) "authorizations")
            for auth = (clempo.rlp:rlp-decode (hex->octets encoded))
            for signature = (octets-concat
                             (integer->octets (clempo.rlp:rlp-decode-integer (fifth auth)) 32)
                             (integer->octets (clempo.rlp:rlp-decode-integer (sixth auth)) 32)
                             (integer->octets (clempo.rlp:rlp-decode-integer (fourth auth)) 1))
            do (is= (authorization-signer auth)
                    (authorization-signer (append (subseq auth 0 3) (list signature))))))))

(deftest signature-parsed-key-authorization-options ()
  (let* ((empty (make-octets 0))
         (signature (make-octets 65))
         (minimal (parse-key-authorization (list (list 1 0 2) signature)))
         (fields (list 1 2 3 100
                       (list (list 4 500) (list 5 600 60))
                       (list (list 6 (list (list (integer->octets #xa9059cbb 4)
                                                (list 7 8))))
                             (list 9 nil))
                       (make-octets 32) 1 (make-octets 20)))
         ;; Exercise the actual decoded-RLP entry point, not just integer convenience.
         (decoded (clempo.rlp:rlp-decode
                   (clempo.rlp:rlp-encode
                    (list (clempo.signatures::key-authorization-fields fields) signature))))
         (view (parse-key-authorization decoded)))
    (is (eq :absent (key-authorization-limits minimal)))
    (is (eq :absent (key-authorization-allowed-calls minimal)))
    (is (null (key-authorization-expiry minimal)))
    (is (null (key-authorization-witness minimal)))
    (is (null (key-authorization-account minimal)))
    (is (null (key-authorization-is-admin minimal)))
    (is= 0 (key-authorization-limit-count minimal))
    (is= 1 (key-authorization-chain-id view))
    (is= 2 (key-authorization-key-type view))
    (is= 3 (key-authorization-key-id view))
    (is= 100 (key-authorization-expiry view))
    (is= 0 (key-authorization-witness view))
    (is= 0 (key-authorization-account view))
    (is (key-authorization-is-admin view))
    (is= 2 (key-authorization-limit-count view))
    (is= 2 (key-authorization-call-scope-count view))
    (is= 1 (key-authorization-selector-count view))
    (is= 2 (key-authorization-recipient-count view))
    (let ((limits (key-authorization-limits view)))
      (is= 4 (key-authorization-limit-token (first limits)))
      (is= 500 (key-authorization-limit-amount (first limits)))
      (is= 0 (key-authorization-limit-period (first limits)))
      (is= 60 (key-authorization-limit-period (second limits)))
      (is (eq limits (key-authorization-limits view))))
    (let* ((scope (first (key-authorization-allowed-calls view)))
           (rule (first (key-authorization-call-scope-selector-rules scope))))
      (is= 6 (key-authorization-call-scope-target scope))
      (is= #xa9059cbb (key-authorization-selector-rule-selector rule))
      (is (equal '(7 8) (key-authorization-selector-rule-recipients rule)))
      (is (eq (key-authorization-selector-rule-recipients rule)
              (key-authorization-selector-rule-recipients rule))))
    (setf (aref (third (first decoded)) 0) 99)
    (is= 3 (key-authorization-key-id view))
    (let ((denied (parse-key-authorization
                   (list (list 1 0 2 empty nil nil) signature))))
      (is (null (key-authorization-limits denied)))
      (is (null (key-authorization-allowed-calls denied)))
      (is= 0 (key-authorization-limit-count denied))
      (is= 0 (key-authorization-call-scope-count denied)))))

(deftest signature-parsed-key-authorization-malformed ()
  (let ((signature (make-octets 65)) (empty (make-octets 0)))
    ;; key type 3 is a T14 ConfigurableAccount grant (SignatureType 3); 4 is no type
    (dolist (fields (list nil (list 1 0) (list 1 4 2) (list 1 0 (make-octets 19))
                         (list 1 0 2 0) (list 1 0 2 empty (list (list 3 4 0)))
                         (list 1 0 2 empty empty (list (list 3 (list (list empty nil)))))
                         (list 1 0 2 empty empty empty (make-octets 31))
                         (list 1 0 2 empty empty empty empty 2)
                         (list 1 0 2 empty empty empty empty empty (make-octets 19))))
      (signals error (parse-key-authorization (list fields signature))))
    (signals signature-error (parse-key-authorization (list (list 1 0 2))))
    (signals signature-error
      (parse-key-authorization (list (list 1 0 2) (octets-of 1))))
    (signals signature-error
      (parse-key-authorization
       (list (list 1 0 2)
             (octets-concat (octets-of 4) (make-octets 20) signature))))))
