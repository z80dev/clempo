(defpackage #:clempo.validation.tests
  (:use #:cl #:clempo.base #:clempo.transactions))
(in-package #:clempo.validation.tests)

(defun signed-grant (fields &optional (private-key 1))
  (let* ((normalized (clempo.signatures::key-authorization-fields fields))
         (hash (clempo.keccak:keccak256 (clempo.rlp:rlp-encode normalized))))
    (multiple-value-bind (r s parity) (clempo.secp256k1:sign hash (integer->octets private-key 32))
      (clempo.rlp:rlp-decode
       (clempo.rlp:rlp-encode
        (list normalized (octets-concat (integer->octets r 32)
                                        (integer->octets s 32) (octets-of parity))))))))

(deftest validation-signature-gas-and-version ()
  (let* ((secp (make-octets 65))
         (p256 (octets-concat (octets-of 1) (make-octets 129)))
         (web (octets-concat (octets-of 2 0 1) (make-octets 128)))
         (v1 (octets-concat (octets-of 3) (make-octets 20) p256))
         (v2 (octets-concat (octets-of 4) (make-octets 20) secp))
         (key-web (octets-concat (octets-of 4) (make-octets 20) web)))
    (is= 0 (tempo-signature-verification-gas secp))
    (is= 5000 (tempo-signature-verification-gas p256))
    (is= 5020 (tempo-signature-verification-gas web))
    (is= 8000 (tempo-signature-verification-gas v1))
    (is= 3000 (tempo-signature-verification-gas v2))
    (is= 8020 (tempo-signature-verification-gas key-web))
    (is (eq t (validate-signature-version v1 nil)))
    (is (eq :legacy-keychain-post-t1c (validate-signature-version v1 t)))
    (is (eq t (validate-signature-version v2 t)))
    (is (eq :keychain-v2-before-t1c (validate-signature-version v2 nil)))
    (is (eq t (validate-signature-version secp t)))
    (is= 10 (calldata-tokens (octets-of 0 1 0 255))))
  (is= 5 (calldata-tokens (octets-of 255 0 1 255) :start 1 :end 3))
  (is= 0 (calldata-tokens (make-octets 0)))
  (signals error (calldata-tokens (octets-of 0) :end 2))
  (signals clempo.signatures:signature-error
    (tempo-signature-verification-gas (octets-of 1))))

(deftest validation-call-creation-policy ()
  (let ((create (list (make-octets 0) (make-octets 0) (make-octets 0)))
        (call (list (make-octets 20) (make-octets 0) (make-octets 0))))
    (is (eq :empty-calls (validate-calls nil nil)))
    (is (eq t (validate-calls (list create call) nil)))
    (is (eq :create-not-first (validate-calls (list call create) nil)))
    (is (eq :create-with-authorizations (validate-calls (list create) '(authorization))))
    (is (eq t (validate-calls (list call call) '(authorization))))))

(deftest validation-recorded-authorization-fields ()
  (let* ((fixture (clempo.signatures.tests::fixture-by-label "tempo/secp256k1/aa-auth"))
         (auth (clempo.rlp:rlp-decode
                (hex->octets (aref (clempo.json:jget fixture "authorizationRlp") 0)))))
    (multiple-value-bind (chain address nonce signature) (tempo-authorization-fields auth)
      (is= (clempo.rlp:rlp-decode-integer (first auth)) chain)
      (is= (octets->integer (second auth)) address)
      (is= (clempo.rlp:rlp-decode-integer (third auth)) nonce)
      (is (eq (fourth auth) signature))
      (is= (octets->integer signature :start 1 :end 21)
           (clempo.signatures:authorization-signer auth)))))

(deftest validation-recorded-key-authorization-policy ()
  (dolist (label '("tempo/p256/keychain-v1/keyauth-webAuthn"
                   "tempo/secp256k1/keychain-v2/sponsored/keyauth-webAuthn"
                   "tempo/secp256k1/keyauth-secp256k1"
                   "tempo/secp256k1/keychain-v2/keyauth-secp256k1"
                   "tempo/secp256k1/keychain-v2/sponsored/keyauth-secp256k1"))
    (let* ((fixture (clempo.signatures.tests::fixture-by-label label))
           (auth (clempo.rlp:rlp-decode
                  (hex->octets (clempo.json:jget fixture "keyAuthorizationRlp"))))
           (signature (clempo.signatures.tests::fixture-signature fixture))
           (key-text (clempo.json:jget fixture "keyId"))
           (key (unless (zerop (length key-text)) (hex->integer key-text)))
           (sender (hex->integer (clempo.json:jget fixture "sender")))
           (chain (clempo.signatures:key-authorization-chain-id
                   (clempo.signatures:parse-key-authorization auth))))
      (is (eq t (validate-key-authorization
                 auth sender key chain
                 (if (eq :keychain-v1 (clempo.signatures:signature-kind signature)) :t1b :t13)
                 :transaction-signature signature))))))

(deftest validation-key-authorization-fork-boundaries ()
  (let* ((root #x7e5f4552091a69125d5dfcb7b8c2659029395bdf)
         (admin-key #x2b5ad5c4795c026514f8317c7a215e218dccd6cf)
         (empty (make-octets 0))
         (grant (signed-grant (list 1 0 2)))
         (signature (second grant)))
    (flet ((check (fields fork &optional access-key (private-key 1) (tx-signature signature))
             (validate-key-authorization (signed-grant fields private-key) root access-key 1 fork
                                         :transaction-signature tx-signature)))
      (is (eq t (validate-key-authorization nil root nil 1 :t0)))
      (is (eq t (validate-key-authorization grant root nil 1 :t13)))
      (is (eq :missing-transaction-signature
              (validate-key-authorization grant root 2 1 :t13)))
      (is (eq t (check (list 0 0 2) :t1b)))
      (is (eq :key-authorization-chain-id-mismatch (check (list 0 0 2) :t1c)))
      (is (eq :key-authorization-chain-id-mismatch (check (list 2 0 2) :t0)))
      (is (eq t (check (list 1 1 2) :t2 2)))
      (is (eq :key-authorization-signature-type-mismatch (check (list 1 1 2) :t3 2)))
      (is (eq :access-key-cannot-authorize-other-keys (check (list 1 0 2) :t5 3)))
      (is (eq :root-grant-requires-root-transaction (check (list 1 0 2) :t6 3)))
      (is (eq t (check (list 1 0 2) :t6 2)))
      (let ((fields (list 1 0 2 empty empty empty (make-octets 32))))
        (is (eq :key-authorization-witness-before-t5 (check fields :t4)))
        (is (eq t (check fields :t5))))
      (let ((fields (list 1 0 2 empty (list (list 3 4 60)))))
        (is (eq :periodic-key-authorization-before-t3 (check fields :t2)))
        (is (eq t (check fields :t3))))
      (let ((fields (list 1 0 2 empty empty nil)))
        (is (eq :key-call-scopes-before-t3 (check fields :t2)))
        (is (eq t (check fields :t3))))
      (let ((fields (list 1 0 2 empty empty empty empty 1)))
        (is (eq :admin-key-before-t6 (check fields :t5)))
        (is (eq t (check fields :t6))))
      (dolist (fields (list (list 1 0 2 100 empty empty empty 1)
                           (list 1 0 2 empty nil empty empty 1)
                           (list 1 0 2 empty empty nil empty 1)))
        (is (eq :admin-key-has-restrictions (check fields :t6))))
      (is (eq :key-authorization-account-mismatch
              (check (list 1 0 2 empty empty empty empty empty 3) :t6)))
      (is (eq :key-authorization-not-signed-by-root (check (list 1 0 2) :t5 nil 2)))
      (is (eq :key-authorization-account-required (check (list 1 0 2) :t6 nil 2)))
      (let ((fields (list 1 0 2 empty empty empty empty empty root)))
        (is (eq t (check fields :t6 admin-key 2)))
        (is (eq :admin-grant-requires-admin-transaction (check fields :t6 nil 2)))
        (is (eq :admin-grant-requires-admin-transaction (check fields :t6 3 2)))
        (is (eq :admin-grant-signature-type-mismatch
                (check fields :t6 admin-key 2
                       (clempo.signatures.tests::fixture-signature
                        (clempo.signatures.tests::fixture-by-label "tempo/webAuthn")))))))))
