(defpackage #:clempo.rpc.aa-request-tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.rpc.aa-request-tests)

;;; Pure pieces of the Tempo request conversion (src/rpc/calls.lisp): mock signatures as
;;; create_mock_tempo_sig builds them, alloy's address Display and the reference's error texts.
;;; Expected texts were read from the reference node (corpus-calls.jsonl, block 0x9bd9c4).

(defun c (name) (symbol-function (find-symbol name "CLEMPO.RPC.CALLS")))

(deftest aa-mock-signatures ()
  (let ((mock (c "MOCK-PRIMITIVE-SIGNATURE")))
    (is= 65 (length (funcall mock :secp256k1 nil)))
    (is= 27 (aref (funcall mock :secp256k1 nil) 64))
    (is= 130 (length (funcall mock :p256 nil)))
    (is= 1 (aref (funcall mock :p256 nil) 0))
    ;; WebAuthn: type byte, the sized data (default 800), r s x y
    (is= (+ 1 800 128) (length (funcall mock :webauthn nil)))
    (is= (+ 1 1024 128) (length (funcall mock :webauthn (octets-of 4 0))))
    ;; a hint below the template's 87 bytes is raised to it; above 8192 clamped
    (is= (+ 1 87 128) (length (funcall mock :webauthn (octets-of 10))))
    (is= (+ 1 8192 128) (length (funcall mock :webauthn (octets-of 0 1 0 0))))))

(deftest aa-checksum-address ()
  (is= "0xABCDEfabCdEf1111111111111111111111111111"
       (funcall (c "CHECKSUM-ADDRESS") #xabcdefabcdef1111111111111111111111111111))
  (is= "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
       (funcall (c "CHECKSUM-ADDRESS") #x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed)))

(deftest aa-halt-names ()
  (is= "InvalidFEOpcode" (funcall (c "HALT-NAME") :invalid-fe-opcode))
  (is= "OpcodeNotFound" (funcall (c "HALT-NAME") :opcode-not-found))
  (is= "OutOfGas" (funcall (c "HALT-NAME") :out-of-gas)))

(deftest aa-invalid-texts ()
  (flet ((text (reason) (nth-value 1 (funcall (c "INVALID-REQUEST-ERROR") reason))))
    (is= "value transfer in Tempo Transaction not allowed" (text "value transfer not allowed in AA"))
    (is= "expiring nonce transaction requires valid_before to be set"
         (text "expiring nonce without valid_before"))
    (is= "keychain validation failed: AccountKeychainError(KeyNotFound(KeyNotFound))"
         (text "keychain validation failed: precompile revert CLEMPO.KEYCHAIN::KEY-NOT-FOUND 0x5f3f479c"))
    (is= "insufficient fee token balance: required 30, but only have 7"
         (text "insufficient fee token balance: 7 < 30"))
    (is= "fee token 0xABCDEfabCdEf1111111111111111111111111111 is not a TIP-20 token; fee tokens must be TIP-20 tokens"
         (text "fee token ABCDEFABCDEF1111111111111111111111111111 is not TIP-20"))))
