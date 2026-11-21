(defpackage #:clempo.transactions.tests
  (:use #:cl #:clempo.base #:clempo.transactions))
(in-package #:clempo.transactions.tests)

(defparameter *rpc-transactions*
  (with-open-file (in (asdf:system-relative-pathname "clempo-types"
                                                    "../../tests/types/data/transaction-rpc.sexp"))
    (let ((*read-eval* nil)) (read in))))

(deftest rpc-transaction-envelopes ()
  (dolist (fixture *rpc-transactions*)
    (destructuring-bind (label envelope hash sender payer key-id) fixture
      (let* ((raw (hex->octets envelope)) (tx (decode-transaction raw)))
        (is-equalp raw (encode-transaction tx) "~A" label)
        (is-equalp (hex->octets hash) (transaction-hash tx) "~A" label)
        (is= (hex->integer sender) (transaction-sender tx) "~A" label)
        (is= (hex->integer payer) (transaction-fee-payer tx) "~A" label)
        (is= (unless (zerop (length key-id)) (hex->integer key-id))
             (transaction-key-id tx) "~A" label)
        (when (search "keyauth-" label)
          (is= (hex->integer sender) (transaction-key-authorization-signer tx) "~A" label))))))

(deftest received-hash-and-reconstructed-encoding ()
  (let* ((raw (hex->octets (second (first *rpc-transactions*))))
         (tx (decode-transaction raw))
         (hash (transaction-hash tx)))
    (setf (aref raw 0) 0)
    (is-equalp hash (transaction-hash tx))
    (setf (first (transaction-fields tx)) (octets-of 19))
    (is-equalp hash (transaction-hash tx))
    (is (not (octets= (hex->octets (second (first *rpc-transactions*)))
                     (encode-transaction tx))))))

(deftest alloy-eip2930-create ()
  ;; alloy-consensus 2.5.0 eip2930.rs::test_decode_create, including its signature.
  (let* ((raw (hex->octets "01f84d018001028003820102c080a0840cfc572845f5786e702984c2a582528cad4b49b2a10b9db1be7fca90058565a025e7109ceb98168d95b09b18bbf6b685130e0562f233877d492b94eee0c5b6d1"))
         (tx (decode-transaction raw)))
    (is= 1 (transaction-type tx))
    (is-equalp raw (encode-transaction tx))
    (is-equalp (clempo.keccak:keccak256 (hex->octets "01ca018001028003820102c0"))
               (transaction-signing-hash tx))))

(deftest legacy-pre-eip155-signing ()
  ;; The EIP-155 example's six transaction fields with a pre-155 v.
  (let* ((fields (list 9 20000000000 21000
                       (hex->octets "3535353535353535353535353535353535353535")
                       1000000000000000000 (make-octets 0) 27 1 1))
         (tx (decode-transaction (clempo.rlp:rlp-encode fields))))
    (is-equalp (clempo.keccak:keccak256 (clempo.rlp:rlp-encode (subseq fields 0 6)))
               (transaction-signing-hash tx))))

(deftest eip155-official-example ()
  (let* ((raw (hex->octets "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"))
         (tx (decode-transaction raw)))
    (is-equalp raw (encode-transaction tx))
    (is-equalp (hex->octets "daf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53")
               (transaction-signing-hash tx))
    (is= #x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f (transaction-sender tx))))

(deftest malformed-transaction-envelopes ()
  (signals error (decode-transaction (make-octets 0)))
  (signals error (decode-transaction (hex->octets "03c0")))
  (signals error (decode-transaction (hex->octets "00c0")))
  (signals error (decode-transaction (hex->octets "01c0")))
  (signals error (decode-transaction (hex->octets "76c0")))
  (let ((raw (hex->octets (second (first *rpc-transactions*)))))
    (signals error (decode-transaction (octets-concat raw (octets-of 0))))))
