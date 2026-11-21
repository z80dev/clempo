(defpackage #:clempo.transactions.tests
  (:use #:cl #:clempo.base #:clempo.transactions))
(in-package #:clempo.transactions.tests)

(defparameter *rpc-transactions*
  (with-open-file (in (asdf:system-relative-pathname "clempo-types"
                                                    "../../tests/types/data/transaction-rpc.sexp"))
    (let ((*read-eval* nil)) (read in))))

(defparameter *alloy-2930*
  (with-open-file (in (asdf:system-relative-pathname "clempo-types"
                                                    "../../tests/types/data/transaction-2930.sexp"))
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
  (let* ((raw (hex->octets (first *alloy-2930*)))
         (tx (decode-transaction raw)))
    (is= 1 (transaction-type tx))
    (is-equalp raw (encode-transaction tx))
    (is-equalp (clempo.keccak:keccak256 (hex->octets (second *alloy-2930*)))
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

(deftest typed-transaction-field-readers ()
  (let ((tx (decode-transaction (hex->octets (first *alloy-2930*)))))
    (is= 1 (transaction-chain-id tx))
    (is= 0 (transaction-nonce tx))
    (is= 1 (transaction-gas-price tx))
    (is= 2 (transaction-gas-limit tx))
    (is= 3 (transaction-value tx))
    (is (null (transaction-to tx)))
    (is (null (transaction-fee-token tx)))
    (is-equalp (octets-of 1 2) (transaction-input tx)))
  (let ((tx (decode-transaction (hex->octets (second (first *rpc-transactions*))))))
    (is= 42431 (transaction-chain-id tx))
    (is= 18 (transaction-nonce tx)))
  (let ((tx (decode-transaction (hex->octets (second (fifth *rpc-transactions*))))))
    (is= 42431 (transaction-chain-id tx))
    (is= 0 (transaction-nonce-key tx))
    (is= 5 (transaction-nonce tx))
    (is= #x20c0000000000000000000000000000000000002 (transaction-fee-token tx))
    (is (null (transaction-valid-before tx)))
    (is (null (transaction-valid-after tx)))
    (is (consp (transaction-calls tx)))))

(deftest transaction-direct-writer ()
  (dolist (fixture *rpc-transactions*)
    (let* ((raw (hex->octets (second fixture)))
           (tx (decode-transaction raw))
           (out (make-octets (+ 10 (transaction-encoded-length tx)))))
      (is= (length raw) (transaction-encoded-length tx))
      (is= (+ 5 (length raw)) (encode-transaction-into tx out 5))
      (is-equalp raw (subseq out 5 (+ 5 (length raw)))))))

(deftest sender-scoped-transaction-identifiers ()
  (let ((fixtures (clempo.json:parse-json
                   (clempo.io:read-file-string
                    (asdf:system-relative-pathname "clempo-types"
                     "../../tests/types/data/signature-mainnet-moderato.json")))))
    (loop for fixture across fixtures
          for label = (clempo.json:jget fixture "label")
          for tx-fixture = (find label *rpc-transactions* :key #'first :test #'string=)
          when tx-fixture
            do (let* ((tx (decode-transaction (hex->octets (second tx-fixture))))
                      (sender (hex->integer (clempo.json:jget fixture "sender")))
                      (expected (clempo.keccak:keccak256
                                 (octets-concat (hex->octets (clempo.json:jget fixture "preimage"))
                                                (integer->octets sender 20)))))
                 (is-equalp expected (transaction-unique-identifier tx sender))
                 (is (not (octets= expected (transaction-unique-identifier tx (logxor 1 sender)))))))))
