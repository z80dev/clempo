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

(defun list-depth (item)
  (if (listp item) (1+ (reduce #'max item :key #'list-depth :initial-value 0)) 0))

(deftest tempo-structures-nest-far-below-the-rlp-limit ()
  ;; The deepest recorded envelope is a keychain-v2 transaction carrying a key authorization
  ;; with call scopes: 8 lists, a quarter of clempo.rlp's depth limit.
  (let ((deepest 0))
    (dolist (fixture *rpc-transactions*)
      (let ((raw (hex->octets (second fixture))))
        (setf deepest (max deepest (list-depth (clempo.rlp:rlp-decode
                                                raw :start (if (>= (aref raw 0) #xc0) 0 1)))))))
    (with-open-file (in (asdf:system-relative-pathname "clempo-types"
                                                       "../../tests/types/data/blocks.jsonl"))
      (loop for line = (read-line in nil) while line
            do (let ((body (clempo.rlp:rlp-decode
                            (hex->octets (clempo.json:jget (clempo.json:parse-json line) "raw")))))
                 (setf deepest (max deepest (list-depth body)))
                 (dolist (tx (second body))
                   (when (typep tx 'octets)
                     (setf deepest (max deepest (list-depth (clempo.rlp:rlp-decode tx :start 1)))))))))
    (is= 8 deepest)
    (is (< deepest clempo.rlp:*rlp-max-depth*))))

;;; EIP-2: alloy's secp256k1::recover_signer rejects s > n/2 for every Ethereum envelope and
;;; for the 0x76 fee payer signature. Each test signature's malleated twin (n - s, other
;;; parity) still recovers the same key under plain ecrecover, so accepting it would let
;;; anyone mint a second valid encoding (and hash) of someone else's transaction.

(defun flip-parity (type v)
  (cond ((plusp type) (logxor v 1))
        ((< v 35) (- 55 v))
        ((evenp (- v 35)) (1+ v))
        (t (1- v))))

(defun high-s-twin (raw)
  "RAW's envelope with its trailing ECDSA signature replaced by the malleated twin."
  (let* ((tx (decode-transaction raw)) (type (transaction-type tx))
         (fields (copy-list (transaction-fields tx))) (n (length fields))
         (v (clempo.rlp:rlp-decode-integer (nth (- n 3) fields)))
         (s (clempo.rlp:rlp-decode-integer (nth (- n 1) fields))))
    (setf (nth (- n 3) fields) (flip-parity type v)
          (nth (- n 1) fields) (- clempo.secp256k1:+secp256k1-n+ s))
    (decode-transaction (if (zerop type)
                            (clempo.rlp:rlp-encode fields)
                            (octets-concat (octets-of type) (clempo.rlp:rlp-encode fields))))))

(defun twin-recovers (twin)
  "The address plain ecrecover (no EIP-2) gives TWIN's signature."
  (let* ((fields (transaction-fields twin)) (n (length fields)) (type (transaction-type twin))
         (v (clempo.rlp:rlp-decode-integer (nth (- n 3) fields))))
    (clempo.secp256k1:ecrecover-address
     (transaction-signing-hash twin)
     (clempo.rlp:rlp-decode-integer (nth (- n 2) fields))
     (clempo.rlp:rlp-decode-integer (nth (- n 1) fields))
     (cond ((plusp type) v) ((< v 35) (- v 27)) (t (mod (- v 35) 2))))))

(deftest mainnet-high-s-ethereum-envelopes-rejected ()
  ;; 24 legacy/EIP-155/EIP-1559 mainnet transactions (tests/crypto/ec/data/README.md).
  (with-open-file (in (asdf:system-relative-pathname
                       "clempo-types" "../../tests/crypto/ec/data/secp256k1/tempo_mainnet_senders.txt"))
    (loop for line = (read-line in nil) while line
          do (let* ((cols (uiop:split-string line :separator '(#\Space)))
                    (raw (hex->octets (first cols))) (sender (hex->integer (second cols)))
                    (twin (high-s-twin raw)))
               (is= sender (transaction-sender (decode-transaction raw)))
               (is= sender (twin-recovers twin))
               (is (not (octets= (transaction-hash twin)
                                 (transaction-hash (decode-transaction raw)))))
               (signals invariant-violation (transaction-sender twin))
               (signals invariant-violation (transaction-fee-payer twin))))))

(deftest eip155-example-high-s-rejected ()
  (let* ((raw (hex->octets "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"))
         (twin (high-s-twin raw)))
    (is= #x9d8a62f656a8d1615c1294fd71e9cfb3e4855a4f (twin-recovers twin))
    (signals invariant-violation (transaction-sender twin))))

(deftest sponsored-high-s-fee-payer-rejected ()
  ;; Tempo's recover_fee_payer is alloy's recover_signer too. The sender signature does not
  ;; cover the fee payer signature, so the twin keeps its sender and loses its fee payer.
  (dolist (label '("tempo/secp256k1/sponsored" "tempo/webAuthn/sponsored"
                   "tempo/secp256k1/keychain-v2/sponsored"))
    (destructuring-bind (envelope hash sender payer key-id)
        (rest (find label *rpc-transactions* :key #'first :test #'string=))
      (declare (ignore hash key-id))
      (let* ((tx (decode-transaction (hex->octets envelope)))
             (fields (copy-list (transaction-fields tx)))
             (sponsor (nth 11 fields))
             (s (clempo.rlp:rlp-decode-integer (third sponsor)))
             (twin-sponsor (list (logxor 1 (clempo.rlp:rlp-decode-integer (first sponsor)))
                                 (second sponsor) (- clempo.secp256k1:+secp256k1-n+ s))))
        (is= (hex->integer payer) (transaction-fee-payer tx) "~A" label)
        (setf (nth 11 fields) twin-sponsor)
        (let* ((twin (decode-transaction
                      (octets-concat (octets-of #x76) (clempo.rlp:rlp-encode fields))))
               (unsigned (butlast (transaction-fields twin))))
          (setf (nth 11 unsigned) (integer->octets (hex->integer sender) 20))
          (is= (hex->integer payer)
               (clempo.secp256k1:ecrecover-address
                (clempo.keccak:keccak256 (octets-concat (octets-of #x78)
                                                        (clempo.rlp:rlp-encode unsigned)))
                (clempo.rlp:rlp-decode-integer (second twin-sponsor)) (third twin-sponsor)
                (first twin-sponsor))
               "~A" label)
          (is= (hex->integer sender) (transaction-sender twin) "~A" label)
          (signals invariant-violation (transaction-fee-payer twin)))))))
