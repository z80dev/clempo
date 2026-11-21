(defpackage #:clempo.rpc.impl-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:archive #:clempo.archive) (#:store #:clempo.store) (#:trie #:clempo.trie)
                    (#:calls #:clempo.rpc.calls) (#:tries #:clempo.rpc.tries)))
(in-package #:clempo.rpc.impl-tests)

;;; eth_callBundle, trace_rawTransaction and debug_stateRootWithUpdates. Expected answers
;;; were recorded from the reference node (CLEMPO_RPC) on 2026-11-21; the same requests are
;;; in ~/cache/clempo/rpc/corpus-impl.jsonl (rpc-record -suite impl).

(defun answer (body &optional backend)
  (json:parse-json (if backend
                       (chain:with-backend-read (backend) (nth-value 1 (rpc:handle-body body)))
                       (nth-value 1 (rpc:handle-body body)))))

(defun is-json (expected actual)
  (is (json:json-equal expected actual) "expected ~A~%got ~A" (json:to-json-string expected) (json:to-json-string actual)))

(defun request (method params)
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":~A}" method params))

(defun error-of (method params)
  (let ((e (json:jget (answer (request method params)) "error")))
    (list (json:jget e "code") (or (json:jget e "data") (json:jget e "message")))))

(deftest call-bundle-request-errors ()
  ;; checked before any state is read, in reth's order
  (loop for (params expected) in
        '(("[{\"txs\":[],\"blockNumber\":\"0x1\",\"stateBlockNumber\":\"0x0\"}]" (-32602 "bundle missing txs"))
          ("[{\"txs\":[\"0x01\"],\"blockNumber\":\"0x0\",\"stateBlockNumber\":\"0x1\"}]" (-32602 "bundle missing blockNumber"))
          ("[{\"txs\":[\"0x01\"],\"blockNumber\":\"0x1\",\"stateBlockNumber\":\"0x1\",\"gasLimit\":\"0x3000000\"}]"
           (-32000 "intrinsic gas too high"))
          ("[{\"txs\":[\"0x\"],\"blockNumber\":\"0x1\",\"stateBlockNumber\":\"0x1\"}]" (-32602 "empty transaction data"))
          ("[{\"txs\":[\"0x01\"],\"blockNumber\":\"0x1\",\"stateBlockNumber\":\"0x1\"}]"
           (-32602 "failed to decode signed transaction"))
          ("[{\"txs\":\"0x01\",\"blockNumber\":\"0x1\",\"stateBlockNumber\":\"0x1\"}]"
           (-32602 "invalid type: string \"0x01\", expected a sequence at line 1 column 13"))
          ("[{\"txs\":[\"0x01\"],\"blockNumber\":\"0x1\"}]"
           (-32602 "missing field `stateBlockNumber` at line 1 column 36"))
          ;; serde's positional form of the struct: the defaulted fields may be left off the end
          ("[[[\"0x01\"],\"0x1\"]]"
           (-32602 "invalid length 2, expected struct EthCallBundle with 10 elements at line 1 column 16"))
          ("[[[],\"0x1\",\"0x1\",null,null,null,null,null,null,null,5]]"
           (-32602 "trailing characters at line 1 column 52"))
          ("[[[],\"0x1\",\"0x1\"]]" (-32602 "bundle missing txs"))
          ("[5]" (-32602 "invalid type: integer `5`, expected struct EthCallBundle at line 1 column 1")))
        do (is= expected (error-of "eth_callBundle" params) "~A" params)))

(deftest raw-transaction-trace-types ()
  (is= '(-32602 "unknown variant `nope`, expected one of `trace`, `vmTrace`, `stateDiff` at line 1 column 7")
       (error-of "trace_rawTransaction" "[\"0x01\",[\"nope\"]]"))
  (is= '(-32602 "empty transaction data") (error-of "trace_rawTransaction" "[\"0x\",[\"trace\"]]")))

;;; Replays on the mainnet archive with historical state: mainnet transaction
;;; 0x8023a19c...c345, the first of block 47,952,533, on its parent's state.

(defparameter *block* 47952533)

(defun replay-backend ()
  (unless (archive:chunk-present-p (* archive:+chunk-blocks+ (floor *block* archive:+chunk-blocks+)) (archive:archive-dir))
    (skip-test "block 47,952,533 is not in the mainnet archive"))
  (let ((b (chain:make-archive-backend :head *block*)))
    (unless (chain:backend-state b (1- *block*))
      (skip-test "no historical state for block 47,952,532 (clempo-dev history-build)"))
    b))

(defun first-raw-tx ()
  (octets->hex (clempo.transactions::transaction-raw
                (first (clempo.blocks:block-transactions (clempo.blocks:decode-block (archive:raw-block *block*)))))))

(deftest call-bundle-replays-a-mainnet-transaction ()
  (let* ((b (replay-backend))
         (result (json:jget (answer (request "eth_callBundle"
                                             (format nil "[{\"txs\":[~S],\"blockNumber\":~S,\"stateBlockNumber\":~S,\"timestamp\":\"0x6aefd1e4\"}]"
                                                     (first-raw-tx) (quantity->hex *block*) (quantity->hex (1- *block*))))
                                    b)
                            "result")))
    (is-json (json:parse-json "{\"bundleHash\":\"0xfa4b676bd403f3cfcb3bcb67f4d2b89af42761176a45d253fe5694e088faa341\",\"bundleGasPrice\":\"0\",\"coinbaseDiff\":\"0\",\"ethSentToCoinbase\":\"0\",\"gasFees\":\"0\",\"results\":[{\"coinbaseDiff\":\"0\",\"ethSentToCoinbase\":\"0\",\"fromAddress\":\"0x14640eecf09d436d7b082d83fa96156ea5b21992\",\"gasFees\":\"0\",\"gasPrice\":\"0\",\"gasUsed\":143074,\"toAddress\":\"0x20c000000000000000000000b9537d11c60e8b50\",\"txHash\":\"0x8023a19cee232b39aff3b3ce29ab92eb3d05f325031a7cd28c101803500ec345\",\"value\":\"0x00000000000000000000000000000000000000000000000000000000000f4114\"}],\"stateBlockNumber\":47952532,\"totalGasUsed\":143074}")
         result)
    ;; on the block's own state the nonce is spent: reth's NonceTooLow text
    (is= (json:parse-json "{\"code\":-32000,\"message\":\"nonce too low: next nonce 1339, tx nonce 1338\"}")
         (json:jget (answer (request "eth_callBundle"
                                     (format nil "[{\"txs\":[~S],\"blockNumber\":~S,\"stateBlockNumber\":~S}]"
                                             (first-raw-tx) (quantity->hex (1+ *block*)) (quantity->hex *block*)))
                            b)
                    "error"))))

(deftest raw-transaction-trace-replays-a-mainnet-transaction ()
  (let* ((b (replay-backend))
         (result (json:jget (answer (request "trace_rawTransaction"
                                             (format nil "[~S,[\"trace\"],~S]" (first-raw-tx) (quantity->hex (1- *block*))))
                                    b)
                            "result")))
    (is= "0x00000000000000000000000000000000000000000000000000000000000f4114" (json:jget result "output"))
    (is (eq :null (json:jget result "stateDiff")))
    ;; a Tempo batch: the synthetic top-level frame and its two calls
    (is= 3 (length (json:jget result "trace")))
    (is= 2 (json:jget (svref (json:jget result "trace") 0) "subtraces"))))

;;; debug_stateRootWithUpdates over a small state database: the hashed post-state is applied
;;; by hashed key, so accounts and slots the state has never seen need no preimage. The
;;; expected root is the store's own root after the same writes made by address and slot.

(defun keccak-hex (n width) (octets->hex (clempo.keccak:keccak256 (integer->octets n width))))

(defun small-db ()
  (let ((db (store:make-statedb)))
    (store:db-begin-block db)
    (store:db-set-account db #xa1 1 100 trie:+empty-code-hash+)
    (loop for slot from 1 to 600 do (store:db-set-storage db #xa1 slot (* slot 3)))
    (store:db-set-account db #xb2 2 0 trie:+empty-code-hash+)
    (loop for slot from 1 to 600 do (store:db-set-storage db #xb2 slot 1))
    (store:db-commit-block db 1 (make-octets 32))
    db))

(defun input (accounts storages)
  (list :accounts (cons :object accounts) :storages (cons :object storages)))

(deftest state-root-of-new-accounts-and-slots ()
  (let* ((db (small-db))
         (expected (let ((db (small-db)))
                     (store:db-begin-block db)
                     (store:db-set-account db #xc3 1 5 trie:+empty-code-hash+)
                     (store:db-set-storage db #xc3 #xdeadbeef 7)
                     (store:db-set-storage db #xa1 99 3)
                     (store:db-set-storage db #xa1 1 0)
                     (store:db-delete-account db #xb2)
                     (octets->hex (store:db-state-root db))))
         (result (calls::hashed-post-state-updates
                  db (input (list (cons (keccak-hex #xc3 20) (json:jobject "nonce" 1 "balance" "0x5" "bytecode_hash" :null))
                                  (cons (keccak-hex #xb2 20) :null))
                            (list (cons (keccak-hex #xc3 20)
                                        (json:jobject "wiped" :false "storage" (json:jobject (keccak-hex #xdeadbeef 32) "0x7")))
                                  (cons (keccak-hex #xa1 20)
                                        (json:jobject "wiped" :false
                                                      "storage" (json:jobject (keccak-hex 99 32) "0x3" (keccak-hex 1 32) "0x0"))))))))
    (is= expected (svref result 0))
    ;; the destroyed account's storage trie is reported removed node by node (reth's
    ;; destroyed_storage_trie_nodes); the new account's storage has no stored branch
    (let ((tries (json:jget (svref result 1) "storage_tries")))
      (is (json:jget tries (keccak-hex #xb2 20)))
      (is (null (json:jget tries (keccak-hex #xc3 20)))))
    ;; the state database itself is unchanged
    (is= (octets->hex (store:db-root-hash (small-db))) (octets->hex (store:db-state-root db)))))

(deftest state-root-ignores-storage-of-a-missing-account ()
  ;; reth iterates the accounts of the state and the post-state: storage of an account in
  ;; neither adds no leaf (recorded: the reference's root is the unchanged one)
  (let ((db (small-db)))
    (is= (octets->hex (store:db-state-root db))
         (svref (calls::hashed-post-state-updates
                 db (input '() (list (cons (keccak-hex #xc3 20)
                                           (json:jobject "wiped" :false "storage" (json:jobject (keccak-hex 1 32) "0x7"))))))
                0))))

(deftest stored-branch-paths-match-the-walker ()
  (let* ((db (small-db)) (storage (store:db-storage-trie db #xa1))
         (paths (calls::stored-branch-paths storage)))
    (is (plusp (length paths)))
    (dolist (p paths) (is (tries::branch-stored-p storage p) "~A" p))))
