(in-package #:clempo.dev)

;;; The R5 corpus: requests to the reference node and its exact answers, as JSONL under
;;; ~/cache/clempo/rpc/. One line per case:
;;;   {"suite":S,"method":M,"request":"<raw body>","status":HTTP,"response":JSON-or-raw-string,
;;;    "class":"stable"|"live"|"node"}
;;; "live" answers depend on the moment (head-relative tags, recorded twice and different);
;;; "node" answers depend on the reference node itself (peers, client version, txpool).
;;; rpcdiff compares stable cases exactly and the others by shape.
;;;
;;;   clempo-dev rpc-probe  [-url U]            which method names the reference answers
;;;   clempo-dev rpc-record [-url U] [-suite S] [-threads N]   record suites (resumable)

(defun corpus-dir () (format nil "~A/cache/clempo/rpc/" (clempo.io:getenv "HOME")))

(defun corpus-path (name) (format nil "~A~A" (corpus-dir) name))

;;; Raw requests

(defun rpc-request-string (method params &key (id 1))
  (to-json-string* (clempo.json:jobject "jsonrpc" "2.0" "id" id "method" method
                                        "params" (coerce params 'simple-vector))))

(defun to-json-string* (v) (clempo.json:to-json-string v))

(defun post-raw (url body &key (retries 8))
  "POST BODY (a string) to URL. Returns (values status response) where response is parsed
JSON, or the raw text when it is not JSON."
  (loop for attempt from 0
        do (multiple-value-bind (status bytes)
               (handler-case (clempo.http:http-post url (string->octets body))
                 (error (e) (values 0 (string->octets (princ-to-string e)))))
             (let* ((text (octets->string bytes))
                    (json (handler-case (clempo.json:parse-json text) (error () text))))
               (if (and (< attempt retries)
                        (or (member status '(0 429 502 503 504))
                            (and (clempo.json:jobject-p json)
                                 (eql -32005 (clempo.json:jget* json "error" "code")))))
                   (sleep (min 30 (* 0.5 (expt 2 attempt))))
                   (return (values status json)))))))

;;; Method discovery

(defparameter *probe-methods*
  '("web3_clientVersion" "web3_sha3" "net_version" "net_listening" "net_peerCount" "rpc_modules"
    "eth_protocolVersion" "eth_syncing" "eth_coinbase" "eth_mining" "eth_hashrate" "eth_gasPrice"
    "eth_maxPriorityFeePerGas" "eth_blobBaseFee" "eth_feeHistory" "eth_accounts" "eth_blockNumber"
    "eth_chainId" "eth_getBalance" "eth_getStorageAt" "eth_getTransactionCount"
    "eth_getBlockTransactionCountByHash" "eth_getBlockTransactionCountByNumber"
    "eth_getUncleCountByBlockHash" "eth_getUncleCountByBlockNumber" "eth_getCode" "eth_sign"
    "eth_signTransaction" "eth_signTypedData" "eth_signTypedData_v4" "eth_sendTransaction"
    "eth_sendRawTransaction" "eth_sendRawTransactionSync" "eth_call" "eth_estimateGas"
    "eth_createAccessList" "eth_getBlockByHash" "eth_getBlockByNumber" "eth_getTransactionByHash"
    "eth_getTransactionByBlockHashAndIndex" "eth_getTransactionByBlockNumberAndIndex"
    "eth_getRawTransactionByHash" "eth_getRawTransactionByBlockHashAndIndex"
    "eth_getRawTransactionByBlockNumberAndIndex" "eth_getTransactionBySenderAndNonce"
    "eth_getTransactionReceipt" "eth_getBlockReceipts" "eth_getUncleByBlockHashAndIndex"
    "eth_getUncleByBlockNumberAndIndex" "eth_newFilter" "eth_newBlockFilter"
    "eth_newPendingTransactionFilter" "eth_uninstallFilter" "eth_getFilterChanges"
    "eth_getFilterLogs" "eth_getLogs" "eth_subscribe" "eth_unsubscribe" "eth_getProof"
    "eth_getAccount" "eth_getAccountInfo" "eth_getHeaderByHash" "eth_getHeaderByNumber"
    "eth_simulateV1" "eth_callMany" "eth_callBundle" "eth_sendBundle" "eth_sendPrivateTransaction"
    "eth_cancelPrivateTransaction" "eth_getWork" "eth_submitWork" "eth_submitHashrate"
    "eth_fillTransaction" "eth_config" "eth_getBlockAccessList" "eth_getBlockAccessListRaw"
    "eth_getBlockAccessListByBlockHash" "eth_getBlockAccessListByBlockNumber" "eth_getMultiProof"
    "eth_getTransactions" "eth_pendingTransactions" "eth_getCompilers" "eth_resend"
    "eth_sendRawTransactionConditional" "eth_getStorageValues" "eth_blockReceipts"
    "eth_getBlockRange" "eth_getBlockByTimestamp" "eth_getFilterChangesV2" "eth_feeHistoryV2"
    "debug_getRawHeader" "debug_getRawBlock" "debug_getRawTransaction" "debug_getRawTransactions"
    "debug_getRawReceipts" "debug_getBadBlocks" "debug_traceChain" "debug_traceBlock"
    "debug_traceBlockByHash" "debug_traceBlockByNumber" "debug_traceTransaction" "debug_traceCall"
    "debug_traceCallMany" "debug_executionWitness" "debug_executionWitnessByBlockHash"
    "debug_backtraceAt" "debug_blockProfile" "debug_chaindbCompact" "debug_chainConfig"
    "debug_chaindbProperty" "debug_codeByHash" "debug_cpuProfile" "debug_dbAncient"
    "debug_dbAncients" "debug_dbGet" "debug_dumpBlock" "debug_freeOSMemory" "debug_freezeClient"
    "debug_gcStats" "debug_getAccessibleState" "debug_getModifiedAccountsByHash"
    "debug_getModifiedAccountsByNumber" "debug_goTrace" "debug_intermediateRoots" "debug_memStats"
    "debug_mutexProfile" "debug_preimage" "debug_printBlock" "debug_seedHash"
    "debug_setBlockProfileRate" "debug_setGCPercent" "debug_setMutexProfileFraction"
    "debug_setTrieFlushInterval" "debug_stacks" "debug_standardTraceBadBlockToFile"
    "debug_standardTraceBlockToFile" "debug_startCPUProfile" "debug_startGoTrace"
    "debug_stateRootWithUpdates" "debug_stopCPUProfile" "debug_stopGoTrace" "debug_storageRangeAt"
    "debug_traceBadBlock" "debug_verbosity" "debug_vmodule" "debug_writeBlockProfile"
    "debug_writeMemProfile" "debug_writeMutexProfile" "debug_accountRange"
    "debug_getTrieFlushInterval" "debug_dumpState" "debug_getRawBlockByHash" "debug_setHead"
    "trace_call" "trace_callMany" "trace_rawTransaction" "trace_replayBlockTransactions"
    "trace_replayTransaction" "trace_block" "trace_filter" "trace_get" "trace_transaction"
    "trace_transactionOpcodeGas" "trace_blockOpcodeGas"
    "txpool_status" "txpool_inspect" "txpool_content" "txpool_contentFrom"
    "admin_addPeer" "admin_removePeer" "admin_addTrustedPeer" "admin_removeTrustedPeer"
    "admin_peers" "admin_nodeInfo" "admin_peerEvents" "admin_validatorKey" "admin_datadir"
    "admin_startHttp" "admin_stopHttp"
    "reth_getBalanceChangesInBlock" "reth_subscribeChainNotifications" "reth_subscribePersistedBlock"
    "reth_unsubscribeChainNotifications"
    "tempo_forkSchedule" "tempo_simulateV1" "tempo_getTransactions" "tempo_feeHistory"
    "consensus_getFinalization" "consensus_getLatest" "consensus_subscribe" "consensus_unsubscribe"
    "consensus_subscribeEvents"
    "token_getRoleHistory" "token_getTokens" "token_getTokensByAddress"
    "operator_peers" "dex_getOrders" "dex_getOrderbooks"
    "ots_getApiLevel" "ots_hasCode" "ots_getInternalOperations" "ots_getTransactionError"
    "ots_traceTransaction" "ots_getBlockDetails" "ots_getBlockDetailsByHash"
    "ots_getBlockTransactions" "ots_searchTransactionsBefore" "ots_searchTransactionsAfter"
    "ots_getTransactionBySenderAndNonce" "ots_getContractCreator"
    "erigon_getHeaderByNumber" "erigon_getBlockByTimestamp" "erigon_getLogsByHash"
    "engine_exchangeCapabilities" "engine_getClientVersionV1" "engine_newPayloadV3"
    "engine_forkchoiceUpdatedV3" "miner_setExtra" "miner_setGasPrice" "miner_setGasLimit"
    "mev_sendBundle" "mev_simBundle" "flashbots_getFeeRefundTotalsByRecipient"
    "validation_validateBuilderSubmissionV3" "personal_listAccounts" "personal_sign"
    "les_serverInfo" "clique_getSigners" "parity_pendingTransactions")
  "Method names probed with empty params. About 200 reth, geth, erigon, otterscan and Tempo names.")

(defparameter *probe-unsafe*
  '("admin_clearTxpool")
  "Answered by the reference but mutating with no params: never probed or recorded. The
reference serves them (admin_clearTxpool answers the number of transactions it dropped).")

(defun method-not-found-p (response)
  (and (clempo.json:jobject-p response)
       (eql -32601 (clempo.json:jget* response "error" "code"))))

(defun probe-methods (url)
  "Return (values answered missing): method names whose empty-params answer is not -32601."
  (let ((answered '()) (missing '()))
    (dolist (m *probe-methods*)
      (multiple-value-bind (status response) (post-raw url (rpc-request-string m '()))
        (declare (ignore status))
        (if (method-not-found-p response) (push m missing) (push m answered))))
    (values (nreverse answered) (nreverse missing))))

(defcommand "rpc-probe" (args)
  "which method names the reference answers: rpc-probe [-url U] (writes methods-<host>.json)"
  (let* ((url (arg args :url (clempo.jsonrpc:local-rpc-url)))
         (host (nth-value 1 (clempo.http:parse-url url))))
    (multiple-value-bind (answered missing) (probe-methods url)
      (let ((path (corpus-path (format nil "methods-~A.json" host))))
        (clempo.io:write-file-atomically
         path (string->octets
               (to-json-string* (clempo.json:jobject
                                 "url" url
                                 "clientVersion" (ignore-errors
                                                  (clempo.jsonrpc:rpc-call "web3_clientVersion" '() :url url))
                                 "probed" (length *probe-methods*)
                                 "answered" (coerce answered 'simple-vector)
                                 "unsafe" (coerce *probe-unsafe* 'simple-vector)
                                 "missing" (coerce missing 'simple-vector)))))
        (format t "~A: ~D of ~D probed names answered (+~D unsafe, not probed) -> ~A~%"
                url (length answered) (length *probe-methods*) (length *probe-unsafe*) path)))
    0))

(defun reference-methods ()
  "Union of the method names any recorded probe found answered, plus the unsafe ones."
  (let ((all '()))
    (dolist (p (directory (corpus-path "methods-*.json")))
      (let ((v (clempo.json:parse-json (clempo.io:read-file-string p))))
        (loop for m across (clempo.json:jget v "answered") do (pushnew m all :test #'string=))))
    (dolist (m *probe-unsafe*) (pushnew m all :test #'string=))
    (sort all #'string<)))

;;; Samples: blocks from every fork and transactions of every shape, chosen once by scanning
;;; the reference and kept in samples.json so the request lists are reproducible.

(defparameter *mainnet-forks*
  '(("T0" . 0) ("T1" . 4494230) ("T1B" . 6253936) ("T1C" . 8967990) ("T2" . 12290227)
    ("T3" . 16986044) ("T4" . 20636964) ("T5" . 24387406) ("T6" . 26629191) ("T7" . 29152418)
    ("T8" . 32434902) ("T9" . 33513646) ("T10" . 35816049) ("T11" . 38891821)
    ("T12" . 43957545) ("T13" . 47250841))
  "Mainnet activation heights (T1A activates with T1).")

(defun fork-at (n)
  (car (find-if (lambda (f) (<= (cdr f) n)) (reverse *mainnet-forks*))))

(defun jhex (obj key) (let ((v (clempo.json:jget obj key))) (and (stringp v) (hex->integer v))))

(defun tx-shape (tx)
  "A key naming what a transaction exercises in the JSON renderer."
  (let* ((type (clempo.json:jget tx "type"))
         (parts (list type)))
    (flet ((add (p) (push p parts)))
      (cond
        ((string= type "0x76")
         (let ((sig (clempo.json:jget tx "signature")))
           (if (clempo.json:jget sig "userAddress")
               (add (format nil "keychain-~A-~A" (clempo.json:jget sig "version")
                            (clempo.json:jget* sig "signature" "type")))
               (add (clempo.json:jget sig "type"))))
         (let ((ka (clempo.json:jget tx "keyAuthorization")))
           (when (clempo.json:jobject-p ka)
             (add (format nil "ka-~A" (or (clempo.json:jget* ka "signature" "type") "?")))
             (when (clempo.json:jobject-p (clempo.json:jget ka "limits")) (add "ka-limits"))))
         (when (clempo.json:jobject-p (clempo.json:jget tx "feePayerSignature")) (add "feepayer"))
         (when (eq :null (clempo.json:jget tx "feeToken")) (add "nofeetoken"))
         (let ((calls (clempo.json:jget tx "calls")))
           (when (> (length calls) 1) (add "batch"))
           (when (some (lambda (c) (eq :null (clempo.json:jget c "to"))) calls) (add "create")))
         (let ((nk (jhex tx "nonceKey")))
           (cond ((eql nk +u256-max+) (add "expiring"))
                 ((and nk (plusp nk)) (add "2d"))))
         (when (stringp (clempo.json:jget tx "validAfter")) (add "validafter"))
         (when (plusp (length (clempo.json:jget tx "aaAuthorizationList" #()))) (add "aaauth")))
        (t
         (when (eq :null (clempo.json:jget tx "to")) (add "create"))
         (when (equal (clempo.json:jget tx "from") "0x0000000000000000000000000000000000000000")
           (add "system"))
         (when (and (string= type "0x0") (null (clempo.json:jget tx "chainId"))) (add "pre155"))
         (when (plusp (length (clempo.json:jget tx "accessList" #()))) (add "accesslist"))
         (when (plusp (length (clempo.json:jget tx "authorizationList" #()))) (add "auth")))))
    (format nil "~{~A~^+~}" (reverse parts))))

(defun fetch-full-blocks (url from count)
  (clempo.jsonrpc:rpc-batch (loop for n from from below (+ from count)
                                  collect (list "eth_getBlockByNumber" (list (quantity->hex n) :true)))
                            :url url))

(defun choose-samples (url &key (windows 8) (span 100) (per-shape 2))
  "Scan WINDOWS runs of SPAN blocks in every fork and keep fork boundaries, the busiest and an
empty block per fork, and up to PER-SHAPE transactions of every shape."
  (let* ((head (hex->integer (clempo.jsonrpc:rpc-call "eth_blockNumber" '() :url url)))
         (blocks '()) (txs '()) (shapes (make-hash-table :test 'equal)))
    (flet ((keep-block (b why)
             (unless (find (jhex b "number") blocks :key (lambda (x) (jhex x "number")))
               (push (clempo.json:jobject "number" (clempo.json:jget b "number")
                                          "hash" (clempo.json:jget b "hash")
                                          "fork" (fork-at (jhex b "number"))
                                          "txs" (length (clempo.json:jget b "transactions"))
                                          "why" why)
                     blocks))))
      (loop for ((name . start) (nil . next)) on (append *mainnet-forks* (list (cons nil head)))
            while name
            do (let* ((end (min (or next head) (- head 1000)))
                      (busiest nil) (empty nil))
                 (when (< start end)
                   (dolist (b (fetch-full-blocks url start 2)) (keep-block b (format nil "~A activation" name)))
                   (loop for w from 0 below windows
                         for from = (+ start (floor (* w (- end start span)) windows))
                         do (dolist (b (fetch-full-blocks url from span))
                              (let ((n (length (clempo.json:jget b "transactions"))))
                                (when (or (null busiest) (> n (length (clempo.json:jget busiest "transactions"))))
                                  (setf busiest b))
                                (when (and (null empty) (<= n 1)) (setf empty b)))
                              (loop for tx across (clempo.json:jget b "transactions")
                                    for shape = (tx-shape tx)
                                    when (< (gethash shape shapes 0) per-shape)
                                      do (incf (gethash shape shapes 0))
                                         (push (clempo.json:jobject "shape" shape "fork" name
                                                                    "hash" (clempo.json:jget tx "hash")
                                                                    "tx" tx)
                                               txs))))
                   (when busiest (keep-block busiest (format nil "~A busiest" name)))
                   (when empty (keep-block empty (format nil "~A system-only" name)))
                   (log-line "samples: ~A done, ~D shapes so far" name (hash-table-count shapes))))))
    (dolist (tx txs)
      (let ((n (jhex (clempo.json:jget tx "tx") "blockNumber")))
        (unless (find n blocks :key (lambda (x) (jhex x "number")))
          (push (clempo.json:jobject "number" (quantity->hex n)
                                     "hash" (clempo.json:jget* tx "tx" "blockHash")
                                     "fork" (fork-at n) "why" (format nil "tx ~A" (clempo.json:jget tx "shape")))
                blocks))))
    (clempo.json:jobject "url" url "head" (quantity->hex head)
                         "blocks" (coerce (sort blocks #'< :key (lambda (b) (jhex b "number"))) 'simple-vector)
                         "txs" (coerce (reverse txs) 'simple-vector))))

(defun load-samples (url &key refresh)
  (let ((path (corpus-path "samples.json")))
    (if (and (probe-file path) (not refresh))
        (clempo.json:parse-json (clempo.io:read-file-string path))
        (let ((s (choose-samples url)))
          (clempo.io:write-file-atomically path (string->octets (to-json-string* s)))
          s))))

;;; Shape search: the reference scan above samples windows and misses rare shapes, so the
;;; archive is scanned locally (every block, decoded) for transaction shapes by wire tags and
;;; the lowest PER examples of each are appended to samples.json. Appending keeps every
;;; recorded request; rpc-record then records only the new ones.

(defun key-authorization-tags (ka)
  (let ((v (ignore-errors (clempo.signatures:parse-key-authorization ka))))
    (if (null v)
        (list "ka-unparsed")
        (append
         (list (format nil "ka-keytype~D" (clempo.signatures:key-authorization-key-type v))
               (format nil "ka-sig-~(~A~)"
                       (ignore-errors (clempo.signatures:signature-kind (second ka)))))
         (unless (eq :absent (clempo.signatures:key-authorization-allowed-calls v)) (list "ka-allowedcalls"))
         (when (clempo.signatures:key-authorization-witness v) (list "ka-witness"))
         (when (clempo.signatures:key-authorization-is-admin v) (list "ka-admin"))
         (when (clempo.signatures:key-authorization-account v) (list "ka-account"))))))

(defun wire-shape-tags (tx)
  "Shape tags of a decoded transaction, read from its wire fields."
  (let ((f (clempo.transactions:transaction-fields tx)))
    (case (clempo.transactions:transaction-type tx)
      (0 (append (unless (clempo.transactions:transaction-chain-id tx) (list "pre155"))
                 (when (zerop (length (nth 3 f))) (list "legacy-create"))))
      (1 (cons "0x1" (when (zerop (length (nth 4 f))) (list "0x1-create"))))
      (2 (when (zerop (length (nth 5 f))) (list "0x2-create")))
      (4 (list "0x4"))
      (#x76
       (let* ((sig (car (last f)))
              (kind (ignore-errors (clempo.signatures:signature-kind sig))))
         (append
          (list (if (member kind '(:keychain-v1 :keychain-v2))
                    (format nil "~(~A-~A~)" kind (clempo.signatures:signature-inner-kind sig))
                    (format nil "~(~A~)" kind)))
          (when (some (lambda (c) (zerop (length (first c)))) (nth 4 f)) (list "76-create"))
          (when (and (listp (nth 12 f)) (nth 12 f)) (list "aaauth"))
          (when (= (length f) 15) (key-authorization-tags (nth 13 f)))))))))

(defparameter *wanted-shape-tags*
  '("0x1" "0x1-create" "0x2-create" "0x4" "pre155" "legacy-create" "p256" "webauthn"
    "keychain-v1-secp256k1" "keychain-v1-p256" "keychain-v1-webauthn"
    "keychain-v2-secp256k1" "keychain-v2-p256" "keychain-v2-webauthn"
    "76-create" "aaauth" "ka-keytype0" "ka-keytype1" "ka-keytype2"
    "ka-sig-secp256k1" "ka-sig-p256" "ka-sig-webauthn"
    "ka-allowedcalls" "ka-witness" "ka-admin" "ka-account")
  "Wire shapes the corpus must cover; rpc-shape-samples reports those with no mainnet example.")

(defun scan-archive-shapes (&key (per 2) (threads 12) (to (clempo.archive:archive-contiguous-height)))
  "Hash table tag -> (count . lowest PER (block index) examples) over archive blocks 0..TO."
  (let ((shapes (make-hash-table :test 'equal))
        (lock (sb-thread:make-mutex :name "shapes"))
        (chunks (loop for c from 0 to to by clempo.archive:+chunk-blocks+ collect c)))
    (flet ((note (tag n i)
             (sb-thread:with-mutex (lock)
               (let ((e (or (gethash tag shapes) (setf (gethash tag shapes) (list 0)))))
                 (incf (car e))
                 (setf (cdr e) (let ((l (merge 'list (list (list n i)) (cdr e)
                                               (lambda (a b) (or (< (first a) (first b))
                                                                 (and (= (first a) (first b))
                                                                      (< (second a) (second b))))))))
                                 (subseq l 0 (min per (length l))))))))
           (next-chunk () (sb-thread:with-mutex (lock) (pop chunks))))
      (mapc #'sb-thread:join-thread
            (loop repeat threads
                  collect (sb-thread:make-thread
                           (lambda ()
                             (loop for first = (next-chunk) while first
                                   do (let ((c (clempo.archive:read-chunk first)))
                                        (loop for n from first
                                                below (min (1+ to) (+ first (clempo.archive:chunk-count c)))
                                              do (loop for tx in (clempo.blocks:block-transactions
                                                                  (clempo.blocks:decode-block
                                                                   (clempo.archive:chunk-block c n)))
                                                       for i from 0
                                                       do (dolist (tag (wire-shape-tags tx)) (note tag n i)))))))
                           :name "shape-scan"))))
    shapes))

(defun sample-code-hashes (url txs &key (limit 6))
  "Distinct non-empty code hashes of the call targets of TXS at their blocks."
  (let ((out '()))
    (dolist (x txs (nreverse out))
      (let ((to (or (txf x "to") (clempo.json:jget* (stx x) "calls" 0 "to"))))
        (when (and (stringp to) (< (length out) limit))
          (let ((code (clempo.jsonrpc:rpc-call "eth_getCode" (list to (txf x "blockNumber")) :url url)))
            (when (> (length code) 2)
              (let ((h (octets->hex (clempo.keccak:keccak256 (hex->octets code)))))
                (unless (find h out :key (lambda (c) (clempo.json:jget c "codeHash")) :test #'string=)
                  (push (jo "address" to "block" (txf x "blockNumber") "codeHash" h) out))))))))))

(defun extend-samples (url &key (per 2))
  "Append archive-found examples of missing wire shapes, their blocks, code hashes and a recent
archive block to samples.json. Returns the tags with no mainnet example."
  (let* ((s (load-samples url))
         (top (clempo.archive:archive-contiguous-height))
         (shapes (scan-archive-shapes :per per :to top))
         (txs (sample-txs s)) (blocks (sample-blocks s)) (added '()))
    (dolist (tag *wanted-shape-tags*)
      (unless (find (format nil "archive:~A" tag) txs
                    :key (lambda (x) (clempo.json:jget x "shape")) :test #'string=)
        (loop for (n i) in (cdr (gethash tag shapes))
              for tx = (clempo.jsonrpc:rpc-call "eth_getTransactionByBlockNumberAndIndex"
                                                (list (quantity->hex n) (quantity->hex i)) :url url)
              unless (find (clempo.json:jget tx "hash") txs
                           :key (lambda (x) (clempo.json:jget x "hash")) :test #'string=)
                do (let ((x (jo "shape" (format nil "archive:~A" tag) "fork" (fork-at n)
                                "hash" (clempo.json:jget tx "hash") "tx" tx)))
                     (setf txs (append txs (list x)))
                     (push x added)
                     (unless (find n blocks :key (lambda (b) (jhex b "number")))
                       (let ((b (clempo.jsonrpc:rpc-call "eth_getBlockByNumber" (list (quantity->hex n) :false)
                                                         :url url)))
                         (setf blocks (append blocks
                                              (list (jo "number" (quantity->hex n) "hash" (clempo.json:jget b "hash")
                                                        "fork" (fork-at n)
                                                        "txs" (length (clempo.json:jget b "transactions"))
                                                        "why" (format nil "tx archive:~A" tag)))))))))))
    (let* ((recent (or (clempo.json:jget s "recent")
                       (let ((b (clempo.jsonrpc:rpc-call "eth_getBlockByNumber" (list (quantity->hex top) :false)
                                                         :url url)))
                         (jo "number" (quantity->hex top) "hash" (clempo.json:jget b "hash")))))
           (codes (or (clempo.json:jget s "codes") (coerce (sample-code-hashes url txs) 'simple-vector)))
           (out (jo "url" (clempo.json:jget s "url") "head" (clempo.json:jget s "head")
                    "blocks" (coerce blocks 'simple-vector) "txs" (coerce txs 'simple-vector)
                    "recent" recent "codes" codes)))
      (clempo.io:write-file-atomically (corpus-path "samples.json") (string->octets (to-json-string* out))))
    (log-line "samples: ~D transactions appended" (length added))
    (loop for tag in *wanted-shape-tags*
          do (log-line "  ~22A ~9D on mainnet" tag (car (gethash tag shapes '(0)))))
    (remove-if (lambda (tag) (gethash tag shapes)) *wanted-shape-tags*)))

(defcommand "rpc-shape-samples" (args)
  "append archive-found rare transaction shapes to samples.json: rpc-shape-samples [-url U] [-per N]"
  (let ((missing (extend-samples (arg args :url (clempo.jsonrpc:local-rpc-url))
                                 :per (arg-int args :per 2))))
    (format t "shapes with no mainnet example: ~{~A~^ ~}~%" missing)
    0))

;;; Suites. Each returns a list of raw request bodies built from the samples.

(defvar *suites* '() "Ordered (name . generator); a generator takes the samples object.")

(defmacro defsuite (name (samples) doc &body body)
  `(progn
     (setf *suites* (append (remove ,name *suites* :key #'car :test #'string=)
                            (list (cons ,name (lambda (,samples) (declare (ignorable ,samples)) ,@body)))))
     (setf (get (intern ,name :keyword) 'suite-doc) ,doc)
     ,name))

(defun r (method &rest params) (rpc-request-string method params))

(defun jo (&rest kvs) (apply #'clempo.json:jobject kvs))
(defun jv (&rest items) (coerce items 'simple-vector))

(defun sample-blocks (s) (coerce (clempo.json:jget s "blocks") 'list))
(defun sample-txs (s) (coerce (clempo.json:jget s "txs") 'list))
(defun bnum (b) (clempo.json:jget b "number"))
(defun bhash (b) (clempo.json:jget b "hash"))
(defun stx (x) (clempo.json:jget x "tx"))
(defun txf (x key) (clempo.json:jget (stx x) key))
(defun hex- (h k) (quantity->hex (max 0 (- (hex->integer h) k))))

(defparameter +zero-hash+ "0x0000000000000000000000000000000000000000000000000000000000000000")
(defparameter +unknown-hash+ "0x1234567890123456789012345678901234567890123456789012345678901234")
(defparameter +far-block+ "0xfffffffff")
(defparameter +pathusd+ "0x20c0000000000000000000000000000000000000")
(defparameter +fee-manager+ "0xfeec000000000000000000000000000000000000")

(defun tx-call-object (x &key (gas t))
  "The call request equivalent of sample transaction X (first call for Tempo batches)."
  (let* ((tx (stx x))
         (call (if (string= (clempo.json:jget tx "type") "0x76") (svref (clempo.json:jget tx "calls") 0) tx))
         (to (clempo.json:jget call "to"))
         (fields (list "from" (clempo.json:jget tx "from")
                       "input" (clempo.json:jget call "input")
                       "value" (clempo.json:jget call "value"))))
    (when (stringp to) (setf fields (list* "to" to fields)))
    (when gas (setf fields (list* "gas" (clempo.json:jget tx "gas") fields)))
    (apply #'jo fields)))

(defsuite "envelope" (s)
  "JSON-RPC envelope: parse errors, batches, ids, versions, notifications"
  (list "" "{" "[" "[]" "[1]" "[1,2]" "null" "1" "\"x\"" "{}" "{\"jsonrpc\":\"2.0\"}"
        "{\"jsonrpc\":\"2.0\",\"id\":1}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\"}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":null}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":{}}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[1,2,3]}"
        "{\"jsonrpc\":\"2.0\",\"id\":\"abc\",\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1.5,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":-7,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":18446744073709551616,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":{\"a\":1},\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":[1],\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":true,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":2,\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":1,\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_nope\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ETH_CHAINID\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[],\"extra\":1}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]} "
        "  {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}x"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"id\":2,\"method\":\"eth_chainId\",\"params\":[]}"
        "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]}]"
        "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"net_version\",\"params\":[]}]"
        "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[]},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"eth_nope\",\"params\":[]},1,{\"jsonrpc\":\"2.0\",\"method\":\"net_version\",\"params\":[]}]"
        "[{\"jsonrpc\":\"2.0\",\"method\":\"eth_chainId\",\"params\":[]}]"
        "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"0x1\"]},{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"0x1\",false]}]"
        "[[]]" "[{}]" "[null]" "[\"x\"]"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"0x1\" false]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"0x1\",false,]}"
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"0x1\",\"\\u0041\"]}"))

(defsuite "static" (s)
  "methods with no chain-specific arguments"
  (append
   (mapcar (lambda (m) (r m))
           '("web3_clientVersion" "net_version" "net_listening" "net_peerCount" "rpc_modules"
             "eth_protocolVersion" "eth_syncing" "eth_coinbase" "eth_mining" "eth_hashrate"
             "eth_gasPrice" "eth_maxPriorityFeePerGas" "eth_blobBaseFee" "eth_accounts"
             "eth_blockNumber" "eth_chainId" "eth_config" "eth_getWork" "eth_pendingTransactions"
             "eth_newBlockFilter" "eth_newPendingTransactionFilter"
             "debug_chainConfig" "debug_getBadBlocks" "debug_chaindbCompact" "debug_dbAncients"
             "debug_freeOSMemory" "debug_gcStats" "debug_memStats"
             "txpool_status" "admin_nodeInfo" "admin_peers" "admin_validatorKey" "admin_peerEvents"
             "tempo_forkSchedule" "eth_subscribe" "eth_unsubscribe"
             "reth_subscribeChainNotifications" "reth_subscribePersistedBlock" "consensus_subscribe"))
   (list (r "web3_sha3" "0x") (r "web3_sha3" "0x68656c6c6f20776f726c64") (r "web3_sha3" "0x0")
         (r "web3_sha3" "68656c6c6f") (r "web3_sha3" 1) (r "web3_sha3" "0xzz")
         (r "eth_feeHistory" "0x4" "latest" (jv 25 75))
         (r "eth_feeHistory" 4 "latest" (jv))
         (r "eth_feeHistory" "0x0" "latest" (jv))
         (r "eth_feeHistory" "0x400" "0x100" (jv 10 50 90))
         (r "eth_feeHistory" "0x2" "0x1312d4d" (jv 50 25))
         (r "eth_feeHistory" "0x2" "0x1312d4d" (jv 101))
         (r "eth_feeHistory" "0x5" +far-block+ (jv))
         (r "eth_feeHistory" "0x3" "earliest" :null)
         (r "eth_feeHistory" "0x10" "0x2faf080" (jv 1 2.5 99.9))
         (r "eth_feeHistory" "0x10" "0x2faf080")
         (r "eth_getUncleCountByBlockNumber" "0x0")
         (r "eth_submitHashrate" "0x1" +zero-hash+)
         (r "eth_submitWork" "0x0000000000000001" +zero-hash+ +zero-hash+)
         (r "eth_sign" +pathusd+ "0x00")
         (r "eth_accounts" 1)
         (r "eth_chainId" "extra" "params")
         (r "net_version" :null)
         (r "debug_seedHash" "0x0") (r "debug_seedHash" "0x1312d4d")
         (r "debug_chaindbProperty" "leveldb.stats")
         (r "debug_dbAncient" "headers" "0x1")
         (r "debug_setGCPercent" "0x64") (r "debug_setGCPercent" 100)
         (r "debug_dbGet" "0x00") (r "debug_dbGet" "0x")
         (r "debug_preimage" +zero-hash+)
         (r "debug_codeByHash" +zero-hash+)
         (r "debug_codeByHash" "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
         (r "debug_codeByHash" "0x00")
         (r "txpool_contentFrom" +pathusd+)
         (r "admin_addPeer" "enode://bad") (r "admin_removePeer" "enode://bad")
         (r "eth_uninstallFilter" "0x1") (r "eth_getFilterChanges" "0x1") (r "eth_getFilterLogs" "0x1")
         (r "eth_getFilterChanges" "0xzz") (r "eth_uninstallFilter" 1)
         (r "eth_sendRawTransaction" "0x") (r "eth_sendRawTransaction" "0x00")
         (r "eth_sendRawTransaction" "0x02c0") (r "eth_sendRawTransaction" "0x76c0")
         (r "eth_sendRawTransactionSync" "0x")
         (r "eth_sendTransaction" (jo "from" +pathusd+))
         (r "eth_signTransaction" (jo "from" +pathusd+))
         (r "eth_signTypedData" +pathusd+ (jo))
         (r "trace_rawTransaction" "0x" (jv "trace"))
         (r "trace_rawTransaction" "0x00" (jv "trace"))
         (r "debug_traceBlock" "0x" (jo))
         (r "debug_traceBlock" "0xc0" (jo))
         (r "debug_traceBadBlock" +zero-hash+ (jo))
         (r "debug_standardTraceBlockToFile" +zero-hash+ (jo))
         (r "debug_standardTraceBadBlockToFile" +zero-hash+ (jo))
         (r "debug_getModifiedAccountsByNumber" "0x1" "0x0")
         (r "debug_getModifiedAccountsByHash" +unknown-hash+)
         (r "debug_setHead" "zz")
         (r "debug_setTrieFlushInterval" "zz")
         (r "debug_accountRange" "0x1" +zero-hash+ 1 :true :true :true)
         (r "debug_dumpBlock" "0x1")
         (r "debug_printBlock" "0x1")
         (r "debug_getAccessibleState" "0x1" "0x2")
         (r "debug_storageRangeAt" +zero-hash+ 0 +pathusd+ +zero-hash+ 1)
         (r "debug_intermediateRoots" +unknown-hash+ :null)
         (r "debug_executionWitness" +far-block+)
         (r "debug_executionWitnessByBlockHash" +unknown-hash+)
         (r "debug_stateRootWithUpdates" (jo) :null)
         (r "eth_getMultiProof" (jv) "latest")
         (r "eth_getTransactions" (jo))
         (r "eth_getTransactions" (jo "limit" 1))
         (r "token_getTokens" (jo)) (r "token_getTokens" (jo "limit" 2))
         (r "token_getTokensByAddress" (jo "address" +pathusd+))
         (r "token_getRoleHistory" (jo))
         (r "tempo_simulateV1" (jo) "latest")
         (r "eth_callBundle" (jo "txs" (jv) "blockNumber" "0x1" "stateBlockNumber" "0x0")))))

(defun block-requests (b)
  (let* ((n (bnum b)) (h (bhash b)) (nt (or (clempo.json:jget b "txs") 0)) (last (quantity->hex (max 0 (1- nt)))))
    (list (r "eth_getBlockByNumber" n :false) (r "eth_getBlockByNumber" n :true)
          (r "eth_getBlockByHash" h :false) (r "eth_getBlockByHash" h :true)
          (r "eth_getHeaderByNumber" n) (r "eth_getHeaderByHash" h)
          (r "eth_getBlockTransactionCountByNumber" n) (r "eth_getBlockTransactionCountByHash" h)
          (r "eth_getUncleCountByBlockNumber" n) (r "eth_getUncleCountByBlockHash" h)
          (r "eth_getUncleByBlockNumberAndIndex" n "0x0") (r "eth_getUncleByBlockHashAndIndex" h "0x0")
          (r "eth_getBlockReceipts" n) (r "eth_getBlockReceipts" h)
          (r "eth_getTransactionByBlockNumberAndIndex" n "0x0")
          (r "eth_getTransactionByBlockNumberAndIndex" n last)
          (r "eth_getTransactionByBlockHashAndIndex" h last)
          (r "eth_getTransactionByBlockNumberAndIndex" n (quantity->hex nt))
          (r "eth_getRawTransactionByBlockNumberAndIndex" n last)
          (r "eth_getRawTransactionByBlockHashAndIndex" h "0x0")
          (r "debug_getRawHeader" n) (r "debug_getRawBlock" n) (r "debug_getRawReceipts" n)
          (r "debug_getRawTransactions" n) (r "debug_getRawHeader" h) (r "debug_getRawBlock" h)
          (r "debug_getRawReceipts" h) (r "debug_getRawTransactions" h)
          (r "eth_getLogs" (jo "fromBlock" n "toBlock" n))
          (r "eth_getLogs" (jo "blockHash" h))
          (r "eth_feeHistory" "0x3" n (jv 10 50 90)))))

(defsuite "blocks" (s)
  "block, header, body, receipt and raw methods over the sample blocks"
  (append
   (loop for b in (sample-blocks s) append (block-requests b))
   (list (r "eth_getBlockByNumber" "0x0" :true) (r "eth_getBlockByNumber" "earliest" :false)
         (r "eth_getBlockByNumber" "0x1" :true) (r "eth_getBlockByNumber" "0x01" :false)
         (r "eth_getBlockByNumber" +far-block+ :false) (r "eth_getBlockByHash" +unknown-hash+ :false)
         (r "eth_getBlockByNumber" "latest" :false) (r "eth_getBlockByNumber" "pending" :false)
         (r "eth_getBlockByNumber" "safe" :false) (r "eth_getBlockByNumber" "finalized" :false)
         (r "eth_getHeaderByNumber" +far-block+) (r "eth_getHeaderByHash" +unknown-hash+)
         (r "eth_getBlockReceipts" +far-block+) (r "eth_getBlockReceipts" +unknown-hash+)
         (r "eth_getBlockReceipts" "latest") (r "eth_getBlockReceipts" "pending")
         (r "eth_getBlockTransactionCountByNumber" +far-block+)
         (r "eth_getBlockTransactionCountByHash" +unknown-hash+)
         (r "eth_getBlockTransactionCountByNumber" "pending")
         (r "eth_getUncleCountByBlockNumber" +far-block+) (r "eth_getUncleCountByBlockHash" +unknown-hash+)
         (r "eth_getTransactionByBlockNumberAndIndex" +far-block+ "0x0")
         (r "eth_getTransactionByBlockHashAndIndex" +unknown-hash+ "0x0")
         (r "eth_getRawTransactionByBlockNumberAndIndex" +far-block+ "0x0")
         (r "debug_getRawHeader" +far-block+) (r "debug_getRawBlock" +far-block+)
         (r "debug_getRawReceipts" +far-block+) (r "debug_getRawTransactions" +far-block+)
         (r "debug_getRawHeader" +unknown-hash+) (r "debug_getRawBlock" +unknown-hash+)
         (r "debug_getRawReceipts" +unknown-hash+) (r "debug_getRawTransactions" +unknown-hash+)
         (r "debug_getRawBlock" "earliest") (r "debug_getRawHeader" "0x0")
         ;; parameter shapes
         (r "eth_getBlockByNumber" "0x1") (r "eth_getBlockByNumber" "1" :false)
         (r "eth_getBlockByNumber" 1 :false) (r "eth_getBlockByNumber" "0x" :false)
         (r "eth_getBlockByNumber" "0x10000000000000000" :false) (r "eth_getBlockByNumber" "0xg" :false)
         (r "eth_getBlockByNumber" :null :false) (r "eth_getBlockByNumber" "0x1" "true")
         (r "eth_getBlockByNumber" "0x1" 1) (r "eth_getBlockByNumber" "0x1" :null)
         (r "eth_getBlockByNumber" (jo "blockNumber" "0x1") :false)
         (r "eth_getBlockByNumber" "Latest" :false) (r "eth_getBlockByNumber" "0X1" :false)
         (r "eth_getBlockByNumber" " 0x1" :false) (r "eth_getBlockByNumber" "-0x1" :false)
         (r "eth_getBlockByNumber" "0x1" :false 3)
         (r "eth_getBlockByHash" "0x00" :false) (r "eth_getBlockByHash" "0x1" :false)
         (r "eth_getBlockByHash" (subseq +unknown-hash+ 0 65) :false)
         (r "eth_getBlockByHash" (concatenate 'string +unknown-hash+ "00") :false)
         (r "eth_getBlockByHash" (string-upcase +unknown-hash+ :start 2) :false)
         (r "eth_getBlockByHash" "latest" :false) (r "eth_getBlockByHash" 1 :false)
         (r "eth_getBlockByHash" (subseq +unknown-hash+ 2) :false)
         (r "eth_getBlockByHash" (jo "blockHash" +unknown-hash+) :false)
         (r "eth_getBlockReceipts" (jo "blockHash" +unknown-hash+))
         (r "eth_getBlockReceipts" (jo "blockNumber" "0x1"))
         (r "eth_getBlockReceipts" (jo "blockHash" +unknown-hash+ "requireCanonical" :true))
         (r "eth_getBlockReceipts" 1) (r "eth_getBlockReceipts" "0x")
         (r "eth_getTransactionByBlockNumberAndIndex" "0x1" "0x") (r "eth_getTransactionByBlockNumberAndIndex" "0x1" 0)
         (r "eth_getTransactionByBlockNumberAndIndex" "0x1" "0x01")
         (r "eth_getTransactionByBlockNumberAndIndex" "0x1" "0x10000000000000000")
         (r "eth_getTransactionByBlockNumberAndIndex" "0x1")
         (r "debug_getRawHeader" "latest") (r "debug_getRawBlock" (jo "blockNumber" "0x1"))
         (r "debug_getRawBlock" 1))))

(defun tx-requests (x)
  (let* ((tx (stx x)) (h (clempo.json:jget tx "hash")) (n (clempo.json:jget tx "blockNumber"))
         (bh (clempo.json:jget tx "blockHash")) (i (clempo.json:jget tx "transactionIndex")))
    (list (r "eth_getTransactionByHash" h) (r "eth_getRawTransactionByHash" h)
          (r "eth_getTransactionReceipt" h) (r "debug_getRawTransaction" h)
          (r "eth_getTransactionByBlockNumberAndIndex" n i) (r "eth_getTransactionByBlockHashAndIndex" bh i)
          (r "eth_getRawTransactionByBlockNumberAndIndex" n i)
          (r "eth_getTransactionBySenderAndNonce" (clempo.json:jget tx "from") (clempo.json:jget tx "nonce")))))

(defsuite "txs" (s)
  "transaction, raw transaction and receipt methods over every sampled transaction shape"
  (append
   (loop for x in (sample-txs s) append (tx-requests x))
   (list (r "eth_getTransactionByHash" +unknown-hash+) (r "eth_getTransactionReceipt" +unknown-hash+)
         (r "eth_getRawTransactionByHash" +unknown-hash+) (r "debug_getRawTransaction" +unknown-hash+)
         (r "eth_getTransactionByHash" "0x00") (r "eth_getTransactionReceipt" "0x12")
         (r "eth_getTransactionByHash" 1) (r "eth_getTransactionByHash")
         (r "eth_getTransactionByHash" (subseq +unknown-hash+ 2))
         (r "eth_getTransactionByHash" (concatenate 'string +unknown-hash+ "ff"))
         (r "eth_getTransactionReceipt" :null) (r "eth_getTransactionReceipt" (jv +unknown-hash+))
         (r "eth_getTransactionBySenderAndNonce" +pathusd+ "0x0")
         (r "eth_getTransactionBySenderAndNonce" +pathusd+ "0x1000000")
         (r "eth_getTransactionBySenderAndNonce" "0x12" "0x0")
         (r "eth_getTransactionBySenderAndNonce" +pathusd+))))

(defun log-filters (s)
  "eth_getLogs filters over the sample blocks that have logs, by address and topics."
  (let ((out '()))
    (dolist (x (sample-txs s) (nreverse out))
      (let ((n (txf x "blockNumber")) (to (or (txf x "to") (clempo.json:jget* (stx x) "calls" 0 "to"))))
        (when (stringp to)
          (push (jo "fromBlock" (hex- n 50) "toBlock" n "address" to) out)
          (push (jo "fromBlock" (hex- n 5) "toBlock" n "address" (jv to +pathusd+)) out))
        (push (jo "fromBlock" (hex- n 20) "toBlock" n
                  "topics" (jv "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"))
              out)
        (push (jo "fromBlock" (hex- n 3) "toBlock" n
                  "topics" (jv :null (jv (format nil "0x000000000000000000000000~A"
                                                 (subseq (txf x "from") 2)))))
              out)))))

(defsuite "logs" (s)
  "eth_getLogs ranges, address and topic filters, range errors"
  (append
   (mapcar (lambda (f) (r "eth_getLogs" f)) (log-filters s))
   (list (r "eth_getLogs" (jo "fromBlock" "0x1" "toBlock" "0x100000"))
         (r "eth_getLogs" (jo "fromBlock" "0x1" "toBlock" "0x186a0"))
         (r "eth_getLogs" (jo "fromBlock" "0x10" "toBlock" "0x1"))
         (r "eth_getLogs" (jo "blockHash" "0x00"))
         (r "eth_getLogs" (jo "blockHash" +unknown-hash+))
         (r "eth_getLogs" (jo "blockHash" +unknown-hash+ "fromBlock" "0x1"))
         (r "eth_getLogs" (jo "fromBlock" "0x1" "toBlock" "0x2" "foo" 1))
         (r "eth_getLogs" (jo "address" "0x12"))
         (r "eth_getLogs" (jo "fromBlock" "0x1312d00" "toBlock" "0x1312d10" "topics" (jv (jv))))
         (r "eth_getLogs" (jo "fromBlock" "0x1312d00" "toBlock" "0x1312d10" "topics" (jv :null :null :null :null :null)))
         (r "eth_getLogs" (jo "fromBlock" "0x1312d00" "toBlock" "0x1312d10" "topics" (jv "0x12")))
         (r "eth_getLogs" (jo "fromBlock" "0x1312d00" "toBlock" "0x1312d10" "address" (jv)))
         (r "eth_getLogs" (jo "fromBlock" "earliest" "toBlock" "0x5"))
         (r "eth_getLogs" (jo "fromBlock" +far-block+ "toBlock" +far-block+))
         (r "eth_getLogs" (jo "fromBlock" "0x1312d00"))
         (r "eth_getLogs" (jo "toBlock" "0x5"))
         (r "eth_getLogs" (jo))
         (r "eth_getLogs" (jo "fromBlock" "latest" "toBlock" "latest"))
         (r "eth_getLogs" (jo "fromBlock" "pending" "toBlock" "pending"))
         (r "eth_getLogs" (jo "fromBlock" 1))
         (r "eth_getLogs" :null) (r "eth_getLogs" "0x1")
         (r "eth_newFilter" (jo "fromBlock" "0x1" "toBlock" "0x2"))
         (r "eth_newFilter" (jo "foo" 1)))))

(defun state-addresses (x)
  (remove-duplicates
   (remove-if-not #'stringp
                  (list (txf x "from") (txf x "to") (clempo.json:jget* (stx x) "calls" 0 "to")
                        (let ((ft (txf x "feeToken"))) (and (stringp ft) ft))))
   :test #'string-equal))

(defsuite "state" (s)
  "balances, nonces, code, storage, accounts and proofs at and before sample transactions"
  (append
   (loop for x in (sample-txs s)
         for n = (txf x "blockNumber")
         append (loop for a in (state-addresses x)
                      append (loop for at in (list n (hex- n 1))
                                   append (list (r "eth_getBalance" a at) (r "eth_getTransactionCount" a at)
                                                (r "eth_getCode" a at) (r "eth_getStorageAt" a "0x0" at)
                                                (r "eth_getAccount" a at) (r "eth_getAccountInfo" a at))))
         append (list (r "eth_getStorageAt" +pathusd+
                         (format nil "0x~(~64,'0x~)" 9) n)
                      (r "eth_getProof" (txf x "from") (jv "0x0") n)
                      (r "eth_getStorageValues" (jo +pathusd+ (jv "0x0" "0x8")) n)))
   (list (r "eth_getBalance" +pathusd+ "latest") (r "eth_getBalance" +pathusd+)
         (r "eth_getBalance" +pathusd+ "earliest") (r "eth_getBalance" +pathusd+ +far-block+)
         (r "eth_getBalance" +pathusd+ (jo "blockHash" +unknown-hash+))
         (r "eth_getBalance" +pathusd+ (jo "blockNumber" "0x1"))
         (r "eth_getBalance" +pathusd+ (jo "blockHash" +unknown-hash+ "requireCanonical" :true))
         (r "eth_getBalance" "0x12" "0x1") (r "eth_getBalance" "20c0000000000000000000000000000000000000" "0x1")
         (r "eth_getBalance" "0x20C0000000000000000000000000000000000000" "0x1")
         (r "eth_getBalance" 1 "0x1") (r "eth_getBalance" +pathusd+ "pending")
         (r "eth_getCode" +pathusd+ "0x0") (r "eth_getCode" +fee-manager+ "0x1")
         (r "eth_getCode" "0x0000000000000000000000000000000000000001" "0x1")
         (r "eth_getStorageAt" +pathusd+ "0x" "0x1") (r "eth_getStorageAt" +pathusd+ "0x00" "0x1")
         (r "eth_getStorageAt" +pathusd+ 0 "0x1")
         (r "eth_getStorageAt" +pathusd+ (concatenate 'string +zero-hash+ "00") "0x1")
         (r "eth_getStorageAt" +pathusd+ "0x2" "0x1")
         (r "eth_getTransactionCount" +pathusd+ "0x1") (r "eth_getTransactionCount" +pathusd+ +far-block+)
         (r "eth_getProof" +pathusd+ (jv) "0x1") (r "eth_getProof" +pathusd+ (jv) "latest")
         (r "eth_getProof" +pathusd+ (jv "0x0" "0x1") "latest")
         (r "eth_getProof" +pathusd+ "0x0" "latest")
         (r "eth_getMultiProof" (jv (jo "address" +pathusd+ "storageKeys" (jv))) "latest")
         (r "eth_getAccount" +pathusd+ "latest") (r "eth_getAccountInfo" +pathusd+ "latest")
         (r "debug_codeByHash" "0x4d3e8d8a1ab5e5c3e3a5dfc0aca1c8b0b5bfd52bba3fd73e7b4e6a3a2db58ef9"))))

(defsuite "calls" (s)
  "eth_call, estimateGas, createAccessList, simulateV1 and callMany replaying sampled transactions"
  (append
   (loop for x in (sample-txs s)
         for n = (txf x "blockNumber")
         for parent = (hex- n 1)
         append (list (r "eth_call" (tx-call-object x) parent)
                      (r "eth_call" (tx-call-object x :gas nil) n)
                      (r "eth_estimateGas" (tx-call-object x :gas nil) parent)
                      (r "eth_createAccessList" (tx-call-object x) parent)
                      (r "eth_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv (tx-call-object x))))) parent)
                      (r "eth_callMany" (jv (jo "transactions" (jv (tx-call-object x))))
                         (jo "blockNumber" parent))))
   (list (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "data" "0x95d89b41") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "input" "0x70a08231000000000000000000000000feec000000000000000000000000000000000000") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "input" "0xdeadbeef") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03" "data" "0x95d89b41") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03") "latest")
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03"))
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03") +far-block+)
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03" "foo" 1) "0x1312d4d")
         (r "eth_call" (jo "to" "0x12") "0x1312d4d")
         (r "eth_call" (jo) "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "gas" "0x1") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "value" "0x1") "0x1312d4d")
         (r "eth_call" (jo "from" +fee-manager+ "to" +pathusd+ "input" "0xa9059cbb000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000f4240") "0x1312d4d")
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03") "0x1312d4d"
            (jo +pathusd+ (jo "balance" "0x1")))
         (r "eth_call" (jo "to" "0x00000000000000000000000000000000000000aa" "input" "0x")
            "0x1312d4d" (jo "0x00000000000000000000000000000000000000aa" (jo "code" "0x60016000526020601ff3")))
         (r "eth_call" (jo "to" +pathusd+ "input" "0x06fdde03") "0x1312d4d" :null (jo "number" "0x1"))
         (r "eth_call" (jo "input" "0x6001600052") "0x1312d4d")
         (r "eth_estimateGas" (jo "to" +pathusd+ "input" "0x06fdde03") "0x1312d4d")
         (r "eth_estimateGas" (jo "to" +pathusd+ "input" "0xdeadbeef") "0x1312d4d")
         (r "eth_estimateGas" (jo "from" +fee-manager+ "to" "0x0000000000000000000000000000000000000001" "value" "0x1") "0x1312d4d")
         (r "eth_estimateGas" (jo "to" +pathusd+ "input" "0x06fdde03"))
         (r "eth_createAccessList" (jo "to" +pathusd+ "input" "0x06fdde03") "0x1312d4d")
         (r "eth_simulateV1" (jo "blockStateCalls" (jv)) "0x1312d4d")
         (r "eth_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv (jo "to" +pathusd+ "input" "0x06fdde03")))
                                                    (jo "blockOverrides" (jo "number" "0x1312d60") "calls" (jv)))
                                 "validation" :false "traceTransfers" :true "returnFullTransactions" :true)
            "0x1312d4d")
         (r "eth_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv (jo "to" +pathusd+ "input" "0x06fdde03"))))
                                 "validation" :true)
            "0x1312d4d")
         (r "tempo_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv (jo "to" +pathusd+ "input" "0x06fdde03")))))
            "0x1312d4d")
         (r "eth_callMany" (jv (jo "transactions" (jv (jo "to" +pathusd+ "input" "0x06fdde03")
                                                      (jo "to" +pathusd+ "input" "0x18160ddd"))))
            (jo "blockNumber" "0x1312d4d"))
         (r "eth_fillTransaction" (jo "from" +fee-manager+ "to" +pathusd+ "input" "0x06fdde03")))))

(defparameter *trace-configs*
  (list (jo "tracer" "callTracer")
        (jo "tracer" "callTracer" "tracerConfig" (jo "onlyTopCall" :true))
        (jo "tracer" "callTracer" "tracerConfig" (jo "withLog" :true))
        (jo "tracer" "prestateTracer")
        (jo "tracer" "prestateTracer" "tracerConfig" (jo "diffMode" :true))
        (jo "tracer" "4byteTracer")
        (jo "tracer" "noopTracer")
        (jo "tracer" "flatCallTracer")
        (jo "tracer" "muxTracer" "tracerConfig" (jo "callTracer" (jo) "4byteTracer" (jo)))))

(defun small-tx-p (x) (< (hex->integer (txf x "gas")) 400000))

(defsuite "traces" (s)
  "debug_trace* with every tracer and the parity trace_* family over sampled transactions and blocks"
  (append
   (loop for x in (sample-txs s)
         for h = (txf x "hash")
         for n = (txf x "blockNumber")
         append (mapcar (lambda (c) (r "debug_traceTransaction" h c)) *trace-configs*)
         when (small-tx-p x)
           append (list (r "debug_traceTransaction" h (jo))
                        (r "debug_traceTransaction" h (jo "disableStack" :true "enableMemory" :true
                                                         "enableReturnData" :true "disableStorage" :true)))
         append (list (r "trace_transaction" h) (r "trace_get" h (jv "0x0"))
                      (r "trace_replayTransaction" h (jv "trace"))
                      (r "trace_replayTransaction" h (jv "stateDiff"))
                      (r "trace_transactionOpcodeGas" h)
                      (r "debug_traceCall" (tx-call-object x) (hex- n 1) (jo "tracer" "callTracer"))
                      (r "debug_traceCall" (tx-call-object x) (hex- n 1) (jo "tracer" "prestateTracer"))
                      (r "trace_call" (tx-call-object x) (jv "trace") (hex- n 1))))
   (loop for b in (sample-blocks s)
         for n = (bnum b)
         append (list (r "debug_traceBlockByNumber" n (jo "tracer" "callTracer"))
                      (r "debug_traceBlockByNumber" n (jo "tracer" "prestateTracer" "tracerConfig" (jo "diffMode" :true)))
                      (r "debug_traceBlockByHash" (bhash b) (jo "tracer" "callTracer"))
                      (r "trace_block" n)
                      (r "trace_replayBlockTransactions" n (jv "trace"))
                      (r "trace_blockOpcodeGas" n)
                      (r "reth_getBalanceChangesInBlock" (bhash b))
                      (r "debug_getModifiedAccountsByNumber" (hex- n 1) n)
                      (r "debug_intermediateRoots" (bhash b) :null)
                      (r "eth_getBlockAccessList" n)))
   (list (r "debug_traceTransaction" +unknown-hash+ (jo))
         (r "debug_traceTransaction" +unknown-hash+ (jo "tracer" "callTracer"))
         (r "debug_traceTransaction" +unknown-hash+ (jo "tracer" "nopeTracer"))
         (r "debug_traceTransaction" +unknown-hash+)
         (r "debug_traceBlockByNumber" +far-block+ (jo))
         (r "debug_traceBlockByHash" +unknown-hash+ (jo))
         (r "trace_transaction" +unknown-hash+) (r "trace_get" +unknown-hash+ (jv "0x0"))
         (r "trace_block" +far-block+) (r "trace_replayTransaction" +unknown-hash+ (jv "trace"))
         (r "trace_replayTransaction" +unknown-hash+ (jv "nope"))
         (r "trace_filter" (jo "fromBlock" "0x1312d4d" "toBlock" "0x1312d4d"))
         (r "trace_filter" (jo "fromBlock" "0x1312d4d" "toBlock" "0x1312d4f"
                               "toAddress" (jv "0x20c000000000000000000000b9537d11c60e8b50")))
         (r "trace_filter" (jo "fromBlock" "0x1312d4d" "toBlock" "0x1312d55" "after" 1 "count" 2))
         (r "trace_filter" (jo "fromBlock" "0x1" "toBlock" "0x100000"))
         (r "trace_callMany" (jv (jv (jo "to" +pathusd+ "input" "0x06fdde03") (jv "trace"))
                                 (jv (jo "to" +pathusd+ "input" "0x18160ddd") (jv "trace" "stateDiff")))
            "0x1312d4d")
         (r "debug_traceCallMany" (jv (jo "transactions" (jv (jo "to" +pathusd+ "input" "0x06fdde03"))))
            (jo "blockNumber" "0x1312d4d") (jo "tracer" "callTracer"))
         (r "eth_getBlockAccessList" +far-block+)
         (r "eth_getBlockAccessListRaw" "0x1312d4d")
         (r "eth_getBlockAccessListByBlockNumber" "0x1312d4d")
         (r "eth_getBlockAccessListByBlockHash" +unknown-hash+))))

(defsuite "consensus" (s)
  "consensus_* (served by the public RPC only)"
  (list (r "consensus_getLatest") (r "consensus_getFinalization" "latest")
        (r "consensus_getFinalization" (jo "height" 1000)) (r "consensus_getFinalization" (jo "height" "0x3e8"))
        (r "consensus_getFinalization" (jo "height" 49000000))
        (r "consensus_getFinalization" (jo "digest" +unknown-hash+))
        (r "consensus_getFinalization" "nope") (r "consensus_getFinalization")
        (r "consensus_getFinalization" (jo "foo" 1))))

(defsuite "success" (s)
  "success paths of debug_codeByHash, debug_executionWitness, debug_intermediateRoots,
debug_stateRootWithUpdates, debug_traceBlock, eth_getMultiProof and tempo_simulateV1. The state
methods are slow far behind the head, so they use the samples' recent archive block."
  (let* ((recent (clempo.json:jget s "recent"))
         (n (and recent (clempo.json:jget recent "number")))
         (h (and recent (clempo.json:jget recent "hash")))
         (raw (and n (octets->hex (clempo.archive:raw-block (hex->integer n)))))
         (call (jo "to" +pathusd+ "input" "0x06fdde03"))
         (holder (format nil "0x70a08231000000000000000000000000~A" (subseq +fee-manager+ 2))))
    (append
     (loop for c across (clempo.json:jget s "codes" #())
           for ch = (clempo.json:jget c "codeHash")
           append (list (r "debug_codeByHash" ch) (r "debug_codeByHash" ch (clempo.json:jget c "block"))))
     (when n
       (list (r "debug_executionWitness" n)
             (r "debug_executionWitnessByBlockHash" h)
             (r "debug_intermediateRoots" h :null)
             (r "debug_intermediateRoots" h (jo "tracer" "callTracer"))
             (r "debug_stateRootWithUpdates" (jo "accounts" (jo) "storages" (jo)) n)
             (r "debug_stateRootWithUpdates"
                (jo "accounts" (jo (octets->hex (clempo.keccak:keccak256 (hex->octets +pathusd+)))
                                   (jo "nonce" 1 "balance" "0x0" "bytecode_hash" :null))
                    "storages" (jo))
                n)
             (r "debug_traceBlock" raw (jo))
             (r "debug_traceBlock" raw (jo "tracer" "callTracer"))
             (r "debug_traceBlock" raw (jo "tracer" "prestateTracer" "tracerConfig" (jo "diffMode" :true)))
             (r "tempo_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv call)))) n)
             (r "tempo_simulateV1" (jo "blockStateCalls" (jv (jo "calls" (jv call (jo "to" +pathusd+ "input" holder))))
                                       "traceTransfers" :true "returnFullTransactions" :true)
                n)))
     (list (r "eth_getMultiProof" (jv (jv +pathusd+ (jv +zero-hash+))) "latest")
           (r "eth_getMultiProof" (jv (jv +pathusd+ (jv)) (jv +fee-manager+ (jv +zero-hash+))) "latest")))))

;;; Recording

(defparameter *node-methods*
  '("web3_clientVersion" "net_peerCount" "admin_nodeInfo" "admin_peers" "admin_validatorKey"
    "txpool_status" "txpool_inspect" "txpool_content" "txpool_contentFrom" "eth_pendingTransactions"
    "eth_syncing" "eth_newBlockFilter" "eth_newPendingTransactionFilter" "eth_newFilter"
    "eth_getTransactionBySenderAndNonce" "eth_fillTransaction")
  "Methods whose answers depend on the reference node itself (its peers, build, pool, filter ids).")

(defparameter *live-tags* '("\"latest\"" "\"pending\"" "\"safe\"" "\"finalized\""))

(defparameter *live-methods* '("consensus_getLatest" "eth_blockNumber")
  "Methods whose answers follow the head whatever their params.")

(defun request-method (body)
  (let ((v (ignore-errors (clempo.json:parse-json body))))
    (or (and (clempo.json:jobject-p v) (stringp (clempo.json:jget v "method")) (clempo.json:jget v "method"))
        (and (vectorp v) (not (stringp v)) "batch")
        "envelope")))

(defun case-class (body method a b)
  (cond ((member method *node-methods* :test #'string=) "node")
        ((and (clempo.json:jobject-p a) (eql 503 (clempo.json:jget* a "error" "code"))) "node")
        ((member method *live-methods* :test #'string=) "live")
        ((not (clempo.json:json-equal a b)) "live")
        ((some (lambda (tag) (search tag body)) *live-tags*) "live")
        (t "stable")))

(defun parallel-map (fn items threads)
  (let* ((v (coerce items 'simple-vector)) (out (make-array (length v)))
         (next 0) (lock (sb-thread:make-mutex :name "pmap")))
    (flet ((worker ()
             (loop (let ((i (sb-thread:with-mutex (lock)
                              (when (< next (length v)) (prog1 next (incf next))))))
                     (unless i (return))
                     (setf (svref out i) (funcall fn (svref v i)))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (max 1 threads) collect (sb-thread:make-thread #'worker :name "record"))))
    (coerce out 'list)))

(defun read-corpus-file (path)
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil) while line
          when (plusp (length line)) collect (clempo.json:parse-json line))))

(defun record-suite (name url &key (threads 8))
  (let* ((path (corpus-path (format nil "corpus-~A.jsonl" name)))
         (gen (cdr (assoc name *suites* :test #'string=)))
         (samples (load-samples url))
         (done (make-hash-table :test 'equal))
         (requests '()))
    (when (probe-file path)
      (dolist (c (read-corpus-file path)) (setf (gethash (clempo.json:jget c "request") done) t)))
    (dolist (body (funcall gen samples))
      (unless (gethash body done) (setf (gethash body done) t) (push body requests)))
    (setf requests (nreverse requests))
    (log-line "~A: ~D new requests" name (length requests))
    (let* ((first (parallel-map (lambda (b) (multiple-value-list (post-raw url b))) requests threads))
           (second (parallel-map (lambda (b) (multiple-value-list (post-raw url b))) requests threads)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create
                                :external-format :utf-8)
        (loop for body in requests
              for (status response) in first
              for (nil response2) in second
              for method = (request-method body)
              do (write-line (to-json-string*
                              (jo "suite" name "method" method "request" body "status" status
                                  "response" response "class" (case-class body method response response2)))
                             out)))
      (length requests))))

(defcommand "rpc-record" (args)
  "record the R5 corpus from the reference: rpc-record [-suite S] [-url U] [-threads N] [-resample]"
  (let ((url (arg args :url (clempo.jsonrpc:local-rpc-url)))
        (wanted (arg args :suite)))
    (when (arg-flag args :resample) (load-samples url :refresh t))
    (dolist (s *suites* 0)
      (when (or (null wanted) (string= wanted (car s)))
        (let ((suite-url (if (and (string= (car s) "consensus") (not (arg args :url)))
                             clempo.jsonrpc:+mainnet-rpc+ url)))
          (format t "~A: ~D recorded from ~A~%" (car s)
                  (record-suite (car s) suite-url :threads (if (string= (car s) "consensus") 1
                                                               (arg-int args :threads 8)))
                  suite-url))))))
