(defpackage #:clempo.rpc.calls
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:h #:clempo.headers) (#:executor #:clempo.executor) (#:blocks #:clempo.blocks)
                    (#:state #:clempo.state) (#:tx #:clempo.transactions) (#:store #:clempo.store)
                    (#:trie #:clempo.trie))
  (:documentation "Methods that execute requests on a state: eth_call, eth_estimateGas,
eth_createAccessList, eth_simulateV1, eth_callMany and tempo_simulateV1, and the replaying
debug_trace* and trace_* families.

Their parameters are read here with the reference's exact errors: Tempo's transaction request
(alloy's TransactionRequest flattened into TempoTransactionRequest, so errors in the inner
fields are reported after the closing brace), state and block overrides, simulate payloads,
bundles and tracing options. Each method then follows reth's order: the block, the checks that
depend only on the request, the state (CLEMPO.RPC.STATE:STATE-AT, which fails loudly for a
state the backend does not keep) and finally execution through the executor's call API
(clempo.executor begin-call-block / execute-request, agreed with the executor agent) or the
tracers (EvmAgent's tracing package); until those land the methods fail with an internal error naming the
missing piece.")
  (:export #:call-request #:tx-request #:state-override #:block-overrides #:simulate-payload
           #:bundles #:state-context #:tracing-options #:trace-types
           #:normalize-request
           ;; the execution helpers src/rpc/simulate.lisp builds on
           #:req-from #:req-to #:req-gas #:req-gas-price #:req-max-fee #:req-max-priority
           #:req-value #:req-nonce #:req-chain-id #:req-access-list #:req-authorizations
           #:req-type #:req-calls #:req-fee-token #:req-nonce-key
           #:request-input #:aa-request-p #:request-type #:request-plist #:request-gas
           #:call-env #:block-hash-fn #:apply-state-overrides #:check-state-overrides #:replay-tx
           #:run-request #:call-result #:revert-reason #:invalid-request-error #:error-text
           #:+invalid-transaction-errors+ #:*rpc-gas-cap* #:header-base-fee
           ;; for src/rpc/filter.lisp and fill.lisp
           #:defobject #:defvec #:q64 #:plain-u64 #:block-traces #:parity-maker #:parity-localized
           #:estimate-request-gas #:copy-call-request #:req-valid-before #:req-valid-after
           #:req-fee-payer-signature #:req-tempo-authorizations #:req-key-authorization
           #:req-account-simulation #:req-key-authorization-simulation #:req-key-type
           #:aa-sim-transaction))

(in-package #:clempo.rpc.calls)

;;; serde building blocks

(defun strip-position (message)
  "MESSAGE without serde_json's \" at line L column C\" suffix."
  (let* ((at (search " at line " message :from-end t))
         (column (and at (search " column " message :start2 at))))
    (flet ((digits-p (s) (and (plusp (length s)) (every #'digit-char-p s))))
      (if (and column
               (digits-p (subseq message (+ at 9) column))
               (digits-p (subseq message (+ column 8))))
          (subseq message 0 at)
          message))))

(defun prefixed-p (s prefix)
  (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(defparam q8 (src) "alloy quantity u8" (read-ruint src 8))
(defparam q64 (src) "alloy quantity u64" (read-ruint src 64))
(defparam q128 (src) "alloy quantity u128" (read-ruint src 128))
(defparam q256 (src) "alloy U256" (read-ruint src 256))

(defparam nonzero-q64 (src) "alloy quantity NonZeroU64"
  (let ((n (read-ruint src 64)))
    (when (zerop n) (serde:serde-fail-here src "expected non-zero quantity"))
    n))

(defparam hex-data (src) "alloy Bytes: a hex string (0x optional) or an array of u8"
  (case (serde:peek-kind src)
    (:string (let* ((s (serde:read-string src))
                    (h (if (or (prefixed-p s "0x") (prefixed-p s "0X")) (subseq s 2) s)))
               (if (or (oddp (length h)) (notevery (lambda (c) (digit-char-p c 16)) h))
                   (serde:serde-fail-here src "invalid value: string ~A, expected a valid hex string"
                                          (serde:rust-debug-string s))
                   (hex->octets h))))
    (:array (let ((bytes '()))
              (serde:do-array (i src)
                (push (serde:read-integer src 0 255 "u8") bytes))
              (coerce (nreverse bytes) 'octets)))
    (t (serde:invalid-type src "a variable number of bytes represented as a hex string, an array of u8, or raw bytes"))))

(defparam tx-kind (src) "alloy TxKind: an address, or null for a creation (:create)"
  (if (serde:read-null-p src) :create (read-param 'address src)))

(defparam hashes (src) "Vec<B256> as a vector (empty and absent differ)"
  (let ((out '())) (serde:do-array (i src) (push (read-param 'hash src) out)) (coerce (nreverse out) 'simple-vector)))

(defun read-raw (src) (serde:read-value src))
(defparam raw (src) "any JSON value, kept as clempo.json data" (read-raw src))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun one-of (names)
    (case (length names)
      (0 "there are no fields")
      (1 (format nil "expected `~A`" (first names)))
      (2 (format nil "expected `~A` or `~A`" (first names) (second names)))
      (t (format nil "expected one of ~{`~A`~^, ~}" names)))))

(defmacro defobject (name (&key expected deny-unknown) &body fields)
  "A reader for a serde-derived struct: each field is (KEY \"jsonName\" TYPE &key ALIASES
OPTIONAL REQUIRED). The reader returns a plist of the present fields. OPTIONAL fields (Rust
Option) treat null as absent; REQUIRED fields fail with \"missing field\" after the object."
  (let ((src (gensym "SRC")) (out (gensym "OUT")) (seen (gensym "SEEN")) (key (gensym "KEY")))
    `(defparam ,name (,src) ,(format nil "serde struct ~A" expected)
       (let ((,out '()) (,seen '()))
         (serde:do-object (,key ,src :expected ,expected)
           (cond
             ,@(loop for (kw json type . opts) in fields
                     collect `((member ,key ',(cons json (getf opts :aliases)) :test #'string=)
                               (when (member ,kw ,seen) (serde:serde-fail-after-key ,src "duplicate field `~A`" ,json))
                               (push ,kw ,seen)
                               ,(if (getf opts :optional)
                                    `(unless (serde:read-null-p ,src)
                                       (setf (getf ,out ,kw) (read-param ',type ,src)))
                                    `(setf (getf ,out ,kw) (read-param ',type ,src)))))
             (t ,(if deny-unknown
                     `(serde:serde-fail-after-key ,src "unknown field `~A`, ~A" ,key
                                                  ,(one-of (mapcar #'second fields)))
                     `(serde:skip-value ,src)))))
         ,@(loop for (kw json nil . opts) in fields
                 when (getf opts :required)
                   collect `(unless (member ,kw ,seen) (serde:serde-fail-here ,src "missing field `~A`" ,json)))
         ,out))))

(defmacro defvec (name element &optional (expected "a sequence"))
  (let ((src (gensym "SRC")) (out (gensym "OUT")) (i (gensym "I")))
    `(defparam ,name (,src) ,(format nil "Vec of ~(~A~)" element)
       (let ((,out '()))
         (serde:do-array (,i ,src :expected ,expected) (push (read-param ',element ,src) ,out))
         (nreverse ,out)))))

(defobject access-list-item (:expected "struct AccessListItem")
  (:address "address" address :required t)
  (:storage-keys "storageKeys" hashes :required t))
(defvec access-list access-list-item)

(defobject signed-authorization (:expected "struct SignedAuthorization")
  (:chain-id "chainId" q256 :required t)
  (:address "address" address :required t)
  (:nonce "nonce" q64 :required t)
  (:y-parity "yParity" q8 :aliases ("v") :required t)
  (:r "r" q256 :required t)
  (:s "s" q256 :required t))
(defvec signed-authorizations signed-authorization)
(defparam authorization-list (src) "Vec<SignedAuthorization> as a vector (empty and absent differ)"
  (coerce (read-param 'signed-authorizations src) 'simple-vector))

(defparam signature-type (src) "Tempo SignatureType (camelCase unit variants)"
  (let ((variants '("secp256k1" "p256" "webAuthn" "configurableAccount")))
    (case (serde:peek-kind src)
      (:string (let ((s (serde:read-string src)))
                 (or (and (member s variants :test #'string=) (intern (string-upcase s) :keyword))
                     (serde:serde-fail-here src "unknown variant `~A`, expected one of ~{`~A`~^, ~}" s variants))))
      (:object (serde:skip-value src) (serde:serde-fail-here src "invalid type: map, expected enum SignatureType"))
      (t (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "expected value")))))

;;; Flattened structs: serde collects the fields it does not know itself and deserializes the
;;; flattened inner struct from them after the closing brace; their errors carry no position
;;; of their own and are reported there.

(defun read-flattened (src expected known handle)
  "Walk the object at SRC. Keys in KNOWN are passed to HANDLE (key src) inline; the others are
returned as a list of (key . raw-text) in order, with the position after the object."
  (let ((rest '()))
    (unless (eq (serde:peek-kind src) :object) (serde:invalid-type src expected))
    (serde:do-object (key src :expected expected)
      (if (member key known :test #'string=)
          (funcall handle key src)
          (push (cons key (serde:read-content src)) rest)))
    (values (nreverse rest) (serde:src-pos src))))

(defun content-value (src end type raw)
  "Read RAW (the text of a buffered field) as TYPE, failing at END without the inner position."
  (handler-case (read-param type (serde:make-src raw))
    (serde:serde-error (e) (serde:serde-fail-at src end "~A" (strip-position (serde:serde-error-message e))))))

(defun read-content-fields (src end entries fields)
  "Deserialize the buffered ENTRIES against FIELDS ((key json type) ...) as a flattened struct
does: unknown keys are left for the next level. Returns (values plist leftover-entries)."
  (let ((out '()) (seen '()) (left '()))
    (dolist (e entries)
      (let ((f (find (car e) fields :key #'second :test #'string=)))
        (cond ((null f) (push e left))
              ((member (first f) seen) (serde:serde-fail-at src end "duplicate field `~A`" (second f)))
              (t (push (first f) seen)
                 (unless (string= (string-trim " " (cdr e)) "null")
                   (setf (getf out (first f)) (content-value src end (third f) (cdr e))))))))
    (values out (nreverse left))))

;;; Tempo's transaction request

(defstruct (call-request (:conc-name req-))
  "A TempoTransactionRequest: alloy's TransactionRequest fields, the input pair and Tempo's."
  from to gas-price max-fee max-priority max-blob-fee gas value nonce chain-id access-list type
  blob-hashes authorizations input data
  fee-token nonce-key (calls '()) key-type key-data key-id (tempo-authorizations '())
  key-authorization account-simulation key-authorization-simulation valid-before valid-after
  fee-payer-signature)

(defparameter +inner-request-fields+
  '((:from "from" address) (:to "to" tx-kind) (:gas-price "gasPrice" q128)
    (:max-fee "maxFeePerGas" q128) (:max-priority "maxPriorityFeePerGas" q128)
    (:max-blob-fee "maxFeePerBlobGas" q128) (:gas "gas" q64) (:value "value" q256)
    (:nonce "nonce" q64) (:chain-id "chainId" q64) (:access-list "accessList" access-list)
    (:type "type" q8) (:blob-hashes "blobVersionedHashes" hashes)
    (:authorizations "authorizationList" authorization-list))
  "alloy TransactionRequest's own fields, all Option.")

(defparameter +input-fields+ '((:input "input" hex-data) (:data "data" hex-data)))

(defparameter +tempo-request-fields+
  '(("feeToken" :fee-token address t) ("nonceKey" :nonce-key q256 t) ("calls" :calls tempo-calls nil)
    ("keyType" :key-type signature-type t) ("keyData" :key-data hex-data t) ("keyId" :key-id address t)
    ("aaAuthorizationList" :tempo-authorizations tempo-authorization-list nil)
    ("keyAuthorization" :key-authorization signed-key-authorization t)
    ("configurableAccountSimulation" :account-simulation account-simulation t)
    ("keyAuthorizationSimulation" :key-authorization-simulation account-simulation t)
    ("validBefore" :valid-before nonzero-q64 t) ("validAfter" :valid-after nonzero-q64 t)
    ("feePayerSignature" :fee-payer-signature signature t))
  "TempoTransactionRequest's own fields: (json key type optional-p).")

(defparam tx-request (src) "Tempo's TempoTransactionRequest; returns a CALL-REQUEST"
  (let ((tempo '()) (seen '()))
    (multiple-value-bind (rest end)
        (read-flattened src "struct TempoTransactionRequest" (mapcar #'first +tempo-request-fields+)
                        (lambda (key src)
                          (destructuring-bind (kw type optional) (rest (assoc key +tempo-request-fields+ :test #'string=))
                            (when (member kw seen) (serde:serde-fail-after-key src "duplicate field `~A`" key))
                            (push kw seen)
                            (unless (and optional (serde:read-null-p src))
                              (setf (getf tempo kw) (read-param type src))))))
      (multiple-value-bind (inner left) (read-content-fields src end rest +inner-request-fields+)
        (let ((input (read-content-fields src end left +input-fields+)))
          (apply #'make-call-request (append inner input tempo)))))))

(defparam tempo-call (src) "Tempo Call {to, value, input|data} (input flattened)"
  (let ((fields '()) (seen '()))
    (multiple-value-bind (rest end)
        (read-flattened src "struct Call" '("to" "value")
                        (lambda (key src)
                          (let ((kw (if (string= key "to") :to :value)))
                            (when (member kw seen) (serde:serde-fail-after-key src "duplicate field `~A`" key))
                            (push kw seen)
                            (setf (getf fields kw) (read-param (if (eq kw :to) 'tx-kind 'q256) src)))))
      (dolist (f '(:to :value))
        (unless (member f seen) (serde:serde-fail-at src end "missing field `~(~A~)`" f)))
      (let ((input (read-content-fields src end rest +input-fields+)))
        (list (getf fields :to) (getf fields :value) (or (getf input :input) (getf input :data) (make-octets 0)))))))

(defvec tempo-calls tempo-call)

;;; Overrides, simulate payloads, bundles

(defparam storage-map (src) "B256HashMap<B256>: ((slot . value) ...)"
  (let ((out '()))
    (serde:do-object (key src)
      (let ((slot (handler-case (read-param 'hash (serde:make-src (json:to-json-string key)))
                    (serde:serde-error (e) (serde:serde-fail-after-key src "~A" (strip-position (serde:serde-error-message e)))))))
        (push (cons (octets->integer slot) (octets->integer (read-param 'hash src))) out)))
    (nreverse out)))

(defobject account-override (:expected "struct AccountOverride" :deny-unknown t)
  (:balance "balance" q256 :optional t)
  (:nonce "nonce" q64 :optional t)
  (:code "code" hex-data :optional t)
  (:state "state" storage-map :optional t)
  (:state-diff "stateDiff" storage-map :optional t)
  (:move-precompile-to "movePrecompileToAddress" address :aliases ("MovePrecompileToAddress") :optional t))

(defparam state-override (src) "alloy StateOverride: ((address . account-override-plist) ...)"
  (let ((out '()))
    (serde:do-object (key src)
      (let ((a (handler-case (read-param 'address (serde:make-src (json:to-json-string key)))
                 (serde:serde-error (e) (serde:serde-fail-after-key src "~A" (strip-position (serde:serde-error-message e)))))))
        (setf out (cons (cons a (read-param 'account-override src)) (remove a out :key #'car)))))
    (nreverse out)))

(defparam block-hash-map (src) "BTreeMap<u64, B256>"
  (let ((out '()))
    (serde:do-object (key src)
      (let ((n (and (plusp (length key)) (every #'digit-char-p key) (parse-integer key))))
        (unless (and n (<= n +u64-max+))
          (serde:serde-fail-after-key src "invalid type: string ~A, expected u64" (serde:rust-debug-string key)))
        (push (cons n (read-param 'hash src)) out)))
    (nreverse out)))

(defobject block-overrides (:expected "struct BlockOverrides")
  (:number "number" q256 :aliases ("blockNumber") :optional t)
  (:difficulty "difficulty" q256 :optional t)
  (:time "time" q64 :aliases ("timestamp") :optional t)
  (:gas-limit "gasLimit" q64 :optional t)
  (:coinbase "coinbase" address :aliases ("feeRecipient") :optional t)
  (:random "random" hash :aliases ("prevRandao") :optional t)
  (:base-fee "baseFee" q256 :aliases ("baseFeePerGas") :optional t)
  (:blob-base-fee "blobBaseFee" q256 :optional t)
  (:beacon-root "beaconRoot" hash :aliases ("parentBeaconBlockRoot") :optional t)
  (:withdrawals "withdrawals" raw :optional t)
  (:block-hash "blockHash" block-hash-map :optional t))

(defvec tx-requests tx-request)

(defobject sim-block (:expected "struct SimBlock")
  (:block-overrides "blockOverrides" block-overrides :optional t)
  (:state-overrides "stateOverrides" state-override :optional t)
  (:calls "calls" tx-requests))
(defvec sim-blocks sim-block)

(defobject simulate-payload (:expected "struct SimulatePayload")
  (:block-state-calls "blockStateCalls" sim-blocks)
  (:trace-transfers "traceTransfers" bool)
  (:validation "validation" bool)
  (:return-full-transactions "returnFullTransactions" bool))

(defobject bundle (:expected "struct Bundle")
  (:transactions "transactions" tx-requests :required t)
  (:block-override "blockOverride" block-overrides :optional t))
(defvec bundles bundle)

(defparam transaction-index (src) "alloy TransactionIndex: -1 or null for all, else an index"
  (case (serde:peek-kind src)
    (:null (serde:read-null-p src) :all)
    (:number (let ((n (serde:read-number src)))
               (cond ((eql n -1) :all)
                     ((and (integerp n) (<= 0 n +u64-max+)) n)
                     (t (serde:serde-fail-here src "Invalid transaction index, expected `-1` or non-negative integer")))))
    (t (serde:invalid-type src "a transaction index"))))

(defobject state-context (:expected "struct StateContext")
  (:block-number "blockNumber" block-id :optional t)
  (:transaction-index "transactionIndex" transaction-index :optional t))

;;; Request normalization: reth's prepare_call_env up to the first state read, and the parts of
;;; normalize_transaction_request (rpc-convert) that need nothing but the request and block.

(defun aa-request-p (r)
  "TempoTransactionRequest::has_aa_fields: the request needs Tempo AA semantics."
  (or (req-calls r) (req-nonce-key r) (req-fee-token r) (req-tempo-authorizations r)
      (req-key-authorization r) (req-key-id r) (req-key-type r) (req-key-data r)
      (req-account-simulation r) (req-key-authorization-simulation r)
      (req-valid-before r) (req-valid-after r) (req-fee-payer-signature r)))

(defun request-input (r)
  "TransactionInput::try_into_unique_input."
  (let ((input (req-input r)) (data (req-data r)))
    (when (and input data (not (octets= input data)))
      (rpc-fail +invalid-params+ "both \"data\" and \"input\" are set and not equal. Please use \"input\" to pass transaction call data"))
    (or input data)))

(defun ensure-fees (r base-fee)
  "reth CallFees::ensure_fees; returns the execution gas price."
  (let ((gp (req-gas-price r)) (max-fee (req-max-fee r)) (prio (req-max-priority r)) (blob (req-max-blob-fee r)))
    (flet ((effective ()
             (if max-fee
                 (let ((p (or prio 0)))
                   (when (and (not (and (zerop max-fee) (zerop p))) (< max-fee base-fee))
                     (rpc-fail -32000 "max fee per gas less than block base fee"))
                   (when (< max-fee p) (rpc-fail -32003 "max priority fee per gas higher than max fee per gas"))
                   (min max-fee (+ base-fee p)))
                 (+ base-fee (or prio 0)))))
      (cond ((and (null max-fee) (null prio) (null blob)) (or gp 0))
            ((and (null gp) (null blob)) (effective))
            ((null gp)
             (prog1 (effective)
               (unless (plusp (length (req-blob-hashes r))) (rpc-fail -32003 "blob transaction missing blob hashes"))))
            (t (rpc-fail +invalid-params+ "both gasPrice and (maxFeePerGas or maxPriorityFeePerGas) specified"))))))

(defun normalize-request (r base-fee)
  "The request-only checks of reth's normalize_transaction_request. Returns the execution gas
price."
  (when (and (req-blob-hashes r) (zerop (length (req-blob-hashes r))))
    (rpc-fail -32003 "blob transaction missing blob hashes"))
  (request-input r)
  (ensure-fees r base-fee))

(defun check-state-overrides (overrides)
  "apply_state_overrides' request-only check."
  (loop for (address . o) in overrides
        when (and (getf o :state) (getf o :state-diff))
          do (rpc-fail +invalid-params+ (format nil "account ~A has both 'state' and 'stateDiff'" (addr address)))))

(defun convert-request (r base-fee)
  "Tempo's TryIntoTxEnv up to the first state-dependent step: an AA request first builds its
mock signature (a configurableAccount key needs its configuration witness), then both kinds
normalize. Requests carrying simulated configurable-account or key-authorization witnesses are
checked by the executor."
  (cond ((not (aa-request-p r)) (normalize-request r base-fee))
        ((or (req-account-simulation r) (req-key-authorization-simulation r) (req-key-authorization r)) nil)
        ((eq (req-key-type r) :configurableaccount)
         (rpc-fail +invalid-params+ "configurable account simulation requires a configuration witness"))
        (t (normalize-request r base-fee))))

;;; Blocks and execution

(defun call-block (id)
  "reth evm_env_at: the header the call executes on (pending executes on the head)."
  (let ((n (block-id-number (or id :latest))))
    (or (and n (<= n chain:*head*) (chain-block n))
        (header-not-found (or id :latest)))))

(defun header-base-fee (b) (or (cblock-base-fee b) 0))

(defun executor-call-api (what)
  "The executor's request API agreed with the executor agent."
  (list :begin (landed-function "CLEMPO.EXECUTOR" "BEGIN-CALL-BLOCK" what)
        :world (landed-function "CLEMPO.EXECUTOR" "CALL-BLOCK-WORLD" what)
        :execute (landed-function "CLEMPO.EXECUTOR" "EXECUTE-REQUEST" what)))

(defun prepare-call (request id what &key state-overrides block-overrides)
  "reth spawn_with_call_at up to execution: the block, the request checks and the state, in
reth's order (its state read cannot fail, so the request-only checks come first here).
Returns (values cblock state)."
  (let ((b (call-block id)))
    (check-state-overrides state-overrides)
    (convert-request request (or (getf block-overrides :base-fee) (header-base-fee b)))
    (let ((state (state-at (list :hash (cblock-hash b)))))
      (executor-call-api what)
      (values b state))))

(defun unsupported-execution (what)
  (internal-error (format nil "~A: request execution over clempo.executor is not wired yet" what)))

;;; Executing requests: reth prepare_call_env over the executor's overlay call block
;;; (clempo.executor begin-call-block / execute-request).

(defparameter *rpc-gas-cap* 50000000 "reth RPC_DEFAULT_GAS_CAP")

(defun call-env (b block-overrides gas-price)
  "The block environment a call runs in: the block's header env with BLOCK-OVERRIDES applied,
and a zero base fee when the request pays no gas price (reth prepare_call_env)."
  (let ((env (executor:header-block-env (cblock-header b))))
    (destructuring-bind (&key number time gas-limit coinbase base-fee random difficulty
                           blob-base-fee beacon-root withdrawals block-hash)
        block-overrides
      (when (or random difficulty blob-base-fee beacon-root withdrawals block-hash)
        (internal-error "block overrides of prevrandao, difficulty, blob fee, beacon root, withdrawals or block hashes are not supported yet"))
      (when number (setf (executor:block-env-number env) number))
      (when time (setf (executor:block-env-timestamp env) time))
      (when gas-limit (setf (executor:block-env-gas-limit env) gas-limit))
      (when coinbase (setf (executor:block-env-beneficiary env) coinbase))
      (when base-fee (setf (executor:block-env-base-fee env) base-fee)))
    (when (zerop gas-price) (setf (executor:block-env-base-fee env) 0))
    env))

(defun apply-state-overrides (world overrides)
  "reth apply_state_overrides on the overlay WORLD."
  (loop for (address . o) in overrides
        do (let ((account (state:get-account world address)))
             (when (getf o :move-precompile-to)
               (internal-error "movePrecompileToAddress overrides are not supported yet"))
             (when (getf o :balance) (state:set-balance world account (getf o :balance)))
             (when (getf o :nonce) (state:set-nonce world account (getf o :nonce)))
             (when (getf o :code) (state:set-code world account (getf o :code)))
             (when (getf o :state)
               (state:wipe-storage account)
               (loop for (slot . value) in (getf o :state) do (state:sstore world address slot value)))
             (loop for (slot . value) in (getf o :state-diff) do (state:sstore world address slot value)))))

(defun request-type (r)
  "alloy TransactionRequest::minimal_tx_type."
  (cond ((req-authorizations r) 4)
        ((or (req-max-fee r) (req-max-priority r)) 2)
        ((req-access-list r) 1)
        ((req-gas-price r) 0)
        (t 2)))

(defparameter +simulation-unique-id+
  (octets->integer (string->octets "TEMPO_RPC_SIMULATION_MPP_CONTEXT"))
  "RPC_SIMULATION_UNIQUE_TX_IDENTIFIER: the transaction identifier of every simulated request.")

(defun request-plist (r world gas price &optional env)
  "The executor's request plist for REQUEST R (Tempo's TryIntoTxEnv after create_txn_env):
an Ethereum request as normalize_transaction_request leaves it, a Tempo one (AA-REQUEST-P) as
its 0x76 transaction with a mock signature. The nonce comes from WORLD, as reth's
prepare_call_env drops the request's. ENV, the call's block environment, is needed for AA
requests."
  (if (aa-request-p r)
      (aa-request-plist r world gas price env)
      (let ((type (request-type r)) (from (or (req-from r) 0))
            (to (let ((to (req-to r))) (if (or (null to) (eq to :create)) nil to))))
        ;; a type-4 request needs a target (rpc-convert) and authorizations (evm2 EmptyAuthorizationList)
        (when (and (= type 4) (or (null to) (zerop (length (req-authorizations r)))))
          (rpc-fail -32003 "EIP-7702 authorization list has invalid fields"))
        (list :from from
              :to to
              :gas gas :type type
              :gas-price (when (member type '(0 1)) (or (req-gas-price r) price))
              :max-fee (unless (member type '(0 1)) (or (req-max-fee r) price))
              :max-priority (unless (member type '(0 1)) (or (req-max-priority r) 0))
              :value (or (req-value r) 0)
              :nonce (state:get-nonce world from)
              :chain-id (req-chain-id r)
              :input (or (request-input r) (make-octets 0))
              :access-list (access-list-plist r)
              :authorizations (authorization-items r)
              :unique-id +simulation-unique-id+))))

(defun authorization-items (r)
  "The request's EIP-7702 authorizationList as a type-4 transaction's decoded RLP items."
  (map 'list (lambda (a)
               (destructuring-bind (&key chain-id address nonce y-parity r s) a
                 (rlp-items (list chain-id (integer->octets address 20) nonce y-parity r s))))
       (req-authorizations r)))

(defun authorization-authorities (r chain-id)
  "evm2 AccessListInspector::with_excluded_from_tx: the recoverable authorities of the
request's EIP-7702 authorizations for CHAIN-ID."
  (loop for item in (authorization-items r)
        for auth-chain = (octets->integer (first item))
        for authority = (and (or (zerop auth-chain) (= auth-chain chain-id))
                             (/= (octets->integer (third item)) +u64-max+)
                             (handler-case (values (clempo.signatures:authorization-signer item))
                               (clempo.signatures:signature-error () nil)))
        when authority collect authority))

;;; Tempo (0x76) requests: TempoTransactionRequest::build_aa with create_mock_tempo_sig.

(defparameter +nonce-manager+ #x4e4f4e4345000000000000000000000000000000 "NONCE_PRECOMPILE_ADDRESS")
(defparameter +expiring-nonce-key+ (1- (ash 1 256)) "TEMPO_EXPIRING_NONCE_KEY")

(defun request-nonce (r world)
  "create_txn_env's nonce: a nonzero nonce key's from the NonceManager (nonces[from][key],
0 for the expiring key), else the account's."
  (let ((from (or (req-from r) 0)) (key (req-nonce-key r)))
    (flet ((word (x) (integer->octets x 32)))
      (cond ((or (null key) (zerop key)) (state:get-nonce world from))
            ((= key +expiring-nonce-key+) 0)
            (t (min +u64-max+
                    (state:sload world +nonce-manager+
                                 (octets->integer
                                  (clempo.keccak:keccak256
                                   (octets-concat (word key)
                                                  (clempo.keccak:keccak256 (octets-concat (word from) (word 0)))))))))))))

(defun mock-webauthn-signature (size)
  "mock_webauthn_signature: authenticator data and a clientDataJSON padded to SIZE bytes."
  (let* ((base "{\"type\":\"webauthn.get\",\"challenge\":\"\",\"origin\":\"\"}")
         (size (max size (+ 37 (length base))))
         (auth (fill (make-octets 37) #xff)))
    (setf (aref auth 32) 1)
    (octets-concat (octets-of 2) auth
                   (string->octets (format nil "{\"type\":\"webauthn.get\",\"challenge\":\"\",\"origin\":\"~A\"}"
                                           (make-string (- size 37 (length base)) :initial-element #\x)))
                   (make-octets 128))))

(defun mock-primitive-signature (type key-data)
  "create_mock_primitive_signature as wire bytes; KEY-DATA of 1, 2 or 4 bytes sizes WebAuthn data."
  (ecase type
    (:secp256k1 (octets-concat (make-octets 64) (octets-of 27)))
    (:p256 (octets-concat (octets-of 1) (make-octets 129)))
    (:webauthn (mock-webauthn-signature
                (min 8192 (if (and key-data (member (length key-data) '(1 2 4)))
                              (octets->integer key-data)
                              800))))))

(defun mock-signature (r from spec)
  "create_mock_tempo_sig: the request's key type as a primitive signature, wrapped in a
keychain signature (V2 from T1C) for an access key (keyId)."
  (let ((inner (mock-primitive-signature (or (req-key-type r) :secp256k1) (req-key-data r))))
    (if (req-key-id r)
        (octets-concat (octets-of (if (clempo.forks:fork>= spec :t1c) 4 3)) (integer->octets from 20) inner)
        inner)))

(defun call-items (calls)
  (mapcar (lambda (c) (list (if (first c) (integer->octets (first c) 20) (make-octets 0)) (second c) (third c)))
          calls))

(defun fee-payer (r fields)
  "TempoTransaction::recover_fee_payer over the unsigned FIELDS (the fee payer signature's
place holding the sender), or NIL when the request carries no fee payer signature."
  (let ((sig (req-fee-payer-signature r)))
    (when sig
      (or (and (<= (getf sig :s) (ash clempo.secp256k1:+secp256k1-n+ -1))
               (clempo.secp256k1:ecrecover-address
                (clempo.keccak:keccak256 (octets-concat (octets-of #x78) (clempo.rlp:rlp-encode fields)))
                (getf sig :r) (getf sig :s) (if (getf sig :y-parity) 1 0)))
          (rpc-fail -32003 "fee payer signature recovery failed")))))

(defun aa-calls (r &key create-default)
  "build_aa's calls: the request's calls, then its to/value/input as one more call. With
CREATE-DEFAULT (eth_simulateV1, which fills a missing to with Create) a request without calls
creates."
  (let ((to (req-to r)) (calls (req-calls r)))
    (mapcar (lambda (c) (list (if (eq (first c) :create) nil (first c)) (second c) (third c)))
            (append calls
                    (when (or (integerp to) (and create-default (null calls)))
                      (list (list to (or (req-value r) 0) (or (request-input r) (make-octets 0)))))))))

(defun aa-fields (r chain-id priority max-fee gas calls access-list nonce sponsor)
  "TempoTransaction's RLP fields without its signature, SPONSOR in the fee payer signature's place."
  (flet ((address-or-empty (a) (if a (integer->octets a 20) (make-octets 0))))
    (append (list chain-id priority max-fee gas (call-items calls)
                  (mapcar (lambda (e) (list (integer->octets (car e) 20)
                                            (mapcar (lambda (k) (integer->octets k 32)) (cdr e))))
                          access-list)
                  (or (req-nonce-key r) 0) nonce
                  (or (req-valid-before r) (make-octets 0)) (or (req-valid-after r) (make-octets 0))
                  (address-or-empty (req-fee-token r)) sponsor (req-tempo-authorizations r))
            (when (req-key-authorization r) (list (req-key-authorization r))))))

(defun aa-plist (r from gas max-fee priority nonce chain-id calls access-list fee-payer signature)
  (list :type #x76 :from from :gas gas :max-fee max-fee :max-priority priority
        :nonce nonce :chain-id chain-id :calls calls :access-list access-list
        :nonce-key (or (req-nonce-key r) 0) :fee-token (req-fee-token r)
        :valid-before (req-valid-before r) :valid-after (req-valid-after r)
        :tempo-authorizations (req-tempo-authorizations r)
        :key-authorization (req-key-authorization r)
        :fee-payer fee-payer :signature signature))

(defun aa-request-plist (r world gas price env)
  (let* ((from (or (req-from r) 0))
         (gp (or (req-gas-price r) (when (member (request-type r) '(0 1)) price)))
         (max-fee (or gp (req-max-fee r) price))
         (priority (or gp (req-max-priority r) 0))
         (calls (aa-calls r)))
    (unless calls (rpc-fail +invalid-params+ "Missing 'calls' or 'to' field for Tempo transaction."))
    (let* ((chain-id (or (req-chain-id r) (executor:block-env-chain-id env)))
           (nonce (request-nonce r world))
           (access-list (access-list-plist r))
           ;; the fee payer signs the request's gas as converted, before prepare_call_env caps it
           (fee-payer (fee-payer r (aa-fields r chain-id priority max-fee
                                              (or (req-gas r) (executor:block-env-gas-limit env))
                                              calls access-list nonce (integer->octets from 20)))))
      (list* :key-id (req-key-id r) :unique-id +simulation-unique-id+
             (aa-plist r from gas max-fee priority nonce chain-id calls access-list fee-payer
                       (mock-signature r from (executor:block-env-spec env)))))))

(define-constant +simulated-aa-signature+
  (octets-concat (integer->octets #x840cfc572845f5786e702984c2a582528cad4b49b2a10b9db1be7fca90058565 32)
                 (integer->octets #x25e7109ceb98168d95b09b18bbf6b685130e0562f233877d492b94eee0c5b6d1 32)
                 (octets-of 27))
  "TempoSignature::default(): alloy's test signature, which eth_simulateV1 signs AA requests with.")

(defun aa-sim-transaction (r gas nonce chain-id)
  "Tempo's TryIntoSimTx for an AA request whose NONCE, GAS and CHAIN-ID eth_simulateV1 has
filled (missing fee caps are 0): its 0x76 transaction signed with the default signature, and
the executor's request plist for it. Returns (values plist transaction)."
  ;; simulate_v1 fills fees by the inner request's type: a legacy or 2930 one gets no max fee
  (when (and (member (request-type r) '(0 1)) (null (req-max-fee r)))
    (rpc-fail +invalid-params+ "Failed to convert transaction into RPC response: Missing 'max_fee_per_gas' field for Tempo transaction."))
  (let* ((from (or (req-from r) 0))
         (max-fee (or (req-max-fee r) 0)) (priority (or (req-max-priority r) 0))
         (calls (aa-calls r :create-default t))
         (access-list (access-list-plist r))
         (sponsor (req-fee-payer-signature r))
         (fee-payer (fee-payer r (aa-fields r chain-id priority max-fee gas calls access-list nonce
                                            (integer->octets from 20))))
         (fields (append (aa-fields r chain-id priority max-fee gas calls access-list nonce
                                    (if sponsor
                                        (list (if (getf sponsor :y-parity) 1 0) (getf sponsor :r) (getf sponsor :s))
                                        (make-octets 0)))
                         (list +simulated-aa-signature+)))
         (tx (tx:decode-transaction (octets-concat (octets-of #x76) (clempo.rlp:rlp-encode fields)))))
    (values (list* :hash (octets->integer (tx:transaction-hash tx))
                   :unique-id (octets->integer (tx:transaction-unique-identifier tx from))
                   (aa-plist r from gas max-fee priority nonce chain-id calls access-list fee-payer
                             +simulated-aa-signature+))
            tx)))

(defun access-list-plist (r)
  "The request's access list as the executor takes it: ((address slot ...) ...)."
  (mapcar (lambda (item)
            (cons (getf item :address) (map 'list #'octets->integer (getf item :storage-keys))))
          (req-access-list r)))

(defparameter +invalid-transaction-errors+
  '(("intrinsic gas too low" -32000 "intrinsic gas too low" -38013)
    ("value transfer not allowed" -32003 "value transfer not allowed" -32003)
    ("fee cap below base fee" -32000 "max fee per gas less than block base fee" -38012)
    ("priority fee greater than max fee" -32003 "max priority fee per gas higher than max fee per gas" -32003)
    ("initcode too large" -32003 "max initcode size exceeded" -38025)
    ("sender has code" -32003 "sender is not an EOA" -38024)
    ("wrong chain id" -32000 "invalid chain ID" -32000))
  "clempo's invalid-transaction reasons -> reth's RpcInvalidTransactionError (code, message)
and eth_simulateV1's code.")

(defvar *invalid-context* nil
  "(request . block-env) of the request whose rejection INVALID-REQUEST-ERROR maps: Tempo's
messages quote the block timestamp and the request's validity window.")

(defun checksum-address (address)
  "EIP-55: alloy's Display of an Address."
  (let* ((hex (format nil "~(~40,'0x~)" address))
         (hash (octets->integer (clempo.keccak:keccak256 (string->octets hex)))))
    (with-output-to-string (s)
      (write-string "0x" s)
      (loop for c across hex for i from 0
            do (write-char (if (and (alpha-char-p c) (logbitp (- 255 (* 4 i)) hash)) (char-upcase c) c) s)))))

(defparameter +precompile-error-variants+
  '(("CLEMPO.KEYCHAIN" . "AccountKeychainError") ("CLEMPO.NONCE" . "NonceError")
    ("CLEMPO.TIP20" . "TIP20") ("CLEMPO.DEX" . "StablecoinDEX") ("CLEMPO.TIP403" . "TIP403RegistryError")
    ("CLEMPO.TIP20-FACTORY" . "TIP20Factory") ("CLEMPO.CHANNEL-RESERVE" . "TIP20ChannelReserveError"))
  "clempo precompile packages -> the TempoPrecompileError variant of their errors.")

(defun precompile-error-debug (reason)
  "The Debug of the TempoPrecompileError a \"precompile revert PKG::NAME ... 0xDATA\" REASON
names (\"AccountKeychainError(KeyNotFound(KeyNotFound))\"), for errors without fields; else NIL."
  (let* ((at (search "precompile revert " reason))
         (start (and at (+ at (length "precompile revert "))))
         (sep (and start (search "::" reason :start2 start)))
         (end (and sep (position #\Space reason :start sep)))
         (variant (and sep (cdr (assoc (subseq reason start sep) +precompile-error-variants+ :test #'string=))))
         (symbol (and variant (find-symbol (subseq reason (+ sep 2) end) (subseq reason start sep))))
         (canonical (and symbol (third (get symbol 'clempo.contract::error-signature)))))
    (when (and canonical (search "()" canonical))
      (let ((name (subseq canonical 0 (position #\( canonical))))
        (format nil "~A(~A(~A))" variant name name)))))

(defun reason-integer (reason prefix)
  (parse-integer reason :start (length prefix) :junk-allowed t))

(defun tempo-invalid-error (reason)
  "(values message data) of TempoInvalidTransaction's Display for the executor's REASON, or NIL."
  (destructuring-bind (&optional request . env) *invalid-context*
    (let ((now (and env (executor:block-env-timestamp env))))
      (flet ((starts (prefix) (and (>= (length reason) (length prefix)) (string= prefix reason :end2 (length prefix))))
             (has (part) (search part reason)))
        (cond
          ((starts "expiring nonce without valid_before") "expiring nonce transaction requires valid_before to be set")
          ((starts "expiring nonce must be zero") "expiring nonce transaction must have nonce == 0 before T12")
          ((has "EXPIRING-NONCE-REPLAY") "expiring nonce transaction already included")
          ((and (has "INVALID-EXPIRING-NONCE-EXPIRY") now request (req-valid-before request))
           (let ((before (req-valid-before request)))
             (if (<= before now)
                 (format nil "nonce manager error: expiring nonce transaction expired: valid_before (~D) <= block timestamp (~D)" before now)
                 (format nil "nonce manager error: expiring nonce valid_before (~D) too far in the future: must be within 30s of block timestamp (~D), max allowed is ~D"
                         before now (+ now 30)))))
          ((and (starts "nonce manager: ") (precompile-error-debug reason))
           (format nil "nonce manager error: Tempo Transaction nonce error: ~A" (precompile-error-debug reason)))
          ((starts "value transfer not allowed in AA") "value transfer in Tempo Transaction not allowed")
          ((starts "calls cannot contain CREATE when the authorization list is non-empty")
           "calls cannot contain CREATE when 'aa_authorization_list' is non-empty")
          ((starts "access key cannot authorize other keys")
           "access keys cannot authorize other keys, only the root key can authorize new keys")
          ((starts "key authorization signature recovery failed") "failed to recover signer from KeyAuthorization signature")
          ((starts "self-sponsored fee payer") "fee payer cannot resolve to sender")
          ((starts "subblock transactions are not supported") "subblock transactions are not supported")
          ((starts "T6 key authorization fields are not active before T6")
           "keychain validation failed: T6 key authorization fields are not active before T6")
          ((and (starts "keychain validation failed: ") (precompile-error-debug reason))
           (format nil "keychain validation failed: ~A" (precompile-error-debug reason)))
          ((and (starts "expired at ") now)
           (format nil "transaction expired: current block timestamp ~D >= validBefore ~D" now (reason-integer reason "expired at ")))
          ((and (starts "not valid before ") now)
           (format nil "transaction not valid yet: current block timestamp ~D < validAfter ~D" now (reason-integer reason "not valid before ")))
          ((and (starts "fee token ") (has " is not TIP-20"))
           (let ((token (checksum-address (parse-integer reason :start 10 :end (search " is not" reason) :radix 16))))
             (values (format nil "fee token ~A is not a TIP-20 token; fee tokens must be TIP-20 tokens" token)
                     (json:jobject "name" "FeeTokenNotTip20Error" "token" token))))
          ((and (starts "insufficient fee token balance: ") (not (has " in ")))
           (let* ((prefix "insufficient fee token balance: ")
                  (lt (search " < " reason)))
             (format nil "insufficient fee token balance: required ~D, but only have ~D"
                     (parse-integer reason :start (+ lt 3)) (parse-integer reason :start (length prefix) :end lt)))))))))

(defun invalid-request-error (reason &key simulate request env)
  "(values code message data) of the reference's error for the executor's rejection REASON of
REQUEST in the block environment ENV, or an internal error naming a reason with no known
reference equivalent."
  (let ((e (assoc reason +invalid-transaction-errors+ :test #'string=)))
    (if e
        (values (if simulate (fourth e) (second e)) (third e))
        (multiple-value-bind (message data)
            (let ((*invalid-context* (cons request env))) (tempo-invalid-error reason))
          (unless message
            (internal-error (format nil "request rejected by clempo's executor with no reference error mapped: ~A" reason)))
          (values -32003 message data)))))

(defun utf8-or-nil (octets)
  (handler-case (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement nil))
    (error () nil)))

(defun revert-reason (data)
  "alloy decode_revert_reason: Error(string), Panic(uint256), else the data as UTF-8 text."
  (cond ((and (>= (length data) 68) (octets= (subseq data 0 4) (octets-of #x08 #xc3 #x79 #xa0)))
         (let* ((offset (octets->integer data :start 4 :end 36))
                (start (+ 4 offset)))
           (when (<= (+ start 32) (length data))
             (let ((len (octets->integer data :start start :end (+ start 32))))
               (when (<= (+ start 32 len) (length data))
                 (utf8-or-nil (subseq data (+ start 32) (+ start 32 len))))))))
        ((and (= (length data) 36) (octets= (subseq data 0 4) (octets-of #x4e #x48 #x7b #x71)))
         (format nil "panic: ~(0x~x~)" (octets->integer data :start 4)))
        ((plusp (length data))
         (let ((text (utf8-or-nil data)))
           (and text (every (lambda (c) (or (graphic-char-p c) (< (char-code c) 32))) text) text)))))

(defun halt-name (halt)
  "The halt keyword as revm's Debug name (out-of-gas -> OutOfGas, invalid-fe-opcode ->
InvalidFEOpcode)."
  (case halt
    (:invalid-fe-opcode "InvalidFEOpcode")
    (:create-contract-starting-with-ef "CreateContractStartingWithEF")
    (t (remove #\- (string-capitalize (string halt))))))

(defun call-result (outcome gas what)
  "reth ensure_success: the output of a successful call; a revert is code 3 with its data, a
halt rpc_invalid_transaction_error's text."
  (declare (ignore what))
  (let ((output (executor:tx-outcome-output outcome))
        (halt (executor:tx-outcome-halt outcome)))
    (cond ((executor:tx-outcome-status outcome) output)
          ((null halt)
           (let ((reason (or (clempo.rpc.revert:precompile-error-text output) (revert-reason output))))
             (rpc-fail 3 (if reason (format nil "execution reverted: ~A" reason) "execution reverted")
                       (hexbytes output))))
          (t (rpc-fail -32003
                       (case halt
                         (:out-of-gas (format nil "out of gas: gas required exceeds: ~D" gas))
                         (:memory-out-of-gas (format nil "out of gas: gas exhausted during memory expansion: ~D" gas))
                         (:precompile-out-of-gas (format nil "out of gas: gas exhausted during precompiled contract execution: ~D" gas))
                         (:invalid-operand-out-of-gas (format nil "out of gas: invalid operand to an opcode: ~D" gas))
                         (:nonce-overflow "nonce has max value")
                         (t (format nil "EVM error: ~A" (halt-name halt)))))))))

(defun error-text (e)
  "How reth writes a call's error into a result (eth_callMany's and eth_createAccessList's
\"error\"): the error's Display, which for Tempo's decoded precompile reverts (an
EthApiError::Other around a jsonrpsee ErrorObject) is that object's Debug."
  (let ((data (rpc-error-data e)))
    (if (and (eql (rpc-error-code e) 3) (stringp data)
             (clempo.rpc.revert:precompile-error-text (hex->octets (subseq data 2))))
        (format nil "ErrorObject { code: ServerError(3), message: ~A, data: Some(RawValue(~A)) }"
                (serde:rust-debug-string (rpc-error-message e)) (json:to-json-string data))
        (rpc-error-message e))))

(defun tx-gas-cap (env)
  "The fork's transaction gas limit cap (EIP-7825 / Tempo T1), or NIL."
  (clempo.chainspec:tx-gas-limit-cap-at (executor:block-env-timestamp env) executor:*chain*))

(defun request-gas (request price env allowance)
  "reth prepare_call_env's gas limit: the request's (capped by the RPC gas cap), else the RPC
gas cap (prepare_call_env lifts the EIP-7825 transaction cap without EIP-8037), which for a
priced request is further capped by the block gas limit and the caller's allowance at that
gas (ALLOWANCE: gas -> what the fee payer's fee token balance buys)."
  (let ((g (req-gas request)))
    (cond (g (min g *rpc-gas-cap*))
          ((zerop price) *rpc-gas-cap*)
          (t (min (funcall allowance *rpc-gas-cap*) *rpc-gas-cap* (executor:block-env-gas-limit env))))))

(defun allowance-fn (ctx request price env)
  "Tempo's caller_gas_allowance for REQUEST in CTX as a function of the request's gas limit."
  (lambda (gas)
    (executor:caller-gas-allowance ctx (request-plist request (executor:call-block-world ctx) gas price env))))

(defun fail-invalid (e on-invalid request env)
  "Fail with the reference's error for the executor's rejection E of REQUEST in ENV, or hand
ON-INVALID its (code message)."
  (multiple-value-bind (code message data)
      (invalid-request-error (executor:execution-failure-reason e) :request request :env env)
    (if on-invalid (funcall on-invalid code message) (rpc-fail code message data))))

(defun request-runner (b state request &key state-overrides block-overrides)
  "Everything a request's executions share: (values run price env allowance), RUN taking a gas
limit and optional plist changes and executing REQUEST on a fresh overlay of STATE (overrides
applied), ALLOWANCE the caller's gas allowance on such an overlay (see ALLOWANCE-FN)."
  (let* ((price (normalize-request request (or (getf block-overrides :base-fee) (header-base-fee b))))
         (env (call-env b block-overrides price)))
    (flet ((overlay ()
             (let ((ctx (executor:begin-call-block state (call-env b block-overrides price)
                                                   :block-hash-fn (block-hash-fn))))
               (apply-state-overrides (executor:call-block-world ctx) state-overrides)
               ctx)))
      (values (lambda (gas &rest changes)
                (flet ((execute ()
                         (let ((ctx (overlay)))
                           (executor:call-request ctx (append changes (request-plist request (executor:call-block-world ctx)
                                                                                     gas price env))))))
                  (refine-halt (execute) #'execute)))
              price env
              (lambda (gas) (funcall (allowance-fn (overlay) request price env) gas))))))

(defclass halt-op-tracer ()
  ((op :initform nil :accessor halt-op))
  (:documentation "Records the opcode a top-level frame halted on as an invalid opcode."))

(defmethod clempo.evm:trace-step-end ((tr halt-op-tracer) frame pc op gas sp stack status)
  (declare (ignore pc gas sp stack))
  (when (and (eq status :invalid-opcode) (zerop (clempo.evm:frame-depth frame)))
    (setf (halt-op tr) op)))

(defun refine-halt (outcome rerun)
  "OUTCOME with revm's halt for an invalid opcode, which names the opcode: InvalidFEOpcode
for 0xFE, NotActivated for an opcode of a later fork, else OpcodeNotFound. The executor only
says :INVALID-OPCODE, so RERUN (a thunk executing the request again on a fresh overlay) runs
under a tracer that finds the opcode."
  (when (eq (executor:tx-outcome-halt outcome) :invalid-opcode)
    (let ((tr (make-instance 'halt-op-tracer)))
      (let ((clempo.evm:*tracer* tr)) (funcall rerun))
      (setf (executor:tx-outcome-halt outcome)
            (let ((op (halt-op tr)))
              (cond ((eql op #xfe) :invalid-fe-opcode)
                    ((and op (clempo.tracing:opcode-name op)) :not-activated)
                    (t :opcode-not-found))))))
  outcome)

(defun make-budget () (list *rpc-gas-cap*))

(defun budget-exceeded ()
  (rpc-fail -32000 (format nil "the request's calls exceed the RPC gas cap of ~D" *rpc-gas-cap*)))

(defun check-call-count (n)
  "reth CallManyBudget::new: at most DEFAULT_MAX_CALL_MANY_CALLS (1024) calls per request."
  (when (> n 1024) (rpc-fail +invalid-params+ (format nil "call count ~D exceeds limit 1024" n))))

(defun run-request (b state request what &key state-overrides block-overrides on-invalid ctx tracer
                                             budget)
  "Execute REQUEST on STATE in block B's environment, or in CTX after its earlier requests,
under TRACER when given. Returns (values tx-outcome gas ctx); an invalid request calls
ON-INVALID with (code message), or fails with reth's error. BUDGET (MAKE-BUDGET) is the gas
the calls of one eth_callMany, debug_traceCallMany or trace_callMany share (reth
CallManyBudget): a call gets at most what is left, and runs out of it with an error. WHAT names
the method."
  (declare (ignore what))
  (let* ((fresh (null ctx))
         (price (normalize-request request (or (getf block-overrides :base-fee) (header-base-fee b))))
         (env (call-env b block-overrides price))
         (ctx (or ctx (executor:begin-call-block state env :block-hash-fn (block-hash-fn))))
         (world (executor:call-block-world ctx))
         (gas (progn (apply-state-overrides world state-overrides)
                     (request-gas request price env (allowance-fn ctx request price env))))
         (capped (and budget (> gas (car budget)))))
    (when capped (setf gas (car budget)))
    (flet ((execute () (executor:call-request ctx (request-plist request world gas price env))))
      (let ((outcome (handler-case (if tracer
                                       (clempo.tracing:with-tracer (tracer world) (execute))
                                       (execute))
                       (executor:invalid-transaction (e)
                         (when (and capped (string= (executor:execution-failure-reason e)
                                                    "intrinsic gas too low"))
                           (budget-exceeded))
                         (fail-invalid e on-invalid request env)))))
        (when (and fresh (not tracer))
          (refine-halt outcome (lambda ()
                                 (let* ((c (executor:begin-call-block state env :block-hash-fn (block-hash-fn)))
                                        (w (executor:call-block-world c)))
                                   (apply-state-overrides w state-overrides)
                                   (executor:call-request c (request-plist request w gas price env))))))
        (when budget
          (let ((halt (executor:tx-outcome-halt outcome)))
            (when (and capped halt (search "OUT-OF-GAS" (symbol-name halt)))
              (budget-exceeded)))
          (decf (car budget) (executor:tx-outcome-gas-used outcome)))
        (values outcome gas ctx)))))

;;; Methods

(defrpc "eth_call" ((request tx-request) (block block-id :optional t)
                    (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "execute a call on the state after BLOCK"
  (multiple-value-bind (b state)
      (prepare-call request block "eth_call" :state-overrides state-overrides :block-overrides block-overrides)
    (multiple-value-bind (outcome gas)
        (run-request b state request "eth_call" :state-overrides state-overrides :block-overrides block-overrides)
      (hexbytes (call-result outcome gas "eth_call")))))

(defun basic-transfer-p (request state)
  "reth is_basic_transfer: no input and a target without code."
  (let ((input (request-input request)) (to (req-to request)))
    (and (or (null input) (zerop (length input)))
         (integerp to)
         (let ((world (state:make-world state)))
           (zerop (state:code-size world to))))))

(defun estimate-request-gas (request block &key state-overrides block-overrides)
  "reth estimate_gas_at: the gas REQUEST needs on the state after BLOCK, as an integer."
  (multiple-value-bind (b state)
      (prepare-call request block "eth_estimateGas" :state-overrides state-overrides :block-overrides block-overrides)
    (multiple-value-bind (run price env allowance)
        (request-runner b state request :state-overrides state-overrides :block-overrides block-overrides)
      (let ((max-gas (min (or (tx-gas-cap env) +u64-max+) (executor:block-env-gas-limit env) *rpc-gas-cap*)))
        (handler-case
            (executor:estimate-gas run :max-gas-limit max-gas :request-gas (req-gas request)
                                       :price-p (or (req-gas-price request) (req-max-fee request))
                                       :basic-transfer-p (and (null state-overrides)
                                                              (basic-transfer-p request state))
                                       ;; the transaction env's gas: the request's, else the block's
                                       :allowance (unless (zerop price)
                                                    (funcall allowance (or (req-gas request)
                                                                           (executor:block-env-gas-limit env)))))
          (executor:invalid-transaction (e) (fail-invalid e nil request env))
          (executor:estimate-failure (e)
            (let ((gas (executor:estimate-failure-gas-limit e))
                  (outcome (executor:estimate-failure-outcome e)))
              (ecase (executor:estimate-failure-kind e)
                (:gas-required-exceeds-allowance
                 (rpc-fail -32000 (format nil "gas required exceeds allowance (~D)" gas)))
                (:basic-out-of-gas
                 (rpc-fail -32003 (format nil "out of gas: gas required exceeds: ~D" gas)))
                ((:revert :halt) (call-result outcome gas "eth_estimateGas"))))))))))

(defrpc "eth_estimateGas" ((request tx-request) (block block-id :optional t)
                           (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "binary-search the gas a call needs"
  (quantity->hex (estimate-request-gas request block :state-overrides state-overrides
                                                     :block-overrides block-overrides)))

(defrpc "eth_createAccessList" ((request tx-request) (block block-id :optional t)
                                (state-overrides state-override :optional t))
  "the access list a call touches and its gas"
  (multiple-value-bind (b state)
      (prepare-call request block "eth_createAccessList" :state-overrides state-overrides)
    (multiple-value-bind (run price env allowance)
        (request-runner b state request :state-overrides state-overrides)
      (let ((gas (request-gas request price env allowance)))
        (handler-case
            (multiple-value-bind (list gas-used outcome)
                (executor:create-access-list (lambda (plist)
                                               (funcall run gas :access-list (getf plist :access-list)))
                                             (list :access-list (access-list-plist request))
                                             :excluded (authorization-authorities
                                                        request (executor:block-env-chain-id env)))
              (let ((error (and (not (executor:tx-outcome-status outcome))
                                (handler-case (progn (call-result outcome gas "eth_createAccessList") nil)
                                  (rpc-error (e) (error-text e))))))
                (apply #'json:jobject
                       "accessList" (coerce (loop for (address . slots) in list
                                                  collect (json:jobject "address" (addr address)
                                                                        "storageKeys" (map 'simple-vector (lambda (s) (integer->hex s :bytes 32)) slots)))
                                            'simple-vector)
                       "gasUsed" (quantity->hex gas-used)
                       (when error (list "error" error)))))
          (executor:invalid-transaction (e) (fail-invalid e nil request env)))))))

(defun call-many (bundles context state-overrides what &key tracer render)
  "reth call_many: the bundles' requests run one after another on one overlay at the
position CONTEXT selects (after the whole block, or after its first transactionIndex
transactions). Each request renders as RENDER (tracer request outcome gas) under a fresh
(funcall TRACER), or as eth_callMany's {value}/{error}. Returns one vector per bundle.
An eth_callMany request that halts on an invalid opcode is named as revm names it
(REFINE-HALT): the overlay is rebuilt, the requests before it rerun, and it runs again under
the tracer that finds the opcode."
  (let* ((target (or (getf context :block-number) :latest))
         (n (block-id-number target)))
    (unless (and n (<= n chain:*head*)) (header-not-found target))
    (check-call-count (loop for bundle in bundles sum (max 1 (length (getf bundle :transactions)))))
    (let* ((b (chain-block n))
           (budget (make-budget))
           (index (getf context :transaction-index :all))
           (all (or (eq index :all) (>= index (length (cblock-txs b)))))
           (state (when all (state-at (list :hash (cblock-hash b)))))
           (done '()))                  ; (request state-overrides block-overrides), newest first
      (labels ((start ()
                 "The overlay before the first request; NIL runs it on a fresh one over STATE."
                 (unless all
                   (let ((ctx (block-context b)))
                     (dotimes (i index ctx) (replay-tx ctx b i)))))
               (run (ctx budget request overrides block-overrides)
                 (run-request b state request what :ctx ctx :budget budget
                                                   :state-overrides overrides :block-overrides block-overrides))
               (rerun ()
                 "Every request so far again on a new overlay, only the last under *TRACER*."
                 (let ((halt-tracer clempo.evm:*tracer*) (ctx (start)) (budget (make-budget)))
                   (destructuring-bind (last &rest earlier) done
                     (let ((clempo.evm:*tracer* nil))
                       (loop for step in (reverse earlier)
                             do (setf ctx (nth-value 2 (apply #'run ctx budget step)))))
                     (let ((clempo.evm:*tracer* halt-tracer))
                       (apply #'run ctx budget last))))))
        (let ((ctx (start)))
          (coerce
           (loop for bundle in bundles for bi from 0
                 collect (coerce
                          (loop for request in (getf bundle :transactions) for ti from 0
                                for overrides = (when (and (zerop bi) (zerop ti)) state-overrides)
                                for block-overrides = (getf bundle :block-override)
                                collect (let ((tr (and tracer (funcall tracer))))
                                          (multiple-value-bind (outcome gas c)
                                              (run-request b state request what
                                                           :ctx ctx :tracer tr :budget budget
                                                           :state-overrides overrides
                                                           :block-overrides block-overrides
                                                           :on-invalid (lambda (code message)
                                                                         (rpc-fail code (format nil "call_many error in bundle ~D and transaction ~D: ~A"
                                                                                                bi ti message))))
                                            (setf ctx c)
                                            (push (list request overrides block-overrides) done)
                                            (if render
                                                (funcall render tr request outcome gas)
                                                (handler-case (json:jobject "value" (hexbytes (call-result (refine-halt outcome #'rerun) gas what)))
                                                  (rpc-error (e) (json:jobject "error" (error-text e))))))))
                          'simple-vector))
           'simple-vector))))))

(defrpc "eth_callMany" ((bundles bundles) (context state-context :optional t)
                        (state-overrides state-override :optional t))
  "execute bundles of calls one after another at a block position"
  (when (null bundles) (rpc-fail +invalid-params+ "bundles are empty."))
  (call-many bundles context state-overrides "eth_callMany"))

;;; Re-executing recorded blocks: witnesses, intermediate roots, raw block traces

(defun known-block (id)
  "reth recovered_block for ID, failing with \"block not found\"."
  (let ((n (block-id-number id)))
    (or (and n (<= n chain:*head*) (chain-block n)) (header-not-found id))))

(defparam witness-mode (src) "reth ExecutionWitnessMode: \"legacy\" or \"canonical\""
  (case (serde:peek-kind src)
    (:string (let ((s (serde:read-string src)))
               (if (member s '("legacy" "canonical") :test #'string=)
                   (intern (string-upcase s) :keyword)
                   (serde:serde-fail-here src "unknown variant `~A`, expected `legacy` or `canonical`" s))))
    (:object (serde:skip-value src) (serde:serde-fail-here src "invalid type: map, expected enum ExecutionWitnessMode"))
    (t (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "expected value"))))

(defclass recording-backend ()
  ((db :initarg :db :reader recording-db)
   (accounts :initform (make-hash-table) :reader recorded-accounts) ; address -> slot table
   (order :initform '() :accessor recorded-order))
  (:documentation "A state backend over a state database that records every account and slot
read from it: what reth's CacheDB holds after executing a block."))

(defun record-read (b address &optional (slot nil slot-p))
  (let ((slots (or (gethash address (recorded-accounts b))
                   (progn (push address (recorded-order b))
                          (setf (gethash address (recorded-accounts b)) (make-hash-table))))))
    (when slot-p (setf (gethash slot slots) t))))

(defmethod state:backend-account ((b recording-backend) address)
  (record-read b address)
  (state:backend-account (recording-db b) address))

(defmethod state:backend-storage ((b recording-backend) address slot)
  (record-read b address slot)
  (state:backend-storage (recording-db b) address slot))

(defmethod state:backend-code ((b recording-backend) code-hash)
  (state:backend-code (recording-db b) code-hash))

(defmethod state:backend-storage-empty-p ((b recording-backend) address)
  (record-read b address)
  (state:backend-storage-empty-p (recording-db b) address))

(defun execution-witness (b mode)
  "reth ExecutionWitnessRecord: block B executed on its parent's state with every account and
slot it loads recorded; the parent trie's proof nodes of those keys (state), the code of the
loaded accounts (codes: every loaded account's, the empty code included, in legacy MODE; the
non-empty ones sorted in canonical mode), the keys themselves (addresses, then each account's
slots) and the headers from the lowest one BLOCKHASH read (else the parent's) to the parent.
reth's witness also holds the nodes its sparse trie needs to apply the block's changes; the
recorded cases compare by shape."
  (let ((n (cblock-number b)))
    (when (zerop n) (header-not-found (list :hash (make-octets 32))))
    (clempo.rpc.tries:with-trie-state (db (1- n))
      (let* ((recorder (make-instance 'recording-backend :db db))
             (lowest (1- n))
             (hashes (block-hash-fn))
             (ctx (executor:begin-call-block recorder (executor:header-block-env (cblock-header b))
                                             :block-hash-fn (lambda (k)
                                                              (setf lowest (min lowest k))
                                                              (funcall hashes k)))))
        (dotimes (i (length (cblock-txs b)))
          (replay-tx ctx b i))
        (executor:finish-call-block ctx)
        (let ((nodes (make-hash-table :test 'equalp)) (state '()) (codes '()) (keys '()))
          (flet ((add (proof) (dolist (node proof)
                                (unless (gethash node nodes)
                                  (setf (gethash node nodes) t)
                                  (push node state)))))
            (dolist (address (reverse (recorded-order recorder)))
              (add (store:db-account-proof db address))
              (push (integer->octets address 20) keys)
              (let ((a (store:db-account db address)))
                (when (and a (or (eq mode :legacy)
                                 (not (octets= (store:account-code-hash a) trie:+empty-code-hash+))))
                  (pushnew (store:db-code db (store:account-code-hash a)) codes :test #'equalp)))
              (loop for slot being the hash-keys of (gethash address (recorded-accounts recorder))
                    do (add (store:db-storage-proof db address slot))
                       (push (integer->octets slot 32) keys))))
          (json:jobject "state" (map 'simple-vector #'hexbytes (nreverse state))
                        "codes" (map 'simple-vector #'hexbytes (sort codes #'octets-before-p))
                        "keys" (map 'simple-vector #'hexbytes (nreverse keys))
                        "headers" (coerce (loop for k from lowest below n
                                                for header = (cblock-header (chain-block k))
                                                collect (hexbytes (or (h:header-raw header) (h:encode-header header))))
                                          'simple-vector)))))))

(defun octets-before-p (a b)
  "Byte-wise lexicographic order (alloy Bytes' Ord)."
  (let ((i (mismatch a b)))
    (cond ((null i) nil)
          ((>= i (length a)) t)
          ((>= i (length b)) nil)
          (t (< (aref a i) (aref b i))))))

(defrpc "debug_executionWitness" ((block block-id) (mode witness-mode :optional t))
  "the trie nodes, codes and keys a block's execution reads"
  (execution-witness (known-block block) (or mode :legacy)))

(defrpc "debug_executionWitnessByBlockHash" ((hash hash) (mode witness-mode :optional t))
  "debug_executionWitness by block hash"
  (execution-witness (known-block (list :hash hash)) (or mode :legacy)))

(defun intermediate-roots (b)
  "reth intermediate_roots: block B re-executed on its parent's state (a state database with
tries, clempo.rpc.tries), the state root after the pre-execution changes and each transaction
in turn; execution stops at the first invalid transaction."
  (let ((n (cblock-number b)))
    ;; reth looks the parent up by hash: the genesis block's is the zero hash
    (when (zerop n) (header-not-found (list :hash (make-octets 32))))
    (clempo.rpc.tries:with-trie-state (db (1- n))
      (store:with-speculative-blocks (db)
        (let* ((block (blocks:decode-block (cblock-raw b)))
               (ctx (executor:begin-block db (executor:header-block-env (cblock-header b)) :block block))
               (roots '()))
          (dotimes (i (length (cblock-txs b)))
            (handler-case (executor:apply-transaction ctx (svref (cblock-txs b) i) (cblock-sender b i))
              (executor:invalid-transaction () (return))
              (executor:execution-failure (e)
                (internal-error (format nil "replaying transaction ~D of block ~D: ~A"
                                        i n (executor:execution-failure-reason e)))))
            (executor:flush-transactions ctx)
            (push (hex32 (store:db-state-root db)) roots))
          (coerce (nreverse roots) 'simple-vector))))))

(defrpc "debug_intermediateRoots" ((hash hash) (options call-tracing-options :optional t))
  "the state root after each transaction of a block"
  (intermediate-roots (known-block (list :hash hash))))

(defobject hashed-post-state (:expected "struct HashedPostState")
  (:accounts "accounts" raw :required t)
  (:storages "storages" raw :required t))

(defun preimages (hashes candidates)
  "HASHES (32-octet keys) -> the CANDIDATES (address or slot integers, WIDTH octets wide as a
cons (width . list)) whose keccak they are, as an EQUALP table."
  (let ((want (make-hash-table :test 'equalp)) (found (make-hash-table :test 'equalp)))
    (dolist (h hashes) (setf (gethash h want) t))
    (destructuring-bind (width . values) candidates
      (dolist (v values found)
        (let ((h (clempo.keccak:keccak256 (integer->octets v width))))
          (when (gethash h want) (setf (gethash h found) v)))))))

(defun trie-updates-json (nodes removed)
  (values (cons :object
                (loop for (path state tree hash hashes) in (sort (copy-list nodes) #'string<
                                                                 :key (lambda (n) (clempo.rpc.tries:nibbles-hex (car n))))
                      collect (cons (clempo.rpc.tries:nibbles-hex path)
                                    (json:jobject "state_mask" state "tree_mask" tree "hash_mask" hash
                                                  "hashes" (map 'simple-vector #'hex32 hashes)
                                                  "root_hash" :null))))
          (sort (map 'simple-vector #'clempo.rpc.tries:nibbles-hex removed) #'string<)))

(defun state-root-with-updates (input n)
  "reth state_root_with_updates: the hashed post-state INPUT (accounts and storages keyed by
keccak) applied to the state after block N; the new state root and reth's trie updates, as
its walker over the stored trie would rebuild them (clempo.rpc.tries:trie-updates). Hashed
keys are mapped back to the state's addresses and slots, so only existing ones can change."
  (clempo.rpc.tries:with-trie-state (db n)
    (store:with-speculative-blocks (db)
      (let* ((accounts (cdr (getf input :accounts)))
             (storages (cdr (getf input :storages)))
             (all-addresses (let ((out '())) (store:map-accounts (lambda (a acc) (declare (ignore acc)) (push a out)) db) out))
             (hashed (lambda (s) (hex->octets s)))
             (addresses (preimages (mapcar (lambda (e) (funcall hashed (car e))) (append accounts storages))
                                   (cons 20 all-addresses)))
             (address-of (lambda (s) (or (gethash (funcall hashed s) addresses)
                                         (internal-error (format nil "no account hashes to ~A" s)))))
             (pre (trie:trie-clone (store:db-account-trie db)))
             (pre-storage (make-hash-table)))
        (loop for (key . storage) in storages
              for address = (funcall address-of key)
              do (setf (gethash address pre-storage) (trie:trie-clone (store:db-storage-trie db address))))
        (store:db-begin-block db)
        (loop for (key . account) in accounts
              for address = (funcall address-of key)
              do (if (eq account :null)
                     (store:db-delete-account db address)
                     (let ((code (json:jget account "bytecode_hash")))
                       (store:db-set-account db address (hex-or-integer (json:jget account "nonce" 0))
                                             (hex-or-integer (json:jget account "balance" 0))
                                             (if (stringp code) (hex->octets code) trie:+empty-code-hash+)))))
        (let ((slot-changes (make-hash-table)))
          (loop for (key . storage) in storages
                for address = (funcall address-of key)
                for slots = (cdr (json:jget storage "storage" '(:object)))
                for known = (let ((out '()) (a (store:db-account db address)))
                              (when a (store:map-account-storage (lambda (s v) (declare (ignore v)) (push s out)) a))
                              out)
                for preimages = (preimages (mapcar (lambda (e) (funcall hashed (car e))) slots) (cons 32 known))
                do (when (eq (json:jget storage "wiped") :true) (store:db-wipe-storage db address))
                   (loop for (slot-key . value) in slots
                         do (store:db-set-storage db address
                                                  (or (gethash (funcall hashed slot-key) preimages)
                                                      (internal-error (format nil "no slot hashes to ~A" slot-key)))
                                                  (hex-or-integer value)))
                   (setf (gethash address slot-changes)
                         (mapcar (lambda (e) (clempo.rpc.tries:octets-nibbles (funcall hashed (car e)))) slots)))
          (let ((root (store:db-state-root db))
                (post (store:db-account-trie db))
                (changes (mapcar (lambda (e) (clempo.rpc.tries:octets-nibbles (funcall hashed (car e))))
                                 (append accounts storages))))
            (multiple-value-bind (nodes removed leaves) (clempo.rpc.tries:trie-updates pre post changes)
              (let* ((leaf-keys (mapcar (lambda (nibbles)
                                          (let ((o (make-octets 32)))
                                            (loop for (a b) on nibbles by #'cddr for i from 0
                                                  do (setf (aref o i) (logior (ash a 4) b)))
                                            o))
                                        leaves))
                     (leaf-addresses (preimages leaf-keys (cons 20 all-addresses)))
                     (storage-tries
                       (loop for key in leaf-keys
                             for address = (gethash key leaf-addresses)
                             for post-storage = (and address (store:db-storage-trie db address))
                             when post-storage
                               append (multiple-value-bind (snodes sremoved)
                                          (clempo.rpc.tries:trie-updates (or (gethash address pre-storage) post-storage)
                                                                         post-storage (gethash address slot-changes))
                                        (when (or snodes sremoved)
                                          (multiple-value-bind (sn rn) (trie-updates-json snodes sremoved)
                                            (list (cons (hex32 key) (json:jobject "storage_nodes" sn "removed_nodes" rn)))))))))
                (multiple-value-bind (an rn) (trie-updates-json nodes removed)
                  (vector (hex32 root)
                          (json:jobject "account_nodes" an "removed_nodes" rn
                                        "storage_tries" (cons :object storage-tries))))))))))))

(defun hex-or-integer (v)
  (etypecase v (integer v) (string (hex->integer v))))

(defrpc "debug_stateRootWithUpdates" ((state hashed-post-state) (block block-id :optional t))
  "the state root of a hashed post-state on top of a block's state, with its trie updates"
  (state-root-with-updates state (state-block block)))

(defun rlp-error-text (e)
  "alloy_rlp's Display for the clempo.rlp failure E."
  (let ((m (princ-to-string e)))
    (cond ((or (search "unexpected end" m) (search "truncated" m) (search "overruns" m)) "input too short")
          ((search "leading zero" m) "leading zero")
          ((search "long form" m) "non-canonical size")
          ((search "single byte" m) "non-canonical single byte")
          (t m))))

(defun decode-raw-block (raw)
  "Decode the RLP block RAW as reth's debug_traceBlock does, failing as it does."
  (flet ((fail (text) (internal-error (format nil "failed to decode raw block ~A" text))))
    (handler-case
        (multiple-value-bind (start end list-p) (clempo.rlp:rlp-item-bounds raw 0 (length raw))
          (unless list-p (fail "unexpected string"))
          (multiple-value-bind (hs he header-list-p) (clempo.rlp:rlp-item-bounds raw start end)
            (declare (ignore hs he))
            (unless header-list-p (fail "unexpected string")))
          (blocks:decode-block raw))
      (clempo.rlp:rlp-error (e) (fail (rlp-error-text e))))))

(defrpc "debug_traceBlock" ((raw hex-data) (options tracing-options :optional t))
  "trace every transaction of an RLP-encoded block on its parent's state"
  (let* ((block (decode-raw-block raw))
         (parent (list :hash (h:header-parent-hash (blocks:block-header block))))
         (b (decoded-cblock block raw)))
    (unless (eql (state-block parent) (1- (cblock-number b)))
      (header-not-found parent))
    (trace-block-replay b options)))

;;; Tracing: geth's debug_trace* and parity's trace_* families, replayed on the executor's
;;; overlay call block over the parent's state (clempo.executor begin-call-block /
;;; apply-transaction) with clempo.tracing's tracers.

(defun block-hash-fn ()
  "BLOCKHASH for replays over historical views: the archive's block hashes."
  (lambda (n)
    (let ((b (and (<= 0 n chain:*head*) (chain-block n))))
      (and b (cblock-hash b)))))

(defun block-context (b)
  "An overlay context for block B on its parent's state, after its pre-execution changes."
  (when (zerop (cblock-number b)) (rpc-fail -32000 "genesis is not traceable"))
  (executor:begin-call-block (state-at (1- (cblock-number b)))
                             (executor:header-block-env (cblock-header b))
                             :block-hash-fn (block-hash-fn)))

(defun replay-tx (ctx b i &optional tracer)
  "Apply transaction I of block B in CTX, under TRACER when given; returns its tx-outcome."
  (let ((tx (svref (cblock-txs b) i)) (sender (cblock-sender b i)))
    (handler-case
        (if tracer
            (clempo.tracing:with-tracer (tracer (executor:call-block-world ctx))
              (executor:apply-transaction ctx tx sender))
            (executor:apply-transaction ctx tx sender))
      (executor:execution-failure (e)
        (internal-error (format nil "replaying transaction ~D of block ~D: ~A"
                                i (cblock-number b) (executor:execution-failure-reason e)))))))

(defun trace-block-txs (b make-tracer result &key (start 0) (end (length (cblock-txs b))))
  "Replay block B's transactions below END, tracing those from START under a fresh
(funcall MAKE-TRACER) each; RESULT (tracer index outcome) renders one. Returns the rendered
results in order."
  (let ((ctx (block-context b)) (out '()))
    (loop for i from 0 below end
          do (let* ((tracer (and (>= i start) (funcall make-tracer)))
                    (outcome (replay-tx ctx b i tracer)))
               (when tracer (push (funcall result tracer i outcome) out))))
    (nreverse out)))

(defun tx-hash-int (b i) (octets->integer (svref (cblock-tx-hashes b) i)))

(defun geth-tx-result (b)
  "RESULT for TRACE-BLOCK-TXS: the geth tracer's output for transaction I of B."
  (lambda (tracer i outcome)
    (clempo.tracing:geth-result tracer
                                :gas-limit (tx:transaction-gas-limit (svref (cblock-txs b) i))
                                :caller (cblock-sender b i)
                                :gas-used (executor:tx-outcome-gas-used outcome)
                                :output (executor:tx-outcome-output outcome)
                                :block-hash (octets->integer (cblock-hash b))
                                :block-number (cblock-number b)
                                :tx-hash (tx-hash-int b i) :tx-index i)))

(defun parity-localized (b)
  (lambda (tracer i outcome)
    (declare (ignore outcome))
    (clempo.tracing:parity-transaction-traces tracer :block-hash (octets->integer (cblock-hash b))
                                                     :block-number (cblock-number b)
                                                     :tx-hash (tx-hash-int b i) :tx-index i)))

(defun parity-replay (b)
  "TraceResultsWithTransactionHash of transaction I of B."
  (lambda (tracer i outcome)
    (append (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome))
            (list (cons "transactionHash" (hex32 (svref (cblock-tx-hashes b) i)))))))

(defun opcode-gas (b)
  (lambda (tracer i outcome)
    (declare (ignore outcome))
    (json:jobject "transactionHash" (hex32 (svref (cblock-tx-hashes b) i))
                  "opcodeGas" (clempo.tracing:opcode-gas-result tracer))))

(defparam plain-u64 (src) "Rust u64 from a JSON number"
  (serde:read-integer src 0 +u64-max+ "u64"))

(defparam tracer-type (src) "alloy GethDebugTracerType (untagged: a built-in tracer name or JavaScript)"
  (if (eq (serde:peek-kind src) :string)
      (serde:read-string src)
      (progn (serde:skip-value src)
             (serde:serde-fail-here src "data did not match any variant of untagged enum GethDebugTracerType"))))

(defparameter +tracing-option-fields+
  '((:tracer "tracer" tracer-type) (:tracer-config "tracerConfig" raw) (:timeout "timeout" string)))

(defparameter +default-tracing-fields+
  '((:enable-memory "enableMemory" bool) (:disable-memory "disableMemory" bool)
    (:disable-stack "disableStack" bool) (:disable-storage "disableStorage" bool)
    (:enable-return-data "enableReturnData" bool) (:disable-return-data "disableReturnData" bool)
    (:debug "debug" bool) (:limit "limit" plain-u64))
  "alloy GethDefaultTracingOptions, flattened into the tracing options.")

(defun content-tracing-options (src end entries)
  "GethDebugTracingOptions from buffered ENTRIES (it is itself flattened in the call options)."
  (multiple-value-bind (options left) (read-content-fields src end entries +tracing-option-fields+)
    (append options (read-content-fields src end left +default-tracing-fields+))))

(defparam tracing-options (src) "alloy GethDebugTracingOptions as a plist"
  (let ((options '()) (seen '()))
    (multiple-value-bind (rest end)
        (read-flattened src "struct GethDebugTracingOptions" (mapcar #'second +tracing-option-fields+)
                        (lambda (key src)
                          (destructuring-bind (kw name type) (find key +tracing-option-fields+ :key #'second :test #'string=)
                            (declare (ignore name))
                            (when (member kw seen) (serde:serde-fail-after-key src "duplicate field `~A`" key))
                            (push kw seen)
                            (unless (and (not (eq kw :tracer-config)) (serde:read-null-p src))
                              (setf (getf options kw) (read-param type src))))))
      (append options (read-content-fields src end rest +default-tracing-fields+)))))

(defparam call-tracing-options (src) "alloy GethDebugTracingCallOptions as a plist"
  (let ((options '()) (seen '())
        (fields '(("stateOverrides" :state-overrides state-override) ("blockOverrides" :block-overrides block-overrides)
                  ("txIndex" :tx-index q64))))
    (multiple-value-bind (rest end)
        (read-flattened src "struct GethDebugTracingCallOptions" (mapcar #'first fields)
                        (lambda (key src)
                          (destructuring-bind (kw type) (rest (assoc key fields :test #'string=))
                            (when (member kw seen) (serde:serde-fail-after-key src "duplicate field `~A`" key))
                            (push kw seen)
                            (unless (serde:read-null-p src)
                              (setf (getf options kw) (read-param type src))))))
      (append options (content-tracing-options src end rest)))))

(defparameter +builtin-tracers+
  '("4byteTracer" "callTracer" "flatCallTracer" "prestateTracer" "stateGasTracer" "noopTracer"
    "muxTracer" "erc7562Tracer"))

(defparameter +tracer-config-fields+
  '(("callTracer" "onlyTopCall" "withLog")
    ("prestateTracer" "diffMode" "disableCode" "disableStorage")
    ("flatCallTracer" "convertParityErrors" "includePrecompiles"))
  "The boolean fields of each tracer's config (alloy CallConfig, PreStateConfig, FlatCallConfig).")

(defun invalid-tracer-config () (rpc-fail +invalid-params+ "invalid tracer config"))

(defun check-tracer-config (name config)
  "evm2-inspectors DebugInspector::new's config parsing for tracer NAME."
  (let ((fields (rest (assoc name +tracer-config-fields+ :test #'string=))))
    (cond ((or (null config) (eq config :null)))
          (fields
           (unless (json:jobject-p config) (invalid-tracer-config))
           (loop for (k . v) in (cdr config)
                 when (and (member k fields :test #'string=) (not (member v '(:true :false :null))))
                   do (invalid-tracer-config)))
          ((string= name "muxTracer")
           (unless (json:jobject-p config) (invalid-tracer-config))
           (loop for (k . v) in (cdr config)
                 do (unless (member k +builtin-tracers+ :test #'string=) (invalid-tracer-config))
                    (check-tracer-config k v))))))

(defun check-tracer (options)
  "Reject what DebugInspector::new rejects before any execution."
  (let ((name (getf options :tracer)))
    (when (and name (plusp (length name)))
      (cond ((member name +builtin-tracers+ :test #'string=)
             (check-tracer-config name (getf options :tracer-config)))
            (t (internal-error "JS Tracer is not enabled"))))))

(defparam trace-types (src) "HashSet<parity TraceType>: a list of :trace :vm-trace :state-diff"
  (let ((out '()))
    (serde:do-array (i src)
      (if (eq (serde:peek-kind src) :string)
          (let* ((s (serde:read-string src))
                 (kind (cdr (assoc s '(("trace" . :trace) ("vmTrace" . :vm-trace) ("stateDiff" . :state-diff))
                                   :test #'string=))))
            (unless kind
              (serde:serde-fail-here src "unknown variant `~A`, expected one of `trace`, `vmTrace`, `stateDiff`" s))
            (pushnew kind out))
          (progn (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "expected value"))))
    (nreverse out)))

(defun options-json (options)
  "The GethDebugTracingOptions plist as the JSON object clempo.tracing reads."
  (let ((kvs '()))
    (loop for (kw name) in (append +tracing-option-fields+ +default-tracing-fields+)
          for v = (getf options kw :absent)
          unless (eq v :absent)
            do (push name kvs)
               (push (if (member kw '(:tracer :tracer-config :timeout :limit)) v (json:jbool v)) kvs))
    (apply #'json:jobject (nreverse kvs))))

(defun geth-tracer (options)
  (handler-case (clempo.tracing:make-geth-tracer (options-json options))
    (clempo.tracing:tracer-error (e) (rpc-fail +invalid-params+ (clempo.tracing:tracer-error-message e)))))

(defun tx-block (hash)
  "(values cblock index) of a transaction at or below the head, or NIL."
  (multiple-value-bind (n i) (chain:backend-tx-location chain:*backend* hash)
    (when (and n (<= n chain:*head*)) (values (chain-block n) i))))

(defun tracing-block-not-found (id)
  "reth TracingBlockNotFound: code -32000, the hash or number without a prefix word."
  (rpc-fail -32000 (format nil "block ~A not found"
                           (if (hash-id-p* id) (hex32 (second id)) (block-id-text id)))))

(defun hash-id-p* (id) (and (consp id) (eq (first id) :hash)))

(defun trace-block-replay (b options)
  "debug_traceBlockBy*: every transaction of B as {txHash, result}."
  (when (zerop (cblock-number b)) (rpc-fail -32000 "genesis is not traceable"))
  (check-tracer options)
  (let ((render (geth-tx-result b)))
    (coerce (trace-block-txs b (lambda () (geth-tracer options))
                             (lambda (tracer i outcome)
                               (json:jobject "txHash" (hex32 (svref (cblock-tx-hashes b) i))
                                             "result" (funcall render tracer i outcome))))
            'simple-vector)))

(defrpc "debug_traceTransaction" ((hash hash) (options tracing-options :optional t))
  "replay a transaction under a geth tracer"
  (multiple-value-bind (b i) (tx-block hash)
    (unless b (rpc-fail -32000 "transaction not found"))
    (check-tracer options)
    (first (trace-block-txs b (lambda () (geth-tracer options)) (geth-tx-result b)
                            :start i :end (1+ i)))))

(defrpc "debug_traceBlockByNumber" ((id block-number-or-tag) (options tracing-options :optional t))
  "replay a block under a geth tracer"
  (let* ((n (block-id-number id))
         (b (and n (<= n chain:*head*) (chain-block n))))
    (unless b (tracing-block-not-found id))
    (trace-block-replay b options)))

(defrpc "debug_traceBlockByHash" ((hash hash) (options tracing-options :optional t))
  "replay a block under a geth tracer"
  (let ((b (let ((n (block-id-number (list :hash hash)))) (and n (<= n chain:*head*) (chain-block n)))))
    (unless b (tracing-block-not-found (list :hash hash)))
    (trace-block-replay b options)))

(defun request-geth-result (tracer request outcome gas)
  (clempo.tracing:geth-result tracer :gas-limit gas :caller (or (req-from request) 0)
                                     :gas-used (executor:tx-outcome-gas-used outcome)
                                     :output (executor:tx-outcome-output outcome)))

(defrpc "debug_traceCall" ((request tx-request) (block block-id :optional t)
                           (options call-tracing-options :optional t))
  "trace a call on the state after BLOCK"
  (let ((b (call-block block))
        (state-overrides (getf options :state-overrides))
        (block-overrides (getf options :block-overrides)))
    (check-state-overrides state-overrides)
    (convert-request request (or (getf block-overrides :base-fee) (header-base-fee b)))
    (check-tracer options)
    (let ((state (state-at (list :hash (cblock-hash b))))
          (tracer (geth-tracer options)))
      (multiple-value-bind (outcome gas)
          (run-request b state request "debug_traceCall" :state-overrides state-overrides
                                                         :block-overrides block-overrides :tracer tracer)
        (request-geth-result tracer request outcome gas)))))

(defrpc "debug_traceCallMany" ((bundles bundles) (context state-context :optional t)
                               (options call-tracing-options :optional t))
  "trace bundles of calls one after another at a block position"
  (when (null bundles) (rpc-fail +invalid-params+ "bundles are empty."))
  (check-tracer options)
  (call-many bundles context (getf options :state-overrides) "debug_traceCallMany"
             :tracer (lambda () (geth-tracer options))
             :render (lambda (tracer request outcome gas)
                       (request-geth-result tracer request outcome gas))))

(defrpc "trace_call" ((request tx-request) (types trace-types) (block block-id :optional t)
                      (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "parity traces of a call"
  (multiple-value-bind (b state)
      (prepare-call request block "trace_call" :state-overrides state-overrides :block-overrides block-overrides)
    (let ((tracer (clempo.tracing:make-parity-tracer types)))
      (let ((outcome (run-request b state request "trace_call" :state-overrides state-overrides
                                                               :block-overrides block-overrides :tracer tracer)))
        (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome))))))

(defparam traced-call (src) "(TxReq, HashSet<TraceType>), a JSON pair"
  (let ((request nil) (types nil) (n 0))
    (serde:do-array (j src :expected "a tuple of size 2")
      (case j
        (0 (setf request (read-param 'tx-request src)))
        (1 (setf types (read-param 'trace-types src)))
        (t (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "trailing characters")))
      (setf n (1+ j)))
    (when (< n 2) (serde:serde-fail-here src "invalid length ~D, expected a tuple of size 2" n))
    (cons request types)))
(defvec traced-calls traced-call)

(defrpc "trace_callMany" ((calls traced-calls) (block block-id :optional t))
  "parity traces of calls executed one after another"
  (check-call-count (length calls))
  (let* ((b (call-block block))
         (state (state-at (list :hash (cblock-hash b))))
         (budget (make-budget))
         (ctx nil))
    (coerce
     (loop for (request . types) in calls
           collect (let ((tracer (clempo.tracing:make-parity-tracer types)))
                     (multiple-value-bind (outcome gas c)
                         (run-request b state request "trace_callMany" :ctx ctx :tracer tracer
                                                                       :budget budget)
                       (declare (ignore gas))
                       (setf ctx c)
                       (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome)))))
     'simple-vector)))

(defun tx-trace (hash make-tracer render)
  "Trace transaction HASH; NIL when it is unknown."
  (multiple-value-bind (b i) (tx-block hash)
    (when b
      (first (trace-block-txs b make-tracer (funcall render b) :start i :end (1+ i))))))

(defun parity-maker (types) (lambda () (clempo.tracing:make-parity-tracer types)))

(defrpc "trace_transaction" ((hash hash))
  "parity traces of a transaction"
  (or (tx-trace hash (parity-maker '(:trace)) #'parity-localized) :null))

(defun index-value (v)
  (cond ((integerp v) v)
        ((and (stringp v) (prefixed-p v "0x")) (hex->integer v))
        ((stringp v) (parse-integer v :junk-allowed t))))

(defrpc "trace_get" ((hash hash) (indices any))
  "the parity trace of a transaction at a traceAddress path (reth: find by trace_address)"
  (let ((traces (tx-trace hash (parity-maker '(:trace)) #'parity-localized))
        (path (and (vectorp indices) (not (stringp indices)) (map 'list #'index-value indices))))
    (or (and traces (vectorp indices) (not (stringp indices))
             (find-if (lambda (tr) (equal (coerce (json:jget tr "traceAddress") 'list) path)) traces))
        :null)))

(defrpc "trace_replayTransaction" ((hash hash) (types trace-types))
  "replay a transaction with parity tracers (reth: None for an unknown transaction)"
  (or (tx-trace hash (parity-maker types) #'parity-replay) :null))

(defrpc "trace_transactionOpcodeGas" ((hash hash))
  "opcode counts and gas of a transaction"
  (or (tx-trace hash #'clempo.tracing:make-opcode-gas-tracer
                #'opcode-gas)
      :null))

(defmacro defblock-trace (name (&rest params) doc &body body)
  "A trace_* method over block B (reth: HeaderNotFound for an unknown block); BODY renders it."
  `(defrpc ,name ((id block-id) ,@params) ,doc
     (let ((b (known-block id)))
       ,@body)))

(defun block-traces (b make-tracer render)
  (if (zerop (cblock-number b)) '() (trace-block-txs b make-tracer (funcall render b))))

(defblock-trace "trace_block" () "parity traces of every transaction of a block"
  (apply #'concatenate 'simple-vector (block-traces b (parity-maker '(:trace)) #'parity-localized)))

(defblock-trace "trace_replayBlockTransactions" ((types trace-types)) "replay a block with parity tracers"
  (coerce (block-traces b (parity-maker types) #'parity-replay) 'simple-vector))

(defblock-trace "trace_blockOpcodeGas" () "opcode counts and gas of every transaction of a block"
  (json:jobject "blockHash" (hex32 (cblock-hash b)) "blockNumber" (cblock-number b)
                "transactions" (coerce (block-traces b #'clempo.tracing:make-opcode-gas-tracer
                                                     #'opcode-gas)
                                       'simple-vector)))
