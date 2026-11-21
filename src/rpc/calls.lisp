(defpackage #:clempo.rpc.calls
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:h #:clempo.headers) (#:executor #:clempo.executor) (#:blocks #:clempo.blocks)
                    (#:state #:clempo.state) (#:tx #:clempo.transactions))
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
           #:call-env #:block-hash-fn #:apply-state-overrides #:check-state-overrides
           #:run-request #:call-result #:revert-reason #:invalid-request-error
           #:+invalid-transaction-errors+ #:*rpc-gas-cap* #:header-base-fee))

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
(defvec authorization-list signed-authorization)

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
    ("aaAuthorizationList" :tempo-authorizations raw nil) ("keyAuthorization" :key-authorization raw t)
    ("configurableAccountSimulation" :account-simulation raw t)
    ("keyAuthorizationSimulation" :key-authorization-simulation raw t)
    ("validBefore" :valid-before nonzero-q64 t) ("validAfter" :valid-after nonzero-q64 t)
    ("feePayerSignature" :fee-payer-signature raw t))
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

(defun request-plist (r world gas price)
  "The executor's request plist for an Ethereum request (normalize_transaction_request with
the nonce taken from the state, as create_txn_env does)."
  (let ((type (request-type r)) (from (or (req-from r) 0)))
    (when (req-authorizations r)
      (internal-error "EIP-7702 authorization lists in call requests are not converted yet"))
    (list :from from
          :to (let ((to (req-to r))) (if (or (null to) (eq to :create)) nil to))
          :gas gas :type type
          :gas-price (when (member type '(0 1)) (or (req-gas-price r) price))
          :max-fee (unless (member type '(0 1)) (or (req-max-fee r) price))
          :max-priority (unless (member type '(0 1)) (or (req-max-priority r) 0))
          :value (or (req-value r) 0)
          :nonce (state:get-nonce world from)
          :chain-id (req-chain-id r)
          :input (or (request-input r) (make-octets 0))
          :access-list (access-list-plist r))))

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

(defun invalid-request-error (reason &key simulate)
  "(values code message) of the reference's error for an invalid request, or an internal
error naming a reason with no known reference equivalent."
  (let ((e (assoc reason +invalid-transaction-errors+ :test #'string=)))
    (unless e (internal-error (format nil "request rejected by clempo's executor with no reference error mapped: ~A" reason)))
    (values (if simulate (fourth e) (second e)) (third e))))

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
  "The halt keyword as revm's Debug name (out-of-gas -> OutOfGas)."
  (remove #\- (string-capitalize (string halt))))

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

(defun tx-gas-cap (env)
  "The fork's transaction gas limit cap (EIP-7825 / Tempo T1), or NIL."
  (clempo.chainspec:tx-gas-limit-cap-at (executor:block-env-timestamp env) executor:*chain*))

(defun request-gas (request price env what)
  "reth prepare_call_env's gas limit: the request's (capped by the RPC gas cap), else the RPC
gas cap bounded by the transaction cap."
  (let ((g (req-gas request)))
    (cond (g (min g *rpc-gas-cap*))
          ((zerop price) (min *rpc-gas-cap* (or (tx-gas-cap env) *rpc-gas-cap*)))
          (t (internal-error (format nil "~A: capping the gas of a priced request by the caller's fee token allowance is not implemented yet" what))))))

(defun fail-invalid (e on-invalid)
  (multiple-value-bind (code message) (invalid-request-error (executor:execution-failure-reason e))
    (if on-invalid (funcall on-invalid code message) (rpc-fail code message))))

(defun request-runner (b state request what &key state-overrides block-overrides)
  "Everything a request's executions share: (values run price env), RUN taking a gas limit
and optional plist changes and executing REQUEST on a fresh overlay of STATE (overrides applied)."
  (when (aa-request-p request)
    (internal-error (format nil "~A: Tempo AA call requests are not converted to executor requests yet" what)))
  (let* ((price (normalize-request request (or (getf block-overrides :base-fee) (header-base-fee b))))
         (env (call-env b block-overrides price)))
    (values (lambda (gas &rest changes)
              (let* ((ctx (executor:begin-call-block state (call-env b block-overrides price)
                                                     :block-hash-fn (block-hash-fn)))
                     (world (executor:call-block-world ctx)))
                (apply-state-overrides world state-overrides)
                (executor:call-request ctx (append changes (request-plist request world gas price)))))
            price env)))

(defun run-request (b state request what &key state-overrides block-overrides on-invalid ctx tracer)
  "Execute REQUEST on STATE in block B's environment, or in CTX after its earlier requests,
under TRACER when given. Returns (values tx-outcome gas ctx); an invalid request calls
ON-INVALID with (code message), or fails with reth's error."
  (when (aa-request-p request)
    (internal-error (format nil "~A: Tempo AA call requests are not converted to executor requests yet" what)))
  (let* ((price (normalize-request request (or (getf block-overrides :base-fee) (header-base-fee b))))
         (env (call-env b block-overrides price))
         (ctx (or ctx (executor:begin-call-block state env :block-hash-fn (block-hash-fn))))
         (world (executor:call-block-world ctx))
         (gas (request-gas request price env what)))
    (apply-state-overrides world state-overrides)
    (flet ((execute () (executor:call-request ctx (request-plist request world gas price))))
      (values (handler-case (if tracer
                                (clempo.tracing:with-tracer (tracer world) (execute))
                                (execute))
                (executor:invalid-transaction (e) (fail-invalid e on-invalid)))
              gas
              ctx))))

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

(defrpc "eth_estimateGas" ((request tx-request) (block block-id :optional t)
                           (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "binary-search the gas a call needs"
  (multiple-value-bind (b state)
      (prepare-call request block "eth_estimateGas" :state-overrides state-overrides :block-overrides block-overrides)
    (multiple-value-bind (run price env)
        (request-runner b state request "eth_estimateGas" :state-overrides state-overrides
                                                          :block-overrides block-overrides)
      (unless (zerop price)
        (internal-error "eth_estimateGas: the caller's fee token allowance for priced requests is not implemented yet"))
      (let ((max-gas (min (or (tx-gas-cap env) +u64-max+) (executor:block-env-gas-limit env) *rpc-gas-cap*)))
        (handler-case
            (quantity->hex (executor:estimate-gas run :max-gas-limit max-gas :request-gas (req-gas request)
                                                 :price-p (or (req-gas-price request) (req-max-fee request))
                                                 :basic-transfer-p (and (null state-overrides)
                                                                        (basic-transfer-p request state))))
          (executor:invalid-transaction (e) (fail-invalid e nil))
          (executor:estimate-failure (e)
            (let ((gas (executor:estimate-failure-gas-limit e))
                  (outcome (executor:estimate-failure-outcome e)))
              (ecase (executor:estimate-failure-kind e)
                (:gas-required-exceeds-allowance
                 (rpc-fail -32000 (format nil "gas required exceeds allowance (~D)" gas)))
                (:basic-out-of-gas
                 (rpc-fail -32003 (format nil "out of gas: gas required exceeds: ~D" gas)))
                ((:revert :halt) (call-result outcome gas "eth_estimateGas"))))))))))

(defrpc "eth_createAccessList" ((request tx-request) (block block-id :optional t)
                                (state-overrides state-override :optional t))
  "the access list a call touches and its gas"
  (multiple-value-bind (b state)
      (prepare-call request block "eth_createAccessList" :state-overrides state-overrides)
    (multiple-value-bind (run price env)
        (request-runner b state request "eth_createAccessList" :state-overrides state-overrides)
      (let ((gas (request-gas request price env "eth_createAccessList")))
        (handler-case
            (multiple-value-bind (list gas-used outcome)
                (executor:create-access-list (lambda (plist)
                                               (funcall run gas :access-list (getf plist :access-list)))
                                             (list :access-list (access-list-plist request)))
              (let ((error (and (not (executor:tx-outcome-status outcome))
                                (handler-case (progn (call-result outcome gas "eth_createAccessList") nil)
                                  (rpc-error (e) (rpc-error-message e))))))
                (apply #'json:jobject
                       "accessList" (coerce (loop for (address . slots) in list
                                                  collect (json:jobject "address" (addr address)
                                                                        "storageKeys" (map 'simple-vector (lambda (s) (integer->hex s :bytes 32)) slots)))
                                            'simple-vector)
                       "gasUsed" (quantity->hex gas-used)
                       (when error (list "error" error)))))
          (executor:invalid-transaction (e) (fail-invalid e nil)))))))

(defun call-many (bundles context state-overrides what &key tracer render)
  "reth call_many: the bundles' requests run one after another on one overlay at the
position CONTEXT selects (after the whole block, or after its first transactionIndex
transactions). Each request renders as RENDER (tracer request outcome gas) under a fresh
(funcall TRACER), or as eth_callMany's {value}/{error}. Returns one vector per bundle."
  (let* ((target (or (getf context :block-number) :latest))
         (n (block-id-number target)))
    (unless (and n (<= n chain:*head*)) (header-not-found target))
    (let* ((b (chain-block n))
           (index (getf context :transaction-index :all))
           (all (or (eq index :all) (>= index (length (cblock-txs b)))))
           (state (when all (state-at (list :hash (cblock-hash b)))))
           (ctx (unless all
                  (let ((ctx (block-context b)))
                    (dotimes (i index ctx) (replay-tx ctx b i))))))
      (coerce
       (loop for bundle in bundles for bi from 0
             collect (coerce
                      (loop for request in (getf bundle :transactions) for ti from 0
                            for overrides = (when (and (zerop bi) (zerop ti)) state-overrides)
                            collect (let ((tr (and tracer (funcall tracer))))
                                      (multiple-value-bind (outcome gas c)
                                          (run-request b state request what
                                                       :ctx ctx :tracer tr
                                                       :state-overrides overrides
                                                       :block-overrides (getf bundle :block-override)
                                                       :on-invalid (lambda (code message)
                                                                     (rpc-fail code (format nil "call_many error in bundle ~D and transaction ~D: ~A"
                                                                                            bi ti message))))
                                        (setf ctx c)
                                        (if render
                                            (funcall render tr request outcome gas)
                                            (handler-case (json:jobject "value" (hexbytes (call-result outcome gas what)))
                                              (rpc-error (e) (json:jobject "error" (rpc-error-message e))))))))
                      'simple-vector))
       'simple-vector))))

(defrpc "eth_callMany" ((bundles bundles) (context state-context :optional t)
                        (state-overrides state-override :optional t))
  "execute bundles of calls one after another at a block position"
  (when (null bundles) (rpc-fail +invalid-params+ "bundles are empty."))
  (call-many bundles context state-overrides "eth_callMany"))

;;; Re-executing recorded blocks: witnesses, intermediate roots, raw block traces

(defun replay-unavailable (what)
  (internal-error (format nil "~A: re-executing a block needs the executor's overlay replay (clempo.executor begin-call-block), which has not landed yet" what)))

(defun known-block (id)
  "reth recovered_block for ID, failing with \"block not found\"."
  (let ((n (block-id-number id)))
    (or (and n (<= n chain:*head*) (chain-block n)) (header-not-found id))))

(defun parent-state (b what)
  "The state a block executes on: its parent's (the genesis block has none)."
  (when (plusp (cblock-number b)) (state-at (1- (cblock-number b))))
  (replay-unavailable what))

(defparam witness-mode (src) "reth ExecutionWitnessMode: \"legacy\" or \"canonical\""
  (case (serde:peek-kind src)
    (:string (let ((s (serde:read-string src)))
               (if (member s '("legacy" "canonical") :test #'string=)
                   (intern (string-upcase s) :keyword)
                   (serde:serde-fail-here src "unknown variant `~A`, expected `legacy` or `canonical`" s))))
    (:object (serde:skip-value src) (serde:serde-fail-here src "invalid type: map, expected enum ExecutionWitnessMode"))
    (t (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "expected value"))))

(defun execution-witness (b)
  (parent-state b "debug_executionWitness"))

(defrpc "debug_executionWitness" ((block block-id) (mode witness-mode :optional t))
  "the trie nodes, codes and keys a block's execution reads"
  (execution-witness (known-block block)))

(defrpc "debug_executionWitnessByBlockHash" ((hash hash) (mode witness-mode :optional t))
  "debug_executionWitness by block hash"
  (execution-witness (known-block (list :hash hash))))

(defrpc "debug_intermediateRoots" ((hash hash) (options call-tracing-options :optional t))
  "the state root after each transaction of a block"
  (parent-state (known-block (list :hash hash)) "debug_intermediateRoots"))

(defobject hashed-post-state (:expected "struct HashedPostState")
  (:accounts "accounts" raw :required t)
  (:storages "storages" raw :required t))

(defrpc "debug_stateRootWithUpdates" ((state hashed-post-state) (block block-id :optional t))
  "the state root of a hashed post-state on top of a block's state, with its trie updates"
  (state-at block)
  (internal-error "debug_stateRootWithUpdates: state roots over hashed keys (keccak-addressed tries) are not implemented over clempo's state database"))

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
         (parent (list :hash (h:header-parent-hash (blocks:block-header block)))))
    (state-block parent)
    (check-tracer options)
    (state-at parent)
    (replay-unavailable "debug_traceBlock")))

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
  "Replay block B's transactions below END, tracing those from START: MAKE-TRACER takes the
block-wide index of the transaction's first log, RESULT (tracer index outcome) renders one.
Returns the rendered results in order."
  (let ((ctx (block-context b)) (logs 0) (out '()))
    (loop for i from 0 below end
          do (let* ((tracer (and (>= i start) (funcall make-tracer logs)))
                    (outcome (replay-tx ctx b i tracer)))
               (when tracer (push (funcall result tracer i outcome) out))
               (incf logs (length (executor:tx-outcome-logs outcome)))))
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
  (lambda (tracer i outcome)
    (let ((r (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome))))
      (if b
          (append r (list (cons "transactionHash" (hex32 (svref (cblock-tx-hashes b) i)))))
          r))))

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
                          (destructuring-bind (kw nil type) (find key +tracing-option-fields+ :key #'second :test #'string=)
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

(defun geth-tracer (options &optional (log-index 0))
  (handler-case (clempo.tracing:make-geth-tracer (options-json options) :log-index log-index)
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
    (coerce (trace-block-txs b (lambda (logs) (geth-tracer options logs))
                             (lambda (tracer i outcome)
                               (json:jobject "txHash" (hex32 (svref (cblock-tx-hashes b) i))
                                             "result" (funcall render tracer i outcome))))
            'simple-vector)))

(defrpc "debug_traceTransaction" ((hash hash) (options tracing-options :optional t))
  "replay a transaction under a geth tracer"
  (multiple-value-bind (b i) (tx-block hash)
    (unless b (rpc-fail -32000 "transaction not found"))
    (check-tracer options)
    (first (trace-block-txs b (lambda (logs) (geth-tracer options logs)) (geth-tx-result b)
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
  (let* ((b (call-block block))
         (state (state-at (list :hash (cblock-hash b))))
         (ctx nil))
    (coerce
     (loop for (request . types) in calls
           collect (let ((tracer (clempo.tracing:make-parity-tracer types)))
                     (multiple-value-bind (outcome gas c)
                         (run-request b state request "trace_callMany" :ctx ctx :tracer tracer)
                       (declare (ignore gas))
                       (setf ctx c)
                       (clempo.tracing:parity-result tracer :output (executor:tx-outcome-output outcome)))))
     'simple-vector)))

(defun tx-trace (hash make-tracer render)
  "Trace transaction HASH; NIL when it is unknown."
  (multiple-value-bind (b i) (tx-block hash)
    (when b
      (first (trace-block-txs b make-tracer (funcall render b) :start i :end (1+ i))))))

(defun parity-maker (types) (lambda (logs) (declare (ignore logs)) (clempo.tracing:make-parity-tracer types)))

(defrpc "trace_transaction" ((hash hash))
  "parity traces of a transaction"
  (or (tx-trace hash (parity-maker '(:trace)) #'parity-localized) :null))

(defun index-value (v)
  (cond ((integerp v) v)
        ((and (stringp v) (prefixed-p v "0x")) (hex->integer v))
        ((stringp v) (parse-integer v :junk-allowed t))))

(defrpc "trace_get" ((hash hash) (indices any))
  "one parity trace of a transaction by its index"
  (let ((traces (tx-trace hash (parity-maker '(:trace)) #'parity-localized)))
    (if (and traces (vectorp indices) (= (length indices) 1))
        (let ((k (index-value (aref indices 0))))
          (if (and k (< k (length traces))) (aref traces k) :null))
        :null)))

(defrpc "trace_replayTransaction" ((hash hash) (types trace-types))
  "replay a transaction with parity tracers"
  (or (tx-trace hash (parity-maker types) (lambda (b) (declare (ignore b)) (parity-replay nil)))
      (rpc-fail -32000 "transaction not found")))

(defrpc "trace_transactionOpcodeGas" ((hash hash))
  "opcode counts and gas of a transaction"
  (or (tx-trace hash (lambda (logs) (declare (ignore logs)) (clempo.tracing:make-opcode-gas-tracer))
                #'opcode-gas)
      :null))

(defmacro defblock-trace (name (&rest params) doc &body body)
  "A trace_* method over block B (null for an unknown block); BODY renders it."
  `(defrpc ,name ((id block-id) ,@params) ,doc
     (let* ((n (block-id-number id))
            (b (and n (<= n chain:*head*) (chain-block n))))
       (if (null b) :null (progn ,@body)))))

(defun block-traces (b make-tracer render)
  (if (zerop (cblock-number b)) '() (trace-block-txs b make-tracer (funcall render b))))

(defblock-trace "trace_block" () "parity traces of every transaction of a block"
  (apply #'concatenate 'simple-vector (block-traces b (parity-maker '(:trace)) #'parity-localized)))

(defblock-trace "trace_replayBlockTransactions" ((types trace-types)) "replay a block with parity tracers"
  (coerce (block-traces b (parity-maker types) #'parity-replay) 'simple-vector))

(defblock-trace "trace_blockOpcodeGas" () "opcode counts and gas of every transaction of a block"
  (json:jobject "blockHash" (hex32 (cblock-hash b)) "blockNumber" (cblock-number b)
                "transactions" (coerce (block-traces b (lambda (logs) (declare (ignore logs))
                                                         (clempo.tracing:make-opcode-gas-tracer))
                                                     #'opcode-gas)
                                       'simple-vector)))
