(defpackage #:clempo.rpc.calls
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:h #:clempo.headers) (#:executor #:clempo.executor) (#:blocks #:clempo.blocks))
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
           #:normalize-request))

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

(defun number-unexpected (n)
  "serde's Unexpected for a JSON number as serde_json visits it (u64, i64 or f64)."
  (cond ((and (integerp n) (<= 0 n +u64-max+)) (format nil "integer `~D`" n))
        ((and (integerp n) (<= (- (ash 1 63)) n -1)) (format nil "integer `~D`" n))
        (t (format nil "floating point `~A`" (clempo.serde::rust-float (coerce n 'double-float))))))

(defun prefixed-p (s prefix)
  (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(defun parse-ruint (s bits)
  "ruint's FromStr: 0x/0o/0b prefixes or decimal, underscores ignored; NIL when invalid or
too large for BITS."
  (multiple-value-bind (digits radix)
      (cond ((or (prefixed-p s "0x") (prefixed-p s "0X")) (values (subseq s 2) 16))
            ((or (prefixed-p s "0o") (prefixed-p s "0O")) (values (subseq s 2) 8))
            ((or (prefixed-p s "0b") (prefixed-p s "0B")) (values (subseq s 2) 2))
            (t (values s 10)))
    (let ((n 0))
      (loop for c across digits
            unless (char= c #\_)
              do (let ((d (digit-char-p c radix)))
                   (unless d (return-from parse-ruint nil))
                   (setf n (+ (* n radix) d))))
      (and (< n (ash 1 bits)) n))))

(defun read-ruint (src bits)
  "ruint Uint<BITS> from JSON: a string (hex, octal, binary or decimal) or a u64 number."
  (let ((expected (format nil "a ~D byte hex string" (ceiling bits 8))))
    (case (serde:peek-kind src)
      (:string (let* ((s (serde:read-string src)) (n (parse-ruint s bits)))
                 (or n (serde:serde-fail-here src "invalid value: string ~A, expected ~A"
                                              (serde:rust-debug-string s) expected))))
      (:number (let ((n (serde:read-number src)))
                 (cond ((and (integerp n) (<= 0 n +u64-max+))
                        (if (< n (ash 1 bits)) n
                            (serde:serde-fail-here src "invalid value: integer `~D`, expected ~A" n expected)))
                       (t (serde:serde-fail-here src "invalid type: ~A, expected ~A" (number-unexpected n) expected)))))
      (t (serde:invalid-type src expected)))))

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
                (push (if (eq (serde:peek-kind src) :number)
                          (let ((n (serde:read-number src)))
                            (if (and (integerp n) (<= 0 n 255)) n
                                (serde:serde-fail-here src "invalid value: ~A, expected u8" (number-unexpected n))))
                          (serde:invalid-type src "u8"))
                      bytes))
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
          (push (cons key (serde:read-raw-value src)) rest)))
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

;;; Methods

(defrpc "eth_call" ((request tx-request) (block block-id :optional t)
                    (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "execute a call on the state after BLOCK"
  (prepare-call request block "eth_call" :state-overrides state-overrides :block-overrides block-overrides)
  (unsupported-execution "eth_call"))

(defrpc "eth_estimateGas" ((request tx-request) (block block-id :optional t)
                           (state-overrides state-override :optional t) (block-overrides block-overrides :optional t))
  "binary-search the gas a call needs"
  (prepare-call request block "eth_estimateGas" :state-overrides state-overrides :block-overrides block-overrides)
  (unsupported-execution "eth_estimateGas"))

(defrpc "eth_createAccessList" ((request tx-request) (block block-id :optional t)
                                (state-overrides state-override :optional t))
  "the access list a call touches and its gas"
  (prepare-call request block "eth_createAccessList" :state-overrides state-overrides)
  (unsupported-execution "eth_createAccessList"))

(defparameter *max-simulate-blocks* 256 "reth max_simulate_blocks default")

(defun simulate (payload block what)
  (when (> (length (getf payload :block-state-calls)) *max-simulate-blocks*)
    (rpc-fail -38026 "too many blocks"))
  (when (null (getf payload :block-state-calls)) (rpc-fail +invalid-params+ "calls are empty."))
  (let* ((n (block-id-number (or block :latest)))
         (b (and n (<= n chain:*head*) (chain-block n))))
    (unless b (rpc-fail -38020 (format nil "block not found: ~A" (block-id-text (or block :latest)))))
    (state-at (list :hash (cblock-hash b)))
    (executor-call-api what)
    (unsupported-execution what)))

(defrpc "eth_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "execute calls in simulated blocks on top of BLOCK"
  (simulate payload block "eth_simulateV1"))

(defrpc "tempo_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "eth_simulateV1 with the metadata of the TIP-20 tokens involved"
  (simulate payload block "tempo_simulateV1"))

(defrpc "eth_callMany" ((bundles bundles) (context state-context :optional t)
                        (state-overrides state-override :optional t))
  "execute bundles of calls one after another at a block position"
  (when (null bundles) (rpc-fail +invalid-params+ "bundles are empty."))
  (let* ((target (or (getf context :block-number) :latest))
         (n (block-id-number target)))
    (unless (and n (<= n chain:*head*)) (header-not-found target))
    (let* ((b (chain-block n))
           (index (getf context :transaction-index :all))
           (all (or (eq index :all) (>= index (length (cblock-txs b))))))
      ;; All transactions replayed: the state after the block itself; otherwise its parent.
      (state-at (if all (list :hash (cblock-hash b)) (max 0 (1- n))))
      (executor-call-api "eth_callMany")
      (unsupported-execution "eth_callMany"))))

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

(defrpc "debug_intermediateRoots" ((hash hash) (options raw :optional t))
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

(defrpc "debug_traceBlock" ((raw hex-data) (options raw :optional t))
  "trace every transaction of an RLP-encoded block on its parent's state"
  (let ((block (decode-raw-block raw)))
    (state-at (list :hash (h:header-parent-hash (blocks:block-header block))))
    (replay-unavailable "debug_traceBlock")))
