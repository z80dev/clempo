(defpackage #:clempo.rpc.subscribe
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:server #:clempo.rpcserver) (#:logs #:clempo.rpc.logs))
  (:documentation "eth_subscribe and eth_unsubscribe as reth's pubsub answers them through
jsonrpsee. Over HTTP both are Internal error whatever their params. Over a websocket
eth_subscribe reads the kind (serde_json's externally tagged unit enum) and the optional
params (alloy's Params, deserialized through a serde_json Value: object keys sorted and
deduplicated, errors without positions), answers a fresh id, and starts the subscription
once that answer is written. A kind whose params reth refuses after accepting
(logs with a boolean, ...) gets its id and never notifies. NOTIFY-HEAD, called on the
connection's writer thread for each head PUBLISH-HEAD queued, renders under the backend's
read lock and then pushes newHeads headers, matching logs, transaction receipts; syncing
says false once, at its start, and never changes.")
  (:export #:subscription #:subscription-id #:subscription-kind #:subscription-filter
           #:subscribe #:unsubscribe))

(in-package #:clempo.rpc.subscribe)

(defstruct (subscription (:constructor make-subscription (id kind &optional filter)))
  id
  kind      ; :new-heads :logs :new-pending-transactions :syncing :transaction-receipts
  filter)   ; a log-filter for :logs, the list of transaction hashes (octets) for receipts

;;; Methods jsonrpsee registers as subscriptions

(defmacro defpubsub (name (params) doc &body body)
  "Declare NAME as a subscription method: over HTTP it answers Internal error before reading
its params; over a websocket BODY runs with PARAMS bound to the raw params src (NIL when
absent), the envelope's array check left to BODY."
  `(progn
     (setf (gethash ,name *methods*)
           (clempo.rpc::make-rpc-method
            :name ,name :doc ,doc :params :raw
            :function (lambda (,params)
                        (unless *connection* (internal-error))
                        ,@body)))
     ,name))

(defun params-sequence (src)
  "The params src positioned inside its array, or NIL when absent or null; any other value
fails as jsonrpsee's sequence parser does."
  (cond ((or (null src) (eq (serde:peek-kind src) :null)) nil)
        ((eq (serde:peek-kind src) :array) (incf (serde:src-pos src)) src)
        (t (invalid-params (format nil "Invalid params. Expected one of '[', ']' or ',' but found '~A'"
                                   (serde:peek src))))))

;;; The kind: alloy SubscriptionKind through serde_json's deserialize_enum

(defparameter +kinds+
  '(("newHeads" . :new-heads) ("logs" . :logs) ("newPendingTransactions" . :new-pending-transactions)
    ("syncing" . :syncing) ("transactionReceipts" . :transaction-receipts)))

(defun variant (src name)
  (or (cdr (assoc name +kinds+ :test #'string=))
      (serde:serde-fail-here src "unknown variant `~A`, expected one of ~{`~A`~^, ~}"
                             name (mapcar #'car +kinds+))))

(defun fail-at-peek (src fmt &rest args)
  "serde_json's peek_error: the position counts the peeked character."
  (serde:peek src)
  (apply #'serde:serde-fail-at src (1+ (serde:src-pos src)) fmt args))

(defparam subscription-kind (src) "alloy SubscriptionKind: \"newHeads\" or {\"newHeads\": null}"
  (case (serde:peek-kind src)
    (:string (variant src (serde:read-string src)))
    (:object
     (incf (serde:src-pos src))
     (unless (eq (serde:peek-kind src) :string) (fail-at-peek src "expected value"))
     (let ((kind (variant src (serde:read-string src))))
       (serde:peek src) (incf (serde:src-pos src))              ; the colon
       (unless (serde:read-null-p src) (serde:invalid-type src "unit"))
       (unless (eql (serde:peek src) #\}) (serde:serde-fail-here src "expected value"))
       (incf (serde:src-pos src))
       kind))
    (t (fail-at-peek src "expected value"))))

;;; The params: alloy Params::from_json_value over a serde_json Value

(defun read-serde-value (src)
  "The next value as serde_json's Value holds it: object keys sorted and deduplicated (the
last wins), numbers as serde_json reads them."
  (ecase (serde:peek-kind src)
    (:string (serde:read-string src))
    (:number (serde:read-number src))
    ((:true :false :null) (let ((kind (serde:peek-kind src))) (serde:skip-value src) kind))
    (:object (let ((pairs '()))
               (serde:do-object (key src)
                 (let ((value (read-serde-value src)))
                   (setf pairs (acons key value (remove key pairs :key #'car :test #'string=)))))
               (cons :object (sort pairs #'string< :key #'car))))
    (:array (let ((items '()))
              (serde:do-array (i src) (push (read-serde-value src) items))
              (coerce (nreverse items) 'simple-vector)))))

(defun unexpected-value (v)
  (cond ((eq v :true) "boolean `true`") ((eq v :false) "boolean `false`") ((eq v :null) "null")
        ((stringp v) (format nil "string ~A" (serde:rust-debug-string v)))
        ((numberp v) (serde:number-unexpected v))
        ((json:jobject-p v) "map")
        (t "sequence")))

(defun strip-position (message)
  "MESSAGE without serde_json's trailing \" at line L column C\"."
  (let ((at (search " at line " message :from-end t)))
    (if (and at
             (let ((rest (subseq message (+ at 9))))
               (let ((col (search " column " rest)))
                 (and col (plusp col)
                      (every #'digit-char-p (subseq rest 0 col))
                      (< (+ col 8) (length rest))
                      (every #'digit-char-p (subseq rest (+ col 8)))))))
        (subseq message 0 at)
        message)))

(defmacro from-value (&body body)
  "Run BODY, a src reader over a re-serialized Value; its errors lose their positions."
  `(handler-case (progn ,@body)
     (serde:serde-error (e) (serde:serde-fail "~A" (strip-position (serde:serde-error-message e))))))

(defun value-src (v) (serde:make-src (json:to-json-string v)))

(defun value-filter (v)
  "alloy Filter from Value V."
  (unless (json:jobject-p v)
    (serde:serde-fail "invalid type: ~A, expected Filter object" (unexpected-value v)))
  ;; Sorted, address is the first field the visitor sees.
  (let ((address (json:jget v "address" :absent)))
    (unless (eq address :absent)
      (when (eq (logs::variadic address 20 #'hex->integer) :fail)
        (serde:serde-fail "~A" logs::+variadic-error+))))
  (from-value (logs:read-log-filter (value-src v))))

(defun value-receipts-params (v)
  "alloy TransactionReceiptsParams from Value V: the list of hashes (octets), NIL for all."
  (let ((hashes (json:jget v "transactionHashes")))
    (cond ((eq hashes :null) nil)
          ((and (vectorp hashes) (not (stringp hashes)))
           (map 'list (lambda (h) (from-value (read-param 'hash (value-src h)))) hashes))
          (t (serde:serde-fail "invalid type: ~A, expected a sequence" (unexpected-value hashes))))))

(defparam subscription-params (src) "alloy Params: (:bool b), (:logs filter) or (:receipts hashes)"
  (let ((v (read-serde-value src)))
    (cond ((or (eq v :null) (and (vectorp v) (not (stringp v)) (zerop (length v)))) :none)
          ((member v '(:true :false)) (list :bool (eq v :true)))
          ((and (json:jobject-p v) (assoc "transactionHashes" (cdr v) :test #'string=))
           (list :receipts (value-receipts-params v)))
          (t (list :logs (value-filter v))))))

;;; eth_subscribe and eth_unsubscribe

(defun default-filter () (logs:read-log-filter (serde:make-src "{}")))

(defun subscription-for (id kind params)
  "The subscription reth runs for KIND with PARAMS, or NIL when it refuses them after
accepting (the id is answered and nothing follows)."
  (destructuring-bind (&optional tag value) (if (eq params :none) '() params)
    (ecase kind
      (:new-heads (make-subscription id kind))
      (:syncing (make-subscription id kind))
      (:logs (case tag
               (:bool nil)
               (:logs (make-subscription id kind value))
               (t (make-subscription id kind (default-filter)))))
      (:new-pending-transactions (when (member tag '(nil :bool)) (make-subscription id kind)))
      (:transaction-receipts (case tag
                               ((nil) (make-subscription id kind '()))
                               (:receipts (make-subscription id kind value))
                               (t nil))))))

(defun subscribe (connection kind params)
  "Answer a subscription id; the subscription starts after the answer is written."
  (let* ((id (server:new-subscription-id))
         (sub (subscription-for id kind params)))
    (when sub
      (server:after-response
       (lambda ()
         (push sub (server:connection-subscriptions connection))
         (when (eq kind :syncing)
           (server:send-notification connection "eth_subscription" id :false)))))
    id))

(defun unsubscribe (connection id)
  (let ((sub (find id (server:connection-subscriptions connection)
                   :key #'subscription-id :test #'equal)))
    (when sub
      (setf (server:connection-subscriptions connection)
            (remove sub (server:connection-subscriptions connection))))
    (and sub t)))

(defpubsub "eth_subscribe" (params)
  "start a newHeads, logs, newPendingTransactions, syncing or transactionReceipts subscription"
  (let* ((seq (params-sequence params))
         (kind (clempo.rpc::next-param seq 'subscription-kind nil nil))
         (more (clempo.rpc::next-param seq 'subscription-params t (lambda () :none))))
    (subscribe *connection* kind more)))

(defpubsub "eth_unsubscribe" (params)
  "end a subscription of this connection: true when it was running"
  ;; jsonrpsee reads exactly one string id and answers false for anything else
  (let ((id (handler-case
                (when (and params (eq (serde:peek-kind params) :array))
                  (let ((items (serde:read-value params)))
                    (and (= (length items) 1) (stringp (svref items 0)) (svref items 0))))
              (serde:serde-error () nil))))
    (if (and id (unsubscribe *connection* id)) :true :false)))

;;; Notifications for a new head

(defun head-json (b)
  "reth's newHeads item: the header with the block's RLP size, ordered as the reference."
  (let ((pairs (cdr (header-json (cblock-header b) (cblock-hash b))))
        (size (cons "size" (q (cblock-size b)))))
    (cons :object
          (if (assoc "timestampMillis" pairs :test #'string=)
              (loop for p in pairs
                    when (string= (car p) "timestampMillis") collect size
                    collect p)
              (append pairs (list size))))))

(defun filter-matches-block-p (f b)
  "alloy Filter::matches_block: the block hash, or the from/to numbers."
  (if (logs:filter-block-hash f)
      (octets= (logs:filter-block-hash f) (cblock-hash b))
      (let ((from (logs:filter-from f)) (to (logs:filter-to f)) (n (cblock-number b)))
        (and (not (and (integerp from) (> from n)))
             (cond ((integerp to) (>= to n))
                   ((eq to :earliest) nil)
                   (t t))))))

(defgeneric notifications (kind subscription block)
  (:documentation "The results to push to SUBSCRIPTION for new block BLOCK, in order.")
  (:method (kind subscription block) (declare (ignore kind subscription block)) '()))

(defmethod notifications ((kind (eql :new-heads)) subscription b)
  (declare (ignore subscription))
  (list (head-json b)))

(defmethod notifications ((kind (eql :logs)) subscription b)
  (let ((f (subscription-filter subscription)))
    (when (filter-matches-block-p f b)
      (logs:filter-logs-in-block f b))))

(defmethod notifications ((kind (eql :transaction-receipts)) subscription b)
  (let* ((hashes (subscription-filter subscription))
         (receipts (loop for rc across (receipt-jsons b)
                         for h across (cblock-tx-hashes b)
                         when (or (null hashes) (member h hashes :test #'octets=))
                           collect rc)))
    (when receipts (list (coerce receipts 'simple-vector)))))

(defun render-head (backend number subscriptions)
  "((subscription . results) ...) for new head NUMBER, rendered under BACKEND's read lock. A
subscription whose results fail (missing receipts, ...) is logged and skipped, as reth logs
a conversion error and goes on."
  (chain:with-backend-read (backend)
    (let ((b (chain-block number)))
      (if (null b)
          (progn (server:log-event "block ~D is not available for subscriptions" number) '())
          (loop for sub in subscriptions
                for results = (handler-case (notifications (subscription-kind sub) sub b)
                                (error (e)
                                  (server:log-event "~(~A~) subscription ~A, block ~D: ~A"
                                                    (subscription-kind sub) (subscription-id sub) number e)
                                  '()))
                when results collect (cons sub results))))))

(defmethod server:notify-head ((backend chain:backend) connection number subscriptions)
  ;; Sent after the read lock is released: a stalled client must not hold back a writer.
  (loop for (sub . results) in (render-head backend number subscriptions)
        do (dolist (result results)
             (server:send-notification connection "eth_subscription" (subscription-id sub) result))))
