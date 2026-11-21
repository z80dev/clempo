(in-package #:clempo.dev)

;;; R5: replay the recorded corpus (rpc-corpus.lisp) against a clempo JSON-RPC server and
;;; compare every answer with the reference's: HTTP status and JSON (json-equal: object key
;;; order ignored). "live" and "node" cases are compared by shape (same keys and types,
;;; same error code), since their values depend on the moment or on the reference node.
;;;
;;;   clempo-dev rpc-index                 build the block and transaction hash index
;;;   clempo-dev rpcdiff [-v] [-method M] [-suite S] [-url U]
;;;   clempo-dev score -rung R5

(defparameter *head-dependent-methods*
  '("eth_blockNumber" "eth_gasPrice" "eth_maxPriorityFeePerGas" "eth_blobBaseFee" "eth_syncing"
    "net_peerCount")
  "Methods whose answer follows the head; their cases are compared by shape.")

(defun corpus-cases (&key suite method)
  (loop for path in (append (sort (directory (corpus-path "corpus-*.jsonl")) #'string< :key #'namestring)
                            (list (asdf:system-relative-pathname "clempo-dev" "../../tests/dev/rpc-extra.jsonl")))
        append (loop for c in (read-corpus-file path)
                     when (and (or (null suite) (string= suite (clempo.json:jget c "suite")))
                               (or (null method) (string= method (clempo.json:jget c "method"))))
                       collect c)))

(defun mentions-head-p (response)
  "An error message that quotes the reference's head block number (\"..., head 50058714\")."
  (let ((message (clempo.json:jget* response "error" "message")))
    (and (stringp message)
         (let ((at (search "head " message :from-end t)))
           (and at (< (+ at 5) (length message)) (digit-char-p (char message (+ at 5))))))))

(defun reference-timeout-p (response)
  "The reference gave no answer: its database read transaction hit MDBX's timeout (-96000).
Deep-history debug_intermediateRoots cases were recorded so under 16 recording threads;
re-sent one at a time the reference answers them (taking 60-300 s each, see FRONTIER R5), so
the error depends on the reference's load, not on the request."
  (let ((message (clempo.json:jget* response "error" "message")))
    (and (stringp message) (search "read transaction has been timed out (-96000)" message))))

(defun case-compare-class (c)
  (let ((class (clempo.json:jget c "class")))
    (cond ((reference-timeout-p (clempo.json:jget c "response")) "reference-timeout")
          ((and (string= class "stable")
                (or (member (clempo.json:jget c "method") *head-dependent-methods* :test #'string=)
                    (mentions-head-p (clempo.json:jget c "response"))))
           "live")
          (t class))))

(defun json-shape-equal (a b)
  "Same JSON types and object keys; error objects must share their code; an empty array
matches only an empty array."
  (cond ((clempo.json:jobject-p a)
         (and (clempo.json:jobject-p b)
              (= (length (cdr a)) (length (cdr b)))
              (loop for (k . v) in (cdr a)
                    always (let ((cell (assoc k (cdr b) :test #'string=)))
                             (and cell
                                  (if (string= k "code") (equal v (cdr cell)) (json-shape-equal v (cdr cell))))))))
        ((and (vectorp a) (not (stringp a)))
         (and (vectorp b) (not (stringp b))
              (eq (zerop (length a)) (zerop (length b)))
              (or (zerop (length a)) (json-shape-equal (aref a 0) (aref b 0)))))
        ((stringp a) (stringp b))
        ((numberp a) (numberp b))
        (t (eq a b))))

(defparameter *witness-methods* '("debug_executionWitness" "debug_executionWitnessByBlockHash")
  "Methods answering an ExecutionWitness; compared by WITNESS-EQUAL whatever their class.")

(defun witness-canonical (result)
  "An ExecutionWitness with the reference's hash-map orders taken out: state and codes sorted,
keys grouped as address -> sorted slots (reth draws all three from maps with a random hasher,
so two calls answer the same block in different orders); headers keep their order."
  (flet ((sorted (field) (sort (coerce (clempo.json:jget result field) 'list) #'string<)))
    (let ((groups '()))
      (loop for k across (clempo.json:jget result "keys")
            do (if (= (length k) 42)
                   (push (list k) groups)
                   (push k (cdr (first groups)))))
      (list (sorted "state") (sorted "codes")
            (sort (mapcar (lambda (g) (cons (car g) (sort (cdr g) #'string<))) groups) #'string< :key #'car)
            (coerce (clempo.json:jget result "headers") 'list)))))

(defun witness-equal (a b)
  "Two debug_executionWitness answers: equal values (WITNESS-CANONICAL), or equal errors."
  (let ((ra (clempo.json:jget a "result")) (rb (clempo.json:jget b "result")))
    (if (and (clempo.json:jobject-p ra) (clempo.json:jobject-p rb))
        (equal (witness-canonical ra) (witness-canonical rb))
        (clempo.json:json-equal a b))))

(defun head-relative (want got)
  "WANT with every string equal to its result.head number or hash replaced by GOT's: an
eth_capabilities answer recorded at the reference's head, compared at clempo's."
  (let ((map (loop for key in '("number" "hash")
                   collect (cons (clempo.json:jget* want "result" "head" key)
                                 (clempo.json:jget* got "result" "head" key)))))
    (labels ((walk (v)
               (cond ((clempo.json:jobject-p v) (cons :object (loop for (k . x) in (cdr v) collect (cons k (walk x)))))
                     ((stringp v) (let ((cell (assoc v map :test #'equal))) (if cell (cdr cell) v)))
                     ((vectorp v) (map 'vector #'walk v))
                     (t v))))
      (walk want))))

(defun case-category (c)
  "How case C is graded: \"value\" (the recorded answer compared exactly, or a \"verified\"
proof checked against the reference's recorded root and values), \"shape\" (it follows the
head or the moment: same keys, types and error code), \"node\" (it depends on the
reference node itself: compared by shape, never counted toward R5), or
\"reference-timeout\"."
  (let ((class (case-compare-class c)))
    (cond ((member class '("stable" "verified") :test #'string=) "value")
          ((string= class "node") "node")
          ((string= class "reference-timeout") class)
          (t "shape"))))

(defun case-matches (c status response)
  "A reference-timeout case matches an answer with a result: the reference recorded none to
compare with. A \"headRelative\" case is value-checked after HEAD-RELATIVE. Witness
answers compare by value, their map orders aside (WITNESS-EQUAL)."
  (and (eql status (clempo.json:jget c "status"))
       (let ((class (case-compare-class c)))
         (cond ((member (clempo.json:jget c "method") *witness-methods* :test #'string=)
                (witness-equal (clempo.json:jget c "response") response))
               ((string= class "stable")
                (clempo.json:json-equal (if (and (eq :true (clempo.json:jget c "headRelative"))
                                                 (clempo.json:jobject-p response))
                                            (head-relative (clempo.json:jget c "response") response)
                                            (clempo.json:jget c "response"))
                                        response))
               ((integerp (clempo.json:jget c "eventHeight"))
                (and (equal "consensus_event" (clempo.json:jget response "method"))
                     (equal "finalized" (clempo.json:jget* response "params" "result" "type"))
                     (json-shape-equal (clempo.json:jget c "response") response)))
               ((string= class "reference-timeout")
                (and (clempo.json:jobject-p response) (assoc "result" (cdr response) :test #'string=) t))
               (t (json-shape-equal (clempo.json:jget c "response") response))))))

(defmacro with-clempo-server ((url-var server &key url upstream) &body body)
  "Bind URL-VAR to URL, or start an in-process SERVER over the archive for BODY."
  `(let* ((,server (unless ,url
                     (clempo.rpcserver:start-server
                      :port 0 :backend (clempo.rpc.chain:make-archive-backend :upstream ,upstream))))
          (,url-var (or ,url (format nil "http://127.0.0.1:~D/" (clempo.rpcserver:server-port ,server)))))
     (unwind-protect (progn ,@body)
       (when ,server (clempo.rpcserver:stop-server ,server)))))

(defun call-with-rpc-websocket (url function)
  (multiple-value-bind (scheme host port path) (clempo.http:parse-url url)
    (unless (member scheme '("http" "ws" "https" "wss") :test #'string=)
      (error "rpcdiff WebSocket replay requires an http(s) or ws(s) target"))
    (multiple-value-bind (stream socket)
        (let ((clempo.http:*read-timeout* 10)) (clempo.http:open-connection scheme host port))
      (unwind-protect
           (progn
             (progn
               (let ((key (clempo.websocket:base64-encode (clempo.io:random-octets 16)))
                     (crlf (coerce '(#\Return #\Newline) 'string)))
                 (write-sequence
                  (string->octets
                   (format nil "GET ~A HTTP/1.1~AHost: ~A:~D~AUpgrade: websocket~AConnection: Upgrade~ASec-WebSocket-Key: ~A~ASec-WebSocket-Version: 13~A~A"
                           path crlf host port crlf crlf crlf key crlf crlf crlf))
                  stream)
                 (force-output stream)
                 (multiple-value-bind (version status headers) (clempo.rpcserver::read-request-head stream)
                   (declare (ignore version))
                   (unless (and (equal status "101")
                                (equal (clempo.rpcserver::header headers "sec-websocket-accept")
                                       (clempo.websocket:accept-key key)))
                     (error "WebSocket handshake refused: ~A" status)))
                 (funcall function stream))))
        (ignore-errors (close stream :abort t))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun ws-receive-json (stream)
  (multiple-value-bind (opcode payload)
      (clempo.websocket:read-message
       stream (lambda (op bytes)
                (when (= op clempo.websocket:+op-ping+)
                  (clempo.websocket:write-frame stream clempo.websocket:+op-pong+ bytes :masked t))))
    (unless (eql opcode clempo.websocket:+op-text+) (error "Expected a WebSocket text message"))
    (clempo.json:parse-json (octets->string payload))))

(defun ws-rpc (stream request)
  (let ((id (clempo.json:jget (clempo.json:parse-json request) "id")))
    (clempo.websocket:write-frame stream clempo.websocket:+op-text+ (string->octets request) :masked t)
    (loop for reply = (ws-receive-json stream)
          when (equal id (clempo.json:jget reply "id")) return reply)))

(defun replay-ws-case (url server c)
  (call-with-rpc-websocket
   url
   (lambda (stream)
     (let* ((request (clempo.json:jget c "request"))
            (setup (clempo.json:jget c "setup" 0))
            (height (clempo.json:jget c "eventHeight" nil))
            (id (loop repeat setup
                      for response = (ws-rpc stream (rpc-request-string "consensus_subscribe" '()))
                      for sub = (clempo.json:jget response "result")
                      unless (stringp sub) do (error "WebSocket setup failed: ~S" response)
                      collect sub into ids
                      finally (return (first ids)))))
       (values
        101
        (cond (height
               (when server
                 ;; Ensure the ack's after-response handler ran before publishing.
                 (ws-rpc stream (rpc-request-string "web3_clientVersion" '()))
                 (clempo.rpcserver:publish-head server height))
               (loop for event = (ws-receive-json stream)
                     when (equal id (clempo.json:jget* event "params" "subscription"))
                       return event))
              ((eq :true (clempo.json:jget c "bindSubscription"))
               (let ((req (clempo.json:parse-json request)))
                 (setf (cdr (assoc "params" (cdr req) :test #'string=)) (vector id))
                 (ws-rpc stream (clempo.json:to-json-string req))))
              (t (ws-rpc stream request))))))))

(defun unsafe-probe-p (method)
  (member method *probe-unsafe* :test #'string=))

(defun verified-case-p (c) (equal "verified" (clempo.json:jget c "class")))

(defun run-rpcdiff (url server cases &key (threads 16))
  "Replay CASES. Mutating requests are only sent to the server rpcdiff owns. Verified proof
cases run last, against the owned server with its proof window opened (the reference's
window is 0; an external URL keeps its own and fails them)."
  (append
   (parallel-map
    (lambda (c)
      (multiple-value-bind (status response)
          (cond ((and (not server) (unsafe-probe-p (clempo.json:jget c "method")))
                 (values nil "mutation not sent to external URL"))
                ((equal "ws" (clempo.json:jget c "transport"))
                 (handler-case (replay-ws-case url server c)
                   (error (e) (values nil (princ-to-string e)))))
                ((clempo.json:jget c "filterSetup")
                 (call-with-filter url (clempo.json:jget c "filterSetup") (clempo.json:jget c "request")))
                (t (post-raw url (clempo.json:jget c "request") :retries 0)))
        (list c status response (case-matches c status response))))
    (remove-if #'verified-case-p cases) threads)
   (let ((verified (remove-if-not #'verified-case-p cases))
         (window clempo.rpc.state:*eth-proof-window*))
     (unwind-protect
          (progn
            (when server (setf clempo.rpc.state:*eth-proof-window* most-positive-fixnum))
            (parallel-map (lambda (c)
                            (multiple-value-bind (status response ok)
                                (handler-case (replay-verified-case url c)
                                  (error (e) (values nil (princ-to-string e) nil)))
                              (list c status response ok)))
                          verified 2))
       (setf clempo.rpc.state:*eth-proof-window* window)))))

(defun served-reference-methods (url server)
  "(values served missing) among the reference's method names, on their native transport."
  (let ((served '()) (missing '()))
    (dolist (m (reference-methods))
      (let ((request (rpc-request-string m '())))
        (if (cond ((and (not server) (unsafe-probe-p m)) nil)
                  ((member m '("consensus_subscribe" "consensus_unsubscribe") :test #'string=)
                   (handler-case
                       (let ((result (clempo.json:jget
                                      (call-with-rpc-websocket url (lambda (stream) (ws-rpc stream request)))
                                      "result")))
                         (if (string= m "consensus_subscribe") (stringp result) (eq result :false)))
                     (error () nil)))
                  (t (not (method-not-found-p (nth-value 1 (post-raw url request :retries 0))))))
            (push m served) (push m missing))))
    (values (nreverse served) (nreverse missing))))

(defun rpcdiff-summary (results)
  "Per reference method: (method cases identical value-cases value-identical). Labels that
are not method names (envelope, batch and unknown-method cases) count as cases, never as
methods. Returns (values rows methods-ok categories), METHODS-OK counting the methods with
at least one value case whose every case matches, CATEGORIES an alist category ->
(identical . total)."
  (let ((table (make-hash-table :test 'equal))
        (known (reference-methods))
        (categories (list (list "value" 0 0) (list "shape" 0 0) (list "node" 0 0)
                          (list "reference-timeout" 0 0))))
    (dolist (r results)
      (let* ((c (first r)) (ok (fourth r))
             (category (assoc (case-category c) categories :test #'string=))
             (m (clempo.json:jget c "method")))
        (incf (third category))
        (when ok (incf (second category)))
        (when (member m known :test #'string=)
          (let ((cell (or (gethash m table) (setf (gethash m table) (list 0 0 0 0)))))
            (incf (first cell))
            (when ok (incf (second cell)))
            (when (string= (first category) "value")
              (incf (third cell))
              (when ok (incf (fourth cell))))))))
    (let ((rows (sort (loop for m being the hash-keys of table using (hash-value v)
                            collect (cons m v))
                      #'string< :key #'first)))
      (values rows
              (count-if (lambda (row) (destructuring-bind (m n ok vn vok) row
                                        (declare (ignore m vok))
                                        (and (= n ok) (plusp vn))))
                        rows)
              (loop for (name ok total) in categories collect (list* name ok total))))))

(defun rpcdiff-suites (results)
  "Per suite, in corpus order: a JSON object of \"suite\": \"identical/total\"."
  (let ((rows '()))
    (dolist (r results)
      (let* ((s (clempo.json:jget (first r) "suite"))
             (cell (or (assoc s rows :test #'string=) (car (push (list s 0 0) rows)))))
        (incf (second cell))
        (when (fourth r) (incf (third cell)))))
    (cons :object (loop for (s n ok) in (reverse rows) collect (cons s (format nil "~D/~D" ok n))))))

(defun json-difference (want got &optional (path "$"))
  "The first place WANT and GOT differ, as \"path: want X got Y\", or NIL."
  (cond ((and (clempo.json:jobject-p want) (clempo.json:jobject-p got))
         (or (loop for (k . v) in (cdr want)
                   for cell = (assoc k (cdr got) :test #'string=)
                   thereis (if cell
                               (json-difference v (cdr cell) (format nil "~A.~A" path k))
                               (format nil "~A.~A: missing" path k)))
             (loop for (k) in (cdr got)
                   unless (assoc k (cdr want) :test #'string=)
                     return (format nil "~A.~A: unexpected" path k))))
        ((and (vectorp want) (not (stringp want)) (vectorp got) (not (stringp got)))
         (or (loop for i from 0 below (min (length want) (length got))
                   thereis (json-difference (aref want i) (aref got i) (format nil "~A[~D]" path i)))
             (unless (= (length want) (length got))
               (format nil "~A: length ~D vs ~D" path (length want) (length got)))))
        ((clempo.json:json-equal want got) nil)
        (t (format nil "~A: want ~A got ~A" path (short-json want 120) (short-json got 120)))))

(defun short-json (v &optional (n 400))
  (let ((s (if (stringp v) v (clempo.json:to-json-string v))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 n) "...") s)))

(defun rpcdiff (&key url suite method verbose (upstream (clempo.jsonrpc:local-rpc-url)) (limit 3))
  "Run the R5 comparison; returns a clempo.json object with the score."
  (let ((cases (corpus-cases :suite suite :method method)))
    (with-clempo-server (server-url server :url url :upstream upstream)
      (let ((results (run-rpcdiff server-url server cases)))
        (multiple-value-bind (rows methods-ok categories) (rpcdiff-summary results)
          (multiple-value-bind (served missing) (served-reference-methods server-url server)
            (flet ((cat (name) (cdr (assoc name categories :test #'string=))))
              (let* ((value (cat "value"))
                     (known (reference-methods))
                     ;; reference methods with no value case, recorded cases or not
                     (unvalued (remove-if (lambda (m)
                                            (let ((row (assoc m rows :test #'string=)))
                                              (and row (plusp (fourth row)))))
                                          known)))
                (when verbose
                  (dolist (row rows)
                    (destructuring-bind (m n ok vn vok) row
                      (format t "~:[  ~;ok~] ~40A ~D/~D (value ~D/~D)~%" (and (= n ok) (plusp vn)) m ok n vok vn)))
                  (let ((shown (make-hash-table :test 'equal)))
                    (dolist (r results)
                      (destructuring-bind (c status response ok) r
                        (let ((m (clempo.json:jget c "method")))
                          (when (and (not ok) (< (gethash m shown 0) limit))
                            (incf (gethash m shown 0))
                            (format t "~&--- ~A [~A] ~A~%  want ~A ~A~%  got  ~A ~A~%  diff ~A~%"
                                    m (case-compare-class c) (short-json (clempo.json:jget c "request") 300)
                                    (clempo.json:jget c "status") (short-json (clempo.json:jget c "response"))
                                    status (short-json response)
                                    (json-difference (clempo.json:jget c "response") response)))))))
                  (format t "methods without a value-checked case: ~{~A~^ ~}~%" unvalued)
                  (format t "missing reference methods: ~{~A~^ ~}~%" missing))
                (clempo.json:jobject
                 ;; R5 is scored on value-identical cases only, as goempo excludes Live and
                 ;; NodeLocal from its identical count; shape and node cases are reported apart.
                 "score" (format nil "~D/~D value-identical cases; ~D/~D methods" (car value) (cdr value)
                                 methods-ok (length known))
                 "cases" (car value) "casesTotal" (cdr value)
                 "methods" methods-ok "methodsTotal" (length known)
                 "methodsWithoutValueCase" (coerce unvalued 'simple-vector)
                 "categories" (cons :object (loop for (name ok . total) in categories
                                                  when (plusp total)
                                                    collect (cons name (format nil "~D/~D" ok total))))
                 "referenceMethodsServed" (length served)
                 "referenceMethods" (+ (length served) (length missing))
                 "suites" (rpcdiff-suites results))))))))))

(defcommand "rpcdiff" (args)
  "R5: replay the RPC corpus against clempo: rpcdiff [-v] [-suite S] [-method M] [-url U] [-limit N]"
  (let ((result (rpcdiff :url (arg args :url) :suite (arg args :suite) :method (arg args :method)
                         :verbose (arg-flag args :v) :limit (arg-int args :limit 3))))
    (write-line (clempo.json:to-json-string result))
    0))

(defrung "R5" (args)
  "share of the reference's RPC methods answering identically on the recorded corpus"
  (rpcdiff :url (arg args :url)))

(defcommand "rpc-index" (args)
  "build the block and transaction hash index over the archive: rpc-index [-threads N]"
  (let ((start (clempo.io:monotonic-ticks)))
    (multiple-value-bind (blocks txs) (clempo.rpc.chain:build-index :threads (arg-int args :threads 16))
      (format t "index: ~D blocks, ~D transactions in ~,1F s -> ~A~%" blocks txs
              (/ (- (clempo.io:monotonic-ticks) start) internal-time-units-per-second)
              (clempo.rpc.chain:index-dir))
      0)))

(defcommand "rpc-serve" (args)
  "serve JSON-RPC over the archive: rpc-serve [-port 8546] [-host H] [-upstream URL]"
  (let* ((backend (clempo.rpc.chain:make-archive-backend :upstream (arg args :upstream)))
         (server (clempo.rpcserver:start-server :host (arg args :host "127.0.0.1")
                                                :port (arg-int args :port 8546) :backend backend)))
    (log-line "rpc: serving blocks 0..~D on port ~D" (clempo.rpc.chain:backend-head backend)
              (clempo.rpcserver:server-port server))
    (loop (sleep 3600))))
