(defpackage #:clempo.rpcserver
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:ws #:clempo.websocket) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain))
  (:documentation "HTTP/1.1 and WebSocket JSON-RPC on one port, on sb-bsd-sockets with a thread
per connection. HTTP answers as the reference does (POST with Content-Type application/json,
405 and 415 otherwise); a GET with Upgrade: websocket switches the connection to frames,
where requests are answered in order and subscriptions push eth_subscription
notifications from a writer thread per websocket, fed by PUBLISH-HEAD through a bounded
queue.

Every input a client controls is bounded before it is buffered: a request line or header
line (+HEADER-LINE-MAX+), the header count (+HEADER-LINES-MAX+), a body, chunked or not, and
a websocket message (+BODY-MAX+, reth's 15 MiB default --rpc.max-request-size, answered as
jsonrpsee answers it: 413 with code -32007), its fragments (WS:*FRAGMENTS-MAX*), control
frames (125 bytes, RFC 6455), a response (RPC::*RESPONSE-MAX*, reth's 160 MiB, code -32008),
subscriptions per connection (+SUBSCRIPTIONS-MAX+, reth's 1024, code -32006) and
connections (MAX-CONNECTIONS). Time is bounded too: a connection waits at most
*IDLE-SECONDS* for its next request, a request's head and body must arrive within
*REQUEST-SECONDS* of its first byte, a websocket frame within *REQUEST-SECONDS* of its
first byte, and a response or notification must be taken by the client within
*WRITE-SECONDS*, so a slow or silent client (slowloris) holds its slot for a bounded time.")
  (:export #:start-server #:stop-server #:server-port #:server-backend #:publish-head
           #:connection #:connection-subscriptions #:send-notification #:*server*
           #:new-subscription-id #:notify-head #:after-response #:log-event
           #:claim-consensus-slot #:release-consensus-slot
           #:check-subscription-room #:reserve-subscription #:add-subscription
           #:remove-subscription-if
           #:+header-line-max+ #:+header-lines-max+ #:+body-max+
           #:+subscriptions-max+ #:*idle-seconds* #:*request-seconds* #:*write-seconds*))

(in-package #:clempo.rpcserver)

(defconstant +header-line-max+ 8192 "Longest request line or header line, in bytes.")
(defconstant +header-lines-max+ 128 "Most header (or chunk trailer) lines in one request.")
(defconstant +body-max+ (* 15 1024 1024)
  "Largest request body or websocket message: reth's RPC_DEFAULT_MAX_REQUEST_SIZE_MB.")
(defconstant +subscriptions-max+ 1024 "reth's RPC_DEFAULT_MAX_SUBS_PER_CONN.")

(defparameter *idle-seconds* 60 "How long a connection may wait between requests.")
(defparameter *request-seconds* 30
  "How long a request's head and body (or a websocket frame) may take to arrive once begun.")
(defparameter *write-seconds* 60 "How long a client may take to accept a response.")

(defvar *server* nil "The server serving the current request.")

(defvar *after-response* nil
  "While a websocket request runs: thunks to call, in order, once its response is sent.")

(defun after-response (thunk)
  "Call THUNK after the current websocket request's response is written (a subscription
starts there, so no notification precedes the response that names it)."
  (setf *after-response* (append *after-response* (list thunk))))

(defstruct (server (:constructor %make-server))
  socket port backend (connections '())
  (consensus-slots 0)
  (lock (sb-thread:make-mutex :name "rpc server"))
  (running t) acceptor (max-connections 256)
  (log *error-output*) (log-lock (sb-thread:make-mutex :name "rpc log")))

(defconstant +outbox-capacity+ 256
  "New heads a websocket subscriber may fall behind before the oldest is skipped: reth's
PER_BLOCK_FEED_CAPACITY, the depth of its per-block broadcast feeds.")

(defstruct (connection (:constructor make-connection (socket stream)))
  socket stream
  (send-lock (sb-thread:make-mutex :name "ws send"))
  (websocket-p nil)
  ;; Replaced whole (never mutated in place) under SUBSCRIPTIONS-LOCK, so the writer thread
  ;; may read the current list without the lock.
  (subscriptions '()) (reserved 0) (consensus-slots 0)
  (subscriptions-lock (sb-thread:make-mutex :name "ws subscriptions"))
  ;; New heads waiting for the writer thread: (number certificate seen . subscriptions).
  (outbox '()) (outbox-length 0) (skipped 0)
  (outbox-lock (sb-thread:make-mutex :name "ws outbox"))
  (outbox-ready (sb-thread:make-waitqueue))
  (closed nil) (writer nil) (thread nil))

(defgeneric notify-head (backend connection number subscriptions certificate seen)
  (:documentation "Send SUBSCRIPTIONS' notifications for head NUMBER and its optional certificate.")
  (:method (backend connection number subscriptions certificate seen)
    (declare (ignore backend connection number subscriptions certificate seen))
    nil))

(defun claim-consensus-slot (connection)
  (sb-thread:with-mutex ((server-lock *server*))
    (cond ((>= (connection-consensus-slots connection) 4)
           (rpc:rpc-fail -32006 "too many consensus subscriptions on this connection" 4))
          ((>= (server-consensus-slots *server*) 256)
           (rpc:rpc-fail -32009 "consensus subscription limit reached, try again later"))
          (t (incf (connection-consensus-slots connection))
             (incf (server-consensus-slots *server*))))))

(defun release-consensus-slot (connection)
  (sb-thread:with-mutex ((server-lock *server*))
    (decf (connection-consensus-slots connection))
    (decf (server-consensus-slots *server*))))

;;; Subscriptions of a connection

(defun check-subscription-room (connection)
  "Fail as jsonrpsee does once CONNECTION has +SUBSCRIPTIONS-MAX+ subscriptions, counting
those accepted in the current request that start after its response."
  (sb-thread:with-mutex ((connection-subscriptions-lock connection))
    (when (>= (+ (length (connection-subscriptions connection)) (connection-reserved connection))
              +subscriptions-max+)
      (rpc:rpc-fail -32006 "Too many subscriptions on the connection"
                    (format nil "Exceeded max limit of ~D" +subscriptions-max+)))))

(defun reserve-subscription (connection)
  "Count a subscription accepted now and added (ADD-SUBSCRIPTION) after the response."
  (sb-thread:with-mutex ((connection-subscriptions-lock connection))
    (incf (connection-reserved connection))))

(defun add-subscription (connection subscription)
  "Start SUBSCRIPTION, reserved by RESERVE-SUBSCRIPTION."
  (sb-thread:with-mutex ((connection-subscriptions-lock connection))
    (decf (connection-reserved connection))
    (setf (connection-subscriptions connection)
          (cons subscription (connection-subscriptions connection)))))

(defun remove-subscription-if (connection predicate)
  "Remove and return the first subscription of CONNECTION satisfying PREDICATE, or NIL."
  (sb-thread:with-mutex ((connection-subscriptions-lock connection))
    (let ((sub (find-if predicate (connection-subscriptions connection))))
      (when sub
        (setf (connection-subscriptions connection) (remove sub (connection-subscriptions connection))))
      sub)))

(defun log-event (fmt &rest args)
  "Write one line to the current server's log (its LOG stream, *ERROR-OUTPUT* by default)."
  (let ((stream (if *server* (server-log *server*) *error-output*)))
    (when stream
      (flet ((emit () (format stream "~&rpc: ~?~%" fmt args) (force-output stream)))
        (if *server* (sb-thread:with-mutex ((server-log-lock *server*)) (emit)) (emit))))))

(defun new-subscription-id ()
  (format nil "0x~(~32,'0x~)" (octets->integer (clempo.io:random-octets 16))))

;;; Deadlines

(define-condition connection-timeout (error) ()
  (:report "the client did not send or accept data in time"))

(defmacro with-io-deadline ((seconds) &body body)
  "Run BODY; a socket read or write still blocked SECONDS after entry signals CONNECTION-TIMEOUT."
  `(handler-case (sb-sys:with-deadline (:seconds ,seconds) ,@body)
     (sb-sys:deadline-timeout () (error 'connection-timeout))))

(defun send-text (connection text)
  (sb-thread:with-mutex ((connection-send-lock connection))
    (with-io-deadline (*write-seconds*)
      (ws:write-frame (connection-stream connection) ws:+op-text+ (string->octets text)))))

(defun send-notification (connection method subscription result)
  "Push {jsonrpc, method, params: {subscription, result}} on a websocket connection."
  (send-text connection
             (clempo.json:to-json-string
              (clempo.json:jobject "jsonrpc" "2.0" "method" method
                                   "params" (clempo.json:jobject "subscription" subscription
                                                                 "result" result)))))

;;; HTTP

(define-condition bad-request (error)
  ((status :initarg :status :reader bad-request-status)
   (body :initarg :body :initform "" :reader bad-request-body))
  (:report (lambda (c s) (format s "HTTP ~D" (bad-request-status c)))))

(defun reject (status &optional (body ""))
  (error 'bad-request :status status :body body))

(defun too-large ()
  (reject 413 (format nil "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32007,\"message\":\"Request is too big\",\"data\":\"Exceeded max limit of ~D\"}}"
                      +body-max+)))

(defun read-line-crlf (stream &optional (limit +header-line-max+))
  "The next line without its CR LF, NIL at end of stream; a line longer than LIMIT bytes
is refused (431) before it is buffered further."
  (let ((out (make-string-output-stream)) (any nil) (n 0))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   ((> (incf n) limit) (reject 431))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun read-request-head (stream)
  "Returns (values method path headers) or NIL at end of stream. Header names are lowercased.
Signals BAD-REQUEST for a malformed request line or header, or one over the limits."
  (let ((line (loop repeat +header-lines-max+
                    for l = (read-line-crlf stream)
                    while (and l (zerop (length l)))
                    finally (return l))))
    (when line
      (let* ((sp1 (position #\Space line)) (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
        (unless (and sp1 sp2 (plusp sp1)) (reject 400))
        (values (subseq line 0 sp1)
                (subseq line (1+ sp1) sp2)
                (loop for count from 0
                      for h = (read-line-crlf stream)
                      until (or (null h) (zerop (length h)))
                      when (>= count +header-lines-max+) do (reject 431)
                      collect (let ((colon (position #\: h)))
                                (unless colon (reject 400))
                                (cons (string-downcase (string-trim " " (subseq h 0 colon)))
                                      (string-trim " " (subseq h (1+ colon)))))))))))

(defun header (headers name) (cdr (assoc name headers :test #'string=)))

(defun read-exactly (stream n)
  (let ((o (make-octets n)))
    (unless (= n (read-sequence o stream)) (error 'end-of-file :stream stream))
    o))

(defun parse-size (text radix)
  "TEXT as a non-negative integer in RADIX, or a 400 refusal."
  (let ((text (string-trim " " text)))
    (unless (and (plusp (length text)) (<= (length text) 16)
                 (every (lambda (c) (digit-char-p c radix)) text))
      (reject 400))
    (parse-integer text :radix radix)))

(defun read-chunked (stream)
  (let ((parts '()) (total 0))
    (loop for line = (or (read-line-crlf stream 1024) (error 'end-of-file :stream stream))
          for size = (parse-size (subseq line 0 (or (position #\; line) (length line))) 16)
          until (zerop size)
          do (when (> (incf total size) +body-max+) (too-large))
             (push (read-exactly stream size) parts)
             (unless (equal "" (read-line-crlf stream 2)) (reject 400)))
    (loop repeat (1+ +header-lines-max+)
          for l = (read-line-crlf stream)
          until (or (null l) (zerop (length l)))
          finally (when (and l (plusp (length l))) (reject 431)))
    (apply #'octets-concat (nreverse parts))))

(defun read-body (stream headers)
  (let ((len (header headers "content-length"))
        (te (header headers "transfer-encoding")))
    (cond ((and len te) (reject 400))   ; both framings: refused (request smuggling)
          (te (unless (string-equal te "chunked") (reject 400))
              (read-chunked stream))
          (len (let ((n (parse-size len 10)))
                 (when (> n +body-max+) (too-large))
                 (read-exactly stream n)))
          (t (make-octets 0)))))

(defun write-http-response (stream status reason content-type body &key extra-headers)
  (let ((bytes (if (stringp body) (string->octets body) body)))
    (with-io-deadline (*write-seconds*)
      (write-sequence
       (string->octets
        (with-output-to-string (h)
          (format h "HTTP/1.1 ~D ~A~C~C" status reason #\Return #\Newline)
          (when content-type (format h "content-type: ~A~C~C" content-type #\Return #\Newline))
          (format h "content-length: ~D~C~C" (length bytes) #\Return #\Newline)
          (loop for (k . v) in extra-headers do (format h "~A: ~A~C~C" k v #\Return #\Newline))
          (format h "~C~C" #\Return #\Newline)))
       stream)
      (write-sequence bytes stream)
      (force-output stream))))

(defun reason (status)
  (case status (200 "OK") (400 "Bad Request") (405 "Method Not Allowed")
    (413 "Payload Too Large") (415 "Unsupported Media Type")
    (431 "Request Header Fields Too Large") (t "Error")))

(defun json-content-type-p (value)
  (and value
       (let ((main (string-trim " " (subseq value 0 (or (position #\; value) (length value))))))
         (string-equal main "application/json"))))

(defun run-body (text)
  "Answer one JSON-RPC body: (values status response), inside the backend's read lock."
  (let ((backend (and *server* (server-backend *server*))))
    (if backend
        (chain:with-backend-read (backend) (rpc:handle-body text))
        (rpc:handle-body text))))

(defun websocket-request-p (method headers)
  (and (string-equal method "GET") (string-equal (or (header headers "upgrade") "") "websocket")))

(defun read-request (stream)
  "The next request: (values method headers body), NIL at end of stream. Its head and body
must arrive within *REQUEST-SECONDS*."
  (with-io-deadline (*request-seconds*)
    (multiple-value-bind (method path headers) (read-request-head stream)
      (declare (ignore path))
      (when method
        (values method headers (unless (websocket-request-p method headers) (read-body stream headers)))))))

(defun serve-http-request (method headers body stream)
  "Answer one HTTP request; returns T to keep the connection open."
  (let ((keep (not (string-equal (or (header headers "connection") "") "close"))))
    (cond ((string-equal method "POST")
           (if (json-content-type-p (header headers "content-type"))
               (multiple-value-bind (status response) (run-body (octets->string body))
                 (write-http-response stream status (reason status) "application/json; charset=utf-8" response))
               (write-http-response stream 415 (reason 415) "text/plain"
                                    "Supplied content type is not allowed. Content-Type: application/json is required")))
          (t (write-http-response stream 405 (reason 405) "text/plain"
                                  "Used HTTP Method is not allowed. POST is required")))
    keep))

;;; WebSocket

(defun serve-websocket (connection headers stream)
  (let ((key (header headers "sec-websocket-key")))
    (unless key (reject 400))
    (with-io-deadline (*write-seconds*)
      (write-sequence
       (string->octets
        (format nil "HTTP/1.1 101 Switching Protocols~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Accept: ~A~C~C~C~C"
                #\Return #\Newline #\Return #\Newline #\Return #\Newline (ws:accept-key key)
                #\Return #\Newline #\Return #\Newline))
       stream)
      (force-output stream))
    (setf (connection-websocket-p connection) t
          (connection-writer connection)
          (let ((server *server*))
            (sb-thread:make-thread (lambda () (write-notifications server connection))
                                   :name "rpc websocket writer")))
    (flet ((control (opcode payload)
             ;; pings may come between the fragments of a message; pongs need no answer
             (when (= opcode ws:+op-ping+)
               (sb-thread:with-mutex ((connection-send-lock connection))
                 (with-io-deadline (*write-seconds*)
                   (ws:write-frame stream ws:+op-pong+ payload)))))
           (close-with (payload)
             (ignore-errors (sb-thread:with-mutex ((connection-send-lock connection))
                              (with-io-deadline (*write-seconds*)
                                (ws:write-frame stream ws:+op-close+ payload))))))
      (loop
        (multiple-value-bind (opcode payload)
            (handler-case (let ((ws:*message-max* +body-max+)
                                (ws:*frame-seconds* *request-seconds*)
                                (ws:*require-mask* t))
                            (ws:read-message stream #'control))
              (ws:frame-error (e)
                (close-with (integer->octets (ws:frame-error-code e) 2))
                (return)))
          (cond ((or (null opcode) (= opcode ws:+op-close+))
                 (close-with (or payload (make-octets 0)))
                 (return))
                (t
                 (let ((rpc:*connection* connection) (*after-response* '()))
                   (multiple-value-bind (status response) (run-body (octets->string payload))
                     (declare (ignore status))
                     (unless (string= response "null") (send-text connection response)))
                   (mapc #'funcall *after-response*)))))))))

;;; Subscription notifications: PUBLISH-HEAD queues, a writer thread per websocket sends.

(defun close-connection (connection)
  "Stop CONNECTION's writer and wake its reader: both see the socket shut down."
  (sb-thread:with-mutex ((connection-outbox-lock connection))
    (setf (connection-closed connection) t)
    (sb-thread:condition-broadcast (connection-outbox-ready connection)))
  (ignore-errors (sb-bsd-sockets:socket-shutdown (connection-socket connection) :direction :io)))

(defun next-head (connection)
  "Wait for the oldest queued head; NIL once CONNECTION is closed."
  (let ((lock (connection-outbox-lock connection)))
    (sb-thread:with-mutex (lock)
      (loop until (or (connection-closed connection) (connection-outbox connection))
            do (sb-thread:condition-wait (connection-outbox-ready connection) lock))
      (unless (connection-closed connection)
        (decf (connection-outbox-length connection))
        (pop (connection-outbox connection))))))

(defun write-notifications (server connection)
  "CONNECTION's writer thread: render and send each queued head's notifications for the
subscriptions still running. A client that cannot be written to, or does not take a
notification within *WRITE-SECONDS*, closes the connection; any other failure is logged and
the next head is sent."
  (let ((*server* server))
    (loop for item = (next-head connection)
          while item
          do (destructuring-bind (number certificate seen . subscriptions) item
               (let* ((current (connection-subscriptions connection))
                      (running (remove-if-not (lambda (s) (member s current)) subscriptions)))
                 (when running
                   (handler-case (notify-head (server-backend server) connection number running certificate seen)
                     ((or stream-error sb-bsd-sockets:socket-error connection-timeout) ()
                       (close-connection connection)
                       (return))
                     (error (e) (log-event "notifications for block ~D failed: ~A" number e)))))))))

(defun queue-head (connection number certificate seen)
  "Queue head NUMBER for CONNECTION's subscriptions; returns the number of heads it skipped
so far when this one pushed the oldest out, else NIL."
  (let ((subscriptions (reverse (connection-subscriptions connection))))
    (when subscriptions
      (sb-thread:with-mutex ((connection-outbox-lock connection))
        (unless (connection-closed connection)
          (setf (connection-outbox connection)
                (nconc (connection-outbox connection) (list (list* number certificate seen subscriptions))))
          (sb-thread:condition-notify (connection-outbox-ready connection))
          (when (> (incf (connection-outbox-length connection)) +outbox-capacity+)
            (pop (connection-outbox connection))
            (decf (connection-outbox-length connection))
            (incf (connection-skipped connection))))))))

;;; Connections and the listener

(defun wait-for-request (connection)
  "True once CONNECTION has input (or end of stream) within *IDLE-SECONDS*."
  (or (listen (connection-stream connection))
      (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor (connection-socket connection))
                                   :input *idle-seconds*)))

(defun serve-connection (server connection)
  (let ((stream (connection-stream connection))
        (*server* server))
    (unwind-protect
         (handler-case
             (loop
               (unless (wait-for-request connection) (return))
               (multiple-value-bind (method headers body)
                   (handler-case (read-request stream)
                     (bad-request (e)
                       (let ((status (bad-request-status e)))
                         (write-http-response stream status (reason status)
                                              (if (= status 413) "application/json; charset=utf-8" "text/plain")
                                              (bad-request-body e)
                                              :extra-headers '(("connection" . "close"))))
                       (return)))
                 (unless method (return))
                 (if (websocket-request-p method headers)
                     (progn (serve-websocket connection headers stream) (return))
                     (unless (serve-http-request method headers body stream) (return)))))
           ;; A broken, closed, slow or hostile connection ends its thread, never the process.
           (error () nil))
      (close-connection connection)
      (let ((writer (connection-writer connection)))
        (when writer (sb-thread:join-thread writer :default nil :timeout 10)))
      (ignore-errors (close stream :abort t))
      (ignore-errors (sb-bsd-sockets:socket-close (connection-socket connection)))
      (sb-thread:with-mutex ((server-lock server))
        (decf (server-consensus-slots server) (connection-consensus-slots connection))
        (setf (server-connections server) (remove connection (server-connections server)))))))

(defun accept-loop (server)
  (loop while (server-running server)
        do (let ((client (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                           (error () nil))))
             (when (and client (server-running server))
               (setf (sb-bsd-sockets:sockopt-tcp-nodelay client) t
                     ;; reads and writes wait in SBCL, where deadlines reach them
                     (sb-bsd-sockets:non-blocking-mode client) t)
               (let ((connection (make-connection
                                  client (sb-bsd-sockets:socket-make-stream
                                          client :input t :output t :element-type '(unsigned-byte 8)
                                                 :buffering :full))))
                 (sb-thread:with-mutex ((server-lock server))
                   (if (>= (length (server-connections server)) (server-max-connections server))
                       (ignore-errors (sb-bsd-sockets:socket-close client))
                       (progn
                         (push connection (server-connections server))
                         ;; Calls and traces run the EVM on this thread: give it its own scratch.
                         (setf (connection-thread connection)
                               (sb-thread:make-thread
                                (lambda () (clempo.evm:with-evm-thread (serve-connection server connection)))
                                :name "rpc connection"))))))))))

(defun start-server (&key (host "127.0.0.1") (port 8545) backend (max-connections 256)
                       (log *error-output*))
  "Listen on HOST:PORT (0 picks a free port) and serve BACKEND, logging to the stream LOG
\(NIL for none). Returns the server; STOP-SERVER joins every thread it started."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address host) port)
    (sb-bsd-sockets:socket-listen socket 128)
    (let ((server (%make-server :socket socket
                                :port (nth-value 1 (sb-bsd-sockets:socket-name socket))
                                :backend backend :max-connections max-connections :log log)))
      (setf (server-acceptor server)
            (sb-thread:make-thread (lambda () (accept-loop server)) :name "rpc accept"))
      server)))

(defun stop-server (server)
  "Stop accepting, close every open connection and join the acceptor and connection threads
\(each connection thread joins its websocket writer)."
  (setf (server-running server) nil)
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  ;; Wake a blocked accept on platforms where closing the socket does not.
  (ignore-errors
   (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
     (sb-bsd-sockets:socket-connect s #(127 0 0 1) (server-port server))
     (sb-bsd-sockets:socket-close s)))
  (ignore-errors (sb-thread:join-thread (server-acceptor server) :timeout 5))
  (let ((connections (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server)))))
    (mapc #'close-connection connections)
    (dolist (c connections)
      (let ((thread (connection-thread c)))
        (when thread (sb-thread:join-thread thread :default nil :timeout 10)))))
  server)

(defun publish-head (server number &key certificate)
  "A new head NUMBER is committed: queue it for every websocket with subscriptions and
return at once. Each connection's writer thread renders and sends, so a slow or stalled
client never holds back the caller; one that falls +OUTBOX-CAPACITY+ heads behind skips
the oldest, as a lagging receiver of reth's per-block feeds does (the connection stays)."
  (let ((*server* server) (seen (clempo.io:now-ms)))
    (dolist (c (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server))))
      (when (connection-websocket-p c)
        (let ((skipped (queue-head c number certificate seen)))
          ;; reth logs every lagging receive at debug level; once per capacity's worth here
          (when (and skipped (= 1 (mod skipped +outbox-capacity+)))
            (log-event "websocket subscriber lagging behind: ~D heads skipped" skipped)))))))
