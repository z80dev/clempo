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

;;; A connection's input: the socket is read into the connection's own buffer, one read
;;; system call per arrival, and request lines are found by scanning those octets (no byte
;;; at a time through a stream). Waits go through WAIT-UNTIL-FD-USABLE, where deadlines
;;; (WITH-IO-DEADLINE) reach them as they reach a stream's.

(defconstant +inbuf-size+ 16384
  "Room for the request lines and header lines being parsed (each at most
+HEADER-LINE-MAX+ bytes); a body is read into its own array.")

(defstruct (inbuf (:constructor make-inbuf (fd)))
  (fd -1 :type fixnum)
  (octets (make-octets +inbuf-size+) :type octets)
  (start 0 :type fixnum)                ; the first unconsumed byte
  (end 0 :type fixnum)                  ; after the last byte read
  (eof nil))

(defstruct (connection (:constructor make-connection
                           (socket stream &aux (in (and socket (make-inbuf (sb-bsd-sockets:socket-file-descriptor socket)))))))
  socket stream in
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

(defun fd-read (fd o start end)
  "Read into O[START,END) what FD has, waiting for at least one byte. Returns the count, 0 at
end of stream."
  (declare (type fixnum fd start end) (type octets o))
  (invariant (< start end) "empty read")
  (loop
    (multiple-value-bind (count errno)
        (sb-sys:with-pinned-objects (o)
          (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap o) start) (- end start)))
      (cond (count (return count))
            ((eql errno sb-unix:ewouldblock) (sb-sys:wait-until-fd-usable fd :input))
            ((eql errno sb-unix:eintr))
            (t (error 'sb-int:simple-stream-error :stream nil
                                                  :format-control "socket read failed: ~A"
                                                  :format-arguments (list (sb-int:strerror errno))))))))

(defun inbuf-fill (in)
  "Read more of the socket after IN's buffered bytes (moved to the front first). NIL at end
of stream."
  (unless (inbuf-eof in)
    (let ((o (inbuf-octets in)) (start (inbuf-start in)) (end (inbuf-end in)))
      (when (plusp start)
        (replace o o :start2 start :end2 end)
        (setf end (- end start) (inbuf-start in) 0 (inbuf-end in) end))
      (let ((count (fd-read (inbuf-fd in) o end (length o))))
        (if (zerop count)
            (progn (setf (inbuf-eof in) t) nil)
            (incf (inbuf-end in) count))))))

(defun latin1-string (o start end)
  (declare (type octets o) (type fixnum start end))
  (let ((s (make-string (- end start))))
    (loop for i of-type fixnum from start below end
          for j of-type fixnum from 0
          do (setf (schar s j) (code-char (aref o i))))
    s))

(defun read-line-crlf (in &optional (limit +header-line-max+))
  "The next line without its CR LF (trailing CRs dropped), NIL at end of stream; a line
longer than LIMIT bytes is refused (431) before it is buffered further."
  (declare (type fixnum limit))
  (let ((scanned 0))
    (declare (type fixnum scanned))
    (loop
      (let* ((o (inbuf-octets in)) (start (inbuf-start in)) (end (inbuf-end in))
             (lf (position 10 o :start (+ start scanned) :end end)))
        (cond (lf
               (when (> (- lf start) limit) (reject 431))
               (setf (inbuf-start in) (1+ lf))
               (return (latin1-string o start (loop with e of-type fixnum = lf
                                                    while (and (> e start) (= 13 (aref o (1- e))))
                                                    do (decf e)
                                                    finally (return e)))))
              ((> (- end start) limit) (reject 431))
              ((not (inbuf-fill in))
               (let ((start (inbuf-start in)) (end (inbuf-end in)))
                 (setf (inbuf-start in) end)
                 (return (and (> end start) (latin1-string (inbuf-octets in) start end)))))
              (t (setf scanned (- end start))))))))

(defun read-request-head (in)
  "Returns (values method path headers) or NIL at end of stream. Header names are lowercased.
Signals BAD-REQUEST for a malformed request line or header, or one over the limits."
  (let ((line (loop repeat +header-lines-max+
                    for l = (read-line-crlf in)
                    while (and l (zerop (length l)))
                    finally (return l))))
    (when line
      (let* ((sp1 (position #\Space line)) (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
        (unless (and sp1 sp2 (plusp sp1)) (reject 400))
        (values (subseq line 0 sp1)
                (subseq line (1+ sp1) sp2)
                (loop for count from 0
                      for h = (read-line-crlf in)
                      until (or (null h) (zerop (length h)))
                      when (>= count +header-lines-max+) do (reject 431)
                      collect (let ((colon (position #\: h)))
                                (unless colon (reject 400))
                                (cons (string-downcase (string-trim " " (subseq h 0 colon)))
                                      (string-trim " " (subseq h (1+ colon)))))))))))

(defun header (headers name) (cdr (assoc name headers :test #'string=)))

(defun read-exactly (in n)
  "The next N octets: those buffered, then the rest read straight into the result."
  (let* ((o (make-octets n))
         (have (min n (- (inbuf-end in) (inbuf-start in)))))
    (replace o (inbuf-octets in) :start2 (inbuf-start in) :end2 (+ (inbuf-start in) have))
    (incf (inbuf-start in) have)
    (loop with pos = have
          while (< pos n)
          do (let ((count (if (inbuf-eof in) 0 (fd-read (inbuf-fd in) o pos n))))
               (when (zerop count)
                 (setf (inbuf-eof in) t)
                 (error 'end-of-file :stream nil))
               (incf pos count)))
    o))

(defun parse-size (text radix)
  "TEXT as a non-negative integer in RADIX, or a 400 refusal."
  (let ((text (string-trim " " text)))
    (unless (and (plusp (length text)) (<= (length text) 16)
                 (every (lambda (c) (digit-char-p c radix)) text))
      (reject 400))
    (parse-integer text :radix radix)))

(defun read-chunked (in)
  (let ((parts '()) (total 0))
    (loop for line = (or (read-line-crlf in 1024) (error 'end-of-file :stream nil))
          for size = (parse-size (subseq line 0 (or (position #\; line) (length line))) 16)
          until (zerop size)
          do (when (> (incf total size) +body-max+) (too-large))
             (push (read-exactly in size) parts)
             (unless (equal "" (read-line-crlf in 2)) (reject 400)))
    (loop repeat (1+ +header-lines-max+)
          for l = (read-line-crlf in)
          until (or (null l) (zerop (length l)))
          finally (when (and l (plusp (length l))) (reject 431)))
    (apply #'octets-concat (nreverse parts))))

(defun read-body (in headers)
  (let ((len (header headers "content-length"))
        (te (header headers "transfer-encoding")))
    (cond ((and len te) (reject 400))   ; both framings: refused (request smuggling)
          (te (unless (string-equal te "chunked") (reject 400))
              (read-chunked in))
          (len (let ((n (parse-size len 10)))
                 (when (> n +body-max+) (too-large))
                 (read-exactly in n)))
          (t (make-octets 0)))))

(defun write-http-response (stream status reason content-type body &key extra-headers)
  "Write the response head and BODY (a string or octets) and send them in one write."
  (let* ((bytes (if (stringp body) (string->octets body) body))
         (crlf (coerce '(#\Return #\Newline) 'string))
         (head (apply #'concatenate 'string
                      "HTTP/1.1 " (princ-to-string status) " " reason crlf
                      (append (when content-type (list "content-type: " content-type crlf))
                              (list "content-length: " (princ-to-string (length bytes)) crlf)
                              (loop for (k . v) in extra-headers
                                    append (list (princ-to-string k) ": " (princ-to-string v) crlf))
                              (list crlf)))))
    (with-io-deadline (*write-seconds*)
      (write-sequence (string->octets head) stream)
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

(defun run-body (octets)
  "Answer one JSON-RPC body: (values status response), inside the backend's read lock. A body
that is not UTF-8 is a parse error, as the reference answers it (400 unless it starts as an
object or array)."
  (let ((text (handler-case (octets->string octets)
                (error ()
                  (let ((first (find-if-not (lambda (b) (member b '(32 9 10 13))) octets)))
                    (return-from run-body
                      (values (if (member first '(123 91)) 200 400)
                              "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}"))))))
        (backend (and *server* (server-backend *server*))))
    (if backend
        (chain:with-backend-read (backend) (rpc:handle-body text))
        (rpc:handle-body text))))

(defun websocket-request-p (method headers)
  (and (string-equal method "GET") (string-equal (or (header headers "upgrade") "") "websocket")))

(defun read-request (in)
  "The next request: (values method headers body), NIL at end of stream. Its head and body
must arrive within *REQUEST-SECONDS*."
  (with-io-deadline (*request-seconds*)
    (multiple-value-bind (method path headers) (read-request-head in)
      (declare (ignore path))
      (when method
        (values method headers (unless (websocket-request-p method headers) (read-body in headers)))))))

(defun serve-http-request (method headers body stream)
  "Answer one HTTP request; returns T to keep the connection open."
  (let ((keep (not (string-equal (or (header headers "connection") "") "close"))))
    (cond ((string-equal method "POST")
           (if (json-content-type-p (header headers "content-type"))
               (multiple-value-bind (status response) (run-body body)
                 (write-http-response stream status (reason status) "application/json; charset=utf-8" response))
               (write-http-response stream 415 (reason 415) "text/plain"
                                    "Supplied content type is not allowed. Content-Type: application/json is required")))
          (t (write-http-response stream 405 (reason 405) "text/plain"
                                  "Used HTTP Method is not allowed. POST is required")))
    keep))

;;; WebSocket

(defclass prefixed-input (sb-gray:fundamental-binary-input-stream)
  ((prefix :initarg :prefix :type octets) (pos :initform 0 :type fixnum) (inner :initarg :inner))
  (:documentation "Frames read after the upgrade: octets the connection buffered past the
upgrade request first, then the socket's stream."))

(defmethod stream-element-type ((s prefixed-input)) '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((s prefixed-input))
  (with-slots (prefix pos inner) s
    (if (< pos (length prefix))
        (prog1 (aref prefix pos) (incf pos))
        (read-byte inner nil :eof))))

(defmethod sb-gray:stream-read-sequence ((s prefixed-input) seq &optional (start 0) end)
  (with-slots (prefix pos inner) s
    (let* ((end (or end (length seq)))
           (n (min (- end start) (- (length prefix) pos))))
      (replace seq prefix :start1 start :end1 (+ start n) :start2 pos)
      (incf pos n)
      (if (< (+ start n) end) (read-sequence seq inner :start (+ start n) :end end) end))))

(defun websocket-input (connection stream)
  "The stream frames are read from: STREAM, after any octets already buffered."
  (let ((in (connection-in connection)))
    (if (< (inbuf-start in) (inbuf-end in))
        (prog1 (make-instance 'prefixed-input :inner stream
                                              :prefix (subseq (inbuf-octets in) (inbuf-start in) (inbuf-end in)))
          (setf (inbuf-start in) (inbuf-end in)))
        stream)))

(defun serve-websocket (connection headers stream)
  (let ((key (header headers "sec-websocket-key"))
        (input (websocket-input connection stream)))
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
                            (ws:read-message input #'control))
              (ws:frame-error (e)
                (close-with (integer->octets (ws:frame-error-code e) 2))
                (return)))
          (cond ((or (null opcode) (= opcode ws:+op-close+))
                 (close-with (or payload (make-octets 0)))
                 (return))
                (t
                 (let ((rpc:*connection* connection) (*after-response* '()))
                   (multiple-value-bind (status response) (run-body payload)
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
                     (serious-condition (e) (log-event "notifications for block ~D failed: ~A" number e)))))))))

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
  (let ((in (connection-in connection)))
    (or (< (inbuf-start in) (inbuf-end in))
        (inbuf-eof in)
        (sb-sys:wait-until-fd-usable (inbuf-fd in) :input *idle-seconds*))))

(defun serve-connection (server connection)
  (let ((stream (connection-stream connection))
        (in (connection-in connection))
        (*server* server))
    (unwind-protect
         (handler-case
             (loop
               (unless (wait-for-request connection) (return))
               (multiple-value-bind (method headers body)
                   (handler-case (read-request in)
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
           ;; SERIOUS-CONDITION: stack or heap exhaustion is not an ERROR, and must end only
           ;; this connection.
           (serious-condition () nil))
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
