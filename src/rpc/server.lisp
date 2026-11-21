(defpackage #:clempo.rpcserver
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:ws #:clempo.websocket) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:poll #:clempo.poll))
  (:documentation "HTTP/1.1 and WebSocket JSON-RPC on one port. A bounded pool of workers (one
per available CPU by default), each with its own readiness poller (CLEMPO.POLL: kqueue or
epoll), shares the connections out in turn as they are accepted. On an event a worker reads
what has arrived into the connection's buffer, parses as far as it goes without waiting, and
answers each request completed there, so a slow client never holds a worker; a socket is
watched by its edges, so a request costs a read and a send between waits. A slow call never
holds the other connections of its worker either: once one event has run for *STALL-MS* the
reaper hands that worker's poller to a spare thread (at most *EXTRA-WORKERS-MAX* of them),
as a blocking task is moved off an async runtime's worker, and the stalled thread joins the
spares when its call returns. HTTP answers as
the reference does (POST with Content-Type application/json, 405 and 415 otherwise); a GET
with Upgrade: websocket hands the connection to a thread of its own, where requests are
answered in order and subscriptions push eth_subscription notifications from a writer thread
per websocket, fed by PUBLISH-HEAD through a bounded queue.

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
(defparameter *stall-ms* 20
  "How long one event may run on a worker before its poller is handed to another thread (the
reaper looks this often, so a hand-over comes after one to two periods).")
(defparameter *extra-workers-max* 64
  "Threads a server may start beyond its workers to take over the pollers of stalled ones.")

(defvar *server* nil "The server serving the current request.")

(defvar *after-response* nil
  "While a websocket request runs: thunks to call, in order, once its response is sent.")

(defun after-response (thunk)
  "Call THUNK after the current websocket request's response is written (a subscription
starts there, so no notification precedes the response that names it)."
  (setf *after-response* (append *after-response* (list thunk))))

(defstruct (server (:constructor %make-server))
  socket port backend
  ;; Connections by descriptor (replaced whole when it grows, under LOCK), and as a list.
  (table (make-array 256 :initial-element nil) :type simple-vector)
  (connections '()) (count 0 :type fixnum)
  (consensus-slots 0)
  (lock (sb-thread:make-mutex :name "rpc server"))
  (running t) (max-connections 256)
  ;; a poller per worker; each connection is watched by one, and run by one thread at a time
  (pollers #() :type simple-vector) (next-poller 0 :type fixnum)
  (listen-fd -1) (wake-read -1) (wake-write -1)
  ;; CREW: the thread serving each poller (a WORKER, by poller index); SPARES: threads whose
  ;; poller went on while they ran a slow call, waiting for another; THREADS: every one
  ;; started, for STOP-SERVER. All three under CREW-LOCK.
  (crew #() :type simple-vector) (spares '()) (threads '()) (extra 0 :type fixnum)
  (crew-lock (sb-thread:make-mutex :name "rpc crew"))
  (crew-wait (sb-thread:make-waitqueue))
  (reaper nil)
  (reaper-lock (sb-thread:make-mutex :name "rpc reaper"))
  (reaper-wait (sb-thread:make-waitqueue))
  (log *error-output*) (log-lock (sb-thread:make-mutex :name "rpc log")))

(defconstant +outbox-capacity+ 256
  "New heads a websocket subscriber may fall behind before the oldest is skipped: reth's
PER_BLOCK_FEED_CAPACITY, the depth of its per-block broadcast feeds.")

(defconstant +inbuf-size+ 16384
  "Room for the request lines and header lines being parsed (each at most
+HEADER-LINE-MAX+ bytes) and for a body that fits; a larger body gets its own array.")

(defstruct (inbuf (:constructor make-inbuf ()))
  (octets (make-octets +inbuf-size+) :type octets)
  (start 0 :type fixnum)                ; the first unconsumed byte
  (end 0 :type fixnum)                  ; after the last byte read
  (eof nil))

;;; A request being parsed: what its lines said so far, kept between arrivals so each byte
;;; is scanned once however the request is split.

(defstruct (request (:conc-name req-))
  ;; :START (blank lines before the request line), :HEADERS, :BODY, :CHUNK-SIZE,
  ;; :CHUNK-DATA, :CHUNK-END, :TRAILERS
  (stage :start)
  (scanned 0 :type fixnum)              ; bytes of the current line searched for its LF
  (lines 0 :type fixnum)                ; blank lines, then header lines, then trailers
  (method :other)                       ; :POST, :GET or :OTHER
  (seen 0 :type fixnum)                 ; a bit per header kind: the first of each counts
  (length nil)                          ; Content-Length: an integer, :BAD, or NIL
  (chunked nil)                         ; Transfer-Encoding: :CHUNKED, :OTHER, or NIL
  (json nil) (close nil) (upgrade nil) (key nil)
  (need 0 :type fixnum)                 ; body (or chunk) bytes still to come
  (body nil) (body-end 0 :type fixnum)  ; the body, when it is not in the input buffer
  (total 0 :type fixnum))               ; chunked: the body's size so far

(defun reset-request (req)
  (setf (req-stage req) :start (req-scanned req) 0 (req-lines req) 0 (req-method req) :other
        (req-seen req) 0 (req-length req) nil (req-chunked req) nil (req-json req) nil
        (req-close req) nil (req-upgrade req) nil (req-key req) nil (req-need req) 0
        (req-body req) nil (req-body-end req) 0 (req-total req) 0))

(defstruct (connection (:constructor make-connection (&optional (fd -1))))
  (fd -1 :type fixnum)
  (in (make-inbuf)) (request (make-request))
  poller                                ; the poller watching it
  ;; 0 idle, 1 a thread is running it, 2 running and to be run once more (RUN-CONNECTION)
  (run 0)
  (must-read t)                         ; input may be waiting that no event will announce
  (hangup nil)                          ; an event said the peer ended its side
  (phase :idle)                         ; :IDLE, :REQUEST, :BUSY, :WRITING or :THREAD
  ;; When the reaper ends it (monotonic milliseconds), one word so it reads it whole.
  (deadline most-positive-fixnum :type fixnum)
  (expired nil)
  (out nil) (out-start 0 :type fixnum)  ; response bytes the socket has not taken yet
  (close-after nil)
  stream
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

(defun now-ms ()
  "Monotonic milliseconds."
  (values (floor (clempo.io:monotonic-ticks) (floor internal-time-units-per-second 1000))))

(defun ms-after (seconds) (+ (now-ms) (round (* 1000 seconds))))

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

;;; HTTP requests, parsed from the octets in the connection's buffer without waiting

(define-condition bad-request (error)
  ((status :initarg :status :reader bad-request-status)
   (body :initarg :body :initform "" :reader bad-request-body))
  (:report (lambda (c s) (format s "HTTP ~D" (bad-request-status c)))))

(defun reject (status &optional (body ""))
  (error 'bad-request :status status :body body))

(defun too-large ()
  (reject 413 (format nil "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32007,\"message\":\"Request is too big\",\"data\":\"Exceeded max limit of ~D\"}}"
                      +body-max+)))

(defun latin1-string (o start end)
  (declare (type octets o) (type fixnum start end))
  (let ((s (make-string (- end start))))
    (loop for i of-type fixnum from start below end
          for j of-type fixnum from 0
          do (setf (schar s j) (code-char (aref o i))))
    s))

(declaim (inline octet-position))
(defun octet-position (byte o start end)
  "The index of the first BYTE in O[START,END), or NIL."
  (declare (type (unsigned-byte 8) byte) (type octets o) (type fixnum start end))
  (loop for i of-type fixnum from start below end
        when (= (aref o i) byte) return i))

(defun next-line (c limit)
  "The next line of C's input, consumed: (values start end) of its octets in the input
buffer, without its LF and trailing CRs (a last line cut by the end of stream is taken as it
is); :MORE while its LF has not arrived; NIL at end of stream. A line longer than LIMIT bytes
is refused (431) as soon as that much of it is here."
  (declare (type fixnum limit))
  (let* ((in (connection-in c)) (req (connection-request c))
         (o (inbuf-octets in)) (start (inbuf-start in)) (end (inbuf-end in))
         (lf (octet-position 10 o (+ start (req-scanned req)) end)))
    (declare (type fixnum start end))
    (cond (lf
           (when (> (- lf start) limit) (reject 431))
           (setf (inbuf-start in) (1+ lf) (req-scanned req) 0)
           (values start (loop with e of-type fixnum = lf
                               while (and (> e start) (= 13 (aref o (1- e))))
                               do (decf e)
                               finally (return e))))
          ((> (- end start) limit) (reject 431))
          ((inbuf-eof in)
           (setf (inbuf-start in) end (req-scanned req) 0)
           (and (> end start) (values start end)))
          (t (setf (req-scanned req) (- end start))
             :more))))

(declaim (inline ci=))
(defun ci= (o start end text)
  "Whether O[START,END) spells TEXT (lowercase ASCII), ignoring ASCII case."
  (declare (type octets o) (type fixnum start end) (type (simple-array character (*)) text))
  (and (= (- end start) (length text))
       (loop for i of-type fixnum from start below end
             for j of-type fixnum from 0
             always (= (let ((b (aref o i))) (if (<= 65 b 90) (+ b 32) b)) (char-code (schar text j))))))

(defun trim-spaces (o start end)
  "O[START,END) without leading and trailing spaces: (values start end)."
  (declare (type octets o) (type fixnum start end))
  (loop while (and (< start end) (= 32 (aref o start))) do (incf start))
  (loop while (and (> end start) (= 32 (aref o (1- end)))) do (decf end))
  (values start end))

(defun parse-size (o start end radix)
  "O[START,END), spaces trimmed, as a non-negative integer in RADIX of at most 16 digits, or
:BAD."
  (declare (type octets o) (type fixnum start end))
  (multiple-value-bind (start end) (trim-spaces o start end)
    (if (and (< start end) (<= (- end start) 16)
             (loop for i from start below end always (digit-char-p (code-char (aref o i)) radix)))
        (loop with n = 0 for i from start below end
              do (setf n (+ (* n radix) (digit-char-p (code-char (aref o i)) radix)))
              finally (return n))
        :bad)))

(defun json-content-type-p (o start end)
  "Whether the Content-Type O[START,END) is application/json, parameters aside."
  (declare (type octets o) (type fixnum start end))
  (multiple-value-bind (s e) (trim-spaces o start (or (octet-position 59 o start end) end))
    (ci= o s e "application/json")))

(defun take-request-line (req o start end)
  (declare (type octets o) (type fixnum start end))
  (let* ((sp1 (octet-position 32 o start end))
         (sp2 (and sp1 (octet-position 32 o (1+ sp1) end))))
    (unless (and sp1 sp2 (> sp1 start)) (reject 400))
    (setf (req-method req) (cond ((ci= o start sp1 "post") :post)
                                 ((ci= o start sp1 "get") :get)
                                 (t :other)))))

(defun take-header (req o start end)
  "Note the header line O[START,END) if it is the first of a kind the server reads."
  (declare (type octets o) (type fixnum start end))
  (let ((colon (octet-position 58 o start end)))
    (unless colon (reject 400))
    (multiple-value-bind (ns ne) (trim-spaces o start colon)
      (let ((bit (cond ((ci= o ns ne "content-length") 1)
                       ((ci= o ns ne "transfer-encoding") 2)
                       ((ci= o ns ne "content-type") 4)
                       ((ci= o ns ne "connection") 8)
                       ((ci= o ns ne "upgrade") 16)
                       ((ci= o ns ne "sec-websocket-key") 32))))
        (when (and bit (not (logtest bit (req-seen req))))
          (setf (req-seen req) (logior bit (req-seen req)))
          (multiple-value-bind (vs ve) (trim-spaces o (1+ colon) end)
            (case bit
              (1 (setf (req-length req) (parse-size o vs ve 10)))
              (2 (setf (req-chunked req) (if (ci= o vs ve "chunked") :chunked :other)))
              (4 (setf (req-json req) (json-content-type-p o vs ve)))
              (8 (setf (req-close req) (ci= o vs ve "close")))
              (16 (setf (req-upgrade req) (ci= o vs ve "websocket")))
              (32 (setf (req-key req) (latin1-string o vs ve))))))))))

(defun grow-body (req size)
  "Room in REQ's body array for SIZE bytes in all."
  (let ((body (req-body req)))
    (when (or (null body) (< (length body) size))
      (let ((new (make-octets (min +body-max+ (max size (if body (* 2 (length body)) 0))))))
        (when body (replace new body :end2 (req-body-end req)))
        (setf (req-body req) new)))))

(defun head-done (req)
  "The head has ended: choose how the body is framed. Returns the next stage, or :WEBSOCKET."
  (let ((length (req-length req)) (chunked (req-chunked req)))
    (cond ((and (eq (req-method req) :get) (req-upgrade req)) :websocket)
          ((and length chunked) (reject 400)) ; both framings: refused (request smuggling)
          (chunked (unless (eq chunked :chunked) (reject 400))
                   :chunk-size)
          (length (when (eq length :bad) (reject 400))
                  (when (> length +body-max+) (too-large))
                  (setf (req-need req) length)
                  (when (> length +inbuf-size+) (grow-body req length))
                  :body)
          (t :done))))

(defun advance (c)
  "Parse C's buffered input as far as it goes: :REQUEST once a whole request is here, :MORE
while the rest has yet to arrive, :WEBSOCKET for an upgrade, NIL when the stream ended
first. Signals BAD-REQUEST for a request to refuse."
  (let* ((req (connection-request c)) (in (connection-in c)) (o (inbuf-octets in)))
    (macrolet ((with-line ((s e limit) &body body)
                 `(multiple-value-bind (,s ,e) (next-line c ,limit)
                    (when (eq ,s :more) (return :more))
                    ,@body)))
      (loop
        (case (req-stage req)
          (:start
           (with-line (s e +header-line-max+)
             (cond ((null s) (return nil))
                   ((= s e)
                    (when (>= (incf (req-lines req)) +header-lines-max+) (reject 400)))
                   (t (take-request-line req o s e)
                      (setf (req-stage req) :headers (req-lines req) 0)))))
          (:headers
           (with-line (s e +header-line-max+)
             (if (or (null s) (= s e))
                 (let ((next (head-done req)))
                   (case next
                     (:websocket (return :websocket))
                     (:done (return :request))
                     (t (setf (req-stage req) next))))
                 (progn (when (>= (req-lines req) +header-lines-max+) (reject 431))
                        (incf (req-lines req))
                        (take-header req o s e)))))
          (:body
           (let ((have (- (inbuf-end in) (inbuf-start in))) (body (req-body req)))
             (cond (body
                    (let ((take (min have (req-need req))))
                      (replace body o :start1 (req-body-end req)
                                      :start2 (inbuf-start in) :end2 (+ (inbuf-start in) take))
                      (incf (req-body-end req) take)
                      (incf (inbuf-start in) take)
                      (decf (req-need req) take)
                      (cond ((zerop (req-need req)) (return :request))
                            ((inbuf-eof in) (return nil))
                            (t (return :more)))))
                   ((>= have (req-need req)) (return :request))
                   ((inbuf-eof in) (return nil))
                   (t (return :more)))))
          (:chunk-size
           (with-line (s e 1024)
             (unless s (return nil))
             (let ((size (parse-size o s (or (position 59 o :start s :end e) e) 16)))
               (when (eq size :bad) (reject 400))
               (cond ((zerop size) (setf (req-stage req) :trailers (req-lines req) 0))
                     (t (when (> (incf (req-total req) size) +body-max+) (too-large))
                        (grow-body req (req-total req))
                        (setf (req-need req) size (req-stage req) :chunk-data))))))
          (:chunk-data
           (let ((take (min (req-need req) (- (inbuf-end in) (inbuf-start in)))))
             (replace (req-body req) o :start1 (req-body-end req)
                                       :start2 (inbuf-start in) :end2 (+ (inbuf-start in) take))
             (incf (req-body-end req) take)
             (incf (inbuf-start in) take)
             (cond ((zerop (decf (req-need req) take)) (setf (req-stage req) :chunk-end))
                   ((inbuf-eof in) (return nil))
                   (t (return :more)))))
          (:chunk-end
           (with-line (s e 2)
             (unless (and s (= s e)) (reject 400))
             (setf (req-stage req) :chunk-size)))
          (:trailers
           (with-line (s e +header-line-max+)
             (when (or (null s) (= s e)) (return :request))
             (when (>= (incf (req-lines req)) (1+ +header-lines-max+)) (reject 431)))))))))

(defun request-body (c)
  "The complete request's body: (values octets start end)."
  (let ((req (connection-request c)) (in (connection-in c)))
    (cond ((req-body req) (values (req-body req) 0 (req-body-end req)))
          ((eq (req-stage req) :body)
           (values (inbuf-octets in) (inbuf-start in) (+ (inbuf-start in) (req-need req))))
          (t (values (inbuf-octets in) 0 0)))))

(defvar *text* nil
  "The calling worker's buffer for request text (a base string), or NIL: a body's text lives
there until the worker reads the next one.")

(defconstant +text-keep+ (* 1024 1024) "The largest text buffer a worker keeps.")

(defun body-text (octets start end)
  "OCTETS[START,END) decoded as UTF-8: (values text length). An ASCII body (nearly every
request) becomes a base string, a byte a character, without the general decoder's buffers,
in the worker's *TEXT* when that has room. Signals for bytes that are not UTF-8."
  (declare (type octets octets) (type fixnum start end))
  (invariant (<= 0 start end (length octets)) "body ~D..~D outside ~D octets" start end (length octets))
  (let* ((n (- end start))
         (s (let ((buffer *text*))
              (cond ((and buffer (<= n (length buffer))) buffer)
                    ((and buffer (<= n +text-keep+))
                     (setf *text* (make-string (max n (* 2 (length buffer))) :element-type 'base-char)))
                    (t (make-string n :element-type 'base-char))))))
    (declare (type simple-base-string s) (type fixnum n))
    (flet ((decoded ()
             (let ((text (octets->string octets :start start :end end)))
               (return-from body-text (values text (length text))))))
      ;; a base character is an octet: eight at a time while none has its top bit
      (let ((i start) (j 0))
        (declare (type fixnum i j))
        (sb-sys:with-pinned-objects (octets s)
          (let ((from (sb-sys:vector-sap octets)) (to (sb-sys:vector-sap s)))
            (loop while (<= (+ i 8) end)
                  do (let ((w (sb-sys:sap-ref-64 from i)))
                       (declare (type (unsigned-byte 64) w))
                       (unless (zerop (logand w #x8080808080808080)) (decoded))
                       (setf (sb-sys:sap-ref-64 to j) w)
                       (incf i 8) (incf j 8)))))
        (loop while (< i end)
              do (let ((b (aref octets i)))
                   (when (>= b #x80) (decoded))
                   (setf (schar s j) (code-char b))
                   (incf i) (incf j)))))
    (values s n)))

(defun run-body (octets &optional (start 0) (end (length octets)))
  "Answer one JSON-RPC body, OCTETS[START,END): (values status response), inside the
backend's read lock. A body that is not UTF-8 is a parse error, as the reference answers it
\(400 unless it starts as an object or array)."
  (multiple-value-bind (text length)
      (handler-case (body-text octets start end)
        (error ()
          (let ((first (find-if-not (lambda (b) (member b '(32 9 10 13))) octets
                                    :start start :end end)))
            (return-from run-body
              (values (if (member first '(123 91)) 200 400)
                      "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}")))))
    (let ((backend (and *server* (server-backend *server*))))
      (if backend
          (chain:with-backend-read (backend) (rpc:handle-body text length))
          (rpc:handle-body text length)))))

;;; HTTP responses, rendered into the worker's buffer

(defvar *out* nil "The calling worker's response buffer (octets).")

(defun reason (status)
  (case status (200 "OK") (400 "Bad Request") (405 "Method Not Allowed")
    (413 "Payload Too Large") (415 "Unsupported Media Type")
    (431 "Request Header Fields Too Large") (t "Error")))

(defun put-ascii (o pos s)
  (declare (type octets o) (type fixnum pos) (type simple-string s))
  (loop for ch across s for i of-type fixnum from pos do (setf (aref o i) (char-code ch)))
  (+ pos (length s)))

(defun put-decimal (o pos n)
  (declare (type octets o) (type fixnum pos) (type (integer 0 #.most-positive-fixnum) n))
  (let ((digits (if (zerop n) 1 (loop for k of-type fixnum = n then (floor k 10)
                                      while (plusp k) count t))))
    (loop for i of-type fixnum from (+ pos digits -1) downto pos
          for k of-type fixnum = n then (floor k 10)
          do (setf (aref o i) (+ 48 (mod k 10))))
    (+ pos digits)))

(defun string-span (s)
  "The characters of the string S as (values simple-string start end), without copying a
string with a fill pointer (as a reused response buffer is)."
  (cond ((typep s 'simple-string) (values s 0 (length s)))
        ((and (array-has-fill-pointer-p s) (not (array-displacement s)))
         (values (sb-ext:array-storage-vector s) 0 (fill-pointer s)))
        (t (let ((s (coerce s 'simple-string))) (values s 0 (length s))))))

(defun utf8-length (s start end)
  "The UTF-8 length of S[START,END) (S a simple string), or NIL when it holds a surrogate."
  (declare #.clempo.base:*hot* (type simple-string s) (type fixnum start end))
  (etypecase s
    (simple-base-string (- end start))
    ((simple-array character (*))
     (let ((n 0))
       (declare (type fixnum n))
       (loop for i of-type fixnum from start below end
             do (let ((code (char-code (schar s i))))
                  (incf n (cond ((< code #x80) 1) ((< code #x800) 2)
                                ((<= #xD800 code #xDFFF) (return-from utf8-length nil))
                                ((< code #x10000) 3) (t 4)))))
       n))
    (simple-string nil)))

(defun put-utf8 (o pos s start end)
  "Encode S[START,END), which UTF8-LENGTH measured, into O from POS (O has room for it)."
  (declare #.clempo.base:*hot* (type octets o) (type fixnum pos start end) (type simple-string s))
  (macrolet ((put (b) `(progn (setf (aref o pos) ,b) (incf pos))))
    (etypecase s
      (simple-base-string
       (loop for i of-type fixnum from start below end do (put (char-code (schar s i)))))
      ((simple-array character (*))
       (loop for i of-type fixnum from start below end
             do (let ((code (char-code (schar s i))))
                  (cond ((< code #x80) (put code))
                        ((< code #x800) (put (logior #xC0 (ash code -6)))
                         (put (logior #x80 (logand code #x3F))))
                        ((< code #x10000) (put (logior #xE0 (ash code -12)))
                         (put (logior #x80 (logand (ash code -6) #x3F)))
                         (put (logior #x80 (logand code #x3F))))
                        (t (put (logior #xF0 (ash code -18)))
                           (put (logior #x80 (logand (ash code -12) #x3F)))
                           (put (logior #x80 (logand (ash code -6) #x3F)))
                           (put (logior #x80 (logand code #x3F)))))))))))

(defun copy-ascii (o pos s start end)
  "Copy S[START,END) into O from POS while its characters are ASCII, four at a time where
it can: the index of S reached (END when every one was)."
  (declare #.clempo.base:*hot* (type octets o) (type fixnum pos start end) (type simple-string s))
  (etypecase s
    (simple-base-string
     (loop for i of-type fixnum from start below end
           for j of-type fixnum from pos
           do (setf (aref o j) (char-code (schar s i))))
     end)
    ((simple-array character (*))
     (let ((i start) (j pos))
       (declare (type fixnum i j))
       ;; characters are 32-bit little-endian words: four ASCII ones in two loads
       #+little-endian
       (sb-sys:with-pinned-objects (s o)
         (let ((from (sb-sys:vector-sap s)) (to (sb-sys:vector-sap o)))
           (loop while (<= (+ i 4) end)
                 do (let ((a (sb-sys:sap-ref-64 from (* 4 i)))
                          (b (sb-sys:sap-ref-64 from (* 4 (+ i 2)))))
                      (declare (type (unsigned-byte 64) a b))
                      (unless (zerop (logand (logior a b) #xFFFFFF80FFFFFF80)) (return))
                      (setf (sb-sys:sap-ref-32 to j)
                            (logior (ldb (byte 8 0) a) (ash (ldb (byte 8 32) a) 8)
                                    (ash (ldb (byte 8 0) b) 16) (ash (ldb (byte 8 32) b) 24)))
                      (incf i 4) (incf j 4)))))
       (loop while (and (< i end) (< (char-code (schar s i)) #x80))
             do (setf (aref o j) (char-code (schar s i)))
                (incf i) (incf j))
       i))
    (simple-string start)))

(defconstant +head-room+ 512
  "Octets of *OUT* before a response body, where its head is put once the body's length is
known (the longest head is under 200).")

(defparameter +json-type+ "application/json; charset=utf-8")

(defun put-head-start (o status content-type)
  "Write a response head into O up to the content length's digits; returns where they go."
  (let* ((pos (put-ascii o 0 "HTTP/1.1 "))
         (pos (put-decimal o pos status))
         (pos (put-ascii o pos " "))
         (pos (put-ascii o pos (reason status)))
         (pos (put-ascii o pos #.(format nil "~C~Ccontent-type: " #\Return #\Newline)))
         (pos (put-ascii o pos content-type)))
    (put-ascii o pos #.(format nil "~C~Ccontent-length: " #\Return #\Newline))))

(defparameter +ok-json-head+
  (let ((o (make-octets 256))) (subseq o 0 (put-head-start o 200 +json-type+)))
  "The head of every JSON answer with status 200, up to its content length's digits.")
(defparameter +head-end+ (string->octets (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline)))
(defparameter +close-head-end+
  (string->octets (format nil "~C~Cconnection: close~C~C~C~C"
                          #\Return #\Newline #\Return #\Newline #\Return #\Newline)))

(defun decimal-length (n)
  (declare (type (integer 0 #.most-positive-fixnum) n))
  (if (zerop n) 1 (loop for k of-type fixnum = n then (floor k 10) while (plusp k) count t)))

(defun render-response (status content-type body refused)
  "Write BODY (a string) into *OUT* after +HEAD-ROOM+ octets, then the response head right
before it: (values start end) of the response in *OUT*. A REFUSED request's answer says
Connection: close."
  (multiple-value-bind (chars start end) (string-span body)
    (flet ((make-room (n)
             (when (< (length *out*) (+ +head-room+ n))
               (setf *out* (make-octets (max (+ +head-room+ n) (* 2 (length *out*))))))))
      ;; nearly every answer is ASCII and goes over in one pass; others are measured first
      (make-room (- end start))
      (let ((size (if (= end (copy-ascii *out* +head-room+ chars start end))
                      (- end start)
                      (let ((size (utf8-length chars start end)))
                        (cond (size (make-room size) (put-utf8 *out* +head-room+ chars start end) size)
                              ;; surrogates: the encoder refuses them, as it always did
                              (t (let ((bytes (string->octets (subseq chars start end))))
                                   (make-room (length bytes))
                                   (replace *out* bytes :start1 +head-room+)
                                   (length bytes))))))))
        (let* ((o *out*)
               (head (make-array 256 :element-type '(unsigned-byte 8)))
               (prefix (if (and (eql status 200) (eq content-type +json-type+)) +ok-json-head+ head))
               (prefix-end (if (eq prefix head) (put-head-start head status content-type) (length prefix)))
               (tail (if refused +close-head-end+ +head-end+))
               (start (- +head-room+ prefix-end (decimal-length size) (length tail))))
          (declare (dynamic-extent head) (type octets prefix tail) (type fixnum prefix-end start))
          (replace o prefix :start1 start :end2 prefix-end)
          (replace o tail :start1 (put-decimal o (+ start prefix-end) size))
          (values start (+ +head-room+ size)))))))

(defun send-out (c o start end)
  "Send O[START,END) on C's socket as far as it takes it: :DONE, :AGAIN (the rest is kept
in C until the socket can take more) or :FAILED."
  (declare (type fixnum start end))
  (loop while (< start end)
        do (multiple-value-bind (count errno) (poll:fd-send-some (connection-fd c) o start end)
             (cond (count (incf start count))
                   ((eql errno sb-unix:ewouldblock)
                    (if (eq o (connection-out c))
                        (setf (connection-out-start c) start)
                        (setf (connection-out c) (subseq o start end) (connection-out-start c) 0))
                    (return-from send-out :again))
                   (t (return-from send-out :failed)))))
  (setf (connection-out c) nil)
  (when (> (length *out*) (* 4 1024 1024)) (setf *out* (make-octets 65536)))
  :done)

(defun send-response (c status content-type body &key refused)
  (multiple-value-bind (start end) (render-response status content-type body refused)
    (setf (connection-phase c) :writing (connection-deadline c) (ms-after *write-seconds*))
    (when refused (setf (connection-close-after c) t))
    (send-out c *out* start end)))

(defun answer (c)
  "Answer C's complete request: :DONE, :AGAIN or :FAILED as SEND-OUT."
  (let ((req (connection-request c)))
    (setf (connection-phase c) :busy (connection-deadline c) most-positive-fixnum)
    (when (req-close req) (setf (connection-close-after c) t))
    (cond ((not (eq (req-method req) :post))
           (send-response c 405 "text/plain" "Used HTTP Method is not allowed. POST is required"))
          ((not (req-json req))
           (send-response c 415 "text/plain"
                          "Supplied content type is not allowed. Content-Type: application/json is required"))
          (t (multiple-value-bind (status response) (multiple-value-call #'run-body (request-body c))
               (send-response c status +json-type+ response))))))

(defun refuse (c condition)
  "Answer a refused request and close once it is sent."
  (let ((status (bad-request-status condition)))
    (send-response c status (if (= status 413) +json-type+ "text/plain")
                   (bad-request-body condition) :refused t)))

(defun request-done (c)
  "After a response: forget the request, consume its body, and wait for the next one."
  (let ((req (connection-request c)) (in (connection-in c)))
    (when (and (eq (req-stage req) :body) (null (req-body req)))
      (incf (inbuf-start in) (req-need req)))
    (reset-request req)
    (if (< (inbuf-start in) (inbuf-end in))
        (setf (connection-phase c) :request (connection-deadline c) (ms-after *request-seconds*))
        (setf (connection-phase c) :idle (connection-deadline c) (ms-after *idle-seconds*)))))

(defun fill-input (c)
  "Read what C's socket has after its buffered input: :READ, :AGAIN (nothing yet), :EOF or
:FAILED. Notes whether more may be waiting (a read that filled the buffer)."
  (let* ((in (connection-in c)) (o (inbuf-octets in)) (start (inbuf-start in)) (end (inbuf-end in)))
    (when (inbuf-eof in) (return-from fill-input :failed))
    (when (plusp start)
      (replace o o :start2 start :end2 end)
      (setf end (- end start) (inbuf-start in) 0 (inbuf-end in) end))
    (invariant (< end (length o)) "input buffer full")
    (multiple-value-bind (count errno) (poll:fd-read-some (connection-fd c) o end (length o))
      (cond ((null count)
             (setf (connection-must-read c) nil)
             (if (eql errno sb-unix:ewouldblock) :again :failed))
            ((zerop count) (setf (inbuf-eof in) t (connection-must-read c) nil) :eof)
            (t (setf (inbuf-end in) (+ end count)
                     ;; a full buffer may have left more behind; after a hang-up the end of
                     ;; stream, announced by no further event, is behind the data
                     (connection-must-read c) (or (= (+ end count) (length o)) (connection-hangup c)))
               (when (eq (connection-phase c) :idle)
                 (setf (connection-phase c) :request (connection-deadline c) (ms-after *request-seconds*)))
               :read)))))

;;; Connections: each is run from its events by one thread at a time

(defun finish (server c)
  "End C: forget it and close its socket (which also leaves its poller)."
  (sb-thread:with-mutex ((server-lock server))
    (let ((fd (connection-fd c)) (table (server-table server)))
      (when (>= fd 0)
        (when (and (< fd (length table)) (eq c (svref table fd)))
          (setf (svref table fd) nil))
        (decf (server-count server))
        (decf (server-consensus-slots server) (connection-consensus-slots c))
        (setf (server-connections server) (delete c (server-connections server)))
        (poll:fd-close fd)
        (setf (connection-fd c) -1)))))

(defun want-write (server c)
  "Wait for room to send the rest of C's response."
  (unless (poll:watch-write (connection-poller c) (connection-fd c))
    (finish server c)))

(defun drive (server c)
  "Run C on an event from its socket: answer every request its input completes, then return
to wait for the next event (input, room to write, or the reaper's shutdown), or end it."
  ;; the event may announce input: a socket watched by its edges must be read until a read
  ;; leaves nothing behind
  (setf (connection-must-read c) t)
  (handler-case
      (loop
        (cond ((connection-expired c) (return (finish server c)))
              ((connection-out c)
               (ecase (send-out c (connection-out c) (connection-out-start c) (length (connection-out c)))
                 (:again (return (want-write server c)))
                 (:failed (return (finish server c)))
                 (:done (if (connection-close-after c)
                            (return (finish server c))
                            (request-done c)))))
              (t
               (let ((step (handler-case (advance c)
                             (bad-request (e) (list (refuse c e))))))
                 (case step
                   (:request
                    (ecase (answer c)
                      (:again (return (want-write server c)))
                      (:failed (return (finish server c)))
                      (:done (when (connection-close-after c) (return (finish server c)))
                             (request-done c))))
                   (:more
                    (unless (connection-must-read c) (return))
                    (ecase (fill-input c)
                      ((:read :eof))
                      (:again (return))
                      (:failed (return (finish server c)))))
                   (:websocket (return (start-websocket server c)))
                   ((nil) (return (finish server c)))
                   (t                   ; refused: the answer went (or waits to go) out
                    (ecase (first step)
                      (:again (return (want-write server c)))
                      ((:done :failed) (return (finish server c))))))))))
    ;; A broken, slow or hostile request ends its connection, never the worker.
    ;; SERIOUS-CONDITION: stack or heap exhaustion is not an ERROR, and must end only this
    ;; connection.
    (serious-condition () (finish server c))))

(defun add-connection (server fd)
  "Take the accepted socket FD, giving it to the next worker's poller in turn."
  (let* ((c (make-connection fd))
         (pollers (server-pollers server))
         (poller (svref pollers (mod (incf (server-next-poller server)) (length pollers))))
         (accepted nil))
    (setf (connection-poller c) poller
          (connection-deadline c) (ms-after *idle-seconds*))
    (sb-thread:with-mutex ((server-lock server))
      (when (and (server-running server) (< (server-count server) (server-max-connections server)))
        (let ((table (server-table server)))
          (when (>= fd (length table))
            (let ((new (make-array (max (1+ fd) (* 2 (length table))) :initial-element nil)))
              (replace new table)
              (setf (server-table server) new table new)))
          (setf (svref table fd) c))
        (push c (server-connections server))
        (incf (server-count server))
        (setf accepted t)))
    (cond ((not accepted) (poll:fd-close fd))
          ;; input already waiting is reported at once
          ((not (poll:watch poller fd)) (finish server c)))))

(defun accept-all (server)
  "Take every pending connection."
  (loop (let ((fd (poll:accept-connection (server-listen-fd server))))
          (unless fd (return))
          (add-connection server fd))))

(defun run-connection (server c)
  "Run C on an event, unless another thread is running it: that one then runs it once more
when it is done, so no event is lost and no connection is run by two threads at once."
  (loop (case (sb-ext:compare-and-swap (connection-run c) 0 1)
          (0 (return))
          (1 (when (eql 1 (sb-ext:compare-and-swap (connection-run c) 1 2))
               (return-from run-connection)))
          (t (return-from run-connection))))
  (loop (drive server c)
        (when (eql 1 (sb-ext:compare-and-swap (connection-run c) 1 0)) (return))
        ;; an event came meanwhile
        (setf (connection-run c) 1)))

;;; Workers. A WORKER is a thread serving one poller. Its STATE counts the events it has
;;; run, odd while one runs. The reaper looks every *STALL-MS*: a state still odd and
;;; unchanged since its last look is a stalled event. Then, while a spare thread is at hand or
;;; another may be started, the reaper swaps the state for :RETIRED (one compare-and-swap, so
;;; it and the worker finishing that event agree on who won) and gives the poller to the other
;;; thread, which first runs every connection of the poller once: the events the stalled
;;; thread had taken but not run (a connection with nothing to do just reads nothing). The
;;; stalled thread finishes its call and becomes a spare; it never touches the poller again.

(defstruct (worker (:constructor make-worker (shard &optional poller)))
  shard
  poller                                ; the poller it serves; NIL while it is a spare
  (state 0)                             ; events run (odd while one runs), or :RETIRED
  (seen nil)                            ; its STATE when the reaper last looked
  (sweep nil))                          ; run every connection of the poller before waiting

(defun run-event (server w c)
  "Run C for one event as W's work. NIL when the reaper gave W's poller on meanwhile."
  (let ((v (worker-state w)))
    (declare (type fixnum v))
    (setf (worker-state w) (1+ v))
    (run-connection server c)
    (eql (1+ v) (sb-ext:compare-and-swap (worker-state w) (1+ v) (+ v 2)))))

(defun runnable (c poller)
  (and c (eq (connection-poller c) poller) (not (eq (connection-phase c) :thread))))

(defun serve-poller (server w poller)
  "Wait for events on POLLER and run each one's connection (the first worker's poller also
watches the listening socket), until the server stops or the reaper gives POLLER on."
  (let ((wake (server-wake-read server))
        (listen (server-listen-fd server)))
    (when (worker-sweep w)
      (setf (worker-sweep w) nil)
      (dolist (c (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server))))
        (when (and (runnable c poller) (not (run-event server w c)))
          (return-from serve-poller))))
    (loop while (server-running server)
          do (dotimes (i (poll:poller-wait poller nil))
               (let ((fd (poll:poller-event poller i)))
                 (cond ((= fd wake))
                       ((= fd listen) (accept-all server))
                       (t (let* ((table (server-table server))
                                 (c (and (< fd (length table)) (svref table fd))))
                            (when (runnable c poller)
                              (when (poll:poller-event-hangup-p poller i)
                                (setf (connection-hangup c) t))
                              ;; given on: the poller's events are another thread's now
                              (unless (run-event server w c) (return-from serve-poller)))))))))))

(defun worker-loop (server w)
  "Serve W's poller; when it is given on, wait as a spare for another, until the server stops."
  (loop
    (let ((poller (worker-poller w)))
      (when poller (serve-poller server w poller)))
    (sb-thread:with-mutex ((server-crew-lock server))
      (setf (worker-poller w) nil)
      (push w (server-spares server))
      (loop while (and (server-running server) (null (worker-poller w)))
            do (sb-thread:condition-wait (server-crew-wait server) (server-crew-lock server)))
      (unless (and (server-running server) (worker-poller w)) (return)))))

(defun call-with-rpc-thread (server function)
  "Call FUNCTION on a thread that runs requests: calls and traces run the EVM there, so it gets
its own EVM scratch and signature arenas (P-384 memoizing: an attestation's CA chain is the
same on every zone verification replayed)."
  (let ((*server* server)
        (*out* (make-octets 65536))
        (rpc:*response-buffer* (rpc:make-response-buffer))
        (clempo.secp256k1:*recovery-scratch* (clempo.secp256k1:make-recovery-scratch))
        (clempo.p256:*p256-scratch* (clempo.p256:make-p256-scratch))
        (clempo.p384:*p384-scratch* (clempo.p384:make-p384-scratch :memoize t)))
    (clempo.evm:with-evm-thread (funcall function))))

(defun start-worker (server w)
  "Start W's thread (under the crew lock)."
  (push (sb-thread:make-thread
         (lambda ()
           (call-with-rpc-thread
            server
            (lambda ()
              ;; each thread counts into a metrics shard of its own (shared past 15 threads)
              (let ((clempo.metrics:*shard* (worker-shard w))
                    (*text* (make-string 4096 :element-type 'base-char)))
                (worker-loop server w)))))
         :name "rpc worker")
        (server-threads server)))

(defun hand-on (server index w state)
  "W, serving the poller at INDEX, has run one event since its STATE: give the poller to a
spare thread, or a new one while fewer than *EXTRA-WORKERS-MAX* were started."
  (sb-thread:with-mutex ((server-crew-lock server))
    (let ((spare (pop (server-spares server))))
      (when (or spare (< (server-extra server) *extra-workers-max*))
        (cond ((eql state (sb-ext:compare-and-swap (worker-state w) state :retired))
               (let ((next (or spare
                               (make-worker (1+ (mod (+ (length (server-crew server)) (server-extra server)) 15))))))
                 (setf (worker-state next) 0 (worker-seen next) nil (worker-sweep next) t
                       (worker-poller next) (worker-poller w)
                       (svref (server-crew server) index) next)
                 (if spare
                     (sb-thread:condition-broadcast (server-crew-wait server))
                     (progn (incf (server-extra server)) (start-worker server next)))))
              ;; the event ended meanwhile
              (spare (push spare (server-spares server))))))))

(defun relieve-stalls (server)
  (let ((crew (server-crew server)))
    (dotimes (i (length crew))
      (let* ((w (svref crew i)) (state (worker-state w)))
        (if (and (typep state 'fixnum) (oddp state) (eql state (worker-seen w)))
            (hand-on server i w state)
            (setf (worker-seen w) state))))))

(defun reap-loop (server)
  "Every *STALL-MS*, give the poller of a worker stalled in one event to another thread; about
every 200 ms, end connections past their deadline: mark them and shut their sockets down,
which wakes the poller for them, and the thread taking that event closes them."
  (let ((next-reap 0))
    (loop while (server-running server)
          do (sb-thread:with-mutex ((server-reaper-lock server))
               (when (server-running server)
                 (sb-thread:condition-wait (server-reaper-wait server) (server-reaper-lock server)
                                           :timeout (/ *stall-ms* 1000.0))))
             (when (server-running server) (relieve-stalls server))
             (let ((now (now-ms)))
               (when (>= now next-reap)
                 (setf next-reap (+ now 200))
                 (sb-thread:with-mutex ((server-lock server))
                   (dolist (c (server-connections server))
                     (when (and (> now (connection-deadline c)) (not (connection-expired c))
                                (>= (connection-fd c) 0))
                       (setf (connection-expired c) t)
                       (poll:fd-shutdown (connection-fd c))))))))))

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

(defun start-websocket (server c)
  "Hand C, upgrading to a websocket, to a thread of its own for the rest of its life."
  (setf (connection-phase c) :thread (connection-deadline c) most-positive-fixnum)
  (poll:unwatch (connection-poller c) (connection-fd c))
  (sb-thread:with-mutex ((server-lock server))
    (setf (connection-thread c)
          (sb-thread:make-thread
           (lambda () (call-with-rpc-thread server (lambda () (serve-websocket-connection server c))))
           :name "rpc websocket"))))

(defun serve-websocket-connection (server c)
  ;; a socket: reading and writing are separate channels (else a write drops buffered input)
  (let ((stream (sb-sys:make-fd-stream (connection-fd c) :input t :output t :dual-channel-p t
                                                         :element-type '(unsigned-byte 8)
                                                         :buffering :full :auto-close nil
                                                         :serve-events nil)))
    (setf (connection-stream c) stream)
    (unwind-protect
         (handler-case (serve-websocket c (req-key (connection-request c)) stream)
           (serious-condition () nil))
      (close-connection server c)
      ;; the writer is joined before the descriptor closes, or it could write to one reused
      ;; meanwhile; the socket is shut down, so its writes fail at once
      (let ((writer (connection-writer c)))
        (when writer (sb-thread:join-thread writer :default nil)))
      ;; FINISH alone closes the descriptor: closing the stream would close it too (an fd-stream
      ;; closes its descriptor even with :AUTO-CLOSE NIL), and the second close could hit a
      ;; descriptor another thread opened in between. The stream has no finalizer.
      (finish server c))))

(defun serve-websocket (connection key stream)
  (let ((input (websocket-input connection stream)))
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

(defun close-connection (server connection)
  "Stop CONNECTION's writer and wake its reader: both see the socket shut down."
  (sb-thread:with-mutex ((connection-outbox-lock connection))
    (setf (connection-closed connection) t)
    (sb-thread:condition-broadcast (connection-outbox-ready connection)))
  ;; under the server lock, so the descriptor is not closed (and reused) meanwhile
  (sb-thread:with-mutex ((server-lock server))
    (when (>= (connection-fd connection) 0)
      (poll:fd-shutdown (connection-fd connection)))))

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
                       (close-connection server connection)
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

;;; The server

(defun start-server (&key (host "127.0.0.1") (port 8545) backend (max-connections 256)
                       (workers (poll:available-cpus)) (log *error-output*))
  "Listen on HOST:PORT (0 picks a free port) and serve BACKEND with WORKERS threads, logging
to the stream LOG (NIL for none). Returns the server; STOP-SERVER joins every thread it
started."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address host) port)
    (sb-bsd-sockets:socket-listen socket 128)
    (setf (sb-bsd-sockets:non-blocking-mode socket) t)
    (multiple-value-bind (wake-read wake-write) (poll:make-wakeup-pipe)
      (let* ((pollers (coerce (loop repeat (max 1 workers) collect (poll:make-poller)) 'simple-vector))
             (listen-fd (sb-bsd-sockets:socket-file-descriptor socket))
             (server (%make-server :socket socket
                                   :port (nth-value 1 (sb-bsd-sockets:socket-name socket))
                                   :backend backend :max-connections max-connections :log log
                                   :pollers pollers :listen-fd listen-fd
                                   :wake-read wake-read :wake-write wake-write)))
        (loop for p across pollers do (poll:watch-level p wake-read))
        (poll:watch-level (svref pollers 0) listen-fd)
        (setf (server-crew server)
              (coerce (loop for p across pollers for i from 0 collect (make-worker (1+ (mod i 15)) p))
                      'simple-vector))
        (sb-thread:with-mutex ((server-crew-lock server))
          (loop for w across (server-crew server) do (start-worker server w)))
        (setf (server-reaper server)
              (sb-thread:make-thread (lambda () (reap-loop server)) :name "rpc reaper"))
        server))))

(defun stop-server (server)
  "Stop accepting and serving, close every open connection, and join the workers, the
reaper and the websocket threads (each joins its notification writer)."
  (setf (server-running server) nil)
  (poll:wakeup (server-wake-write server))
  (sb-thread:with-mutex ((server-reaper-lock server))
    (sb-thread:condition-broadcast (server-reaper-wait server)))
  (sb-thread:join-thread (server-reaper server) :default nil :timeout 10)
  ;; the reaper starts no more threads now
  (sb-thread:with-mutex ((server-crew-lock server))
    (sb-thread:condition-broadcast (server-crew-wait server)))
  (dolist (thread (sb-thread:with-mutex ((server-crew-lock server)) (copy-list (server-threads server))))
    (sb-thread:join-thread thread :default nil :timeout 10))
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (let ((connections (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server)))))
    (dolist (c connections)
      (if (eq (connection-phase c) :thread)
          (close-connection server c)
          (finish server c)))
    (dolist (c connections)
      (let ((thread (connection-thread c)))
        (when thread (sb-thread:join-thread thread :default nil :timeout 10)))))
  (loop for p across (server-pollers server) do (poll:poller-close p))
  (poll:fd-close (server-wake-read server))
  (poll:fd-close (server-wake-write server))
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
