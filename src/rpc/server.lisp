(defpackage #:clempo.rpcserver
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:ws #:clempo.websocket) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain))
  (:documentation "HTTP/1.1 and WebSocket JSON-RPC on one port, on sb-bsd-sockets with a thread
per connection. HTTP answers as the reference does (POST with Content-Type application/json,
405 and 415 otherwise); a GET with Upgrade: websocket switches the connection to frames,
where requests are answered in order and subscriptions push eth_subscription
notifications.")
  (:export #:start-server #:stop-server #:server-port #:server-backend #:publish-head
           #:connection #:connection-subscriptions #:send-notification #:*server*
           #:new-subscription-id #:notify-head))

(in-package #:clempo.rpcserver)

(defvar *server* nil "The server serving the current request.")

(defstruct (server (:constructor %make-server))
  socket port backend (connections '())
  (lock (sb-thread:make-mutex :name "rpc server"))
  (running t) acceptor (max-connections 256))

(defstruct (connection (:constructor make-connection (socket stream)))
  socket stream
  (send-lock (sb-thread:make-mutex :name "ws send"))
  (websocket-p nil)
  (subscriptions '()))

(defgeneric notify-head (backend connection number)
  (:documentation "Send CONNECTION's subscription notifications for new head NUMBER.")
  (:method (backend connection number) (declare (ignore backend connection number)) nil))

(defun new-subscription-id ()
  (format nil "0x~(~32,'0x~)" (octets->integer (clempo.io:random-octets 16))))

(defun send-text (connection text)
  (sb-thread:with-mutex ((connection-send-lock connection))
    (ws:write-frame (connection-stream connection) ws:+op-text+ (string->octets text))))

(defun send-notification (connection method subscription result)
  "Push {jsonrpc, method, params: {subscription, result}} on a websocket connection."
  (send-text connection
             (clempo.json:to-json-string
              (clempo.json:jobject "jsonrpc" "2.0" "method" method
                                   "params" (clempo.json:jobject "subscription" subscription
                                                                 "result" result)))))

;;; HTTP

(defun read-line-crlf (stream)
  (let ((out (make-string-output-stream)) (any nil))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun read-request-head (stream)
  "Returns (values method path headers) or NIL at end of stream. Header names are lowercased."
  (let ((line (loop for l = (read-line-crlf stream)
                    while (and l (zerop (length l)))
                    finally (return l))))
    (when line
      (let* ((sp1 (position #\Space line)) (sp2 (and sp1 (position #\Space line :start (1+ sp1)))))
        (values (subseq line 0 sp1)
                (and sp1 (subseq line (1+ sp1) sp2))
                (loop for h = (read-line-crlf stream)
                      until (or (null h) (zerop (length h)))
                      for colon = (position #\: h)
                      when colon collect (cons (string-downcase (subseq h 0 colon))
                                               (string-trim " " (subseq h (1+ colon))))))))))

(defun header (headers name) (cdr (assoc name headers :test #'string=)))

(defun read-body (stream headers)
  (let ((len (header headers "content-length")))
    (cond (len (let ((o (make-octets (parse-integer len))))
                 (read-sequence o stream) o))
          ((search "chunked" (or (header headers "transfer-encoding") ""))
           (let ((parts '()))
             (loop for size = (parse-integer (read-line-crlf stream) :radix 16 :junk-allowed t)
                   until (zerop size)
                   do (let ((o (make-octets size))) (read-sequence o stream) (push o parts))
                      (read-line-crlf stream))
             (loop for l = (read-line-crlf stream) until (or (null l) (zerop (length l))))
             (apply #'octets-concat (nreverse parts))))
          (t (make-octets 0)))))

(defun write-http-response (stream status reason content-type body &key extra-headers)
  (let ((bytes (if (stringp body) (string->octets body) body)))
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
    (force-output stream)))

(defun reason (status)
  (case status (200 "OK") (400 "Bad Request") (405 "Method Not Allowed")
    (415 "Unsupported Media Type") (t "Error")))

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

(defun serve-http-request (connection method headers stream)
  "Answer one HTTP request; returns T to keep the connection open."
  (declare (ignorable connection))
  (let ((body (read-body stream headers))
        (keep (not (string-equal (or (header headers "connection") "") "close"))))
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
    (write-sequence
     (string->octets
      (format nil "HTTP/1.1 101 Switching Protocols~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Accept: ~A~C~C~C~C"
              #\Return #\Newline #\Return #\Newline #\Return #\Newline (ws:accept-key key)
              #\Return #\Newline #\Return #\Newline))
     stream)
    (force-output stream)
    (setf (connection-websocket-p connection) t)
    (loop
      (multiple-value-bind (opcode payload) (ws:read-message stream)
        (cond ((or (null opcode) (= opcode ws:+op-close+))
               (ignore-errors (sb-thread:with-mutex ((connection-send-lock connection))
                                (ws:write-frame stream ws:+op-close+ (or payload (make-octets 0)))))
               (return))
              ((= opcode ws:+op-ping+)
               (sb-thread:with-mutex ((connection-send-lock connection))
                 (ws:write-frame stream ws:+op-pong+ payload)))
              ((member opcode (list ws:+op-text+ ws:+op-binary+))
               (let ((rpc:*connection* connection))
                 (multiple-value-bind (status response) (run-body (octets->string payload))
                   (declare (ignore status))
                   (unless (string= response "null") (send-text connection response))))))))))

;;; Connections and the listener

(defun serve-connection (server connection)
  (let ((stream (connection-stream connection))
        (*server* server))
    (unwind-protect
         (handler-case
             (loop
               (multiple-value-bind (method path headers) (read-request-head stream)
                 (declare (ignore path))
                 (unless method (return))
                 (if (and (string-equal method "GET")
                          (string-equal (or (header headers "upgrade") "") "websocket"))
                     (progn (serve-websocket connection headers stream) (return))
                     (unless (serve-http-request connection method headers stream) (return)))))
           ;; A broken or closed connection ends its thread, never the process.
           (error () nil))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close (connection-socket connection)))
      (sb-thread:with-mutex ((server-lock server))
        (setf (server-connections server) (remove connection (server-connections server)))))))

(defun accept-loop (server)
  (loop while (server-running server)
        do (let ((client (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                           (error () nil))))
             (when (and client (server-running server))
               (setf (sb-bsd-sockets:sockopt-tcp-nodelay client) t)
               (let ((connection (make-connection
                                  client (sb-bsd-sockets:socket-make-stream
                                          client :input t :output t :element-type '(unsigned-byte 8)
                                                 :buffering :full))))
                 (sb-thread:with-mutex ((server-lock server))
                   (if (>= (length (server-connections server)) (server-max-connections server))
                       (progn (ignore-errors (sb-bsd-sockets:socket-close client)) (setf connection nil))
                       (push connection (server-connections server))))
                 (when connection
                   (sb-thread:make-thread (lambda () (serve-connection server connection))
                                          :name "rpc connection")))))))

(defun start-server (&key (host "127.0.0.1") (port 8545) backend (max-connections 256))
  "Listen on HOST:PORT (0 picks a free port) and serve BACKEND. Returns the server."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:make-inet-address host) port)
    (sb-bsd-sockets:socket-listen socket 128)
    (let ((server (%make-server :socket socket
                                :port (nth-value 1 (sb-bsd-sockets:socket-name socket))
                                :backend backend :max-connections max-connections)))
      (setf (server-acceptor server)
            (sb-thread:make-thread (lambda () (accept-loop server)) :name "rpc accept"))
      server)))

(defun stop-server (server)
  "Stop accepting, close every open connection and wait for the acceptor."
  (setf (server-running server) nil)
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  ;; Wake a blocked accept on platforms where closing the socket does not.
  (ignore-errors
   (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
     (sb-bsd-sockets:socket-connect s #(127 0 0 1) (server-port server))
     (sb-bsd-sockets:socket-close s)))
  (ignore-errors (sb-thread:join-thread (server-acceptor server) :timeout 5))
  (dolist (c (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server))))
    (ignore-errors (sb-bsd-sockets:socket-shutdown (connection-socket c) :direction :io)))
  server)

(defun publish-head (server number)
  "A new head NUMBER is committed: push subscription notifications to every websocket."
  (dolist (c (sb-thread:with-mutex ((server-lock server)) (copy-list (server-connections server))))
    (when (and (connection-websocket-p c) (connection-subscriptions c))
      (handler-case (notify-head (server-backend server) c number)
        (error () nil)))))
