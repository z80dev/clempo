(defpackage #:clempo.rpc.server-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:server #:clempo.rpcserver) (#:ws #:clempo.websocket)
                    (#:logs #:clempo.rpc.logs)))
(in-package #:clempo.rpc.server-tests)

;;; The HTTP/WebSocket server under hostile and concurrent clients, with no archive: a
;;; backend-less server answers web3_* and the envelope, which is all the transport needs.
;;; Limits and their answers are reth's (rpc.max-request-size 15 MiB, max-response-size
;;; 160 MiB, max-subscriptions-per-connection 1024); the 413/-32007 and -32006 bodies were
;;; recorded from the reference node (2026-11-21, its limits configured larger: "Exceeded
;;; max limit of 209715200" for a 201 MiB body; the 1025th eth_subscribe on one websocket).

(defmacro with-server ((port &rest options) &body body)
  (let ((s (gensym "SERVER")))
    `(let* ((,s (server:start-server :port 0 :log nil ,@options))
            (,port (server:server-port ,s)))
       (declare (ignorable ,port))
       (unwind-protect (progn ,@body) (server:stop-server ,s)))))

(defmacro with-limits ((&rest bindings) &body body)
  "Set server specials globally (connection threads read the global values) for BODY."
  (let ((olds (loop repeat (length bindings) collect (gensym "OLD"))))
    `(let ,(loop for (var) in bindings for o in olds collect `(,o ,var))
       (unwind-protect (progn (setf ,@(loop for (var val) in bindings append (list var val))) ,@body)
         (setf ,@(loop for (var) in bindings for o in olds append (list var o)))))))

(defun open-client (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
    (values (sb-bsd-sockets:socket-make-stream socket :input t :output t :timeout 20
                                                      :element-type '(unsigned-byte 8) :buffering :full)
            socket)))

(defun send (stream data)
  ;; each character is one byte, so a test can send bytes that are not UTF-8
  (write-sequence (if (stringp data) (map 'octets #'char-code data) data) stream)
  (force-output stream))

(defun read-line-crlf (stream)
  "The client's next response line without its CR LF, NIL at end of stream."
  (let ((out (make-string-output-stream)) (any nil))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun read-exactly (stream n)
  (let ((o (make-octets n)))
    (unless (= n (read-sequence o stream)) (error 'end-of-file :stream stream))
    o))

(defun read-all (stream)
  "Everything until the server closes the connection, as a string."
  (let ((out (make-string-output-stream)))
    (handler-case (loop for b = (read-byte stream nil nil) while b do (write-char (code-char b) out))
      (error () nil))
    (get-output-stream-string out)))

(defun exchange (port data)
  "Send DATA on a fresh connection, half-close, and return the whole answer."
  (multiple-value-bind (stream socket) (open-client port)
    (unwind-protect
         ;; the server may refuse and close before taking everything: Linux then fails the
         ;; rest of the send (EPIPE) or the shutdown (ENOTCONN); its answer is still readable
         (progn (ignore-errors (send stream data))
                (ignore-errors (sb-bsd-sockets:socket-shutdown socket :direction :output))
                (read-all stream))
      (ignore-errors (close stream)))))

(defun crlf (&rest lines)
  (format nil "~{~A~C~C~}" (loop for l in lines append (list l #\Return #\Newline))))

(defun post (body &key (headers '()))
  (concatenate 'string
               (apply #'crlf "POST / HTTP/1.1" "Host: x" "Content-Type: application/json"
                      (append headers (list (format nil "Content-Length: ~D" (length body)) "")))
               body))

(defparameter +client-version-request+ "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"web3_clientVersion\",\"params\":[]}")

(defun status-of (answer) (and (> (length answer) 12) (parse-integer answer :start 9 :end 12)))
(defun body-of (answer) (let ((at (search (crlf "" "") answer))) (and at (subseq answer (+ at 4)))))

(defun answers-p (port)
  "The server still answers a plain request."
  (eql 200 (status-of (exchange port (post +client-version-request+)))))

(deftest http-limits-refuse-hostile-requests ()
  (with-server (port)
    ;; a huge Content-Length is refused before anything is allocated
    (let ((a (exchange port (apply #'crlf "POST / HTTP/1.1" "Content-Type: application/json"
                                   "Content-Length: 99999999999" '("")))))
      (is= 413 (status-of a))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32007,\"message\":\"Request is too big\",\"data\":\"Exceeded max limit of 15728640\"}}"
           (body-of a)))
    (is= 413 (status-of (exchange port (apply #'crlf "POST / HTTP/1.1" (format nil "Content-Length: ~D" (1+ server:+body-max+)) '("")))))
    (dolist (bad '("-1" "1e3" "0x10" "" "12 34" "99999999999999999999"))
      (is= 400 (status-of (exchange port (crlf "POST / HTTP/1.1" (format nil "Content-Length: ~A" bad) "")))
           "content-length ~S" bad))
    ;; both framings (request smuggling) and unknown codings
    (is= 400 (status-of (exchange port (crlf "POST / HTTP/1.1" "Content-Length: 3" "Transfer-Encoding: chunked" "" "0" "" ""))))
    (is= 400 (status-of (exchange port (crlf "POST / HTTP/1.1" "Transfer-Encoding: gzip" "" ""))))
    ;; chunked: a declared chunk over the limit is refused before its data is read
    (is= 413 (status-of (exchange port (crlf "POST / HTTP/1.1" "Transfer-Encoding: chunked" "" "FFFFFFF" ""))))
    (is= 400 (status-of (exchange port (crlf "POST / HTTP/1.1" "Transfer-Encoding: chunked" "" "zz" ""))))
    (let* ((body +client-version-request+)
           (a (exchange port (concatenate 'string
                                          (crlf "POST / HTTP/1.1" "Content-Type: application/json"
                                                "Transfer-Encoding: chunked" "" (format nil "~X" 10))
                                          (subseq body 0 10) (crlf "" (format nil "~X" (- (length body) 10)))
                                          (subseq body 10) (crlf "" "0" "" "")))))
      (is= 200 (status-of a))
      (is (search "\"result\":\"clempo/" (body-of a))))
    ;; header lines: too long, too many, malformed
    (is= 431 (status-of (exchange port (crlf "POST / HTTP/1.1" (format nil "X: ~A" (make-string 9000 :initial-element #\a)) ""))))
    (is= 431 (status-of (exchange port (format nil "GET /~A HTTP/1.1~C~C~C~C" (make-string 9000 :initial-element #\a)
                                               #\Return #\Newline #\Return #\Newline))))
    (is= 431 (status-of (exchange port (apply #'crlf "POST / HTTP/1.1"
                                              (append (loop for i below 200 collect (format nil "X-~D: 1" i)) '(""))))))
    (is= 400 (status-of (exchange port (crlf "POST / HTTP/1.1" "no colon here" ""))))
    (is= 400 (status-of (exchange port (crlf "GARBAGE" ""))))
    ;; an endless run of blank lines is refused after +HEADER-LINES-MAX+ of them
    (is= 400 (status-of (exchange port (make-string 100000 :initial-element #\Newline))))
    ;; bodies that are not UTF-8: a parse error, as the reference answers (recorded 2026-11-21)
    (let ((a (exchange port (post (concatenate 'string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[\""
                                               (string (code-char #xFF)) "\"]}")))))
      (is= 200 (status-of a))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}" (body-of a)))
    ;; a body nested past the control stack costs at most its own connection
    (exchange port (post (make-string 200000 :initial-element #\[)))
    (is (answers-p port) "the server survives every hostile request")))

(deftest response-limit ()
  (with-limits ((rpc::*response-max* 100))
    (is= "{\"jsonrpc\":\"2.0\",\"id\":7,\"error\":{\"code\":-32008,\"message\":\"Response is too big\",\"data\":\"Exceeded max limit of 100\"}}"
         (nth-value 1 (rpc:handle-body "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"web3_sha3\",\"params\":[\"0x00\"]}")))
    (is= "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32008,\"message\":\"Response is too big\",\"data\":\"Exceeded max limit of 100\"}}"
         (nth-value 1 (rpc:handle-body "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"net_listening\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"net_listening\"},{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"net_listening\"}]")))))

(defun closed-within-p (port seconds data)
  "A connection that sends DATA and then stalls is closed by the server within SECONDS."
  (multiple-value-bind (stream socket) (open-client port)
    (declare (ignore socket))
    (unwind-protect
         (let ((start (clempo.io:now-ms)))
           (when data (send stream data))
           (read-all stream)
           (< (- (clempo.io:now-ms) start) (* 1000 seconds)))
      (ignore-errors (close stream)))))

(deftest slow-and-idle-clients-are-dropped ()
  (with-limits ((server:*idle-seconds* 1) (server:*request-seconds* 1) (server:*write-seconds* 1))
    (with-server (port :max-connections 4)
      ;; generous bounds: the 1 s deadlines must fire, but a loaded host may be slow to run them
      (is (closed-within-p port 15 nil) "a silent connection")
      (is (closed-within-p port 15 "POST / HTTP/1.1") "a request line that never ends (slowloris)")
      (is (closed-within-p port 15 (crlf "POST / HTTP/1.1" "Content-Length: 100" "" "{\"jsonrpc\""))
          "a body that never ends")
      ;; the connection slots come back: fill them with silent clients, then wait them out
      (let ((idle (loop repeat 4 collect (open-client port))))
        (unwind-protect
             (progn (sleep 0.3)
                    (is (string= "" (exchange port (post +client-version-request+))) "over the connection limit")
                    (is (loop repeat 30 thereis (progn (sleep 0.5) (answers-p port)))
                        "slots freed by the idle timeout"))
          (mapc (lambda (s) (ignore-errors (close s :abort t))) idle))))))

(deftest stalled-reader-releases-its-connection ()
  ;; a client that never reads a large answer: the write deadline ends its connection
  (with-limits ((server:*write-seconds* 1))
    (let* ((n 60000)
           (batch (format nil "[~{~A~^,~}]" (loop for i below n collect +client-version-request+)))
           (s (server:start-server :port 0 :log nil)))
      (unwind-protect
           (multiple-value-bind (stream socket) (open-client (server:server-port s))
             (setf (sb-bsd-sockets:sockopt-receive-buffer socket) 4096)
             (send stream (post batch))
             (is (loop repeat 100
                       thereis (progn (sleep 0.1)
                                      (null (sb-thread:with-mutex ((server::server-lock s))
                                              (server::server-connections s)))))
                 "the stalled connection is closed")
             (ignore-errors (close stream :abort t)))
        (server:stop-server s)))))

(deftest concurrent-keep-alive-clients ()
  (with-server (port)
    (let* ((threads 24) (requests 25)
           (workers
             (loop for k below threads
                   collect (let ((k k))
                             (sb-thread:make-thread
                              (lambda ()
                                (handler-case
                                (multiple-value-bind (stream socket) (open-client port)
                                  (declare (ignore socket))
                                  (unwind-protect
                                       (loop for i below requests
                                             count (let ((body (format nil "{\"jsonrpc\":\"2.0\",\"id\":~D,\"method\":\"web3_sha3\",\"params\":[\"0x~(~4,'0x~)\"]}" i (+ (* 256 k) i))))
                                                     (send stream (post body))
                                                     (let* ((head (loop for line = (read-line-crlf stream)
                                                                        until (equal line "") collect line))
                                                            (len (parse-integer (subseq (find "content-length" head :test (lambda (a b) (search a b :test #'char-equal))) 16)))
                                                            (got (octets->string (read-exactly stream len))))
                                                       (equal (json:jget (json:parse-json got) "result")
                                                              (octets->hex (clempo.keccak:keccak256 (integer->octets (+ (* 256 k) i) 2)))))))
                                    (close stream)))
                                  (error () 0)))
                              :name "test client")))))
      (is= (* threads requests) (reduce #'+ (mapcar #'sb-thread:join-thread workers))))))

;;; WebSocket frames

(defun ws-open (port)
  (multiple-value-bind (stream socket) (open-client port)
    (declare (ignore socket))
    (send stream (crlf "GET / HTTP/1.1" "Host: x" "Upgrade: websocket" "Connection: Upgrade"
                       "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" "Sec-WebSocket-Version: 13" ""))
    (loop for line = (read-line-crlf stream) until (equal line ""))
    stream))

(defun close-code (stream)
  "The status code of the close frame the server sends next (NIL when it just hangs up)."
  (handler-case
      (multiple-value-bind (opcode payload) (ws:read-message stream)
        (and (eql opcode ws:+op-close+) (>= (length payload) 2) (octets->integer payload :end 2)))
    (error () nil)))

(defun frame-bytes (b0 len &key (masked t) (payload (make-octets 0)))
  (octets-concat (octets b0)
                 (cond ((< len 126) (octets (logior (if masked #x80 0) len)))
                       ((< len 65536) (octets-concat (octets (logior (if masked #x80 0) 126)) (integer->octets len 2)))
                       (t (octets-concat (octets (logior (if masked #x80 0) 127)) (integer->octets len 8))))
                 (if masked (make-octets 4) (make-octets 0))
                 payload))

(defun octets (&rest bytes) (coerce bytes 'octets))

(deftest websocket-limits ()
  (with-limits ((server:*request-seconds* 1))
    (with-server (port)
      (flet ((code-after (bytes)
               (let ((s (ws-open port)))
                 (unwind-protect (progn (send s bytes) (close-code s)) (ignore-errors (close s))))))
        (is= 1009 (code-after (frame-bytes #x81 (ash 1 40))) "a 1 TiB frame header")
        (is= 1009 (code-after (frame-bytes #x81 (1+ server:+body-max+))) "a frame over the message limit")
        (is= 1002 (code-after (frame-bytes #x81 5 :masked nil :payload (string->octets "hello"))) "an unmasked client frame")
        (is= 1002 (code-after (frame-bytes #x89 126 :payload (make-octets 126))) "a 126-byte ping")
        (is= 1002 (code-after (frame-bytes #x09 0)) "a fragmented ping")
        (is= 1002 (code-after (frame-bytes #xC1 0)) "reserved bits")
        (is= 1002 (code-after (frame-bytes #x83 0)) "a reserved opcode")
        (is= 1002 (code-after (frame-bytes #x80 0)) "a continuation without a message")
        (is= 1009 (code-after (apply #'octets-concat (frame-bytes #x01 1 :payload (octets 32))
                                     (loop repeat 1100 collect (frame-bytes #x00 1 :payload (octets 32)))))
            "too many fragments")
        ;; a frame that stops halfway: the connection is dropped
        (let ((s (ws-open port)) (start (clempo.io:now-ms)))
          (send s (frame-bytes #x81 100))
          (is (null (close-code s)))
          (is (< (- (clempo.io:now-ms) start) 5000) "a stalled frame is dropped by the deadline")
          (ignore-errors (close s)))
        ;; a well-formed request still works after all that
        (let ((s (ws-open port)))
          (unwind-protect
               (progn (ws:write-frame s ws:+op-text+ (string->octets +client-version-request+) :masked t)
                      (multiple-value-bind (opcode payload) (ws:read-message s)
                        (is= ws:+op-text+ opcode)
                        (is (search "clempo/" (octets->string payload)))))
            (ignore-errors (close s))))))))

;;; Subscriptions per connection

(defun handle (text) (nth-value 1 (rpc:handle-body text)))

(deftest subscriptions-per-connection-limit ()
  (let ((rpc:*connection* (server::make-connection)))
    (loop repeat server:+subscriptions-max+
          do (let ((server::*after-response* '()))
               (is (search "\"result\":\"0x" (handle "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_subscribe\",\"params\":[\"newHeads\"]}")))
               (mapc #'funcall server::*after-response*)))
    (is= server:+subscriptions-max+ (length (server:connection-subscriptions rpc:*connection*)))
    ;; as the reference answers the 1025th (taken before the params are read)
    (dolist (params '("[\"newHeads\"]" "[\"nope\"]"))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32006,\"message\":\"Too many subscriptions on the connection\",\"data\":\"Exceeded max limit of 1024\"}}"
           (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_subscribe\",\"params\":~A}" params))))
    ;; two accepted in one batch count before they start
    (let ((id (clempo.rpc.subscribe:subscription-id (first (server:connection-subscriptions rpc:*connection*)))))
      (is (search "true" (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_unsubscribe\",\"params\":[~S]}" id)))))
    (let ((server::*after-response* '()))
      (let ((answer (handle "[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_subscribe\",\"params\":[\"newHeads\"]},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"eth_subscribe\",\"params\":[\"newHeads\"]}]")))
        (is (search "\"id\":1,\"result\"" answer))
        (is (search "\"id\":2,\"error\":{\"code\":-32006" answer)))
      (mapc #'funcall server::*after-response*))
    (is= server:+subscriptions-max+ (length (server:connection-subscriptions rpc:*connection*)))))

(deftest subscriptions-change-safely-under-concurrency ()
  ;; adds and removes from many threads never lose an update
  (let ((c (server::make-connection)))
    (mapc #'sb-thread:join-thread
          (loop for k below 8
                collect (let ((k k))
                          (sb-thread:make-thread
                           (lambda ()
                             (dotimes (i 100)
                               (server:reserve-subscription c)
                               (server:add-subscription c (cons k i))
                               (when (oddp i)
                                 (server:remove-subscription-if c (lambda (s) (equal s (cons k i)))))))))))
    (is= 400 (length (server:connection-subscriptions c)))))

;;; The served chain follows the backend

(defclass fixed-backend (chain:backend) ())
(defmethod chain:backend-head ((b fixed-backend)) 0)

(defun answer-on (backend body)
  (json:parse-json (chain:with-backend-read (backend) (handle body))))

(defun call-on (backend method)
  (json:jget (answer-on backend (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":[]}" method))
             "result"))

(deftest chain-methods-follow-the-backend ()
  (let ((mainnet (make-instance 'fixed-backend))
        (moderato (make-instance 'fixed-backend :chain :moderato)))
    (is= "0x1079" (call-on mainnet "eth_chainId"))
    (is= "0xa5bf" (call-on moderato "eth_chainId"))
    (is= 4217 (json:jget (call-on mainnet "debug_chainConfig") "chainId"))
    (is= 42431 (json:jget (call-on moderato "debug_chainConfig") "chainId"))
    (is (not (json:json-equal (call-on mainnet "tempo_forkSchedule") (call-on moderato "tempo_forkSchedule"))))
    (is= "0xa5bf" (json:jget* (call-on moderato "eth_config") "current" "chainId"))))

;;; Installed filters expire as reth's do

(deftest stale-filters-are-evicted ()
  (let ((b (make-instance 'fixed-backend)))
    (flet ((call (method &rest params)
             (answer-on b (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":[~{~S~^,~}]}" method params))))
      (let ((kept (json:jget (call "eth_newBlockFilter") "result")))
        (is (vectorp (json:jget (call "eth_getFilterChanges" kept) "result")))
        (with-limits ((logs::*stale-filter-seconds* 0))
          (let ((id (json:jget (call "eth_newBlockFilter") "result")))
            (is= "filter not found" (json:jget* (call "eth_getFilterChanges" id) "error" "message"))
            (is= "filter not found" (json:jget* (call "eth_getFilterChanges" kept) "error" "message"))))
        (is (zerop (hash-table-count logs::*filters*)))))))

;;; Nesting: unbounded in the envelope and in params a method ignores, serde_json's limit of
;;; 128 within a typed param (answers recorded from the reference, 2026-11-21).

(deftest nesting-as-the-reference-answers ()
  (let ((b (make-instance 'fixed-backend))
        (deep (format nil "~A~A" (make-string 20000 :initial-element #\[) (make-string 20000 :initial-element #\]))))
    (flet ((nested-call (n)
             (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_call\",\"params\":[~{~A~}\"0x\"~A]}"
                     (loop repeat n collect "{\"a\":") (make-string n :initial-element #\}))))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x1079\"}"
           (chain:with-backend-read (b)
             (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_chainId\",\"params\":[~A]}" deep))))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\",\"data\":\"invalid type: sequence, expected u8 at line 1 column 1\"}}"
           (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"web3_sha3\",\"params\":[~A]}" deep)))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\",\"data\":\"recursion limit exceeded at line 1 column 636\"}}"
           (chain:with-backend-read (b) (handle (nested-call 128))))
      (is (not (search "recursion limit" (chain:with-backend-read (b) (handle (nested-call 127)))))))))

(deftest sha3-reads-u8-arrays-as-the-reference ()
  ;; recorded from the reference, 2026-11-21
  (loop for (params answer) in
        '(("[[1,2]]" "\"result\":\"0x22ae6da6b482f9b1b19b0b897c3fd43884180a1c5ee361e1107a1bc635649dda\"")
          ("[[]]" "\"result\":\"0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\"")
          ("[[256]]" "\"data\":\"invalid value: integer `256`, expected u8 at line 1 column 4\"")
          ("[[\"a\"]]" "\"data\":\"invalid type: string \\\"a\\\", expected u8 at line 1 column 4\"")
          ("[[1,[2]]]" "\"data\":\"invalid type: sequence, expected u8 at line 1 column 3\"")
          ("[{}]" "\"data\":\"invalid type: map, expected a variable number of bytes represented as a hex string, an array of u8, or raw bytes at line 1 column 2\""))
        do (is (search answer (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"web3_sha3\",\"params\":~A}" params)))
               "~A" params)))

(deftest byte-params-refuse-maps-where-the-reference-does ()
  ;; serde_json ends an empty map before reporting (recorded from the reference, 2026-11-21)
  (loop for (params column) in '(("[{}]" 2) ("[{\"a\":1}]" 1) ("[{\"a\":[1,2]}]" 1))
        do (is (search (format nil "an array of u8, or raw bytes at line 1 column ~D\"" column)
                       (handle (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBalance\",\"params\":~A}" params)))
               "~A" params)))

(deftest calls-are-counted-and-timed-per-method ()
  (flet ((count-of (method outcome)
           (or (clempo.metrics:metric-value rpc::*requests-metric*
                                            :labels (list (cons "method" method) (cons "outcome" outcome)))
               0)))
    (let ((ok (count-of "web3_clientVersion" "ok"))
          (bad (count-of "web3_sha3" "error"))
          (unknown (count-of "unknown" "error")))
      (handle +client-version-request+)
      (handle "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"web3_sha3\",\"params\":[\"zz\"]}")
      (handle "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"no_suchMethod\",\"params\":[]}")
      (is= (1+ ok) (count-of "web3_clientVersion" "ok"))
      (is= (1+ bad) (count-of "web3_sha3" "error"))
      (is= (1+ unknown) (count-of "unknown" "error"))
      (is (null (clempo.metrics:metric-value rpc::*requests-metric*
                                             :labels (list (cons "method" "no_suchMethod") (cons "outcome" "error")))))
      (is (plusp (car (clempo.metrics:metric-value rpc::*latency-metric*
                                                   :labels (list (cons "method" "web3_clientVersion")))))))))
