(defpackage #:clempo.rpc.ws-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:server #:clempo.rpcserver) (#:ws #:clempo.websocket)
                    (#:render #:clempo.rpc.render) (#:archive #:clempo.archive)))
(in-package #:clempo.rpc.ws-tests)

;;; The expected answers below were recorded from the reference node's websocket
;;; (tempo node --ws, ws://127.0.0.1:8546) on 2026-11-21.

;;; A websocket client: RFC 6455 handshake and masked client frames over sb-bsd-sockets.

(defstruct (client (:constructor %make-client (socket stream))) socket stream (queue '()) (pongs '()))

(defun read-line-crlf (stream)
  (let ((out (make-string-output-stream)))
    (loop for b = (read-byte stream)
          until (= b 10)
          unless (= b 13) do (write-char (code-char b) out))
    (get-output-stream-string out)))

(defun connect (port &key receive-buffer)
  "Open a websocket to 127.0.0.1:PORT; reads time out after 10 seconds. RECEIVE-BUFFER sets
SO_RCVBUF (a small one makes a client that stops reading stall the server quickly)."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (when receive-buffer (setf (sb-bsd-sockets:sockopt-receive-buffer socket) receive-buffer))
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
    (let* ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t :timeout 10
                                                             :element-type '(unsigned-byte 8)
                                                             :buffering :full))
           (key (ws:base64-encode (clempo.io:random-octets 16))))
      (write-sequence (string->octets
                       (format nil "GET / HTTP/1.1~C~CHost: 127.0.0.1:~D~C~CUpgrade: websocket~C~CConnection: Upgrade~C~CSec-WebSocket-Key: ~A~C~CSec-WebSocket-Version: 13~C~C~C~C"
                               #\Return #\Newline port #\Return #\Newline #\Return #\Newline #\Return #\Newline
                               key #\Return #\Newline #\Return #\Newline #\Return #\Newline))
                      stream)
      (force-output stream)
      (let ((status (read-line-crlf stream))
            (headers (loop for line = (read-line-crlf stream)
                           until (zerop (length line))
                           collect (let ((colon (position #\: line)))
                                     (cons (string-downcase (subseq line 0 colon))
                                           (string-trim " " (subseq line (1+ colon))))))))
        (invariant (search " 101 " status) "handshake refused: ~A" status)
        (invariant (equal (ws:accept-key key) (cdr (assoc "sec-websocket-accept" headers :test #'string=)))
                   "bad Sec-WebSocket-Accept")
        (%make-client socket stream)))))

(defun send-frame (client opcode payload &key (fin t))
  "One masked client frame."
  (let* ((stream (client-stream client))
         (payload (if (stringp payload) (string->octets payload) payload))
         (len (length payload))
         (mask (clempo.io:random-octets 4)))
    (write-byte (logior (if fin #x80 0) opcode) stream)
    (cond ((< len 126) (write-byte (logior #x80 len) stream))
          ((< len 65536) (write-byte (logior #x80 126) stream) (write-sequence (integer->octets len 2) stream))
          (t (write-byte (logior #x80 127) stream) (write-sequence (integer->octets len 8) stream)))
    (write-sequence mask stream)
    (write-sequence (let ((o (copy-seq payload)))
                      (dotimes (i len o) (setf (aref o i) (logxor (aref o i) (aref mask (mod i 4))))))
                    stream)
    (force-output stream)))

(defun receive (client)
  "The next data or close message: (values opcode payload); pongs are kept in CLIENT-PONGS."
  (ws:read-message (client-stream client)
                   (lambda (opcode payload)
                     (when (= opcode ws:+op-pong+)
                       (setf (client-pongs client) (append (client-pongs client) (list (octets->string payload))))))))

(defun receive-json (client)
  (multiple-value-bind (opcode payload) (receive client)
    (invariant (eql opcode ws:+op-text+) "expected a text message, got opcode ~A" opcode)
    (json:parse-json (octets->string payload))))

(defun call (client id method &optional (params #()))
  "Send a request and return its response object; notifications read meanwhile are queued."
  (send-frame client ws:+op-text+
              (json:to-json-string (json:jobject "jsonrpc" "2.0" "id" id "method" method "params" params)))
  (call-raw client id))

(defun call-raw (client id &optional text)
  (when text (send-frame client ws:+op-text+ text))
  (loop for m = (receive-json client)
        until (equal (json:jget m "id") id)
        do (setf (client-queue client) (append (client-queue client) (list m)))
        finally (return m)))

(defun notifications (client count)
  "At least the next COUNT notifications (subscriptions are pushed from the server's writer
thread, so they may come after later responses), with any others received before the
answer to one more round trip."
  (loop while (< (length (client-queue client)) count)
        do (let ((m (receive-json client)))
             (setf (client-queue client) (append (client-queue client) (list m)))))
  (call client "drain" "web3_clientVersion")
  (prog1 (client-queue client) (setf (client-queue client) '())))

(defun results-for (notes subscription)
  (loop for n in notes
        when (equal (json:jget* n "params" "subscription") subscription)
          collect (json:jget* n "params" "result")))

(defun close-client (client)
  (ignore-errors (close (client-stream client)))
  (ignore-errors (sb-bsd-sockets:socket-close (client-socket client))))

(defun subscription-id-p (s)
  (and (stringp s) (= (length s) 34) (string= "0x" s :end2 2)
       (every (lambda (c) (find c "0123456789abcdef")) (subseq s 2))))

;;; Parameters and errors, without a socket: the method runs as on a websocket connection.

(defun ws-answer (body)
  (let ((rpc:*connection* (server::make-connection nil nil))
        (server::*after-response* '()))
    (nth-value 1 (rpc:handle-body body))))

(defun request (method params)
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"~A\"~@[,\"params\":~A~]}" method params))

(defun error-text (data)
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"~@[,\"data\":~A~]}}"
          (and data (json:to-json-string data))))

(defparameter *subscribe-errors*
  '(("[]" "No more params")
    (nil "No more params")
    ("\"newHeads\"" "Invalid params. Expected one of '[', ']' or ',' but found '\"'")
    ("{\"kind\":\"newHeads\"}" "Invalid params. Expected one of '[', ']' or ',' but found '{'")
    ("[\"foo\"]" "unknown variant `foo`, expected one of `newHeads`, `logs`, `newPendingTransactions`, `syncing`, `transactionReceipts` at line 1 column 5")
    ("[ \"foo\" ]" "unknown variant `foo`, expected one of `newHeads`, `logs`, `newPendingTransactions`, `syncing`, `transactionReceipts` at line 1 column 6")
    ("[\"newheads\"]" "unknown variant `newheads`, expected one of `newHeads`, `logs`, `newPendingTransactions`, `syncing`, `transactionReceipts` at line 1 column 10")
    ("[1]" "expected value at line 1 column 1")
    ("[null]" "expected value at line 1 column 1")
    ("[true]" "expected value at line 1 column 1")
    ("[[]]" "expected value at line 1 column 1")
    ("[{}]" "expected value at line 1 column 2")
    ("[{ }]" "expected value at line 1 column 3")
    ("[{\"newHeads\":1}]" "invalid type: integer `1`, expected unit at line 1 column 13")
    ("[{\"newHeads\":{}}]" "invalid type: map, expected unit at line 1 column 12")
    ("[{\"newHeads\":\"x\"}]" "invalid type: string \"x\", expected unit at line 1 column 15")
    ("[{\"newHeads\":null,\"x\":1}]" "expected value at line 1 column 16")
    ("[{\"foo\":null}]" "unknown variant `foo`, expected one of `newHeads`, `logs`, `newPendingTransactions`, `syncing`, `transactionReceipts` at line 1 column 6")
    ("[\"logs\",{\"address\":\"bad\"}]" "Invalid variadic value or array type: data did not match any variant of untagged enum Variadic")
    ("[\"logs\",{\"topics\":\"x\",\"address\":\"bad\"}]" "Invalid variadic value or array type: data did not match any variant of untagged enum Variadic")
    ("[\"logs\",{\"topics\":\"x\",\"fromBlock\":\"bad\"}]" "hex string without 0x prefix")
    ("[\"logs\",{\"zzz\":1,\"topics\":\"x\"}]" "invalid type: string \"x\", expected a sequence")
    ("[\"logs\",{\"zzz\":1,\"address\":\"0x1\"}]" "Invalid variadic value or array type: data did not match any variant of untagged enum Variadic")
    ("[\"newHeads\",{\"foo\":1}]" "unknown field `foo`, expected one of `fromBlock`, `toBlock`, `address`, `topics`, `blockHash`")
    ("[\"newHeads\"  ,  {\"foo\"  :1}]" "unknown field `foo`, expected one of `fromBlock`, `toBlock`, `address`, `topics`, `blockHash`")
    ("[\"newHeads\",{\"foo\":1},3]" "unknown field `foo`, expected one of `fromBlock`, `toBlock`, `address`, `topics`, `blockHash`")
    ("[\"logs\",[1]]" "invalid type: sequence, expected Filter object")
    ("[\"logs\",1.5]" "invalid type: floating point `1.5`, expected Filter object")
    ("[\"logs\",5]" "invalid type: integer `5`, expected Filter object")
    ("[\"logs\",\"x\"]" "invalid type: string \"x\", expected Filter object")
    ("[\"syncing\",5]" "invalid type: integer `5`, expected Filter object")
    ("[\"newHeads\",-1]" "invalid type: integer `-1`, expected Filter object")
    ("[\"newHeads\",1e300]" "invalid type: floating point `1e+300`, expected Filter object")
    ("[\"newHeads\",1.5e20]" "invalid type: floating point `1.5e+20`, expected Filter object")
    ("[\"newHeads\",123456789012345678901]" "invalid type: floating point `1.2345678901234567e+20`, expected Filter object")
    ("[\"newHeads\",1e16]" "invalid type: floating point `1e+16`, expected Filter object")
    ("[\"newHeads\",10000000000000000]" "invalid type: integer `10000000000000000`, expected Filter object")
    ("[\"newHeads\",0.000001]" "invalid type: floating point `1e-6`, expected Filter object")
    ("[\"newHeads\",18446744073709551615]" "invalid type: integer `18446744073709551615`, expected Filter object")
    ("[\"newHeads\",18446744073709551616]" "invalid type: floating point `1.8446744073709552e+19`, expected Filter object")
    ("[\"newHeads\",-9223372036854775809]" "invalid type: floating point `-9.223372036854776e+18`, expected Filter object")
    ("[\"newHeads\",1E2]" "invalid type: floating point `100.0`, expected Filter object")
    ("[\"newHeads\",1e15]" "invalid type: floating point `1000000000000000.0`, expected Filter object")
    ("[\"newHeads\",123456789012345.6]" "invalid type: floating point `123456789012345.6`, expected Filter object")
    ("[\"newHeads\",-0]" "invalid type: floating point `-0.0`, expected Filter object")
    ("[\"newHeads\",0.1e1]" "invalid type: floating point `1.0`, expected Filter object")
    ("[\"newHeads\",1.00000000000000000000001]" "invalid type: floating point `1.0`, expected Filter object")
    ("[\"newHeads\",18446744073709551619]" "invalid type: floating point `1.8446744073709552e+19`, expected Filter object")
    ("[\"newHeads\",1844674407370955161.66]" "invalid type: floating point `1.8446744073709553e+18`, expected Filter object")
    ("[\"newHeads\",-1e-400]" "invalid type: floating point `-0.0`, expected Filter object")
    ("[\"newHeads\",2.5e-5]" "invalid type: floating point `0.000025`, expected Filter object")
    ("[\"newHeads\",123e-2]" "invalid type: floating point `1.23`, expected Filter object")
    ("[\"newHeads\",99999999999999999999999999]" "invalid type: floating point `1e+26`, expected Filter object")
    ("[\"newHeads\",[1,2]]" "invalid type: sequence, expected Filter object")
    ("[\"newPendingTransactions\",\"x\"]" "invalid type: string \"x\", expected Filter object")
    ("[\"logs\",{\"transactionHashes\":\"x\"}]" "invalid type: string \"x\", expected a sequence")
    ("[\"logs\",{\"transactionHashes\":[\"0x12\"]}]" "invalid string length")
    ("[\"logs\",{\"blockHash\":\"0x12\"}]" "invalid string length")
    ("[\"logs\",{\"fromBlock\":5}]" "invalid type: integer `5`, expected a string")
    ("[\"logs\",{\"fromBlock\":\"0x1\",\"blockHash\":\"0x1eebae980f7a9e9efd43e122562073085b2114da8699557a30d4df341271dcfa\"}]"
     "cannot specify both blockHash and fromBlock/toBlock, choose one or the other")
    ("[\"logs\",{\"topics\":[null,null,null,null,null]}]" "exceeded maximum topics len")))

(defparameter *subscribe-accepted*
  '("[\"newHeads\"]" "[ \"new\\u0048eads\" ]" "[{\"newHeads\":null}]" "[{\"newHeads\" : null }]"
    "[\"newHeads\",null]" "[\"newHeads\",true]" "[\"newHeads\",false]" "[\"newHeads\",{\"transactionHashes\":[]}]"
    "[\"logs\"]" "[\"logs\",true]" "[\"logs\",[]]" "[\"logs\",{\"fromBlock\":\"0x1\"}]"
    "[\"logs\",{\"transactionHashes\":null,\"foo\":1}]"
    "[\"logs\",{\"address\":\"0x1\",\"address\":\"0x20c000000000000000000000b9537d11c60e8b50\"}]"
    "[ {\"logs\":null}, {\"address\":\"0x20c000000000000000000000b9537d11c60e8b50\"}]"
    "[\"newPendingTransactions\"]" "[\"syncing\"]" "[\"transactionReceipts\"]"))

(deftest subscribe-params-as-reference ()
  (loop for (params data) in *subscribe-errors*
        do (is= (error-text data) (ws-answer (request "eth_subscribe" params)) "params ~A" params))
  (dolist (params *subscribe-accepted*)
    (let ((r (json:parse-json (ws-answer (request "eth_subscribe" params)))))
      (is (subscription-id-p (json:jget r "result")) "params ~A answered ~S" params r)))
  ;; jsonrpsee answers false to anything but one id of this connection
  (dolist (params '("[]" nil "null" "\"x\"" "{\"a\":1}" "[null]" "[123]" "[\"0x1234\",1]"
                    "[\"0x1234\"]" "[\"foo\"]"))
    (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":false}"
         (ws-answer (request "eth_unsubscribe" params)) "unsubscribe ~A" params)))

(deftest subscribe-over-http-is-internal-error ()
  (dolist (method '("eth_subscribe" "eth_unsubscribe"))
    (dolist (params '("[]" nil "\"x\"" "[\"newHeads\"]" "[\"foo\"]" "{\"a\":1}"))
      (is= "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}"
           (nth-value 1 (rpc:handle-body (request method params))) "~A ~A" method params))))

;;; A live server over the block archive

(defclass test-backend (chain:archive-backend) ()
  (:documentation "The archive with a head the test moves."))

(defparameter *block* 3463 "A mainnet block with two transactions and three logs.")

(defun test-backend ()
  "A backend headed one below *BLOCK*, or a skip when the archive or the block's stored
receipts are absent."
  (unless (archive:chunk-present-p 0 (archive:archive-dir))
    (skip-test "no mainnet archive under ~/cache/clempo/mainnet"))
  (let ((b (chain:make-archive-backend :class 'test-backend :head (1- *block*))))
    (unless (chain:receipts-store-get (chain::archive-backend-receipts b) *block*)
      (skip-test (format nil "receipts of block ~D are not in ~~/cache/clempo/receipts" *block*)))
    b))

(defmacro with-test-server ((server backend &optional (log (gensym "LOG"))) &body body)
  "Run BODY with SERVER serving BACKEND (a TEST-BACKEND), its log going to the string output
stream LOG."
  `(let* ((,backend (test-backend))
          (,log (make-string-output-stream))
          (,server (server:start-server :port 0 :backend ,backend :log ,log)))
     (declare (ignorable ,log))
     (unwind-protect (progn ,@body)
       (server:stop-server ,server))))

(defun answer (backend body)
  "BODY answered over HTTP against BACKEND, parsed."
  (json:parse-json (chain:with-backend-read (backend) (nth-value 1 (rpc:handle-body body)))))

(defun advance-head (server backend)
  (setf (chain:archive-backend-head backend) *block*)
  (server:publish-head server *block*))

(defparameter +transfer+ "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")

(deftest ws-request-ping-close ()
  (with-test-server (server backend)
    (let ((c (connect (server:server-port server))))
      (unwind-protect
           (progn
             (is= (json:parse-json (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"~A\"}" (quantity->hex (1- *block*))))
                  (call c 1 "eth_blockNumber"))
             ;; a fragmented text message
             (send-frame c ws:+op-text+ "{\"jsonrpc\":\"2.0\",\"id\":2," :fin nil)
             (send-frame c 0 "\"method\":\"eth_chainId\"}")
             (is= (json:parse-json "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"0x1079\"}") (call-raw c 2))
             ;; a batch, and a notification (no answer)
             (send-frame c ws:+op-text+ "{\"jsonrpc\":\"2.0\",\"method\":\"eth_chainId\"}")
             (send-frame c ws:+op-text+ "[{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"eth_chainId\"},{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"eth_chainId\"}]")
             (let ((batch (receive-json c)))
               (is= 2 (length batch))
               (is= '(3 4) (map 'list (lambda (r) (json:jget r "id")) batch)))
             (send-frame c ws:+op-ping+ "hi")
             (is= clempo.rpc.eth:*client-version* (json:jget (call c 5 "web3_clientVersion") "result"))
             (is= '("hi") (client-pongs c))
             (send-frame c ws:+op-close+ (integer->octets 1000 2))
             (multiple-value-bind (op payload) (receive c)
               (is= ws:+op-close+ op)
               (is-equalp (integer->octets 1000 2) payload))
             (is (null (receive c)) "the server closes after the close handshake"))
        (close-client c)))))

(deftest ws-subscriptions-notify ()
  (with-test-server (server backend)
    (let ((c (connect (server:server-port server))))
      (unwind-protect
           (let* ((heads (json:jget (call c 1 "eth_subscribe" #("newHeads")) "result"))
                  (all-logs (json:jget (call c 2 "eth_subscribe" #("logs")) "result"))
                  (transfers (json:jget (call c 3 "eth_subscribe"
                                              (vector "logs" (json:jobject "topics" (vector (vector +transfer+)))))
                                        "result"))
                  (elsewhere (json:jget (call c 4 "eth_subscribe"
                                              (vector "logs" (json:jobject "address" "0x20c000000000000000000000b9537d11c60e8b50")))
                                        "result"))
                  (later (json:jget (call c 5 "eth_subscribe" (vector "logs" (json:jobject "fromBlock" (quantity->hex (1+ *block*)))))
                                    "result"))
                  (refused (json:jget (call c 6 "eth_subscribe" #("logs" :true)) "result"))
                  (receipts (json:jget (call c 7 "eth_subscribe" #("transactionReceipts")) "result"))
                  (syncing (json:jget (call c 8 "eth_subscribe" #("syncing")) "result")))
             (dolist (id (list heads all-logs transfers elsewhere later refused receipts syncing))
               (is (subscription-id-p id) "subscription id ~S" id))
             (is= 8 (length (remove-duplicates (list heads all-logs transfers elsewhere later refused receipts syncing)
                                               :test #'equal)))
             ;; syncing says false once, right after its id
             (is-equalp (list :false) (results-for (notifications c 1) syncing))
             (advance-head server backend)
             (let ((notes (notifications c 7))
                   (expected-logs (json:jget (answer backend
                                                     (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getLogs\",\"params\":[{\"fromBlock\":\"~A\",\"toBlock\":\"~:*~A\"}]}"
                                                             (quantity->hex *block*)))
                                             "result")))
               (is (every (lambda (n) (and (equal "2.0" (json:jget n "jsonrpc"))
                                           (equal "eth_subscription" (json:jget n "method"))))
                          notes))
               (let ((h (results-for notes heads)))
                 (is= 1 (length h))
                 (let ((h (first h)))
                   ;; recorded from the reference's eth_getBlockByNumber
                   (is= "0x37ebc26f23baadc49718923d7d732a128099c229003435d3e653320e34f12a6d" (json:jget h "hash"))
                   (is= "0x581" (json:jget h "size"))
                   (is= "0xd87" (json:jget h "number"))
                   (is= "0x19bc8b42dfb" (json:jget h "timestampMillis"))
                   (is (null (json:jget h "transactions")) "a header carries no transactions")
                   ;; the reference's newHeads item is its block without the body fields
                   (let ((block (json:jget (answer backend (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockByNumber\",\"params\":[\"~A\",false]}"
                                                                   (quantity->hex *block*)))
                                           "result")))
                     (is (json:json-equal (cons :object (remove-if (lambda (p) (member (car p) '("uncles" "transactions" "withdrawals")
                                                                                        :test #'string=))
                                                                   (cdr block)))
                                          h)
                         "newHeads item = eth_getBlockByNumber without uncles, transactions, withdrawals"))))
               (let ((logs (results-for notes all-logs)))
                 (is= 3 (length logs))
                 (is (json:json-equal expected-logs (coerce logs 'simple-vector)) "logs match eth_getLogs")
                 (is= '("0x0" "0x1" "0x2") (mapcar (lambda (l) (json:jget l "logIndex")) logs))
                 (is (every (lambda (l) (eq :false (json:jget l "removed"))) logs)))
               (is= '("0x0" "0x2") (mapcar (lambda (l) (json:jget l "logIndex")) (results-for notes transfers)))
               (is (null (results-for notes elsewhere)))
               (is (null (results-for notes later)) "fromBlock above the block")
               (is (null (results-for notes refused)) "logs with a boolean never notifies")
               (let ((r (results-for notes receipts)))
                 (is= 1 (length r))
                 (is= 2 (length (first r)))
                 ;; the reference sends what eth_getBlockReceipts answers
                 (is (json:json-equal (json:jget (answer backend (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_getBlockReceipts\",\"params\":[\"~A\"]}"
                                                                         (quantity->hex *block*)))
                                                 "result")
                                      (first r))
                     "transactionReceipts item = eth_getBlockReceipts"))
               (is (null (results-for notes syncing)))
               (is= (+ 1 3 2 1) (length notes)))
             ;; unsubscribe: true once, then false; a refused subscription never ran
             (is= :true (json:jget (call c 10 "eth_unsubscribe" (vector heads)) "result"))
             (is= :false (json:jget (call c 11 "eth_unsubscribe" (vector heads)) "result"))
             (is= :false (json:jget (call c 12 "eth_unsubscribe" (vector refused)) "result"))
             (is= :true (json:jget (call c 13 "eth_unsubscribe" (vector syncing)) "result"))
             (server:publish-head server *block*)
             (let ((notes (notifications c 6)))
               (is= 6 (length notes))
               (is (null (results-for notes heads)))
               (is (null (results-for notes syncing)))
               (is= 3 (length (results-for notes all-logs)))
               (is= 2 (length (results-for notes transfers)))
               (is= 1 (length (results-for notes receipts)))))
        (close-client c)))))

(deftest ws-concurrent-clients ()
  (with-test-server (server backend)
    (let* ((n 8)
           (clients (loop repeat n collect (connect (server:server-port server))))
           (ids (make-array n)))
      (unwind-protect
           (progn
             ;; every client subscribes and calls at once
             (let ((threads (loop for c in clients for i from 0
                                  collect (let ((c c) (i i))
                                            (sb-thread:make-thread
                                             (lambda ()
                                               (setf (aref ids i) (json:jget (call c 1 "eth_subscribe" #("newHeads")) "result"))
                                               (loop for k from 2 below 20
                                                     always (equal (quantity->hex (1- *block*))
                                                                   (json:jget (call c k "eth_blockNumber") "result")))))))))
               (is (every #'sb-thread:join-thread threads) "every client's answers"))
             (is= n (length (remove-duplicates (coerce ids 'list) :test #'equal)))
             (advance-head server backend)
             (loop for c in clients for i from 0
                   do (let ((h (results-for (notifications c 1) (aref ids i))))
                        (is= 1 (length h))
                        (is= "0xd87" (json:jget (first h) "number")))))
        (mapc #'close-client clients)))))

(deftest ws-control-frames-between-fragments ()
  ;; RFC 6455 5.4: control frames may be injected in the middle of a fragmented message
  (let ((server (server:start-server :port 0 :log nil)))
    (unwind-protect
         (let ((c (connect (server:server-port server))))
           (unwind-protect
                (progn
                  (send-frame c ws:+op-text+ "{\"jsonrpc\":\"2.0\"," :fin nil)
                  (send-frame c ws:+op-ping+ "first")
                  (send-frame c 0 "\"id\":7," :fin nil)
                  (send-frame c ws:+op-pong+ "unsolicited")
                  (send-frame c ws:+op-ping+ "second")
                  (send-frame c 0 "\"method\":\"web3_sha3\",\"params\":[\"0x\"]}")
                  (is= (json:parse-json "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":\"0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\"}")
                       (call-raw c 7))
                  (is= '("first" "second") (client-pongs c))
                  ;; a close between fragments ends the connection
                  (send-frame c ws:+op-text+ "{\"jsonrpc\":" :fin nil)
                  (send-frame c ws:+op-close+ (integer->octets 1000 2))
                  (is= ws:+op-close+ (receive c))
                  (is (null (receive c))))
             (close-client c)))
      (server:stop-server server))))

;;; Slow subscribers: PUBLISH-HEAD only queues, a lagging connection skips its oldest heads.

(defun head-numbers (notes subscription)
  (mapcar (lambda (h) (parse-integer (json:jget h "number") :start 2 :radix 16))
          (results-for notes subscription)))

(defun strictly-increasing-p (list) (every #'< list (rest list)))

(deftest ws-stalled-client-never-blocks-the-follower ()
  (with-test-server (server backend log)
    (let* ((first 2000) (last 3999)
           (stalled (connect (server:server-port server) :receive-buffer 4096))
           (live (connect (server:server-port server))))
      (unwind-protect
           (let ((stalled-heads (json:jget (call stalled 1 "eth_subscribe" #("newHeads")) "result"))
                 (stalled-logs (json:jget (call stalled 2 "eth_subscribe" #("logs")) "result"))
                 (live-heads (json:jget (call live 1 "eth_subscribe" #("newHeads")) "result")))
             (declare (ignore stalled-logs))
             (setf (chain:archive-backend-head backend) last)
             ;; LIVE reads on its own thread until the last head; STALLED reads nothing.
             (let* ((reader (sb-thread:make-thread
                             (lambda ()
                               (loop for m = (receive-json live)
                                     collect m
                                     until (equal (json:jget* m "params" "result" "number") (quantity->hex last))))))
                    (follower (sb-thread:make-thread
                               (lambda ()
                                 ;; as the node commits: under the write lock, then publish
                                 (loop for n from first to last
                                       maximize (let ((start (get-internal-real-time)))
                                                  (chain:with-backend-write (backend) nil)
                                                  (server:publish-head server n)
                                                  (- (get-internal-real-time) start))))))
                    (slowest (sb-thread:join-thread follower :default :blocked :timeout 60)))
               (is (integerp slowest) "the follower finished (~S)" slowest)
               (when (integerp slowest)
                 (is (< slowest (* 2 internal-time-units-per-second)) "slowest commit+publish ~D ms"
                     (round (* 1000 slowest) internal-time-units-per-second)))
               (let ((numbers (head-numbers (sb-thread:join-thread reader :default '() :timeout 60) live-heads)))
                 (is= last (car (last numbers)))
                 (is (strictly-increasing-p numbers) "the live client's heads are in order")))
             ;; the live client has everything; the stalled one's writer is blocked on its
             ;; socket with a full queue
             (is= (list server::+outbox-capacity+ 0)
                  (sort (mapcar #'server::connection-outbox-length (server::server-connections server)) #'>))
             (is (plusp (reduce #'+ (mapcar #'server::connection-skipped (server::server-connections server)))))
             (is (search "websocket subscriber lagging behind" (get-output-stream-string log)))
             ;; once STALLED reads again it gets the newest heads, in order, the oldest skipped
             (let ((numbers (head-numbers (loop for m = (receive-json stalled)
                                                collect m
                                                until (equal (json:jget* m "params" "result" "number") (quantity->hex last)))
                                          stalled-heads)))
               (is= last (car (last numbers)))
               (is (strictly-increasing-p numbers) "the stalled client's heads are in order")
               (is (< (length numbers) (1+ (- last first))) "~D heads arrived" (length numbers))))
        (close-client stalled)
        (close-client live)))))

(deftest ws-notification-errors-are-logged ()
  (with-test-server (server backend log)
    ;; a block whose receipts are not stored, and no upstream to fetch them
    (let ((n (loop for n from 49000000 below 49000100
                   when (and (chain:backend-block-raw (progn (setf (chain:archive-backend-head backend) n) backend) n)
                             (null (chain:receipts-store-get (chain::archive-backend-receipts backend) n)))
                     return n)))
      (unless n (skip-test "no archived block without stored receipts near 49,000,000"))
      (let ((c (connect (server:server-port server))))
        (unwind-protect
             (let ((heads (json:jget (call c 1 "eth_subscribe" #("newHeads")) "result"))
                   (logs (json:jget (call c 2 "eth_subscribe" #("logs")) "result")))
               (server:publish-head server n)
               (let ((notes (notifications c 1)))
                 (is= (list (quantity->hex n)) (mapcar (lambda (h) (json:jget h "number")) (results-for notes heads)))
                 (is (null (results-for notes logs))))
               (let ((text (get-output-stream-string log)))
                 (is (search (format nil "logs subscription ~A, block ~D: receipts of block ~D are not available" logs n n)
                             text)
                     "log: ~S" text)))
          (close-client c))))))
