(defpackage #:clempo.http
  (:use #:cl #:clempo.base #:clempo.io)
  (:documentation "Minimal HTTP/1.1 client over pooled keep-alive connections: plain TCP for
http://, clempo.tls for https://. Connects and reads time out, and responses are capped.")
  (:export #:http-post #:http-get #:http-error #:parse-url #:open-connection
           #:*connect-timeout* #:*read-timeout* #:*max-response-bytes*))

(in-package #:clempo.http)

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message))
  (:report (lambda (c s) (format s "http: ~A" (http-error-message c)))))

(defun http-fail (fmt &rest args) (error 'http-error :message (apply #'format nil fmt args)))

(defvar *connect-timeout* 30 "Seconds to establish a TCP connection.")
(defvar *read-timeout* 120 "Seconds a read may block before the request fails.")
(defvar *max-response-bytes* (* 512 1024 1024) "Largest response body accepted.")
(defparameter *max-header-line* 65536)
(defparameter *max-headers* 256)

(defun parse-url (url)
  "Returns (values scheme host port path)."
  (let* ((sep (search "://" url))
         (scheme (if sep (string-downcase (subseq url 0 sep)) "http"))
         (rest (if sep (subseq url (+ sep 3)) url))
         (slash (position #\/ rest))
         (hostport (subseq rest 0 (or slash (length rest))))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: hostport)))
    (values scheme
            (if colon (subseq hostport 0 colon) hostport)
            (if colon
                (parse-integer hostport :start (1+ colon))
                (if (member scheme '("https" "wss") :test #'string=) 443 80))
            path)))

;;; Connections.

(defvar *pool* (make-hash-table :test 'equal))
(defvar *pool-lock* (sb-thread:make-mutex :name "http pool"))

(defstruct conn socket stream)

(defun resolve (host)
  (if (string= host "localhost")
      #(127 0 0 1)
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))

(defun connect-with-timeout (socket address port)
  "Non-blocking connect, waiting at most *CONNECT-TIMEOUT* seconds."
  (setf (sb-bsd-sockets:non-blocking-mode socket) t)
  (handler-case (sb-bsd-sockets:socket-connect socket address port)
    (sb-bsd-sockets:operation-in-progress ()
      (unless (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor socket)
                                           :output *connect-timeout*)
        (http-fail "connect to ~{~D~^.~}:~D timed out" (coerce address 'list) port))
      ;; A connected socket has a peer; otherwise connecting again reports why it failed.
      (handler-case (sb-bsd-sockets:socket-peername socket)
        (sb-bsd-sockets:socket-error ()
          (sb-bsd-sockets:socket-connect socket address port)
          (http-fail "connect to ~{~D~^.~}:~D failed" (coerce address 'list) port)))))
  (setf (sb-bsd-sockets:non-blocking-mode socket) nil))

(defun open-connection (scheme host port &key (alpn '("http/1.1")))
  "A connected binary stream to HOST:PORT, wrapped in TLS for https/wss. Values stream, socket."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-bsd-sockets:socket-close s)))))
      (setf (sb-bsd-sockets:sockopt-tcp-nodelay s) t)
      (connect-with-timeout s (resolve host) port)
      (let ((stream (sb-bsd-sockets:socket-make-stream
                     s :input t :output t :element-type '(unsigned-byte 8)
                       :buffering :full :timeout *read-timeout*)))
        (values (if (member scheme '("https" "wss") :test #'string=)
                    (clempo.tls:tls-connect stream host :alpn alpn)
                    stream)
                s)))))

(defun open-conn (scheme host port)
  (multiple-value-bind (stream socket) (open-connection scheme host port)
    (make-conn :socket socket :stream stream)))

(defun take-conn (key scheme host port)
  "A pooled connection (second value T) or a new one."
  (let ((pooled (sb-thread:with-mutex (*pool-lock*) (pop (gethash key *pool*)))))
    (if pooled (values pooled t) (values (open-conn scheme host port) nil))))

(defun give-conn (key conn)
  (sb-thread:with-mutex (*pool-lock*) (push conn (gethash key *pool*))))

(defun close-conn (conn)
  (ignore-errors (close (conn-stream conn) :abort t))
  (ignore-errors (sb-bsd-sockets:socket-close (conn-socket conn))))

;;; Responses.

(defun read-line-octets (stream)
  "Header line as a string without CRLF; NIL at EOF."
  (let ((out (make-string-output-stream)) (any nil) (n 0))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   ((> (incf n) *max-header-line*) (http-fail "header line over ~D bytes" *max-header-line*))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun read-exact (stream n)
  (let ((o (make-octets n)))
    (let ((got (read-sequence o stream)))
      (unless (= got n) (http-fail "short body: ~D of ~D" got n)))
    o))

(defun check-size (n)
  (when (> n *max-response-bytes*) (http-fail "response over ~D bytes" *max-response-bytes*)))

(defun read-response (stream)
  "Returns (values status body keep-alive-p)."
  (let ((status-line (read-line-octets stream)))
    (unless status-line (http-fail "connection closed"))
    (unless (and (>= (length status-line) 12) (string= "HTTP/1." status-line :end2 7))
      (http-fail "bad status line ~S" (subseq status-line 0 (min 64 (length status-line)))))
    (let ((status (or (parse-integer status-line :start 9 :end 12 :junk-allowed t)
                      (http-fail "bad status line")))
          (length nil) (chunked nil) (close nil))
      (loop for line = (read-line-octets stream)
            for count from 0
            until (or (null line) (zerop (length line)))
            do (when (> count *max-headers*) (http-fail "more than ~D headers" *max-headers*))
               (let ((colon (or (position #\: line) (http-fail "malformed header"))))
                 (let ((name (string-downcase (subseq line 0 colon)))
                       (value (string-trim " " (subseq line (1+ colon)))))
                   (cond ((string= name "content-length")
                          (setf length (or (ignore-errors (parse-integer value))
                                           (http-fail "bad content-length")))
                          (when (minusp length) (http-fail "bad content-length"))
                          (check-size length))
                         ((string= name "transfer-encoding")
                          (setf chunked (search "chunked" (string-downcase value))))
                         ((string= name "connection")
                          (setf close (string-equal value "close")))))))
      (let ((body
              (cond (chunked
                     (let ((parts '()) (total 0))
                       (loop for size-line = (or (read-line-octets stream) (http-fail "truncated chunk"))
                             for size = (or (parse-integer size-line :radix 16 :junk-allowed t)
                                            (http-fail "bad chunk size"))
                             until (zerop size)
                             do (check-size (incf total size))
                                (push (read-exact stream size) parts)
                                (read-line-octets stream))
                       (loop for line = (read-line-octets stream)
                             until (or (null line) (zerop (length line))))
                       (apply #'octets-concat (nreverse parts))))
                    (length (read-exact stream length))
                    (t (setf close t)
                       (let ((parts '()) (total 0) (buf (make-octets 65536)))
                         (loop for n = (read-sequence buf stream)
                               while (plusp n)
                               do (check-size (incf total n)) (push (subseq buf 0 n) parts))
                         (apply #'octets-concat (nreverse parts)))))))
        (values status body (not close))))))

(defun request (method url body headers)
  (multiple-value-bind (scheme host port path) (parse-url url)
    (unless (member scheme '("http" "https") :test #'string=)
      (http-fail "unsupported scheme ~A" scheme))
    (loop for (k . v) in headers
          when (find-if (lambda (c) (member c '(#\Return #\Newline))) (concatenate 'string k v))
            do (http-fail "header ~S contains a line break" k))
    ;; a pooled stream keeps the read timeout it was opened with: reuse it only under that one
    (let ((key (format nil "~A://~A:~D ~A" scheme host port *read-timeout*))
          (head (string->octets
                 (with-output-to-string (h)
                   (format h "~A ~A HTTP/1.1~C~CHost: ~A~@[:~D~]~C~C" method path
                           #\Return #\Newline host
                           (unless (= port (if (string= scheme "https") 443 80)) port)
                           #\Return #\Newline)
                   (loop for (k . v) in headers
                         do (format h "~A: ~A~C~C" k v #\Return #\Newline))
                   (format h "Content-Length: ~D~C~C~C~C" (length body)
                           #\Return #\Newline #\Return #\Newline)))))
      (loop
        (multiple-value-bind (conn reused) (take-conn key scheme host port)
          (handler-case
              (let ((s (conn-stream conn)))
                (write-sequence head s)
                (write-sequence body s)
                (force-output s)
                (multiple-value-bind (status rbody keep) (read-response s)
                  (if keep (give-conn key conn) (close-conn conn))
                  (return-from request (values status rbody))))
            ((or error sb-sys:interactive-interrupt) (e)
              (close-conn conn)
              ;; A pooled connection may have been closed by the server: try the next
              ;; one, then a fresh one. A failure on a fresh connection is final.
              (unless (and reused (not (typep e 'http-error))) (error e)))))))))

(defun http-post (url body &key (headers '(("Content-Type" . "application/json"))))
  "POST BODY (octets). Returns (values status body-octets)."
  (request "POST" url body headers))

(defun http-get (url &key headers)
  (request "GET" url (make-octets 0) headers))
