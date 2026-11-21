(defpackage #:clempo.http
  (:use #:cl #:clempo.base #:clempo.io)
  (:documentation "Minimal HTTP/1.1 client. Plain http keeps pooled keep-alive sockets;
https goes through curl (docs/DEVIATIONS.md: no TLS stack in clempo).")
  (:export #:http-post #:http-get #:http-error #:parse-url))

(in-package #:clempo.http)

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message))
  (:report (lambda (c s) (format s "http: ~A" (http-error-message c)))))

(defun http-fail (fmt &rest args) (error 'http-error :message (apply #'format nil fmt args)))

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
                (if (string= scheme "https") 443 80))
            path)))

;;; Connection pool for plain http.

(defvar *pool* (make-hash-table :test 'equal))
(defvar *pool-lock* (sb-thread:make-mutex :name "http pool"))

(defstruct conn socket stream)

(defun resolve (host)
  (if (string= host "localhost")
      #(127 0 0 1)
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))

(defun open-conn (host port)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay s) t)
    (sb-bsd-sockets:socket-connect s (resolve host) port)
    (make-conn :socket s
               :stream (sb-bsd-sockets:socket-make-stream
                        s :input t :output t :element-type '(unsigned-byte 8)
                          :buffering :full))))

(defun take-conn (key host port)
  (or (sb-thread:with-mutex (*pool-lock*) (pop (gethash key *pool*)))
      (open-conn host port)))

(defun give-conn (key conn)
  (sb-thread:with-mutex (*pool-lock*) (push conn (gethash key *pool*))))

(defun close-conn (conn)
  (ignore-errors (sb-bsd-sockets:socket-close (conn-socket conn))))

(defun read-line-octets (stream)
  "Header line as a string without CRLF; NIL at EOF."
  (let ((out (make-string-output-stream)) (any nil))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun read-exact (stream n)
  (let ((o (make-octets n)))
    (let ((got (read-sequence o stream)))
      (unless (= got n) (http-fail "short body: ~D of ~D" got n)))
    o))

(defun read-response (stream)
  "Returns (values status body keep-alive-p)."
  (let ((status-line (read-line-octets stream)))
    (unless status-line (http-fail "connection closed"))
    (let ((status (parse-integer status-line :start 9 :end 12))
          (length nil) (chunked nil) (close nil))
      (loop for line = (read-line-octets stream)
            until (or (null line) (zerop (length line)))
            do (let* ((colon (position #\: line))
                      (name (string-downcase (subseq line 0 colon)))
                      (value (string-trim " " (subseq line (1+ colon)))))
                 (cond ((string= name "content-length") (setf length (parse-integer value)))
                       ((string= name "transfer-encoding")
                        (setf chunked (search "chunked" (string-downcase value))))
                       ((string= name "connection")
                        (setf close (string-equal value "close"))))))
      (let ((body
              (cond (chunked
                     (let ((parts '()))
                       (loop for size-line = (read-line-octets stream)
                             for size = (parse-integer size-line :radix 16 :junk-allowed t)
                             until (zerop size)
                             do (push (read-exact stream size) parts)
                                (read-line-octets stream))
                       (loop for line = (read-line-octets stream)
                             until (or (null line) (zerop (length line))))
                       (apply #'octets-concat (nreverse parts))))
                    (length (read-exact stream length))
                    (t (setf close t)
                       (let ((parts '()) (buf (make-octets 65536)))
                         (loop for n = (read-sequence buf stream)
                               while (plusp n) do (push (subseq buf 0 n) parts))
                         (apply #'octets-concat (nreverse parts)))))))
        (values status body (not close))))))

(defun plain-request (method host port path body headers)
  (let ((key (format nil "~A:~D" host port)))
    (loop for attempt from 0 below 2
          do (let ((conn (take-conn key host port)))
               (handler-case
                   (let ((s (conn-stream conn)))
                     (write-sequence
                      (string->octets
                       (with-output-to-string (h)
                         (format h "~A ~A HTTP/1.1~C~CHost: ~A:~D~C~C" method path
                                 #\Return #\Newline host port #\Return #\Newline)
                         (loop for (k . v) in headers
                               do (format h "~A: ~A~C~C" k v #\Return #\Newline))
                         (format h "Content-Length: ~D~C~C~C~C" (length body)
                                 #\Return #\Newline #\Return #\Newline)))
                      s)
                     (write-sequence body s)
                     (force-output s)
                     (multiple-value-bind (status rbody keep) (read-response s)
                       (if keep (give-conn key conn) (close-conn conn))
                       (return-from plain-request (values status rbody))))
                 ((or error sb-sys:interactive-interrupt) (e)
                   (close-conn conn)
                   ;; A pooled socket may have been closed by the server; retry once fresh.
                   (when (= attempt 1) (error e))))))))

(defun curl-request (method url body headers)
  (let* ((out (format nil "/tmp/clempo-curl-~D-~D.out" (sb-unix:unix-getpid)
                      (sb-ext:atomic-incf (car (load-time-value (list 0))))))
         (args (append (list "-s" "-S" "-X" method "-o" out "-w" "%{http_code}" "--max-time" "120")
                       (loop for (k . v) in headers append (list "-H" (format nil "~A: ~A" k v)))
                       (when body (list "--data-binary" "@-"))
                       (list url))))
    (unwind-protect
         (multiple-value-bind (code-out exit) (run-capture "curl" args :input body)
           (unless (zerop exit) (http-fail "curl exited ~D for ~A" exit url))
           (values (parse-integer (octets->string code-out))
                   (if (probe-file out) (read-file-octets out) (make-octets 0))))
      (when (probe-file out) (delete-file out)))))

(defun http-post (url body &key (headers '(("Content-Type" . "application/json"))))
  "POST BODY (octets). Returns (values status body-octets)."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (if (string= scheme "https")
        (curl-request "POST" url body headers)
        (plain-request "POST" host port path body headers))))

(defun http-get (url &key headers)
  (multiple-value-bind (scheme host port path) (parse-url url)
    (if (string= scheme "https")
        (curl-request "GET" url nil headers)
        (plain-request "GET" host port path (make-octets 0) headers))))
