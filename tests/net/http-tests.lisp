(defpackage #:clempo.http.tests (:use #:cl #:clempo.base #:clempo.http))
(in-package #:clempo.http.tests)

;;; A one-connection local server whose behavior each test scripts.

(defun call-with-server (handler function)
  "Listen on an ephemeral port; HANDLER gets the accepted connection's binary stream."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 4)
    (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
           (thread (sb-thread:make-thread
                    (lambda ()
                      (let ((client (sb-bsd-sockets:socket-accept listener)))
                        (unwind-protect
                             (ignore-errors
                              (funcall handler (sb-bsd-sockets:socket-make-stream
                                                client :input t :output t :buffering :full
                                                       :element-type '(unsigned-byte 8))))
                          (sb-bsd-sockets:socket-close client))))
                    :name "http test server")))
      (unwind-protect (funcall function (format nil "http://127.0.0.1:~D/" port) port)
        (ignore-errors (sb-thread:terminate-thread thread))
        (ignore-errors (sb-thread:join-thread thread :default nil :timeout 5))
        (sb-bsd-sockets:socket-close listener)))))

(defun read-request-head (stream)
  (let ((last '()))
    (loop for b = (read-byte stream nil nil)
          while b
          do (setf last (cons b (subseq last 0 (min 3 (length last)))))
          until (equal last '(10 13 10 13)))))

(defun respond (stream text)
  (write-sequence (string->octets text) stream)
  (force-output stream))

(defun crlf (&rest lines) (format nil "~{~A~C~C~}" (loop for l in lines append (list l #\Return #\Newline))))

(deftest keep-alive-reuses-the-connection ()
  ;; The server accepts only one connection, so the second request must reuse it.
  (call-with-server
   (lambda (s)
     (dotimes (i 2)
       (read-request-head s)
       (respond s (crlf "HTTP/1.1 200 OK" "Content-Length: 2" ""))
       (respond s (format nil "~D~D" i i))))
   (lambda (url port)
     (declare (ignore port))
     (is= "00" (octets->string (nth-value 1 (http-get url))))
     (is= "11" (octets->string (nth-value 1 (http-get url)))))))

(deftest read-timeout ()
  (call-with-server
   (lambda (s) (read-request-head s) (sleep 10))
   (lambda (url port)
     (declare (ignore port))
     (let ((*read-timeout* 1) (start (get-internal-real-time)))
       (is (handler-case (progn (http-get (concatenate 'string url "slow")) nil) (error () t)))
       (is (< (- (get-internal-real-time) start) (* 6 internal-time-units-per-second)))))))

(deftest response-size-cap ()
  (dolist (head (list (crlf "HTTP/1.1 200 OK" "Content-Length: 1000" "")
                      (crlf "HTTP/1.1 200 OK" "Transfer-Encoding: chunked" "" "3e8")))
    (call-with-server
     (lambda (s) (read-request-head s) (respond s head) (respond s (make-string 1000 :initial-element #\x)))
     (lambda (url port)
       (declare (ignore port))
       (let ((*max-response-bytes* 100))
         (signals http-error (http-get (concatenate 'string url "big"))))))))

(deftest https-to-a-plain-server-fails ()
  ;; No silent downgrade and no retry: a server that does not speak TLS is an error.
  (call-with-server
   (lambda (s) (respond s (crlf "HTTP/1.1 400 Bad Request" "Content-Length: 0" "")))
   (lambda (url port)
     (declare (ignore url))
     (let ((*read-timeout* 5))
       (is (handler-case (progn (http-get (format nil "https://127.0.0.1:~D/" port)) nil)
             (error () t)))))))

(deftest header-injection-is-refused ()
  (signals http-error
    (http-get "http://127.0.0.1:1/" :headers (list (cons "X-A" (format nil "a~C~Cb: c" #\Return #\Newline))))))
