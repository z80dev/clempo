(defpackage #:clempo.sync.fake-upstream
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:socket #:sb-bsd-sockets) (#:json #:clempo.json))
  (:export #:call-with-fake-upstream #:rpc-failure)
  (:documentation "A loopback JSON-RPC server for tests: a handler (method params) returns a
result, or signals RPC-FAILURE for a JSON-RPC error or (ERROR) for an HTTP 502."))

(in-package #:clempo.sync.fake-upstream)

(define-condition rpc-failure (error)
  ((code :initarg :code :reader rpc-failure-code)
   (message :initarg :message :initform "fake failure" :reader rpc-failure-message)
   (data :initarg :data :initform nil :reader rpc-failure-data)))

(defun read-line-crlf (stream)
  (let ((line (read-line stream nil "")))
    (string-right-trim '(#\Return) line)))

(defun answer (handler request)
  (let ((id (json:jget request "id" 0)))
    (handler-case
        (json:jobject "jsonrpc" "2.0" "id" id
                      "result" (funcall handler (json:jget request "method")
                                        (coerce (json:jget request "params" #()) 'list)))
      (rpc-failure (c)
        (json:jobject "jsonrpc" "2.0" "id" id
                      "error" (if (rpc-failure-data c)
                                  (json:jobject "code" (rpc-failure-code c) "message" (rpc-failure-message c)
                                                "data" (rpc-failure-data c))
                                  (json:jobject "code" (rpc-failure-code c)
                                                "message" (rpc-failure-message c))))))))

(defun respond (stream status body)
  (let ((octets (string->octets body)))
    (format stream "HTTP/1.1 ~D X~C~CContent-Type: application/json~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
            status #\Return #\Newline #\Return #\Newline (length octets) #\Return #\Newline
            #\Return #\Newline #\Return #\Newline)
    (write-string body stream)
    (finish-output stream)))

(defun serve (handler client)
  (with-open-stream (stream (socket:socket-make-stream client :input t :output t
                                                      :external-format :latin-1 :timeout 5))
    (read-line-crlf stream)
    (let ((length 0))
      (loop for line = (read-line-crlf stream)
            until (string= line "")
            do (when (and (> (length line) 15) (string-equal "content-length:" line :end2 15))
                 (setf length (parse-integer line :start 15))))
      (let ((body (make-string length)))
        (read-sequence body stream)
        (handler-case
            (let* ((request (json:parse-json body))
                   (reply (if (vectorp request)
                              (map 'simple-vector (lambda (r) (answer handler r)) request)
                              (answer handler request))))
              (respond stream 200 (json:to-json-string reply)))
          (error () (respond stream 502 "{}")))))))

(defun call-with-fake-upstream (handler function)
  "Call FUNCTION with the URL of a JSON-RPC server answering with HANDLER."
  (let ((listener (make-instance 'socket:inet-socket :type :stream :protocol :tcp))
        (stop nil))
    (setf (socket:sockopt-reuse-address listener) t)
    (socket:socket-bind listener #(127 0 0 1) 0)
    (socket:socket-listen listener 16)
    (let ((thread (sb-thread:make-thread
                   (lambda ()
                     (loop until stop
                           when (sb-sys:wait-until-fd-usable (socket:socket-file-descriptor listener)
                                                             :input 0.05)
                             do (let ((client (ignore-errors (socket:socket-accept listener))))
                                  (when client
                                    (unwind-protect (ignore-errors (serve handler client))
                                      (ignore-errors (socket:socket-close client)))))))
                   :name "fake upstream")))
      (unwind-protect
           (funcall function (format nil "http://127.0.0.1:~D/" (nth-value 1 (socket:socket-name listener))))
        (setf stop t)
        (sb-thread:join-thread thread :default nil)
        (socket:socket-close listener)))))
