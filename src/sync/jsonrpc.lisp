(defpackage #:clempo.jsonrpc
  (:use #:cl #:clempo.base #:clempo.json)
  (:local-nicknames (#:http #:clempo.http))
  (:documentation "JSON-RPC 2.0 client over clempo.http, with batches.")
  (:export #:rpc-call #:rpc-batch #:rpc-error #:rpc-error-code #:rpc-error-data
           #:*rpc-url* #:+mainnet-rpc+ #:+moderato-rpc+ #:local-rpc-url))

(in-package #:clempo.jsonrpc)

(defparameter +mainnet-rpc+ "https://rpc.tempo.xyz")
(defparameter +moderato-rpc+ "https://rpc.moderato.tempo.xyz")

(defun local-rpc-url ()
  "The private mainnet node used for bulk fetches (CLEMPO_RPC), else the public RPC."
  (or (clempo.io:getenv "CLEMPO_RPC") +mainnet-rpc+))

(defvar *rpc-url* nil "Default endpoint; NIL means (local-rpc-url).")

(define-condition rpc-error (error)
  ((code :initarg :code :reader rpc-error-code)
   (message :initarg :message :reader rpc-error-message)
   (data :initarg :data :initform nil :reader rpc-error-data))
  (:report (lambda (c s) (format s "rpc error ~A: ~A" (rpc-error-code c) (rpc-error-message c)))))

(defun post (url payload)
  (multiple-value-bind (status body) (http:http-post url (to-json-octets payload))
    (let ((v (handler-case (parse-json body)
               (json-error ()
                 (error 'rpc-error :code status :message (octets->string body))))))
      (when (and (jobject-p v) (jget v "error") (null (jget v "id" nil)))
        (error 'rpc-error :code (jget* v "error" "code") :message (jget* v "error" "message")))
      (unless (= status 200)
        (error 'rpc-error :code status :message (to-json-string v)))
      v)))

(defun request (id method params)
  (jobject "jsonrpc" "2.0" "id" id "method" method "params" (coerce params 'simple-vector)))

(defun rpc-call (method params &key (url (or *rpc-url* (local-rpc-url))) (retries 5))
  "Call METHOD; returns the result or signals RPC-ERROR. Rate limits are retried with backoff."
  (loop for attempt from 0
        do (handler-case
               (let ((v (post url (request 1 method params))))
                 (let ((err (jget v "error")))
                   (when err
                     (error 'rpc-error :code (jget err "code") :message (jget err "message")
                                       :data (jget err "data"))))
                 (return (jget v "result")))
             (rpc-error (e)
               (if (and (< attempt retries) (member (rpc-error-code e) '(429 -32005 502 503 504)))
                   (sleep (* 0.5 (expt 2 attempt)))
                   (error e))))))

(defun rpc-batch (calls &key (url (or *rpc-url* (local-rpc-url))) (retries 8))
  "CALLS is a list of (method params). Returns a list of results in order; an element is
an RPC-ERROR object when that call failed."
  (loop for attempt from 0
        do (handler-case
               (let* ((v (post url (coerce (loop for (m p) in calls for i from 0 collect (request i m p))
                                           'simple-vector)))
                      (out (make-array (length calls) :initial-element nil)))
                 (unless (vectorp v)
                   (error 'rpc-error :code (jget* v "error" "code") :message (jget* v "error" "message")))
                 (loop for r across v
                       for id = (jget r "id")
                       for err = (jget r "error")
                       do (setf (aref out id)
                                (if err
                                    (make-condition 'rpc-error :code (jget err "code")
                                                               :message (jget err "message")
                                                               :data (jget err "data"))
                                    (jget r "result"))))
                 (return (coerce out 'list)))
             (rpc-error (e)
               (if (and (< attempt retries) (member (rpc-error-code e) '(429 -32005 502 503 504)))
                   (sleep (* 0.5 (expt 2 (min attempt 5))))
                   (error e))))))
