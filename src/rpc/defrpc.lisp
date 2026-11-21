(defpackage #:clempo.rpc
  (:use #:cl #:clempo.base #:clempo.serde)
  (:local-nicknames (#:json #:clempo.json))
  (:documentation "The JSON-RPC method table and envelope. DEFRPC declares a method once:
its name, typed positional parameters (each read by a DEFPARAM reader with the reference's
error messages), its documentation and body; the macro generates the parameter parsing
with jsonrpsee's sequence semantics. HANDLE-BODY turns a request body into the HTTP status
and response body exactly as the reference node does (single, batch, notifications,
envelope errors).")
  (:export #:defrpc #:defparam #:rpc-fail #:rpc-error #:rpc-error-code #:rpc-error-message
           #:rpc-error-data #:invalid-params #:internal-error
           #:find-method* #:method-names #:method-doc #:method-params
           #:handle-body #:handle-request-object #:*methods* #:read-param
           #:*response-buffer* #:make-response-buffer
           #:*connection* #:+invalid-params+ #:+internal-error+ #:+method-not-found+
           ;; parameter types (params.lisp)
           #:hash #:address #:bool #:any #:index #:block-number-or-tag #:block-id #:bytes #:u64 #:u256
           #:read-ruint))

(in-package #:clempo.rpc)

(defconstant +parse-error+ -32700)
(defconstant +invalid-request+ -32600)
(defconstant +method-not-found+ -32601)
(defconstant +invalid-params+ -32602)
(defconstant +internal-error+ -32603)

(define-condition rpc-error (error)
  ((code :initarg :code :reader rpc-error-code)
   (message :initarg :message :reader rpc-error-message)
   (data :initarg :data :initform nil :reader rpc-error-data))
  (:report (lambda (c s) (format s "rpc error ~D: ~A~@[ (~A)~]" (rpc-error-code c) (rpc-error-message c)
                                 (rpc-error-data c)))))

(defun rpc-fail (code message &optional data)
  "Answer the current request with an error object. DATA is clempo.json data, or NIL for none."
  (error 'rpc-error :code code :message message :data data))

(defun invalid-params (data) (rpc-fail +invalid-params+ "Invalid params" data))
(defun internal-error (&optional (message "Internal error") data) (rpc-fail +internal-error+ message data))

(defvar *connection* nil
  "The connection serving the current request (a websocket for subscriptions), or NIL.")

;;; Parameter readers

(defvar *param-readers* (make-hash-table :test 'eq))

(defmacro defparam (name (src) doc &body body)
  "Define the reader for parameter type NAME: BODY reads one value from SRC and returns it."
  `(progn
     (setf (gethash ',name *param-readers*)
           (lambda (,src) (declare (ignorable ,src)) ,@body))
     (setf (get ',name 'param-doc) ,doc)
     ',name))

(defun read-param (type src)
  (let ((fn (gethash type *param-readers*)))
    (invariant fn "no reader for parameter type ~S" type)
    (funcall fn src)))

;;; Methods

(defstruct rpc-method name params doc function)

(defvar *methods* (make-hash-table :test 'equal) "name -> rpc-method")

(defun find-method* (name) (gethash name *methods*))
(defun method-names () (sort (loop for k being the hash-keys of *methods* collect k) #'string<))
(defun method-doc (name) (rpc-method-doc (find-method* name)))
(defun method-params (name) (rpc-method-params (find-method* name)))

(defmacro defrpc (name (&rest params) doc &body body)
  "Declare JSON-RPC method NAME. Each param is (var type &key default optional): a missing
or null optional (or defaulted) param takes its default; a missing required one fails with
\"No more params\". Extra positional params are ignored, as in the reference."
  (let ((src (gensym "SRC")))
    `(progn
       (setf (gethash ,name *methods*)
             (make-rpc-method
              :name ,name :doc ,doc
              :params ',(mapcar (lambda (p) (destructuring-bind (var type &rest opts) p
                                              (list* (string-downcase var) type opts)))
                                params)
              :function
              (lambda (,src)
                (declare (ignorable ,src))
                (let* (,@(loop for p in params
                               collect (destructuring-bind (var type &key (default nil defaultp) optional) p
                                         `(,var (next-param ,src ',type
                                                            ,(or defaultp optional)
                                                            (lambda () ,default))))))
                  (declare (ignorable ,@(mapcar #'first params)))
                  ,@body))))
       ,name)))

(defun next-param (seq type optional default-fn)
  "Read the next positional param from the params sequence SEQ (a src positioned inside the
array, or NIL when params were absent)."
  (let* ((start (and seq (src-pos seq)))
         (c (and seq (peek seq))))
    (cond ((or (null seq) (eql c #\]))
           (if optional (funcall default-fn) (invalid-params "No more params")))
          (t
           ;; A param's positions count from just after the preceding [ or , (its leading
           ;; whitespace included).
           (setf (clempo.serde::src-start seq) start)
           (let ((value (if (and optional (eq (peek-kind seq) :null))
                            (progn (read-null-p seq) (funcall default-fn))
                            (handler-case (read-param type seq)
                              (serde-error (e) (invalid-params (serde-error-message e)))))))
             (when (eql (peek seq) #\,) (incf (src-pos seq)))
             value)))))

;;; Envelope

(defvar *response-buffer* nil
  "A thread's reusable response buffer (MAKE-RESPONSE-BUFFER), or NIL to write each response
into a fresh string. While it is bound, a response (and so HANDLE-BODY's answer to a single
request) is this buffer itself, a string with a fill pointer, valid only until the thread
writes its next response: a server binds it per thread and sends each answer at once.")

(defun make-response-buffer ()
  (make-array 4096 :element-type 'character :fill-pointer 0 :adjustable t))

(defun write-response (out id-raw resultp result error)
  (write-string "{\"jsonrpc\":\"2.0\",\"id\":" out)
  (write-string (or id-raw "null") out)
  (if resultp
      (progn (write-string ",\"result\":" out) (json:write-json result out))
      (progn (write-string ",\"error\":" out)
             (json:write-json error out)))
  (write-char #\} out))

(defun response-string (id-raw &key (result nil resultp) error)
  (let ((buffer *response-buffer*))
    (cond ((null buffer)
           (with-output-to-string (out) (write-response out id-raw resultp result error)))
          (t ;; the room a huge answer took is not kept for the small ones after it
             (if (> (array-dimension buffer 0) (* 4 1024 1024))
                 (adjust-array buffer 4096 :fill-pointer 0)
                 (setf (fill-pointer buffer) 0))
             (with-output-to-string (out buffer) (write-response out id-raw resultp result error))
             buffer))))

(defun error-object (code message &optional data)
  (if data
      (json:jobject "code" code "message" message "data" data)
      (json:jobject "code" code "message" message)))

(defun error-response (id-raw code message &optional data)
  (response-string id-raw :error (error-object code message data)))

(defun valid-id-raw (src)
  "The raw text of a valid id (u64 number, string or null), or NIL."
  (case (peek-kind src)
    (:string (read-raw-value src))
    (:null (read-raw-value src))
    (:number (let ((raw (read-raw-value src)))
               (and (every #'digit-char-p raw)
                    (< (parse-integer raw) (ash 1 64))
                    raw)))
    (t (skip-value src) nil)))

(defun invoke-method (m params-src)
  "Run method M; return the response pieces (values result-or-nil error-object-or-nil). A Lisp
error inside a method is a clempo bug or missing data: it answers -32603 with its report."
  (handler-case (values (funcall (rpc-method-function m) params-src) nil)
    (rpc-error (e) (values nil (error-object (rpc-error-code e) (rpc-error-message e) (rpc-error-data e))))
    (error (e) (values nil (error-object +internal-error+ "Internal error" (princ-to-string e))))))

(defun handle-request-object (text start end)
  "Process the request object in TEXT[START,END). Returns the response string, or NIL for a
notification."
  (let ((src (make-src text :start start :end end))
        (fields '()) (duplicate nil))
    ;; jsonrpsee reads the envelope without serde's recursion limit; each param gets it afresh
    (let ((*recursion-limit* nil))
      (do-object (key src)
        (skip-ws src)
        (if (assoc key fields :test #'string=)
            (setf duplicate t)
            (push (list key (src-pos src)) fields))
        (skip-value src)))
    (when duplicate
      (return-from handle-request-object (error-response nil +invalid-request+ "Invalid request")))
    (flet ((field-src (key)
             (let ((f (assoc key fields :test #'string=)))
               (and f (make-src text :start (second f) :end end)))))
      (let* ((id-src (field-src "id"))
             (id-raw (if id-src (valid-id-raw id-src) "null"))
             (version (let ((s (field-src "jsonrpc")))
                        (and s (eq (peek-kind s) :string) (read-string s))))
             (method-src (field-src "method"))
             (method (and method-src (eq (peek-kind method-src) :string) (read-string method-src)))
             (params-src (field-src "params")))
        (cond ((null id-raw) (error-response nil +invalid-request+ "Invalid request"))
              ((or (not (equal version "2.0")) (null method))
               (error-response id-raw +invalid-request+ "Invalid request"))
              (t
               (let ((m (find-method* method))
                     (notification (null id-src)))
                 (multiple-value-bind (result err)
                     (measured
                      ;; only served names become labels: a client cannot grow the registry
                      (if m method "unknown")
                      (lambda ()
                     (cond ((null m) (values nil (error-object +method-not-found+ "Method not found")))
                           ;; a :raw method reads its params itself, untouched by the envelope
                           ((eq (rpc-method-params m) :raw) (invoke-method m params-src))
                           ((null (rpc-method-params m)) (invoke-method m nil))
                           (t (let ((seq (and params-src (not (eq (peek-kind params-src) :null)) params-src)))
                                (cond ((null seq) (invoke-method m nil))
                                      ((eq (peek-kind seq) :array)
                                       (incf (src-pos seq))
                                       (invoke-method m seq))
                                      (t (values nil (error-object
                                                      +invalid-params+ "Invalid params"
                                                      (format nil "Invalid params. Expected one of '[', ']' or ',' but found '~A'"
                                                              (peek seq)))))))))))
                   (cond (notification nil)
                         (err (response-string id-raw :error err))
                         (t (capped-response id-raw (response-string id-raw :result result))))))))))))

(defparameter *requests-metric*
  (clempo.metrics:define-metric "clempo_rpc_requests_total" :counter
                                "JSON-RPC calls answered, by method and outcome (ok or error)"))
(defparameter *latency-metric*
  (clempo.metrics:define-metric "clempo_rpc_request_duration_seconds" :summary
                                "Time to answer a JSON-RPC call, by method"))

(defun measured (method thunk)
  "Call THUNK, which returns (values result error-object), counting the call and its time
under METHOD."
  (let ((start (clempo.io:monotonic-ticks)))
    (multiple-value-bind (result err) (funcall thunk)
      (let ((labels (list (cons "method" method))))
        (clempo.metrics:metric-inc *requests-metric*
                                   :labels (append labels (list (cons "outcome" (if err "error" "ok")))))
        (clempo.metrics:metric-observe *latency-metric*
                                       (/ (- (clempo.io:monotonic-ticks) start)
                                          (float internal-time-units-per-second 1d0))
                                       :labels labels))
      (values result err))))

(defparameter *response-max* (* 160 1024 1024)
  "Largest response, as reth's default --rpc.max-response-size (160 MiB) caps it.")

(defun response-too-big (id-raw)
  (error-response id-raw -32008 "Response is too big"
                  (format nil "Exceeded max limit of ~D" *response-max*)))

(defun capped-response (id-raw response)
  (if (> (length response) *response-max*) (response-too-big id-raw) response))

(defun value-spans (text start end)
  "Start and end positions of each element of the array in TEXT[START,END)."
  (let ((src (make-src text :start start :end end)) (spans '()) (*recursion-limit* nil))
    (incf (src-pos src))
    (unless (eql (peek src) #\])
      (loop (skip-ws src)
            (let ((s (src-pos src))) (skip-value src) (push (cons s (src-pos src)) spans))
            (if (eql (peek src) #\,) (incf (src-pos src)) (return))))
    (nreverse spans)))

(defun handle-body (text)
  "Process a JSON-RPC HTTP body. Returns (values http-status response-string)."
  (let* ((text (coerce text 'simple-string))
         (start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) text)
                    (length text)))
         (first (and (< start (length text)) (char text start))))
    (unless (member first '(#\{ #\[))
      (return-from handle-body (values 400 (error-response nil +parse-error+ "Parse error"))))
    ;; nesting is bounded per typed param (serde's recursion limit), not for the whole body:
    ;; the reference answers a deeply nested param a method ignores
    (let ((parsed (handler-case (json:parse-json text :max-depth nil) (json:json-error () :invalid))))
      (cond ((eq parsed :invalid) (values 200 (error-response nil +parse-error+ "Parse error")))
            ((char= first #\{)
             (values 200 (or (handle-request-object text start (length text)) "null")))
            ((zerop (length parsed)) (values 200 (error-response nil +invalid-request+ "Invalid request")))
            (t
             (let ((responses
                     (loop for (s . e) in (value-spans text start (length text))
                           for r = (if (char= (char text s) #\{)
                                       (handle-request-object text s e)
                                       (error-response nil +invalid-request+ "Invalid request"))
                           ;; each kept apart from the reused response buffer
                           when r collect (coerce r 'simple-string))))
               (values 200 (cond ((null responses) "null")
                                 ((> (reduce #'+ responses :key #'length) *response-max*)
                                  (response-too-big nil))
                                 (t (format nil "[~{~A~^,~}]" responses))))))))))
