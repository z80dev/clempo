(defpackage #:clempo.rpc
  (:use #:cl #:clempo.base #:clempo.serde)
  (:local-nicknames (#:json #:clempo.json) (#:metrics #:clempo.metrics))
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

(defstruct rpc-method name params doc function
  (cells nil))   ; its metric cells (ok, error, latency), each made on first use

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

;;; Metrics: each method counts into its own cells

(defparameter *requests-metric*
  (metrics:define-metric "clempo_rpc_requests_total" :counter
                         "JSON-RPC calls answered, by method and outcome (ok or error)"))
(defparameter *latency-metric*
  (metrics:define-metric "clempo_rpc_request_duration_seconds" :summary
                         "Time to answer a JSON-RPC call, by method"))

(defvar *unknown-method* (make-rpc-method :name "unknown")
  "Where calls of a method not served are counted.")

(defun method-cell (m index)
  "M's metric cell: 0 calls answered ok, 1 calls answered with an error, 2 their time."
  (let ((cells (or (rpc-method-cells m)
                   (setf (rpc-method-cells m) (make-array 3 :initial-element nil)))))
    (or (svref cells index)
        (setf (svref cells index)
              (let ((labels (list (cons "method" (rpc-method-name m)))))
                (case index
                  (0 (metrics:metric-cell *requests-metric* (append labels (list (cons "outcome" "ok")))))
                  (1 (metrics:metric-cell *requests-metric* (append labels (list (cons "outcome" "error")))))
                  (t (metrics:metric-cell *latency-metric* labels))))))))

;;; Envelope

(defvar *response-buffer* nil
  "A thread's reusable response buffer (MAKE-RESPONSE-BUFFER), or NIL to write each response
into a fresh string. While it is bound, a response (and so HANDLE-BODY's answer to a single
request) is this buffer itself, a string with a fill pointer, valid only until the thread
writes its next response: a server binds it per thread and sends each answer at once.")

(defun make-response-buffer ()
  (make-array 4096 :element-type 'character :fill-pointer 0 :adjustable t))

(defun put-text (buffer s &optional (start 0) (end (length s)))
  "Append S[START,END) (S a simple string) to BUFFER, a response buffer."
  (declare (type simple-string s) (type fixnum start end))
  (invariant (<= 0 start end (length s)) "put-text ~D..~D of ~D" start end (length s))
  (let* ((fill (fill-pointer buffer))
         (need (+ fill (- end start))))
    (declare (type fixnum fill need))
    (when (> need (array-dimension buffer 0))
      (adjust-array buffer (max need (* 2 (array-dimension buffer 0)))))
    (let ((v (sb-ext:array-storage-vector buffer)))
      (declare (type (simple-array character (*)) v))
      (invariant (<= need (length v)) "response buffer short")
      (macrolet ((copy (type)
                   `(let ((s s))
                      (declare #.*hot* (type ,type s))
                      (loop for i of-type fixnum from start below end
                            for j of-type fixnum from fill
                            do (setf (schar v j) (schar s i))))))
        (etypecase s
          (simple-base-string (copy simple-base-string))
          ((simple-array character (*)) (copy (simple-array character (*)))))))
    (setf (fill-pointer buffer) need)))

(defun plain-string-p (s)
  "Whether the simple string S is written in JSON as itself between quotes: no character in
it needs an escape."
  (declare (type simple-string s))
  (macrolet ((scan (type)
               `(let ((s s))
                  (declare (type ,type s))
                  (loop for c across s
                        never (or (char= c #\") (char= c #\\) (< (char-code c) 32))))))
    (etypecase s
      (simple-base-string (scan simple-base-string))
      ((simple-array character (*)) (scan (simple-array character (*)))))))

(defun put-json (buffer value)
  "Append VALUE as JSON to BUFFER: a plain string directly, anything else by WRITE-JSON."
  (if (and (simple-string-p value) (plain-string-p value))
      (progn (put-text buffer "\"") (put-text buffer value) (put-text buffer "\""))
      ;; output to a string with a fill pointer goes after its fill pointer
      (with-output-to-string (out buffer) (json:write-json value out))))

(defun write-response (buffer id-text id-start id-end resultp result error)
  "A response with the raw id ID-TEXT[ID-START,ID-END) into BUFFER."
  (put-text buffer "{\"jsonrpc\":\"2.0\",\"id\":")
  (put-text buffer id-text id-start id-end)
  (if resultp
      (progn (put-text buffer ",\"result\":") (put-json buffer result))
      (progn (put-text buffer ",\"error\":") (put-json buffer error)))
  (put-text buffer "}"))

(defun response-string (id-text id-start id-end resultp result error)
  "The response to the request whose raw id is ID-TEXT[ID-START,ID-END): RESULT when
RESULTP, else the error object ERROR."
  (let ((buffer *response-buffer*))
    (cond ((null buffer)
           (let ((buffer (make-response-buffer)))
             (write-response buffer id-text id-start id-end resultp result error)
             (coerce buffer 'simple-string)))
          (t ;; the room a huge answer took is not kept for the small ones after it
             (if (> (array-dimension buffer 0) (* 4 1024 1024))
                 (adjust-array buffer 4096 :fill-pointer 0)
                 (setf (fill-pointer buffer) 0))
             (write-response buffer id-text id-start id-end resultp result error)
             buffer))))

(defun error-object (code message &optional data)
  (if data
      (json:jobject "code" code "message" message "data" data)
      (json:jobject "code" code "message" message)))

(defun error-response (id-text id-start id-end code message &optional data)
  (response-string id-text id-start id-end nil nil (error-object code message data)))

(defun null-id-error (code message)
  "An error answered with a null id."
  (error-response "null" 0 4 code message))

(defun json-string-end (text pos end)
  "The position after the JSON string that starts at TEXT[POS], in a valid JSON text."
  (declare (type simple-string text) (type fixnum pos end))
  (let ((i (1+ pos)))
    (declare (type fixnum i))
    (loop while (< i end)
          do (case (schar text i)
               (#\" (return-from json-string-end (1+ i)))
               (#\\ (incf i 2))
               (t (incf i))))
    end))

(defun valid-id (text pos end)
  "The raw text of the valid id (u64 number, string or null) at TEXT[POS] in a valid JSON
text, as (values text start end), or NIL."
  (declare (type simple-string text) (type fixnum pos end))
  (case (schar text pos)
    (#\" (values text pos (json-string-end text pos end)))
    (#\n (values text pos (+ pos 4)))
    (t (let ((stop (or (position-if-not (lambda (c) (or (char<= #\0 c #\9) (find c ".eE+-")))
                                        text :start pos :end end)
                       end)))
         ;; digits only (a JSON number has no leading zero), below 2^64
         (and (< pos stop)
              (loop for i from pos below stop always (char<= #\0 (schar text i) #\9))
              (or (< (- stop pos) 20)
                  (and (= (- stop pos) 20) (< (parse-integer text :start pos :end stop) (ash 1 64))))
              (values text pos stop))))))

(defun string-value (text pos end)
  "The string value at TEXT[POS] (in a valid JSON text) as (values text start end) of its
characters when it holds no escape, else as a fresh decoded string; NIL for another value."
  (declare (type simple-string text) (type fixnum pos end))
  (when (char= (schar text pos) #\")
    (let ((stop (clempo.serde::quote-or-escape text (1+ pos) end)))
      (if (and stop (char= (schar text stop) #\"))
          (values text (1+ pos) stop)
          (let ((s (read-string (make-src text :start pos :end end))))
            (values s 0 (length s)))))))

(defun lookup-method (text start end)
  "The method named TEXT[START,END), or NIL."
  (declare (type simple-string text) (type fixnum start end))
  (let ((n (- end start)))
    (if (and (<= n 64) (typep text 'simple-base-string))
        (let ((name (make-string n :element-type 'base-char)))
          (declare (dynamic-extent name))
          (loop for i of-type fixnum from start below end
                for j of-type fixnum from 0
                do (setf (schar name j) (schar text i)))
          (find-method* name))
        (find-method* (subseq text start end)))))

(defun scan-fields (text start end)
  "The envelope fields of the request object in TEXT[START,END), a valid JSON text, read key
by key: (values id version method params duplicate), the position of each field's value
\(NIL when absent) and whether a key repeats."
  (let ((src (make-src text :start start :end end)) (keys '())
        (id nil) (version nil) (method nil) (params nil) (duplicate nil))
    ;; jsonrpsee reads the envelope without serde's recursion limit; each param gets it afresh
    (let ((*recursion-limit* nil))
      (do-object (key src)
        (skip-ws src)
        (if (member key keys :test #'string=)
            (setf duplicate t)
            (let ((pos (src-pos src)))
              (push key keys)
              (cond ((string= key "id") (setf id pos))
                    ((string= key "jsonrpc") (setf version pos))
                    ((string= key "method") (setf method pos))
                    ((string= key "params") (setf params pos)))))
        (skip-value src)))
    (values id version method params duplicate)))

(defun plain-keys-p (text members count)
  "Whether no key in JSON-VALID-P's member layout holds an escape."
  (declare (type simple-string text) (type (simple-array fixnum (*)) members) (type fixnum count))
  (macrolet ((scan (type)
               `(let ((text text))
                  (declare (type ,type text))
                  (loop for k of-type fixnum below (* 3 count) by 3
                        never (loop for i of-type fixnum from (aref members k) below (aref members (1+ k))
                                    thereis (char= (schar text i) #\\))))))
    (etypecase text
      (simple-base-string (scan simple-base-string))
      ((simple-array character (*)) (scan (simple-array character (*)))))))

(defun spans= (text a1 b1 a2 b2)
  "Whether TEXT[A1,B1) and TEXT[A2,B2) hold the same characters."
  (declare (type simple-string text) (type fixnum a1 b1 a2 b2))
  (and (= (- b1 a1) (- b2 a2))
       (macrolet ((compare (type)
                    `(let ((text text))
                       (declare (type ,type text))
                       (loop for i of-type fixnum from a1 below b1
                             for j of-type fixnum from a2
                             always (char= (schar text i) (schar text j))))))
         (etypecase text
           (simple-base-string (compare simple-base-string))
           ((simple-array character (*)) (compare (simple-array character (*))))))))

(defun span-is (text start end name)
  "Whether TEXT[START,END) holds the characters of NAME."
  (declare (type simple-string text) (type (simple-array character (*)) name) (type fixnum start end))
  (and (= (- end start) (length name))
       (macrolet ((compare (type)
                    `(let ((text text))
                       (declare (type ,type text))
                       (loop for i of-type fixnum from start below end
                             for j of-type fixnum from 0
                             always (char= (schar text i) (schar name j))))))
         (etypecase text
           (simple-base-string (compare simple-base-string))
           ((simple-array character (*)) (compare (simple-array character (*))))))))

(defun member-fields (text members count)
  "SCAN-FIELDS from JSON-VALID-P's member layout of a request object whose keys hold no
escape: a key is its raw characters."
  (declare (type simple-string text) (type (simple-array fixnum (*)) members) (type fixnum count))
  (let ((id nil) (version nil) (method nil) (params nil) (duplicate nil))
    (loop for k of-type fixnum below (* 3 count) by 3
          do (let ((ks (1+ (aref members k))) (ke (1- (aref members (1+ k))))
                   (pos (aref members (+ k 2))))
               (declare (type fixnum ks ke))
               (cond ((loop for j of-type fixnum below k by 3
                            thereis (spans= text (1+ (aref members j)) (1- (aref members (1+ j))) ks ke))
                      (setf duplicate t))
                     ((span-is text ks ke "id") (setf id pos))
                     ((span-is text ks ke "jsonrpc") (setf version pos))
                     ((span-is text ks ke "method") (setf method pos))
                     ((span-is text ks ke "params") (setf params pos)))))
    (values id version method params duplicate)))

(defun invoke-method (m params-src)
  "Run method M; return the response pieces (values result-or-nil error-object-or-nil). A Lisp
error inside a method is a clempo bug or missing data: it answers -32603 with its report."
  (handler-case (values (funcall (rpc-method-function m) params-src) nil)
    (rpc-error (e) (values nil (error-object (rpc-error-code e) (rpc-error-message e) (rpc-error-data e))))
    (error (e) (values nil (error-object +internal-error+ "Internal error" (princ-to-string e))))))

(defun run-method (m text params end)
  "Call M (NIL: an unknown method) with the params value at TEXT[PARAMS] (NIL: absent):
\(values result error-object)."
  (macrolet ((with-params-src ((var) &body body)
               `(if params
                    (let ((,var (make-src text :start params :end end)))
                      (declare (dynamic-extent ,var))
                      ,@body)
                    (let ((,var nil)) ,@body))))
    (cond ((null m) (values nil (error-object +method-not-found+ "Method not found")))
          ;; a :raw method reads its params itself, untouched by the envelope
          ((eq (rpc-method-params m) :raw) (with-params-src (src) (invoke-method m src)))
          ((null (rpc-method-params m)) (invoke-method m nil))
          (t (with-params-src (src)
               (let ((seq (and src (not (eq (peek-kind src) :null)) src)))
                 (cond ((null seq) (invoke-method m nil))
                       ((eq (peek-kind seq) :array)
                        (incf (src-pos seq))
                        (invoke-method m seq))
                       (t (values nil (error-object
                                       +invalid-params+ "Invalid params"
                                       (format nil "Invalid params. Expected one of '[', ']' or ',' but found '~A'"
                                               (peek seq))))))))))))

(defun handle-request-object (text start end &optional members count)
  "Process the request object in TEXT[START,END), a valid JSON text. Returns the response
string, or NIL for a notification. MEMBERS and COUNT, when given, are JSON-VALID-P's member
layout of the object."
  (let ((text (coerce text 'simple-string)))
    (multiple-value-bind (id-pos version-pos method-pos params-pos duplicate)
        (if (and count (plain-keys-p text members count))
            (member-fields text members count)
            (scan-fields text start end))
      (when duplicate
        (return-from handle-request-object (null-id-error +invalid-request+ "Invalid request")))
      (multiple-value-bind (id-text id-start id-end)
          (if id-pos (valid-id text id-pos end) (values "null" 0 4))
        (unless id-text
          (return-from handle-request-object (null-id-error +invalid-request+ "Invalid request")))
        (multiple-value-bind (v vs ve) (and version-pos (string-value text version-pos end))
          (multiple-value-bind (name ns ne) (and method-pos (string-value text method-pos end))
            (if (or (null v) (not (span-is v vs ve "2.0")) (null name))
                (error-response id-text id-start id-end +invalid-request+ "Invalid request")
                (let* ((m (lookup-method name ns ne))
                       ;; only served names become labels: a client cannot grow the registry
                       (measured (or m *unknown-method*))
                       (t0 (clempo.io:monotonic-ticks)))
                  (multiple-value-bind (result err) (run-method m text params-pos end)
                    (metrics:cell-inc (method-cell measured (if err 1 0)))
                    (metrics:cell-observe-ticks (method-cell measured 2)
                                                (max 0 (- (clempo.io:monotonic-ticks) t0)))
                    (cond ((null id-pos) nil)  ; a notification
                          (err (response-string id-text id-start id-end nil nil err))
                          (t (capped-response id-text id-start id-end
                                              (response-string id-text id-start id-end t result nil)))))))))))))

(defparameter *response-max* (* 160 1024 1024)
  "Largest response, as reth's default --rpc.max-response-size (160 MiB) caps it.")

(defun response-too-big (id-text id-start id-end)
  (error-response id-text id-start id-end -32008 "Response is too big"
                  (format nil "Exceeded max limit of ~D" *response-max*)))

(defun capped-response (id-text id-start id-end response)
  (if (> (length response) *response-max*) (response-too-big id-text id-start id-end) response))

(defconstant +members-max+ 16 "Request object members whose layout validation records.")

(defconstant +batch-max+ (floor (* 15 1024 1024) (1+ (length "{\"jsonrpc\":\"2.0\",\"method\":\"\"}")))
  "Most entries in a batch: jsonrpsee's default (BatchRequestConfig::Unlimited) bound, the
15 MiB request cap over the shortest request and its comma, so no batch of valid requests
is refused. Entry MAX + 1 refuses the batch, however the rest of it reads.")

(defun handle-body (text &optional (end (length text)))
  "Process the JSON-RPC HTTP body TEXT[0,END). Returns (values http-status response-string)."
  (let* ((text (coerce text 'simple-string))
         (start (or (position-if-not (lambda (c) (or (char= c #\Space) (char= c #\Tab)
                                                     (char= c #\Newline) (char= c #\Return)))
                                     text :end end)
                    end))
         (first (and (< start end) (char text start))))
    (case first
      ;; nesting is bounded per typed param (serde's recursion limit), not for the whole body:
      ;; the reference answers a deeply nested param a method ignores, and a deeply nested
      ;; batch entry as an invalid request (serde_json skips a raw value with one byte a level).
      ;; A body is only checked (the same grammar as PARSE-JSON, no values built, one bit a
      ;; level), recording where a single object's members or a batch's entries are.
      (#\{
       (let ((members (make-array (* 3 +members-max+) :element-type 'fixnum)))
         (declare (dynamic-extent members))
         (multiple-value-bind (valid count) (json:json-valid-p text :end end :members members)
           (if valid
               (values 200 (or (handle-request-object text start end members count) "null"))
               (values 200 (null-id-error +parse-error+ "Parse error"))))))
      (#\[
       (let ((spans (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0)))
         (multiple-value-bind (valid why)
             (json:json-valid-p text :end end :elements spans :max-elements +batch-max+)
           (cond ((eq why :too-many)
                  (values 200 (error-response "null" 0 4 -32010 "The batch request was too large"
                                              (format nil "Exceeded max limit of ~D" +batch-max+))))
                 ((not valid) (values 200 (null-id-error +parse-error+ "Parse error")))
                 ((zerop (fill-pointer spans)) (values 200 (null-id-error +invalid-request+ "Invalid request")))
                 (t
                  (let ((responses
                          (loop for i from 0 below (fill-pointer spans) by 2
                                for s = (aref spans i) and e = (aref spans (1+ i))
                                for r = (if (char= (char text s) #\{)
                                            (handle-request-object text s e)
                                            (null-id-error +invalid-request+ "Invalid request"))
                                ;; each kept apart from the reused response buffer
                                when r collect (coerce r 'simple-string))))
                    (values 200 (cond ((null responses) "null")
                                      ((> (reduce #'+ responses :key #'length) *response-max*)
                                       (response-too-big "null" 0 4))
                                      (t (format nil "[~{~A~^,~}]" responses))))))))))
      (t (values 400 (null-id-error +parse-error+ "Parse error"))))))
