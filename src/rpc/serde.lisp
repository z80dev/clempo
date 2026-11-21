(defpackage #:clempo.serde
  (:use #:cl #:clempo.base)
  (:documentation "Typed reading of one JSON-RPC parameter from its raw text, failing with the
messages the reference node's deserializer (serde_json through jsonrpsee) gives, positions
included: an error is reported \"at line L column C\" where C counts the characters consumed
on the line, with an opening bracket or brace peeked rather than consumed.

A reader is a function of a SRC. Readers compose: an object reader walks keys with
DO-OBJECT and hands each value to a field reader. Values are returned in clempo.json's
representation.")
  (:export #:src #:make-src #:src-text #:src-pos #:serde-error #:serde-error-message
           #:serde-fail #:serde-fail-here #:serde-fail-at
           #:skip-ws #:peek #:peek-kind #:at-end-p #:expect-end
           #:read-string #:read-bool #:read-number #:read-null-p #:skip-value #:read-value
           #:read-raw-value #:invalid-type #:do-object #:do-array #:unexpected-text
           #:rust-debug-string))

(in-package #:clempo.serde)

(define-condition serde-error (error)
  ((message :initarg :message :reader serde-error-message))
  (:report (lambda (c s) (write-string (serde-error-message c) s))))

(defstruct (src (:constructor %make-src (text pos end)))
  (text "" :type simple-string)
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (line-start 0 :type fixnum)
  (start 0 :type fixnum))

(defun make-src (text &key (start 0) (end (length text)))
  (let ((s (%make-src (coerce text 'simple-string) start end)))
    (setf (src-line-start s) start (src-start s) start)
    s))

(defun position-suffix (src &optional (pos (src-pos src)))
  (let* ((text (src-text src))
         (line 1) (line-start (src-start src)))
    (loop for i from (src-start src) below pos
          when (char= (schar text i) #\Newline) do (incf line) (setf line-start (1+ i)))
    (format nil " at line ~D column ~D" line (- pos line-start))))

(defun serde-fail (fmt &rest args)
  "An error without a position (raised after the value was read)."
  (error 'serde-error :message (apply #'format nil fmt args)))

(defun serde-fail-here (src fmt &rest args)
  "An error at the current position."
  (error 'serde-error :message (concatenate 'string (apply #'format nil fmt args) (position-suffix src))))

(defun serde-fail-at (src pos fmt &rest args)
  (error 'serde-error :message (concatenate 'string (apply #'format nil fmt args) (position-suffix src pos))))

(defun skip-ws (src)
  (let ((text (src-text src)))
    (loop while (and (< (src-pos src) (src-end src))
                     (member (schar text (src-pos src)) '(#\Space #\Tab #\Newline #\Return)))
          do (incf (src-pos src)))))

(defun at-end-p (src) (skip-ws src) (>= (src-pos src) (src-end src)))

(defun peek (src)
  (skip-ws src)
  (if (< (src-pos src) (src-end src)) (schar (src-text src) (src-pos src)) nil))

(defun peek-kind (src)
  (let ((c (peek src)))
    (case c
      ((nil) :eof) (#\" :string) (#\{ :object) (#\[ :array) (#\t :true) (#\f :false) (#\n :null)
      (t (if (or (digit-char-p c) (char= c #\-)) :number :invalid)))))

(defun advance (src n) (incf (src-pos src) n))

(defun eof-error (src) (serde-fail-here src "EOF while parsing a value"))

(defun read-string (src)
  "Consume a JSON string (the next value must be one) and return it."
  (unless (eql (peek src) #\") (unreachable "read-string on non-string"))
  (let ((text (src-text src)) (out (make-string-output-stream)))
    (advance src 1)
    (loop
      (when (>= (src-pos src) (src-end src)) (serde-fail-here src "EOF while parsing a string"))
      (let ((c (schar text (src-pos src))))
        (advance src 1)
        (cond ((char= c #\") (return (get-output-stream-string out)))
              ((char= c #\\)
               (let ((e (schar text (src-pos src))))
                 (advance src 1)
                 (case e
                   (#\" (write-char #\" out)) (#\\ (write-char #\\ out)) (#\/ (write-char #\/ out))
                   (#\b (write-char #\Backspace out)) (#\f (write-char #\Page out))
                   (#\n (write-char #\Newline out)) (#\r (write-char #\Return out))
                   (#\t (write-char #\Tab out))
                   (#\u (flet ((hex4 () (prog1 (parse-integer text :start (src-pos src) :end (+ 4 (src-pos src)) :radix 16)
                                          (advance src 4))))
                          (let ((cp (hex4)))
                            (when (and (<= #xD800 cp #xDBFF) (< (+ 1 (src-pos src)) (src-end src))
                                       (char= (schar text (src-pos src)) #\\)
                                       (char= (schar text (1+ (src-pos src))) #\u))
                              (advance src 2)
                              (setf cp (+ #x10000 (ash (- cp #xD800) 10) (- (hex4) #xDC00))))
                            (write-char (code-char cp) out))))
                   (t (serde-fail-here src "invalid escape")))))
              (t (write-char c out)))))))

(defun read-number (src)
  "Consume a JSON number; integers stay integers, anything with . or e is a double."
  (skip-ws src)
  (let* ((text (src-text src)) (start (src-pos src)) (float nil))
    (when (char= (schar text start) #\-) (advance src 1))
    (loop while (< (src-pos src) (src-end src))
          do (let ((c (schar text (src-pos src))))
               (cond ((digit-char-p c))
                     ((find c ".eE+-") (setf float t))
                     (t (return))))
             (advance src 1))
    (if float
        (let ((*read-default-float-format* 'double-float))
          (coerce (let ((*read-eval* nil)) (read-from-string text t nil :start start :end (src-pos src))) 'double-float))
        (parse-integer text :start start :end (src-pos src)))))

(defun read-literal (src word)
  (skip-ws src)
  (advance src (length word)))

(defun read-bool (src expected)
  (case (peek-kind src)
    (:true (read-literal src "true") t)
    (:false (read-literal src "false") nil)
    (t (invalid-type src expected))))

(defun read-null-p (src)
  "Consume a null if one is next."
  (when (eq (peek-kind src) :null) (read-literal src "null") t))

(defun rust-debug-string (s)
  "Rust's {:?} of a str, as serde prints an unexpected string."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across s
          do (case c
               (#\" (write-string "\\\"" out)) (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out)) (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code c) 32)
                      (format out "\\u{~(~x~)}" (char-code c))
                      (write-char c out)))))
    (write-char #\" out)))

(defun rust-float (x)
  (if (and (= x (ffloor x)) (< (abs x) 1d16))
      (format nil "~D.0" (truncate x))
      (let ((*read-default-float-format* 'double-float)) (prin1-to-string x))))

(defun unexpected-text (src)
  "Describe the next value as serde's Unexpected does, consuming scalars but not containers."
  (ecase (peek-kind src)
    (:string (format nil "string ~A" (rust-debug-string (read-string src))))
    (:number (let ((n (read-number src)))
               (if (integerp n) (format nil "integer `~D`" n) (format nil "floating point `~A`" (rust-float n)))))
    (:true (read-literal src "true") "boolean `true`")
    (:false (read-literal src "false") "boolean `false`")
    (:null (read-literal src "null") "null")
    (:object "map")
    (:array "sequence")
    (:eof (eof-error src))
    (:invalid (serde-fail-here src "expected value"))))

(defun invalid-type (src expected)
  "Fail with serde's invalid-type message for the next value against EXPECTED."
  (let ((what (unexpected-text src)))
    (serde-fail-here src "invalid type: ~A, expected ~A" what expected)))

(defun skip-value (src)
  (ecase (peek-kind src)
    (:string (read-string src))
    (:number (read-number src))
    (:true (read-literal src "true"))
    (:false (read-literal src "false"))
    (:null (read-literal src "null"))
    (:object (advance src 1)
     (unless (eql (peek src) #\})
       (loop (read-string src) (peek src) (advance src 1) (skip-value src)
             (if (eql (peek src) #\,) (advance src 1) (return))))
     (peek src) (advance src 1))
    (:array (advance src 1)
     (unless (eql (peek src) #\])
       (loop (skip-value src) (if (eql (peek src) #\,) (advance src 1) (return))))
     (peek src) (advance src 1))
    (:eof (eof-error src))
    (:invalid (serde-fail-here src "expected value"))))

(defun read-raw-value (src)
  "The raw text of the next value."
  (skip-ws src)
  (let ((start (src-pos src)))
    (skip-value src)
    (subseq (src-text src) start (src-pos src))))

(defun read-value (src)
  "The next value as clempo.json data."
  (clempo.json:parse-json (read-raw-value src)))

(defmacro do-object ((key src &key (expected "a map")) &body body)
  "Walk the object at SRC: KEY is bound to each key (consumed, the value not yet); BODY must
consume the value. A non-object fails with invalid type against EXPECTED."
  (let ((s (gensym "SRC")))
    `(let ((,s ,src))
       (unless (eq (peek-kind ,s) :object) (invalid-type ,s ,expected))
       (advance-1 ,s)
       (unless (eql (peek ,s) #\})
         (loop
           (let ((,key (read-string ,s)))
             (peek ,s) (advance-1 ,s)          ; the colon
             ,@body)
           (if (eql (peek ,s) #\,) (advance-1 ,s) (return))))
       (peek ,s) (advance-1 ,s))))

(defmacro do-array ((index src &key (expected "a sequence")) &body body)
  "Walk the array at SRC; BODY consumes each element, INDEX counts them."
  (let ((s (gensym "SRC")))
    `(let ((,s ,src) (,index 0))
       (declare (ignorable ,index))
       (unless (eq (peek-kind ,s) :array) (invalid-type ,s ,expected))
       (advance-1 ,s)
       (unless (eql (peek ,s) #\])
         (loop ,@body (incf ,index)
               (if (eql (peek ,s) #\,) (advance-1 ,s) (return))))
       (peek ,s) (advance-1 ,s))))

(defun advance-1 (src) (advance src 1))

(defun expect-end (src)
  (unless (at-end-p src) (serde-fail-here src "trailing characters")))
