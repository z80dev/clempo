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
           #:read-enum
           #:read-raw-value #:invalid-type #:do-object #:do-array #:unexpected-text
           #:*key-end* #:serde-fail-after-key
           #:rust-debug-string #:rust-float #:number-unexpected #:parse-serde-number
           #:read-integer #:read-content #:*depth* #:*recursion-limit* #:refuse-map))

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


(defvar *key-end* 0 "Inside DO-OBJECT: the position just after the current key.")

(defun serde-fail-after-key (src fmt &rest args)
  "An error positioned just after the current object key (unknown field, duplicate field)."
  (apply #'serde-fail-at src *key-end* fmt args))
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

(defun number-out-of-range () (serde-fail "number out of range"))

(defun f64-from-parts (positive significand exponent)
  "serde_json's f64_from_parts: the significand as an f64 scaled once by a power of ten."
  (let ((f (coerce significand 'double-float)))
    (loop
      (cond ((<= (abs exponent) 308)
             (let ((pow (coerce (expt 10 (abs exponent)) 'double-float)))
               (setf f (if (>= exponent 0)
                           (handler-case (* f pow) (floating-point-overflow () (number-out-of-range)))
                           (/ f pow))))
             (return))
            ((zerop f) (return))
            ((>= exponent 0) (number-out-of-range))
            (t (setf f (/ f 1d308)) (incf exponent 308))))
    (if positive f (- f))))

(defun parse-serde-number (text &key (start 0) (end (length text)))
  "The JSON number TEXT[START,END) as serde_json parses it (without float_roundtrip): a u64
or an i64 when it is an integer that fits, otherwise an f64 built from the digits that fit
a u64 and one power of ten, so it may be an ulp from the nearest double. \"-0\" is -0.0."
  (let ((pos start) (positive t) (significand 0) (exponent 0) (float nil))
    (labels ((digit () (and (< pos end) (digit-char-p (char text pos))))
             (overflowp (d) (let ((q (floor +u64-max+ 10)))
                              (or (> significand q) (and (= significand q) (> d (mod +u64-max+ 10)))))))
      (when (and (< pos end) (char= (char text pos) #\-)) (setf positive nil) (incf pos))
      (loop for d = (digit) while d
            do (if (or float (overflowp d))
                   (setf float t exponent (1+ exponent))
                   (setf significand (+ (* significand 10) d)))
               (incf pos))
      (when (and (< pos end) (char= (char text pos) #\.))
        (setf float t) (incf pos)
        (let ((overflowed nil))
          (loop for d = (digit) while d
                do (unless (or overflowed (setf overflowed (overflowp d)))
                     (setf significand (+ (* significand 10) d))
                     (decf exponent))
                   (incf pos))))
      (when (and (< pos end) (char-equal (char text pos) #\e))
        (setf float t) (incf pos)
        (let ((positive-exp t) (e 0))
          (case (and (< pos end) (char text pos))
            (#\+ (incf pos))
            (#\- (setf positive-exp nil) (incf pos)))
          (loop for d = (digit) while d do (setf e (+ (* e 10) d)) (incf pos))
          (when (> e (1- (ash 1 31)))
            (return-from parse-serde-number
              (if (and positive-exp (plusp significand)) (number-out-of-range) (if positive 0d0 -0d0))))
          (setf exponent (max (- (ash 1 31)) (min (1- (ash 1 31)) (if positive-exp (+ exponent e) (- exponent e)))))))
      (cond (float (f64-from-parts positive significand exponent))
            (positive significand)
            ((zerop significand) -0d0)
            ((<= significand (ash 1 63)) (- significand))
            (t (- (coerce significand 'double-float)))))))

(defun skip-number (src)
  "Consume a JSON number without converting it (serde_json's ignore_number); returns its start."
  (skip-ws src)
  (let ((text (src-text src)) (start (src-pos src)))
    (when (char= (schar text start) #\-) (advance src 1))
    (loop while (and (< (src-pos src) (src-end src))
                     (let ((c (schar text (src-pos src)))) (or (digit-char-p c) (find c ".eE+-"))))
          do (advance src 1))
    start))

(defun read-number (src)
  "Consume a JSON number as serde_json reads it: an integer in u64 or i64 range, otherwise a
double (see PARSE-SERDE-NUMBER); one beyond the double range fails \"number out of range\"."
  (let ((start (skip-number src)))
    (handler-case (parse-serde-number (src-text src) :start start :end (src-pos src))
      (serde-error (e) (serde-fail-here src "~A" (serde-error-message e))))))

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
  "An f64 as serde prints an unexpected float: the shortest round-trip digits, in exponent
form (1e-6, 1.5e+20) below 1e-5 and from 1e16, else positional with at least one decimal."
  (if (zerop x)
      (if (minusp (float-sign x)) "-0.0" "0.0")
      (multiple-value-bind (k digits) (sb-impl::flonum-to-digits (abs x))
        (let ((e (1- k)) (n (length digits)))
          (concatenate
           'string (if (minusp x) "-" "")
           (cond ((or (< e -5) (>= e 16))
                  (format nil "~A~:[.~A~;~*~]e~:[~;+~]~D"
                          (char digits 0) (= n 1) (subseq digits 1) (>= e 0) e))
                 ((<= k 0) (format nil "0.~v,,,'0A~A" (- k) "" digits))
                 ((>= k n) (format nil "~A~v,,,'0A.0" digits (- k n) ""))
                 (t (format nil "~A.~A" (subseq digits 0 k) (subseq digits k)))))))))

(defun number-unexpected (n)
  "serde's Unexpected for a number READ-NUMBER returned: integer `N` or floating point `F`."
  (if (integerp n) (format nil "integer `~D`" n) (format nil "floating point `~A`" (rust-float n))))

(defun unexpected-text (src)
  "Describe the next value as serde's Unexpected does, consuming scalars but not containers."
  (ecase (peek-kind src)
    (:string (format nil "string ~A" (rust-debug-string (read-string src))))
    (:number (number-unexpected (read-number src)))
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

(defun refuse-map (src expected)
  "Fail as serde_json's deserialize_any does when its visitor refuses the map at SRC: the
brace is consumed, and the closing one too when the map is empty (serde_json ends the map
before it reports the error, recorded: `{}` fails at column 2, `{\"a\":1}` at column 1)."
  (advance src 1)
  (when (eql (peek src) #\}) (advance src 1))
  (serde-fail-here src "invalid type: map, expected ~A" expected))

(defun read-integer (src min max expected)
  "A Rust primitive integer (u8, i32, u64, usize ...) from a JSON number, as serde's
primitive visitors take it: an integer out of [MIN, MAX] is an invalid value, a float an
invalid type, anything else an invalid type against EXPECTED."
  (if (eq (peek-kind src) :number)
      (let ((n (read-number src)))
        (cond ((not (integerp n)) (serde-fail-here src "invalid type: ~A, expected ~A" (number-unexpected n) expected))
              ((<= min n max) n)
              (t (serde-fail-here src "invalid value: integer `~D`, expected ~A" n expected))))
      (invalid-type src expected)))

(defconstant +recursion-limit+ 128
  "serde_json's recursion limit: nested arrays and objects one param may open, those it
skips included (recorded from the reference: 127 nested objects in an eth_call request
answer, 128 fail with \"recursion limit exceeded\").")

(defvar *depth* 0 "Arrays and objects open around the value being read, within one param.")

(defvar *recursion-limit* +recursion-limit+
  "The nesting at which reading fails, or NIL while scanning the request envelope, which
jsonrpsee does without serde's limit.")

(defun enter-container (src)
  "The depth inside a container whose opening bracket SRC just consumed; fails past the limit."
  (let ((depth (1+ *depth*)))
    (when (and *recursion-limit* (>= depth *recursion-limit*))
      (serde-fail-here src "recursion limit exceeded"))
    depth))

(defun skip-value (src &optional numbers)
  "Consume the next value. With NUMBERS, numbers are converted as serde_json does when it
buffers a value (a Value or a flattened struct's Content), so one beyond the double range
fails where it ends; without, they are only scanned (serde's IgnoredAny). Nesting counts
toward +RECURSION-LIMIT+ from *DEPTH*. Iterative: no input exhausts the control stack."
  (prog ((stack '()) (*depth* *depth*))   ; closers (#\] or #\}) of the open containers
   value
     (ecase (peek-kind src)
       (:string (read-string src))
       (:number (if numbers (read-number src) (skip-number src)))
       (:true (read-literal src "true"))
       (:false (read-literal src "false"))
       (:null (read-literal src "null"))
       ((:object :array)
        (let ((closer (if (eq (peek-kind src) :object) #\} #\])))
          (advance src 1)
          (setf *depth* (enter-container src))
          (push closer stack)
          (unless (eql (peek src) closer)
            (when (eql closer #\}) (read-string src) (peek src) (advance src 1))
            (go value))))
       (:eof (eof-error src))
       (:invalid (serde-fail-here src "expected value")))
   after                                ; a value ended: next element, or close
     (when (null stack) (return))
     (cond ((eql (peek src) #\,)
            (advance src 1)
            (when (eql (first stack) #\}) (read-string src) (peek src) (advance src 1))
            (go value))
           (t (peek src) (advance src 1) (pop stack) (decf *depth*) (go after)))))

(defun read-raw-value (src &optional numbers)
  "The raw text of the next value (NUMBERS as for SKIP-VALUE)."
  (skip-ws src)
  (let ((start (src-pos src)))
    (skip-value src numbers)
    (subseq (src-text src) start (src-pos src))))

(defun read-content (src)
  "The raw text of the next value, buffered as serde's Content is for a flattened struct."
  (read-raw-value src t))

(defun read-value (src)
  "The next value as clempo.json data; a number beyond the double range fails as it does
when serde_json reads a Value."
  (clempo.json:parse-json (read-raw-value src t) :max-depth nil))   ; READ-RAW-VALUE bounds it

(defmacro do-object ((key src &key (expected "a map")) &body body)
  "Walk the object at SRC: KEY is bound to each key (consumed, the value not yet); BODY must
consume the value. A non-object fails with invalid type against EXPECTED."
  (let ((s (gensym "SRC")))
    `(let ((,s ,src))
       (unless (eq (peek-kind ,s) :object) (invalid-type ,s ,expected))
       (advance-1 ,s)
       (let ((*depth* (enter-container ,s)))
         (unless (eql (peek ,s) #\})
           (loop
             (let* ((,key (read-string ,s))
                    (*key-end* (src-pos ,s)))
               (peek ,s) (advance-1 ,s)          ; the colon
               ,@body)
             (if (eql (peek ,s) #\,) (advance-1 ,s) (return)))))
       (peek ,s) (advance-1 ,s))))

(defmacro do-array ((index src &key (expected "a sequence")) &body body)
  "Walk the array at SRC; BODY consumes each element, INDEX counts them."
  (let ((s (gensym "SRC")))
    `(let ((,s ,src) (,index 0))
       (declare (ignorable ,index))
       (unless (eq (peek-kind ,s) :array) (invalid-type ,s ,expected))
       (advance-1 ,s)
       (let ((*depth* (enter-container ,s)))
         (unless (eql (peek ,s) #\])
           (loop ,@body (incf ,index)
                 (if (eql (peek ,s) #\,) (advance-1 ,s) (return)))))
       (peek ,s) (advance-1 ,s))))

(defun advance-1 (src) (advance src 1))

(defun expect-end (src)
  (unless (at-end-p src) (serde-fail-here src "trailing characters")))

(defun read-enum (src variants &key newtype)
  "serde_json's deserialize_enum for an externally tagged enum read directly (not from a
buffered Content): a string names a unit variant, a single-key map a variant with its value.
VARIANTS are the variant names; NEWTYPE is an alist (name . reader) of the newtype variants,
the others are unit variants whose map value must be null. Returns (values name value)."
  (labels ((expected ()
             (if (= (length variants) 2)
                 (format nil "`~A` or `~A`" (first variants) (second variants))
                 (format nil "one of ~{`~A`~^, ~}" variants)))
           (variant (name)
             (unless (member name variants :test #'string=)
               (serde-fail-here src "unknown variant `~A`, expected ~A" name (expected)))
             (assoc name newtype :test #'string=))
           (expected-value-peeked ()
             (advance src 1)
             (serde-fail-here src "expected value")))
    (case (peek-kind src)
      (:string (let ((name (read-string src)))
                 (when (variant name) (serde-fail "invalid type: unit variant, expected newtype variant"))
                 (values name nil)))
      (:object
       (advance src 1)
       (unless (eq (peek-kind src) :string) (expected-value-peeked))
       (let* ((name (read-string src))
              (reader (cdr (variant name))))
         (peek src) (advance src 1)        ; the colon
         (let ((value (cond (reader (funcall reader src))
                            ((read-null-p src) nil)
                            (t (invalid-type src "unit")))))
           (unless (eql (peek src) #\}) (serde-fail-here src "expected value"))
           (advance src 1)
           (values name value))))
      (t (peek src) (expected-value-peeked)))))
