(defpackage #:clempo.json
  (:use #:cl #:clempo.base)
  (:documentation "JSON with an order-preserving representation:
  object -> (:object (\"key\" . value) ...)   array -> simple-vector
  string -> string   number -> integer or double-float
  true/false/null -> :true :false :null")
  (:export #:parse-json #:write-json #:to-json-string #:to-json-octets
           #:jget #:jget* #:jobject #:jobject-p #:json-error #:json-equal
           #:jbool #:*json-max-depth* #:+json-max-integer-digits+))

(in-package #:clempo.json)

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message))
  (:report (lambda (c s) (format s "json: ~A" (json-error-message c)))))

(defun jfail (fmt &rest args) (error 'json-error :message (apply #'format nil fmt args)))

(defun jobject (&rest kvs)
  "(jobject \"a\" 1 \"b\" 2) => (:object (\"a\" . 1) (\"b\" . 2))"
  (cons :object (loop for (k v) on kvs by #'cddr collect (cons k v))))

(defun jobject-p (x) (and (consp x) (eq (car x) :object)))

(defun jbool (x) (if x :true :false))

(defun jget (obj key &optional default)
  (if (jobject-p obj)
      (let ((cell (assoc key (cdr obj) :test #'string=)))
        (if cell (cdr cell) default))
      default))

(defun jget* (obj &rest path)
  "Follow string keys and integer indexes."
  (dolist (k path obj)
    (setf obj (etypecase k
                (string (jget obj k))
                (integer (if (and (simple-vector-p obj) (< k (length obj))) (svref obj k) nil))))))

;;; Parsing from a string (octets are decoded as UTF-8 first). The parser is a loop over
;;; an explicit stack, so input nesting never reaches the control stack; every malformed
;;; input, including invalid UTF-8, signals JSON-ERROR.

(defvar *json-max-depth* 127
  "The most arrays and objects a parsed value may nest, NIL for no limit. 127 is serde_json's
recursion limit (remaining_depth 128, failing when it reaches 0), the reference's own
bound for a typed value.")

(defconstant +json-max-integer-digits+ 1000
  "Integer literals up to this many digits parse exactly. A longer one lies beyond the
double range and reads as a double (an infinity), as serde_json reads an integer that does
not fit 64 bits, so no literal costs quadratic bignum work.")

(defun json-text (input)
  (etypecase input
    (simple-string input)
    (string (coerce input 'simple-string))
    (vector (handler-case (octets->string (coerce input 'octets))
              (error () (jfail "invalid UTF-8"))))))

(defun decimal-to-double (negative digits start end exponent)
  "The double nearest (ties to even) to the decimal DIGITS[START,END) * 10^EXPONENT, where
DIGITS holds only #\\0..#\\9 (the decimal point removed). Beyond the double range the result
is an infinity, below it a zero of the right sign."
  (declare (type simple-string digits) (type fixnum start end) (type integer exponent))
  (let ((start (or (position #\0 digits :start start :end end :test-not #'char=) end)))
    (flet ((signed (x) (if negative (- x) x)))
      (when (= start end)
        (return-from decimal-to-double (signed 0d0)))
      ;; Magnitude: 10^(MAGNITUDE-1) <= value < 10^MAGNITUDE.
      (let ((magnitude (+ (- end start) exponent)))
        (cond ((> magnitude 310) (if negative
                                     sb-ext:double-float-negative-infinity
                                     sb-ext:double-float-positive-infinity))
              ((< magnitude -330) (signed 0d0))
              (t
               ;; 768 significant digits decide every halfway case; keep 800 and fold the
               ;; rest into a sticky nonzero digit so the rounding direction is unchanged.
               (let* ((kept (min (- end start) 800))
                      (mantissa (parse-integer digits :start start :end (+ start kept)))
                      (scale (+ exponent (- (- end start) kept))))
                 (when (find #\0 digits :start (+ start kept) :end end :test-not #'char=)
                   (setf mantissa (+ (* 10 mantissa) 1) scale (1- scale)))
                 (let ((x (handler-case
                              (coerce (if (minusp scale)
                                          (/ mantissa (expt 10 (- scale)))
                                          (* mantissa (expt 10 scale)))
                                      'double-float)
                            (floating-point-overflow () sb-ext:double-float-positive-infinity))))
                   (signed x)))))))))

(defun parse-json (input &key (max-depth *json-max-depth*))
  "Parse one JSON text (RFC 8259) from a string or UTF-8 octets. Arrays and objects may
nest at most MAX-DEPTH deep (NIL: unbounded). Signals JSON-ERROR on any malformed input."
  (let* ((s (json-text input))
         (pos 0)
         (n (length s))
         ;; Open containers, innermost first: (:array . items) or (:object key . pairs),
         ;; items and pairs in reverse order.
         (stack '())
         (depth 0))
    (declare (type simple-string s) (type fixnum pos n depth))
    (labels ((peek () (if (< pos n) (schar s pos) (jfail "unexpected end")))
             (skip-ws ()
               (loop while (and (< pos n) (member (schar s pos) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf pos)))
             (expect (ch)
               (unless (and (< pos n) (char= (schar s pos) ch)) (jfail "expected ~C at ~D" ch pos))
               (incf pos))
             (literal (word value)
               (unless (and (<= (+ pos (length word)) n) (string= word s :start2 pos :end2 (+ pos (length word))))
                 (jfail "bad literal at ~D" pos))
               (incf pos (length word))
               value)
             (open-container ()
               (incf pos)
               (incf depth)
               (when (and max-depth (> depth max-depth))
                 (jfail "nesting deeper than ~D at ~D" max-depth pos)))
             (digits ()
               "Skip a run of ASCII digits; return how many."
               (let ((start pos))
                 (loop while (and (< pos n) (char<= #\0 (schar s pos) #\9)) do (incf pos))
                 (- pos start)))
             (hex4 ()
               (when (> (+ pos 4) n) (jfail "short \\u escape at ~D" pos))
               (let ((code 0))
                 (dotimes (i 4 code)
                   (let* ((c (schar s pos))
                          (d (cond ((char<= #\0 c #\9) (- (char-code c) 48))
                                   ((char<= #\a c #\f) (- (char-code c) 87))
                                   ((char<= #\A c #\F) (- (char-code c) 55)))))
                     (unless d (jfail "bad \\u escape at ~D" pos))
                     (setf code (+ (* code 16) d))
                     (incf pos)))))
             (str ()
               (expect #\")
               (let ((start pos))
                 ;; fast path: no escapes
                 (loop while (and (< pos n)
                                  (let ((c (schar s pos)))
                                    (not (or (char= c #\") (char= c #\\) (< (char-code c) 32)))))
                       do (incf pos))
                 (when (>= pos n) (jfail "unterminated string"))
                 (if (char= (schar s pos) #\")
                     (prog1 (subseq s start pos) (incf pos))
                     (let ((out (make-string-output-stream)))
                       (write-string s out :start start :end pos)
                       (loop
                         (when (>= pos n) (jfail "unterminated string"))
                         (let ((c (schar s pos)))
                           (incf pos)
                           (cond ((char= c #\") (return (get-output-stream-string out)))
                                 ((< (char-code c) 32)
                                  (jfail "control character in string at ~D" (1- pos)))
                                 ((char= c #\\)
                                  (when (>= pos n) (jfail "unterminated string"))
                                  (let ((e (schar s pos)))
                                    (incf pos)
                                    (case e
                                      (#\" (write-char #\" out)) (#\\ (write-char #\\ out))
                                      (#\/ (write-char #\/ out)) (#\b (write-char #\Backspace out))
                                      (#\f (write-char #\Page out)) (#\n (write-char #\Newline out))
                                      (#\r (write-char #\Return out)) (#\t (write-char #\Tab out))
                                      (#\u (let ((cp (hex4)))
                                             (cond ((<= #xD800 cp #xDBFF)
                                                    (unless (and (< (1+ pos) n) (char= (schar s pos) #\\)
                                                                 (char= (schar s (1+ pos)) #\u))
                                                      (jfail "lone leading surrogate at ~D" pos))
                                                    (incf pos 2)
                                                    (let ((lo (hex4)))
                                                      (unless (<= #xDC00 lo #xDFFF)
                                                        (jfail "invalid trailing surrogate at ~D" pos))
                                                      (setf cp (+ #x10000 (ash (- cp #xD800) 10)
                                                                  (- lo #xDC00)))))
                                                   ((<= #xDC00 cp #xDFFF)
                                                    (jfail "lone trailing surrogate at ~D" pos)))
                                             (write-char (code-char cp) out)))
                                      (t (jfail "bad escape \\~C" e)))))
                                 (t (write-char c out)))))))))
             (num ()
               ;; number = [ "-" ] ( "0" / digit1-9 *digit ) [ "." 1*digit ] [ e [ "+" / "-" ] 1*digit ]
               (let ((start pos) (negative nil) (int-start 0) (int-end 0)
                     (frac-start 0) (frac-end 0) (exponent 0))
                 (declare (type fixnum start int-start int-end frac-start frac-end)
                          (type integer exponent))
                 (when (char= (peek) #\-) (setf negative t) (incf pos))
                 (setf int-start pos)
                 (cond ((and (< pos n) (char= (schar s pos) #\0)) (incf pos))
                       ((zerop (digits)) (jfail "bad number at ~D" start)))
                 (setf int-end pos frac-start pos frac-end pos)
                 (when (and (< pos n) (char= (schar s pos) #\.))
                   (incf pos)
                   (setf frac-start pos)
                   (when (zerop (digits)) (jfail "bad number at ~D" start))
                   (setf frac-end pos))
                 (when (and (< pos n) (char-equal (schar s pos) #\e))
                   (incf pos)
                   (let ((sign 1))
                     (when (and (< pos n) (find (schar s pos) "+-"))
                       (when (char= (schar s pos) #\-) (setf sign -1))
                       (incf pos))
                     (let ((e-start pos))
                       (when (zerop (digits)) (jfail "bad number at ~D" start))
                       ;; Saturate: past 10^7 every value is already an infinity or a zero.
                       (loop for i from e-start below pos
                             do (setf exponent (min 10000000 (+ (* 10 exponent)
                                                                (- (char-code (schar s i)) 48)))))
                       (setf exponent (* sign exponent)))))
                 (cond ((and (= frac-start frac-end) (= pos frac-end)
                             (<= (- int-end int-start) +json-max-integer-digits+))
                        (let ((v (parse-integer s :start int-start :end int-end)))
                          (if negative (- v) v)))
                       ((= frac-start frac-end)
                        (decimal-to-double negative s int-start int-end exponent))
                       (t
                        ;; Join the integer and fraction digits; only the digits matter.
                        (let ((joined (concatenate 'simple-string
                                                   (subseq s int-start int-end)
                                                   (subseq s frac-start frac-end))))
                          (decimal-to-double negative joined 0 (length joined)
                                             (- exponent (- frac-end frac-start)))))))))
      (let ((value nil))
        (loop
          ;; Parse a value, entering containers until one completes.
          (skip-ws)
          (setf value
                (case (peek)
                  (#\{ (open-container) (skip-ws)
                   (cond ((eql (peek) #\}) (incf pos) (decf depth) (list :object))
                         (t (let ((key (str))) (skip-ws) (expect #\:)
                              (push (list :object key) stack))
                            nil)))
                  (#\[ (open-container) (skip-ws)
                   (cond ((eql (peek) #\]) (incf pos) (decf depth) (vector))
                         (t (push (list :array) stack) nil)))
                  (#\" (str))
                  (#\t (literal "true" :true))
                  (#\f (literal "false" :false))
                  (#\n (literal "null" :null))
                  (t (num))))
          ;; NIL means a container was opened and its first member comes next.
          (when value
            ;; Hand the value to the innermost container; close containers that end.
            (loop
              (let ((frame (first stack)))
                (when (null frame)
                  (skip-ws)
                  (unless (= pos n) (jfail "trailing data at ~D" pos))
                  (return-from parse-json value))
                (skip-ws)
                (if (eq (first frame) :array)
                    (progn
                      (push value (cdr frame))
                      (case (peek)
                        (#\, (incf pos) (return))
                        (#\] (incf pos) (pop stack) (decf depth)
                         (setf value (coerce (nreverse (cdr frame)) 'simple-vector)))
                        (t (jfail "expected , or ] at ~D" pos))))
                    (progn
                      (push (cons (second frame) value) (cddr frame))
                      (case (peek)
                        (#\, (incf pos) (skip-ws)
                         (setf (second frame) (str))
                         (skip-ws) (expect #\:)
                         (return))
                        (#\} (incf pos) (pop stack) (decf depth)
                         (setf value (cons :object (nreverse (cddr frame)))))
                        (t (jfail "expected , or } at ~D" pos)))))))))))))

;;; Writing

(defun write-json-string (str out)
  (write-char #\" out)
  (loop for c across str
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Return (write-string "\\r" out))
             (#\Tab (write-string "\\t" out))
             (#\Backspace (write-string "\\b" out))
             (#\Page (write-string "\\f" out))
             (t (if (< (char-code c) 32)
                    (format out "\\u~(~4,'0x~)" (char-code c))
                    (write-char c out)))))
  (write-char #\" out))

(defun write-json (v out)
  (cond ((eq v :true) (write-string "true" out))
        ((eq v :false) (write-string "false" out))
        ((or (eq v :null) (null v)) (write-string "null" out))
        ((integerp v) (format out "~D" v))
        ;; JSON has no infinity or NaN; serde_json's serializer writes null for both.
        ((floatp v) (if (or (sb-ext:float-infinity-p v) (sb-ext:float-nan-p v))
                        (write-string "null" out)
                        (let ((*read-default-float-format* 'double-float)) (format out "~F" v))))
        ((stringp v) (write-json-string v out))
        ((jobject-p v)
         (write-char #\{ out)
         (loop for (pair . rest) on (cdr v)
               do (write-json-string (car pair) out)
                  (write-char #\: out)
                  (write-json (cdr pair) out)
                  (when rest (write-char #\, out)))
         (write-char #\} out))
        ((vectorp v)
         (write-char #\[ out)
         (loop for i from 0 below (length v)
               do (when (plusp i) (write-char #\, out))
                  (write-json (aref v i) out))
         (write-char #\] out))
        ((listp v) (write-json (coerce v 'simple-vector) out))
        (t (jfail "cannot encode ~S" v))))

(defun to-json-string (v)
  (with-output-to-string (out) (write-json v out)))

(defun to-json-octets (v) (string->octets (to-json-string v)))

(defun json-equal (a b)
  "Structural equality; object key order is ignored."
  (cond ((jobject-p a)
         (and (jobject-p b)
              (= (length (cdr a)) (length (cdr b)))
              (loop for (k . v) in (cdr a)
                    always (let ((cell (assoc k (cdr b) :test #'string=)))
                             (and cell (json-equal v (cdr cell)))))))
        ((and (vectorp a) (not (stringp a)))
         (and (vectorp b) (not (stringp b)) (= (length a) (length b))
              (every #'json-equal a b)))
        ((stringp a) (and (stringp b) (string= a b)))
        ((numberp a) (and (numberp b) (= a b)))
        (t (eq a b))))
