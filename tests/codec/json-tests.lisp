(defpackage #:clempo.json.tests
  (:use #:cl #:clempo.base #:clempo.json))

(in-package #:clempo.json.tests)

(deftest json-round-trip ()
  (let* ((text "{\"a\":[1,2,{\"b\":null}],\"c\":\"x\\ny\\u00e9\",\"d\":true,\"e\":-1.5,\"f\":{}}")
         (v (parse-json text)))
    (is= 2 (jget* v "a" 1))
    (is= :null (jget* v "a" 2 "b"))
    (is= (format nil "x~%y~C" (code-char #xe9)) (jget v "c"))
    (is= -1.5d0 (jget v "e"))
    (is (json-equal v (parse-json (to-json-string v))))
    (is= (format nil "{\"a\":[1,2,{\"b\":null}],\"c\":\"x\\ny~C\",\"d\":true,\"e\":-1.5,\"f\":{}}"
                 (code-char #xe9))
         (to-json-string v))))

(deftest json-equal-ignores-key-order ()
  (is (json-equal (parse-json "{\"a\":1,\"b\":[]}") (parse-json "{\"b\":[],\"a\":1}")))
  (is (not (json-equal (parse-json "[]") (parse-json "{}")))))

(deftest json-rejects-garbage ()
  (signals json-error (parse-json "{\"a\":1,}"))
  (signals json-error (parse-json "[1 2]"))
  (signals json-error (parse-json "\"open")))

(deftest json-control-escapes-are-lowercase ()
  ;; The reference (serde_json) writes \u001f, not \u001F.
  (is= "\"\\u001f\"" (to-json-string (string (code-char 31)))))

(defun nested (open close depth &optional (inner ""))
  (with-output-to-string (s)
    (dotimes (i depth) (write-string open s))
    (write-string inner s)
    (dotimes (i depth) (write-string close s))))

(deftest json-malformed-input-signals-json-error ()
  ;; Each of these once escaped as a reader, type or bounds error instead of JSON-ERROR.
  (dolist (text (list "." "-" "\"\\" "\"\\uZZZZ\"" "\"\\uDBFF\\uFFFF\"" "1.2.3.4" "1e1e1"
                      "\"\\u+123\"" "\"\\u-123\"" "\"\\uD800\"" "\"\\uD800x\"" "\"\\uDC00\""
                      "01" "-01" "1." ".5" "+1" "1e" "1e+" "-e1" "0x10" "1_000" "Infinity"
                      "NaN" "-Infinity" "1.5e" "[1,]" "{\"a\"}" "{\"a\":}" "{1:2}" "[" "]" "{"
                      "" "   " "nul" "tru" "[1 2]" "\"a\"b" "\"tab	in string\""
                      (format nil "\"~C\"" (code-char 0)) (format nil "~C" (code-char #x661))
                      (format nil "1~C" (code-char #xff11)) "#()" "#.(error \"x\")" "|a|"
                      "1d0" "1f0" "1/2" "#x10" "1s0"))
    (is= :json-error (handler-case (progn (parse-json text) :parsed)
                       (json-error () :json-error)
                       (error (e) (type-of e)))
         "~S" text))
  (signals json-error (parse-json (coerce #(255 254 65) '(simple-array (unsigned-byte 8) (*)))))
  (signals json-error (parse-json (coerce #(34 #xc3 34) '(simple-array (unsigned-byte 8) (*))))))

(deftest json-never-interns-symbols ()
  ;; The old float path handed tokens like 1.2.3.4 to READ-FROM-STRING, interning them.
  (dotimes (i 200)
    (dolist (text (list (format nil "1.2.3.~D" i) (format nil "9e9e~D" i)))
      (handler-case (parse-json text) (json-error ()))))
  (dolist (package (list *package* "CL-USER" "CLEMPO.JSON"))
    (is (null (find-symbol "1.2.3.7" package)))
    (is (null (find-symbol "9E9E7" package)))))

(deftest json-nesting-depth-is-bounded-without-recursion ()
  ;; serde_json's recursion limit: 127 nested containers parse, 128 do not.
  (is= 127 *json-max-depth*)
  (is (vectorp (parse-json (nested "[" "]" 127))))
  (is (jobject-p (parse-json (nested "{\"a\":" "}" 126 "{}"))))
  (signals json-error (parse-json (nested "[" "]" 128)))
  (signals json-error (parse-json (nested "{\"a\":" "}" 128 "1")))
  ;; Deep hostile input fails at the limit, never on the control stack.
  (signals json-error (parse-json (nested "[" "]" 200000)))
  (signals json-error (parse-json (make-string 200000 :initial-element #\[)))
  ;; An unbounded parse of very deep input uses the heap, not the control stack.
  (let ((v (parse-json (nested "[" "]" 200000 "7") :max-depth nil)))
    (loop repeat 199999 do (setf v (svref v 0)))
    (is (equalp #(7) v)))
  (is (vectorp (parse-json (nested "[" "]" 10) :max-depth 10)))
  (signals json-error (parse-json (nested "[" "]" 11) :max-depth 10)))

(deftest json-numbers-follow-rfc-8259 ()
  (is= 0 (parse-json "0"))
  (is= 0 (parse-json "-0"))
  (is= -12 (parse-json "-12"))
  (is= (1- (expt 2 256)) (parse-json (format nil "~D" (1- (expt 2 256)))))
  (is= (- (expt 10 999)) (parse-json (format nil "-1~v,,,'0A" 999 "")))
  (is (eql 1.5d0 (parse-json "1.5")))
  (is (eql -0.0d0 (parse-json "-0.0")))
  (is (eql 100d0 (parse-json "1E2")))
  (is (eql 100d0 (parse-json "1e+2")))
  (is (eql 0.01d0 (parse-json "1e-2")))
  (is (eql 1.7976931348623157d308 (parse-json "1.7976931348623157e308")))
  (is (eql 4.9406564584124654d-324 (parse-json "4.9406564584124654e-324")))
  (is (eql 2.2250738585072014d-308 (parse-json "2.2250738585072014e-308")))
  (is (eql 0.1d0 (parse-json "0.1")))
  (is (eql 0.0d0 (parse-json "2.4703282292062327e-324")))   ; below half the least denormal
  (is (eql 4.9406564584124654d-324 (parse-json "2.4703282292062328e-324")))
  ;; Beyond the double range: an infinity or a zero, never a Lisp error.
  (is (eql sb-ext:double-float-positive-infinity (parse-json "1e400")))
  (is (eql sb-ext:double-float-negative-infinity (parse-json "-1e400")))
  (is (eql sb-ext:double-float-positive-infinity (parse-json "1.8e308")))
  (is (eql sb-ext:double-float-positive-infinity (parse-json "1e99999999999999999999999")))
  (is (eql 0.0d0 (parse-json "1e-99999999999999999999999")))
  (is (eql -0.0d0 (parse-json "-1e-400")))
  (is (eql 1d0 (parse-json (format nil "0.~v,,,'0A1e1001" 1000 ""))))
  ;; More than 768 significant digits: the tail still decides a halfway case.
  (let ((halfway "9007199254740993")) ; 2^53 + 1, exactly between two doubles
    (is (eql 9007199254740992d0 (parse-json (concatenate 'string halfway ".0"))))
    (is (eql 9007199254740994d0
             (parse-json (format nil "~A.~v,,,'0A1" halfway 2000 "")))))
  ;; A digit flood: an integer past the exact range costs linear work and reads as a double.
  (let ((text (make-string 2000000 :initial-element #\7))
        (start (get-internal-real-time)))
    (is (eql sb-ext:double-float-positive-infinity (parse-json text)))
    (is (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))
  (is (integerp (parse-json (make-string +json-max-integer-digits+ :initial-element #\9)))))

(deftest json-floats-match-the-lisp-reader ()
  ;; The trusted Lisp reader as an oracle for correctly rounded decimal conversion.
  (let ((state (sb-ext:seed-random-state 2026))
        (*read-default-float-format* 'double-float))
    (dotimes (i 3000)
      (let* ((mantissa (random (expt 10 (1+ (random 25 state))) state))
             (exponent (- (random 640 state) 330))
             (text (format nil "~De~D" mantissa exponent))
             (expected (let ((*read-eval* nil))
                         (handler-case (coerce (read-from-string text) 'double-float)
                           (error () nil)))))
        (when (and expected (/= expected 0))
          (is (eql expected (parse-json text)) "~A" text))))
    ;; Mantissas past the 800 kept digits, so the sticky digit carries the tail.
    (dotimes (i 200)
      (let* ((digits (+ 780 (random 400 state)))
             (mantissa (random (expt 10 digits) state))
             (exponent (- (random 600 state) digits 300))
             (text (format nil "~De~D" mantissa exponent))
             (expected (let ((*read-eval* nil))
                         (coerce (read-from-string text) 'double-float))))
        (unless (zerop expected)
          (is (eql expected (parse-json text)) "~A" text))))))

(deftest json-strings-are-strict ()
  (is= (string (code-char #x1F600)) (parse-json "\"\\ud83d\\ude00\""))
  (is= (string (code-char #x10FFFF)) (parse-json "\"\\uDBFF\\uDFFF\""))
  (is= "a/b" (parse-json "\"a\\/b\""))
  (is= (string (code-char #xe9)) (parse-json (string->octets (format nil "\"~C\"" (code-char #xe9)))))
  (signals json-error (parse-json (format nil "\"a~Cb\"" #\Newline)))
  (signals json-error (parse-json "\"\\x41\"")))

(deftest json-writer-emits-only-json ()
  ;; An infinity parsed from an out-of-range literal once wrote as
  ;; #.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY; serde_json's serializer writes null.
  (is= "[null,null,1.5]" (to-json-string (parse-json "[1e999,-1e999,1.5]")))
  (is (json-equal #(:null :null 1.5d0)
                  (parse-json (to-json-string (parse-json "[1e999,-1e999,1.5]"))))))
