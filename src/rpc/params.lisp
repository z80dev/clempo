(in-package #:clempo.rpc)

;;; Parameter types shared by the eth, debug, trace and tempo namespaces, with the messages
;;; alloy's deserializers give on bad input.

(defun rust-parse-uint (digits radix bits)
  "Rust's uN::from_str_radix: returns the integer or signals serde-error with its message."
  (let ((s (if (and (plusp (length digits)) (char= (char digits 0) #\+)) (subseq digits 1) digits)))
    (cond ((zerop (length digits)) (serde-fail "cannot parse integer from empty string"))
          ((and (zerop (length s)) (plusp (length digits))) (serde-fail "invalid digit found in string"))
          ((notevery (lambda (c) (digit-char-p c radix)) s) (serde-fail "invalid digit found in string"))
          (t (let ((n (parse-integer s :radix radix)))
               (when (>= n (ash 1 bits)) (serde-fail "number too large to fit in target type"))
               n)))))

(defun hex-prefixed-p (s)
  (and (>= (length s) 2) (char= (char s 0) #\0) (char-equal (char s 1) #\x)))

(defun plain-string-span (src)
  "When the next value is a string without escapes: (values start end) of its characters in
SRC's text, consuming nothing; else NIL."
  (when (eql (peek src) #\")
    (let* ((text (src-text src))
           (start (1+ (src-pos src)))
           (stop (clempo.serde::quote-or-escape text start (clempo.serde::src-end src))))
      (declare (type simple-string text) (type fixnum start))
      (when (and stop (char= (schar text stop) #\"))
        (values start stop)))))

(declaim (inline ascii-hex-value))
(defun ascii-hex-value (c)
  "The value of the ASCII hex digit C, or NIL (DIGIT-CHAR-P also takes other scripts' digits)."
  (let ((code (char-code c)))
    (cond ((<= 48 code 57) (- code 48))
          ((<= 97 code 102) (- code 87))
          ((<= 65 code 70) (- code 55)))))

(defun ascii-hex-p (text start end)
  (declare (type simple-string text) (type fixnum start end))
  (macrolet ((scan (type)
               `(let ((text text))
                  (declare (type ,type text))
                  (loop for i of-type fixnum from start below end
                        always (ascii-hex-value (schar text i))))))
    (etypecase text
      (simple-base-string (scan simple-base-string))
      ((simple-array character (*)) (scan (simple-array character (*)))))))

(defun fixed-hex-span (src n)
  "When the next value is a string of exactly 2N ASCII hex digits (0x optional) and no
escapes, consume it and return (values start end) of the digits in SRC's text; else NIL,
consuming nothing. A string READ-FIXED-HEX takes without complaint, read in place."
  (declare (type fixnum n))
  (multiple-value-bind (start end) (plain-string-span src)
    (when start
      (let* ((text (src-text src))
             (digits (if (and (>= (- end start) 2) (char= (schar text start) #\0)
                              (char-equal (schar text (1+ start)) #\x))
                         (+ start 2)
                         start)))
        (declare (type simple-string text) (type fixnum start end digits))
        (when (and (= (- end digits) (* 2 n)) (ascii-hex-p text digits end))
          (setf (src-pos src) (1+ end))
          (values digits end))))))

(defun hex-span-into (text start end o)
  "Decode the ASCII hex digits TEXT[START,END) (an even count) into the octets O."
  (declare (type simple-string text) (type fixnum start end) (type octets o))
  (loop for i of-type fixnum from start below end by 2
        for k of-type fixnum from 0
        do (setf (aref o k) (+ (* 16 (the (integer 0 15) (ascii-hex-value (schar text i))))
                               (the (integer 0 15) (ascii-hex-value (schar text (1+ i)))))))
  o)

(defun read-fixed-hex (src n expected)
  "alloy FixedBytes<N>: a hex string (0x optional) of 2N digits, or an array of N bytes."
  (multiple-value-bind (start end) (fixed-hex-span src n)
    (when start
      (return-from read-fixed-hex (hex-span-into (src-text src) start end (make-octets n)))))
  (case (peek-kind src)
    (:string
     (let* ((s (read-string src))
            (h (if (hex-prefixed-p s) (subseq s 2) s)))
       (cond ((oddp (length h)) (serde-fail-here src "odd number of digits"))
             ((/= (length h) (* 2 n)) (serde-fail-here src "invalid string length"))
             (t (let ((bad (position-if-not (lambda (c) (digit-char-p c 16)) h)))
                  (when bad
                    (serde-fail-here src "invalid character '~A' at position ~D"
                                     (string (char h bad)) bad))
                  (hex->octets h))))))
    (:array
     (let ((bytes '()))
       (do-array (i src)
         (push (read-integer src 0 255 "u8") bytes))
       (unless (= (length bytes) n)
         (serde-fail-here src "invalid length ~D, expected exactly ~D bytes" (length bytes) n))
       (coerce (nreverse bytes) 'octets)))
    (:object (refuse-map src expected))
    (t (invalid-type src expected))))

(defparam hash (src) "32 bytes as hex; returns octets"
  (read-fixed-hex src 32 "32 bytes, represented as a hex string of length 64, an array of u8, or raw bytes"))

(defparam address (src) "20 bytes as hex; returns the address as an integer"
  (multiple-value-bind (start end) (fixed-hex-span src 20)
    (if start
        (let ((o (make-octets 20)))
          (declare (dynamic-extent o))
          (octets->integer (hex-span-into (src-text src) start end o)))
        (octets->integer
         (read-fixed-hex src 20 "20 bytes, represented as a hex string of length 40, an array of u8, or raw bytes")))))

(defparam bool (src) "JSON boolean" (read-bool src "a boolean"))

(defparam any (src) "any JSON value, as clempo.json data" (read-value src))

(defparam index (src) "alloy Index: hex string, decimal string or number"
  (case (peek-kind src)
    (:string (let ((s (read-string src)))
               (handler-case (if (hex-prefixed-p s)
                                 (rust-parse-uint (subseq s 2) 16 64)
                                 (rust-parse-uint s 10 64))
                 (serde-error (e)
                   (serde-fail-here src "~A: ~A"
                                    (if (hex-prefixed-p s)
                                        "Failed to parse hex encoded index value"
                                        "Failed to parse numeric index")
                                    (serde-error-message e))))))
    ;; its visitor takes only a u64: any other number is an invalid type
    (:number (let ((n (read-number src)))
               (if (and (integerp n) (<= 0 n +u64-max+)) n
                   (serde-fail-here src "invalid type: ~A, expected hex-encoded or decimal index"
                                    (number-unexpected n)))))
    (t (invalid-type src "hex-encoded or decimal index"))))

(defparameter +block-tags+ '("latest" "earliest" "pending" "safe" "finalized"))
(defparameter +block-tag-keywords+ (mapcar (lambda (tag) (intern (string-upcase tag) :keyword)) +block-tags+))

(defun parse-block-number-or-tag (s)
  "A string block selector: a tag keyword or a u64. Errors carry no position."
  (let ((tag (find s +block-tags+ :test #'string-equal)))
    (cond (tag (intern (string-upcase tag) :keyword))
          ((hex-prefixed-p s) (rust-parse-uint (subseq s 2) 16 64))
          (t (serde-fail "hex string without 0x prefix")))))

(defun plain-block-selector (src)
  "When the next value is a string without escapes that is 0x and 1 to 15 ASCII hex digits
or a tag, consume it and return what PARSE-BLOCK-NUMBER-OR-TAG makes of it; else NIL,
consuming nothing."
  (multiple-value-bind (start end) (plain-string-span src)
    (when start
      (let* ((text (src-text src))
             (value (if (and (<= 3 (- end start) 17)
                             (char= (schar text start) #\0) (char-equal (schar text (1+ start)) #\x))
                        (and (ascii-hex-p text (+ start 2) end)
                             (let ((n 0))
                               (declare (type (unsigned-byte 60) n))
                               (loop for i of-type fixnum from (+ start 2) below end
                                     do (setf n (+ (* n 16) (the (integer 0 15) (ascii-hex-value (schar text i))))))
                               n))
                        (loop for tag in +block-tags+ for keyword in +block-tag-keywords+
                              when (string-equal tag text :start2 start :end2 end) return keyword))))
        (declare (type simple-string text) (type fixnum start end))
        (when value
          (setf (src-pos src) (1+ end))
          value)))))

(defparam block-number-or-tag (src) "alloy BlockNumberOrTag: :latest ... or an integer"
  (cond ((plain-block-selector src))
        ((eq (peek-kind src) :string) (parse-block-number-or-tag (read-string src)))
        (t (invalid-type src "a string"))))

(defparam block-id (src) "alloy BlockId: a number or tag, a block hash (octets), or an object"
  (case (peek-kind src)
    (:string (let ((s (or (plain-block-selector src) (read-string src))))
               (cond ((not (stringp s)) s)
                     ((and (= (length s) 66) (hex-prefixed-p s))
                      (let ((h (subseq s 2)))
                        (let ((bad (position-if-not (lambda (c) (digit-char-p c 16)) h)))
                          (when bad (serde-fail-here src "invalid character '~A' at position ~D" (string (char h bad)) bad)))
                        (list :hash (hex->octets h))))
                     (t (handler-case (parse-block-number-or-tag s)
                          (serde-error (e) (serde-fail-here src "~A" (serde-error-message e))))))))
    (:object
     (let ((number nil) (hash nil) (canonical nil))
       (do-object (key src)
         (cond ((string= key "blockNumber")
                (setf number (if (eq (peek-kind src) :string)
                                 (handler-case (parse-block-number-or-tag (read-string src))
                                   (serde-error (e) (serde-fail-here src "~A" (serde-error-message e))))
                                 (invalid-type src "a string"))))
               ((string= key "blockHash") (setf hash (read-param 'hash src)))
               ((string= key "requireCanonical") (setf canonical (read-bool src "a boolean")))
               (t (serde-fail-after-key src "unknown field `~A`, expected one of `blockNumber`, `blockHash`, `requireCanonical`" key))))
       (cond (hash (list :hash hash :require-canonical canonical))
             (number number)
             (t (serde-fail-here src "Expected `blockNumber` or `blockHash` with `requireCanonical` optionally")))))
    (t (invalid-type src "Block identifier following EIP-1898"))))

(defparam bytes (src) "alloy Bytes: 0x-prefixed hex"
  (if (eq (peek-kind src) :string)
      (let* ((s (read-string src)) (h (if (hex-prefixed-p s) (subseq s 2) s)))
        (cond ((oddp (length h)) (serde-fail-here src "odd number of digits"))
              (t (let ((bad (position-if-not (lambda (c) (digit-char-p c 16)) h)))
                   (when bad (serde-fail-here src "invalid character '~A' at position ~D" (string (char h bad)) bad))
                   (hex->octets h)))))
      (invalid-type src "a valid hex string")))

(defun parse-ruint (s bits)
  "ruint's FromStr: 0x/0o/0b prefixes or decimal, underscores ignored; NIL when invalid or
too large for BITS."
  (flet ((prefixed-p (prefix) (and (>= (length s) 2) (string-equal prefix s :end2 2))))
    (multiple-value-bind (digits radix)
        (cond ((prefixed-p "0x") (values (subseq s 2) 16))
              ((prefixed-p "0o") (values (subseq s 2) 8))
              ((prefixed-p "0b") (values (subseq s 2) 2))
              (t (values s 10)))
      (let ((n 0))
        (loop for c across digits
              unless (char= c #\_)
                do (let ((d (digit-char-p c radix)))
                     (unless d (return-from parse-ruint nil))
                     (setf n (+ (* n radix) d))))
        (and (< n (ash 1 bits)) n)))))

(defun read-ruint (src bits)
  "ruint Uint<BITS> (alloy's U64, U256 ...) from JSON: a string (hex, octal, binary or
decimal) or a u64 number."
  (let ((expected (format nil "a ~D byte hex string" (ceiling bits 8))))
    (case (peek-kind src)
      (:string (let* ((s (read-string src)) (n (parse-ruint s bits)))
                 (or n (serde-fail-here src "invalid value: string ~A, expected ~A"
                                        (rust-debug-string s) expected))))
      (:number (let ((n (read-number src)))
                 (cond ((and (integerp n) (<= 0 n +u64-max+))
                        (if (< n (ash 1 bits)) n
                            (serde-fail-here src "invalid value: integer `~D`, expected ~A" n expected)))
                       (t (serde-fail-here src "invalid type: ~A, expected ~A" (number-unexpected n) expected)))))
      (t (invalid-type src expected)))))

(defparam u64 (src) "alloy U64 (ruint)" (read-ruint src 64))
(defparam u256 (src) "alloy U256 (ruint)" (read-ruint src 256))
(defparam string (src) "a JSON string" (if (eq (peek-kind src) :string) (read-string src) (invalid-type src "a string")))
