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

(defun read-fixed-hex (src n expected)
  "alloy FixedBytes<N>: a hex string (0x optional) of 2N digits, or an array of N bytes."
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
         (push (if (eq (peek-kind src) :number) (read-number src) (invalid-type src "u8")) bytes))
       (unless (= (length bytes) n)
         (serde-fail-here src "invalid length ~D, expected exactly ~D bytes" (length bytes) n))
       (coerce (nreverse bytes) 'octets)))
    ;; deserialize_any: a map's brace is consumed before the visitor refuses it
    (:object (incf (src-pos src)) (serde-fail-here src "invalid type: map, expected ~A" expected))
    (t (invalid-type src expected))))

(defparam hash (src) "32 bytes as hex; returns octets"
  (read-fixed-hex src 32 "32 bytes, represented as a hex string of length 64, an array of u8, or raw bytes"))

(defparam address (src) "20 bytes as hex; returns the address as an integer"
  (octets->integer
   (read-fixed-hex src 20 "20 bytes, represented as a hex string of length 40, an array of u8, or raw bytes")))

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
    (:number (let ((n (read-number src)))
               (if (and (integerp n) (<= 0 n +u64-max+)) n
                   (serde-fail-here src "invalid value: integer `~A`, expected hex-encoded or decimal index" n))))
    (t (invalid-type src "hex-encoded or decimal index"))))

(defparameter +block-tags+ '("latest" "earliest" "pending" "safe" "finalized"))

(defun parse-block-number-or-tag (s)
  "A string block selector: a tag keyword or a u64. Errors carry no position."
  (let ((tag (find s +block-tags+ :test #'string-equal)))
    (cond (tag (intern (string-upcase tag) :keyword))
          ((hex-prefixed-p s) (rust-parse-uint (subseq s 2) 16 64))
          (t (serde-fail "hex string without 0x prefix")))))

(defparam block-number-or-tag (src) "alloy BlockNumberOrTag: :latest ... or an integer"
  (if (eq (peek-kind src) :string)
      (parse-block-number-or-tag (read-string src))
      (invalid-type src "a string")))

(defparam block-id (src) "alloy BlockId: a number or tag, a block hash (octets), or an object"
  (case (peek-kind src)
    (:string (let ((s (read-string src)))
               (if (and (= (length s) 66) (hex-prefixed-p s))
                   (let ((h (subseq s 2)))
                     (let ((bad (position-if-not (lambda (c) (digit-char-p c 16)) h)))
                       (when bad (serde-fail-here src "invalid character '~A' at position ~D" (string (char h bad)) bad)))
                     (list :hash (hex->octets h)))
                   (handler-case (parse-block-number-or-tag s)
                     (serde-error (e) (serde-fail-here src "~A" (serde-error-message e)))))))
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
               (t (serde-fail-here src "unknown field `~A`, expected one of `blockNumber`, `blockHash`, `requireCanonical`" key))))
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

(defun read-quantity (src bits expected)
  (case (peek-kind src)
    (:string (let ((s (read-string src)))
               (if (hex-prefixed-p s)
                   (handler-case (rust-parse-uint (subseq s 2) 16 bits)
                     (serde-error (e) (serde-fail-here src "~A" (serde-error-message e))))
                   (serde-fail-here src "invalid value: string ~A, expected ~A" (rust-debug-string s) expected))))
    (:number (let ((n (read-number src)))
               (if (and (integerp n) (< -1 n (ash 1 bits))) n
                   (serde-fail-here src "invalid value: integer `~A`, expected ~A" n expected))))
    (t (invalid-type src expected))))

(defparam u64 (src) "quantity up to 2^64" (read-quantity src 64 "a 64-bit unsigned integer"))
(defparam u256 (src) "quantity up to 2^256" (read-quantity src 256 "a 256-bit unsigned integer"))
(defparam string (src) "a JSON string" (if (eq (peek-kind src) :string) (read-string src) (invalid-type src "a string")))
