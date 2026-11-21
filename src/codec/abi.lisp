(defpackage #:clempo.abi
  (:use #:cl #:clempo.base)
  (:documentation "Solidity ABI: types parsed from signature strings, encode and decode of
static and dynamic values, tuples and arrays, selectors, event topics and error encoding.
Decoding follows alloy-sol-types 1.7's decoder, including its strict mode (canonical
offsets, zero padding, no trailing bytes) and validation, which Tempo turns on by fork.

Values: uintN/intN/address/bytesN/enum are integers (bytesN as the big-endian value of its N
bytes, int as a signed integer), bool is T or NIL, bytes is an octet vector, string a Lisp
string, arrays and tuples are lists.

Types: :address :bool :bytes :string (:uint n) (:int n) (:bytes-n n) (:enum count)
(:array type) (:array type n) (:tuple type...).")
  (:export
   #:abi-error #:abi-decode-error #:abi-error-message
   #:parse-abi-type #:parse-signature #:abi-type-string #:canonical-signature
   #:abi-dynamic-p #:abi-min-words
   #:abi-selector #:abi-selector-octets #:event-topic #:signature-types
   #:abi-encode #:abi-encode-call #:abi-encode-value
   #:abi-decode #:abi-decode-call
   #:abi-encode-error #:+error-string-selector+ #:+panic-selector+ #:panic-data
   #:utf8-decode #:utf8-encode #:utf8-valid-p))

(in-package #:clempo.abi)

(define-condition abi-error (error)
  ((message :initarg :message :reader abi-error-message))
  (:report (lambda (c s) (format s "abi: ~A" (abi-error-message c)))))

(define-condition abi-decode-error (abi-error) ())

(defun abi-fail (fmt &rest args)
  (error 'abi-decode-error :message (apply #'format nil fmt args)))

;;; UTF-8 with Rust's semantics: strict validation, and lossy decoding that replaces each
;;; maximal invalid subpart with U+FFFD (String::from_utf8_lossy).

(defun utf8-sequence-length (data i end)
  "Length of the valid UTF-8 sequence starting at I, or NIL with the length of the maximal
invalid subpart as second value."
  (declare (type octets data) (type fixnum i end))
  (let ((b0 (aref data i)))
    (flet ((cont (k lo hi)
             (and (< (+ i k) end) (<= lo (aref data (+ i k)) hi))))
      (cond ((< b0 #x80) 1)
            ((<= #xc2 b0 #xdf) (if (cont 1 #x80 #xbf) 2 (values nil 1)))
            ((<= #xe0 b0 #xef)
             (let ((lo (if (= b0 #xe0) #xa0 #x80)) (hi (if (= b0 #xed) #x9f #xbf)))
               (cond ((not (cont 1 lo hi)) (values nil 1))
                     ((not (cont 2 #x80 #xbf)) (values nil 2))
                     (t 3))))
            ((<= #xf0 b0 #xf4)
             (let ((lo (if (= b0 #xf0) #x90 #x80)) (hi (if (= b0 #xf4) #x8f #xbf)))
               (cond ((not (cont 1 lo hi)) (values nil 1))
                     ((not (cont 2 #x80 #xbf)) (values nil 2))
                     ((not (cont 3 #x80 #xbf)) (values nil 3))
                     (t 4))))
            (t (values nil 1))))))

(defun utf8-valid-p (data &key (start 0) (end (length data)))
  (loop with i = start
        while (< i end)
        do (let ((n (utf8-sequence-length data i end)))
             (if n (incf i n) (return nil)))
        finally (return t)))

(defun utf8-decode (data &key (start 0) (end (length data)) lossy)
  "Decode UTF-8. Invalid input signals ABI-DECODE-ERROR unless LOSSY, which replaces each
maximal invalid subpart with U+FFFD as Rust's from_utf8_lossy does."
  (declare (type octets data))
  (let ((out (make-string-output-stream)))
    (loop with i = start
          while (< i end)
          do (multiple-value-bind (n bad) (utf8-sequence-length data i end)
               (cond (n
                      (let ((b0 (aref data i)))
                        (write-char
                         (code-char
                          (ecase n
                            (1 b0)
                            (2 (logior (ash (logand b0 #x1f) 6) (logand (aref data (+ i 1)) #x3f)))
                            (3 (logior (ash (logand b0 #x0f) 12) (ash (logand (aref data (+ i 1)) #x3f) 6)
                                       (logand (aref data (+ i 2)) #x3f)))
                            (4 (logior (ash (logand b0 #x07) 18) (ash (logand (aref data (+ i 1)) #x3f) 12)
                                       (ash (logand (aref data (+ i 2)) #x3f) 6)
                                       (logand (aref data (+ i 3)) #x3f)))))
                         out))
                      (incf i n))
                     (lossy (write-char (code-char #xfffd) out) (incf i bad))
                     (t (abi-fail "invalid utf-8 at byte ~D" (- i start))))))
    (get-output-stream-string out)))

(defun utf8-encode (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

;;; Types.

(defun parse-abi-type (string &key (start 0) (end (length string)))
  "Parse one Solidity type (\"uint256\", \"(address,bytes)[]\", \"tuple(uint8 a)\"),
ignoring a trailing parameter name. Returns the type."
  (let ((s (string-trim " " (subseq string start end))))
    ;; drop a parameter name and data location: "address to", "string calldata s"
    (let ((parts (split-top-level s #\Space)))
      (setf s (first parts)))
    (parse-type-core s)))

(defun split-top-level (s sep)
  "Split S at SEP characters that are not inside parentheses."
  (let ((parts '()) (depth 0) (begin 0))
    (loop for i from 0 below (length s)
          for c = (char s i)
          do (case c
               (#\( (incf depth))
               (#\) (decf depth))
               (t (when (and (char= c sep) (zerop depth))
                    (push (subseq s begin i) parts)
                    (setf begin (1+ i))))))
    (push (subseq s begin) parts)
    (remove "" (mapcar (lambda (p) (string-trim " " p)) (nreverse parts)) :test #'string=)))

(defun parse-type-core (s)
  ;; array suffixes bind last: T[] / T[n]
  (let ((n (length s)))
    (when (and (> n 0) (char= (char s (1- n)) #\]))
      (let* ((open (position #\[ s :from-end t))
             (inner (parse-type-core (subseq s 0 open)))
             (dim (subseq s (1+ open) (1- n))))
        (return-from parse-type-core
          (if (string= dim "")
              (list :array inner)
              (list :array inner (parse-integer dim))))))
    (cond ((and (> n 0) (char= (char s 0) #\())
           (cons :tuple (mapcar #'parse-abi-type (split-top-level (subseq s 1 (1- n)) #\,))))
          ((and (> n 5) (string= "tuple" s :end2 5))
           (parse-type-core (subseq s 5)))
          ((string= s "address") :address)
          ((string= s "bool") :bool)
          ((string= s "string") :string)
          ((string= s "bytes") :bytes)
          ((string= s "uint") '(:uint 256))
          ((string= s "int") '(:int 256))
          ((and (> n 4) (string= "uint" s :end2 4) (every #'digit-char-p (subseq s 4)))
           (let ((bits (parse-integer s :start 4)))
             (unless (and (<= 8 bits 256) (zerop (mod bits 8))) (error 'abi-error :message s))
             (list :uint bits)))
          ((and (> n 3) (string= "int" s :end2 3) (every #'digit-char-p (subseq s 3)))
           (let ((bits (parse-integer s :start 3)))
             (unless (and (<= 8 bits 256) (zerop (mod bits 8))) (error 'abi-error :message s))
             (list :int bits)))
          ((and (> n 5) (string= "bytes" s :end2 5) (every #'digit-char-p (subseq s 5)))
           (let ((len (parse-integer s :start 5)))
             (unless (<= 1 len 32) (error 'abi-error :message s))
             (list :bytes-n len)))
          ((and (> n 5) (string= "enum:" s :end2 5))
           (list :enum (parse-integer s :start 5)))
          (t (error 'abi-error :message (format nil "unknown type ~S" s))))))

(defun parse-signature (signature)
  "\"transfer(address to, uint256)\" -> (values \"transfer\" (:address (:uint 256)))."
  (let* ((open (position #\( signature))
         (close (position-of-matching signature open)))
    (values (string-trim " " (subseq signature 0 open))
            (mapcar #'parse-abi-type (split-top-level (subseq signature (1+ open) close) #\,)))))

(defun position-of-matching (s open)
  (loop with depth = 0
        for i from open below (length s)
        do (case (char s i)
             (#\( (incf depth))
             (#\) (decf depth) (when (zerop depth) (return i))))
        finally (error 'abi-error :message (format nil "unbalanced ~S" s))))

(defun abi-type-string (type)
  "Canonical Solidity spelling of TYPE, as used in selectors."
  (etypecase type
    (keyword (string-downcase (symbol-name type)))
    (cons (ecase (first type)
            (:uint (format nil "uint~D" (second type)))
            (:int (format nil "int~D" (second type)))
            (:bytes-n (format nil "bytes~D" (second type)))
            (:enum "uint8")
            (:array (if (cddr type)
                        (format nil "~A[~D]" (abi-type-string (second type)) (third type))
                        (format nil "~A[]" (abi-type-string (second type)))))
            (:tuple (format nil "(~{~A~^,~})" (mapcar #'abi-type-string (rest type))))))))

(defun canonical-signature (name types)
  (format nil "~A(~{~A~^,~})" name (mapcar #'abi-type-string types)))

(defun signature-types (signature)
  "The parameter types of SIGNATURE."
  (nth-value 1 (parse-signature signature)))

(defun abi-dynamic-p (type)
  (etypecase type
    (keyword (member type '(:bytes :string)))
    (cons (case (first type)
            (:array (or (null (cddr type)) (abi-dynamic-p (second type))))
            (:tuple (some #'abi-dynamic-p (rest type)))
            (t nil)))))

(defun abi-min-words (type)
  "alloy's MINIMUM_WORDS: head words of a value of TYPE."
  (if (abi-dynamic-p type)
      1
      (if (consp type)
          (case (first type)
            (:array (* (third type) (abi-min-words (second type))))
            (:tuple (reduce #'+ (rest type) :key #'abi-min-words))
            (t 1))
          1)))

;;; Selectors and topics.

(defun signature-hash (signature)
  (multiple-value-bind (name types) (parse-signature signature)
    (clempo.keccak:keccak256 (string->octets (canonical-signature name types)))))

(defun abi-selector (signature)
  "The 4-byte function selector of SIGNATURE as an integer."
  (octets->integer (signature-hash signature) :end 4))

(define-compiler-macro abi-selector (&whole w signature)
  (if (stringp signature) (abi-selector signature) w))

(defun abi-selector-octets (signature)
  (octets-slice (signature-hash signature) 0 4))

(defun event-topic (signature)
  "Topic 0 of the event SIGNATURE as an integer."
  (octets->integer (signature-hash signature)))

(define-compiler-macro event-topic (&whole w signature)
  (if (stringp signature) (event-topic signature) w))

;;; Encoding.

(defun word-of (type value)
  "The 32-byte word of a static scalar VALUE as an integer."
  (etypecase type
    (keyword (ecase type
               (:address value)
               (:bool (if value 1 0))))
    (cons (ecase (first type)
            ((:uint :enum) value)
            (:int (ldb (byte 256 0) value))
            (:bytes-n (ash value (* 8 (- 32 (second type)))))))))

(defun encoded-words (type value)
  "Total words VALUE of TYPE takes (head + tail) when encoded in place."
  (cond ((not (abi-dynamic-p type))
         (abi-min-words type))
        (t (+ 1 (tail-words type value)))))

(defun tail-words (type value)
  (etypecase type
    (keyword (+ 1 (ceiling (length (if (stringp value) (utf8-encode value) value)) 32)))
    (cons (ecase (first type)
            (:array (let ((elem (second type)))
                      (+ (if (cddr type) 0 1)
                         (loop for v in value sum (encoded-words elem v)))))
            (:tuple (loop for ty in (rest type) for v in value sum (encoded-words ty v)))))))

(defun encode-sequence (types values out pos)
  "Write the tuple encoding of VALUES at POS of OUT; return the position after it."
  (declare (type octets out) (type fixnum pos))
  (let* ((head-words (loop for ty in types sum (if (abi-dynamic-p ty) 1 (abi-min-words ty))))
         (tail (+ pos (* 32 head-words)))
         (head pos))
    (loop for ty in types
          for v in values
          do (if (abi-dynamic-p ty)
                 (progn
                   (put-word out head (- tail pos))
                   (incf head 32)
                   (setf tail (encode-tail ty v out tail)))
                 (setf head (encode-static ty v out head))))
    tail))

(defun put-word (out pos n)
  (declare (type octets out) (type fixnum pos) (type integer n))
  (loop for i from 0 below 32
        do (setf (aref out (+ pos i)) (ldb (byte 8 (* 8 (- 31 i))) n))))

(defun encode-static (type value out pos)
  (if (and (consp type) (member (first type) '(:array :tuple)))
      (let ((types (if (eq (first type) :tuple)
                       (rest type)
                       (make-list (third type) :initial-element (second type)))))
        (loop for ty in types for v in value
              do (setf pos (encode-static ty v out pos)))
        pos)
      (progn (put-word out pos (word-of type value)) (+ pos 32))))

(defun encode-tail (type value out pos)
  "Write the tail of dynamic VALUE at POS; return the position after it."
  (etypecase type
    (keyword
     (let ((bytes (if (stringp value) (utf8-encode value) value)))
       (put-word out pos (length bytes))
       (replace out bytes :start1 (+ pos 32))
       (+ pos 32 (* 32 (ceiling (length bytes) 32)))))
    (cons
     (ecase (first type)
       (:array (if (cddr type)
                   (encode-sequence (make-list (third type) :initial-element (second type)) value out pos)
                   (progn
                     (put-word out pos (length value))
                     (encode-sequence (make-list (length value) :initial-element (second type))
                                      value out (+ pos 32)))))
       (:tuple (encode-sequence (rest type) value out pos))))))

(defun abi-encode (types values)
  "The ABI encoding of VALUES as the tuple of TYPES (function arguments, return data)."
  (let* ((words (loop for ty in types for v in values sum (encoded-words ty v)))
         (out (make-octets (* 32 words))))
    (encode-sequence types values out 0)
    out))

(defun abi-encode-value (type value)
  "The encoding of a single VALUE (alloy's abi_encode: a one-element tuple)."
  (abi-encode (list type) (list value)))

(defun abi-encode-call (selector types values)
  "SELECTOR (integer) followed by the encoding of VALUES."
  (let* ((args (abi-encode types values))
         (out (make-octets (+ 4 (length args)))))
    (loop for i from 0 below 4 do (setf (aref out i) (ldb (byte 8 (* 8 (- 3 i))) selector)))
    (replace out args :start1 4)
    out))

(defun abi-encode-error (signature &rest args)
  "Revert data for the Solidity error SIGNATURE with ARGS."
  (abi-encode-call (abi-selector signature) (signature-types signature) args))

(define-constant +error-string-selector+ #x08c379a0)
(define-constant +panic-selector+ #x4e487b71)

(defun panic-data (code)
  "Revert data of Panic(uint256 CODE)."
  (abi-encode-call +panic-selector+ '((:uint 256)) (list code)))

;;; Decoding, after alloy-sol-types 1.7 abi/decoder.rs and abi/token.rs.

(defstruct (decoder (:constructor %make-decoder))
  (data nil :type octets)
  (start 0 :type fixnum)                ; this decoder's buffer is DATA[START..END)
  (end 0 :type fixnum)
  (offset 0 :type fixnum)               ; relative to START
  (depth 0 :type fixnum)
  (strict nil)
  (validate nil)
  (next 0 :type fixnum)                 ; strict_next_offset, relative to START
  (parent nil))

(defconstant +recursion-limit+ 16)

(defun dec-remaining (d) (- (decoder-end d) (decoder-start d) (decoder-offset d)))

(defun child-decoder (d offset)
  (when (>= (decoder-depth d) +recursion-limit+) (abi-fail "recursion limit"))
  (unless (<= offset (- (decoder-end d) (decoder-start d))) (abi-fail "overrun"))
  (%make-decoder :data (decoder-data d) :start (+ (decoder-start d) offset) :end (decoder-end d)
                 :depth (1+ (decoder-depth d)) :strict (decoder-strict d)
                 :validate (decoder-validate d) :parent d))

(defun finish-child (child)
  "alloy's Decoder::drop: a strict child reports how far it reached to its parent."
  (let ((parent (decoder-parent child)))
    (when (and parent (decoder-strict child))
      (let ((reach (+ (- (decoder-start child) (decoder-start parent))
                      (max (decoder-offset child) (decoder-next child)))))
        (setf (decoder-next parent) (max (decoder-next parent) reach))))))

(defmacro with-child ((var form) &body body)
  `(let ((,var ,form))
     (multiple-value-prog1 (progn ,@body) (finish-child ,var))))

(defun set-strict-head-words (d words)
  (when (decoder-strict d)
    (setf (decoder-next d) (max (decoder-next d) (+ (decoder-offset d) (* 32 words))))))

(defun take-word (d)
  (when (< (dec-remaining d) 32) (abi-fail "overrun"))
  (let ((pos (+ (decoder-start d) (decoder-offset d))))
    (incf (decoder-offset d) 32)
    (octets->integer (decoder-data d) :start pos :end (+ pos 32))))

(defun take-offset (d)
  (let ((w (take-word d)))
    (unless (< w (ash 1 64)) (abi-fail "offset out of range"))
    w))

(defun take-indirection (d)
  (let ((offset (take-offset d)))
    (when (and (decoder-strict d) (/= offset (decoder-next d)))
      (abi-fail "non-canonical offset ~D" offset))
    (child-decoder d offset)))

(defun take-padded-slice (d len)
  (when (> len (dec-remaining d)) (abi-fail "overrun"))
  (let* ((pos (+ (decoder-start d) (decoder-offset d)))
         (bytes (subseq (decoder-data d) pos (+ pos len))))
    (incf (decoder-offset d) len)
    (when (decoder-strict d)
      (let ((pad (mod (- 32 (mod len 32)) 32)))
        (when (> pad (dec-remaining d)) (abi-fail "overrun"))
        (let ((p (+ (decoder-start d) (decoder-offset d))))
          (unless (loop for i from p below (+ p pad) always (zerop (aref (decoder-data d) i)))
            (abi-fail "non-zero padding")))
        (incf (decoder-offset d) pad)))
    bytes))

(defun word-value (type w validate)
  "Detokenize (and with VALIDATE, type-check) the word W as TYPE."
  (flet ((bad () (abi-fail "invalid ~A" (abi-type-string type))))
    (etypecase type
      (keyword
       (ecase type
         (:address (when (and validate (>= w (ash 1 160))) (bad)) (ldb (byte 160 0) w))
         (:bool (when (and validate (> w 1)) (bad)) (/= w 0))))
      (cons
       (ecase (first type)
         (:uint (let ((bits (second type)))
                  (when (and validate (>= w (ash 1 bits))) (bad))
                  (ldb (byte bits 0) w)))
         (:int (let* ((bits (second type))
                      (v (ldb (byte bits 0) w))
                      (signed (if (logbitp (1- bits) v) (- v (ash 1 bits)) v)))
                 (when (and validate (/= (ldb (byte 256 0) signed) w)) (bad))
                 signed))
         (:bytes-n (let ((shift (* 8 (- 32 (second type)))))
                     (when (and validate (/= 0 (ldb (byte shift 0) w))) (bad))
                     (ash w (- shift))))
         (:enum (when (and validate (>= w (second type))) (bad))
          (let ((b (ldb (byte 8 0) w)))
            (if (< b (second type)) b 255))))))))

(defun decode-from (type d)
  (etypecase type
    (keyword
     (case type
       ((:bytes :string)
        (with-child (c (take-indirection d))
          (let* ((len (take-offset c))
                 (bytes (take-padded-slice c len)))
            (if (eq type :bytes)
                bytes
                (utf8-decode bytes :lossy (not (decoder-validate d)))))))
       (t (word-value type (take-word d) (decoder-validate d)))))
    (cons
     (case (first type)
       (:array
        (let ((elem (second type)))
          (if (cddr type)
              (let ((types (make-list (third type) :initial-element elem)))
                (if (abi-dynamic-p elem)
                    (with-child (c (take-indirection d)) (decode-sequence types c))
                    (decode-sequence types d)))
              (with-child (c (take-indirection d))
                (let* ((len (take-offset c))
                       (required (* (abi-min-words elem) len)))
                  (with-child (c2 (child-decoder c (decoder-offset c)))
                    (set-strict-head-words c2 required)
                    (when (> required (floor (dec-remaining c2) 32)) (abi-fail "overrun"))
                    (loop repeat len collect (decode-from elem c2))))))))
       (:tuple
        (if (abi-dynamic-p type)
            (with-child (c (take-indirection d)) (decode-sequence (rest type) c))
            (decode-sequence (rest type) d)))
       (t (word-value type (take-word d) (decoder-validate d)))))))

(defun decode-sequence (types d)
  (set-strict-head-words d (reduce #'+ types :key #'abi-min-words))
  (loop for ty in types collect (decode-from ty d)))

(defun abi-decode (types data &key (start 0) (end (length data)) strict validate allow-trailing)
  "Decode DATA[START..END) as the tuple TYPES; returns the list of values. STRICT is
alloy's strict mode (canonical layout, which also validates); VALIDATE type-checks values;
ALLOW-TRAILING lets a strict decode leave trailing bytes. Signals ABI-DECODE-ERROR."
  (let* ((d (%make-decoder :data data :start start :end end :strict strict
                           :validate (or validate strict)))
         (values (decode-sequence types d)))
    (when (and strict (not allow-trailing) (/= (decoder-next d) (- end start)))
      (abi-fail "trailing or missing bytes"))
    values))

(defun abi-decode-call (types data &rest keys)
  "Decode the arguments of calldata DATA (selector skipped)."
  (apply #'abi-decode types data :start 4 keys))
