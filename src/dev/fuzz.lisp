(defpackage #:clempo.fuzz
  (:use #:cl #:clempo.base #:clempo.state #:clempo.evm)
  (:local-nicknames (#:io #:clempo.io) (#:rlp #:clempo.rlp) (#:json #:clempo.json)
                    (#:abi #:clempo.abi) (#:snappy #:clempo.snappy) (#:tx #:clempo.transactions))
  (:documentation "Seeded fuzzing. Every case draws its input from CLEMPO.IO:*ENTROPY* bound
to a stream seeded by (target, seed, case), so any reported case replays alone with
`clempo-dev fuzz -target T -seed S -case N -debug`. A case fails when it signals a
condition the target does not declare for bad input (a stack or heap exhaustion, a type
error, a timeout) or when a property fails: a round trip, canonical re-encoding, or
agreement with a slow reference model or the other EVM execution path.

tests/fuzz/cases.sexp holds two kinds of entry. (:known :target T :match S ...) is a
finding routed to its owner and not yet fixed: a failure of T whose description contains
S is reported without failing the run. (:replay :target T :seed S :case N ...) is a case
that once failed and must now pass; every run includes it.")
  (:export #:run-fuzz #:fuzz-targets #:load-cases))

(in-package #:clempo.fuzz)

;;; Targets and the driver

(defstruct (target (:constructor make-target (name owner doc fn)))
  name owner doc fn)

(defvar *targets* '() "Targets in definition order.")

(defmacro deffuzz (name owner doc &body body)
  "Define fuzz target NAME (a string), owned by agent OWNER. BODY runs one case."
  `(let ((tg (make-target ,name ,owner ,doc (lambda () ,@body))))
     (setf *targets* (append (remove ,name *targets* :key #'target-name :test #'string=)
                             (list tg)))))

(defun fuzz-targets () (mapcar #'target-name *targets*))

(define-condition property-failure (error)
  ((control :initarg :control :reader property-failure-control)
   (message :initarg :message :reader property-failure-message))
  (:report (lambda (c s) (write-string (property-failure-message c) s))))

(defun fail (control &rest args)
  (error 'property-failure :control control :message (apply #'format nil control args)))

(defvar *input* nil "The current case's input, printed with a finding.")

(defvar *tally* (cons 0 0) "(accepted . rejected) hostile inputs of the running target.")

(defmacro rejecting ((&rest condition-types) &body body)
  "BODY's value, or :REJECTED when it signals one of CONDITION-TYPES: the errors a
decoder declares for bad input. Any other condition escapes to the driver."
  `(handler-case (multiple-value-prog1 (progn ,@body) (incf (car *tally*)))
     ((or ,@condition-types) () (incf (cdr *tally*)) :rejected)))

(defun case-seed (target seed case)
  (octets->integer (clempo.keccak:keccak256
                    (string->octets (format nil "clempo-fuzz/~A/~D/~D" target seed case)))
                   :end 8))

(defparameter *case-seconds* 20 "A case running longer than this is a finding.")

(defun run-case (target seed case &key debug)
  "Run one case. Returns (values description signature input): DESCRIPTION is NIL when
the case passes; SIGNATURE groups findings of one kind (the condition type, or the
failed property); INPUT is what the case last recorded in *INPUT*."
  (let ((io:*entropy* (io:make-seeded-entropy (case-seed (target-name target) seed case)))
        (*input* nil))
    (multiple-value-bind (description signature)
        (if debug
            (progn (funcall (target-fn target)) nil)
            (handler-case
                (sb-ext:with-timeout *case-seconds*
                  (funcall (target-fn target))
                  nil)
              (sb-ext:timeout ()
                (values (format nil "timeout after ~D s" *case-seconds*) "timeout"))
              (property-failure (c)
                (values (property-failure-message c) (property-failure-control c)))
              (serious-condition (c)
                (values (format nil "~A: ~A" (type-of c)
                                (handler-case (princ-to-string c)
                                  (serious-condition () "<unprintable>")))
                        (string (type-of c))))))
      (values description signature *input*))))

(defun input-text (input)
  (typecase input
    (null "")
    (string (format nil "~S" (if (> (length input) 400)
                                 (concatenate 'string (subseq input 0 400) "...")
                                 input)))
    (octets (format nil "~A~:[~;...~] (~D bytes)"
                    (octets->hex input :end (min (length input) 256))
                    (> (length input) 256) (length input)))
    (cons (format nil "(~{~A~^ ~})" (mapcar #'input-text input)))
    (t (prin1-to-string input))))

(defun load-cases (path)
  "The entries of tests/fuzz/cases.sexp (see the package documentation)."
  (if (probe-file path)
      (with-open-file (in path) (let ((*read-eval* nil)) (read in)))
      '()))

(defun run-fuzz (&key (targets (fuzz-targets)) (seed 1) cases seconds (case-from 0)
                      (entries '()) (replay t) debug (out *standard-output*))
  "Run CASES cases (or SECONDS seconds, shared among TARGETS) of each target from
CASE-FROM under SEED, then (when REPLAY) the :replay ENTRIES of TARGETS. Each kind of
finding prints its first case in full. Returns (values new known): failing cases not
matched by a :known entry, and those that were."
  (let ((new 0) (known 0))
    (dolist (name targets)
      (let* ((tg (or (find name *targets* :key #'target-name :test #'string=)
                     (error "unknown fuzz target ~S (targets: ~{~A~^ ~})" name (fuzz-targets))))
             (*tally* (cons 0 0))
             (kinds (make-hash-table :test 'equal))
             (deadline (and seconds (+ (io:monotonic-ticks)
                                       (floor (* seconds internal-time-units-per-second)
                                              (length targets)))))
             (replays (and replay
                           (loop for e in entries
                                 when (and (eq (first e) :replay)
                                           (string= (getf (rest e) :target) name))
                                   collect (list (getf (rest e) :seed) (getf (rest e) :case)))))
             (ran 0) (failed 0) (known-here 0)
             (t0 (io:monotonic-ticks)))
        (flet ((one (seed case)
                 (incf ran)
                 (multiple-value-bind (what signature input) (run-case tg seed case :debug debug)
                   (when what
                     (let ((entry (find-if (lambda (e) (and (eq (first e) :known)
                                                            (string= (getf (rest e) :target) name)
                                                            (search (getf (rest e) :match) what)))
                                           entries)))
                       (if entry (incf known-here) (incf failed))
                       (when (= 1 (incf (gethash signature kinds 0)))
                         (format out "~&~:[FINDING~;KNOWN~] ~A seed ~D case ~D (owner ~A): ~A~@[~%  known: ~A~]~%  replay: bin/clempo-dev fuzz -target ~A -seed ~D -case ~D -debug~%  input: ~A~%"
                                 entry name seed case (target-owner tg) what
                                 (and entry (getf (rest entry) :note))
                                 name seed case (input-text input))))))))
          (loop for case from case-from
                while (if cases (< (- case case-from) cases) (< (io:monotonic-ticks) deadline))
                do (one seed case))
          (loop for (s c) in replays do (one s c)))
        (incf new failed)
        (incf known known-here)
        (format out "~&~A: ~D cases (seed ~D from case ~D, ~D replayed), hostile inputs ~D accepted / ~D rejected, ~D new + ~D known failing in ~D kind~:P, ~,2F s~%"
                name ran seed case-from (length replays) (car *tally*) (cdr *tally*)
                failed known-here (hash-table-count kinds)
                (/ (- (io:monotonic-ticks) t0) internal-time-units-per-second))))
    (values new known)))

;;; Random draws, all from io:*entropy*

(defun rnd (n) (if (<= n 1) 0 (io:random-below n)))
(defun chance (num den) (< (rnd den) num))
(defun pick (seq) (elt seq (rnd (length seq))))
(defun rbytes (n) (if (zerop n) (make-octets 0) (io:random-octets n)))

(defun rsize (max)
  "A length up to MAX, biased to the edges that matter for length-prefixed formats."
  (min max (case (rnd 4)
             (0 (rnd 4))
             (1 (pick '(0 1 2 31 32 33 55 56 57 64 255 256)))
             (t (rnd (1+ max))))))

(defun rint (bits)
  "An integer below 2^BITS biased to 0, 1, powers of two and their neighbours, the top."
  (let ((top (ash 1 bits)))
    (case (rnd 8)
      (0 0) (1 1) (2 (1- top)) (3 (ash top -1)) (4 (1- (ash top -1)))
      (5 (let ((b (rnd bits))) (mod (+ (ash 1 b) (1- (rnd 3))) top)))
      (6 (rnd 256))
      (t (rnd top)))))

(defun mutate (o &optional (rounds (1+ (rnd 4))))
  "O with ROUNDS random byte-level edits."
  (let ((v (copy-seq o)))
    (dotimes (i rounds v)
      (let ((n (length v)))
        (case (rnd (if (zerop n) 2 9))
          (0 (setf v (concatenate 'octets (subseq v 0 (rnd (1+ n))) (rbytes (1+ (rnd 8)))
                                  (subseq v (rnd (1+ n))))))
          (1 (setf v (concatenate 'octets v (rbytes (1+ (rnd 4))))))
          (2 (setf (aref v (rnd n)) (logxor (aref v (rnd n)) (ash 1 (rnd 8)))))
          (3 (setf (aref v (rnd n)) (pick '(0 1 #x7f #x80 #x81 #xb7 #xb8 #xbf #xc0 #xf7 #xf8 #xff))))
          (4 (setf (aref v (rnd n)) (rnd 256)))
          (5 (setf v (subseq v 0 (rnd n))))
          (6 (let* ((a (rnd n)) (b (+ a (rnd (- n a)))))
               (setf v (concatenate 'octets (subseq v 0 a) (subseq v b)))))
          (7 (let* ((a (rnd n)) (b (+ a (rnd (1+ (- n a))))))
               (setf v (concatenate 'octets (subseq v 0 b) (subseq v a)))))
          (t (setf v (subseq v (rnd n)))))))))

;;; RLP

(defun gen-rlp-item (depth)
  (if (and (plusp depth) (chance 1 3))
      (loop repeat (rnd 6) collect (gen-rlp-item (1- depth)))
      (rbytes (if (chance 1 30) (+ 1000 (rnd 3000)) (rsize 80)))))

(defun deep-rlp-list (n)
  "N nested lists around an empty list, each header the canonical one."
  (let ((headers '()) (len 1))
    (dotimes (i n)
      (let ((h (if (<= len 55)
                   (octets-of (+ #xc0 len))
                   (let ((l (integer->octets-minimal len)))
                     (concatenate 'octets (octets-of (+ #xf7 (length l))) l)))))
        (push h headers)
        (incf len (length h))))
    (let ((out (make-octets len)) (pos 0))
      (dolist (h headers)
        (replace out h :start1 pos)
        (incf pos (length h)))
      (setf (aref out pos) #xc0)
      out)))

(defun deep-depth () (pick '(16 200 2000 12000 50000)))

(deffuzz "rlp" "SecurityAgent"
  "encode/decode round trip, canonical re-encoding of anything accepted, deep nesting"
  (case (rnd 4)
    (0 (let* ((item (gen-rlp-item 4))
              (enc (rlp:rlp-encode item)))
         (setf *input* enc)
         (unless (= (length enc) (rlp:rlp-encoded-length item))
           (fail "rlp-encoded-length ~D but encoding has ~D bytes"
                 (rlp:rlp-encoded-length item) (length enc)))
         (unless (equalp (rlp:rlp-decode enc) item) (fail "decode of encode differs"))
         (unless (eq :rejected (rejecting (rlp:rlp-error)
                                 (rlp:rlp-decode (concatenate 'octets enc (octets-of 0)))))
           (fail "trailing byte accepted"))))
    (1 (let ((enc (deep-rlp-list (deep-depth))))
         (setf *input* enc)
         (let ((v (rejecting (rlp:rlp-error) (rlp:rlp-decode enc))))
           (unless (or (eq v :rejected) (equalp (rlp:rlp-encode v) enc))
             (fail "deep list re-encodes differently")))))
    (2 (let ((o (rbytes (rsize 40))))
         (setf *input* o)
         (let ((v (rejecting (rlp:rlp-error) (rlp:rlp-decode-integer o))))
           (unless (or (eq v :rejected) (equalp (integer->octets-minimal v) o))
             (fail "integer ~D accepted from a non-canonical encoding" v)))))
    (t (let ((o (if (chance 1 4)
                    (rbytes (rsize 100))
                    (mutate (rlp:rlp-encode (gen-rlp-item 3))))))
         (setf *input* o)
         (let ((v (rejecting (rlp:rlp-error) (rlp:rlp-decode o))))
           (unless (or (eq v :rejected) (equalp (rlp:rlp-encode v) o))
             (fail "accepted input is not canonical: re-encodes as ~A"
                   (octets->hex (rlp:rlp-encode v)))))))))

;;; JSON

(defun gen-json-string ()
  (coerce (loop repeat (rsize 20)
                collect (case (rnd 6)
                          (0 (code-char (rnd 32)))
                          (1 (pick '(#\" #\\ #\/ #\u)))
                          (2 (code-char (pick '(#xe9 #x4e2d #x1F600 #xfffd #x7f #x80))))
                          (t (code-char (+ 32 (rnd 95))))))
          'string))

(defun gen-json (depth)
  (case (rnd (if (plusp depth) 9 7))
    (0 (pick '(:true :false :null)))
    (1 (- (rint 64)))
    (2 (rint (pick '(8 64 256 1000))))
    (3 (* (if (chance 1 2) 1 -1) (coerce (rnd 1000000) 'double-float)
          (expt 10d0 (- (rnd 600) 300))))
    ((4 5 6) (gen-json-string))
    (7 (coerce (loop repeat (rnd 5) collect (gen-json (1- depth))) 'simple-vector))
    ;; distinct keys: with duplicates JSON-EQUAL's comparison is not defined
    (t (cons :object (remove-duplicates
                      (loop repeat (rnd 5) collect (cons (gen-json-string) (gen-json (1- depth))))
                      :key #'car :test #'string=)))))

(defun json-mutate (s)
  (let ((v (coerce s 'list)))
    (dotimes (i (1+ (rnd 4)) (coerce v 'string))
      (let ((n (length v))
            (ins (case (rnd 3)
                   (0 (coerce (pick '("\\" "\\u" "\\ud800" "\\udbff\\uffff" "\\uZZZZ" "\\u-001"
                                      "\\u+7f" "1e999" "-" "+1" "1.2.3" "0x10" "01" "1e"
                                      "{" "}" "[" "]" "\"" ":" "," "tru" "nul" "-0" ".5"))
                              'list))
                   (1 (list (code-char (rnd 256))))
                   (t (list (pick "{}[]\":,\\-+.eE0123456789tfnu "))))))
        (case (rnd 3)
          (0 (let ((p (rnd (1+ n)))) (setf v (append (subseq v 0 p) ins (nthcdr p v)))))
          (1 (when (plusp n) (setf v (subseq v 0 (rnd n)))))
          (t (when (plusp n) (let ((p (rnd n))) (setf v (append (subseq v 0 p) (nthcdr (1+ p) v)))))))))))

(defun check-json-round-trip (v &key (equal t))
  "V's text parses, and writes back as the same text. EQUAL also compares the parsed value
with V (only for generated values: a parsed object may repeat a key)."
  (let ((text (json:to-json-string v)))
    (let ((back (rejecting (json:json-error) (json:parse-json text))))
      (when (eq back :rejected) (fail "writer output does not parse: ~A" (input-text text)))
      (let ((again (json:to-json-string back)))
        (unless (string= again text) (fail "rewrite differs: ~A" (input-text again))))
      (when (and equal (not (json:json-equal back v)))
        (fail "round trip differs: ~A" (input-text text))))))

(deffuzz "json" "SecurityAgent"
  "hostile text and octets: only JSON-ERROR, and whatever parses writes back equal"
  (case (rnd 5)
    (0 (let ((v (gen-json 4)))
         (setf *input* (json:to-json-string v))
         (check-json-round-trip v)))
    (1 (let* ((n (deep-depth))
              (s (if (chance 1 2)
                     (make-string n :initial-element #\[)
                     (format nil "~{~A~}1~{~A~}" (make-list n :initial-element "{\"a\":[")
                             (make-list n :initial-element "]}")))))
         (setf *input* s)
         (let ((v (rejecting (json:json-error) (json:parse-json s))))
           (unless (eq v :rejected) (check-json-round-trip v :equal nil)))))
    (2 (let ((o (if (chance 1 2)
                    (rbytes (rsize 64))
                    (mutate (string->octets (json:to-json-string (gen-json 2)))))))
         (setf *input* o)
         (let ((v (rejecting (json:json-error) (json:parse-json o))))
           (unless (eq v :rejected) (check-json-round-trip v :equal nil)))))
    (t (let ((s (json-mutate (json:to-json-string (gen-json 3)))))
         (setf *input* s)
         (let ((v (rejecting (json:json-error) (json:parse-json s))))
           (unless (eq v :rejected) (check-json-round-trip v :equal nil)))))))

;;; ABI

(defun gen-abi-type (depth)
  (case (rnd (if (plusp depth) 10 7))
    (0 :address) (1 :bool) (2 (list :uint (* 8 (1+ (rnd 32)))))
    (3 (list :int (* 8 (1+ (rnd 32))))) (4 (list :bytes-n (1+ (rnd 32))))
    (5 (pick '(:bytes :string))) (6 (list :enum (1+ (rnd 255))))
    (7 (list :array (gen-abi-type (1- depth))))
    (8 (list :array (gen-abi-type (1- depth)) (1+ (rnd 3))))
    (t (cons :tuple (loop repeat (1+ (rnd 3)) collect (gen-abi-type (1- depth)))))))

(defun gen-abi-value (type)
  (etypecase type
    (keyword (ecase type
               (:address (rint 160))
               (:bool (chance 1 2))
               (:bytes (rbytes (rsize 70)))
               (:string (coerce (loop repeat (rsize 40)
                                      collect (code-char (pick '(#x41 #x7a #x00 #xe9 #x4e2d #x1F600 #x20))))
                                'string))))
    (cons (ecase (first type)
            (:uint (rint (second type)))
            (:int (- (rint (second type)) (ash 1 (1- (second type)))))
            (:bytes-n (rint (* 8 (second type))))
            (:enum (rnd (second type)))
            (:array (loop repeat (or (third type) (rnd 4)) collect (gen-abi-value (second type))))
            (:tuple (mapcar #'gen-abi-value (rest type)))))))

(defun abi-value-equal (type a b)
  (etypecase type
    (keyword (ecase type
               ((:address) (eql a b))
               (:bool (eq (not a) (not b)))
               (:bytes (equalp a b))
               (:string (and (stringp b) (string= a b)))))
    (cons (ecase (first type)
            ((:uint :int :bytes-n :enum) (eql a b))
            (:array (and (listp b) (= (length a) (length b))
                         (every (lambda (x y) (abi-value-equal (second type) x y)) a b)))
            (:tuple (and (listp b) (= (length a) (length b))
                         (every #'abi-value-equal (rest type) a b)))))))

(defun abi-word-mutate (o)
  "Overwrite whole words with offsets, lengths and extremes; byte edits too."
  (let ((v (copy-seq o)))
    (dotimes (i (1+ (rnd 3)) v)
      (if (and (>= (length v) 32) (chance 2 3))
          (let ((w (* 32 (rnd (floor (length v) 32))))
                (x (case (rnd 6)
                     (0 (* 32 (rnd 8))) (1 (length v)) (2 (rnd 8)) (3 (1- (ash 1 256)))
                     (4 (ash 1 (rnd 256))) (t (rint 256)))))
            (replace v (integer->octets x 32) :start1 w))
          (setf v (mutate v 1))))))

(deffuzz "abi" "SecurityAgent"
  "encode/decode round trip (strict and loose), hostile data: only ABI-ERROR, and a strict
accept is canonical"
  (let* ((types (loop repeat (1+ (rnd 3)) collect (gen-abi-type 3)))
         (vals (mapcar #'gen-abi-value types))
         (enc (abi:abi-encode types vals)))
    (setf *input* (list (mapcar #'abi:abi-type-string types) enc))
    (if (chance 1 3)
        (dolist (strict '(nil t))
          (let ((back (abi:abi-decode types enc :strict strict :validate t)))
            (unless (every #'abi-value-equal types vals back)
              (fail "~:[loose~;strict~] decode of the encoding differs" strict))))
        (let* ((data (abi-word-mutate enc))
               (strict (chance 1 2))
               (validate (chance 1 2)))
          (setf *input* (list (mapcar #'abi:abi-type-string types) strict validate data))
          (let ((back (rejecting (abi:abi-error)
                        (abi:abi-decode types data :strict strict :validate validate
                                                   :memory-limit (ash 1 20)))))
            (when (and strict (not (eq back :rejected)))
              (let ((again (abi:abi-encode types back)))
                (unless (equalp again data)
                  (fail "strict decode accepted a non-canonical encoding (canonical ~A)"
                        (input-text again))))))))))

;;; 256-bit EVM arithmetic against a slow reference model

(define-constant +m+ (ash 1 256))

(defun sgn (x) (if (logbitp 255 x) (- x +m+) x))
(defun wrap (x) (mod x +m+))

(defun ref-pow (b e m)
  (cond ((zerop e) (mod 1 m))
        ((evenp e) (ref-pow (mod (* b b) m) (floor e 2) m))
        (t (mod (* b (ref-pow b (1- e) m)) m))))

(defparameter *word-ops*
  ;; name opcode arity reference
  `((:add #x01 2 ,(lambda (a b) (wrap (+ a b))))
    (:mul #x02 2 ,(lambda (a b) (wrap (* a b))))
    (:sub #x03 2 ,(lambda (a b) (wrap (- a b))))
    (:div #x04 2 ,(lambda (a b) (if (zerop b) 0 (values (floor a b)))))
    (:sdiv #x05 2 ,(lambda (a b) (if (zerop b) 0 (wrap (truncate (sgn a) (sgn b))))))
    (:mod #x06 2 ,(lambda (a b) (if (zerop b) 0 (mod a b))))
    (:smod #x07 2 ,(lambda (a b) (if (zerop b) 0
                                     (wrap (* (signum (sgn a)) (mod (abs (sgn a)) (abs (sgn b))))))))
    (:addmod #x08 3 ,(lambda (a b n) (if (zerop n) 0 (mod (+ a b) n))))
    (:mulmod #x09 3 ,(lambda (a b n) (if (zerop n) 0 (mod (* a b) n))))
    (:exp #x0a 2 ,(lambda (a b) (ref-pow a b +m+)))
    (:signextend #x0b 2 ,(lambda (k x)
                           (if (>= k 31) x
                               (let* ((bits (* 8 (1+ k))) (v (mod x (ash 1 bits))))
                                 (wrap (if (>= v (ash 1 (1- bits))) (- v (ash 1 bits)) v))))))
    (:lt #x10 2 ,(lambda (a b) (if (< a b) 1 0)))
    (:gt #x11 2 ,(lambda (a b) (if (> a b) 1 0)))
    (:slt #x12 2 ,(lambda (a b) (if (< (sgn a) (sgn b)) 1 0)))
    (:sgt #x13 2 ,(lambda (a b) (if (> (sgn a) (sgn b)) 1 0)))
    (:eq #x14 2 ,(lambda (a b) (if (= a b) 1 0)))
    (:iszero #x15 1 ,(lambda (a) (if (zerop a) 1 0)))
    (:and #x16 2 ,(lambda (a b) (logand a b)))
    (:or #x17 2 ,(lambda (a b) (logior a b)))
    (:xor #x18 2 ,(lambda (a b) (logxor a b)))
    (:not #x19 1 ,(lambda (a) (- +m+ 1 a)))
    (:byte #x1a 2 ,(lambda (i x) (if (< i 32) (mod (floor x (expt 256 (- 31 i))) 256) 0)))
    (:shl #x1b 2 ,(lambda (n x) (if (< n 256) (wrap (* x (expt 2 n))) 0)))
    (:shr #x1c 2 ,(lambda (n x) (if (< n 256) (values (floor x (expt 2 n))) 0)))
    (:sar #x1d 2 ,(lambda (n x) (if (< n 256) (wrap (floor (sgn x) (expt 2 n)))
                                    (if (minusp (sgn x)) (1- +m+) 0))))
    (:clz #x1e 1 ,(lambda (a) (- 256 (integer-length a))))))

(defun push32 (x) (concatenate 'octets (octets-of #x7f) (integer->octets x 32)))

(defun run-code (accounts &key (to #xc0de) (gas 100000) (fork :osaka) (input (make-octets 0)) jit)
  "Call TO in a world of ACCOUNTS, a list of (address code); the observable outcome."
  (let ((b (make-memory-backend)))
    (memory-backend-put-account b #xca11 :balance (expt 10 18))
    (loop for (address code) in accounts
          do (memory-backend-put-account b address :code code :balance 1000))
    (let* ((world (make-world b))
           (*world* world)
           (*spec* (make-spec fork))
           (*block* (make-block-env :number 1 :gas-limit 30000000))
           (*jit* (and jit #'jit-run-frame))
           (*jit-compiler* (and jit #'jit-compile-synchronously))
           (*jit-max-code-size* most-positive-fixnum)
           (*jit-threshold* 0))
      (multiple-value-bind (status gas-left output)
          (call-message :call #xca11 to to 0 input gas 0 nil)
        (let ((refund (world-refund world))
              (logs (mapcar (lambda (l) (list (log-entry-address l) (log-entry-topics l)
                                              (log-entry-data l)))
                            (world-logs world))))
          (finalize-tx world)
          (apply-changes b (drain-changes world))
          (list status gas-left output refund logs
                (sort (loop for address being the hash-keys of (memory-backend-accounts b)
                              using (hash-value a)
                            collect (list address (mem-account-nonce a) (mem-account-balance a)
                                          (octets->hex (mem-account-code-hash a))
                                          (sort (loop for k being the hash-keys of (mem-account-storage a)
                                                        using (hash-value v)
                                                      collect (cons k v))
                                                #'< :key #'car)))
                      #'< :key #'first)))))))

(defun jit-installs () (+ (jit-stats-compiled *jit-stats*) (jit-stats-shared *jit-stats*)))

(defun jit-run-code (accounts &rest keys)
  "RUN-CODE compiled; fails unless some code was actually compiled (or taken from the
cache), so an API change that leaves the JIT off cannot make a differential vacuous."
  (let* ((failed (jit-stats-failed *jit-stats*))
         (installs (jit-installs))
         (outcome (apply #'run-code accounts :jit t keys)))
    (unless (= failed (jit-stats-failed *jit-stats*))
      (fail "JIT compiler failed: ~A" (third (first (jit-stats-failures *jit-stats*)))))
    (when (= installs (jit-installs)) (fail "the JIT run compiled nothing"))
    outcome))

(defun jit-differ (accounts &rest keys)
  "Run interpreted and compiled; fail when they differ or the JIT compiler failed."
  (let ((want (apply #'run-code accounts keys))
        (got (apply #'jit-run-code accounts keys)))
    (unless (equalp want got)
      (fail "interpreter ~S~%  JIT ~S" (subseq want 0 3) (subseq got 0 3)))
    want))

(deffuzz "u256" "EvmJitAgent"
  "every 256-bit arithmetic, comparison, bitwise and shift opcode against a slow reference
model, interpreted and compiled"
  (let* ((cases (loop repeat 8
                      collect (let* ((op (pick *word-ops*))
                                     (args (loop repeat (third op)
                                                 collect (if (and (chance 1 3)
                                                                  (member (first op) '(:byte :shl :shr :sar :signextend)))
                                                             (rnd 300)
                                                             (rint 256)))))
                                (list op args))))
         (code (apply #'octets-concat
                      (append
                       (loop for (op args) in cases
                             for slot from 0
                             append (append (mapcar #'push32 (reverse args))
                                            (list (octets-of (second op) #x61 0 (* 32 slot) #x52))))
                       (list (octets-of #x61 1 0 #x60 0 #xf3)))))
         (want (apply #'octets-concat
                      (loop for (op args) in cases
                            collect (integer->octets (apply (fourth op) args) 32)))))
    (setf *input* (mapcar (lambda (c) (list (first (first c)) (second c))) cases))
    (dolist (jit '(nil t))
      (destructuring-bind (status gas-left output &rest more)
          (if jit
              (jit-run-code `((#xc0de ,code)) :gas 1000000)
              (run-code `((#xc0de ,code)) :gas 1000000))
        (declare (ignore gas-left more))
        (unless (eq status :return) (fail "~:[interpreter~;JIT~] halted with ~S" jit status))
        (loop for i from 0 below (length cases)
              for (op args) in cases
              unless (equalp (subseq output (* 32 i) (* 32 (1+ i))) (subseq want (* 32 i) (* 32 (1+ i))))
                do (fail "~:[interpreter~;JIT~] ~(~A~)~{ #x~X~}: got #x~X, reference #x~X"
                         jit (first op) args
                         (octets->integer output :start (* 32 i) :end (* 32 (1+ i)))
                         (octets->integer want :start (* 32 i) :end (* 32 (1+ i)))))))))

;;; Interpreter against JIT on random bytecode

(defun random-program (n)
  "N instructions of random defined opcodes; pushes mostly small, jumps mostly to a
JUMPDEST of the program."
  (let ((ops (loop for i from 0 below 256 when (op-info i) collect i))
        (items '()) (dests '()) (pc 0))
    (dotimes (i n)
      (let ((op (case (rnd 6)
                  (0 #x5b)
                  (1 (pick '(#x60 #x61 #x7f)))
                  (2 (pick '(#x56 #x57)))
                  (t (pick ops)))))
        (when (= op #x5b) (push pc dests))
        (cond ((<= #x60 op #x7f)
               (let* ((len (- op #x5f))
                      (v (if (chance 3 4) (rnd (pick '(4 32 256 #xc0e0))) (rint (* 8 len)))))
                 (push (concatenate 'octets (octets-of op) (integer->octets (ldb (byte (* 8 len) 0) v) len))
                       items)
                 (incf pc (1+ len))))
              ((and (member op '(#x56 #x57)) (chance 3 4))
               (push (list :jump op) items)
               (incf pc 4))
              (t (push (octets-of op) items) (incf pc)))))
    (apply #'octets-concat
           (mapcar (lambda (item)
                     (if (octets-p item)
                         item
                         (let ((target (if dests (pick dests) (rnd pc))))
                           (octets-of #x61 (ldb (byte 8 8) target) (ldb (byte 8 0) target)
                                      (second item)))))
                   (nreverse items)))))

(defun octets-p (x) (typep x 'octets))

(deffuzz "evm-jit" "EvmJitAgent"
  "random bytecode (two contracts calling each other) interpreted and compiled agree on
status, gas, output, refund, logs and state"
  (let* ((fork (pick '(:istanbul :berlin :london :shanghai :cancun :prague :osaka :osaka)))
         (a (random-program (1+ (rnd 60))))
         (b (random-program (1+ (rnd 30))))
         (gas (case (rnd 3) (0 (rnd 200)) (1 (rnd 30000)) (t (rnd 300000))))
         (input (rbytes (rsize 64))))
    (setf *input* (list fork gas a b input))
    (jit-differ `((#xc0de ,a) (#xc0e0 ,b)) :gas gas :fork fork :input input)))

;;; MODEXP

(defun naive-modexp (b e m)
  (cond ((zerop m) 0)
        (t (let ((r 1) (x (mod b m)))
             (loop for i from 0 below (integer-length e)
                   do (when (logbitp i e) (setf r (mod (* r x) m)))
                      (setf x (mod (* x x) m)))
             (mod r m)))))

(deffuzz "modexp" "SecurityAgent"
  "modular exponentiation against right-to-left square-and-multiply, and the precompile on
hostile length headers"
  (let ((bl (rsize 96)) (el (rsize 64)) (ml (rsize 96)))
    (let ((base (rbytes bl)) (exp (rbytes el)) (modulus (rbytes ml)))
      (when (and (plusp ml) (chance 1 4)) (setf (aref modulus (1- ml)) (logand (aref modulus (1- ml)) #xfe)))
      (when (and (plusp ml) (chance 1 8)) (fill modulus 0) (setf (aref modulus (1- ml)) 1))
      (setf *input* (list base exp modulus))
      (let ((got (clempo.modexp:modexp base exp modulus))
            (want (integer->octets (naive-modexp (octets->integer base) (octets->integer exp)
                                                 (octets->integer modulus))
                                   ml)))
        (unless (equalp got want) (fail "modexp ~A, naive ~A" (octets->hex got) (octets->hex want))))
      ;; the precompile: hostile declared lengths with whatever data follows
      (let* ((lens (list (if (chance 1 4) (rint 256) bl) (if (chance 1 4) (rint 256) el)
                         (if (chance 1 4) (rint 256) ml)))
             (input (apply #'octets-concat (append (mapcar (lambda (x) (integer->octets x 32)) lens)
                                                  (list base exp modulus))))
             (fork (pick '(:istanbul :berlin :osaka)))
             (gas (pick '(100000 10000000))))
        (setf *input* (list fork gas input))
        (destructuring-bind (status gas-left output &rest more)
            (run-code '() :to 5 :input input :fork fork :gas gas)
          (declare (ignore gas-left more))
          (when (and (eq status :return) (equal lens (list bl el ml)))
            (let ((want (integer->octets (naive-modexp (octets->integer base) (octets->integer exp)
                                                       (octets->integer modulus))
                                         ml)))
              (unless (equalp output want)
                (fail "precompile returned ~A, naive ~A" (octets->hex output) (octets->hex want))))))))))

;;; Snappy against a slow reference decoder

(defun ref-snappy (data max)
  "Decode a raw Snappy block, or :ERROR."
  (block ref
    (let ((p 0) (n (length data)) (size 0))
      (loop for shift from 0 by 7
            do (when (or (>= p n) (> shift 28)) (return-from ref :error))
               (let ((b (aref data p)))
                 (incf p)
                 (setf size (logior size (ash (logand b 127) shift)))
                 (when (< b 128) (return))))
      (when (or (> size #xffffffff) (> size max)) (return-from ref :error))
      (let ((out (make-octets size)) (q 0))
        (loop while (< p n)
              do (let ((tag (aref data p)))
                   (incf p)
                   (if (zerop (logand tag 3))
                       (let ((len (ash tag -2)))
                         (when (>= len 60)
                           (let ((extra (- len 59)))
                             (when (> (+ p extra) n) (return-from ref :error))
                             (setf len (loop for i from 0 below extra
                                             sum (ash (aref data (+ p i)) (* 8 i))))
                             (incf p extra)))
                         (incf len)
                         (when (or (> (+ p len) n) (> (+ q len) size)) (return-from ref :error))
                         (replace out data :start1 q :start2 p :end2 (+ p len))
                         (incf p len) (incf q len))
                       (let* ((kind (logand tag 3))
                              (extra (ecase kind (1 1) (2 2) (3 4)))
                              (len (if (= kind 1) (+ 4 (ldb (byte 3 2) tag)) (1+ (ash tag -2)))))
                         (when (> (+ p extra) n) (return-from ref :error))
                         (let ((offset (if (= kind 1)
                                           (logior (ash (ash tag -5) 8) (aref data p))
                                           (loop for i from 0 below extra
                                                 sum (ash (aref data (+ p i)) (* 8 i))))))
                           (incf p extra)
                           (when (or (zerop offset) (> offset q) (> (+ q len) size))
                             (return-from ref :error))
                           (dotimes (i len) (setf (aref out (+ q i)) (aref out (+ (- q offset) i))))
                           (incf q len))))))
        (if (= q size) out :error)))))

(deffuzz "snappy" "SecurityAgent"
  "compress/decompress round trip; hostile blocks decode exactly as a slow reference decoder
does (or both reject), only P2P-ERROR"
  (let* ((plain (if (chance 1 2)
                    (rbytes (rsize 300))
                    (let ((unit (rbytes (1+ (rnd 8)))))
                      (apply #'octets-concat (loop repeat (rnd 200) collect unit)))))
         (packed (snappy:compress plain)))
    (setf *input* plain)
    (unless (equalp (snappy:decompress packed) plain) (fail "round trip differs"))
    (let* ((data (if (chance 1 5) (rbytes (rsize 40)) (mutate packed)))
           (max (pick '(#xffffff 64 0))))
      (setf *input* (list max data))
      (let ((got (rejecting (clempo.p2p-wire:p2p-error) (snappy:decompress data :max-size max)))
            (want (ref-snappy data max)))
        (unless (if (eq got :rejected) (eq want :error) (equalp got want))
          (fail "decompress ~A, reference ~A"
                (if (eq got :rejected) "rejected" (input-text got))
                (if (eq want :error) "rejects" (input-text want))))))))

;;; Transactions

(defvar *tx-seeds* nil)

(defun tx-seeds ()
  (or *tx-seeds*
      (setf *tx-seeds*
            (with-open-file (in (asdf:system-relative-pathname
                                 "clempo-dev" "../../tests/types/data/transaction-rpc.sexp"))
              (let ((*read-eval* nil))
                (mapcar (lambda (row) (hex->octets (second row))) (read in)))))))

(defun mutate-tree (item)
  "Change one leaf or list of a decoded RLP tree."
  (labels ((walk (x)
             (cond ((and (listp x) (chance 1 6))
                    (case (rnd 4)
                      (0 (butlast x)) (1 (append x (list (rbytes (rsize 33)))))
                      (2 '()) (t (list x))))
                   ((listp x)
                    (if (null x) x
                        (let ((i (rnd (length x))))
                          (loop for y in x for j from 0 collect (if (= i j) (walk y) y)))))
                   (t (case (rnd 6)
                        (0 (make-octets 0)) (1 (octets-of 0)) (2 (rbytes (rsize 70)))
                        (3 (concatenate 'octets (octets-of 0) x))
                        (4 (if (plusp (length x)) (subseq x 1) x))
                        (t (list x)))))))
    (walk item)))

(deffuzz "tx" "SecurityAgent"
  "mutated mainnet/moderato envelopes of every type: only RLP-ERROR, INVARIANT-VIOLATION or
SIGNATURE-ERROR; whatever decodes re-encodes byte for byte and its sender is computable
or rejected"
  (let* ((seed (pick (tx-seeds)))
         (raw (if (chance 1 2)
                  (mutate seed)
                  (let* ((typed (< (aref seed 0) #xc0))
                         (fields (rlp:rlp-decode seed :start (if typed 1 0)))
                         (enc (rlp:rlp-encode (mutate-tree fields))))
                    (if typed
                        (concatenate 'octets (octets-of (if (chance 1 10) (rnd 256) (aref seed 0))) enc)
                        enc)))))
    (setf *input* raw)
    (when (plusp (length raw))
      (let ((tx (rejecting (rlp:rlp-error invariant-violation clempo.signatures:signature-error)
                  (tx:decode-transaction raw))))
        (unless (eq tx :rejected)
          (let ((again (tx:encode-transaction tx)))
            (unless (equalp again raw)
              (fail "decoded transaction re-encodes as ~A" (input-text again))))
          (tx:transaction-hash tx)
          (rejecting (rlp:rlp-error invariant-violation clempo.signatures:signature-error)
            (tx:transaction-sender tx)))))))

;;; The command

(in-package #:clempo.dev)

(defcommand "fuzz" (args)
  "seeded fuzzing: fuzz [-target T[,T...]|all] [-seed S] [-cases N | -seconds N] [-from N] [-case N -debug]"
  (let* ((target (arg args :target "all"))
         (targets (if (string= target "all")
                      (clempo.fuzz:fuzz-targets)
                      (loop for start = 0 then (1+ comma)
                            for comma = (position #\, target :start start)
                            collect (subseq target start comma)
                            while comma)))
         (single (arg-int args :case))
         (seconds (and (not single) (arg args :seconds) (arg-int args :seconds))))
    (multiple-value-bind (new known)
        (clempo.fuzz:run-fuzz :targets targets :seed (arg-int args :seed 1)
                              :cases (cond (single 1) (seconds nil) (t (arg-int args :cases 100)))
                              :seconds seconds
                              :case-from (or single (arg-int args :from 0))
                              :entries (clempo.fuzz:load-cases
                                        (asdf:system-relative-pathname
                                         "clempo-dev" "../../tests/fuzz/cases.sexp"))
                              :replay (not single)
                              :debug (arg-flag args :debug))
      (format t "~&fuzz: ~D new failing case~:P, ~D known~%" new known)
      (if (plusp new) 1 0))))
