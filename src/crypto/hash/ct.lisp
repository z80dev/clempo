;;; Constant-time arithmetic for secret operands: Montgomery fields over fixed
;;; word counts, a quadratic extension, and complete projective short-Weierstrass
;;; groups with fixed-window and comb multiplication.
;;;
;;; The rules every function here keeps: no branch, early exit or array index
;;; depends on a secret word; conditional moves use all-ones/all-zeros masks; a
;;; table lookup reads every entry. Loop bounds and exponents are public. A secret
;;; choice crosses a function boundary as a bit, never as a mask word: SBCL boxes an
;;; all-ones (unsigned-byte 64) into a bignum, and that allocation shows in the timing
;;; (ct-timing caught it). Elements live in word arenas at word offsets; outputs may
;;; alias inputs exactly.
(defpackage #:clempo.ct
  (:use #:cl)
  (:export #:with-data-independent-timing #:words #:make-words
           #:+digits+ #:digits #:make-digits #:recode-odd! #:bit-mask #:equal-mask #:nonzero-bit
           #:words-from-octets! #:words-to-octets! #:wipe #:with-wiped
           #:hex-to-octets #:octets-to-hex
           #:define-ct-field #:define-ct-quadratic-field #:define-ct-curve))
(in-package #:clempo.ct)

;;; Arm's PSTATE.DIT bit (FEAT_DIT) promises that the listed integer instructions take
;;; data-independent time. Without it an Apple M3 multiplies faster or slower depending
;;; on the operands: ct-timing measured |t| near 90 for one Montgomery multiplication on
;;; sparse versus random words, and none with DIT set. Every secret-key entry point runs
;;; under WITH-DATA-INDEPENDENT-TIMING, which sets the bit and restores it on exit.
#+arm64
(progn
  (sb-ext:defglobal **dit** nil)
  (defun detect-dit () (setf **dit** (clempo.io:cpu-feature-p :arm64-dit)))
  (detect-dit)
  (pushnew 'detect-dit sb-ext:*init-hooks*))

;;; MSR DIT, #imm (Arm DDI 0602: op1 = 3, op2 = 2, CRm = imm) and MRS Xt, DIT
;;; (S3_3_C4_C2_5). Backend symbols are found by name, as in sha256-arm64.lisp.
#+arm64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %set-dit ((integer 0 1)) (values) () :overwrite-fndb-silently t)
  (sb-c:define-vop (set-dit)
    (:translate %set-dit)
    (:policy :fast-safe)
    (:info on)
    (:arg-types (:constant (integer 0 1)))
    (:generator 1
      (let ((opcode (if (= on 1) #xd503415f #xd503405f)))
        (dotimes (i 4)
          (sb-assem:inst #.(find-symbol "BYTE" "SB-ARM64-ASM") (ldb (byte 8 (* 8 i)) opcode))))))
  (sb-c:defknown %read-dit () (unsigned-byte 64) () :overwrite-fndb-silently t)
  (sb-c:define-vop (read-dit)
    (:translate %read-dit)
    (:policy :fast-safe)
    (:results (r :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM"))))
    (:result-types #.(find-symbol "UNSIGNED-NUM" "SB-VM"))
    (:generator 1
      (let ((opcode (logior #xd53b42a0 (sb-c:tn-offset r))))
        (dotimes (i 4)
          (sb-assem:inst #.(find-symbol "BYTE" "SB-ARM64-ASM") (ldb (byte 8 (* 8 i)) opcode)))))))

#+arm64
(defun call-with-data-independent-timing (thunk)
  (declare (type function thunk))
  (if (and **dit** (not (logbitp 24 (%read-dit))))
      (progn (%set-dit 1)
             (unwind-protect (funcall thunk) (%set-dit 0)))
      (funcall thunk)))

(defmacro with-data-independent-timing (&body body)
  "Run BODY with the CPU's data-independent timing mode on (Arm PSTATE.DIT where the
CPU has FEAT_DIT; a no-op elsewhere), restoring the previous mode afterwards."
  #+arm64 `(flet ((body () ,@body))
             (declare (dynamic-extent #'body))
             (call-with-data-independent-timing #'body))
  #-arm64 `(progn ,@body))

(deftype word () '(unsigned-byte 64))
(deftype words () '(simple-array (unsigned-byte 64) (*)))

(defun make-words (n)
  (make-array n :element-type '(unsigned-byte 64) :initial-element 0))

(declaim (inline mac bit-mask nonzero-bit equal-mask))
(defun mac (a b c d)
  "high, low of a*b+c+d."
  (declare #.clempo.base:*hot* (type word a b c d))
  ;; SBCL 2.6.8 ARM64's four-argument multiply-add can alias Y with HI.
  (multiple-value-bind (hi lo) (clempo.ec.word:multiply-and-add a b c)
    (multiple-value-bind (low carry) (clempo.ec.word:add-with-carry lo d 0)
      (values (ldb (byte 64 0) (+ hi carry)) low))))

(defun bit-mask (bit)
  "All ones for 1, zero for 0: the word 0 - BIT."
  (declare #.clempo.base:*hot* (type bit bit))
  ;; Through the borrow primitive rather than (LDB (BYTE 64 0) (- BIT)): where SBCL
  ;; misses the modular form it computes a signed result, tests whether it fits a fixnum
  ;; and boxes a bignum when it does not, which branches and allocates on the secret.
  (values (clempo.ec.word:subtract-with-borrow 0 bit 1)))

(defun nonzero-bit (w)
  "1 when the word W is nonzero: 0 - W borrows exactly then."
  (declare #.clempo.base:*hot* (type word w))
  (logxor 1 (nth-value 1 (clempo.ec.word:subtract-with-borrow 0 w 1))))

(defun equal-mask (a b)
  "All ones when the words A and B are equal."
  (declare #.clempo.base:*hot* (type word a b))
  (bit-mask (logxor 1 (nonzero-bit (logxor a b)))))

(defun words-from-octets! (out o n octets start &key little-endian)
  "Load N words from 8N octets at START (most significant first unless LITTLE-ENDIAN)."
  (declare #.clempo.base:*hot* (type words out) (type clempo.base:octets octets)
           (type fixnum o n start))
  (dotimes (i n)
    (let ((w 0))
      (declare (type word w))
      (dotimes (j 8)
        (let ((byte (if little-endian
                        (aref octets (+ start (* 8 i) j))
                        (aref octets (+ start (* 8 (- n 1 i)) (- 7 j))))))
          (setf w (logior w (ash byte (* 8 j))))))
      (setf (aref out (+ o i)) w)))
  (values))

(defun words-to-octets! (octets start a ao n &key little-endian)
  (declare #.clempo.base:*hot* (type words a) (type clempo.base:octets octets)
           (type fixnum ao n start))
  (dotimes (i n)
    (let ((w (aref a (+ ao i))))
      (dotimes (j 8)
        (setf (aref octets (if little-endian
                               (+ start (* 8 i) j)
                               (+ start (* 8 (- n 1 i)) (- 7 j))))
              (ldb (byte 8 (* 8 j)) w)))))
  (values))

;;; Secret octets at the edges: wiping, and hex for key files without a table index or a
;;; branch on a secret digit.

(defun wipe (&rest vectors)
  "Overwrite each octet or word vector in VECTORS (NIL allowed) with zeros."
  (dolist (v vectors)
    (etypecase v
      (null)
      (clempo.base:octets (fill v 0))
      (words (fill v 0))))
  (values))

(defmacro with-wiped ((&rest bindings) &body body)
  "Bind each (VAR FORM) like LET*, and wipe every VAR's vector when BODY exits."
  `(let* ,bindings
     (unwind-protect (progn ,@body)
       (wipe ,@(mapcar #'first bindings)))))

(declaim (inline negative-bit))
(defun negative-bit (x)
  "1 when the fixnum X is negative, else 0, from its sign bit."
  (declare #.clempo.base:*hot* (type fixnum x))
  (logand 1 (ash x -62)))

(defun hex-to-octets (string)
  "STRING's hex digits (no prefix, either case) as octets, or NIL unless STRING is an even
count of hex digits. Every digit is decoded with arithmetic masks; only the public verdict
on the whole string branches."
  (let ((n (length string)))
    (when (oddp n) (return-from hex-to-octets nil))
    (let ((out (clempo.base:make-octets (floor n 2))) (bad 0))
      (declare (type bit bad))
      (flet ((nibble (char)
               (let* ((c (char-code char))
                      (d (- c 48))
                      (l (- (logior c 32) 97))
                      (digit (logxor 1 (logior (negative-bit d) (negative-bit (- 9 d)))))
                      (letter (logxor 1 (logior (negative-bit l) (negative-bit (- 5 l))))))
                 (declare (type fixnum c d l) (type bit digit letter))
                 (setf bad (logior bad (logxor 1 (logior digit letter))))
                 (logand 15 (logior (logand d (- digit)) (logand (+ l 10) (- letter)))))))
        (dotimes (i (length out))
          (setf (aref out i) (logior (ash (nibble (char string (* 2 i))) 4)
                                     (nibble (char string (1+ (* 2 i))))))))
      (if (zerop bad) out (progn (wipe out) nil)))))

(defun octets-to-hex (octets)
  "Lowercase hex of OCTETS, each digit computed with arithmetic rather than a table."
  (declare (type clempo.base:octets octets))
  (let ((s (make-string (* 2 (length octets)))))
    (flet ((digit (v)
             (declare (type (integer 0 15) v))
             ;; '0' + v, plus 39 more when v > 9 to land on 'a'.
             (code-char (+ 48 v (logand 39 (- (negative-bit (- 9 v))))))))
      (dotimes (i (length octets) s)
        (let ((b (aref octets i)))
          (setf (char s (* 2 i)) (digit (ash b -4))
                (char s (1+ (* 2 i))) (digit (logand b 15))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ct-name (prefix suffix)
    (intern (format nil "~A-~A" prefix suffix) (symbol-package prefix)))
  (defun word-list (x n) (loop for i below n collect (ldb (byte 64 (* 64 i)) x)))
  (defun negated-inverse-2^64 (m)
    (let ((x 1))
      (loop repeat 7 do (setf x (ldb (byte 64 0) (* x (- 2 (* m x))))))
      (ldb (byte 64 0) (- x)))))

(defun words-integer (w n)
  "The nonnegative integer whose little-endian words are W[0..N)."
  (declare (type words w) (type (integer 1 16) n))
  (let ((b (sb-bignum:%allocate-bignum (1+ n))))
    (dotimes (i n) (sb-bignum:%bignum-set b i (aref w i)))
    (sb-bignum:%bignum-set b n 0)
    (sb-bignum::%normalize-bignum b (1+ n))))

(defmacro integer-word (integer index)
  "Word INDEX of a nonnegative INTEGER without allocating."
  `(if (typep ,integer 'fixnum) ,(if (zerop index) integer 0)
       (if (< ,index (sb-bignum:%bignum-length ,integer))
           (sb-bignum::%bignum-ref ,integer ,index) 0)))

(defmacro define-ct-field (prefix modulus)
  "Define PREFIX-ADD!, -SUB!, -NEG!, -MUL!, -SQUARE!, -INV! (Fermat, fixed exponent),
-COPY!, -ZERO!, -ONE!, -CMOV!, -ZERO-BIT, -LESS-BIT, -FROM-WORDS!, -FROM-INTEGER!,
-TO-WORDS!, -TO-INTEGER and +PREFIX-WORDS+ over the odd MODULUS in Montgomery form."
  (let* ((n (ceiling (integer-length modulus) 64))
         (ps (word-list modulus n))
         (r (ash 1 (* 64 n)))
         (factor (negated-inverse-2^64 modulus))
         (name (lambda (s) (ct-name prefix s)))
         (r2 (funcall name "R2")) (r3 (funcall name "R3"))
         (unit (funcall name "INTEGER-ONE")) (mont-one (word-list (mod r modulus) n))
         (full (= (integer-length modulus) (* 64 n))))
    (flet ((gens (stem) (loop repeat n collect (gensym stem))))
      (let ((as (gens "A")) (ts (gens "T")))
        `(progn
           (defconstant ,(intern (format nil "+~A-WORDS+" prefix) (symbol-package prefix)) ,n)
           (declaim (type words ,r2 ,r3 ,unit))
           (defparameter ,r2 (let ((w (make-words ,n)))
                               (dotimes (i ,n w) (setf (aref w i) (ldb (byte 64 (* 64 i)) ,(mod (* r r) modulus))))))
           (defparameter ,r3 (let ((w (make-words ,n)))
                               (dotimes (i ,n w) (setf (aref w i) (ldb (byte 64 (* 64 i)) ,(mod (* r r r) modulus))))))
           (defparameter ,unit (let ((w (make-words ,n))) (setf (aref w 0) 1) w))
           (defun ,(funcall name "COPY!") (out o a ao)
             (declare #.clempo.base:*hot* (type words out a) (type fixnum o ao))
             ,@(loop for i below n collect `(setf (aref out (+ o ,i)) (aref a (+ ao ,i))))
             (values))
           (defun ,(funcall name "ZERO!") (out o)
             (declare #.clempo.base:*hot* (type words out) (type fixnum o))
             ,@(loop for i below n collect `(setf (aref out (+ o ,i)) 0))
             (values))
           (defun ,(funcall name "ONE!") (out o)
             (declare #.clempo.base:*hot* (type words out) (type fixnum o))
             ,@(loop for w in mont-one for i from 0 collect `(setf (aref out (+ o ,i)) ,w))
             (values))
           (defun ,(funcall name "CMOV!") (out o a ao choice)
             "OUT := A when CHOICE is 1; unchanged when it is 0. Choices cross calls as
bits: a boxed all-ones mask word would allocate, and the allocation would show."
             (declare #.clempo.base:*hot* (type words out a) (type fixnum o ao) (type bit choice))
             (let ((mask (bit-mask choice)))
               (declare (type word mask))
               ,@(loop for i below n collect
                   `(setf (aref out (+ o ,i))
                          (logior (logand mask (aref a (+ ao ,i)))
                                  (logandc1 mask (aref out (+ o ,i)))))))
             (values))
           (defun ,(funcall name "ZERO-BIT") (a ao)
             "1 when A is zero, else 0."
             (declare #.clempo.base:*hot* (type words a) (type fixnum ao))
             (logxor 1 (nonzero-bit (logior ,@(loop for i below n collect `(aref a (+ ao ,i)))))))
           (defun ,(funcall name "LESS-BIT") (a ao)
             "1 when the raw words A are below the modulus, else 0."
             (declare #.clempo.base:*hot* (type words a) (type fixnum ao))
             (let ((borrow 1) (w 0))
               (declare (type bit borrow) (type word w) (ignorable w))
               ,@(loop for p in ps for i from 0 collect
                   `(setf (values w borrow) (clempo.ec.word:subtract-with-borrow (aref a (+ ao ,i)) ,p borrow)))
               (logxor 1 borrow)))
           (declaim (inline ,(funcall name "ADD!") ,(funcall name "SUB!")))
           (defun ,(funcall name "ADD!") (out o a ao b bo)
             (declare #.clempo.base:*hot* (type words out a b) (type fixnum o ao bo))
             (let (,@(loop for x in ts collect `(,x 0)) ,@(loop for x in as collect `(,x 0))
                   (carry 0) (borrow 1))
               (declare (type word ,@ts ,@as) (type bit carry borrow))
               ,@(loop for x in ts for i from 0 collect
                   `(setf (values ,x carry) (clempo.ec.word:add-with-carry (aref a (+ ao ,i)) (aref b (+ bo ,i)) carry)))
               ,@(loop for x in ts for d in as for p in ps collect
                   `(setf (values ,d borrow) (clempo.ec.word:subtract-with-borrow ,x ,p borrow)))
               (setf borrow (nth-value 1 (clempo.ec.word:subtract-with-borrow carry 0 borrow)))
               (let ((mask (bit-mask borrow)))
                 ,@(loop for x in ts for d in as for i from 0 collect
                     `(setf (aref out (+ o ,i)) (logior (logand mask ,d) (logandc1 mask ,x))))))
             (values))
           (defun ,(funcall name "SUB!") (out o a ao b bo)
             (declare #.clempo.base:*hot* (type words out a b) (type fixnum o ao bo))
             (let (,@(loop for x in ts collect `(,x 0)) (borrow 1) (carry 0))
               (declare (type word ,@ts) (type bit borrow carry))
               ,@(loop for x in ts for i from 0 collect
                   `(setf (values ,x borrow) (clempo.ec.word:subtract-with-borrow (aref a (+ ao ,i)) (aref b (+ bo ,i)) borrow)))
               (let ((mask (bit-mask (logxor 1 borrow))))
                 ,@(loop for x in ts for p in ps for i from 0 collect
                     `(progn (setf (values ,x carry) (clempo.ec.word:add-with-carry ,x (logand mask ,p) carry))
                             (setf (aref out (+ o ,i)) ,x)))))
             (values))
           (defun ,(funcall name "MUL!") (out o a ao b bo)
             "Montgomery product a*b/R for A below the modulus and B below R."
             (declare #.clempo.base:*hot* (type words out a b) (type fixnum o ao bo))
             (let (,@(loop for x in ts collect `(,x 0))
                   (high 0) ,@(when full '((spill 0))) (bi 0) (m 0) (carry 0) (bit 0) (borrow 1))
               (declare (type word ,@ts high ,@(when full '(spill)) bi m carry) (type bit bit borrow))
               ,@(loop for i below n append
                   (append
                    `((setf bi (aref b (+ bo ,i)) carry 0))
                    (loop for tt in ts for j from 0 collect
                      `(setf (values carry ,tt) (mac (aref a (+ ao ,j)) bi ,tt carry)))
                    ;; A full-width modulus can carry one bit past HIGH (SPILL).
                    (if full
                        `((setf (values high bit) (clempo.ec.word:add-with-carry high carry 0)
                                spill bit))
                        `((setf high (ldb (byte 64 0) (+ high carry)))))
                    `((setf m (ldb (byte 64 0) (* ,(first ts) ,factor)))
                      (setf (values carry ,(first ts)) (mac m ,(first ps) ,(first ts) 0)))
                    (loop for p in (rest ps) for tt in (rest ts) for prev in ts collect
                      `(setf (values carry ,prev) (mac m ,p ,tt carry)))
                    (if full
                        `((setf (values ,(car (last ts)) bit) (clempo.ec.word:add-with-carry high carry 0)
                                high (+ spill bit)))
                        `((setf (values ,(car (last ts)) bit) (clempo.ec.word:add-with-carry high carry 0)
                                high bit)))))
               ;; The result is below twice the modulus; HIGH is its top word.
               (let (,@(loop for x in as collect `(,x 0)))
                 (declare (type word ,@as))
                 ,@(loop for x in as for tt in ts for p in ps collect
                     `(setf (values ,x borrow) (clempo.ec.word:subtract-with-borrow ,tt ,p borrow)))
                 (setf (values m borrow) (clempo.ec.word:subtract-with-borrow high 0 borrow))
                 (let ((mask (bit-mask borrow)))
                   ,@(loop for x in as for tt in ts for i from 0 collect
                       `(setf (aref out (+ o ,i)) (logior (logand mask ,x) (logandc1 mask ,tt)))))))
             (values))
           (declaim (inline ,(funcall name "SQUARE!")))
           (defun ,(funcall name "SQUARE!") (out o a ao)
             (,(funcall name "MUL!") out o a ao a ao))
           (defun ,(funcall name "NEG!") (out o a ao)
             (declare #.clempo.base:*hot* (type words out a) (type fixnum o ao))
             (let ((zero (make-words ,n)))
               (declare (dynamic-extent zero))
               (,(funcall name "SUB!") out o zero 0 a ao)))
           (defun ,(funcall name "INV!") (out o a ao)
             "a^(m-2): the inverse of a unit, zero for zero. The exponent is public."
             (declare #.clempo.base:*hot* (type words out a) (type fixnum o ao))
             (let ((acc (make-words ,n)) (base (make-words ,n)))
               (declare (dynamic-extent acc base))
               (,(funcall name "COPY!") base 0 a ao)
               (,(funcall name "ONE!") acc 0)
               (loop for i of-type fixnum downfrom ,(1- (integer-length (- modulus 2))) to 0 do
                 (,(funcall name "MUL!") acc 0 acc 0 acc 0)
                 (when (logbitp i ,(- modulus 2))
                   (,(funcall name "MUL!") acc 0 acc 0 base 0)))
               (,(funcall name "COPY!") out o acc 0)))
           (defun ,(funcall name "FROM-WORDS!") (out o a ao)
             "Montgomery form of the raw words A, which may exceed the modulus."
             (,(funcall name "MUL!") out o ,r2 0 a ao))
           (defun ,(funcall name "FROM-WIDE-WORDS!") (out o low lo high ho)
             "Montgomery form of LOW + HIGH*R for raw word vectors LOW and HIGH."
             (declare #.clempo.base:*hot* (type words out low high) (type fixnum o lo ho))
             (let ((w (make-words ,n)))
               (declare (dynamic-extent w))
               (,(funcall name "MUL!") w 0 ,r2 0 low lo)
               (,(funcall name "MUL!") out o ,r3 0 high ho)
               (,(funcall name "ADD!") out o out o w 0)))
           (defun ,(funcall name "TO-WORDS!") (out o a ao)
             "Canonical raw words of the Montgomery element A."
             (,(funcall name "MUL!") out o a ao ,unit 0))
           (defun ,(funcall name "FROM-INTEGER!") (out o integer)
             "Public integer boundary: reduce INTEGER and convert it."
             (declare (type words out) (type fixnum o) (type integer integer))
             (let ((x (if (<= 0 integer ,(1- modulus)) integer (mod integer ,modulus))))
               ,@(loop for i below n collect `(setf (aref out (+ o ,i)) (integer-word x ,i))))
             (,(funcall name "FROM-WORDS!") out o out o))
           (defun ,(funcall name "TO-INTEGER") (a ao)
             "The canonical integer of A. Digits are copied into one bignum of fixed
length, so allocation does not depend on the value (shifting words together would)."
             (declare (type words a) (type fixnum ao))
             (let ((w (make-words ,n)))
               (,(funcall name "TO-WORDS!") w 0 a ao)
               (words-integer w ,n)))
           ',prefix)))))

(defmacro define-ct-quadratic-field (prefix base width)
  "Fp2 = Fp[u]/(u^2+1) over the field BASE of WIDTH words per coordinate, with the
operations DEFINE-CT-FIELD defines (no LESS-BIT or word import)."
  (let ((name (lambda (s) (ct-name prefix s)))
        (op (lambda (s) (ct-name base s)))
        (w2 (* 2 width)))
    (flet ((both (s &rest args)
             (let ((f (funcall op s)))
               `(progn (,f ,@(mapcar (lambda (a) (if (consp a) (car a) a)) args))
                       (,f ,@(mapcar (lambda (a) (if (consp a) (subst width 'width (cdr a)) a)) args))))))
      `(progn
         (defun ,(funcall name "COPY!") (out o a ao)
           ,(both "COPY!" 'out '(o . (+ o width)) 'a '(ao . (+ ao width))))
         (defun ,(funcall name "ZERO!") (out o) ,(both "ZERO!" 'out '(o . (+ o width))))
         (defun ,(funcall name "ONE!") (out o)
           (,(funcall op "ONE!") out o) (,(funcall op "ZERO!") out (+ o ,width)))
         (defun ,(funcall name "CMOV!") (out o a ao choice)
           ,(both "CMOV!" 'out '(o . (+ o width)) 'a '(ao . (+ ao width)) 'choice))
         (defun ,(funcall name "ZERO-BIT") (a ao)
           (logand (,(funcall op "ZERO-BIT") a ao) (,(funcall op "ZERO-BIT") a (+ ao ,width))))
         ,@(loop for s in '("ADD!" "SUB!") collect
             `(defun ,(funcall name s) (out o a ao b bo)
                ,(both s 'out '(o . (+ o width)) 'a '(ao . (+ ao width)) 'b '(bo . (+ bo width)))))
         (defun ,(funcall name "NEG!") (out o a ao)
           ,(both "NEG!" 'out '(o . (+ o width)) 'a '(ao . (+ ao width))))
         (defun ,(funcall name "MUL!") (out o a ao b bo)
           (declare #.clempo.base:*hot* (type words out a b) (type fixnum o ao bo))
           (let ((w (make-words ,(* 4 width))))
             (declare (dynamic-extent w))
             (,(funcall op "MUL!") w 0 a ao b bo)
             (,(funcall op "MUL!") w ,width a (+ ao ,width) b (+ bo ,width))
             (,(funcall op "ADD!") w ,(* 2 width) a ao a (+ ao ,width))
             (,(funcall op "ADD!") w ,(* 3 width) b bo b (+ bo ,width))
             (,(funcall op "MUL!") w ,(* 2 width) w ,(* 2 width) w ,(* 3 width))
             (,(funcall op "SUB!") w ,(* 2 width) w ,(* 2 width) w 0)
             (,(funcall op "SUB!") out (+ o ,width) w ,(* 2 width) w ,width)
             (,(funcall op "SUB!") out o w 0 w ,width))
           (values))
         (declaim (inline ,(funcall name "SQUARE!")))
         (defun ,(funcall name "SQUARE!") (out o a ao) (,(funcall name "MUL!") out o a ao a ao))
         (defun ,(funcall name "INV!") (out o a ao)
           "(a0 - a1 u)/(a0^2 + a1^2), zero for zero."
           (declare #.clempo.base:*hot* (type words out a) (type fixnum o ao))
           (let ((w (make-words ,(* 2 width))))
             (declare (dynamic-extent w))
             (,(funcall op "MUL!") w 0 a ao a ao)
             (,(funcall op "MUL!") w ,width a (+ ao ,width) a (+ ao ,width))
             (,(funcall op "ADD!") w 0 w 0 w ,width)
             (,(funcall op "INV!") w 0 w 0)
             (,(funcall op "MUL!") out o a ao w 0)
             (,(funcall op "MUL!") w ,width a (+ ao ,width) w 0)
             (,(funcall op "NEG!") out (+ o ,width) w ,width))
           (values))
         (defun ,(funcall name "FROM-INTEGER!") (out o pair)
           "PAIR is (c0 . c1), the element c0 + c1 u, or an integer c0."
           (,(funcall op "FROM-INTEGER!") out o (if (consp pair) (car pair) pair))
           (,(funcall op "FROM-INTEGER!") out (+ o ,width) (if (consp pair) (cdr pair) 0)))
         (defun ,(funcall name "TO-INTEGER") (a ao)
           (cons (,(funcall op "TO-INTEGER") a ao) (,(funcall op "TO-INTEGER") a (+ ao ,width))))
         (defconstant ,(intern (format nil "+~A-WORDS+" prefix) (symbol-package prefix)) ,w2)
         ',prefix))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Renes, Costello and Batina, "Complete addition formulas for prime order
  ;; elliptic curves" (2016): straight-line register programs, (op dst x y).
  (defparameter *complete-formulas*
    '((:generic
       :add ((* t0 x1 x2) (* t1 y1 y2) (* t2 z1 z2) (+ t3 x1 y1) (+ t4 x2 y2) (* t3 t3 t4)
             (+ t4 t0 t1) (- t3 t3 t4) (+ t4 x1 z1) (+ t5 x2 z2) (* t4 t4 t5) (+ t5 t0 t2)
             (- t4 t4 t5) (+ t5 y1 z1) (+ x3 y2 z2) (* t5 t5 x3) (+ x3 t1 t2) (- t5 t5 x3)
             (* z3 ca t4) (* x3 cb3 t2) (+ z3 x3 z3) (- x3 t1 z3) (+ z3 t1 z3) (* y3 x3 z3)
             (+ t1 t0 t0) (+ t1 t1 t0) (* t2 ca t2) (* t4 cb3 t4) (+ t1 t1 t2) (- t2 t0 t2)
             (* t2 ca t2) (+ t4 t4 t2) (* t0 t1 t4) (+ y3 y3 t0) (* t0 t5 t4) (* x3 t3 x3)
             (- x3 x3 t0) (* t0 t3 t1) (* z3 t5 z3) (+ z3 z3 t0))
       :double ((* t0 x1 x1) (* t1 y1 y1) (* t2 z1 z1) (* t3 x1 y1) (+ t3 t3 t3) (* z3 x1 z1)
                (+ z3 z3 z3) (* x3 ca z3) (* y3 cb3 t2) (+ y3 x3 y3) (- x3 t1 y3) (+ y3 t1 y3)
                (* y3 x3 y3) (* x3 t3 x3) (* z3 cb3 z3) (* t2 ca t2) (- t3 t0 t2) (* t3 ca t3)
                (+ t3 t3 z3) (+ z3 t0 t0) (+ t0 z3 t0) (+ t0 t0 t2) (* t0 t0 t3) (+ y3 y3 t0)
                (* t2 y1 z1) (+ t2 t2 t2) (* t0 t2 t3) (- x3 x3 t0) (* z3 t2 t1) (+ z3 z3 z3)
                (+ z3 z3 z3)))
      (:minus-3
       :add ((* t0 x1 x2) (* t1 y1 y2) (* t2 z1 z2) (+ t3 x1 y1) (+ t4 x2 y2) (* t3 t3 t4)
             (+ t4 t0 t1) (- t3 t3 t4) (+ t4 y1 z1) (+ x3 y2 z2) (* t4 t4 x3) (+ x3 t1 t2)
             (- t4 t4 x3) (+ x3 x1 z1) (+ y3 x2 z2) (* x3 x3 y3) (+ y3 t0 t2) (- y3 x3 y3)
             (* z3 cb t2) (- x3 y3 z3) (+ z3 x3 x3) (+ x3 x3 z3) (- z3 t1 x3) (+ x3 t1 x3)
             (* y3 cb y3) (+ t1 t2 t2) (+ t2 t1 t2) (- y3 y3 t2) (- y3 y3 t0) (+ t1 y3 y3)
             (+ y3 t1 y3) (+ t1 t0 t0) (+ t0 t1 t0) (- t0 t0 t2) (* t1 t4 y3) (* t2 t0 y3)
             (* y3 x3 z3) (+ y3 y3 t2) (* x3 t3 x3) (- x3 x3 t1) (* z3 t4 z3) (* t1 t3 t0)
             (+ z3 z3 t1))
       :double ((* t0 x1 x1) (* t1 y1 y1) (* t2 z1 z1) (* t3 x1 y1) (+ t3 t3 t3) (* z3 x1 z1)
                (+ z3 z3 z3) (* y3 cb t2) (- y3 y3 z3) (+ x3 y3 y3) (+ y3 x3 y3) (- x3 t1 y3)
                (+ y3 t1 y3) (* y3 x3 y3) (* x3 x3 t3) (+ t3 t2 t2) (+ t2 t2 t3) (* z3 cb z3)
                (- z3 z3 t2) (- z3 z3 t0) (+ t3 z3 z3) (+ z3 z3 t3) (+ t3 t0 t0) (+ t0 t3 t0)
                (- t0 t0 t2) (* t0 t0 z3) (+ y3 y3 t0) (* t0 y1 z1) (+ t0 t0 t0) (* z3 t0 z3)
                (- x3 x3 z3) (* z3 t0 t1) (+ z3 z3 z3) (+ z3 z3 z3)))
      (:zero
       :add ((* t0 x1 x2) (* t1 y1 y2) (* t2 z1 z2) (+ t3 x1 y1) (+ t4 x2 y2) (* t3 t3 t4)
             (+ t4 t0 t1) (- t3 t3 t4) (+ t4 y1 z1) (+ x3 y2 z2) (* t4 t4 x3) (+ x3 t1 t2)
             (- t4 t4 x3) (+ x3 x1 z1) (+ y3 x2 z2) (* x3 x3 y3) (+ y3 t0 t2) (- y3 x3 y3)
             (+ x3 t0 t0) (+ t0 x3 t0) (* t2 cb3 t2) (+ z3 t1 t2) (- t1 t1 t2) (* y3 cb3 y3)
             (* x3 t4 y3) (* t2 t3 t1) (- x3 t2 x3) (* y3 y3 t0) (* t1 t1 z3) (+ y3 t1 y3)
             (* t0 t0 t3) (* z3 z3 t4) (+ z3 z3 t0))
       :double ((* t0 y1 y1) (+ z3 t0 t0) (+ z3 z3 z3) (+ z3 z3 z3) (* t1 y1 z1) (* t2 z1 z1)
                (* t2 cb3 t2) (* x3 t2 z3) (+ y3 t0 t2) (* z3 t1 z3) (+ t1 t2 t2) (+ t2 t1 t2)
                (- t0 t0 t2) (* y3 t0 y3) (+ y3 x3 y3) (* t1 x1 y1) (* x3 t0 t1) (+ x3 x3 x3))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +digits+ 65
    "Signed odd base-16 digits of a scalar below 2^257."))

(deftype digits () '(simple-array (signed-byte 8) (65)))
(declaim (inline make-digits))
(defun make-digits () (make-array +digits+ :element-type '(signed-byte 8) :initial-element 0))

(defun recode-odd! (digits scalar order)
  "Write k' = k + (1 - k mod 2) ORDER, for the 32 big-endian SCALAR octets k, as
+DIGITS+ odd digits in [-15, 15], least significant first. k' is odd and congruent to
k modulo ORDER (odd), so k'P = kP for P of that order; no digit is zero, so no table
read is the identity. Each step is d = (k' mod 32) - 16, k' = (k' - d)/16, keeping k'
odd (Joye and Tunstall's regular recoding), with masks in place of branches."
  (declare #.clempo.base:*hot* (type digits digits) (type clempo.base:octets scalar)
           (type words order))
  (assert (= (length scalar) 32))
  (let ((k (make-words 5)))
    (declare (dynamic-extent k))
    (words-from-octets! k 0 4 scalar 0)
    (let ((mask (bit-mask (logxor 1 (logand (aref k 0) 1)))) (carry 0))
      (declare (type word mask) (type bit carry))
      (dotimes (i 4)
        (setf (values (aref k i) carry)
              (clempo.ec.word:add-with-carry (aref k i) (logand mask (aref order i)) carry)))
      (setf (aref k 4) carry))
    (dotimes (i (1- +digits+))
      (let* ((low (logand (aref k 0) 31))
             (adjust (ldb (byte 64 0) (- 16 low)))
             (extend (bit-mask (ldb (byte 1 63) adjust)))
             (carry 0))
        (declare (type (integer 0 31) low) (type word adjust extend) (type bit carry))
        (setf (aref digits i) (- low 16))
        ;; k' - d = k' + (16 - low), sign-extended across the words, then / 16.
        (setf (values (aref k 0) carry) (clempo.ec.word:add-with-carry (aref k 0) adjust 0))
        (loop for j from 1 below 5 do
          (setf (values (aref k j) carry) (clempo.ec.word:add-with-carry (aref k j) extend carry)))
        (dotimes (j 4)
          (setf (aref k j) (logior (ash (aref k j) -4) (ldb (byte 64 0) (ash (aref k (1+ j)) 60)))))
        (setf (aref k 4) (ash (aref k 4) -4))))
    (setf (aref digits (1- +digits+)) (aref k 0))
    digits))

(defmacro define-ct-curve (prefix &key field width a b order)
  "Projective points (X:Y:Z) on y^2 = x^3 + A x + B over FIELD (WIDTH words per
coordinate). A and B are integers, or (c0 . c1) pairs for Fp2; A = 0 and A = -3
select specialised formulas. Addition and doubling use the complete formulas of
Renes, Costello and Batina, so the identity (0:1:0), P+P and P+(-P) take the same
path as every other input. Defines PREFIX-IDENTITY!, -ADD!, -DOUBLE!, -SELECT!,
-SIGNED-ENTRY!, -MUL!, -BUILD-COMB, -MUL-COMB!, -FROM-AFFINE-INTEGERS! and -AFFINE!.
ORDER is the odd order of the points multiplied (RECODE-ODD!)."
  (let* ((name (lambda (s) (ct-name prefix s)))
         (f (lambda (s) (ct-name field s)))
         (point (* 3 width))
         (b3 (if (consp b) (cons (* 3 (car b)) (* 3 (cdr b))) (* 3 b)))
         (formulas (cdr (assoc (cond ((eql a 0) :zero) ((eql a -3) :minus-3) (t :generic))
                               *complete-formulas*)))
         (slots (loop repeat point collect (gensym "S")))
         (constants (funcall name "CONSTANTS"))
         (order-words (funcall name "ORDER")))
    (labels ((reg (sym)
               ;; Registers live in the stack arena W; inputs in P/Q, output in W too.
               (let ((i (position sym '(t0 t1 t2 t3 t4 t5 x3 y3 z3 ca cb cb3))))
                 (cond ((member sym '(x1 y1 z1))
                        (list 'p `(+ po ,(* width (position sym '(x1 y1 z1))))))
                       ((member sym '(x2 y2 z2))
                        (list 'q `(+ qo ,(* width (position sym '(x2 y2 z2))))))
                       ((member sym '(ca cb cb3))
                        (list constants (* width (- i 9))))
                       (t (list 'w (* width i))))))
             (emit (op dst x y)
               `(,(funcall f (ecase op (* "MUL!") (+ "ADD!") (- "SUB!")))
                 ,@(reg dst) ,@(reg x) ,@(reg y)))
             (program (key lambda-list)
               `(defun ,(funcall name (if (eq key :add) "ADD!" "DOUBLE!")) ,lambda-list
                  (declare #.clempo.base:*hot* (type words out p ,@(when (eq key :add) '(q)))
                           (type fixnum o po ,@(when (eq key :add) '(qo))))
                  (let ((w (make-words ,(* 9 width))))
                    (declare (dynamic-extent w))
                    ,@(loop for (op dst x y) in (getf formulas key) collect (emit op dst x y))
                    (dotimes (i ,point) (setf (aref out (+ o i)) (aref w (+ ,(* 6 width) i)))))
                  (values))))
      `(progn
         (declaim (type words ,constants ,order-words))
         (defparameter ,order-words
           (let ((w (make-words 4)))
             (dotimes (i 4 w) (setf (aref w i) (ldb (byte 64 (* 64 i)) ,order)))))
         (defparameter ,constants
           (let ((c (make-words ,(* 3 width))))
             (,(funcall f "FROM-INTEGER!") c 0 ',a)
             (,(funcall f "FROM-INTEGER!") c ,width ',b)
             (,(funcall f "FROM-INTEGER!") c ,(* 2 width) ',b3)
             c))
         (defun ,(funcall name "IDENTITY!") (out o)
           (declare (type words out) (type fixnum o))
           (,(funcall f "ZERO!") out o) (,(funcall f "ONE!") out (+ o ,width))
           (,(funcall f "ZERO!") out (+ o ,(* 2 width))))
         ,(program :add '(out o p po q qo))
         ,(program :double '(out o p po))
         (defun ,(funcall name "SELECT!") (out o table to count index)
           "OUT := entry INDEX < COUNT of the points at TO, reading every entry."
           (declare #.clempo.base:*hot* (type words out table) (type fixnum o to count)
                    (type (unsigned-byte 32) index))
           (let (,@(loop for s in slots collect `(,s 0)))
             (declare (type word ,@slots))
             (dotimes (k count)
               (let ((mask (equal-mask k index)) (from (+ to (* k ,point))))
                 (declare (type word mask) (type fixnum from))
                 ,@(loop for s in slots for i from 0
                         collect `(setf ,s (logior ,s (logand mask (aref table (+ from ,i))))))))
             ,@(loop for s in slots for i from 0 collect `(setf (aref out (+ o ,i)) ,s)))
           (values))
         (defun ,(funcall name "FROM-AFFINE-INTEGERS!") (out o x y)
           (,(funcall f "FROM-INTEGER!") out o x)
           (,(funcall f "FROM-INTEGER!") out (+ o ,width) y)
           (,(funcall f "ONE!") out (+ o ,(* 2 width))))
         (defun ,(funcall name "SIGNED-ENTRY!") (out o table to digit)
           "OUT := DIGIT*Q from TO's odd multiples Q, 3Q, ..., 15Q: a masked read of entry
(|DIGIT|-1)/2, then a masked negation of y for a negative DIGIT."
           (declare #.clempo.base:*hot* (type words out table) (type fixnum o to)
                    (type (integer -15 15) digit))
           (let* ((sign (logand 1 (ash digit -63)))
                  (magnitude (+ (logxor digit (- sign)) sign))
                  (y (make-words ,width)))
             (declare (type bit sign) (type (integer 0 15) magnitude) (dynamic-extent y))
             (,(funcall name "SELECT!") out o table to 8 (ash magnitude -1))
             (,(funcall f "NEG!") y 0 out (+ o ,width))
             (,(funcall f "CMOV!") out (+ o ,width) y 0 sign))
           (values))
         (defun ,(funcall name "MUL!") (out o p po scalar)
           "OUT := SCALAR*P for 32 big-endian SCALAR octets. Signed odd 4-bit digits
(RECODE-ODD!) make every window four doublings and one addition of a masked table
entry that is never the identity."
           (declare #.clempo.base:*hot* (type words out p) (type fixnum o po)
                    (type clempo.base:octets scalar))
           (let ((table (make-words ,(* 8 point))) (acc (make-words ,point))
                 (entry (make-words ,point)) (digits (make-digits)))
             (declare (dynamic-extent table acc entry digits))
             (recode-odd! digits scalar ,order-words)
             (dotimes (i ,point) (setf (aref table i) (aref p (+ po i))))
             (,(funcall name "DOUBLE!") entry 0 p po)
             (loop for k from 1 below 8 do
               (,(funcall name "ADD!") table (* k ,point) table (* (1- k) ,point) entry 0))
             (,(funcall name "SIGNED-ENTRY!") acc 0 table 0 (aref digits ,(1- +digits+)))
             (loop for i from ,(- +digits+ 2) downto 0 do
               (dotimes (j 4) (,(funcall name "DOUBLE!") acc 0 acc 0))
               (,(funcall name "SIGNED-ENTRY!") entry 0 table 0 (aref digits i))
               (,(funcall name "ADD!") acc 0 acc 0 entry 0))
             (dotimes (i ,point) (setf (aref out (+ o i)) (aref acc i))))
           (values))
         (defun ,(funcall name "BUILD-COMB") (x y)
           "Row i < +DIGITS+ holds (2j+1)*16^i*(X,Y) for j < 8, from public affine integers."
           (let ((table (make-words (* +digits+ 8 ,point)))
                 (base (make-words ,point)) (twice (make-words ,point)))
             (,(funcall name "FROM-AFFINE-INTEGERS!") base 0 x y)
             (dotimes (i +digits+ table)
               (let ((row (* i 8 ,point)))
                 (dotimes (k ,point) (setf (aref table (+ row k)) (aref base k)))
                 (,(funcall name "DOUBLE!") twice 0 base 0)
                 (loop for j from 1 below 8 do
                   (,(funcall name "ADD!") table (+ row (* j ,point)) table (+ row (* (1- j) ,point)) twice 0))
                 ;; 16 * base = 15 * base + base.
                 (,(funcall name "ADD!") base 0 table (+ row (* 7 ,point)) base 0)))))
         (defun ,(funcall name "MUL-COMB!") (out o table scalar)
           "OUT := SCALAR*G for a comb TABLE of G and 32 big-endian SCALAR octets: one
masked, never-identity entry per signed odd digit and no doublings."
           (declare #.clempo.base:*hot* (type words out table) (type fixnum o)
                    (type clempo.base:octets scalar))
           (let ((acc (make-words ,point)) (entry (make-words ,point)) (digits (make-digits)))
             (declare (dynamic-extent acc entry digits))
             (recode-odd! digits scalar ,order-words)
             (,(funcall name "SIGNED-ENTRY!") acc 0 table 0 (aref digits 0))
             (loop for i from 1 below +digits+ do
               (,(funcall name "SIGNED-ENTRY!") entry 0 table (* i 8 ,point) (aref digits i))
               (,(funcall name "ADD!") acc 0 acc 0 entry 0))
             (dotimes (i ,point) (setf (aref out (+ o i)) (aref acc i))))
           (values))
         (defun ,(funcall name "AFFINE!") (out o p po)
           "OUT := x, y (Montgomery form) of P; return 1 when P is the identity, else 0."
           (declare #.clempo.base:*hot* (type words out p) (type fixnum o po))
           (let ((zi (make-words ,width)))
             (declare (dynamic-extent zi))
             (,(funcall f "INV!") zi 0 p (+ po ,(* 2 width)))
             (,(funcall f "MUL!") out o p po zi 0)
             (,(funcall f "MUL!") out (+ o ,width) p (+ po ,width) zi 0)
             (,(funcall f "ZERO-BIT") p (+ po ,(* 2 width)))))
         ',prefix))))
