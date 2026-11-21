(defpackage #:clempo.cwcodec
  (:use #:cl #:clempo.base)
  (:documentation "The commonware codec (commonware-codec 2026.10.1), the binary format of
Tempo's consensus artifacts: finalization certificates, votes, DKG outcomes and dealer logs.

Conventions, as in Rust:
  fixed integers (u8..u128) are big-endian; bool is one byte 0/1;
  UInt (varint) is LEB128 with a unique encoding (no zero continuation byte, no overflow);
  usize (lengths) is a u32 varint checked against a range;
  [u8; N] is N raw bytes; Bytes/Vec<T> is a usize length then the items;
  Option<T> is a bool then T; ordered::Set is a Vec whose items are sorted and unique;
  ordered::Map is the key Set then a Vec of exactly as many values;
  enums are a u8 tag then the variant.

Types are declared once as forms: DEFCODEC declares a struct and its wire shape, DEFCWUNION
a tagged enum, DEFCWTYPE a hand-written leaf. Type specs (keywords and lists below) compile to
straight-line reader and writer code at macroexpansion time.

  :u8 :u16 :u32 :u64 :u128 :bool              fixed
  :varint (:varint BITS) :svarint             UInt<u64>/UInt<uBITS>, SInt (zigzag)
  :usize (:usize :min A :max B)               u32 varint length, range-checked
  (:array N)                                  [u8; N], an octet vector
  :bytes (:bytes :min A :max B)               length-prefixed octets
  (:vec SPEC :min A :max B)                   length-prefixed list
  (:set SPEC :min A :max B)                   sorted unique list
  (:map KSPEC VSPEC :min A :max B)            list of (key . value), keys sorted unique
  (:option SPEC)                              NIL or a value
  (:tuple SPEC...)                            list of values
  NAME                                        a DEFCODEC, DEFCWUNION or DEFCWTYPE type
:min and :max are forms, evaluated when reading.")
  (:export
   #:cw-error #:cw-fail
   ;; buffers
   #:cw-writer #:make-cw-writer #:cw-writer-octets #:cw-writer-position
   #:cw-reader #:make-cw-reader #:cw-reader-position #:cw-reader-remaining #:cw-reader-end
   ;; primitives
   #:write-u8 #:read-u8 #:write-uint #:read-uint #:write-varint #:read-varint #:varint-size
   #:write-svarint #:read-svarint #:write-bool #:read-bool
   #:write-raw #:read-raw #:write-length #:read-length
   #:varint-octets
   ;; declarative types
   #:defcodec #:defcwunion #:defcwtype #:cw-type-p
   #:cw-write #:cw-read #:cw-encode #:cw-decode
   #:cw-order<
   ;; namespaces
   #:union-unique #:namespace-union))

(in-package #:clempo.cwcodec)

(define-condition cw-error (error)
  ((message :initarg :message :reader cw-error-message))
  (:report (lambda (c s) (format s "commonware codec: ~A" (cw-error-message c)))))

(defun cw-fail (fmt &rest args)
  (error 'cw-error :message (apply #'format nil fmt args)))

;;; Writer: a growing octet buffer.

(defstruct (cw-writer (:constructor make-cw-writer (&optional (capacity 64)
                                                    &aux (buffer (make-octets capacity)))))
  (buffer (make-octets 0) :type octets)
  (position 0 :type (and fixnum unsigned-byte)))

(declaim (inline ensure-room))
(defun ensure-room (w n)
  (declare (type cw-writer w) (type fixnum n))
  (let ((buf (cw-writer-buffer w))
        (need (+ (cw-writer-position w) n)))
    (when (> need (length buf))
      (let ((new (make-octets (max need (* 2 (length buf))))))
        (replace new buf :end2 (cw-writer-position w))
        (setf (cw-writer-buffer w) new)))))

(defun cw-writer-octets (w)
  "The bytes written so far, as a fresh octet vector."
  (subseq (cw-writer-buffer w) 0 (cw-writer-position w)))

(declaim (inline write-u8))
(defun write-u8 (w b)
  (declare (type cw-writer w) (type octet b))
  (ensure-room w 1)
  (setf (aref (cw-writer-buffer w) (cw-writer-position w)) b)
  (incf (cw-writer-position w))
  w)

(defun write-raw (w octets &key (start 0) (end (length octets)))
  (declare (type cw-writer w) (type octets octets) (type fixnum start end))
  (let ((n (- end start)))
    (ensure-room w n)
    (replace (cw-writer-buffer w) octets :start1 (cw-writer-position w) :start2 start :end2 end)
    (incf (cw-writer-position w) n))
  w)

(defun write-uint (w value nbytes)
  "Fixed-width big-endian unsigned integer."
  (declare (type cw-writer w) (type unsigned-byte value) (type fixnum nbytes))
  (invariant (<= (integer-length value) (* 8 nbytes)) "~D does not fit in ~D bytes" value nbytes)
  (ensure-room w nbytes)
  (let ((buf (cw-writer-buffer w)) (pos (cw-writer-position w)))
    (loop for i from (1- nbytes) downto 0
          for shift of-type fixnum from 0 by 8
          do (setf (aref buf (+ pos i)) (ldb (byte 8 shift) value))))
  (incf (cw-writer-position w) nbytes)
  w)

(defun write-varint (w value &optional (bits 64))
  "UInt<uBITS>: LEB128, seven bits per byte, low bits first."
  (declare (type cw-writer w) (type unsigned-byte value))
  (invariant (<= (integer-length value) bits) "varint ~D wider than ~D bits" value bits)
  (loop while (>= value #x80)
        do (write-u8 w (logior #x80 (logand value #x7f)))
           (setf value (ash value -7)))
  (write-u8 w value))

(defun varint-size (value)
  (max 1 (ceiling (integer-length value) 7)))

(defun varint-octets (value)
  (let ((w (make-cw-writer 10))) (write-varint w value) (cw-writer-octets w)))

(defun write-svarint (w value &optional (bits 64))
  "SInt: zigzag, then varint."
  (write-varint w (if (minusp value) (1- (* -2 value)) (* 2 value)) bits))

(declaim (inline write-bool))
(defun write-bool (w value) (write-u8 w (if value 1 0)))

(defun write-length (w n)
  "usize: a u32 varint."
  (write-varint w n 32))

;;; Reader: a cursor over an octet vector.

(defstruct (cw-reader (:constructor make-cw-reader (octets &key (start 0) (end (length octets))
                                                    &aux (position start))))
  (octets (make-octets 0) :type octets)
  (position 0 :type (and fixnum unsigned-byte))
  (end 0 :type (and fixnum unsigned-byte)))

(declaim (inline cw-reader-remaining))
(defun cw-reader-remaining (r)
  (- (cw-reader-end r) (cw-reader-position r)))

(declaim (inline need))
(defun need (r n)
  (declare (type cw-reader r) (type fixnum n))
  (when (> n (cw-reader-remaining r))
    (cw-fail "end of buffer: need ~D bytes at ~D, have ~D" n (cw-reader-position r)
             (cw-reader-remaining r))))

(declaim (inline read-u8))
(defun read-u8 (r)
  (declare (type cw-reader r))
  (need r 1)
  (prog1 (aref (cw-reader-octets r) (cw-reader-position r))
    (incf (cw-reader-position r))))

(defun read-raw (r n)
  "N bytes as a fresh octet vector."
  (declare (type cw-reader r) (type fixnum n))
  (need r n)
  (let ((pos (cw-reader-position r)))
    (setf (cw-reader-position r) (+ pos n))
    (subseq (cw-reader-octets r) pos (+ pos n))))

(defun read-uint (r nbytes)
  (declare (type cw-reader r) (type fixnum nbytes))
  (need r nbytes)
  (let ((buf (cw-reader-octets r)) (pos (cw-reader-position r)) (v 0))
    (dotimes (i nbytes) (setf v (logior (ash v 8) (aref buf (+ pos i)))))
    (setf (cw-reader-position r) (+ pos nbytes))
    v))

(defun read-varint (r &optional (bits 64))
  "UInt<uBITS> with commonware's canonical-form checks: a zero byte after the first and set
bits beyond BITS are both errors."
  (declare (type cw-reader r) (type fixnum bits))
  (let ((value 0) (shift 0))
    (declare (type fixnum shift))
    (loop
      (let ((byte (read-u8 r)))
        (when (and (zerop byte) (plusp shift))
          (cw-fail "invalid varint: zero continuation byte"))
        (let ((remaining (- bits shift)))
          (when (and (<= remaining 7) (> (integer-length byte) remaining))
            (cw-fail "invalid varint: overflows ~D bits" bits)))
        (setf value (logior value (ash (logand byte #x7f) shift)))
        (when (zerop (logand byte #x80)) (return value))
        (incf shift 7)))))

(defun read-svarint (r &optional (bits 64))
  (let ((u (read-varint r bits)))
    (if (oddp u) (- (ash (1+ u) -1)) (ash u -1))))

(defun read-bool (r)
  (let ((b (read-u8 r)))
    (case b (0 nil) (1 t) (t (cw-fail "invalid bool ~D" b)))))

(defun read-length (r &optional (min 0) max)
  "usize: a u32 varint in [MIN, MAX]."
  (let ((n (read-varint r 32)))
    (when (or (< n min) (and max (> n max)))
      (cw-fail "length ~D not in range [~D, ~A]" n min (or max "..")))
    n))

;;; Ordering used by ordered::Set and ordered::Map: integers numerically, octet vectors
;;; lexicographically (as Rust's Ord on byte arrays), lists element-wise.

(defun cw-order< (a b)
  (etypecase a
    (integer (< a b))
    (vector (let ((n (min (length a) (length b))))
              (dotimes (i n (< (length a) (length b)))
                (let ((x (aref a i)) (y (aref b i)))
                  (cond ((< x y) (return t))
                        ((> x y) (return nil)))))))
    (list (loop for x in a for y in b
                do (cond ((cw-order< x y) (return t))
                         ((cw-order< y x) (return nil)))
                finally (return (< (length a) (length b)))))))

;;; Namespaces (commonware_utils::union and union_unique).

(defun namespace-union (a b)
  "union(a, b): plain concatenation."
  (octets-concat a b))

(defun union-unique (namespace message)
  "union_unique(namespace, msg) = varint(len namespace) || namespace || msg."
  (octets-concat (varint-octets (length namespace)) namespace message))

;;; The type registry. Each named type maps to a writer and a reader function name; the spec
;;; compiler emits direct calls, so the registry is consulted at macroexpansion time only.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cw-types* (make-hash-table :test 'eq)
    "type name -> (writer-function . reader-function)")

  (defun register-cw-type (name writer reader)
    (setf (gethash name *cw-types*) (cons writer reader))
    name)

  (defun cw-type-entry (name)
    (or (gethash name *cw-types*)
        (error "commonware codec: unknown type ~S" name)))

  (defun spec-head (spec) (if (consp spec) (first spec) spec))

  (defun spec-arity (head)
    "How many positional arguments a list spec with HEAD takes before its keywords."
    (case head
      ((:varint :svarint :array :vec :set :option) 1)
      (:map 2)
      ((:bytes :usize) 0)
      (t nil)))

  (defun spec-keys (spec)
    "The keyword arguments of a list spec: (:vec :u8 :max 10) -> (:max 10)."
    (let ((n (spec-arity (first spec))))
      (if n (nthcdr (1+ n) spec) '())))

  (defun spec-args (spec)
    (let ((n (spec-arity (first spec))))
      (if n (subseq (rest spec) 0 n) (rest spec))))

  (defparameter +fixed-widths+ '((:u8 . 1) (:u16 . 2) (:u32 . 4) (:u64 . 8) (:u128 . 16)))

  (defun writer-form (spec w v)
    "A form writing value V (a form, evaluated once) of type SPEC to writer W (a symbol)."
    (let ((head (spec-head spec)))
      (case head
        ((:u8 :u16 :u32 :u64 :u128)
         (if (eq head :u8)
             `(write-u8 ,w ,v)
             `(write-uint ,w ,v ,(cdr (assoc head +fixed-widths+)))))
        (:bool `(write-bool ,w ,v))
        (:varint `(write-varint ,w ,v ,(if (consp spec) (second spec) 64)))
        (:svarint `(write-svarint ,w ,v ,(if (consp spec) (second spec) 64)))
        (:usize `(write-length ,w ,v))
        (:array (let ((x (gensym "X")) (n (second spec)))
                  `(let ((,x ,v))
                     (invariant (= (length ,x) ,n) "expected ~D bytes, got ~D" ,n (length ,x))
                     (write-raw ,w ,x))))
        (:bytes (let ((x (gensym "X")))
                  `(let ((,x ,v)) (write-length ,w (length ,x)) (write-raw ,w ,x))))
        ((:vec :set)
         (let ((x (gensym "X")) (item (gensym "ITEM")))
           `(let ((,x ,v))
              (write-length ,w (length ,x))
              (dolist (,item ,x) ,(writer-form (second spec) w item)))))
        (:map
         (let ((x (gensym "X")) (cell (gensym "CELL")))
           `(let ((,x ,v))
              (write-length ,w (length ,x))
              (dolist (,cell ,x) ,(writer-form (second spec) w `(car ,cell)))
              (write-length ,w (length ,x))
              (dolist (,cell ,x) ,(writer-form (third spec) w `(cdr ,cell))))))
        (:option
         (let ((x (gensym "X")))
           `(let ((,x ,v))
              (write-bool ,w ,x)
              (when ,x ,(writer-form (second spec) w x)))))
        (:tuple
         (let ((x (gensym "X")))
           `(let ((,x ,v))
              ,@(loop for s in (rest spec)
                      collect (writer-form s w `(pop ,x))))))
        (t `(,(car (cw-type-entry head)) ,w ,v)))))

  (defun reader-form (spec r)
    "A form reading a value of type SPEC from reader R (a symbol)."
    (let ((head (spec-head spec)))
      (destructuring-bind (&key (min 0) max &allow-other-keys)
          (and (consp spec) (spec-keys spec))
        (case head
          (:u8 `(read-u8 ,r))
          ((:u16 :u32 :u64 :u128) `(read-uint ,r ,(cdr (assoc head +fixed-widths+))))
          (:bool `(read-bool ,r))
          (:varint `(read-varint ,r ,(if (consp spec) (second spec) 64)))
          (:svarint `(read-svarint ,r ,(if (consp spec) (second spec) 64)))
          (:usize `(read-length ,r ,min ,max))
          (:array `(read-raw ,r ,(second spec)))
          (:bytes `(read-raw ,r (read-length ,r ,min ,max)))
          (:vec `(loop repeat (read-length ,r ,min ,max)
                       collect ,(reader-form (second spec) r)))
          (:set `(check-sorted-unique
                  (loop repeat (read-length ,r ,min ,max)
                        collect ,(reader-form (second spec) r))))
          (:map (let ((keys (gensym "KEYS")))
                  `(let ((,keys (check-sorted-unique
                                 (loop repeat (read-length ,r ,min ,max)
                                       collect ,(reader-form (second spec) r)))))
                     (read-length ,r (length ,keys) (length ,keys))
                     (loop for k in ,keys
                           collect (cons k ,(reader-form (third spec) r))))))
          (:option `(when (read-bool ,r) ,(reader-form (second spec) r)))
          (:tuple `(list ,@(loop for s in (spec-args spec) collect (reader-form s r))))
          (t `(,(cdr (cw-type-entry head)) ,r)))))))

(defun check-sorted-unique (items)
  (loop for (a b) on items
        while b
        unless (cw-order< a b)
          do (cw-fail "set items must be sorted and unique"))
  items)

;;; Generic entry points.

(defun cw-type-p (name) (nth-value 1 (gethash name *cw-types*)))

(defun cw-write (type writer value)
  "Write VALUE as TYPE (a registered type name) to WRITER."
  (funcall (car (cw-type-entry type)) writer value))

(defun cw-read (type reader)
  (funcall (cdr (cw-type-entry type)) reader))

(defun cw-encode (type value)
  "Encode VALUE as TYPE into a fresh octet vector."
  (let ((w (make-cw-writer)))
    (cw-write type w value)
    (cw-writer-octets w)))

(defun cw-decode (type octets &key partial (start 0) (end (length octets)))
  "Decode TYPE from OCTETS. Like Rust's Decode, leftover bytes are an error unless PARTIAL
\(Rust's Read); returns (values value end-position)."
  (let* ((r (make-cw-reader octets :start start :end end))
         (v (cw-read type r)))
    (when (and (not partial) (plusp (cw-reader-remaining r)))
      (cw-fail "~D extra bytes after ~S" (cw-reader-remaining r) type))
    (values v (cw-reader-position r))))

;;; Declarations.

(defmacro defcwtype (name &key write read)
  "A leaf type with hand-written WRITE (writer value) and READ (reader) functions, given as
function names."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-cw-type ',name ',write ',read)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest parts)
    (intern (format nil "~{~A~}" parts))))

(defmacro defcodec (name-and-options &body slots)
  "Declare a struct and its commonware wire shape, fields in wire order.
  (defcodec proposal (round round) (parent :varint) (payload (:array 32)))
NAME-AND-OPTIONS is NAME or (NAME &rest defstruct-options). Each slot is
\(SLOT-NAME SPEC &key default type doc). Defines the struct, WRITE-<NAME> (writer value),
READ-<NAME> (reader) and registers NAME for CW-ENCODE / CW-DECODE and other specs."
  (destructuring-bind (name &rest options) (if (consp name-and-options)
                                               name-and-options
                                               (list name-and-options))
    (let* ((writer (symbolicate "WRITE-" name))
           (reader (symbolicate "READ-" name))
           (constructor (symbolicate "MAKE-" name))
           (conc (or (second (assoc :conc-name options)) (symbolicate name "-")))
           (temps (loop for s in slots collect (gensym (symbol-name (first s)))))
           (w (gensym "W")) (r (gensym "R")) (v (gensym "V")))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (register-cw-type ',name ',writer ',reader))
         (defstruct (,name (:constructor ,constructor
                               (&key ,@(loop for s in slots
                                             collect (list (first s) (getf (cddr s) :default)))))
                           ,@(remove :constructor options :key #'first))
           ,@(loop for (slot nil . keys) in slots
                   collect `(,slot ,(getf keys :default)
                                   ,@(when (getf keys :type) `(:type ,(getf keys :type)))
                                   ,@(when (getf keys :doc) `(:documentation ,(getf keys :doc))))))
         (defun ,writer (,w ,v)
           (declare (type cw-writer ,w) (type ,name ,v))
           ,@(loop for (slot spec) in slots
                   collect (writer-form spec w `(,(symbolicate conc slot) ,v)))
           ,w)
         (defun ,reader (,r)
           (declare (type cw-reader ,r))
           (let* ,(loop for (nil spec) in slots for g in temps
                        collect `(,g ,(reader-form spec r)))
             (,constructor ,@(loop for s in slots for g in temps
                                   append (list (intern (symbol-name (first s)) :keyword) g)))))
         ',name))))

(defmacro defcwunion (name &body variants)
  "A Rust enum written as a u8 tag then the variant.
Each variant is (TAG TYPE) where TYPE is a DEFCODEC struct (the value is that struct), or
\(TAG KEY SPEC) where the value is (KEY . payload) and SPEC is the payload's shape, or
\(TAG KEY) for a unit variant whose value is the keyword KEY."
  (let ((writer (symbolicate "WRITE-" name))
        (reader (symbolicate "READ-" name))
        (w (gensym "W")) (r (gensym "R")) (v (gensym "V")) (tag (gensym "TAG")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-cw-type ',name ',writer ',reader))
       (defun ,writer (,w ,v)
         (cond
           ,@(loop for (n type spec) in variants
                   collect (cond ((and (keywordp type) spec)
                                  `((and (consp ,v) (eq (car ,v) ,type))
                                    (write-u8 ,w ,n)
                                    ,(writer-form spec w `(cdr ,v))))
                                 ((keywordp type)
                                  `((eq ,v ,type) (write-u8 ,w ,n)))
                                 (t `((typep ,v ',type)
                                      (write-u8 ,w ,n)
                                      ,(writer-form type w v)))))
           (t (unreachable "~S is not a variant of ~S" ,v ',name)))
         ,w)
       (defun ,reader (,r)
         (let ((,tag (read-u8 ,r)))
           (case ,tag
             ,@(loop for (n type spec) in variants
                     collect `(,n ,(cond ((and (keywordp type) spec)
                                          `(cons ,type ,(reader-form spec r)))
                                         ((keywordp type) type)
                                         (t (reader-form type r)))))
             (t (cw-fail "invalid ~(~A~) tag ~D" ',name ,tag)))))
       ',name)))
