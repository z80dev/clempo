(in-package #:clempo.contract)

;;; Storable types (crates/precompiles/src/storage/types). A type is written as a form:
;;;   u8 .. u256, i8 .. i256, bool, address, b256, b1 .. b24   primitives (Layout::Bytes)
;;;   string, bytes                                           dynamic, one slot
;;;   (mapping K V), (vec T), (array T N), (set T)
;;;   a DEFSTORABLE struct name, a DEFENUM name
;;; STYPE is the resolved descriptor; it carries everything READ and WRITE need.

(defstruct (struct-layout (:constructor make-struct-layout (name fields slots constructor)))
  name
  fields                          ; vector of SFIELD
  (slots 1 :type fixnum)
  constructor)                    ; function of the field values in order

(defstruct (sfield (:constructor make-sfield (name spec slot offset reader)))
  name spec slot offset reader
  (stype-cache nil))

(defun sfield-stype (f)
  (or (sfield-stype-cache f) (setf (sfield-stype-cache f) (stype (sfield-spec f)))))

(defstruct (stype (:constructor %make-stype))
  (spec nil)
  (kind nil :type keyword)       ; :prim :string :bytes :mapping :vec :array :set :struct
  (word nil)                     ; for :prim, (:uint bits) (:int bits) (:bool) (:address) (:fixed n) (:enum count)
  (bytes 32 :type fixnum)        ; StorableType::BYTES
  (slots 1 :type fixnum)         ; StorableType::SLOTS
  (packable nil)                 ; IS_PACKABLE: Layout::Bytes(n) with n < 32
  (dynamic nil)                  ; IS_DYNAMIC
  (key nil)                      ; mapping key stype
  (elem nil)                     ; mapping value / vec / array / set element stype
  (count 0 :type fixnum)         ; array length
  (struct nil))                  ; :struct, the STRUCT-LAYOUT

(defvar *stypes* (make-hash-table :test 'equal) "spec -> stype")

(defun prim (spec word bytes)
  (%make-stype :spec spec :kind :prim :word word :bytes bytes :slots 1 :packable (< bytes 32)))

(defun primitive-word (spec)
  "(values word-kind bytes) for a primitive spec symbol, or NIL."
  (when (symbolp spec)
    (let ((name (symbol-name spec)))
      (flet ((digits (start) (and (> (length name) start)
                                  (every #'digit-char-p (subseq name start))
                                  (parse-integer name :start start))))
        (cond ((string= name "BOOL") (values '(:bool) 1))
              ((string= name "ADDRESS") (values '(:address) 20))
              ((string= name "B256") (values '(:fixed 32) 32))
              ((and (char= (char name 0) #\U) (digits 1))
               (let ((bits (digits 1))) (values (list :uint bits) (floor bits 8))))
              ((and (char= (char name 0) #\I) (digits 1))
               (let ((bits (digits 1))) (values (list :int bits) (floor bits 8))))
              ((and (char= (char name 0) #\B) (digits 1))
               (let ((n (digits 1))) (values (list :fixed n) n))))))))

(defun resolve-stype (spec)
  (cond
    ((and (symbolp spec) (get spec 'storable-layout))
     (let ((layout (get spec 'storable-layout)))
       (%make-stype :spec spec :kind :struct :bytes (* 32 (struct-layout-slots layout))
                    :slots (struct-layout-slots layout) :struct layout)))
    ((and (symbolp spec) (get spec 'enum-variants))
     (prim spec (list :enum (length (get spec 'enum-variants))) 1))
    ((and (symbolp spec) (string= (symbol-name spec) "STRING"))
     (%make-stype :spec spec :kind :string :bytes 32 :slots 1 :dynamic t))
    ((and (symbolp spec) (string= (symbol-name spec) "BYTES"))
     (%make-stype :spec spec :kind :bytes :bytes 32 :slots 1 :dynamic t))
    ((symbolp spec)
     (multiple-value-bind (word bytes) (primitive-word spec)
       (unless word (error "unknown storable type ~S" spec))
       (prim spec word bytes)))
    ((consp spec)
     (let ((head (symbol-name (first spec))))
       (cond
         ((string= head "MAPPING")
          (%make-stype :spec spec :kind :mapping :bytes 32 :slots 1
                       :key (stype (second spec)) :elem (stype (third spec))))
         ((string= head "VEC")
          (%make-stype :spec spec :kind :vec :bytes 32 :slots 1 :dynamic t :elem (stype (second spec))))
         ((string= head "SET")
          (%make-stype :spec spec :kind :set :bytes 64 :slots 2 :dynamic t :elem (stype (second spec))))
         ((string= head "ARRAY")
          (let* ((elem (stype (second spec)))
                 (n (third spec))
                 (slots (if (and (eq (stype-kind elem) :prim) (< (stype-bytes elem) 32))
                            (ceiling n (floor 32 (stype-bytes elem)))
                            (* n (stype-slots elem)))))
            (%make-stype :spec spec :kind :array :bytes (* 32 slots) :slots slots :elem elem :count n)))
         (t (error "unknown storable type ~S" spec)))))
    (t (error "unknown storable type ~S" spec))))

(defun stype (spec)
  "The descriptor of the storable type SPEC (cached)."
  (if (stype-p spec)
      spec
      (or (gethash spec *stypes*)
          (setf (gethash spec *stypes*) (resolve-stype spec)))))

(defun type-slots (spec) (stype-slots (stype spec)))
(defun type-bytes (spec) (stype-bytes (stype spec)))
(defun type-packable-p (spec) (stype-packable (stype spec)))
(defun type-dynamic-p (spec) (stype-dynamic (stype spec)))

;;; Word conversions (FromWord / StorageKey).

(defun word->value (word-kind w)
  (ecase (first word-kind)
    (:uint (let ((bits (second word-kind)))
             (if (and (< bits 256) (>= w (ash 1 bits))) (panic +panic-under-overflow+) w)))
    (:int (let ((bits (second word-kind)))
            (when (and (< bits 256) (>= w (ash 1 bits))) (panic +panic-under-overflow+))
            (if (logbitp (1- bits) w) (- w (ash 1 bits)) w)))
    (:bool (/= w 0))
    (:address (ldb (byte 160 0) w))
    (:fixed (ldb (byte (* 8 (second word-kind)) 0) w))
    (:enum (when (> w 255) (panic +panic-under-overflow+))
     (if (< w (second word-kind)) w (panic +panic-enum-conversion+)))))

(defun value->word (word-kind v)
  (ecase (first word-kind)
    ((:uint :address :fixed :enum) v)
    (:int (ldb (byte (second word-kind) 0) v))
    (:bool (if v 1 0))))

(defun mapping-key-word (key-stype key)
  "The key word hashed into a mapping slot: the key's raw bytes left-padded (StorageKey)."
  (let ((word (stype-word key-stype)))
    (ecase (first word)
      ((:uint :address :fixed :enum) key)
      (:int (ldb (byte (second word) 0) key))
      (:bool (if key 1 0)))))

(defun mapping-slot (key-word base-slot)
  "keccak256(be32(KEY-WORD) || be32(BASE-SLOT)) as an integer. The buffer is per call
\(stack-allocated), so concurrent callers (parallel eth_calls) never share it."
  (declare (type (unsigned-byte 256) key-word base-slot))
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (loop for i from 0 below 32
          do (setf (aref buf (- 31 i)) (ldb (byte 8 (* 8 i)) key-word)
                   (aref buf (- 63 i)) (ldb (byte 8 (* 8 i)) base-slot)))
    (clempo.keccak:keccak256-integer buf)))

(defun data-slot (slot)
  "keccak256(be32(SLOT)): where a dynamic value's data starts."
  (clempo.keccak:keccak256-integer (integer->octets slot 32)))

(declaim (inline slot+))
(defun slot+ (slot n) (ldb (byte 256 0) (+ slot n)))

;;; Places: a typed location (Rust's handlers). LOCATION-free accessors build them; only the
;;; verbs below touch storage.

(defstruct (place (:constructor make-place (address slot type &optional offset)))
  (address 0 :type (integer 0))
  (slot 0 :type (integer 0))
  (type nil :type stype)
  (offset nil))                   ; NIL: full slot; an integer: packed at that byte offset

(defmethod print-object ((p place) s)
  (print-unreadable-object (p s :type t)
    (format s "~X/~X~@[+~D~] ~S" (place-address p) (place-slot p) (place-offset p)
            (stype-spec (place-type p)))))

(defun place (address slot spec &optional offset)
  (make-place address slot (stype spec) offset))

(defvar *transient* nil "True while a T-READ/T-WRITE/T-DELETE runs: loads and stores are TLOAD/TSTORE.")

(declaim (inline %load %store))
(defun %load (address slot) (if *transient* (tload address slot) (sload address slot)))
(defun %store (address slot value) (if *transient* (tstore address slot value) (sstore address slot value)))

;;; Packing (storage/packing.rs).

(defun extract-word (slot-value offset bytes)
  (ldb (byte (* 8 bytes) (* 8 offset)) slot-value))

(defun insert-word (current field-word offset bytes)
  (dpb (ldb (byte (* 8 bytes) 0) field-word) (byte (* 8 bytes) (* 8 offset)) current))

(defun element-location (index elem-bytes)
  "calc_element_loc: (values slot-offset byte-offset) of element INDEX in a packed run."
  (let ((per-slot (floor 32 elem-bytes)))
    (values (floor index per-slot) (* (mod index per-slot) elem-bytes))))

(defun packed-slot-count (n elem-bytes)
  (ceiling n (floor 32 elem-bytes)))

;;; Loading and storing, by kind.

(defun load-prim (st address slot offset)
  (let ((w (%load address slot)))
    (word->value (stype-word st) (if offset (extract-word w offset (stype-bytes st)) w))))

(defun store-prim (st address slot offset value)
  (let ((w (value->word (stype-word st) value)))
    (if offset
        (%store address slot (insert-word (%load address slot) w offset (stype-bytes st)))
        (%store address slot w))))

(defun delete-prim (st address slot offset)
  (if offset
      (let ((current (%load address slot)))
        (%store address slot (dpb 0 (byte (* 8 (stype-bytes st)) (* 8 offset)) current)))
      (%store address slot 0)))

;;; Strings and bytes (bytes_like.rs).

(defun long-string-p (w) (logbitp 0 w))

(defun string-length-of (w long)
  (if long
      (let ((len (ash (1- w) -1)))
        (when (> len #xffffffff) (panic +panic-under-overflow+))
        len)
      (let ((len (floor (ldb (byte 8 0) w) 2)))
        (when (> len 31) (fatal "short string length ~D exceeds maximum of 31 bytes" len))
        len)))

(defun load-bytes-like (st address slot)
  (let* ((base (%load address slot))
         (long (long-string-p base))
         (len (string-length-of base long))
         (data (make-octets len)))
    (if long
        (let ((start (data-slot slot)))
          (loop for i from 0 below (ceiling len 32)
                do (let ((chunk (%load address (slot+ start i))))
                     (loop for j from 0 below (min 32 (- len (* 32 i)))
                           do (setf (aref data (+ (* 32 i) j)) (ldb (byte 8 (* 8 (- 31 j))) chunk))))))
        (loop for j from 0 below len
              do (setf (aref data j) (ldb (byte 8 (* 8 (- 31 j))) base))))
    (if (eq (stype-kind st) :string)
        (handler-case (clempo.abi:utf8-decode data)
          (clempo.abi:abi-decode-error (e)
            (fatal "Invalid UTF-8 in stored string: ~A" (clempo.abi:abi-error-message e))))
        data)))

(defun store-bytes-like (value address slot init)
  (let* ((bytes (if (stringp value) (clempo.abi:utf8-encode value) value))
         (len (length bytes))
         (long (> len 31)))
    (when (and (not init) (spec>= :t5))
      (let ((prev (%load address slot)))
        (when (long-string-p prev)
          (let ((prev-chunks (ceiling (string-length-of prev t) 32))
                (new-chunks (if long (ceiling len 32) 0)))
            (when (> prev-chunks new-chunks)
              (let ((start (data-slot slot)))
                (loop for i from new-chunks below prev-chunks
                      do (%store address (slot+ start i) 0))))))))
    (if (not long)
        (let ((w 0))
          (loop for j from 0 below len
                do (setf w (dpb (aref bytes j) (byte 8 (* 8 (- 31 j))) w)))
          (%store address slot (logior w (* 2 len))))
        (progn
          (%store address slot (1+ (* 2 len)))
          (let ((start (data-slot slot)))
            (loop for i from 0 below (ceiling len 32)
                  do (let ((w 0))
                       (loop for j from 0 below (min 32 (- len (* 32 i)))
                             do (setf w (dpb (aref bytes (+ (* 32 i) j)) (byte 8 (* 8 (- 31 j))) w)))
                       (%store address (slot+ start i) w))))))))

(defun delete-bytes-like (address slot)
  (let ((base (%load address slot)))
    (when (long-string-p base)
      (let ((start (data-slot slot)))
        (loop for i from 0 below (ceiling (string-length-of base t) 32)
              do (%store address (slot+ start i) 0))))
    (%store address slot 0)))

(defun string-length (place)
  "BytesLikeHandler::len: one SLOAD."
  (let ((base (sload (place-address place) (place-slot place))))
    (string-length-of base (long-string-p base))))

;;; Structs (DEFSTORABLE; precompiles-macros/src/storable.rs).

(defun direct-fields (layout)
  "The fields the Storable derive loads and stores: every field but mappings, which it
treats as indirect (precompiles-macros/src/storable.rs)."
  (remove :mapping (struct-layout-fields layout) :key (lambda (f) (stype-kind (sfield-stype f)))))

(defun load-struct (st address slot)
  (let* ((layout (stype-struct st))
         (fields (direct-fields layout))
         (cached 0)
         (loaded '()))
    (loop for idx from 0 below (length fields)
          for f = (aref fields idx)
          for fst = (sfield-stype f)
          for at = (slot+ slot (sfield-slot f))
          do (push (cons f (cond ((not (stype-packable fst))
                                  (load-value fst address at nil))
                                 (t
                                  (unless (and (> idx 0) (= (sfield-slot f) (sfield-slot (aref fields (1- idx)))))
                                    (setf cached (%load address at)))
                                  (word->value (stype-word fst)
                                               (extract-word cached (sfield-offset f) (stype-bytes fst))))))
                   loaded))
    ;; mapping fields have no value of their own (Default in Rust)
    (apply (struct-layout-constructor layout)
           (map 'list (lambda (f) (cdr (assoc f loaded))) (struct-layout-fields layout)))))

(defun store-struct (st address slot value init)
  (let* ((layout (stype-struct st))
         (fields (direct-fields layout))
         (n (length fields))
         (pending 0)
         (pending-offset nil))
    (flet ((flush ()
             (when pending-offset
               (%store address (slot+ slot pending-offset) pending)
               (setf pending-offset nil)))
           (fresh (off)
             ;; T4+: a packed group starts from zero; before, from the stored word.
             (setf pending (if (spec>= :t4) 0 (%load address (slot+ slot off)))
                   pending-offset off)))
      (loop for idx from 0 below n
            for f = (aref fields idx)
            for fst = (sfield-stype f)
            for off = (sfield-slot f)
            for v = (funcall (sfield-reader f) value)
            for next = (and (< (1+ idx) n) (aref fields (1+ idx)))
            for should-store = (or (null next) (/= off (sfield-slot next))
                                   (not (stype-packable (sfield-stype next))))
            do (cond ((and (stype-packable fst) (> idx 0)
                           (= off (sfield-slot (aref fields (1- idx)))))
                      (setf pending (insert-word pending (value->word (stype-word fst) v)
                                                 (sfield-offset f) (stype-bytes fst))))
                     ((stype-packable fst)
                      (flush)
                      (fresh off)
                      (setf pending (insert-word pending (value->word (stype-word fst) v)
                                                 (sfield-offset f) (stype-bytes fst))))
                     (t
                      (flush)
                      (store-at fst address (slot+ slot off) nil v
                                   (and init (stype-dynamic fst)))))
               (when (and pending-offset should-store) (flush))))))

(defun delete-struct (st address slot)
  (let* ((layout (stype-struct st))
         (fields (direct-fields layout)))
    (loop for f across fields
          when (stype-dynamic (sfield-stype f))
            do (delete-value (sfield-stype f) address (slot+ slot (sfield-slot f)) nil))
    (loop for k from 0 below (struct-layout-slots layout)
          when (loop for f across fields
                     thereis (and (not (stype-dynamic (sfield-stype f)))
                                  (<= (sfield-slot f) k)
                                  (< k (+ (sfield-slot f) (stype-slots (sfield-stype f))))))
            do (%store address (slot+ slot k) 0))))

;;; Dynamic arrays (vec.rs).

(defun load-checked-length (address slot)
  (let ((raw (%load address slot)))
    (when (> raw #xffffffff) (panic +panic-under-overflow+))
    raw))

(defun vec-packed-p (elem) (<= (stype-bytes elem) 16))

(defun element-place-in (address data-start elem index packed)
  (if packed
      (multiple-value-bind (s o) (element-location index (stype-bytes elem))
        (make-place address (slot+ data-start s) elem o))
      (make-place address (slot+ data-start (* index (stype-slots elem))) elem nil)))

(defun load-vec (st address slot)
  (let ((len (load-checked-length address slot))
        (elem (stype-elem st)))
    (if (zerop len)
        '()
        (let ((start (data-slot slot)))
          (if (vec-packed-p elem)
              (let* ((bytes (stype-bytes elem))
                     (per (floor 32 bytes)))
                (loop for s from 0 below (packed-slot-count len bytes)
                      nconc (let ((w (%load address (slot+ start s))))
                              (loop for k from 0 below (min per (- len (* s per)))
                                    collect (word->value (stype-word elem)
                                                         (extract-word w (* k bytes) bytes))))))
              (loop for i from 0 below len
                    collect (load-value elem address (slot+ start (* i (stype-slots elem))) nil)))))))

(defun clear-elements (elem address start from to)
  (when (< from to)
    (if (vec-packed-p elem)
        (loop for s from (packed-slot-count from (stype-bytes elem))
                below (packed-slot-count to (stype-bytes elem))
              do (%store address (slot+ start s) 0))
        (loop for i from from below to
              do (delete-value elem address (slot+ start (* i (stype-slots elem))) nil)))))

(defun store-packed-elements (elem address start values)
  (let* ((bytes (stype-bytes elem))
         (per (floor 32 bytes))
         (vec (coerce values 'simple-vector))
         (n (length vec)))
    (loop for s from 0 below (packed-slot-count n bytes)
          do (let ((w 0))
               (loop for k from 0 below (min per (- n (* s per)))
                     do (setf w (insert-word w (value->word (stype-word elem) (svref vec (+ (* s per) k)))
                                             (* k bytes) bytes)))
               (%store address (slot+ start s) w)))))

(defun store-vec (st address slot values init)
  (let* ((elem (stype-elem st))
         (start (data-slot slot))
         (new-len (length values)))
    (when (and (not init) (spec>= :t5))
      (let ((prev-len (load-checked-length address slot)))
        (when (> prev-len new-len)
          (clear-elements elem address start new-len prev-len))))
    (%store address slot new-len)
    (when (plusp new-len)
      (if (vec-packed-p elem)
          (store-packed-elements elem address start values)
          (loop for v in values for i from 0
                do (store-at elem address (slot+ start (* i (stype-slots elem))) nil v nil))))))

(defun delete-vec (st address slot)
  (let ((len (load-checked-length address slot)))
    (%store address slot 0)
    (when (plusp len)
      (clear-elements (stype-elem st) address (data-slot slot) 0 len))))

;;; Fixed arrays (array.rs and the storable_arrays! expansion).

(defun array-packed-p (elem) (and (eq (stype-kind elem) :prim) (< (stype-bytes elem) 32)))

(defun load-array (st address slot)
  (let ((elem (stype-elem st)) (n (stype-count st)))
    (if (array-packed-p elem)
        ;; one SLOAD per element, as the generated code does
        (loop for i from 0 below n
              collect (multiple-value-bind (s o) (element-location i (stype-bytes elem))
                        (word->value (stype-word elem)
                                     (extract-word (%load address (slot+ slot s)) o (stype-bytes elem)))))
        (loop for i from 0 below n
              collect (load-value elem address (slot+ slot (* i (stype-slots elem))) nil)))))

(defun store-array (st address slot values)
  (let ((elem (stype-elem st)))
    (if (array-packed-p elem)
        (store-packed-elements elem address slot values)
        (loop for v in values for i from 0
              do (store-at elem address (slot+ slot (* i (stype-slots elem))) nil v nil)))))

;;; Generic dispatch over kinds.

(defun load-value (st address slot offset)
  (ecase (stype-kind st)
    (:prim (load-prim st address slot offset))
    ((:string :bytes) (load-bytes-like st address slot))
    (:struct (load-struct st address slot))
    (:vec (load-vec st address slot))
    (:set (load-vec st address slot))
    (:array (load-array st address slot))))

(defun store-at (st address slot offset value init)
  (ecase (stype-kind st)
    (:prim (store-prim st address slot offset value))
    ((:string :bytes) (store-bytes-like value address slot init))
    (:struct (store-struct st address slot value init))
    (:vec (store-vec st address slot value init))
    (:array (store-array st address slot value))
    (:set (fatal "Set must be stored via SetHandler::write() to maintain position invariants"))))

(defun delete-value (st address slot offset)
  (ecase (stype-kind st)
    (:prim (delete-prim st address slot offset))
    ((:string :bytes) (delete-bytes-like address slot))
    (:struct (delete-struct st address slot))
    (:vec (delete-vec st address slot))
    (:array (loop for k from 0 below (stype-slots st) do (%store address (slot+ slot k) 0)))
    (:set (let ((elem (stype-elem st)))
            (dolist (v (load-vec st address slot))
              (%store address (mapping-slot (mapping-key-word elem v) (slot+ slot 1)) 0))
            (delete-vec st address slot)))))

;;; The verbs.

(defun read (place)
  "Load the value at PLACE: exactly the SLOADs the reference's Handler::read does."
  (let ((st (place-type place)))
    (case (stype-kind st)
      (:set (set-read place))
      (:mapping (error "cannot read a whole mapping"))
      (t (load-value st (place-address place) (place-slot place) (place-offset place))))))

(defun write (place value)
  "Store VALUE at PLACE (Handler::write): a full slot is one SSTORE, a packed field an SLOAD
and an SSTORE."
  (let ((st (place-type place)))
    (case (stype-kind st)
      (:set (set-write place value))
      (:mapping (error "cannot write a whole mapping"))
      (t (store-at st (place-address place) (place-slot place) (place-offset place) value nil))))
  value)

(defun delete (place)
  "Zero the value at PLACE (Handler::delete)."
  (let ((st (place-type place)))
    (case (stype-kind st)
      (:set (set-delete place))
      (:mapping (error "cannot delete a whole mapping"))
      (t (delete-value st (place-address place) (place-slot place) (place-offset place))))))

(defun t-read (place) (let ((*transient* t)) (read place)))
(defun t-write (place value) (let ((*transient* t)) (write place value)))
(defun t-delete (place) (let ((*transient* t)) (delete place)))

(defun sinc (place delta)
  "Slot<U256>::sinc: load, checked add, store."
  (let ((v (read place)))
    (write place (checked+ v delta))))

(defun sdec (place delta)
  "Slot<U256>::sdec: load, checked sub (StorageDeltaUnderflow is Panic 0x11), store."
  (let ((v (read place)))
    (write place (checked- v delta))))

;;; Navigation. None of these touch storage.

(defun at (place key)
  "The value place of mapping PLACE at KEY, or element KEY of an array place (unchecked)."
  (let ((st (place-type place)))
    (ecase (stype-kind st)
      (:mapping (make-place (place-address place)
                            (mapping-slot (mapping-key-word (stype-key st) key) (place-slot place))
                            (stype-elem st)))
      (:array (let ((elem (stype-elem st)))
                (invariant (< key (stype-count st)) "index ~D out of bounds" key)
                (element-place-in (place-address place) (place-slot place) elem key
                                  (vec-packed-p elem))))
      (:vec (vec-element place key)))))

(defun field (place name)
  "The place of field NAME of the struct at PLACE."
  (let* ((st (place-type place))
         (layout (or (stype-struct st) (error "~S is not a struct place" place)))
         (all (struct-layout-fields layout))
         (f (or (find name all :key #'sfield-name :test #'string=)
                (error "~S has no field ~S" (struct-layout-name layout) name)))
         (fst (sfield-stype f))
         (fields (direct-fields layout))
         (idx (position f fields))
         (shares (and idx
                      (or (and (> idx 0) (= (sfield-slot (aref fields (1- idx))) (sfield-slot f)))
                          (and (< (1+ idx) (length fields))
                               (= (sfield-slot (aref fields (1+ idx))) (sfield-slot f)))))))
    (make-place (place-address place) (slot+ (place-slot place) (sfield-slot f)) fst
                (and shares (stype-packable fst) (sfield-offset f)))))

(defun vec-length (place)
  "VecHandler::len: one SLOAD."
  (let ((*transient* nil))
    (load-checked-length (place-address place) (place-slot place))))

(defun vec-element (place index)
  "Element INDEX of the vec at PLACE, unchecked (vec[index])."
  (let ((elem (stype-elem (place-type place))))
    (element-place-in (place-address place) (data-slot (place-slot place)) elem index
                      (vec-packed-p elem))))

(defun vec-at (place index)
  "VecHandler::at: the element place, or NIL when INDEX >= length (one SLOAD)."
  (when (< index (vec-length place)) (vec-element place index)))

(defun vec-push (place value)
  "VecHandler::push."
  (let* ((len (vec-length place))
         (elem (stype-elem (place-type place)))
         (max-index (if (vec-packed-p elem)
                        (floor #xffffffff (stype-bytes elem))
                        (floor #xffffffff (stype-slots elem)))))
    (when (>= len max-index) (fatal "Vec is at max capacity"))
    (let ((at (vec-element place len)))
      (if (vec-packed-p elem)
          (write at value)
          (store-at elem (place-address at) (place-slot at) nil value t)))
    (sstore (place-address place) (place-slot place) (1+ len))))

(defun vec-pop (place)
  "VecHandler::pop: the last element, or NIL when empty."
  (let ((len (vec-length place)))
    (unless (zerop len)
      (let* ((at (vec-element place (1- len)))
             (v (read at)))
        (delete at)
        (sstore (place-address place) (place-slot place) (1- len))
        v))))

;;; Sets (set.rs): a vec of values at the base slot and a position mapping (1-based u32) at
;;; base + 1.

(defun set-positions (place)
  (make-place (place-address place) (slot+ (place-slot place) 1)
              (stype (list 'mapping (stype-spec (stype-elem (place-type place))) 'u32))))

(defun set-values (place)
  (make-place (place-address place) (place-slot place)
              (stype (list 'vec (stype-spec (stype-elem (place-type place)))))))

(defun set-length (place) (vec-length (set-values place)))

(defun set-contains (place value)
  (/= 0 (read (at (set-positions place) value))))

(defun checked-position (index)
  (if (>= (1+ index) (ash 1 32)) (panic +panic-under-overflow+) (1+ index)))

(defun set-insert (place value)
  "SetHandler::insert: T when VALUE was added."
  (unless (set-contains place value)
    (let ((len (set-length place)))
      (write (at (set-positions place) value) (checked-position len))
      (vec-push (set-values place) value)
      t)))

(defun set-remove (place value)
  "SetHandler::remove: T when VALUE was present."
  (let* ((positions (set-positions place))
         (position (read (at positions value))))
    (unless (zerop position)
      (let* ((values (set-values place))
             (len (vec-length values))
             (last (1- len))
             (index (1- position)))
        (when (/= index last)
          (let ((last-value (read (vec-element values last))))
            (write (at positions last-value) position)
            (write (vec-element values index) last-value)))
        (delete (vec-element values last))
        (sstore (place-address place) (place-slot place) last)
        (delete (at positions value))
        t))))

(defun set-at (place index)
  (let ((values (set-values place)))
    (when (< index (vec-length values))
      (read (vec-element values index)))))

(defun set-range (place start end)
  "SetHandler::read_range."
  (let* ((values (set-values place))
         (len (vec-length values))
         (end (min end len))
         (start (min start end)))
    (loop for i from start below end collect (read (vec-element values i)))))

(defun set-read (place)
  (let* ((values (set-values place))
         (len (vec-length values)))
    (loop for i from 0 below len collect (read (vec-element values i)))))

(defun set-write (place new)
  (let* ((values (set-values place))
         (positions (set-positions place))
         (old-len (vec-length values))
         (new-len (length new)))
    (loop for i from 0 below old-len
          do (delete (at positions (read (vec-element values i)))))
    (loop for v in new for i from 0
          do (write (at positions v) (checked-position i))
             (write (vec-element values i) v))
    (sstore (place-address place) (place-slot place) new-len)
    (loop for i from new-len below old-len do (delete (vec-element values i)))))

(defun set-delete (place)
  (let* ((values (set-values place))
         (positions (set-positions place))
         (len (vec-length values)))
    (loop for i from 0 below len
          do (delete (at positions (read (vec-element values i)))))
    (delete-vec (stype (list 'vec (stype-spec (stype-elem (place-type place)))))
                (place-address place) (place-slot place))))
