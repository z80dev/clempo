(in-package #:clempo.contract)

;;; Layouts. DEFCONTRACT and DEFSTORABLE compute slots and offsets at macroexpansion time
;;; with the reference's allocator (precompiles-macros/src/packing.rs), keep the result as
;;; data on the symbol, and generate place accessors and slot constants from it.

(defstruct (layout-field (:conc-name field-))
  name            ; symbol
  type            ; storable spec
  slot            ; integer (absolute for a contract, relative for a struct)
  offset          ; byte offset in the slot
  bytes           ; StorableType::BYTES
  access          ; :full or :packed
  since           ; fork or NIL
  reserved        ; never read or written
  manual          ; :slot n given
  label           ; Solidity name
  doc)

(defmethod make-load-form ((f layout-field) &optional environment)
  (make-load-form-saving-slots f :environment environment))

(defun camel-case (symbol)
  "LOGO-URI -> \"logoUri\"."
  (let ((parts (split-dashes (string-downcase (symbol-name symbol)))))
    (format nil "~A~{~A~}" (first parts) (mapcar #'string-capitalize (rest parts)))))

(defun pascal-case (symbol)
  (format nil "~{~A~}" (mapcar #'string-capitalize (split-dashes (string-downcase (symbol-name symbol))))))

(defun split-dashes (s)
  (loop with start = 0
        for pos = (position #\- s :start start)
        collect (subseq s start pos)
        while pos do (setf start (1+ pos))))

(defun allocate-fields (specs &key contract)
  "SPECS: (name type &key slot base-slot since reserved doc label). Returns a list of
LAYOUT-FIELDs with the reference allocator's slots, offsets and access."
  (let ((fields '()) (prev nil))
    (dolist (spec specs)
      (destructuring-bind (name type &key slot base-slot since reserved doc label) spec
        (let* ((st (stype type))
               (bytes (stype-bytes st))
               (f (make-layout-field :name name :type type :bytes bytes :since since
                                     :reserved reserved :doc doc :manual (and slot t)
                                     :label (or label (camel-case name)))))
          (cond (slot
                 (setf (field-slot f) slot (field-offset f) 0))
                (base-slot
                 (setf (field-slot f) base-slot (field-offset f) 0 prev f))
                ((null prev)
                 (setf (field-slot f) 0 (field-offset f) 0 prev f))
                (t
                 (let ((pst (stype (field-type prev))))
                   (if (<= (+ (field-offset prev) (stype-bytes pst) bytes) 32)
                       (setf (field-slot f) (field-slot prev)
                             (field-offset f) (+ (field-offset prev) (stype-bytes pst)))
                       (setf (field-slot f) (+ (field-slot prev) (stype-slots pst))
                             (field-offset f) 0))
                   (setf prev f))))
          (push f fields))))
    (setf fields (nreverse fields))
    ;; access: packed when sharing a slot with a neighbour and packable (gen_layout_ctx_expr)
    (loop for (p f n) on (cons nil fields)
          while f
          do (setf (field-access f)
                   (if (and (not (and contract (field-manual f)))
                            (stype-packable (stype (field-type f)))
                            (or (and p (= (field-slot p) (field-slot f)))
                                (and n (= (field-slot n) (field-slot f)))))
                       :packed :full)))
    fields))

(defun check-collisions (name fields)
  "Signal if two fields overlap (the reference's __check_all_collisions)."
  (loop for (a . rest) on fields
        do (loop for b in rest
                 do (let* ((sa (stype (field-type a))) (sb (stype (field-type b)))
                           (a-end (+ (field-slot a) (stype-slots sa)))
                           (b-end (+ (field-slot b) (stype-slots sb))))
                      (when (and (< (field-slot a) b-end) (< (field-slot b) a-end))
                        ;; overlapping slot ranges are fine only for packed bytes that don't overlap
                        (unless (and (= (stype-slots sa) 1) (= (stype-slots sb) 1)
                                     (stype-packable sa) (stype-packable sb)
                                     (or (<= (+ (field-offset a) (stype-bytes sa)) (field-offset b))
                                         (<= (+ (field-offset b) (stype-bytes sb)) (field-offset a))))
                          (error "~S: fields ~S and ~S collide" name (field-name a) (field-name b))))))))

;;; Enums.

(defmacro defenum (name &rest variants)
  "A Storable unit enum: one byte, variants numbered from 0. (enum-value NAME VARIANT)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'enum-variants) ',(coerce variants 'vector))
     (remhash ',name *stypes*)
     ',name))

(defun enum-count (name) (length (get name 'enum-variants)))

(defmacro enum-value (name variant)
  "The number of VARIANT of enum NAME, at compile time."
  (or (position variant (get name 'enum-variants) :test #'string=)
      (error "~S has no variant ~S" name variant)))

(defun enum-name (name value)
  (let ((v (get name 'enum-variants))) (and (< value (length v)) (aref v value))))

;;; Structs.

(defmacro defstorable (name &rest field-specs)
  "A Storable struct (#[derive(Storable)]): fields packed like Solidity's, starting on a
fresh slot. Defines the Lisp struct NAME (MAKE-NAME, NAME-FIELD accessors) for values and
registers the layout for places; (field place 'f) navigates."
  (let* ((specs (mapcar (lambda (s) (if (consp s) s (error "bad field ~S" s))) field-specs))
         (fields (allocate-fields specs))
         (slots (if fields
                    (reduce #'max fields :key (lambda (f) (+ (field-slot f) (type-slots (field-type f)))))
                    0))
         (conc (format nil "~A-" (symbol-name name)))
         (ctor (intern (format nil "%MAKE-~A" (symbol-name name)))))
    `(progn
       (defstruct (,name (:constructor ,(intern (format nil "MAKE-~A" (symbol-name name)))
                             (&key ,@(mapcar (lambda (f) (list (field-name f) (default-value (field-type f))))
                                             fields)))
                         (:constructor ,ctor ,(mapcar #'field-name fields))
                         (:copier nil))
         ,@(mapcar (lambda (f) (list (field-name f) (default-value (field-type f)))) fields))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-struct-layout
          ',name ,slots
          (list ,@(mapcar (lambda (f)
                            `(list ',(field-name f) ',(field-type f) ,(field-slot f) ,(field-offset f)
                                   ',(intern (concatenate 'string conc (symbol-name (field-name f)))
                                             (symbol-package name))))
                          fields))
          ',ctor
          ',fields))
       ',name)))

(defun default-value (spec)
  (let ((st (ignore-errors (stype spec))))
    (cond ((null st) nil)
          ((eq (stype-kind st) :prim) (if (equal (stype-word st) '(:bool)) nil 0))
          ((eq (stype-kind st) :string) "")
          ((eq (stype-kind st) :bytes) '(make-octets 0))
          (t nil))))

(defun register-struct-layout (name slots field-list constructor fields)
  (setf (get name 'storable-layout)
        (make-struct-layout name
                            (coerce (loop for (fname spec slot offset reader) in field-list
                                          collect (make-sfield fname spec slot offset reader))
                                    'vector)
                            slots
                            (lambda (&rest args) (apply constructor args))))
  (setf (get name 'layout-fields) fields)
  ;; any cached descriptor that embeds this struct is stale
  (clrhash *stypes*)
  name)

;;; Contracts.

(defstruct (contract-layout (:constructor make-contract-layout (name address fields)))
  name address fields)

(defvar *contracts* '() "Contract names in definition order.")

(defun contracts () (reverse *contracts*))

(defun layout-of (name)
  (or (get name 'contract-layout) (error "~S is not a contract" name)))

(defun layout-fields (name)
  (let ((l (get name 'contract-layout)))
    (if l (contract-layout-fields l) (get name 'layout-fields))))

(defun layout-field (name field)
  (find field (layout-fields name) :key #'field-name :test #'string=))

(defun contract-address (name) (contract-layout-address (layout-of name)))

(defun mapping-depth (spec)
  (if (and (consp spec) (string= (symbol-name (first spec)) "MAPPING"))
      (1+ (mapping-depth (third spec)))
      0))

(defmacro defcontract (name (&key address prefix) &body field-specs)
  "Declare a precompile's storage layout (#[contract]). Each field is
(name type &key slot base-slot since reserved doc label). Defines one accessor per field,
(FIELD self &optional key...) -> place, taking one key per mapping level, and the constant
+NAME/FIELD+ holding its slot. With PREFIX (a string, or T for the contract's name)
accessors are named PREFIX-FIELD, for packages holding several contracts."
  (let ((fields (allocate-fields field-specs :contract t))
        (prefix (if (eq prefix t) (symbol-name name) prefix)))
    (check-collisions name fields)
    (flet ((accessor (f)
             (if prefix
                 (intern (format nil "~A-~A" prefix (symbol-name (field-name f))) (symbol-package name))
                 (field-name f))))
    (dolist (f fields)
      (when (eq (symbol-package (accessor f)) (find-package '#:cl))
        (error "defcontract ~S: field ~S names a COMMON-LISP symbol; shadow it in the package"
               name (field-name f))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'contract-layout)
               (make-contract-layout ',name ,address ',fields))
         (pushnew ',name *contracts*))
       ,@(loop for f in fields
               for since = (field-since f)
               when since
                 collect `(record-fork-site ,since :field '(,name ,(field-name f)) ,(field-doc f)))
       ,@(loop for f in fields
               for const = (intern (format nil "+~A/~A+" (symbol-name name) (symbol-name (field-name f)))
                                   (symbol-package name))
               for depth = (mapping-depth (field-type f))
               for keys = (loop for i from 1 to depth
                                collect (intern (format nil "KEY~D" i) (symbol-package name)))
               collect `(defconstant ,const ,(field-slot f))
               collect `(declaim (inline ,(accessor f)))
               collect `(defun ,(accessor f) (self &optional ,@keys)
                          ,(format nil "Place of ~A (slot ~D~@[, offset ~D~])."
                                   (field-label f) (field-slot f)
                                   (and (eq (field-access f) :packed) (field-offset f)))
                          (let ((p (make-place self ,const (load-time-value (stype ',(field-type f)))
                                               ,(and (eq (field-access f) :packed) (field-offset f)))))
                            ,@(loop for k in keys collect `(when ,k (setf p (at p ,k))))
                            p)))
       ',name))))

;;; Exporters.

(defun solc-type-string (spec)
  "solc's storage type id, struct and enum ids normalized to t_struct(Name) / t_enum(Name)."
  (let ((st (stype spec)))
    (ecase (stype-kind st)
      (:prim (let ((w (stype-word st)))
               (ecase (first w)
                 (:uint (format nil "t_uint~D" (second w)))
                 (:int (format nil "t_int~D" (second w)))
                 (:bool "t_bool")
                 (:address "t_address")
                 (:fixed (format nil "t_bytes~D" (second w)))
                 (:enum (format nil "t_enum(~A)" (pascal-case spec))))))
      (:string "t_string_storage")
      (:bytes "t_bytes_storage")
      (:mapping (format nil "t_mapping(~A,~A)" (solc-type-string (second spec))
                        (solc-type-string (third spec))))
      (:vec (format nil "t_array(~A)dyn_storage" (solc-type-string (second spec))))
      (:array (format nil "t_array(~A)~D_storage" (solc-type-string (second spec)) (third spec)))
      (:set (format nil "t_set(~A)" (solc-type-string (second spec))))
      (:struct (format nil "t_struct(~A)" (pascal-case spec))))))

(defun layout-json (name)
  "The layout in solc's storageLayout shape (storage entries and struct member types), with
normalized struct and enum type ids, as a clempo.json value."
  (let ((types '()))
    (labels ((note-type (spec)
               (let ((st (stype spec)))
                 (case (stype-kind st)
                   (:struct
                    (let ((id (solc-type-string spec)))
                      (unless (assoc id types :test #'string=)
                        (push (cons id (clempo.json:jobject
                                        "label" (format nil "struct ~A" (pascal-case spec))
                                        "numberOfBytes" (princ-to-string (* 32 (stype-slots st)))
                                        "members" (coerce (mapcar #'entry (get spec 'layout-fields))
                                                          'vector)))
                              types))))
                   (:mapping (note-type (second spec)) (note-type (third spec)))
                   ((:vec :array :set) (note-type (second spec))))))
             (entry (f)
               (note-type (field-type f))
               (clempo.json:jobject "label" (field-label f)
                                    "offset" (field-offset f)
                                    "slot" (princ-to-string (field-slot f))
                                    "type" (solc-type-string (field-type f)))))
      (let ((storage (mapcar #'entry (layout-fields name))))
        (clempo.json:jobject "storage" (coerce storage 'vector)
                             "types" (cons :object (reverse types)))))))

(defun layout-markdown (name)
  "A Markdown slot table of contract NAME."
  (with-output-to-string (s)
    (format s "| Field | Type | Slot | Offset | Access | Since |~%|---|---|---|---|---|---|~%")
    (dolist (f (layout-fields name))
      (format s "| ~(~A~)~:[~; (reserved)~] | ~(~S~) | ~D | ~D | ~(~A~) | ~@[~A~] |~%"
              (field-name f) (field-reserved f) (field-type f) (field-slot f) (field-offset f)
              (field-access f) (and (field-since f) (fork-name (field-since f)))))))
