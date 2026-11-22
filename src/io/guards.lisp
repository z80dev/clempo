(defpackage #:clempo.guard
  (:use #:cl #:clempo.base)
  (:documentation "Declared guards for state that threads share, the counterpart of Clang's
thread-safety analysis (GUARDED_BY). DEFINE-SHARED-STRUCT and DEFINE-SLOT-GUARDS give each
slot of a shared structure or class its guard; their accessors check it in check builds and
compile to the bare slot access in release builds. DEFSHARED declares the guard of a global
variable, which the source gate checks at every mutation (scripts/check-layers.lisp).

A guard is one of:
  (:LOCK slot)            reads and writes hold the mutex in SLOT of the same object
  (:LOCK-WRITES slot)     writes hold it; reads take whatever value was last published
                          (a whole value replaced under the lock, read without it)
  (:HELD (self) form)     reads and writes hold the mutex FORM returns for object SELF
  (:HELD-WRITES (self) form)
  (:THREAD (self) form)   only the thread FORM returns (when not NIL) touches the slot
  :IMMUTABLE              set when the object is made, never written after (read-only)
  :ATOMIC                 written only by COMPARE-AND-SWAP after the object is made
  (:UNCHECKED reason)     shared by a protocol the checker cannot see; REASON says which
A violated guard signals GUARD-VIOLATION, once per place and access, and the test harness
fails the test it happened in.")
  (:export #:define-shared-struct #:define-slot-guards #:defshared #:with-held #:held-p
           #:guard-violation #:guard-violation-place #:guard-violation-access
           #:take-guard-violations))

(in-package #:clempo.guard)

(defun checked-p () (not (eq *build-mode* :release)))

(declaim (inline held-p))
(defun held-p (mutex)
  "True when this thread holds MUTEX."
  (and mutex (sb-thread:holding-mutex-p mutex)))

;;; Violations

(defvar *violation-lock* (sb-thread:make-mutex :name "guard violations")
  "Guards the records below; outside the lock-order graph.")
(defvar *violations* '() "Descriptions not yet taken by TAKE-GUARD-VIOLATIONS.")
(defvar *reported* (make-hash-table :test 'equal) "(place access) pairs already signalled.")

(define-condition guard-violation (error)
  ((place :initarg :place :reader guard-violation-place)
   (access :initarg :access :reader guard-violation-access)
   (guard :initarg :guard :reader guard-violation-guard)
   (thread :initarg :thread :reader guard-violation-thread))
  (:report (lambda (c s)
             (format s "guard violation in thread ~S: ~(~A~) of ~{~S~^ ~} without its guard ~S"
                     (guard-violation-thread c) (guard-violation-access c)
                     (guard-violation-place c) (guard-violation-guard c)))))

(defun guard-violated (place access guard)
  "Record and signal, with a CONTINUE restart, the first violation of PLACE's guard by ACCESS."
  (let ((condition nil))
    (sb-thread:with-mutex (*violation-lock*)
      (unless (gethash (list place access) *reported*)
        (setf (gethash (list place access) *reported*) t
              condition (make-condition 'guard-violation
                                        :place place :access access :guard guard
                                        :thread (sb-thread:thread-name sb-thread:*current-thread*)))
        (push (princ-to-string condition) *violations*)))
    (when condition
      (restart-case (error condition)
        (continue () :report "Access the place anyway.")))))

(defun take-guard-violations ()
  "The violations seen since the last call, oldest first."
  (sb-thread:with-mutex (*violation-lock*)
    (reverse (shiftf *violations* '()))))

(pushnew 'take-guard-violations *test-audits*)

;;; Guards as code. GUARD-TESTS returns (values kind read-test write-test): KIND is
;;; :MUTABLE, :IMMUTABLE or :ATOMIC; a test is a form over the object in OBJECT, true when
;;; the access is allowed, or NIL for none. LOCK-OF turns a slot name into the form that
;;; reads that slot of OBJECT.

(defun guard-tests (guard object lock-of)
  (flet ((bind (spec)
           (destructuring-bind ((var) form) spec
             `(let ((,var ,object)) (declare (ignorable ,var)) ,form))))
    (cond ((eq guard :immutable) (values :immutable nil nil))
          ((eq guard :atomic) (values :atomic nil nil))
          ((atom guard) (error "Unknown guard ~S" guard))
          (t (destructuring-bind (kind &rest args) guard
               (ecase kind
                 (:lock (let ((test `(held-p ,(funcall lock-of (first args)))))
                          (values :mutable test test)))
                 (:lock-writes (values :mutable nil `(held-p ,(funcall lock-of (first args)))))
                 (:held (let ((test `(held-p ,(bind args)))) (values :mutable test test)))
                 (:held-writes (values :mutable nil `(held-p ,(bind args))))
                 (:thread (let ((test `(let ((owner ,(bind args)))
                                         (or (null owner) (eq owner sb-thread:*current-thread*)))))
                            (values :mutable test test)))
                 (:unchecked (unless (stringp (first args))
                               (error "An :UNCHECKED guard needs the reason as a string: ~S" guard))
                             (values :mutable nil nil))))))))

(defun checked-form (test place access guard form)
  (if (and test (checked-p))
      `(progn (unless ,test (guard-violated ',place ,access ',guard)) ,form)
      form))

;;; Structures

(defmacro define-shared-struct (name-and-options &rest slots)
  "DEFSTRUCT whose every slot names its guard: (slot initform :guard guard ...). Accessors
keep their usual names and check the guard in check builds; an :IMMUTABLE slot has no
writer, an :ATOMIC one only a COMPARE-AND-SWAP writer."
  (destructuring-bind (name &rest options) (if (consp name-and-options) name-and-options (list name-and-options))
    (let* ((conc-option (find :conc-name options :key (lambda (o) (and (consp o) (car o)))))
           (conc (cond ((null conc-option) (format nil "~A-" (symbol-name name)))
                       ((null (second conc-option)) "")
                       (t (string (second conc-option)))))
           (doc (when (stringp (first slots)) (list (pop slots))))
           (object (gensym "OBJECT"))
           (raw-slots '()) (accessors '()))
      (flet ((raw (slot) (intern (format nil "%%~A~A" conc (symbol-name slot))))
             (public (slot) (intern (format nil "~A~A" conc (symbol-name slot)))))
        (dolist (spec slots)
          (unless (and (consp spec) (getf (cddr spec) :guard))
            (error "Slot ~S of shared structure ~S needs a :GUARD" spec name))
          (destructuring-bind (slot init &rest keys &key guard &allow-other-keys) spec
            (let ((keys (loop for (k v) on keys by #'cddr unless (eq k :guard) append (list k v))))
              (multiple-value-bind (kind read write)
                  (guard-tests guard object (lambda (lock) `(,(raw lock) ,object)))
                (push `(,slot ,init ,@keys ,@(when (eq kind :immutable) '(:read-only t))) raw-slots)
                (let ((place (list name slot)) (accessor (public slot)) (raw (raw slot)))
                  (push `(declaim (inline ,accessor ,@(when (eq kind :mutable) `((setf ,accessor)))))
                        accessors)
                  (push `(defun ,accessor (,object)
                           ,(checked-form read place :read guard `(,raw ,object)))
                        accessors)
                  (when (eq kind :mutable)
                    (push `(defun (setf ,accessor) (value ,object)
                             ,(checked-form write place :write guard `(setf (,raw ,object) value)))
                          accessors))
                  (when (eq kind :atomic)
                    (push `(defun (sb-ext:cas ,accessor) (old new ,object)
                             (sb-ext:cas (,raw ,object) old new))
                          accessors)))))))
        `(progn
           (defstruct (,name (:conc-name ,(intern (format nil "%%~A" conc)))
                       ,@(remove conc-option options))
             ,@doc ,@(reverse raw-slots))
           ,@(reverse accessors)
           ',name)))))

;;; Classes. The guards are :AROUND methods on the slot's readers and writers, installed in
;;; check builds once the class is defined; SLOT-VALUE goes around them, and the gate
;;; refuses (SETF SLOT-VALUE) of a guarded slot.

(defmacro define-slot-guards (class &rest slot-guards)
  "Give each (slot guard) of CLASS's slots its guard (see the package documentation)."
  (let ((self (gensym "SELF")))
    `(progn
       ,@(when (checked-p)
           (loop for (slot guard) in slot-guards
                 collect (multiple-value-bind (kind read write)
                             (guard-tests guard self (lambda (lock) `(slot-value ,self ',lock)))
                           (declare (ignore kind))
                           `(install-slot-guard ',class ',slot
                                                ,(when read `(lambda (,self) ,read))
                                                ,(when write `(lambda (,self) ,write))
                                                ',guard))))
       ',class)))

(defun install-slot-guard (class-name slot read-test write-test guard)
  "Wrap the readers and writers of CLASS-NAME's direct slot SLOT in checks of its guard."
  (let* ((class (find-class class-name))
         (definition (or (find slot (sb-mop:class-direct-slots class) :key #'sb-mop:slot-definition-name)
                         (error "~S has no direct slot ~S" class-name slot)))
         (place (list class-name slot)))
    (flet ((wrap (function-name test access setter)
             (let* ((gf (fdefinition function-name))
                    (method (make-instance 'standard-method
                              :qualifiers '(:around)
                              :lambda-list (if setter '(value object) '(object))
                              :specializers (if setter (list (find-class t) class) (list class))
                              :function (lambda (arguments next-methods)
                                          (unless (funcall test (if setter (second arguments) (first arguments)))
                                            (guard-violated place access guard))
                                          (funcall (sb-mop:method-function (first next-methods))
                                                   arguments (rest next-methods))))))
               (let ((old (find-method gf '(:around) (sb-mop:method-specializers method) nil)))
                 (when old (remove-method gf old)))
               (add-method gf method))))
      (when read-test
        (dolist (reader (sb-mop:slot-definition-readers definition))
          (wrap reader read-test :read nil)))
      (when write-test
        (dolist (writer (sb-mop:slot-definition-writers definition))
          (wrap writer write-test :write t))))))

;;; Global variables

(defmacro defshared (name value doc &key guard)
  "DEFVAR NAME, a global that threads share, mutated only as GUARD allows: (:LOCK form)
means inside WITH-LOCK, WITH-RECURSIVE-LOCK or WITH-HELD of that form; :ATOMIC means only
through COMPARE-AND-SWAP; :IMMUTABLE means never after it is defined; (:UNCHECKED reason)
exempts it. The source gate checks each mutation; the value itself is not wrapped."
  (unless (or (member guard '(:atomic :immutable))
              (and (consp guard) (eq (first guard) :lock) (consp (cdr guard)))
              (and (consp guard) (eq (first guard) :unchecked) (stringp (second guard))))
    (error "DEFSHARED ~S: the guard must be (:LOCK form), :ATOMIC, :IMMUTABLE or (:UNCHECKED reason), not ~S"
           name guard))
  `(defvar ,name ,value ,doc))

(defmacro with-held ((mutex) &body body)
  "Run BODY, which mutates state guarded by MUTEX, in a function its callers call holding
MUTEX; check builds check that they do."
  (if (checked-p)
      (let ((m (gensym "MUTEX")))
        `(let ((,m ,mutex))
           (unless (held-p ,m) (guard-violated '(with-held ,mutex) :write '(:lock ,mutex)))
           ,@body))
      `(progn ,@body)))
