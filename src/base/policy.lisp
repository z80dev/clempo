(in-package #:clempo.base)

;;; One switch for the whole build. The :clempo-release feature (scripts/build.lisp sets it
;;; for CLEMPO_BUILD=release) removes the runtime type checks that check builds keep inside
;;; hot functions; invariants stay on in both.
;;; Hot code reads the policy at read time: (declare #.clempo.base:*hot*).

(defparameter *build-mode*
  (if (member :clempo-release *features*) :release :check))

(defparameter *hot*
  (if (eq *build-mode* :release)
      '(optimize (speed 3) (safety 0) (debug 0))
      '(optimize (speed 3) (safety 1) (debug 1))))

(defparameter *checked* '(optimize (speed 1) (safety 3) (debug 2)))

(define-condition invariant-violation (error)
  ((form :initarg :form :reader invariant-violation-form)
   (message :initarg :message :initform nil :reader invariant-violation-message))
  (:report (lambda (c s)
             (format s "invariant violated: ~S~@[ (~A)~]"
                     (invariant-violation-form c) (invariant-violation-message c)))))

(defmacro invariant (form &optional control &rest args)
  "Assertion that stays on in release builds."
  `(unless ,form
     (error 'invariant-violation
            :form ',form
            :message ,(when control `(format nil ,control ,@args)))))

(defmacro unreachable (&optional control &rest args)
  `(error 'invariant-violation :form :unreachable
                               :message ,(when control `(format nil ,control ,@args))))

(defmacro define-constant (name value &optional doc)
  "DEFCONSTANT that tolerates reloading non-EQL values (bignums, vectors)."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
