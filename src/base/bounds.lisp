(in-package #:clempo.base)

;;; Compile-time bound proofs: the counterpart of Go's `const _ = uint(a - b)`.
;;;
;;; STATIC-ASSERT evaluates its form when the enclosing code is macroexpanded, which for
;;; a file is when it is compiled. A false result signals STATIC-ASSERTION-FAILED from
;;; the compiler, so the build fails before any code runs; a true one leaves nothing in
;;; the fasl. The form sees only what the compiler sees: constants, and functions and
;;; macros already loaded.
;;;
;;; DEFBOUND names a fixed limit (an integer constant) together with the relations that
;;; make it safe: that a buffer holds what its loop writes, that a product cannot leave
;;; the fixnums, that a limb count covers the field's width. The source gate
;;; (scripts/check-layers.lisp) requires every fixed-count loop in hot code to take its
;;; count from a DEFBOUND limit, so no hot loop runs to an unproven magic number.

(define-condition static-assertion-failed (error)
  ((form :initarg :form :reader static-assertion-form)
   (message :initarg :message :initform nil :reader static-assertion-message))
  (:report (lambda (c s)
             (format s "static assertion failed at compile time: ~S~@[ (~A)~]"
                     (static-assertion-form c) (static-assertion-message c)))))

(defmacro static-assert (form &optional control &rest args)
  "Evaluate FORM at macroexpansion time and fail the compilation when it is false.
CONTROL and ARGS, also evaluated then, describe the failure. Expands into NIL."
  (unless (eval form)
    (error 'static-assertion-failed
           :form form
           :message (when control (apply #'format nil control (mapcar #'eval args)))))
  nil)

(defmacro defbound (name value doc &body proofs)
  "Define NAME as the integer constant VALUE, a fixed limit, and prove each of PROOFS
when the definition is compiled. A proof is a form over NAME and other constants that
must be true; a false one fails the build naming the bound and the relation."
  `(progn
     (define-constant ,name ,value ,doc)
     (static-assert (typep ,name 'integer) "bound ~S is not an integer" ',name)
     ,@(loop for proof in proofs
             collect `(static-assert ,proof "proof of bound ~S = ~D" ',name ,name))
     ',name))
