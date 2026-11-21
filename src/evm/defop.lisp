(in-package #:clempo.evm)

;;; DEFOP declares an opcode as data: its byte, name, the stack words it pops (bound to
;;; the given variables, top of stack first), how many it pushes, its static gas, the
;;; fork it appears in and its body. DEFINE-INTERPRETER later splices every body into one
;;; dense CASE, which SBCL compiles to a jump table.
;;;
;;; Inside a body these are in scope:
;;;   frame code code-len pc sp stack gas world spec gp block fork
;;;   (push-word x)        push X (the stack room was checked from :pushes)
;;;   (charge n)           spend N gas or halt out of gas
;;;   (halt status [out])  end the frame (:stop :return :revert, or an error keyword)
;;;   (mem-ensure off n)   expand memory over [OFF, OFF+N) charging for it; N may be 0
;;;   (sync-gas) (reload-gas)  hand GAS to out-of-line code through FRAME-GAS and back
;;; :manual-pc t means the body sets PC itself; otherwise PC advances by one.

(defstruct (op-info (:constructor make-op-info
                        (opcode name args pushes needs gas since manual-pc body doc)))
  opcode name args pushes needs gas since manual-pc body doc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *op-table* (make-array 256 :initial-element nil)
    "opcode -> OP-INFO, filled by DEFOP."))

(defmacro defop (opcode name (&rest args) (&key (gas 0) (pushes 0) (needs (length args))
                                               (since :istanbul) manual-pc doc)
                 &body body)
  "Declare opcode OPCODE. ARGS are popped (top first); NEEDS is the stack depth required
\(defaults to the number of ARGS; DUP and SWAP need more than they pop)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (aref *op-table* ,opcode)
           (make-op-info ,opcode ',name ',args ,pushes ,needs ',gas ,since ,manual-pc ',body ,doc))
     ',name))

(defun op-info (opcode-or-name)
  "The OP-INFO for an opcode byte or name symbol."
  (if (integerp opcode-or-name)
      (aref *op-table* opcode-or-name)
      (find opcode-or-name *op-table* :key (lambda (o) (and o (op-info-name o))))))

(defun ops-since (fork)
  "The opcodes introduced exactly in FORK: (ops-since :cancun)."
  (loop for o across *op-table*
        when (and o (eq (op-info-since o) fork)) collect (op-info-name o)))
