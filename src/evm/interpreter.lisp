(in-package #:clempo.evm)

;;; The interpreter loop, generated from *OP-TABLE*. DEFINE-INTERPRETER expands every
;;; DEFOP body into a clause of one CASE on the opcode byte; the traced variant adds a
;;; TRACE-STEP call per instruction and is chosen only when *TRACER* is set.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +success-statuses+ '(:stop :return :revert)
    "Statuses that keep the gas left; every other status is an exceptional halt."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun op-clause (info)
    (let* ((args (op-info-args info))
           (nargs (length args))
           (needs (max nargs (op-info-needs info)))
           (net (- (op-info-pushes info) nargs))
           (gas (op-info-gas info))
           (since (op-info-since info)))
      `(,(op-info-opcode info)
        ,@(unless (eq since :istanbul)
            `((unless (>= fork ,(fork-index since)) (halt :invalid-opcode))))
        ;; evm2's order: static gas, then (for CREATE and LOG) the static context, then
        ;; the stack (defop.lisp)
        ,@(unless (eql gas 0) `((charge ,gas)))
        ,@(when (op-info-static-first info) '((static-guard)))
        ,@(when (plusp needs) `((when (< sp ,needs) (halt :stack-underflow))))
        ,@(when (plusp net) `((when (> sp ,(- 1024 net)) (halt :stack-overflow))))
        (let (,@(loop for a in args for i from 1 collect `(,a (svref stack (- sp ,i)))))
          ,@(when args `((declare (type word ,@args) (ignorable ,@args))))
          ,@(when (plusp nargs) `((decf sp ,nargs)))
          ,@(op-info-body info))
        ,@(unless (op-info-manual-pc info) '((incf pc)))))))

;;; Gas and memory helpers shared by every runner of DEFOP bodies (this interpreter and
;;; the compiler in compile.lisp). They expect GAS, FRAME and GP in scope and a local
;;; HALT macro.

(defmacro charge (n)
  `(let ((c ,n))
     (declare (type fixnum c))
     (decf gas c)
     (when (< gas 0) (halt :out-of-gas))))

(defmacro sync-gas () '(setf (frame-gas frame) gas))
(defmacro reload-gas () '(setf gas (frame-gas frame)))

(defmacro mem-ensure (offset size)
  `(unless (eql ,size 0)
     (let ((end (mem-end ,offset ,size)))
       (unless end (halt :memory-out-of-gas))
       (when (> end (frame-mem-size frame))
         (let ((c (expansion-cost frame end gp)))
           (declare (type fixnum c))
           (decf gas c)
           (when (< gas 0) (halt :memory-out-of-gas)))
         (grow-memory frame end)))))

(defmacro define-interpreter (name &key traced)
  `(defun ,name (frame &optional (start-pc 0) (start-sp 0))
     ,(format nil "Run FRAME's code~:[~; calling TRACE-STEP and TRACE-STEP-END around each op~]
from START-PC with START-SP words already on its depth's stack (compiled code resumes here).
Returns (values status output); FRAME-GAS holds the gas left (0 after an exceptional halt)."
              traced)
     (declare #.*hot* (type frame frame) (type fixnum start-pc start-sp)
              (sb-ext:muffle-conditions sb-ext:compiler-note))
     (let* ((code (frame-code frame))
            (code-len (length code))
            (analysis (code-analysis code))
            (jumpdests (analysis-jumpdests analysis))
            (pushes (analysis-pushes analysis))
            (stack (acquire-stack (frame-depth frame)))
            (sp start-sp)
            (pc start-pc)
            (gas (frame-gas frame))
            (world *world*)
            (spec *spec*)
            (gp (spec-gas spec))
            (fork (spec-fork spec))
            (block *block*)
            (step-pc 0)
            ,@(when traced '((tracer *tracer*))))
       (declare (type octets code) (type fixnum code-len sp pc fork step-pc) (type gas gas)
                (type simple-bit-vector jumpdests) (type simple-vector pushes stack)
                (type gas-params gp) (type spec spec) (type block-env block)
                (ignorable jumpdests pushes world block step-pc))
       (macrolet ((current-fork () 'fork)
                  (when-tracing (&body body)
                    (declare (ignorable body))
                    ,(if traced '(cons 'progn body) nil))
                  (push-word (x) `(progn (setf (svref stack sp) ,x) (incf sp)))
                  (halt (status &optional (output '+empty-code+))
                    `(let ((st ,status) (out ,output))
                       (setf (frame-gas frame)
                             ,(cond ((member status +success-statuses+) 'gas)
                                    ((keywordp status) 0)
                                    (t `(if (member st +success-statuses+) gas 0))))
                       ,@',(when traced
                             '((trace-step-end tracer frame step-pc
                                (if (< step-pc code-len) (aref code step-pc) 0)
                                gas sp stack st)))   ; gas the op had left, before zeroing
                       (return-from run (values st out)))))
         (block run
           (loop
             (let ((op (if (< pc code-len) (aref code pc) 0)))
               (declare (type (unsigned-byte 8) op))
               ,@(when traced
                   '((setf step-pc pc)
                     (trace-step tracer frame pc op gas sp stack)))
               (case op
                 ,@(loop for info across *op-table* when info collect (op-clause info))
                 (t
                  (let ((fn (svref (spec-extra-opcodes spec) op)))
                    (unless fn (halt :invalid-opcode))
                    (sync-gas)
                    (multiple-value-bind (new-sp status output)
                        (funcall (the function fn) frame stack sp)
                      ;; the op may have charged FRAME-GAS: halt reports it, not the stale local
                      (when status (reload-gas) (halt status (or output +empty-code+)))
                      (setf sp new-sp)
                      (reload-gas)
                      (incf pc)))))
               ,@(when traced
                   '((trace-step-end tracer frame step-pc op gas sp stack nil))))))))))

(define-interpreter interpret)
(define-interpreter interpret-traced :traced t)

(defvar *jit* nil
  "NIL, or a function (frame) -> (values status output) that runs FRAME's code (compiled
or not; see compile.lisp). Never used while *TRACER* is set.")

(defun run-frame (frame)
  "Execute FRAME's code. Returns (values status output) with the gas left in FRAME-GAS."
  (cond (*tracer* (interpret-traced frame))
        (*jit* (funcall (the function *jit*) frame))
        (t (interpret frame))))
