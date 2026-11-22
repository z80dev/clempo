(in-package #:clempo.evm)

;;; EVM bytecode -> Lisp -> native code. COMPILE-CODE turns one code vector, for one
;;; fork, into a function of a FRAME that behaves exactly like INTERPRET on that frame:
;;;
;;; - Basic blocks become TAGBODY tags named by their pc. A block starts at pc 0, at every
;;;   JUMPDEST and after JUMPI or an unassigned opcode; code after a halt, JUMP or invalid
;;;   opcode that is not a JUMPDEST is unreachable and dropped.
;;; - Inside a block the operand stack is symbolic: PUSH values are literals, results are
;;;   lexical variables, words below the block's entry height are read from the stack
;;;   vector on use. DUP, SWAP and POP move symbols and cost nothing at run time. The
;;;   vector is written once, when the block ends (or hands over to the interpreter).
;;; - Every other op is its DEFOP body with its arguments bound to the symbolic stack and
;;;   PUSH-WORD as its value: spliced in for pure ops (and GAS MLOAD MSTORE EXP), called
;;;   through a function compiled once from the same body for the other impure ones. The
;;;   semantics are the interpreter's, not a copy.
;;; - Static gas is charged once per segment: the ops up to and including the first one
;;;   whose body can see or spend gas or halt (GAS, SLOAD, MSTORE, CALL, ...). That op's
;;;   body therefore runs with exactly the interpreter's gas, and the pure ops before it
;;;   cannot fail. When a segment's gas is short, or the block's stack bounds would be
;;;   broken, the stack is written out and INTERPRET resumes at the segment's (or block's)
;;;   first pc, so the halting op, status and gas are the interpreter's own.
;;; - JUMP/JUMPI to a PUSHed constant is a GO; any other target goes through a dispatch:
;;;   a pc -> block-index vector and a dense CASE (a jump table).
;;; - Blocks are grouped into regions of about *JIT-REGION-SIZE* instructions, each its
;;;   own function, because SBCL's compile time grows with the square of a function.
;;; - The fork is a constant (FORK>= folds); gas parameters, precompiles, extra opcodes
;;;   and the SSTORE hook are read from *SPEC* at run time, so one compiled function
;;;   serves every spec of a fork.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(code-lambdas compile-code code-blocks *jit-max-block* *jit-region-size*) :clempo.evm))

(defparameter *jit-max-block* 200
  "Longest run of instructions compiled as one block; longer straight-line code is split
\(the nesting of one block's forms grows with its length).")

;;; Instructions and blocks

(defstruct (insn (:constructor make-insn (pc kind info arg next)))
  (pc 0 :type fixnum)
  kind                                  ; :push :dup :swap :pop :jump :jumpi :op :invalid :extern
  info                                  ; the OP-INFO
  arg                                   ; PUSH value, DUP/SWAP depth
  (next 0 :type fixnum))                ; pc of the following instruction

(defstruct (cblock (:constructor make-cblock (start)))
  (start 0 :type fixnum)
  (insns '())                           ; in order
  (end nil))                            ; how it ends when its last insn does not: (:fall pc) or :end

(defun decode-insn (code pc fork pushes)
  (declare (type octets code) (type fixnum pc fork))
  (let* ((op (aref code pc))
         (info (aref *op-table* op)))
    (cond ((null info) (make-insn pc :extern nil op (1+ pc)))
          ((> (fork-index (op-info-since info)) fork) (make-insn pc :invalid info op (1+ pc)))
          ((<= #x5f op #x7f)
           (make-insn pc :push info (if (= op #x5f) 0 (svref pushes pc)) (+ pc 1 (- op #x5f))))
          ((<= #x80 op #x8f) (make-insn pc :dup info (- op #x7f) (1+ pc)))
          ((<= #x90 op #x9f) (make-insn pc :swap info (- op #x8f) (1+ pc)))
          ((= op #x50) (make-insn pc :pop info nil (1+ pc)))
          ((= op #x56) (make-insn pc :jump info nil (1+ pc)))
          ((= op #x57) (make-insn pc :jumpi info nil (1+ pc)))
          (t (make-insn pc :op info nil (1+ pc))))))

(defun insn-ends-block-p (insn)
  (case (insn-kind insn)
    ((:jump :jumpi :invalid :extern) t)
    (:op (body-halts-p (insn-info insn)))))

(defun code-blocks (code fork &optional (analysis (analyze-code code)))
  "The reachable basic blocks of CODE under FORK (an index), in pc order. ANALYSIS is
CODE's (by default a private one: the compiler leaves the shared caches alone)."
  (declare (type octets code))
  (let* ((jumpdests (analysis-jumpdests analysis))
         (pushes (analysis-pushes analysis))
         (len (length code))
         (blocks '())
         (current nil)
         (leader 0)                     ; a fallthrough target that must start a block
         (pc 0))
    (flet ((open-block (pc)
             (setf current (make-cblock pc))
             (push current blocks))
           (close-block (end)
             (setf (cblock-insns current) (nreverse (cblock-insns current))
                   (cblock-end current) end
                   current nil)))
      (loop while (< pc len)
            do (let ((insn (decode-insn code pc fork pushes)))
                 (when (or (= pc leader) (= 1 (sbit jumpdests pc)))
                   (when current (close-block `(:fall ,pc)))
                   (open-block pc))
                 (when current
                   (push insn (cblock-insns current))
                   (cond ((insn-ends-block-p insn)
                          (close-block nil)
                          (when (member (insn-kind insn) '(:jumpi :extern))
                            (setf leader (insn-next insn))))
                         ((and (>= (length (cblock-insns current)) *jit-max-block*)
                               (< (insn-next insn) len))
                          (setf leader (insn-next insn)))))
                 (setf pc (insn-next insn))))
      (when current (close-block :end)))
    (nreverse blocks)))

;;; Op properties, read off the DEFOP declarations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +gas-observers+ '(gas charge halt sync-gas reload-gas mem-ensure static-guard)
    "Symbols whose presence in an op body means the body sees or spends gas, or may halt.")

  (defparameter +inline-impure-ops+ '(gas mload mstore exp)
    "Impure ops spliced into compiled code. Every other impure op that does not end the
frame runs out of line through its *OP-FUNCTIONS* entry: compiled once at load from the
same body, it keeps each call site small (compile time grows with code size). MSTORE8
stays out of line: inline with a constant offset near 32 KiB, SBCL 2.6.8's arm64 backend
emits an unencodable STR immediate and the compile fails.")

  (defun mentions-p (tree symbols)
    (cond ((symbolp tree) (member tree symbols))
          ((consp tree) (or (mentions-p (car tree) symbols) (mentions-p (cdr tree) symbols)))))

  (defun body-halts-p (info)
    "True when the op always ends the frame: its body's last form is a HALT."
    (let ((last (car (last (op-info-body info)))))
      (and (consp last) (eq (car last) 'halt))))

  (defun out-of-line-p (info)
    (and (not (op-info-manual-pc info))
         (not (<= #x80 (op-info-opcode info) #x9f))
         (/= (op-info-opcode info) #x50)
         (mentions-p (op-run-body info) +gas-observers+)
         (not (body-halts-p info))
         (not (member (op-info-name info) +inline-impure-ops+)))))

(defvar *op-functions* (make-array 256 :initial-element nil)
  "opcode -> the out-of-line function of an impure op: (frame &rest args) -> (values
word status output), STATUS non-NIL when the op halted (FRAME-GAS then already final).")

(defmacro define-op-functions ()
  `(progn
     ,@(loop for info across *op-table*
             when (and info (out-of-line-p info))
               collect
               (let ((args (op-info-args info)))
                 `(setf (svref *op-functions* ,(op-info-opcode info))
                        (lambda (frame ,@args)
                          (declare #.*hot* (type frame frame)
                                   ,@(when args `((type word ,@args) (ignorable ,@args)))
                                   (sb-ext:muffle-conditions sb-ext:compiler-note))
                          (block op
                            (let* ((code (frame-code frame))
                                   (world *world*)
                                   (spec *spec*)
                                   (gp (spec-gas spec))
                                   (block *block*)
                                   (fork (spec-fork spec)))
                              (declare (type octets code) (type spec spec) (type gas-params gp)
                                       (type block-env block) (type fixnum fork)
                                       (ignorable code world gp block fork))
                              (symbol-macrolet ((gas (frame-gas frame)) (code-len (length code)))
                                (macrolet ((current-fork () 'fork)
                                           (when-tracing (&body body) (declare (ignore body)) nil)
                                           (push-word (x) x)
                                           (sync-gas () nil)
                                           (reload-gas () nil)
                                           (halt (status &optional (output '+empty-code+))
                                             `(let ((st ,status) (out ,output))
                                                ,@(unless (member status +success-statuses+)
                                                    `((setf gas ,(if (keywordp status)
                                                                     0
                                                                     `(if (member st +success-statuses+) gas 0)))))
                                                (return-from op (values 0 st out)))))
                                  (values (progn ,@(op-run-body info)) nil nil)))))))))))

(define-op-functions)

(defun insn-pure-p (insn)
  "Pure instructions neither observe gas nor halt, so their static gas may be charged
ahead of them together with their segment's."
  (case (insn-kind insn)
    ((:push :dup :swap :pop) t)
    (:op (not (mentions-p (op-run-body (insn-info insn)) +gas-observers+)))))

(defun insn-stack-effect (insn)
  "(values needs net) of INSN, as the interpreter checks them."
  (let ((info (insn-info insn)))
    (case (insn-kind insn)
      ((:invalid :extern) (values 0 0))
      (t (let ((nargs (length (op-info-args info))))
           (values (max nargs (op-info-needs info)) (- (op-info-pushes info) nargs)))))))

(defun tail-push-p (forms)
  "True when PUSH-WORD occurs in FORMS (a body) exactly as the value of every path, so
PUSH-WORD may simply return its argument."
  (labels ((free (x) (not (mentions-p x '(push-word))))
           (body (forms)
             (let ((forms (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) forms)))
               (and forms (every #'free (butlast forms)) (tail (car (last forms))))))
           (tail (f)
             (and (consp f)
                  (case (car f)
                    (push-word (free (cdr f)))
                    (progn (body (cdr f)))
                    ((let let*) (and (free (second f)) (body (cddr f))))
                    (multiple-value-bind (and (free (third f)) (body (cdddr f))))
                    (if (and (free (second f)) (tail (third f)) (tail (fourth f))))))))
    (body forms)))

(loop for info across *op-table*
      when (and info (not (op-info-manual-pc info)) (/= (op-info-opcode info) #x50)
                (not (<= #x80 (op-info-opcode info) #x9f)))
        do (unless (if (plusp (op-info-pushes info))
                       (and (= 1 (op-info-pushes info)) (tail-push-p (op-run-body info)))
                       (not (mentions-p (op-run-body info) '(push-word))))
             (error "compile.lisp: ~A does not end in PUSH-WORD; teach the compiler its shape"
                    (op-info-name info))))

;;; The symbolic stack of the block being translated: *STK* holds entries above the
;;; block's entry height, top first; *BELOW* counts the entry words consumed so far.
;;; An entry is a literal integer, a variable, or (:in k), the word at SP-k on entry.

(defvar *stk*)
(defvar *below*)
(defvar *vars*)

(defun spop ()
  (if *stk* (pop *stk*) (list :in (incf *below*))))

(defun sdeep (n)
  "Make the top N entries explicit."
  (loop while (< (length *stk*) n)
        do (setf *stk* (append *stk* (list (list :in (incf *below*)))))))

(defun entry-form (e)
  (if (consp e) `(svref stack (- sp ,(second e))) e))

(defun fresh-var ()
  (make-symbol (format nil "V~D" (incf *vars*))))

(defun materialize-form ()
  "Write the symbolic stack into the stack vector and move SP to its height."
  (let* ((h (- (length *stk*) *below*))
         (temps '())
         (writes '()))
    (loop for e in *stk*
          for off downfrom (1- h)
          unless (and (consp e) (= (- (second e)) off))
            do (let ((val (if (consp e)
                              (let ((tmp (fresh-var)))
                                (push `(,tmp ,(entry-form e)) temps)
                                tmp)
                              e)))
                 (push `(setf (svref stack (+ sp ,off)) ,val) writes)))
    `(let (,@(nreverse temps))
       ,@(nreverse writes)
       ,@(unless (zerop h) `((setf sp (+ sp ,h)))))))

(defun exit-form (pc)
  "Hand the frame to the interpreter at PC with the current symbolic stack."
  `(progn ,(materialize-form) (resume ,pc)))

(defun gas-form (insns)
  "The static gas of INSNS: an integer, or a form over GP."
  (let ((k 0) (forms '()))
    (dolist (i insns)
      (let ((g (op-info-gas (insn-info i))))
        (if (integerp g) (incf k g) (push g forms))))
    (if forms `(+ ,k ,@(nreverse forms)) k)))

(defun segments (insns)
  "Split INSNS at each impure one (kept as its segment's last); invalid and unassigned
opcodes, which charge nothing, are left out."
  (let ((segs '()) (cur '()))
    (dolist (i insns)
      (unless (member (insn-kind i) '(:invalid :extern))
        (push i cur)
        (unless (insn-pure-p i) (push (nreverse cur) segs) (setf cur '()))))
    (when cur (push (nreverse cur) segs))
    (nreverse segs)))

(defvar *region* nil
  "Hash set of the block starts compiled into the function being generated.")

(defun goto-form (pc)
  "Continue at the block starting at PC: a GO inside this region, else hand PC and SP
back to RUN-COMPILED."
  (if (gethash pc *region*) `(go ,pc) `(transfer ,pc)))

(defun jump-form (dest jumpdests len)
  (if (integerp dest)
      (if (and (< dest len) (= 1 (sbit jumpdests dest))) (goto-form dest) '(halt :invalid-jump))
      `(progn (setf target ,dest) (go :dispatch))))

(defun next-form (pc len)
  (if (< pc len) (goto-form pc) '(halt :stop)))

(defun insn-form (insn jumpdests len)
  "Translate INSN against the symbolic stack. Returns (values statement terminal):
STATEMENT is NIL or the form that runs INSN; TERMINAL, when INSN ends the block, is the
block's last form."
  (let ((info (insn-info insn)))
    (ecase (insn-kind insn)
      (:push (push (insn-arg insn) *stk*) nil)
      (:dup (sdeep (insn-arg insn)) (push (nth (1- (insn-arg insn)) *stk*) *stk*) nil)
      (:swap (let ((n (insn-arg insn)))
               (sdeep (1+ n))
               (rotatef (nth 0 *stk*) (nth n *stk*)))
             nil)
      (:pop (spop) nil)
      (:jump
       (let* ((dest (spop))
              (d (if (integerp dest) dest (fresh-var))))
         (values nil `(let (,@(unless (integerp dest) `((,d ,(entry-form dest)))))
                        ,(materialize-form)
                        ,(jump-form d jumpdests len)))))
      (:jumpi
       (let* ((dest (spop))
              (condition (spop))
              (d (if (integerp dest) dest (fresh-var)))
              (c (fresh-var)))
         (values nil `(let ((,c ,(entry-form condition))
                            ,@(unless (integerp dest) `((,d ,(entry-form dest)))))
                        ,(materialize-form)
                        (if (eql ,c 0)
                            ,(next-form (insn-next insn) len)
                            ,(jump-form d jumpdests len))))))
      (:invalid (values nil '(halt :invalid-opcode)))
      (:extern
       (values nil `(progn
                      ,(materialize-form)
                      (let ((fn (svref (spec-extra-opcodes spec) ,(insn-arg insn))))
                        (unless fn (halt :invalid-opcode))
                        (sync-gas)
                        (multiple-value-bind (new-sp status output)
                            (funcall (the function fn) frame stack sp)
                          (when status (halt status (or output +empty-code+)))
                          (setf sp new-sp)
                          (reload-gas)
                          ,(next-form (insn-next insn) len))))))
      (:op
       (let* ((args (op-info-args info))
              (forms (mapcar (lambda (a) (declare (ignore a)) (entry-form (spop))) args))
              (fn (svref *op-functions* (op-info-opcode info)))
              (body (if fn
                        `(multiple-value-bind (r st out) (funcall (the function ,fn) frame ,@forms)
                           (when st (return-from run (values st out)))
                           r)
                        `(symbol-macrolet ((pc ,(insn-pc insn)))
                           (let (,@(mapcar #'list args forms))
                             ,@(when args `((declare (type word ,@args) (ignorable ,@args))))
                             ,@(op-run-body info))))))
         (if (plusp (op-info-pushes info))
             (let ((v (fresh-var)))
               (push v *stk*)
               `(setq ,v ,body))
             body))))))

(defun block-form (block jumpdests len)
  "The straight-line code of BLOCK, entered with SP words on the stack vector."
  (let ((*stk* '()) (*below* 0)
        (statements '())
        (terminal nil)
        (insns (cblock-insns block)))
    ;; stack bounds of the whole block, checked once on entry
    (let ((rel 0) (need 0) (grow 0))
      (dolist (i insns)
        (multiple-value-bind (needs net) (insn-stack-effect i)
          (setf need (max need (- needs rel)))
          (when (plusp net) (setf grow (max grow (+ rel net))))
          (incf rel net)))
      (let ((tests (append (when (plusp need) `((< sp ,need)))
                           (when (plusp grow) `((> sp ,(- +stack-limit+ grow)))))))
        (when tests
          (push `(when (or ,@tests) (resume ,(cblock-start block))) statements))))
    (dolist (seg (segments insns))
      (let ((cost (gas-form seg)))
        (unless (eql cost 0)
          (push `(let ((cost ,cost))
                   (declare (type fixnum cost))
                   (when (< gas cost) ,(exit-form (insn-pc (first seg))))
                   (decf gas cost))
                statements)))
      (dolist (i seg)
        (multiple-value-bind (statement term) (insn-form i jumpdests len)
          (when statement (push statement statements))
          (when term (setf terminal term)))))
    (let ((last (car (last insns))))
      (when (and last (member (insn-kind last) '(:invalid :extern)))
        (setf terminal (nth-value 1 (insn-form last jumpdests len)))))
    (unless terminal
      (setf terminal (let ((end (cblock-end block)))
                       (cond ((eq end :end) '(halt :stop))
                             ((consp end) `(progn ,(materialize-form) ,(goto-form (second end))))
                             (t nil)))))      ; the last op halted
    (nest-statements (reverse statements) terminal)))

(defun nest-statements (statements terminal)
  "STATEMENTS then TERMINAL as one form, each (SETQ v x) turned into a LET of v over the
rest, so every result variable is bound once and never assigned."
  (let ((body (list terminal)))
    (dolist (s (reverse statements) (if (rest body) `(progn ,@body) (first body)))
      (if (and (consp s) (eq (first s) 'setq))
          (setf body `((let ((,(second s) ,(third s)))
                         (declare (type word ,(second s)))
                         ,@body)))
          (push s body)))))

;;; Regions. SBCL's compile time grows with the square of a function's size, so a
;;; contract is cut into regions of consecutive blocks, each its own function of
;;; (frame stack sp entry-pc). Inside a region blocks GO to each other; a jump out of it
;;; returns (values pc sp) and RUN-COMPILED calls the region holding PC. A halt returns
;;; (values status output), status being a keyword.

(defparameter *jit-region-size* 120
  "Instructions per compiled region (a single block may exceed it).")

(defun code-regions (blocks)
  "BLOCKS cut into consecutive runs of about *JIT-REGION-SIZE* instructions."
  (let ((regions '()) (cur '()) (n 0))
    (dolist (b blocks)
      (let ((k (length (cblock-insns b))))
        (when (and cur (> (+ n k) *jit-region-size*))
          (push (nreverse cur) regions)
          (setf cur '() n 0))
        (push b cur)
        (incf n k)))
    (when cur (push (nreverse cur) regions))
    (nreverse regions)))

(defparameter +shared-halts+
  '(:out-of-gas :memory-out-of-gas :invalid-jump :invalid-opcode :static-violation
    :return-data-out-of-bounds)
  "Exceptional halts compiled as a GO to one exit per region rather than inline.")

(defun region-lambda (blocks self len fork jumpdests block-region block-local)
  "The function of region number SELF made of BLOCKS."
  (let ((*region* (make-hash-table)))
    (dolist (b blocks) (setf (gethash (cblock-start b) *region*) t))
    `(lambda (frame stack sp entry)
       (declare ,*hot* (optimize (sb-c:store-source-form 0)) ; see COMPILE-REGION
                (type frame frame) (type simple-vector stack) (type (integer 0 ,+stack-limit+) sp)
                (type fixnum entry) (sb-ext:muffle-conditions sb-ext:compiler-note))
       ;; GAS is FRAME-GAS itself, not a variable: SBCL's propagation over a variable
       ;; assigned and compared once per segment is superlinear in the code size.
       (let* ((code (frame-code frame))
              (world *world*)
              (spec *spec*)
              (gp (spec-gas spec))
              (block *block*)
              (target 0))
         (declare (type octets code) (type spec spec) (type gas-params gp)
                  (type block-env block) (type word target)
                  (ignorable code world gp block target))
         (symbol-macrolet ((code-len ,len) (fork ,fork) (gas (frame-gas frame)))
           (macrolet ((current-fork () ,fork)
                      (when-tracing (&body body) (declare (ignore body)) nil)
                      (push-word (x) x)
                      (sync-gas () nil)
                      (reload-gas () nil)
                      (halt (status &optional (output '+empty-code+))
                        (if (and (member status ',+shared-halts+) (eq output '+empty-code+))
                            `(go ,status)
                            `(let ((st ,status) (out ,output))
                               ,@(unless (member status +success-statuses+)
                                   `((setf gas ,(if (keywordp status)
                                                    0
                                                    `(if (member st +success-statuses+) gas 0)))))
                               (return-from run (values st out)))))
                      (resume (pc) `(return-from run (interpret frame ,pc sp)))
                      (transfer (pc) `(return-from run (values ,pc sp))))
             (block run
               (tagbody
                  (let ((i (aref (the (simple-array (signed-byte 32) (*)) ,block-local) entry)))
                    (declare (type (signed-byte 32) i))
                    ,(dispatch-form 'i (mapcar #'cblock-start blocks)))
                  ,@(loop for b in blocks
                          nconc (list (cblock-start b) (block-form b jumpdests len)))
                  :dispatch
                  (if (and (typep target 'fixnum) (< target ,len) (= 1 (sbit ,jumpdests target)))
                      (if (= ,self (aref (the (simple-array (signed-byte 32) (*)) ,block-region)
                                         target))
                          (let ((i (aref (the (simple-array (signed-byte 32) (*)) ,block-local)
                                         target)))
                            (declare (type (signed-byte 32) i))
                            ,(dispatch-form 'i (mapcar #'cblock-start blocks)))
                          (transfer target))
                      (halt :invalid-jump))
                  ,@(loop for status in +shared-halts+
                          nconc `(,status
                                  (setf gas 0)
                                  (return-from run (values ,status +empty-code+))))))))))))

(defun dispatch-form (var tags)
  "GO to the VAR-th of TAGS: one dense CASE (a jump table), two levels of them when
there are many tags, since a long CASE nests as deeply as it has clauses."
  (let ((n (length tags)))
    (if (<= n 256)
        `(case ,var
           ,@(loop for tag in tags for i from 0 collect `(,i (go ,tag))))
        `(case (ash ,var -8)
           ,@(loop for chunk from 0 below (ceiling n 256)
                   collect `(,chunk
                             (case ,var
                               ,@(loop for tag in (subseq tags (* 256 chunk) (min n (* 256 (1+ chunk))))
                                       for i from (* 256 chunk)
                                       collect `(,i (go ,tag))))))))))

(defun code-lambdas (code fork)
  "CODE (octets, not empty) for FORK (index or keyword) as Lisp: (values region-forms
block-region), BLOCK-REGION mapping each block's start pc to the index of the form that
holds it."
  (declare (type octets code))
  (let* ((fork (fork-index fork))
         (len (length code))
         (analysis (analyze-code code))
         (jumpdests (analysis-jumpdests analysis))
         (regions (code-regions (code-blocks code fork analysis)))
         (block-region (make-array len :element-type '(signed-byte 32) :initial-element -1))
         (block-local (make-array len :element-type '(signed-byte 32) :initial-element -1))
         (*vars* 0))
    (invariant (plusp len) "code-lambdas: empty code has no blocks")
    (loop for r in regions for self from 0
          do (loop for b in r for i from 0
                   do (setf (aref block-region (cblock-start b)) self
                            (aref block-local (cblock-start b)) i)))
    (values (loop for r in regions for self from 0
                  collect (region-lambda r self len fork jumpdests block-region block-local))
            block-region)))

(defun run-compiled (regions block-region frame)
  "Run FRAME through its code's compiled REGIONS from pc 0."
  (declare #.*hot* (type simple-vector regions) (type (simple-array (signed-byte 32) (*)) block-region)
           (type frame frame))
  (let ((stack (acquire-stack (frame-depth frame)))
        (pc 0)
        (sp 0))
    (declare (type fixnum pc sp))
    (loop
      (multiple-value-bind (a b)
          (funcall (the function (svref regions (aref block-region pc))) frame stack sp pc)
        (if (typep a 'fixnum)
            (setf pc a sp b)
            (return (values a b)))))))

(defun empty-code-run (frame)
  "Empty code, compiled: INTERPRET reads STOP past the end of any code."
  (declare (ignore frame))
  (values :stop +empty-code+))

(defun compile-region (form)
  "FORM compiled, without the copy of FORM that COMPILE keeps in the function's debug
source (SBCL stores every in-memory compilation's toplevel form there for the debugger).
For this generated code that form is about 6 times the native code it produced, so a
cache of compiled contracts would mostly hold Lisp source no one reads."
  (let* ((fn (compile nil form))
         (source (sb-c::debug-info-source
                  (sb-kernel:%code-debug-info (sb-kernel:fun-code-header (sb-kernel:%fun-fun fn))))))
    (when (typep source 'sb-c::core-debug-source)
      (setf (sb-c::core-debug-source-form source) nil))
    fn))

(defun native-bytes (fn)
  "Bytes FN keeps alive: its code object and debug info (with its function map)."
  (let* ((code (sb-kernel:fun-code-header (sb-kernel:%fun-fun fn)))
         (info (sb-kernel:%code-debug-info code))
         (map (and (typep info 'sb-c::compiled-debug-info) (sb-c::compiled-debug-info-fun-map info))))
    (+ (sb-ext:primitive-object-size code)
       (if info (sb-ext:primitive-object-size info) 0)
       (if (typep map 'vector) (sb-ext:primitive-object-size map) 0))))

(defun compile-code (code fork)
  "CODE compiled for FORK: a function of a FRAME that returns (values status output) and
leaves FRAME-GAS as INTERPRET would. Second value: the full warnings the compiler raised
\(muffled; any is a translation bug). Third: the bytes the function keeps alive (its code
objects, debug info and tables; measured within 10% of the heap it retains)."
  (declare (type octets code))
  (when (zerop (length code))
    (return-from compile-code (values #'empty-code-run '() 0)))
  (multiple-value-bind (forms block-region) (code-lambdas code fork)
    (let* ((warnings '())
           (regions (handler-bind ((warning (lambda (w)
                                              (unless (typep w 'style-warning)
                                                (push (princ-to-string w) warnings))
                                              (muffle-warning w))))
                      (map 'simple-vector #'compile-region forms)))
           ;; the code objects, plus the pc tables they hold (block-region, block-local
           ;; of the same size, and the jump-destination bits)
           (bytes (+ (* 2 (sb-ext:primitive-object-size block-region))
                     (ceiling (length code) 8)
                     (sb-ext:primitive-object-size regions)
                     (loop for r across regions sum (native-bytes r)))))
      (values (if (= 1 (length regions))
                  (let ((fn (svref regions 0)))
                    (lambda (frame)
                      (funcall (the function fn) frame (acquire-stack (frame-depth frame)) 0 0)))
                  (lambda (frame) (run-compiled regions block-region frame)))
              (nreverse warnings)
              bytes))))
