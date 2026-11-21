(defpackage #:clempo.evm.compile-tests
  (:use #:cl #:clempo.base #:clempo.state #:clempo.evm))

(in-package #:clempo.evm.compile-tests)

;;; The compiled path against the interpreter: every case runs both ways and must agree
;;; on status, gas left, output, storage, logs and refund. Gas sweeps run a program at
;;; every gas limit in a range, so out-of-gas lands in every position inside its blocks.

(defun asm (&rest items)
  "Assemble ITEMS: opcode names as keywords (:push1 :add ...); an integer after PUSHn is
its immediate; a string is raw hex."
  (let ((out '()))
    (loop while items
          do (let ((item (pop items)))
               (etypecase item
                 (string (loop for b across (hex->octets item) do (push b out)))
                 (keyword
                  (let* ((info (find (symbol-name item) (map 'list #'identity (loop for i from 0 below 256 collect (op-info i)))
                                     :key (lambda (o) (and o (symbol-name (op-info-name o))))
                                     :test #'string=))
                         (op (or (and info (op-info-opcode info)) (error "no op ~A" item))))
                    (push op out)
                    (when (<= #x60 op #x7f)
                      (let ((n (- op #x5f)) (v (pop items)))
                        (loop for i from (1- n) downto 0 do (push (ldb (byte 8 (* 8 i)) v) out))))))
                 (integer (push item out)))))
    (coerce (nreverse out) 'octets)))

(defun run-accounts (accounts &key (to #xc0de) (gas 100000) (fork :osaka) (input (make-octets 0))
                                   jit (threshold 0) spec (compiler #'jit-compile-synchronously))
  "Call TO in a world of ACCOUNTS, a list of (address code); the observable outcome. JIT:
run through JIT-RUN-FRAME with COMPILER as the executor (compiling synchronously)."
  (let ((b (make-memory-backend)))
    (memory-backend-put-account b #xca11 :balance (expt 10 18))
    (loop for (address code) in accounts
          do (memory-backend-put-account b address :code code :balance 1000))
    (let* ((world (make-world b))
           (*world* world)
           (*spec* (or spec (make-spec fork)))
           (*block* (make-block-env :number 1 :gas-limit 30000000))
           (*jit* (and jit #'jit-run-frame))
           (*jit-compiler* (and jit compiler))
           (*jit-max-code-size* most-positive-fixnum)
           (*jit-threshold* threshold))
      (multiple-value-bind (status gas-left output)
          (call-message :call #xca11 to to 0 input gas 0 nil)
        (let ((refund (world-refund world))
              (logs (mapcar (lambda (l) (list (log-entry-address l) (log-entry-topics l)
                                              (log-entry-data l)))
                            (world-logs world))))
          (finalize-tx world)
          (apply-changes b (drain-changes world))
          (list status gas-left output refund logs
                (sort (loop for address being the hash-keys of (memory-backend-accounts b)
                              using (hash-value a)
                            collect (list address (mem-account-nonce a) (mem-account-balance a)
                                          (octets->hex (mem-account-code-hash a))
                                          (sort (loop for k being the hash-keys of (mem-account-storage a)
                                                        using (hash-value v)
                                                      collect (cons k v))
                                                #'< :key #'car)))
                      #'< :key #'first)))))))

(defun same (accounts &rest keys)
  "True when ACCOUNTS behave the same interpreted and compiled; returns the outcome."
  (let ((want (apply #'run-accounts accounts keys))
        (got (apply #'run-accounts accounts :jit t keys)))
    (is-equalp want got "~S" keys)
    want))

(defun sweep (code from to &rest keys)
  "SAME for CODE at every gas limit in [FROM, TO]."
  (loop for gas from from to to
        always (equalp (apply #'run-accounts `((#xc0de ,code)) :gas gas keys)
                       (apply #'run-accounts `((#xc0de ,code)) :gas gas :jit t keys))
        finally (return t)))

(defun compiled-during (thunk)
  "How many codes got compiled while THUNK ran."
  (let ((before (jit-stats-compiled *jit-stats*)))
    (funcall thunk)
    (- (jit-stats-compiled *jit-stats*) before)))

(deftest compile-straight-line-and-memory ()
  (let ((code (asm :push1 1 :push1 2 :add :push1 3 :mul :dup1 :push1 0 :mstore
                   :gas :push1 32 :mstore :push1 7 :push1 0 :sstore
                   :push1 64 :push1 0 :return)))
    (is= :return (first (same `((#xc0de ,code)))))
    (is (sweep code 0 120))
    (is (sweep code 2250 2350))                 ; SSTORE's 2300 sentry, GAS pushed exactly
    (is (sweep code 22150 22250))))

(deftest compile-loop-out-of-gas-everywhere ()
  ;; 10 iterations of: JUMPDEST PUSH1 1 SWAP1 SUB DUP1 PUSH1 2 JUMPI ; STOP
  (let ((code (asm :push1 10 :jumpdest :push1 1 :swap1 :sub :dup1 :push1 2 :jumpi :stop)))
    (is= :stop (first (same `((#xc0de ,code)))))
    (is (sweep code 0 260))))

(deftest compile-stack-bounds ()
  (is= :stack-underflow (first (same `((#xc0de ,(asm :push1 1 :add))))))
  (is (sweep (asm :push1 1 :push1 2 :add :add) 0 12))
  ;; pushes forever: JUMPDEST PUSH1 1 PUSH1 0 JUMP overflows at 1025
  (is= :stack-overflow (first (same `((#xc0de ,(asm :jumpdest :push1 1 :push1 0 :jump))))))
  (is= :stack-overflow (first (same `((#xc0de ,(asm :push1 1 :jumpdest :dup1 :push1 2 :jump))))))
  (is= :stack-underflow (first (same `((#xc0de ,(asm :push1 1 :push1 2 :swap2))))))
  (is= :stack-underflow (first (same `((#xc0de ,(asm :push1 1 :dup2))))))
  ;; the overflow lands on a gas boundary too
  (is (sweep (asm :jumpdest :push1 1 :push1 0 :jump) 15360 15400)))

(deftest compile-jumps ()
  ;; static jump to a non-JUMPDEST, into PUSH data holding 0x5b, dynamic and huge targets
  (is= :invalid-jump (first (same `((#xc0de ,(asm :push1 3 :jump :stop))))))
  (is= :invalid-jump (first (same `((#xc0de ,(asm :push1 #x5b :push1 1 :jump))))))
  (is= :invalid-jump (first (same `((#xc0de ,(asm :push1 1 :push1 2 :add :jump :jumpdest :stop))))))
  (is= :invalid-jump (first (same `((#xc0de ,(asm :push32 (1- (ash 1 256)) :jump :jumpdest))))))
  ;; dynamic jump to a valid JUMPDEST at pc 6; JUMPI not taken to a bad target
  (is= :stop (first (same `((#xc0de ,(asm :push1 2 :push1 4 :add :jump :jumpdest :stop))))))
  (is= :return (first (same `((#xc0de ,(asm :push1 0 :push1 99 :jumpi :push1 1 :push1 0 :mstore
                                             :push1 32 :push1 0 :return))))))
  ;; unreachable bytes after a halt, and a JUMPDEST reached only through the dispatch
  (is= :revert (first (same `((#xc0de ,(asm :push1 4 :push1 5 :add :jump "fe01ff"
                                             :jumpdest :push1 0 :dup1 :revert))))))
  ;; a truncated PUSH at the end of the code stops
  (is= :stop (first (same `((#xc0de ,(asm :push1 1 "61aa")))))))

(deftest compile-many-jump-targets ()
  ;; 600 JUMPDESTs; jump dynamically to the last one (two-level dispatch)
  (let* ((n 600)
         (code (apply #'asm :push1 1 :push2 (+ 7 (1- n)) :mul :jump
                      (append (loop repeat n collect :jumpdest)
                              (list :push1 42 :push1 0 :mstore :push1 32 :push1 0 :return)))))
    (is= 42 (octets->integer (third (same `((#xc0de ,code))))))))

(deftest compile-long-straight-line ()
  ;; longer than *JIT-MAX-BLOCK*: split blocks still charge and fail exactly
  (let ((code (apply #'asm (append (loop repeat 300 append (list :push1 1 :pop)) (list :stop)))))
    (is= :stop (first (same `((#xc0de ,code)))))
    (is (sweep code 1480 1520))
    (is (sweep code 0 20))))

(deftest compile-create-paths ()
  ;; Write initcode A into memory, CREATE it, overwrite memory with initcode B, CREATE
  ;; again: two code vectors born in one frame, each returning a different runtime code;
  ;; then call both. Runtime code: PUSH1 v PUSH1 0 SSTORE STOP.
  (flet ((initcode (v)
           ;; copy the 6-byte runtime from the initcode's tail into memory and return it
           (let ((runtime (asm :push1 v :push1 0 :sstore :stop)))
             (concatenate 'octets
                          (asm :push1 (length runtime) :push1 12 :push1 0 :codecopy
                               :push1 (length runtime) :push1 0 :return)
                          runtime))))
    (let* ((a (initcode 7)) (b (initcode 9))
           (code (apply #'asm
                        (append
                         (list :push32 (octets->integer (concatenate 'octets a (make-octets (- 32 (length a)))))
                               :push1 0 :mstore
                               :push1 (length a) :push1 0 :push1 0 :create)
                         (list :push32 (octets->integer (concatenate 'octets b (make-octets (- 32 (length b)))))
                               :push1 0 :mstore
                               :push1 (length b) :push1 0 :push1 0 :create)
                         ;; call both created contracts
                         (list :push1 0 :push1 0 :push1 0 :push1 0 :push1 0 :dup6 :gas :call :pop
                               :push1 0 :push1 0 :push1 0 :push1 0 :push1 0 :dup7 :gas :call :pop
                               :stop)))))
      (let ((outcome (same `((#xc0de ,code)) :gas 400000)))
        (is= :stop (first outcome))
        (is (find '((0 . 7)) (sixth outcome) :key #'fifth :test #'equal))
        (is (find '((0 . 9)) (sixth outcome) :key #'fifth :test #'equal)))
      (is (sweep code 32000 32100))
      (is (sweep code 64000 64100)))))

(deftest compile-static-calls ()
  ;; #xbeef writes storage; #xf00d only reads. STATICCALL both, store the results.
  (let ((writer (asm :push1 1 :push1 0 :sstore :stop))
        (reader (asm :push1 0 :sload :pop :stop))
        (code (asm :push1 0 :push1 0 :push1 0 :push1 0 :push2 #xbeef :push2 #x5000 :staticcall
                   :push1 1 :sstore
                   :push1 0 :push1 0 :push1 0 :push1 0 :push2 #xf00d :push2 #x5000 :staticcall
                   :push1 2 :sstore :stop)))
    (let ((outcome (same `((#xc0de ,code) (#xbeef ,writer) (#xf00d ,reader)) :gas 200000)))
      (is= :stop (first outcome))
      (is (find '((2 . 1)) (sixth outcome) :key #'fifth :test #'equal)))))

(deftest compile-extra-and-gated-opcodes ()
  ;; an opcode the spec adds (like Tempo's 0x4f) runs through the spec's function
  (let ((spec (make-spec :osaka)))
    (setf (svref (spec-extra-opcodes spec) #x4f)
          (lambda (frame stack sp)
            (decf (frame-gas frame) 2)
            (setf (svref stack sp) 42)
            (values (1+ sp) nil)))
    (is= 42 (octets->integer
             (third (same `((#xc0de ,(asm "4f" :push1 0 :mstore :push1 32 :push1 0 :return)))
                          :spec spec)))))
  ;; one that ends the frame keeps the gas it charged (Tempo's 0xef account shim)
  (let ((spec (make-spec :osaka))
        (charged nil))
    (setf (svref (spec-extra-opcodes spec) #x4f)
          (lambda (frame stack sp)
            (declare (ignore stack))
            (decf (frame-gas frame) 500)
            (values sp :return (octets-of 7))))
    (let ((plain (same `((#xc0de ,(asm :push1 1 :pop :stop))) :spec spec))
          (halted (same `((#xc0de ,(asm :push1 1 :pop "4f"))) :spec spec)))
      (setf charged (- (second plain) (second halted)))
      (is= :return (first halted))
      (is= 500 charged)))
  (is= :invalid-opcode (first (same `((#xc0de ,(asm :push1 1 "4f"))))))
  (is= :invalid-opcode (first (same `((#xc0de ,(asm :push1 1 :clz))) :fork :prague)))
  (is= :invalid-opcode (first (same `((#xc0de ,(asm :push0))) :fork :istanbul)))
  (is= :stop (first (same `((#xc0de ,(asm :push0 :pop))) :fork :shanghai))))

(defun jit-state (code)
  (clempo.evm::analysis-jit (clempo.evm::code-analysis code)))

(defun jit-count-of (code)
  "The gas CODE has been counted for, or NIL before its first frame under the JIT."
  (let ((s (jit-state code)))
    (and s (svref s clempo.evm::+jit-count-slot+))))

(defun jit-fn (code &optional (fork :osaka))
  "What CODE's cache entry for FORK holds (a function, :PENDING, :FAILED, :EVICTED), or
NIL without one."
  (let* ((s (jit-state code))
         (e (and s (svref s (fork-index fork)))))
    (and e (clempo.evm::jit-entry-fn e))))

(deftest compile-hot-threshold ()
  ;; 7 bytes spending 11 gas a call; at 2 gas per byte it compiles on the third call
  (let ((*jit-cache* (make-jit-cache))
        (code (asm :push1 1 :push1 2 :add :pop :stop)))
    (is= 0 (compiled-during (lambda ()
                              (run-accounts `((#xc0de ,code)) :jit t :threshold 2)
                              (run-accounts `((#xc0de ,code)) :jit t :threshold 2))))
    (is= 22 (jit-count-of code))
    (run-accounts `((#xc0de ,code)) :jit t :threshold 2)
    (is (functionp (jit-fn code)))
    ;; another vector with the same bytes adopts it at its first frame, uncounted
    (let ((twin (copy-seq code)))
      (is= 0 (compiled-during (lambda () (run-accounts `((#xc0de ,twin)) :jit t :threshold 2))))
      (is (functionp (jit-fn twin))))))

(defclass step-counter () ((n :initform 0 :accessor steps)))
(defmethod trace-step ((tr step-counter) frame pc op gas sp stack)
  (declare (ignore frame pc op gas sp stack))
  (incf (steps tr)))

(deftest compile-tracing-interprets ()
  (let ((tr (make-instance 'step-counter)))
    (let ((*tracer* tr))
      (run-accounts `((#xc0de ,(asm :push1 1 :push1 2 :add :pop :stop))) :jit t))
    (is= 5 (steps tr))))

;;; Where and when compilation happens, and what bounds it.

(defun with-evm-context (thunk)
  (let ((*world* (make-world (make-memory-backend)))
        (*spec* (make-spec :osaka))
        (*block* (make-block-env :number 1 :gas-limit 30000000)))
    (funcall thunk)))

(deftest compile-empty-code ()
  ;; Empty code compiles to what the interpreter does with it (STOP, all gas kept); the
  ;; compiler used to read pc 0 of the empty pc table, out of bounds under safety 0.
  (with-evm-context
   (lambda ()
     (let ((fn (compile-code (make-octets 0) (fork-index :osaka)))
           (frame (clempo.evm::%make-frame :code (make-octets 0) :gas 1000)))
       (multiple-value-bind (status output) (funcall fn frame)
         (is= :stop status)
         (is= 0 (length output))
         (is= 1000 (frame-gas frame))))
     ;; and the JIT runs empty code as the interpreter does, compiling nothing
     (let ((*jit-cache* (make-jit-cache))
           (*jit-compiler* #'jit-compile-synchronously)
           (*jit-threshold* 0)
           (frame (clempo.evm::%make-frame :code (make-octets 0) :gas 1000)))
       (is= :stop (jit-run-frame frame))
       (is= 1000 (frame-gas frame))
       (is= 0 (jit-cache-count))))))

(deftest jit-never-compiles-on-the-calling-thread ()
  ;; With an executor installed a hot code is only handed over: the frame that made it hot
  ;; and every frame until the executor ran the thunk are interpreted.
  (let* ((*jit-cache* (make-jit-cache))
         (queued '())
         (executor (lambda (thunk) (push thunk queued) t))
         (code (asm :push1 5 :jumpdest :push1 1 :swap1 :sub :dup1 :push1 2 :jumpi :stop))
         (want (run-accounts `((#xc0de ,code)))))
    (is= 0 (compiled-during
            (lambda ()
              (is-equalp want (run-accounts `((#xc0de ,code)) :jit t :compiler executor))
              (is-equalp want (run-accounts `((#xc0de ,code)) :jit t :compiler executor)))))
    (is= 1 (length queued) "handed over once")
    (is (eq :pending (jit-fn code)))
    (is= 1 (jit-cache-pending *jit-cache*))
    ;; the compiler thread's turn
    (is= 1 (compiled-during (lambda () (funcall (pop queued)))))
    (is (functionp (jit-fn code)))
    (is= 0 (jit-cache-pending *jit-cache*))
    (is-equalp want (run-accounts `((#xc0de ,code)) :jit t :compiler executor))
    (is (null queued))))

(deftest jit-pending-is-bounded ()
  (let* ((*jit-cache* (make-jit-cache))
         (*jit-max-pending* 2)
         (queued '())
         (executor (lambda (thunk) (push thunk queued) t))
         (codes (loop for i below 5 collect (asm :push2 (+ 1000 i) :pop :stop)))
         (refused (jit-stats-refused *jit-stats*)))
    (dolist (c codes) (run-accounts `((#xc0de ,c)) :jit t :compiler executor))
    (is= 2 (length queued))
    (is= 2 (jit-cache-pending *jit-cache*))
    (is= 3 (- (jit-stats-refused *jit-stats*) refused))
    ;; a refused code counts from zero again: the frame's own 5 gas
    (is= 5 (jit-count-of (fifth codes)))
    (is (null (jit-fn (fifth codes))))
    (is= 2 (compiled-during (lambda () (mapc #'funcall queued))))
    (is= 0 (jit-cache-pending *jit-cache*))))

(deftest jit-executor-refusal ()
  (let ((*jit-cache* (make-jit-cache))
        (code (asm :push2 2000 :pop :stop)))
    (run-accounts `((#xc0de ,code)) :jit t :compiler (constantly nil))
    (is= 0 (jit-cache-count))
    (is= 0 (jit-cache-pending *jit-cache*))
    (is= 5 (jit-count-of code))))

(deftest jit-remove-cancels-pending ()
  (let* ((*jit-cache* (make-jit-cache))
         (*jit* nil)
         (*jit-compiler* nil)
         (*jit-threshold* *jit-threshold*)
         (queued '())
         (code (asm :push2 2500 :pop :stop)))
    (install-jit-compiler (lambda (thunk) (push thunk queued) t) :threshold 0)
    (run-accounts `((#xc0de ,code)) :jit t :compiler *jit-compiler*)
    (is= 1 (jit-cache-pending *jit-cache*))
    (remove-jit-compiler)
    (is (null *jit*))
    (is= 0 (jit-cache-pending *jit-cache*))
    (is (eq :evicted (jit-fn code)))
    ;; the thunk the compiler thread still holds does nothing
    (is= 0 (compiled-during (lambda () (funcall (pop queued)))))
    (is= 0 (jit-cache-count))))

(deftest jit-cache-evicts-least-recently-run ()
  (let ((*jit-cache* (make-jit-cache))
        (*jit-cache-limit* 4))
    (destructuring-bind (a b c d e) (loop for i below 5 collect (asm :push2 (+ 3000 i) :pop :stop))
      (dolist (x (list a b c d)) (run-accounts `((#xc0de ,x)) :jit t))
      (is= 4 (jit-cache-count))
      (run-accounts `((#xc0de ,a)) :jit t)  ; A ran last but one
      (let ((evicted (jit-stats-evicted *jit-stats*)))
        (run-accounts `((#xc0de ,e)) :jit t)  ; over the limit: down to 7/8 of it
        (is= 3 (jit-cache-count))
        (is= 2 (- (jit-stats-evicted *jit-stats*) evicted)))
      (is (functionp (jit-fn a)))
      (is (functionp (jit-fn d)))
      (is (functionp (jit-fn e)))
      (is (eq :evicted (jit-fn b)))
      (is (eq :evicted (jit-fn c)))
      ;; an evicted code lets go of its function and counts from zero again
      (run-accounts `((#xc0de ,b)) :jit t :threshold 100)
      (is (null (jit-fn b)))
      (is= 5 (jit-count-of b)))))

(deftest jit-cache-bounds-native-bytes ()
  (let ((*jit-cache* (make-jit-cache))
        (codes (loop for i below 8 collect (asm :push2 (+ 4000 i) :pop :push1 1 :pop :stop))))
    (run-accounts `((#xc0de ,(first codes))) :jit t)
    (let* ((one (jit-cache-bytes *jit-cache*))
           (*jit-cache-max-bytes* (* 3 one)))
      (is (plusp one))
      (dolist (c (rest codes))
        (run-accounts `((#xc0de ,c)) :jit t)
        (is (<= (jit-cache-bytes *jit-cache*) (* 3 one))))
      (is (<= (jit-cache-count) 3)))))

(deftest jit-counts-own-work ()
  (let ((*jit-cache* (make-jit-cache)))
    ;; REVERT keeps its gas: its work counts (PUSH1 PUSH1 ADD POP PUSH1 DUP1 REVERT)
    (let ((code (asm :push1 1 :push1 2 :add :pop :push1 0 :dup1 :revert)))
      (run-accounts `((#xc0de ,code)) :jit t :threshold 1000)
      (is= 17 (jit-count-of code)))
    ;; an exceptional halt zeroes the gas: nothing is known of its work, nothing counts
    (let ((code (asm :push1 1 :push1 2 :add :pop "fe")))
      (run-accounts `((#xc0de ,code)) :jit t :threshold 1000)
      (is= 0 (jit-count-of code)))
    ;; a caller is not credited with what its callee spent: 5 PUSH1, PUSH2, GAS, a cold
    ;; CALL (100 + 2,500), POP; the callee loops 100 times for 2,603
    (let ((callee (asm :push1 100 :jumpdest :push1 1 :swap1 :sub :dup1 :push1 2 :jumpi :stop))
          (caller (asm :push1 0 :push1 0 :push1 0 :push1 0 :push1 0 :push2 #xbeef :gas :call
                       :pop :stop)))
      (run-accounts `((#xc0de ,caller) (#xbeef ,callee)) :jit t :threshold 100000 :gas 200000)
      (is= (+ 15 3 2 2600 2) (jit-count-of caller))
      (is= 2603 (jit-count-of callee)))))

(deftest analysis-store-is-bounded ()
  ;; The store keeps at most two generations' worth of code analysed, however many code
  ;; vectors stay alive, while a code in steady use keeps its one analysis.
  (let ((clempo.evm::*analysis-store* (clempo.evm::make-analysis-store))
        (clempo.evm::*analysis-l1* (clempo.evm::make-analysis-l1))
        (clempo.evm::*analysis-cache-code-bytes* 1000)
        (kept (asm :push1 1 :stop))
        (alive '()))
    (let ((a (clempo.evm::code-analysis kept)))
      (dotimes (i 200)
        (let ((c (make-octets 100)))
          (push c alive)
          (clempo.evm::code-analysis c))
        (is (eq a (clempo.evm::code-analysis kept))))
      (let ((store clempo.evm::*analysis-store*))
        (is (<= (+ (hash-table-count (clempo.evm::analysis-store-young store))
                   (hash-table-count (clempo.evm::analysis-store-old store)))
                22)
            "~D analyses stored" (+ (hash-table-count (clempo.evm::analysis-store-young store))
                                    (hash-table-count (clempo.evm::analysis-store-old store))))))
    (is= 200 (length alive))))
