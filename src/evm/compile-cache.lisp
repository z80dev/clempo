(in-package #:clempo.evm)

;;; When compiled code runs. With *JIT* set to #'JIT-RUN-FRAME, RUN-FRAME hands every
;;; untraced frame here.
;;;
;;; Hotness is gas: per code vector (by identity, in the thread's code-analysis cache)
;;; ANALYSIS-JIT sums the gas its interpreted frames spent. Compiling costs about 0.1 ms
;;; per code byte and the interpreter runs about 1 gas per ns, so once a code has burned
;;; *JIT-THRESHOLD* (100,000) gas per byte of itself, interpreting it has cost about as
;;; much as compiling it would: it is then compiled (the ski-rental rule; never worse
;;; than twice the better choice). A code that loops hard compiles early; a code called
;;; often for little work never pays for its compilation and stays interpreted.
;;;
;;; Once hot, ANALYSIS-JIT becomes a vector indexed by fork holding the compiled
;;; function, :PENDING or :FAILED, plus the code's hash. Compiled functions are shared
;;; between threads and code vectors through *JIT-CACHE*, keyed by (code hash, fork): a
;;; code vector with the same bytes reuses the function. Compiled code depends on nothing
;;; else (gas parameters, precompiles and hooks are read from *SPEC* as it runs), and
;;; code is immutable by its hash, so nothing ever invalidates an entry; the cache is
;;; only emptied when it outgrows *JIT-CACHE-LIMIT*.
;;;
;;; Compilation runs in the calling thread unless *JIT-COMPILE-HOOK* is set: then the hook
;;; gets a thunk to run wherever it likes (a background thread in an :io layer) and the
;;; code is interpreted until the thunk has run.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*jit* jit-run-frame *jit-threshold* *jit-compile-hook* *jit-cache*
            *jit-cache-limit* *jit-max-code-size* enable-jit disable-jit clear-jit-cache
            jit-stats jit-stats-compiled jit-stats-failed jit-stats-shared jit-stats-failures
            *jit-stats*)
          :clempo.evm))

(defparameter *jit-threshold* 100000
  "Gas a code must spend interpreted, per byte of its length, before it is compiled; 0
compiles every code at its first call.")

(defparameter *jit-max-code-size* 65536
  "Code longer than this is never compiled.")

(defvar *jit-compile-hook* nil
  "NIL to compile synchronously, or a function of a thunk that compiles and installs.")

(defvar *jit-cache* (make-hash-table :test 'eql :synchronized t)
  "(code-hash * 16 + fork) -> compiled function, :PENDING or :FAILED.")

(defparameter *jit-cache-limit* 4096
  "Entries in *JIT-CACHE* before it is emptied.")

(defstruct jit-stats
  (compiled 0 :type sb-ext:word)        ; functions compiled
  (failed 0 :type sb-ext:word)          ; codes the compiler failed on (a bug)
  (shared 0 :type sb-ext:word)          ; installs served by *JIT-CACHE*
  (failures '()))                       ; (code fork reason) of the first failures

(defvar *jit-stats* (make-jit-stats))

(define-constant +jit-hash-slot+ (length *forks*))

(defun jit-compile (code fork)
  "Compile CODE for FORK; NIL (counted, and the first few kept) when the compiler fails
or warns."
  (multiple-value-bind (fn warnings)
      (handler-case (compile-code code fork)
        (serious-condition (c) (values nil (list (princ-to-string c)))))
    (cond ((and fn (null warnings)) (sb-ext:atomic-incf (jit-stats-compiled *jit-stats*)) fn)
          (t (when (< (sb-ext:atomic-incf (jit-stats-failed *jit-stats*)) 20)
               (sb-ext:atomic-push (list code fork warnings) (jit-stats-failures *jit-stats*)))
             nil))))

(defun jit-request (code fork state)
  "Find or start the compiled function of CODE for FORK, recording it in STATE."
  (declare (type simple-vector state) (type fixnum fork))
  (let* ((key (+ (* 16 (svref state +jit-hash-slot+)) fork))
         (cache *jit-cache*)
         (found (gethash key cache)))
    (cond ((functionp found)
           (sb-ext:atomic-incf (jit-stats-shared *jit-stats*))
           (setf (svref state fork) found))
          ((eq found :failed) (setf (svref state fork) :failed))
          ((eq found :pending) (setf (svref state fork) :pending))
          ((> (length code) *jit-max-code-size*) (setf (svref state fork) :failed))
          (t
           (when (>= (hash-table-count cache) *jit-cache-limit*) (clrhash cache))
           (setf (gethash key cache) :pending
                 (svref state fork) :pending)
           (flet ((work ()
                    (let ((fn (jit-compile code fork)))
                      (setf (gethash key cache) (or fn :failed)
                            (svref state fork) (or fn :failed)))))
             (let ((hook *jit-compile-hook*))
               (if hook (funcall hook #'work) (work))))))))

(defun jit-cold (frame analysis)
  "JIT-RUN-FRAME when ANALYSIS has no compiled function for the running fork yet."
  (declare (type frame frame) (type analysis analysis))
  (let ((state (analysis-jit analysis))
        (fork (spec-fork *spec*)))
    (unless (simple-vector-p state)
      (let ((spent (or state 0))
            (code (frame-code frame)))
        (declare (type fixnum spent))
        (when (< spent (* *jit-threshold* (length code)))
          (let ((gas (frame-gas frame)))
            (return-from jit-cold
              (multiple-value-bind (status output) (interpret frame)
                ;; Count work, not the gas an exceptional halt burns (on the state tests,
                ;; counting it compiles big codes that never run again and the suite
                ;; takes four times as long). A recursive frame of the same code may have
                ;; made it hot meanwhile.
                (let ((now (analysis-jit analysis)))
                  (when (and (member status +success-statuses+) (not (simple-vector-p now)))
                    (setf (analysis-jit analysis)
                          (+ (or now 0) (max 0 (- gas (frame-gas frame)))))))
                (values status output)))))
        (setf state (make-array (1+ +jit-hash-slot+) :initial-element nil)
              (svref state +jit-hash-slot+) (octets->integer (keccak256 code))
              (analysis-jit analysis) state)))
    (let ((entry (svref state fork)))
      (cond ((null entry) (jit-request (frame-code frame) fork state))
            ((eq entry :pending)
             (let ((found (gethash (+ (* 16 (svref state +jit-hash-slot+)) fork) *jit-cache*)))
               (cond ((functionp found) (setf (svref state fork) found))
                     ((eq found :failed) (setf (svref state fork) :failed))
                     ((null found) (setf (svref state fork) nil)))))))   ; cache was emptied
    (let ((fn (svref state fork)))
      (if (functionp fn) (funcall fn frame) (interpret frame)))))

(defun jit-run-frame (frame)
  "Run FRAME's code compiled when it is, else interpreted (counting it toward compilation)."
  (declare #.*hot* (type frame frame))
  (let* ((analysis (code-analysis (frame-code frame)))
         (state (analysis-jit analysis)))
    (if (simple-vector-p state)
        (let ((fn (svref state (spec-fork *spec*))))
          (if (functionp fn) (funcall fn frame) (jit-cold frame analysis)))
        (jit-cold frame analysis))))

(defun enable-jit (&key (threshold *jit-threshold*))
  "Run untraced frames through JIT-RUN-FRAME, compiling a code once it has spent
THRESHOLD gas per byte of itself interpreted (0: at its first call)."
  (setf *jit-threshold* threshold
        *jit* #'jit-run-frame))

(defun disable-jit () (setf *jit* nil))

(defun clear-jit-cache ()
  "Forget every shared compiled function (per-thread code analyses keep theirs)."
  (clrhash *jit-cache*))

;;; The default policy is on: at 100,000 gas per code byte the state-test suite runs in
;;; the same time as with the interpreter alone (12.9 s against 12.8 s, 23 codes
;;; compiled) and evm-bench is unchanged (its loops run inside a single frame, which never
;;; turns hot while it runs).
(enable-jit)
