(in-package #:clempo.evm)

;;; When compiled code runs, and where it is compiled.
;;;
;;; Compilation never runs on a thread that executes blocks or serves RPC. clempo.evm
;;; loads with the JIT off; INSTALL-JIT-COMPILER turns it on with an executor, a function
;;; of a thunk that hands the thunk to a compiler thread owned by an :io layer (the node,
;;; the validator, clempo-dev) and returns true, or returns NIL to refuse it. A code is
;;; interpreted until its thunk has run there. JIT-COMPILE-SYNCHRONOUSLY, the executor
;;; that compiles on the calling thread at once, is for the differential tools and tests.
;;;
;;; Hotness is a code's own interpretation work. Per code vector (in its analysis, which
;;; every thread shares), JIT-COUNT sums the gas its interpreted frames spent themselves:
;;; what their calls and creates passed down and did not get back (FRAME-CHILD-GAS) is
;;; the callee's work, not theirs. Compiling costs about 0.1 ms per code byte and the
;;; interpreter runs about 1 gas per ns, so once a code has burned *JIT-THRESHOLD*
;;; (100,000) gas per byte of itself, interpreting it has cost about as much as compiling
;;; it would: it is then compiled (the ski-rental rule; never worse than twice the better
;;; choice). A frame that ends in REVERT did its work and counts like one that returns.
;;; An exceptional halt zeroes the frame's gas, so the work it did before is unknown: it
;;; does not count (counting the gas it burned compiled the state tests' big one-shot codes
;;; and made the suite four times slower).
;;;
;;; Bounds. Code longer than *JIT-MAX-CODE-SIZE* (EIP-170's 24 KiB) is never compiled. At
;;; most *JIT-MAX-PENDING* compilations wait for the executor; a code that turns hot
;;; beyond that, or that the executor refuses, counts from zero again. The shared cache
;;; holds at most *JIT-CACHE-LIMIT* entries and *JIT-CACHE-MAX-BYTES* of native code: an
;;; install that goes over either evicts the least recently run entries down to 7/8 of
;;; both. Analyses point at entries, not functions, so an evicted function is unreachable
;;; at once (once no frame runs it) and its code counts from zero again.
;;;
;;; A code's JIT state (its analysis's ANALYSIS-JIT, made at its first frame under the
;;; JIT) holds its hash, its count and its cache entry per fork. The cache maps (code
;;; hash, fork) to the entry, so code vectors with the same bytes share one compilation,
;;; and a code whose analysis was dropped finds its function again at its next first
;;; frame. Compiled code depends on nothing else (gas parameters, precompiles and hooks
;;; are read from *SPEC* as it runs), and code is immutable by its hash, so no entry is
;;; ever stale.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*jit* jit-run-frame *jit-threshold* *jit-compiler* *jit-max-pending*
            *jit-cache* *jit-cache-limit* *jit-cache-max-bytes* *jit-max-code-size*
            install-jit-compiler remove-jit-compiler jit-compile-synchronously
            clear-jit-cache make-jit-cache jit-cache-count jit-cache-bytes jit-cache-pending
            jit-stats jit-stats-compiled jit-stats-failed jit-stats-shared jit-stats-refused
            jit-stats-evicted jit-stats-failures *jit-stats*)
          :clempo.evm))

(defparameter *jit-threshold* 100000
  "Gas a code must spend interpreted, per byte of its length, before it is compiled; 0
compiles every code at its first call.")

(defparameter *jit-max-code-size* 24576
  "Code longer than this is never compiled (EIP-170's limit: compile time grows faster
than linearly, about 8 s for 24 KiB).")

(defvar *jit-compiler* nil
  "NIL, or the executor compilations are handed to (see INSTALL-JIT-COMPILER).")

(defparameter *jit-max-pending* 16
  "Compilations handed to the executor and not yet run, at most.")

(defparameter *jit-cache-limit* 4096
  "Entries in the shared cache, at most.")

(defparameter *jit-cache-max-bytes* (* 256 1024 1024)
  "Bytes of compiled code and its tables in the shared cache, at most.")

(defstruct jit-stats
  (compiled 0 :type sb-ext:word)        ; functions compiled
  (failed 0 :type sb-ext:word)          ; codes the compiler failed on (a bug)
  (shared 0 :type sb-ext:word)          ; installs served by the cache
  (refused 0 :type sb-ext:word)         ; hot codes turned away (pending bound or executor)
  (evicted 0 :type sb-ext:word)         ; entries evicted
  (failures '()))                       ; (code fork reason) of the first failures

(defvar *jit-stats* (make-jit-stats))

;;; The shared cache. Every change to it (the table, an entry leaving :PENDING, the byte
;;; and pending counts) happens under the table's lock; frames read an entry's FN without
;;; it.

(defstruct (jit-entry (:constructor make-jit-entry (key)))
  (key 0 :type integer)                 ; code hash * 16 + fork
  (fn :pending)                         ; the function, :PENDING, :FAILED or :EVICTED
  (bytes 0 :type fixnum)                ; its code and tables
  (stamp 0 :type fixnum))               ; **JIT-CLOCK** when it last ran

(defstruct (jit-cache (:constructor make-jit-cache ()))
  (table (make-hash-table :test 'eql :synchronized t) :type hash-table)
  (bytes 0 :type fixnum)                ; of the compiled entries
  (pending 0 :type fixnum))             ; entries waiting for the executor

(defvar *jit-cache* (make-jit-cache)
  "Compiled functions by (code hash, fork), shared by every thread.")

(sb-ext:defglobal **jit-clock** 0
  "The cache's LRU time: advanced by each install, copied into an entry when it runs.")
(declaim (type fixnum **jit-clock**))

(declaim (inline jit-touch))
(defun jit-touch (entry)
  (let ((now **jit-clock**))
    (unless (eql (jit-entry-stamp entry) now)
      (setf (jit-entry-stamp entry) now))))

(defun jit-cache-count (&optional (cache *jit-cache*))
  (hash-table-count (jit-cache-table cache)))

;;; A code's JIT state (ANALYSIS-JIT), made at its first frame under the JIT: a vector of
;;; its cache entry per fork, its hash, and its count.
(define-constant +jit-hash-slot+ (length *forks*))
(define-constant +jit-count-slot+ (1+ +jit-hash-slot+))
(invariant (<= +jit-hash-slot+ 16) "jit keys hold the fork in 4 bits")

(declaim (inline jit-key))
(defun jit-key (state fork)
  (+ (* 16 (the integer (svref state +jit-hash-slot+))) fork))

(defun jit-compile (code fork)
  "CODE compiled for FORK: (values function bytes), or NIL (counted, and the first few
kept) when the compiler fails or warns."
  (multiple-value-bind (fn warnings bytes)
      (handler-case (compile-code code fork)
        (serious-condition (c) (values nil (list (princ-to-string c)) 0)))
    (cond ((and fn (null warnings))
           (sb-ext:atomic-incf (jit-stats-compiled *jit-stats*))
           (values fn bytes))
          (t (when (< (sb-ext:atomic-incf (jit-stats-failed *jit-stats*)) 20)
               (sb-ext:atomic-push (list code fork warnings) (jit-stats-failures *jit-stats*)))
             nil))))

(defun jit-evict (cache)
  "With the table locked: when CACHE is over a bound, drop the least recently run entries
until it is within 7/8 of both."
  (let ((table (jit-cache-table cache))
        (limit *jit-cache-limit*)
        (max-bytes *jit-cache-max-bytes*))
    (when (or (> (hash-table-count table) limit) (> (jit-cache-bytes cache) max-bytes))
      (let ((victims (stable-sort (loop for e being the hash-values of table
                                        unless (eq (jit-entry-fn e) :pending) collect e)
                                  #'< :key #'jit-entry-stamp)))
        (loop for e in victims
              while (or (> (hash-table-count table) (floor (* 7 limit) 8))
                        (> (jit-cache-bytes cache) (floor (* 7 max-bytes) 8)))
              do (remhash (jit-entry-key e) table)
                 (decf (jit-cache-bytes cache) (jit-entry-bytes e))
                 (setf (jit-entry-fn e) :evicted)
                 (sb-ext:atomic-incf (jit-stats-evicted *jit-stats*)))))))

(defun jit-settle (cache entry fn bytes)
  "Take ENTRY out of :PENDING: to FN (NIL: :FAILED, :EVICTED: cancelled). Does nothing
when it already left (cancelled while it compiled)."
  (let ((table (jit-cache-table cache)))
    (sb-ext:with-locked-hash-table (table)
      (when (eq (jit-entry-fn entry) :pending)
        (decf (jit-cache-pending cache))
        (cond ((eq fn :evicted)
               (remhash (jit-entry-key entry) table)
               (setf (jit-entry-fn entry) :evicted))
              ((null fn) (setf (jit-entry-fn entry) :failed))
              (t (setf (jit-entry-bytes entry) bytes
                       (jit-entry-stamp entry) (incf **jit-clock**)
                       (jit-entry-fn entry) fn)
                 (incf (jit-cache-bytes cache) bytes)
                 (jit-evict cache)))))))

(defun jit-request (code fork state)
  "The cache entry of CODE for FORK, recorded in STATE: the shared one when a code with
the same bytes has one, else a new :PENDING entry whose compilation went to
*JIT-COMPILER*. NIL when the pending bound or the executor refused it; the code then
counts from zero again."
  (declare (type simple-vector state) (type fixnum fork))
  (let* ((cache *jit-cache*)
         (table (jit-cache-table cache))
         (executor *jit-compiler*)
         (key (jit-key state fork))
         (new nil))
    (let ((entry (sb-ext:with-locked-hash-table (table)
                   (or (gethash key table)
                       (when (and executor (< (jit-cache-pending cache) *jit-max-pending*))
                         (incf (jit-cache-pending cache))
                         (setf new (make-jit-entry key)
                               (gethash key table) new))))))
      (cond ((null entry) (jit-refused state))
            ((not new)
             (when (functionp (jit-entry-fn entry))
               (sb-ext:atomic-incf (jit-stats-shared *jit-stats*)))
             (setf (svref state fork) entry))
            ((funcall (the function executor)
                      (lambda ()
                        (when (eq (jit-entry-fn new) :pending)   ; else cancelled
                          (multiple-value-bind (fn bytes) (jit-compile code fork)
                            (jit-settle cache new fn bytes)))
                        t))
             (setf (svref state fork) new))
            (t (jit-settle cache new :evicted 0)
               (jit-refused state))))))

(defun jit-refused (state)
  (sb-ext:atomic-incf (jit-stats-refused *jit-stats*))
  (setf (svref state +jit-count-slot+) 0)
  nil)

(defun jit-new-state (analysis code fork)
  "ANALYSIS's JIT state, made at its code's first frame under the JIT. A code compiled
before (for another vector with the same bytes, or an analysis since dropped) is adopted
at once rather than counted again."
  (let* ((state (make-array (+ 2 +jit-hash-slot+) :initial-element nil))
         (table (jit-cache-table *jit-cache*)))
    (setf (svref state +jit-hash-slot+) (octets->integer (keccak256 code))
          (svref state +jit-count-slot+) 0)
    (let ((found (gethash (jit-key state fork) table)))
      (when found
        (when (functionp (jit-entry-fn found))
          (sb-ext:atomic-incf (jit-stats-shared *jit-stats*)))
        (setf (svref state fork) found)))
    (setf (analysis-jit analysis) state)))

(defun jit-count (frame state)
  "Interpret FRAME, adding the gas it spent itself to its code's count."
  (declare (type frame frame) (type simple-vector state))
  (let ((gas (frame-gas frame)))
    (multiple-value-bind (status output) (interpret frame)
      (when (member status +success-statuses+)
        (setf (svref state +jit-count-slot+)
              (+ (the fixnum (svref state +jit-count-slot+))
                 (max 0 (- gas (frame-gas frame) (frame-child-gas frame))))))
      (values status output))))

(defun jit-cold (frame analysis)
  "JIT-RUN-FRAME when ANALYSIS has no compiled function for the running fork."
  (declare (type frame frame) (type analysis analysis))
  (let* ((code (frame-code frame))
         (len (length code)))
    (if (or (null *jit-compiler*) (zerop len) (> len *jit-max-code-size*))
        (interpret frame)
        (let* ((fork (spec-fork *spec*))
               (state (or (analysis-jit analysis) (jit-new-state analysis code fork)))
               (entry (or (svref state fork)
                          (and (>= (the fixnum (svref state +jit-count-slot+))
                                   (* *jit-threshold* len))
                               (jit-request code fork state))))
               (fn (and entry (jit-entry-fn entry))))
          (declare (type simple-vector state))
          (cond ((functionp fn) (jit-touch entry) (funcall fn frame))
                ((null fn) (jit-count frame state))
                ((eq fn :evicted)          ; evicted: forget it and count from zero again
                 (setf (svref state fork) nil
                       (svref state +jit-count-slot+) 0)
                 (jit-count frame state))
                (t (interpret frame)))))))  ; :pending or :failed

(defun jit-run-frame (frame)
  "Run FRAME's code compiled when it is, else interpreted (counting it toward compilation)."
  (declare #.*hot* (type frame frame))
  (let* ((analysis (code-analysis (frame-code frame)))
         (state (analysis-jit analysis)))
    (if state
        (let ((entry (svref (the simple-vector state) (spec-fork *spec*))))
          (if entry
              (let ((fn (jit-entry-fn (the jit-entry entry))))
                (if (functionp fn)
                    (progn (jit-touch entry) (funcall fn frame))
                    (jit-cold frame analysis)))
              (jit-cold frame analysis)))
        (jit-cold frame analysis))))

(defun jit-cancel (cache)
  "Cancel every compilation CACHE is waiting for."
  (let ((table (jit-cache-table cache)))
    (sb-ext:with-locked-hash-table (table)
      (loop for e in (loop for e being the hash-values of table
                           when (eq (jit-entry-fn e) :pending) collect e)
            do (decf (jit-cache-pending cache))
               (remhash (jit-entry-key e) table)
               (setf (jit-entry-fn e) :evicted)))))

(defun install-jit-compiler (executor &key (threshold *jit-threshold*))
  "Run untraced frames through JIT-RUN-FRAME, a code being compiled once it has spent
THRESHOLD gas per byte of itself interpreted (0: at its first call). EXECUTOR is called
with a thunk of no arguments that compiles one code and installs it; it must run the thunk
on a thread of its own and return true, or return NIL to refuse it. It is never called
with the cache locked."
  (check-type executor function)
  (setf *jit-threshold* threshold
        *jit-compiler* executor
        *jit* #'jit-run-frame)
  executor)

(defun remove-jit-compiler ()
  "Interpret every frame from now on and hand nothing more to the executor; compilations
it has not run yet are cancelled (their thunks do nothing when they run)."
  (setf *jit* nil
        *jit-compiler* nil)
  (jit-cancel *jit-cache*)
  nil)

(defun jit-compile-synchronously (thunk)
  "An executor that compiles on the calling thread, at once: the differential tools' and
the tests'. A node never installs it."
  (funcall (the function thunk))
  t)

(defun clear-jit-cache (&optional (cache *jit-cache*))
  "Forget every shared compiled function; the codes that ran them count from zero again."
  (jit-cancel cache)
  (let ((table (jit-cache-table cache)))
    (sb-ext:with-locked-hash-table (table)
      (loop for e being the hash-values of table do (setf (jit-entry-fn e) :evicted))
      (clrhash table)
      (setf (jit-cache-bytes cache) 0))))
