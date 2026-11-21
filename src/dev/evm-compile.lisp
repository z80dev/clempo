(defpackage #:clempo.dev.evm-compile
  (:use #:cl #:clempo.base #:clempo.dev)
  (:import-from #:clempo.io #:read-file-string)
  (:documentation "Run-time compilation of EVM code from clempo-dev: the -jit option of
statetest and evm-bench, a background compiler thread, and jit-latency.")
  (:export #:jit-option #:jit-summary #:with-jit-option #:start-background-compiler
           #:stop-background-compiler))

(in-package #:clempo.dev.evm-compile)

;;; Without an option clempo.evm interprets: it compiles only when a compiler is installed.
;;; -jit [N]  compile on the calling thread, every code of any size once it has spent N gas
;;;           per byte interpreted (no N: 0, every code at its first call): the
;;;           differential mode, never a node's
;;; -jit-bg   compile in a background thread with clempo.evm's policy (or -jit N's
;;;           threshold); frames interpret until their code is ready
;;; -interp   never compile: the interpreter alone

(defun jit-option (args)
  "Apply the -jit, -jit-bg and -interp options of a command process-wide (worker
threads see global values). Returns true when compilation is on."
  (let ((jit (arg args :jit))
        (bg (arg-flag args :jit-bg)))
    (cond ((arg-flag args :interp) (stop-background-compiler) (clempo.evm:remove-jit-compiler))
          (bg (start-background-compiler
               :threshold (if (stringp jit) (parse-integer jit) clempo.evm:*jit-threshold*)))
          (jit (setf clempo.evm:*jit-max-code-size* most-positive-fixnum)
               (clempo.evm:install-jit-compiler #'clempo.evm:jit-compile-synchronously
                                                :threshold (if (eq jit t) 0 (parse-integer jit)))))
    (and clempo.evm:*jit* t)))

(defun jit-summary (&optional (stream *standard-output*))
  (let ((s clempo.evm:*jit-stats*))
    (format stream "jit: threshold ~D, ~:D compiled, ~:D shared from cache, ~:D failed, ~:D refused, ~:D evicted~%"
            clempo.evm:*jit-threshold* (clempo.evm:jit-stats-compiled s)
            (clempo.evm:jit-stats-shared s) (clempo.evm:jit-stats-failed s)
            (clempo.evm:jit-stats-refused s) (clempo.evm:jit-stats-evicted s))
    (loop for (code fork reasons) in (clempo.evm:jit-stats-failures s)
          do (format stream "  failed: ~D bytes, ~A: ~A~%" (length code)
                     (clempo.evm:fork-name fork)
                     (let ((r (princ-to-string (first reasons))))
                       (subseq r 0 (min 300 (length r))))))))

;;; The background compiler: one thread draining a queue of compile thunks. clempo.evm
;;; hands it at most clempo.evm:*jit-max-pending* at a time.

(defvar *compiler-thread* nil)
(defvar *queue* '())
(defvar *queue-lock* (sb-thread:make-mutex :name "jit queue"))
(defvar *queue-ready* (sb-thread:make-waitqueue :name "jit queue"))

(defun enqueue (thunk)
  (sb-thread:with-mutex (*queue-lock*)
    (setf *queue* (nconc *queue* (list thunk)))
    (sb-thread:condition-notify *queue-ready*))
  t)

(defun dequeue ()
  (sb-thread:with-mutex (*queue-lock*)
    (loop until *queue* do (sb-thread:condition-wait *queue-ready* *queue-lock*))
    (pop *queue*)))

(defun start-background-compiler (&key (threshold clempo.evm:*jit-threshold*))
  "Compile hot code in a background thread from now on."
  (unless *compiler-thread*
    (setf *compiler-thread*
          (sb-thread:make-thread (lambda ()
                                   (loop for thunk = (dequeue)
                                         until (eq thunk :stop)
                                         do (funcall (the function thunk))))
                                 :name "jit compiler")))
  (clempo.evm:install-jit-compiler #'enqueue :threshold threshold))

(defun stop-background-compiler ()
  (when *compiler-thread*
    (clempo.evm:remove-jit-compiler)
    (enqueue :stop)
    (sb-thread:join-thread *compiler-thread*)
    (setf *compiler-thread* nil)))

(defmacro with-jit-option ((on args) &body body)
  "Run BODY with ARGS' -jit, -jit-bg or -interp applied and ON true when compilation is
on; then print the JIT summary (when on) and stop the background compiler, if any."
  `(let ((,on (jit-option ,args)))
     (unwind-protect (progn ,@body)
       (when ,on (jit-summary))
       (stop-background-compiler))))

;;; jit-latency: compile time against code size, over the distinct contracts of the
;;; state-test fixtures; every one must compile without a warning.

(defun fixture-codes (dir limit)
  "Up to LIMIT distinct non-empty codes from the fixture alloc files under DIR."
  (let ((seen (make-hash-table :test 'equal))
        (codes '()))
    (dolist (file (sort (directory (merge-pathnames "**/*.json" dir)) #'string< :key #'namestring)
                  codes)
      (when (>= (length codes) limit) (return codes))
      (let ((doc (clempo.json:parse-json (read-file-string file))))
        (loop for (nil . test) in (cdr doc)
              do (loop for (nil . acct) in (cdr (clempo.json:jget test "pre"))
                       for hex = (clempo.json:jget acct "code")
                       when (and (stringp hex) (> (length hex) 2) (not (gethash hex seen)))
                         do (setf (gethash hex seen) t)
                            (push (hex->octets hex) codes)))))))

(defun seconds-since (start)
  (/ (- (clempo.io:monotonic-ticks) start) internal-time-units-per-second))

(defcommand "jit-latency" (args)
  "compile latency by code size: jit-latency [dir] [-n contracts] [-fork Osaka]"
  (let* ((dir (or (first (arg args :rest))
                  (namestring (merge-pathnames "cache/clempo/fixtures/fixtures/state_tests/"
                                               (user-homedir-pathname)))))
         (fork (clempo.evm:parse-fork (arg args :fork "Osaka")))
         (codes (fixture-codes dir (arg-int args :n 1000000)))
         (buckets '((0 . 64) (64 . 256) (256 . 1024) (1024 . 4096) (4096 . 16384) (16384 . 65536)))
         (rows (make-array (length buckets) :initial-element nil))
         (failed 0))
    (dolist (code codes)
      (let ((start (clempo.io:monotonic-ticks)))
        (multiple-value-bind (fn warnings)
            (handler-case (clempo.evm:compile-code code fork)
              (serious-condition (c) (values nil (list (princ-to-string c)))))
          (when (or (null fn) warnings)
            (incf failed)
            (format t "failed: ~A~%  ~A~%" (octets->hex code) (first warnings))))
        (let ((secs (seconds-since start))
              (b (position-if (lambda (r) (< (length code) (cdr r))) buckets)))
          (when b (push (cons (length code) secs) (aref rows b))))))
    (format t "~&~12A ~8A ~12A ~12A ~12A~%" "bytes" "codes" "mean ms" "max ms" "us/byte")
    (loop for (lo . hi) in buckets
          for row across rows
          when row
            do (let ((n (length row))
                     (secs (reduce #'+ row :key #'cdr))
                     (bytes (reduce #'+ row :key #'car)))
                 (format t "~12A ~8D ~12,2F ~12,2F ~12,1F~%" (format nil "~D-~D" lo hi) n
                         (* 1000 (/ secs n)) (* 1000 (reduce #'max row :key #'cdr))
                         (/ (* 1d6 secs) (max 1 bytes)))))
    (format t "jit-latency: ~D codes, ~D failed, ~,1F s compiling~%" (length codes) failed
            (loop for row across rows sum (reduce #'+ row :key #'cdr)))
    (if (zerop failed) 0 1)))
