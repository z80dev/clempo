(in-package #:clempo.dev)

;;; The ladder. Each rung's scorer lives with the code that climbs it and registers here
;;; with DEFRUNG; `clempo-dev score` runs one rung or all of them in ladder order.

(defvar *rungs* '() "Ordered list of (name doc function).")

(defparameter *ladder* '("R0" "R1" "R2" "R3" "R3e" "R3p" "R4" "R5" "R6" "R6-devnet" "R7"))

(define-condition score-input-error (simple-error) ())

(defun score-input-error (format-control &rest format-arguments)
  (error 'score-input-error :format-control format-control :format-arguments format-arguments))

(defun required-score-option (args key usage)
  (let ((value (arg args key)))
    (unless (and (stringp value) (plusp (length value)))
      (score-input-error "~A" usage))
    value))

(defmacro defrung (name (args-var) doc &body body)
  "Register the scorer for rung NAME. The body returns a JSON object (clempo.json:jobject)
that must include a \"score\" member; it may include anything else worth recording."
  `(progn
     (setf *rungs* (remove ,name *rungs* :key #'first :test #'string=))
     (setf *rungs*
           (sort (cons (list ,name ,doc (lambda (,args-var) (declare (ignorable ,args-var)) ,@body))
                       *rungs*)
                 #'< :key (lambda (r) (or (position (first r) *ladder* :test #'string=) 99))))
     ,name))

(defun today ()
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (+ (clempo.io:now-seconds) (encode-universal-time 0 0 0 1 1 1970 0)) 0)
    (declare (ignore s m h))
    (format nil "~D-~2,'0D-~2,'0D" year month day)))

(defcommand "score" (args)
  "measure rungs: score [-rung NAME] [-json]; score -help lists input requirements"
  (let* ((wanted (arg args :rung))
         (rungs (if wanted
                    (remove-if-not (lambda (r) (string-equal (first r) wanted)) *rungs*)
                    *rungs*))
         (json (arg-flag args :json)))
    (when (arg-flag args :help)
      (dolist (r rungs 0) (format t "~A: ~A~%" (first r) (second r)))
      (return-from command 0))
    (when (null rungs)
      (log-line "no scorer for rung ~A; known: ~{~A~^ ~}" wanted (mapcar #'first *rungs*))
      (return-from command 2))
    (let ((exit-code 0))
      (dolist (r rungs exit-code)
        (destructuring-bind (name doc fn) r
          (declare (ignore doc))
          (handler-case
              (multiple-value-bind (result code) (funcall fn args)
                (setf exit-code (max exit-code (or code 0)))
                (if json
                    (write-line (clempo.json:to-json-string
                                 (append (clempo.json:jobject "date" (today) "rung" name) (cdr result))))
                    (format t "~A: ~A~%" name (clempo.json:jget result "score"))))
            (score-input-error (err)
              (log-line "~A: ~A" name err)
              (setf exit-code 2))))))))

(defrung "R0" (args)
  "the gate passes"
  (multiple-value-bind (out code) (clempo.io:run-capture "scripts/verify.sh" '())
    (declare (ignore out))
    (clempo.json:jobject "score" (if (zerop code) "yes" "no"))))

(defrung "R4" (args)
  "run a certified live hour with RPC probes: -datadir STOPPED_DIR -run-dir NEW_DIR"
  (let* ((usage "Use score -rung R4 -datadir STOPPED_DIR -run-dir NEW_DIR.")
         (state (required-score-option args :datadir usage))
         (run (required-score-option args :run-dir usage))
         (code (sb-ext:process-exit-code
                (sb-ext:run-program "bash" (list "scripts/r4/run-hour.sh" state run)
                                    :search t :output *error-output* :error *error-output*))))
    (unless (member code '(0 1))
      (score-input-error "Hour harness failed before grading (exit ~A); see ~A." code run))
    (let ((report (merge-pathnames "r4.json" (uiop:ensure-directory-pathname run))))
      (unless (probe-file report)
        (score-input-error "Hour harness did not produce ~A." report))
      (values (clempo.json:parse-json (clempo.io:read-file-string report)) code))))

(defrung "R7" (args)
  "measure root-checked throughput: -range early|busy|recent -result JSON [bench options]; busy/recent require -parent and -datadir"
  (let* ((usage "Use score -rung R7 -range early|busy|recent -result FILE with the inputs from `clempo-dev bench`.")
         (range (required-score-option args :range usage))
         (result (expand-home (required-score-option args :result usage)))
         (options (loop for key in '(:parent :datadir :sender-threads :profile :profile-mode :profile-threads)
                        for value = (arg args key)
                        when value append (list (format nil "-~(~A~)" key) value)))
         (code (run-command "bench" (list* "-range" range "-result" result options)))
         (report (clempo.json:parse-json (clempo.io:read-file-string result))))
    (values (append (clempo.json:jobject "score" (if (zerop code)
                                                    (clempo.json:jget report "blocks_per_s")
                                                    "incomplete")
                                         "comparison" "clempo only; pair with Rust on the same host")
                    (cdr report))
            code)))
