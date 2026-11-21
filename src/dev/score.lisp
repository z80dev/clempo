(in-package #:clempo.dev)

;;; The ladder. Each rung's scorer lives with the code that climbs it and registers here
;;; with DEFRUNG; `clempo-dev score` runs one rung or all of them in ladder order.

(defvar *rungs* '() "Ordered list of (name doc function).")

(defparameter *ladder* '("R0" "R1" "R2" "R3" "R3e" "R3p" "R4" "R5" "R6" "R7"))

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
  (multiple-value-bind (s m h day month year) (decode-universal-time (get-universal-time) 0)
    (declare (ignore s m h))
    (format nil "~D-~2,'0D-~2,'0D" year month day)))

(defcommand "score" (args)
  "measure rungs: score [-rung R1] [-json]"
  (let* ((wanted (arg args :rung))
         (rungs (if wanted
                    (remove-if-not (lambda (r) (string-equal (first r) wanted)) *rungs*)
                    *rungs*))
         (json (arg-flag args :json)))
    (when (null rungs)
      (log-line "no scorer for rung ~A; known: ~{~A~^ ~}" wanted (mapcar #'first *rungs*))
      (return-from command 2))
    (dolist (r rungs 0)
      (destructuring-bind (name doc fn) r
        (declare (ignore doc))
        (let* ((result (funcall fn args))
               (row (append (clempo.json:jobject "date" (today) "rung" name) (cdr result))))
          (if json
              (write-line (clempo.json:to-json-string row))
              (format t "~A: ~A~%" name (clempo.json:jget result "score"))))))))

(defrung "R0" (args)
  "the gate passes"
  (multiple-value-bind (out code) (clempo.io:run-capture "scripts/verify.sh" '())
    (declare (ignore out))
    (clempo.json:jobject "score" (if (zerop code) "yes" "no"))))
