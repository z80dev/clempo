(defpackage #:clempo.dev
  (:use #:cl #:clempo.base)
  (:documentation "clempo-dev: the measurement and development commands. Each file in
src/dev/ adds commands with DEFCOMMAND; this file only dispatches.")
  (:export #:defcommand #:main #:run-command #:arg #:arg-int #:arg-flag #:log-line))

(in-package #:clempo.dev)

(defvar *commands* (make-hash-table :test 'equal)
  "name -> (function . docstring)")

(defmacro defcommand (name (args-var) doc &body body)
  "Define a clempo-dev subcommand. ARGS-VAR is bound to the parsed argument plist:
-flag value pairs become (:flag \"value\"); a -flag followed by another flag or nothing
becomes (:flag t). Positional words collect under :rest. The body returns an exit code."
  `(setf (gethash ,name *commands*)
         (cons (lambda (,args-var) (declare (ignorable ,args-var)) ,@body) ,doc)))

(defun parse-args (words)
  (let ((plist '()) (rest '()))
    (loop while words
          do (let ((w (pop words)))
               (if (and (> (length w) 1) (char= (char w 0) #\-)
                        (not (digit-char-p (char w 1))))
                   (let ((key (intern (string-upcase (string-left-trim "-" w)) :keyword)))
                     (if (and words (not (and (> (length (first words)) 1)
                                              (char= (char (first words) 0) #\-)
                                              (not (digit-char-p (char (first words) 1))))))
                         (setf (getf plist key) (pop words))
                         (setf (getf plist key) t)))
                   (push w rest))))
    (list* :rest (nreverse rest) plist)))

(defun arg (args key &optional default) (getf args key default))

(defun arg-int (args key &optional default)
  (let ((v (getf args key)))
    (cond ((null v) default)
          ((and (> (length v) 2) (string-equal "0x" (subseq v 0 2))) (parse-integer v :start 2 :radix 16))
          (t (parse-integer (remove #\_ (remove #\, v)))))))

(defun arg-flag (args key &optional default)
  (let ((v (getf args key :absent)))
    (cond ((eq v :absent) default)
          ((eq v t) t)
          (t (not (member v '("false" "0" "no") :test #'string-equal))))))

(defun log-line (fmt &rest args)
  (format *error-output* "~&~?~%" fmt args)
  (force-output *error-output*))

(defun usage ()
  (format t "usage: clempo-dev <command> [-flag value ...]~%~%")
  (let ((names (sort (loop for k being the hash-keys of *commands* collect k) #'string<)))
    (dolist (n names)
      (format t "  ~16A ~A~%" n (cdr (gethash n *commands*)))))
  2)

(defun run-command (name words)
  (let ((entry (gethash name *commands*)))
    (if entry
        (funcall (car entry) (parse-args words))
        (progn (format *error-output* "unknown command ~S~%" name) (usage)))))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  "Entry point: (main '(\"score\" \"-rung\" \"R1\"))."
  (let ((code (if (null argv)
                  (usage)
                  (handler-case (run-command (first argv) (rest argv))
                    (sb-sys:interactive-interrupt () 130)))))
    (finish-output)
    (sb-ext:exit :code (if (integerp code) code 0) :abort nil)))

(defcommand "help" (args)
  "list commands"
  (usage)
  0)
