(in-package #:clempo.base)

;;; A small test harness: DEFTEST registers a named test, IS records a failure with the
;;; failing form and keeps going, and a test that signals an unhandled error fails.

(defvar *tests* '() "Registered tests, newest first: (name . function).")
(defvar *test-verbose* nil)
(defvar *current-failures* nil)

(define-condition test-skipped (condition)
  ((reason :initarg :reason :reader test-skipped-reason)))

(defmacro deftest (name () &body body)
  `(progn
     (setf *tests* (cons (cons ',name (lambda () ,@body))
                         (remove ',name *tests* :key #'car)))
     ',name))

(defun record-failure (fmt &rest args)
  (push (apply #'format nil fmt args) *current-failures*)
  nil)

(defmacro is (form &optional control &rest args)
  `(or ,form
       (record-failure "~S~@[: ~A~]" ',form ,(when control `(format nil ,control ,@args)))))

(defmacro is= (expected form &optional control &rest args)
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,form))
       (or (equal ,e ,a)
           (record-failure "~S~@[ (~A)~]~%    expected ~S~%    got      ~S" ',form
                           ,(when control `(format nil ,control ,@args)) ,e ,a)))))

(defmacro is-equalp (expected form &optional control &rest args)
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,form))
       (or (equalp ,e ,a)
           (record-failure "~S~@[ (~A)~]~%    expected ~S~%    got      ~S" ',form
                           ,(when control `(format nil ,control ,@args)) ,e ,a)))))

(defmacro signals (condition-type form)
  `(handler-case (progn ,form
                        (record-failure "~S did not signal ~S" ',form ',condition-type))
     (,condition-type () t)))

(defun skip-test (reason)
  (signal 'test-skipped :reason reason)
  (throw 'skip reason))

(defun test-package-name (name)
  (package-name (symbol-package name)))

(defun run-tests (&key (prefixes nil) (stream *standard-output*))
  "Run registered tests (oldest first) whose package name starts with one of PREFIXES
\(all when NIL). Returns T when every test passed."
  (let ((passed 0) (failed 0) (skipped 0))
    (dolist (entry (reverse *tests*))
      (destructuring-bind (name . fn) entry
        (when (or (null prefixes)
                  (some (lambda (p) (eql 0 (search p (test-package-name name)))) prefixes))
          (let ((*current-failures* '())
                (start (get-internal-real-time))
                (skip-reason nil))
            (setf skip-reason
                  (catch 'skip
                    (handler-case (progn (funcall fn) nil)
                      (error (e)
                        (record-failure "unhandled ~A: ~A" (type-of e) e)
                        nil))))
            (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                             internal-time-units-per-second)))
              (cond (skip-reason
                     (incf skipped)
                     (format stream "~&SKIP ~S: ~A~%" name skip-reason))
                    (*current-failures*
                     (incf failed)
                     (format stream "~&FAIL ~S (~D ms)~%" name ms)
                     (dolist (f (reverse *current-failures*))
                       (format stream "  ~A~%" f)))
                    (t
                     (incf passed)
                     (when (or *test-verbose* (> ms 2000))
                       (format stream "~&ok   ~S (~D ms)~%" name ms)))))))))
    (format stream "~&~D passed, ~D failed, ~D skipped~%" passed failed skipped)
    (zerop failed)))

(defun test-failure-count () (length *current-failures*))
