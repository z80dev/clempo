;;; Source audit. Load systems to obtain the actual package/export/nickname
;;; environment, then read source without executing #. or selecting only the
;;; host's reader-conditional branches. Loaded after scripts/build.lisp.

(in-package #:clempo-build)

(defparameter *native-io-packages*
  '("SB-BSD-SOCKETS" "SB-POSIX" "SB-THREAD" "SB-CONCURRENCY" "SB-UNIX" "SB-ALIEN"
    "SB-PROFILE" "SB-SPROF"))

(defparameter *native-clock-and-random*
  '(get-universal-time get-internal-real-time get-internal-run-time
    random make-random-state *random-state* sb-ext:get-time-of-day sb-ext:seed-random-state))

(defparameter *native-file-operations*
  '(open with-open-file file-write-date file-author delete-file rename-file
    probe-file directory ensure-directories-exist truename user-homedir-pathname load compile-file))

(defparameter *native-io-operations*
  '(sb-ext:run-program sb-ext:posix-getenv uiop:run-program uiop:launch-program
    uiop:delete-file-if-exists uiop:directory-files uiop:subdirectories
    uiop:getenv sleep))

(defun loop-word-p (word &rest names)
  (and (symbolp word) (member (symbol-name word) names :test #'string=)))

(defun native-clock-p (symbol)
  (or (member symbol *native-clock-and-random*)
      (let ((package (symbol-package symbol)))
        (and package (string= (package-name package) "SB-UNIX")
             (loop-word-p symbol "UNIX-GETTIMEOFDAY" "UNIX-CLOCK-GETTIME" "UNIX-TIME")))))

(defun native-io-operator-p (symbol)
  (or (member symbol *native-file-operations*) (member symbol *native-io-operations*)))

(defun bootstrap-timer-p (path symbol)
  (and (string= path "src/base/test.lisp") (eq symbol 'get-internal-real-time)))

(defun bounded-loop-p (clauses)
  ;; LOOP's finite built-in iteration drivers. A predicate or THEN alone
  ;; need not ever terminate.
  (loop for (clause var driver . rest) on clauses
        thereis
        (or (loop-word-p clause "REPEAT")
            (and (loop-word-p clause "FOR" "AS" "AND")
                 (or (loop-word-p driver "IN" "ON" "ACROSS")
                     (and (loop-word-p driver "BEING")
                          (loop-word-p (first rest) "EACH" "THE")
                          (loop-word-p (second rest) "HASH-KEY" "HASH-KEYS" "HASH-VALUE" "HASH-VALUES"
                                                     "SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
                                                     "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS"))
                     (loop-word-p driver "TO" "UPTO" "DOWNTO" "BELOW" "ABOVE")
                     (and (loop-word-p driver "FROM" "UPFROM" "DOWNFROM")
                          (or (loop-word-p (second rest) "TO" "UPTO" "DOWNTO" "BELOW" "ABOVE")
                              (and (loop-word-p (second rest) "BY")
                                   (loop-word-p (fourth rest) "TO" "UPTO" "DOWNTO" "BELOW" "ABOVE")))))))))

(defun bounded-do-p (form)
  ;; One counter moving toward its terminating inequality.
  (let ((end (third form)) (bindings (second form)))
    (when (and (consp end) (consp (first end)) (listp bindings))
      (let* ((test (first end)) (direction (first test)) (var (second test))
             (binding (find var bindings :key (lambda (x) (if (consp x) (car x) x))))
             (step (and (consp binding) (third binding)))
             (up (member direction '(>= >))))
        (and (member direction '(>= > <= <))
             (symbolp var) (consp step) (eq var (second step))
             (or (eq (first step) (if up '1+ '1-))
                 (and (eq (first step) (if up '+ '-))
                      (typep (third step) '(integer 1 *)))))))))

(defun source-files (dir)
  (directory (merge-pathnames (format nil "src/~A/**/*.lisp" dir) *root*)))

(defun source-read-eval (stream subchar argument)
  (declare (ignore subchar argument))
  ;; Keep references used inside #. visible, but do not evaluate them.
  (list 'source-read-eval (read stream t nil t)))

(defun source-read-conditional (stream subchar argument)
  (declare (ignore subchar argument))
  ;; Read the feature designator as the standard reader does; audit the
  ;; guarded code regardless of whether the running image selects this branch.
  (let ((*package* (find-package "KEYWORD")))
    (read stream t nil t))
  (read stream t nil t))

(defun read-source-stream (in origin)
  (with-standard-io-syntax
    (let ((*readtable* (copy-readtable nil)) (*read-eval* nil)
          (*package* (find-package "COMMON-LISP-USER"))
          (eof (gensym "EOF")))
      (set-dispatch-macro-character #\# #\. #'source-read-eval)
      (set-dispatch-macro-character #\# #\+ #'source-read-conditional)
      (set-dispatch-macro-character #\# #\- #'source-read-conditional)
      (loop for form = (read in nil eof) until (eq form eof)
            when (and (consp form) (eq (car form) 'in-package))
              do (setf *package* (or (find-package (second form))
                                    (error "Unknown in-package in ~A: ~A" origin (second form))))
            collect form))))

(defun read-source (f)
  (with-open-file (in f :external-format :utf-8)
    (read-source-stream in f)))

(defun package-mentions (form)
  ;; DEFPACKAGE options use string designators, not references with home packages.
  (flet ((name (x) (and x (string x))))
    (case (and (consp form) (car form))
      (in-package (list (name (second form))))
      (defpackage
       (loop for (option . values) in (cddr form)
             append (case option
                      (:use (mapcar #'name values))
                      ((:import-from :shadowing-import-from) (list (name (first values))))
                      (:local-nicknames (mapcar (lambda (pair) (name (second pair))) values))))))))

(defun audit-form (form)
  "Return symbol references, syntactic operator references and iteration counts.
Walk quoted and backquoted code too: macros can generate executable forms from them."
  (let ((symbols (make-hash-table :test #'eq))
        (operators (make-hash-table :test #'eq))
        (hash-scans 0)
        (unbounded-loops 0))
    (labels ((note-operator (op)
               (when (symbolp op)
                 (incf (gethash op operators 0))
                 (when (member op '(maphash with-hash-table-iterator)) (incf hash-scans))))
             (visit-list (x)
               (loop for tail = x then (cdr tail) while (consp tail)
                     do (visit (car tail))
                     finally (unless (null tail) (visit tail))))
             (visit-bindings (x)
               (loop for tail = x then (cdr tail) while (consp tail)
                     for binding = (car tail)
                     do (if (consp binding) (visit-list binding) (visit binding))
                     finally (unless (null tail) (visit tail))))
             (visit (x)
               (typecase x
                 (symbol (setf (gethash x symbols) t))
                 (cons
                  (let ((op (car x)) (args (cdr x)))
                    (note-operator op)
                    (when (and (eq op 'function) (consp args))
                      (note-operator (first args)))
                    (when (and (member op '(funcall apply)) (consp args)
                               (consp (first args)) (eq (caar args) 'quote))
                      (note-operator (second (first args))))
                    (when (or (and (member op '(do do*)) (not (bounded-do-p x)))
                              (and (eq op 'loop) (not (bounded-loop-p args))))
                      (incf unbounded-loops))
                    (when (eq op 'loop)
                      (loop for (word article kind) on args
                            when (and (loop-word-p word "BEING") (loop-word-p article "EACH" "THE")
                                      (loop-word-p kind "HASH-KEY" "HASH-KEYS" "HASH-VALUE" "HASH-VALUES"))
                              do (incf hash-scans)))
                    (visit op)
                    (cond ((and (member op '(let let* lambda)) (consp args))
                           ;; OPEN in a binding name isn't a call to CL:OPEN.
                           (visit-bindings (first args))
                           (visit-list (rest args)))
                          ((and (member op '(defun defmacro define-compiler-macro)) (consp args))
                           (visit (first args))
                           (visit-bindings (second args))
                           (visit-list (cddr args)))
                          (t (visit-list args)))))
                 (sb-impl::comma (visit (sb-impl::comma-expr x)))
                 ((or string bit-vector) nil)
                 (array
                  (dotimes (i (array-total-size x)) (visit (row-major-aref x i)))))))
      (visit form))
    (values symbols operators hash-scans unbounded-loops)))

(defun native-violations (dir path io-p symbols operators packages report)
  (unless io-p
    (dolist (package packages)
      (when (member (package-name package) *native-io-packages* :test #'string=)
        (funcall report "native I/O package ~A outside the I/O boundary" (package-name package)))))
  (unless (string= dir "io")
    (maphash (lambda (op ignored)
               (declare (ignore ignored))
               (when (and (native-clock-p op) (not (bootstrap-timer-p path op)))
                 (funcall report "native clock or entropy ~S must go through clempo.io" op)))
             operators)
    (when (gethash '*random-state* symbols)
      (funcall report "native entropy *RANDOM-STATE* must go through clempo.io")))
  (unless io-p
    (maphash (lambda (op ignored)
               (declare (ignore ignored))
               (when (native-io-operator-p op)
                 (funcall report "native I/O operation ~S outside the I/O boundary" op)))
             operators)))

(defun layer-definition (form)
  (if (and (consp form) (consp (cdr form)))
      (let ((name (second form)))
        (typecase name ((or symbol string) (string name)) (t (prin1-to-string name))))
      "<top level>"))

(defun read-layer-policy ()
  (with-open-file (in (merge-pathnames "scripts/layer-policy.sexp" *root*) :external-format :utf-8)
    (let ((*read-eval* nil))
      (read in))))

(defun record-site-use (uses path definition count)
  (when (plusp count)
    (incf (gethash (list path definition) uses 0) count)))

(defun check-iteration-uses (policy kind uses description report)
  (dolist (file (getf policy kind))
    (dolist (rule (cdr file))
      (destructuring-bind (definition limit reason) rule
        (let* ((path (car file))
               (key (list path definition))
               (actual (gethash key uses 0)))
          (unless (and (typep limit '(integer 1 *)) (plusp (length reason)))
            (error "Invalid ~A policy rule for ~A (~A)" kind path definition))
          (unless (= actual limit)
            (funcall report "~A (~A): ~D ~A sites; policy declares ~D" path definition actual description limit))
          (remhash key uses)))))
  (maphash (lambda (key actual)
             (funcall report "~A (~A): ~D ~A sites without a policy entry"
                      (first key) (second key) actual description))
           uses))

(defun form-packages (form symbols)
  (let ((packages (remove nil (mapcar #'find-package (package-mentions form)))))
    (maphash (lambda (symbol ignored)
               (declare (ignore ignored))
               (let ((package (symbol-package symbol)))
                 (when package (pushnew package packages))))
             symbols)
    packages))

(defun assertions-in (operators)
  (loop for op being the hash-keys of operators using (hash-value count)
        when (or (member op '(assert check-type error))
                 (and (symbol-package op)
                      (string= (package-name (symbol-package op)) "CLEMPO.BASE")
                      (loop-word-p op "INVARIANT" "UNREACHABLE")))
          sum count))

(defun check-thread-owners (policy starts joins report)
  (loop for (path start limit owner reason) in (getf policy :threads)
        for uses = (gethash (list path start) starts 0)
        do (unless (and (typep limit '(integer 1 *)) (plusp (length reason)))
             (error "Invalid thread ownership rule for ~A (~A)" path start))
           (unless (= uses limit)
             (funcall report "~A (~A): ~D thread start~:P, policy declares ~D" path start uses limit))
           (unless (plusp (gethash (list path owner) joins 0))
             (funcall report "~A (~A): join owner ~A has no JOIN-THREAD reference" path start owner))
           (remhash (list path start) starts))
  (maphash (lambda (key uses)
             (funcall report "~A (~A): ~D thread start~:P without a declared join owner"
                      (first key) (second key) uses))
           starts))

(defun check-layers ()
  (let* ((layers (layers))
         (policy (read-layer-policy))
         (owner (make-hash-table :test 'eq))
         (units (make-hash-table :test 'equal))
         (loops (make-hash-table :test 'equal))
         (hash-order (make-hash-table :test 'equal))
         (starts (make-hash-table :test 'equal))
         (joins (make-hash-table :test 'equal))
         (assertions (make-hash-table :test 'equal))
         (violations '()))
    (dolist (layer layers)
      (let ((dir (getf layer :dir)))
        (dolist (file (source-files dir))
          (let ((forms (read-source file)))
            (push (cons file forms) (gethash dir units))
            (dolist (form forms)
              (when (and (consp form) (eq (car form) 'defpackage))
                (let* ((package (find-package (second form)))
                       (prior (gethash package owner)))
                  (when package
                    (when (and prior (string/= dir prior))
                      (push (format nil "~A: package ~A already owned by ~A" file package prior) violations))
                    (setf (gethash package owner) dir)))))))))
    (loop for layer in layers
          for dir = (getf layer :dir)
          collect dir into allowed
          do (dolist (unit (gethash dir units))
               (destructuring-bind (file . forms) unit
                 (let* ((path (enough-namestring file *root*))
                        (layer-root (merge-pathnames (format nil "src/~A/" dir) *root*))
                        (relative (enough-namestring file layer-root))
                        (io-p (or (getf layer :io)
                                  (member relative (getf layer :io-exempt) :test #'string=))))
                   (dolist (form forms)
                     (let ((definition (layer-definition form)))
                       (flet ((report (control &rest args)
                                (pushnew (format nil "~A (~A): ~A" path definition (apply #'format nil control args))
                                         violations :test #'string=)))
                         (multiple-value-bind (symbols operators hash-scans unbounded-loops) (audit-form form)
                           (let ((packages (form-packages form symbols)))
                             (dolist (package packages)
                               (let ((name (package-name package)) (target (gethash package owner)))
                                 (cond ((and target (not (member target allowed :test #'string=)))
                                        (report "~A layer may not use ~A (layer ~A)" dir name target))
                                       ((and (not target) (eql 0 (search "CLEMPO." name)))
                                        (report "unknown project package ~A" name)))))
                             (native-violations dir path io-p symbols operators packages #'report))
                           (unless io-p
                             (record-site-use hash-order path definition hash-scans)
                             (record-site-use loops path definition unbounded-loops))
                           (let ((start (gethash 'sb-thread:make-thread operators))
                                 (join (gethash 'sb-thread:join-thread operators)))
                             (when start
                               (incf (gethash (list path definition) starts 0) start)
                               (unless io-p (report "thread created outside the declared I/O boundary")))
                             (when join (incf (gethash (list path definition) joins 0) join)))
                           (incf (gethash path assertions 0) (assertions-in operators))))))))))
    (flet ((report (control &rest args) (push (apply #'format nil control args) violations)))
      (check-iteration-uses policy :loops loops "unproven iteration" #'report)
      (check-iteration-uses policy :hash-order hash-order "hash traversal" #'report)
      (check-thread-owners policy starts joins #'report))
    (loop for (path count reason) in (getf policy :assertions)
          do (unless (and (plusp count) (plusp (length reason)))
               (error "Invalid assertion rule for ~A" path))
             (when (< (gethash path assertions 0) count)
               (push (format nil "~A: consensus-critical module lost its required assertions" path)
                     violations)))
    (setf violations (sort violations #'string<))
    (dolist (v violations) (format t "~&layers: ~A~%" v))
    (format t "~&layers: ~D violation~:P~%" (length violations))
    (null violations)))

(unless (member :clempo-layer-check-library *features*)
  (load-all)
  (load (merge-pathnames "tests/dev/layer-check-tests.lisp" *root*))
  (unless (check-layers) (sb-ext:exit :code 1 :abort t)))
