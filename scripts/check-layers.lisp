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

;;; Fixed-count loops in hot code take their count from a DEFBOUND limit, whose
;;; definition proves at compile time what the count is safe for (clempo.base's
;;; bounds.lisp). A count is fixed when it is a compile-time integer expression;
;;; loops over data (a length, a list, a variable) are bounded by that data instead.

(defun named-p (x package name)
  (and (symbolp x) (symbol-package x)
       (string= (package-name (symbol-package x)) package)
       (string= (symbol-name x) name)))

(defun hot-form-p (form)
  "True when FORM declares the hot build policy (#.*HOT*) or SPEED 3 anywhere in it."
  (labels ((walk (x)
             (typecase x
               (cons (or (and (eq (car x) 'source-read-eval) (consp (cdr x))
                              (symbolp (second x)) (string= (symbol-name (second x)) "*HOT*"))
                         (and (eq (car x) 'optimize) (member '(speed 3) (cdr x) :test #'equal))
                         (loop for tail = x then (cdr tail) while (consp tail)
                                 thereis (walk (car tail)))))
               (sb-impl::comma (walk (sb-impl::comma-expr x))))))
    (walk form)))

(defun bound-names (forms)
  "Names defined by DEFBOUND anywhere in FORMS."
  (let ((names '()))
    (labels ((walk (x)
               (when (consp x)
                 (if (and (named-p (car x) "CLEMPO.BASE" "DEFBOUND") (consp (cdr x)) (symbolp (second x)))
                     (push (second x) names)
                     (loop for tail = x then (cdr tail) while (consp tail) do (walk (car tail)))))))
      (mapc #'walk forms))
    names))

(defun constant-count-p (x)
  "True when X is an integer expression the compiler knows: literals, integer constants,
read-time values, and arithmetic over them."
  (typecase x
    (integer t)
    (symbol (and x (not (keywordp x)) (constantp x) (boundp x) (integerp (symbol-value x))))
    (cons (or (eq (car x) 'source-read-eval)
              (and (member (car x) '(+ - * 1+ 1- ash floor ceiling truncate round min max
                                     logand logior expt mod rem integer-length))
                   (every #'constant-count-p (cdr x)))))))

(defun cites-bound-p (x bounds)
  (typecase x
    (symbol (gethash x bounds))
    (cons (loop for tail = x then (cdr tail) while (consp tail)
                  thereis (cites-bound-p (car tail) bounds)))))

(defun fixed-counts (form)
  "The count expressions of FORM if it is an iteration that runs a fixed number of
times: each is a list of the expressions that together set one count."
  (flet ((word (x &rest names) (apply #'loop-word-p x names)))
    (case (and (consp form) (car form))
      (dotimes (let ((spec (second form)))
                 (when (and (consp spec) (consp (cdr spec)) (constant-count-p (second spec)))
                   (list (list (second spec))))))
      ((do do*) (let* ((end (third form)) (test (and (consp end) (first end)))
                       (bindings (second form)))
                  (when (and (consp test) (member (first test) '(< <= > >=)) (listp bindings))
                    (let ((binding (find (second test) bindings :key (lambda (b) (if (consp b) (car b) b)))))
                      (when (and (consp binding) (constant-count-p (second binding))
                                 (constant-count-p (third test)))
                        (list (list (second binding) (third test))))))))
      (loop (let ((tokens (cdr form)) (counts '()))
              (loop for (token next) on tokens
                    when (and (word token "REPEAT") (constant-count-p next))
                      do (push (list next) counts))
              ;; Each FOR/AS/AND clause: FROM start (default 0) and TO/BELOW/... end.
              (loop for tail on tokens
                    when (word (car tail) "FOR" "AS" "AND")
                      do (let ((start 0) (end nil) (numeric nil))
                           (loop for (token next) on (cdr tail)
                                 until (word token "FOR" "AS" "AND" "WITH" "WHILE" "UNTIL" "DO" "DOING"
                                             "REPEAT" "COLLECT" "COLLECTING" "APPEND" "NCONC" "SUM" "COUNT"
                                             "MAXIMIZE" "MINIMIZE" "WHEN" "IF" "UNLESS" "RETURN"
                                             "FINALLY" "INITIALLY" "ALWAYS" "NEVER" "THEREIS")
                                 do (cond ((word token "FROM" "UPFROM" "DOWNFROM") (setf start next))
                                          ((word token "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE")
                                           (setf end next numeric t))))
                           (when (and numeric (constant-count-p start) (constant-count-p end))
                             (push (list start end) counts))))
              (nreverse counts))))))

(defun uncited-hot-loops (form bounds)
  "Fixed counts in FORM, a hot definition, that name no DEFBOUND limit."
  (let ((uncited '()))
    (labels ((walk (x)
               (typecase x
                 (cons
                  (unless (eq (car x) 'source-read-eval)
                    (dolist (count (fixed-counts x))
                      (unless (cites-bound-p count bounds) (push count uncited)))
                    (loop for tail = x then (cdr tail) while (consp tail) do (walk (car tail)))))
                 (sb-impl::comma (walk (sb-impl::comma-expr x))))))
      (walk form))
    (nreverse uncited)))

;;; Locks are taken through clempo.lock, which checks their order in check builds.

(defparameter *untracked-lock-operators*
  '(sb-thread:with-mutex sb-thread:with-recursive-lock sb-thread:grab-mutex
    sb-thread:release-mutex sb-thread:condition-wait))

(defun lock-violations (path operators report)
  ;; The checkers' own locks (lock order, guard violations) stay outside what they check.
  (unless (member path '("src/io/locks.lisp" "src/io/guards.lisp") :test #'string=)
    (dolist (op *untracked-lock-operators*)
      (when (gethash op operators)
        (funcall report "~S bypasses the lock-order checker; use clempo.lock" op)))))

(defun mutex-names (form)
  "The :NAME of every SB-THREAD:MAKE-MUTEX in FORM; NIL for one without a literal name.
A mutex's name is its lock class, so each needs one of its own."
  (let ((names '()))
    (labels ((walk (x)
               (typecase x
                 (cons (when (eq (car x) 'sb-thread:make-mutex)
                         (let* ((args (cdr x))
                                (name (and (ignore-errors (evenp (list-length args))) (getf args :name))))
                           (push (and (stringp name) name) names)))
                       (loop for tail = x then (cdr tail) while (consp tail) do (walk (car tail))))
                 (sb-impl::comma (walk (sb-impl::comma-expr x))))))
      (walk form))
    names))

;;; Shared state (clempo.guard), in the layers that run threads. A structure or class that
;;; holds a mutex, or whose slot some function mutates under a lock, is shared: each of its
;;; slots declares its guard, which its accessors check in check builds. A global mutated
;;; at run time is shared too: DEFSHARED declares its guard, and every mutation must sit in
;;; WITH-LOCK, WITH-RECURSIVE-LOCK or WITH-HELD of the declared lock.

(defparameter *shared-state-dirs* '("rpc" "store" "sync" "node" "validator" "p2p"))

(defun op-named-p (x package &rest names)
  (and (symbolp x) (symbol-package x)
       (string= (package-name (symbol-package x)) package)
       (member (symbol-name x) names :test #'string=)
       t))

(defun makes-mutex-p (form)
  (labels ((walk (x)
             (typecase x
               (cons (or (eq (car x) 'sb-thread:make-mutex)
                         (loop for tail = x then (cdr tail) while (consp tail) thereis (walk (car tail)))))
               (sb-impl::comma (walk (sb-impl::comma-expr x))))))
    (walk form)))

(defun struct-parts (form)
  "(values name conc-name slots) of a DEFSTRUCT or DEFINE-SHARED-STRUCT form; each slot is
(name initform . options)."
  (let* ((head (second form))
         (name (if (consp head) (first head) head))
         (conc-option (and (consp head)
                           (find :conc-name (rest head) :key (lambda (o) (and (consp o) (car o))))))
         (conc (cond ((null conc-option) (format nil "~A-" (symbol-name name)))
                     ((null (second conc-option)) "")
                     (t (string (second conc-option)))))
         (slots (loop for slot in (cddr form)
                      unless (stringp slot) collect (if (consp slot) slot (list slot nil)))))
    (values name conc slots)))

(defun struct-accessor (name conc slot &optional (prefix ""))
  (intern (format nil "~A~A~A" prefix conc (symbol-name slot)) (symbol-package name)))

(defun class-slot-functions (slot-spec)
  "The reader and accessor names of a DEFCLASS slot spec: the operators of its places."
  (when (consp slot-spec)
    (loop for (key value) on (rest slot-spec) by #'cddr
          when (member key '(:reader :accessor)) collect value)))

(defun walk-mutations (form fn)
  "Call FN with (kind place locks bound) for each mutation in FORM: KIND :SET or :ATOMIC,
PLACE the mutated place ((:CONTENTS table) for REMHASH and CLRHASH), LOCKS the lock forms of
the lexically enclosing WITH-LOCK, WITH-RECURSIVE-LOCK and WITH-HELD forms, BOUND the
variables LET-bound around it."
  (labels ((lock-op-p (op)
             (or (op-named-p op "CLEMPO.LOCK" "WITH-LOCK" "WITH-RECURSIVE-LOCK")
                 (op-named-p op "CLEMPO.GUARD" "WITH-HELD")))
           (walk-list (list locks bound)
             (loop for tail = list then (cdr tail) while (consp tail) do (walk (car tail) locks bound)))
           (place (p kind locks bound) (funcall fn kind p locks bound))
           (note (op args locks bound)
             (cond ((member op '(setf setq psetf psetq))
                    (loop for (p) on args by #'cddr do (place p :set locks bound)))
                   ((member op '(incf decf pop remf))
                    (when (consp args) (place (first args) :set locks bound)))
                   ((member op '(push pushnew))
                    (when (consp (cdr args)) (place (second args) :set locks bound)))
                   ((member op '(rotatef shiftf))
                    (dolist (p (if (eq op 'shiftf) (butlast args) args)) (place p :set locks bound)))
                   ((eq op 'remhash)
                    (when (consp (cdr args)) (place (list :contents (second args)) :set locks bound)))
                   ((eq op 'clrhash)
                    (when (consp args) (place (list :contents (first args)) :set locks bound)))
                   ((op-named-p op "SB-EXT" "ATOMIC-PUSH")
                    (when (consp (cdr args)) (place (second args) :atomic locks bound)))
                   ((op-named-p op "SB-EXT" "COMPARE-AND-SWAP" "CAS" "ATOMIC-INCF" "ATOMIC-DECF"
                                "ATOMIC-POP" "ATOMIC-UPDATE")
                    (when (consp args) (place (first args) :atomic locks bound)))))
           (walk (x locks bound)
             (typecase x
               (sb-impl::comma (walk (sb-impl::comma-expr x) locks bound))
               (cons
                (let ((op (car x)) (args (cdr x)))
                  (cond ((member op '(quote source-read-eval)))
                        ((and (lock-op-p op) (consp args) (consp (first args)))
                         (walk (first (first args)) locks bound)
                         (walk-list (rest args) (cons (first (first args)) locks) bound))
                        ((and (member op '(let let*)) (consp args) (listp (first args)))
                         (dolist (b (first args)) (when (consp b) (walk-list (rest b) locks bound)))
                         (walk-list (rest args) locks
                                    (append (loop for b in (first args) collect (if (consp b) (car b) b))
                                            bound)))
                        (t (when (listp args) (note op args locks bound))
                           (walk-list x locks bound))))))))
    (walk form '() '())))

(defun shared-state-violations (units report)
  "Check the shared-state rules over UNITS, (dir . ((file . forms) ...)) pairs. REPORT takes
the path, the definition and a message."
  (let ((plain-accessors (make-hash-table :test 'eq))  ; accessor -> (type slot), undeclared
        (raw-accessors (make-hash-table :test 'eq))    ; %% accessor -> type
        (class-slots (make-hash-table :test 'eq))      ; class -> ((slot . functions) ...)
        (guarded-class-slots (make-hash-table :test 'equal))
        (guarded-slot-names (make-hash-table :test 'eq))
        (globals (make-hash-table :test 'eq))          ; variable -> :plain or its guard
        (scope '()))
    (loop for (dir . files) in units
          when (member dir *shared-state-dirs* :test #'string=)
            do (dolist (unit files) (push unit scope)))
    ;; Declarations, from every file in scope.
    (loop for (nil . forms) in scope
          do (dolist (form forms)
               (when (consp form)
                 (let ((op (car form)))
                   (cond ((eq op 'defstruct)
                          (multiple-value-bind (name conc slots) (struct-parts form)
                            (dolist (slot slots)
                              (setf (gethash (struct-accessor name conc (first slot)) plain-accessors)
                                    (list name (first slot))))))
                         ((op-named-p op "CLEMPO.GUARD" "DEFINE-SHARED-STRUCT")
                          (multiple-value-bind (name conc slots) (struct-parts form)
                            (dolist (slot slots)
                              (setf (gethash (struct-accessor name conc (first slot) "%%") raw-accessors) name))))
                         ((eq op 'defclass)
                          (setf (gethash (second form) class-slots)
                                (loop for spec in (fourth form)
                                      collect (cons (if (consp spec) (first spec) spec)
                                                    (class-slot-functions spec)))))
                         ((op-named-p op "CLEMPO.GUARD" "DEFINE-SLOT-GUARDS")
                          (loop for (slot) in (cddr form)
                                do (setf (gethash (list (second form) slot) guarded-class-slots) t
                                         (gethash slot guarded-slot-names) t)))
                         ((member op '(defvar defparameter))
                          (setf (gethash (second form) globals) :plain))
                         ((op-named-p op "CLEMPO.GUARD" "DEFSHARED")
                          (setf (gethash (second form) globals) (getf (cddddr form) :guard))))))))
    ;; Class accessors of slots without a declared guard are plain.
    (maphash (lambda (class slots)
               (loop for (slot . functions) in slots
                     unless (gethash (list class slot) guarded-class-slots)
                       do (dolist (f functions) (setf (gethash f plain-accessors) (list class slot)))))
             class-slots)
    ;; Uses.
    (loop for (file . forms) in scope
          for path = (enough-namestring file *root*)
          do (dolist (form forms)
               (let ((definition (layer-definition form)))
                 (flet ((report (control &rest args)
                          (funcall report path definition (apply #'format nil control args))))
                   (when (consp form)
                     (case (car form)
                       (defstruct
                        (when (makes-mutex-p form)
                          (report "structure ~S holds a lock: declare its slots' guards with DEFINE-SHARED-STRUCT"
                                  (struct-parts form))))
                       (defclass
                        (when (makes-mutex-p form)
                          (loop for (slot) in (gethash (second form) class-slots)
                                unless (gethash (list (second form) slot) guarded-class-slots)
                                  do (report "slot ~S of class ~S, which holds a lock, has no guard (DEFINE-SLOT-GUARDS)"
                                             slot (second form)))))))
                   (multiple-value-bind (symbols) (audit-form form)
                     (maphash (lambda (symbol ignored)
                                (declare (ignore ignored))
                                (when (gethash symbol raw-accessors)
                                  (report "~S is the unguarded slot access of shared structure ~S"
                                          symbol (gethash symbol raw-accessors))))
                              symbols))
                   (unless (and (consp form) (member (car form) '(setf setq defvar defparameter)))
                     (walk-mutations
                      form
                      (lambda (kind place locks bound)
                        (let ((root (cond ((symbolp place) place)
                                          ((and (consp place) (eq (first place) :contents) (symbolp (second place)))
                                           (second place))))
                              (accessor (and (consp place) (symbolp (first place)) (first place))))
                          (when (and accessor (not (eq accessor :contents)))
                            (let ((plain (gethash accessor plain-accessors)))
                              (when (and plain locks)
                                (report "~S of ~S is mutated under a lock: declare its guards (DEFINE-SHARED-STRUCT or DEFINE-SLOT-GUARDS)"
                                        (second plain) (first plain)))
                              (when (and (eq accessor 'slot-value) (consp (cddr place))
                                         (consp (third place)) (eq (first (third place)) 'quote)
                                         (gethash (second (third place)) guarded-slot-names))
                                (report "(SETF SLOT-VALUE) of guarded slot ~S goes around its guard"
                                        (second (third place))))
                              ;; a container held in a global: (gethash k *table*), (aref *v* i),
                              ;; (nth i *list*): the container argument, not a key that names one
                              (unless (or plain (eq accessor 'slot-value))
                                (let ((container (if (member accessor '(gethash nth nthcdr))
                                                     (third place)
                                                     (second place))))
                                  (setf root (and (symbolp container) (gethash container globals) container))))))
                          (when (and root (not (member root bound)))
                            (let ((guard (gethash root globals)))
                              (cond ((null guard))
                                    ((eq guard :plain)
                                     (report "global ~S is mutated at run time: declare its guard with DEFSHARED" root))
                                    ((eq guard :immutable)
                                     (report "immutable global ~S is mutated" root))
                                    ((eq guard :atomic)
                                     (unless (eq kind :atomic)
                                       (report "atomic global ~S is mutated without COMPARE-AND-SWAP" root)))
                                    ((eq (first guard) :lock)
                                     (unless (member (second guard) locks :test #'equal)
                                       (report "global ~S is mutated outside its lock ~S" root (second guard)))))))))))))))))

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
         (bounds (make-hash-table :test 'eq))
         (mutex-sites (make-hash-table :test 'equal))
         (violations '()))
    (dolist (layer layers)
      (let ((dir (getf layer :dir)))
        (dolist (file (source-files dir))
          (let ((forms (read-source file)))
            (push (cons file forms) (gethash dir units))
            (dolist (name (bound-names forms)) (setf (gethash name bounds) t))
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
                           (lock-violations path operators #'report)
                           (dolist (name (mutex-names form))
                             (if name
                                 (push (format nil "~A (~A)" path definition) (gethash name mutex-sites))
                                 (report "SB-THREAD:MAKE-MUTEX needs a literal :NAME, its lock class")))
                           (when (hot-form-p form)
                             (dolist (count (uncited-hot-loops form bounds))
                               (report "fixed-count hot loop ~{~S~^ to ~} must take its count from a DEFBOUND limit"
                                       count)))
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
      (check-thread-owners policy starts joins #'report)
      (maphash (lambda (name sites)
                 (when (rest sites)
                   (report "mutex name ~S names ~D lock classes: ~{~A~^, ~}" name (length sites) (reverse sites))))
               mutex-sites))
    (shared-state-violations
     (loop for layer in layers collect (cons (getf layer :dir) (gethash (getf layer :dir) units)))
     (lambda (path definition message)
       (pushnew (format nil "~A (~A): ~A" path definition message) violations :test #'string=)))
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
