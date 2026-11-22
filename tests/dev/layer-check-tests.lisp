(in-package #:clempo-build)

(defun audit-fixture (source &optional io-p)
  (with-input-from-string (in source)
    (let ((violations nil) (hashes 0) (loops 0))
      (dolist (form (read-source-stream in "layer-check fixture"))
        (multiple-value-bind (symbols operators hash-scans unbounded-loops) (audit-form form)
          (incf hashes hash-scans)
          (incf loops unbounded-loops)
          (native-violations "codec" "src/codec/test-fixture.lisp" io-p symbols operators
                             (form-packages form symbols)
                             (lambda (control &rest args) (push (apply #'format nil control args) violations)))))
      (values violations hashes loops))))

;; Strings and Lisp comments may contain intact prohibited forms.
(assert (null (audit-fixture "\"(open \" ; (probe-file x)
#| (sb-ext:get-time-of-day) #| (delete-file x) |# |#
(let ((open 1)) open)")))
(assert (null (audit-fixture "(defun client-hello (random) random)" t)))

(dolist (fixture '(("(funcall #'cl:probe-file p)" . "PROBE-FILE")
                   ("\"semicolon ;\" (cl::delete-file p)" . "DELETE-FILE")
                   ("(funcall 'cl:rename-file old new)" . "RENAME-FILE")
                   ("(open p)" . "OPEN")
                   ("(random 2)" . "RANDOM")
                   ("#.(progn (error \"must not run\") (sb-ext:get-time-of-day))" . "GET-TIME-OF-DAY")
                   ("#+unused-check-fixture (sb-ext:get-time-of-day)" . "GET-TIME-OF-DAY")
                   ("(sb-alien:alien-funcall callable)" . "SB-ALIEN")
                   ("(sb-unix:unix-getpid)" . "SB-UNIX")))
  (assert (some (lambda (v) (search (cdr fixture) v)) (audit-fixture (car fixture)))))

(assert (audit-fixture "#2A((sb-thread:make-thread))"))

(let ((starts (make-hash-table :test #'equal)) (joins (make-hash-table :test #'equal))
      (violations nil)
      (policy '(:threads (("src/io/fixture.lisp" "START" 1 "STOP" "STOP joins START's worker.")))))
  (setf (gethash '("src/io/fixture.lisp" "START") starts) 1)
  (check-thread-owners policy starts joins
                       (lambda (control &rest args) (push (apply #'format nil control args) violations)))
  (assert (and (= 1 (length violations)) (search "join owner STOP has no JOIN-THREAD" (first violations)))))

(let* ((name (symbol-name (gensym "LAYER-CHECK-FIXTURE")))
       (package (make-package name :use '(#:cl))))
  (unwind-protect
       (progn
         (sb-ext:add-package-local-nickname "WORKERS" "SB-THREAD" package)
         (assert (some (lambda (v) (search "SB-THREAD" v))
                       (audit-fixture (format nil "(in-package ~S) (workers:make-thread #'identity)" name)))))
    (delete-package package)))

(assert (null (audit-fixture "(open p)" t)))
(assert (audit-fixture "(sb-ext:get-time-of-day)" t)) ; clocks are narrower than I/O
(multiple-value-bind (violations hashes loops)
    (audit-fixture "(loop for item in items do (use item))
                    (do ((i 0 (1+ i))) ((>= i n)) (use i))
                    (loop do (work))
                    (do () (nil) (work))
                    (loop for k being each hash-key of table collect k)
                    (maphash #'use table)")
  (assert (null violations))
  (assert (= 2 hashes))
  (assert (= 2 loops)))

;; Fixed-count loops in hot code cite a DEFBOUND limit; loops over data, and cold code, need not.
(flet ((uncited (source &rest bound-names)
         (let ((bounds (make-hash-table :test 'eq)))
           (dolist (name bound-names) (setf (gethash name bounds) t))
           (with-input-from-string (in source)
             (loop for form in (read-source-stream in "layer-check fixture")
                   when (hot-form-p form) append (uncited-hot-loops form bounds))))))
  (assert (equal '((16) (3 0) (8) (0 4) (0 2))
                 (uncited "(defun f (v n)
                             (declare #.*hot*)
                             (dotimes (i 16) (use i))
                             (loop for i from 3 downto 0 do (use i))
                             (loop repeat 8 do (use))
                             (do ((i 0 (1+ i))) ((>= i 4)) (use i))
                             (loop for i below 2 do (use i))
                             (dotimes (i n) (use i))
                             (loop for x across v for i below (length v) do (use x i)))")))
  (assert (null (uncited "(defun f () (dotimes (i 16) (use i)))")))
  (assert (null (uncited "(defun f () (declare (optimize (speed 3))) (dotimes (i (ash most-positive-fixnum -60)) (use i)))"
                         'most-positive-fixnum)))
  (assert (equal '((most-positive-fixnum))
                 (uncited "(defun f () (declare (optimize (speed 3))) (dotimes (i most-positive-fixnum) (use i)))"))))

;; Locks go through clempo.lock, and each mutex names its own lock class.
(let ((violations '()))
  (lock-violations "src/codec/fixture.lisp"
                   (nth-value 1 (audit-form '(sb-thread:with-mutex (m) (sb-thread:condition-wait q m))))
                   (lambda (control &rest args) (push (apply #'format nil control args) violations)))
  (assert (= 2 (length violations))))
(assert (equal '(nil "pool")
               (mutex-names '(list (sb-thread:make-mutex :name "pool") (sb-thread:make-mutex)))))

;; Shared state: a structure holding a lock, a plain structure mutated under one, and run-time
;; mutations of globals (a let-rebound one excepted) declare guards and respect them.
(let ((violations '()))
  (with-input-from-string (in "(defstruct box (lock (sb-thread:make-mutex :name \"box\")) (n 0))
                               (defstruct plain (n 0))
                               (defvar *count* 0)
                               (clempo.guard:defshared *table* (make-hash-table) \"t\" :guard (:lock *table-lock*))
                               (defun bump (b p)
                                 (clempo.lock:with-lock ((box-lock b)) (incf (plain-n p)))
                                 (incf *count*)
                                 (setf (gethash 1 *table*) 2)
                                 (clempo.lock:with-lock (*table-lock*) (remhash 1 *table*))
                                 (let ((*count* 0)) (incf *count*)))")
    (shared-state-violations
     (list (cons "rpc" (list (cons (merge-pathnames "src/rpc/fixture.lisp" *root*)
                                   (read-source-stream in "shared-state fixture")))))
     (lambda (path definition message)
       (declare (ignore path definition))
       (pushnew message violations :test #'string=))))
  (assert (= 4 (length violations)))
  (dolist (expected '("holds a lock" "is mutated under a lock" "COUNT* is mutated at run time"
                      "TABLE* is mutated outside its lock"))
    (assert (find expected violations :test #'search))))
