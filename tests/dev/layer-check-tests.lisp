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
