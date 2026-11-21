;;; Build driver: registers src/ with ASDF, puts fasls under build/fasl/<mode>/,
;;; and loads or tests the layers listed in layers.sexp.
;;; Usage: sbcl --script-ish --load scripts/build.lisp --eval '(clempo-build:test-all)'

(require :asdf)

(defpackage #:clempo-build
  (:use #:cl)
  (:export #:*root* #:layers #:load-all #:test-all #:load-layer #:test-layer))

(in-package #:clempo-build)

(defparameter *root*
  (truename (merge-pathnames "../" (make-pathname :name nil :type nil
                                                  :defaults *load-truename*))))

(defparameter *mode*
  (let ((m (sb-ext:posix-getenv "CLEMPO_BUILD")))
    (if (and m (string-equal m "release")) "release" "check")))

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(merge-pathnames "src/" *root*)) :ignore-inherited-configuration))

(when (string= *mode* "release") (pushnew :clempo-release *features*))
(setf *compile-verbose* nil *compile-print* nil)

(asdf:initialize-output-translations
 `(:output-translations
   (,(merge-pathnames "**/*.*" *root*)
    ,(merge-pathnames (format nil "build/fasl/~A/**/*.*" *mode*) *root*))
   :inherit-configuration))

(defun layers ()
  (with-open-file (in (merge-pathnames "layers.sexp" *root*))
    (let ((*read-eval* nil)) (read in))))

(defun layer-system (layer) (format nil "clempo-~A" (getf layer :dir)))

(defun system-exists-p (name)
  (handler-case (asdf:find-system name nil) (error () nil)))

(defun present-layers ()
  (remove-if-not (lambda (l) (system-exists-p (layer-system l))) (layers)))

(defun load-layer (dir &key force)
  (asdf:load-system (format nil "clempo-~A" dir) :force force))

(defun call-strictly (strict thunk)
  "Run THUNK; with STRICT, warnings fail the build, and so do undefined functions and
variables, which SBCL reports only as style warnings at the end of a compilation unit."
  (let ((asdf:*compile-file-warnings-behaviour* (if strict :error :warn))
        (asdf:*compile-file-failure-behaviour* (if strict :error :warn))
        (undefined '()))
    (handler-bind ((style-warning
                     (lambda (w)
                       (let ((text (princ-to-string w)))
                         (when (search "undefined" text) (push text undefined))))))
      (with-compilation-unit () (funcall thunk)))
    (when (and strict undefined)
      (format *error-output* "~&build: undefined references:~%~{  ~A~%~}" (reverse undefined))
      (sb-ext:exit :code 1 :abort t))
    t))

(defun load-all (&key force strict)
  "Load every present layer. With STRICT, warnings and undefined references fail."
  (call-strictly strict
                 (lambda ()
                   (dolist (l (present-layers))
                     (asdf:load-system (layer-system l) :force (and force :all))))))

(defun test-systems (&optional dirs)
  (loop for l in (present-layers)
        for name = (format nil "~A/tests" (layer-system l))
        when (and (or (null dirs) (member (getf l :dir) dirs :test #'string=))
                  (system-exists-p name))
          collect name))

(defun test-all (&key dirs strict)
  "Load the test systems of DIRS (all when NIL) and run them. Exits non-zero on failure."
  (call-strictly strict (lambda () (dolist (s (test-systems dirs)) (asdf:load-system s))))
  (let ((ok (funcall (find-symbol "RUN-TESTS" "CLEMPO.BASE"))))
    (unless ok (sb-ext:exit :code 1 :abort t))
    ok))
