;;; Layer check: every clempo package a file names must be defined in the file's own
;;; src/ directory or in one listed above it in layers.sexp, and only :io layers may
;;; name the clock, randomness, files, processes, sockets or threads.
;;; Loaded after scripts/build.lisp; exits non-zero on a violation.

(in-package #:clempo-build)

(defparameter *io-tokens*
  '("sb-bsd-sockets" "sb-posix" "sb-thread:" "run-program" "with-open-file" "(open "
    "get-universal-time" "get-internal-real-time" "(random " "make-random-state"
    "sb-ext:posix-getenv" "probe-file" "(directory "))

(defun read-text (path)
  (with-open-file (in path :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defun source-files (dir)
  (directory (merge-pathnames (format nil "src/~A/**/*.lisp" dir) *root*)))

(defun strip-comments (text)
  "Drop ; comments, docstring-ish text is kept (it is rare to name packages there)."
  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for line = (read-line in nil)
            while line
            do (write-line (subseq line 0 (or (position #\; line) (length line))) out)))))

(defun scan-names (text prefix)
  "Lowercase package names starting with PREFIX that appear in TEXT."
  (let ((names '()) (low (string-downcase text)) (start 0))
    (loop for pos = (search prefix low :start2 start)
          while pos
          do (let ((end (or (position-if-not (lambda (c) (or (alphanumericp c) (find c ".-")))
                                             low :start (1+ pos))
                            (length low))))
               (pushnew (string-right-trim "." (subseq low pos end)) names :test #'string=)
               (setf start end)))
    names))

(defun defined-packages (text)
  (let ((low (string-downcase text)) (names '()) (start 0))
    (loop for pos = (search "(defpackage" low :start2 start)
          while pos
          do (let* ((s (position-if (lambda (c) (alphanumericp c)) low :start (+ pos 11)))
                    (e (position-if-not (lambda (c) (or (alphanumericp c) (find c ".-"))) low :start s)))
               (push (subseq low s e) names)
               (setf start e)))
    names))

(defun check-layers ()
  (let* ((layers (layers))
         (owner (make-hash-table :test 'equal))
         (violations '()))
    (dolist (l layers)
      (dolist (f (source-files (getf l :dir)))
        (dolist (p (defined-packages (read-text f)))
          (setf (gethash p owner) (getf l :dir)))))
    (loop for l in layers
          for index from 0
          for allowed = (mapcar (lambda (x) (getf x :dir)) (subseq layers 0 (1+ index)))
          do (dolist (f (source-files (getf l :dir)))
               (let ((text (strip-comments (read-text f))))
                 (dolist (name (scan-names text "clempo."))
                   (let ((dir (gethash name owner)))
                     (cond ((null dir)
                            (push (format nil "~A: unknown package ~A" (enough-namestring f *root*) name)
                                  violations))
                           ((not (member dir allowed :test #'string=))
                            (push (format nil "~A: ~A layer may not use ~A (layer ~A)"
                                          (enough-namestring f *root*) (getf l :dir) name dir)
                                  violations)))))
                 (unless (or (getf l :io)
                             (member (file-namestring f) (getf l :io-exempt) :test #'string=))
                   (dolist (tok *io-tokens*)
                     (when (search tok (string-downcase text))
                       (push (format nil "~A: ~A is an io operation outside an :io layer"
                                     (enough-namestring f *root*) tok)
                             violations)))))))
    (dolist (v (reverse violations)) (format t "~&layers: ~A~%" v))
    (format t "~&layers: ~D violation~:P~%" (length violations))
    (null violations)))

(unless (check-layers) (sb-ext:exit :code 1 :abort t))
