(in-package #:clempo.dev)

(defun spec-source-p (file prefix)
  (and (stringp file) (<= (length prefix) (length file))
       (string= prefix file :end2 (length prefix))))

(defun spec-contract< (a b)
  (let ((pa (package-name (symbol-package a)))
        (pb (package-name (symbol-package b))))
    (if (string= pa pb)
        (string< (symbol-name a) (symbol-name b))
        (string< pa pb))))

(defun spec-file-stem (symbol)
  (let ((stem (string-downcase (symbol-name symbol))))
    ;; A declaration's name must never escape its owned output directory.
    (unless (and (plusp (length stem))
                 (every (lambda (c) (or (find c "abcdefghijklmnopqrstuvwxyz0123456789")
                                        (find c "-_")))
                        stem))
      (error "Unsafe specification filename for ~S" symbol))
    stem))

(defstruct (spec-address (:constructor spec-address (value)))
  "An address inside a printed fork-site form: prints as 0x and 40 hex digits."
  value)

(defmethod print-object ((a spec-address) stream)
  (format stream "0x~(~40,'0x~)" (spec-address-value a)))

(defun spec-hex-addresses (form)
  "FORM with every integer wider than 32 bits (the addresses fork sites name) as a 0x
address; small integers (form paths, versions) stay decimal."
  (cond ((and (integerp form) (< 32 (integer-length form) 161)) (spec-address form))
        ((consp form) (cons (spec-hex-addresses (car form)) (spec-hex-addresses (cdr form))))
        (t form)))

(defun spec-fork-site-markdown (site)
  (with-output-to-string (out)
    (format out "- Source: [~A](../../~A)~%- Top-level form: ~D (1-based)~%- Nested form path: `~S`~%- Definition: `~S`~%- Kind: `~S`~%- Relation: `~S`~%- Where: `~S`~%"
            (clempo.forks:fork-site-file site)
            (clempo.forks:fork-site-file site)
            (clempo.forks:fork-site-form-number site)
            (clempo.forks:fork-site-form-path site)
            (spec-hex-addresses (clempo.forks:fork-site-definition site))
            (clempo.forks:fork-site-kind site)
            (clempo.forks:fork-site-relation site)
            (spec-hex-addresses (clempo.forks:fork-site-where site)))
    (when (clempo.forks:fork-site-doc site)
      (format out "~%~A~%" (clempo.forks:fork-site-doc site)))))

(defun spec-fork-markdown (fork)
  (let ((sites (sort (loop for site in (clempo.forks:fork-sites fork)
                          when (spec-source-p (clempo.forks:fork-site-file site) "src/")
                            collect (spec-fork-site-markdown site))
                     #'string<)))
    (with-output-to-string (out)
      (format out "# ~A~%~%" (string-downcase (symbol-name fork)))
      (if sites
          (loop for site in sites for index from 1
                do (format out "## Site ~D~%~%~A~%" index site))
          (format out "No recorded source sites.~%")))))

(defun spec-outputs ()
  "Render compiled declarations before touching files; reject filename collisions."
  (let ((*package* (find-package :keyword))
        (*print-pretty* nil)
        (*print-case* :downcase)
        (*print-base* 10)
        (*print-radix* nil)
        (*print-length* nil)
        (*print-level* nil)
        (*print-circle* nil)
        (*print-escape* t)
        (names (make-hash-table :test 'equal))
        (outputs '()))
    (labels ((add (path content)
               (when (gethash path names)
                 (error "Duplicate generated specification filename: ~A" path))
               (setf (gethash path names) t)
               (push (cons path content) outputs)))
      (dolist (contract (sort (remove-if-not
                              (lambda (contract)
                                (spec-source-p (clempo.contract:contract-source-file contract)
                                               "src/tempo/precompiles/"))
                              (copy-list (clempo.contract:contracts)))
                             #'spec-contract<))
        (let ((stem (spec-file-stem contract)))
          (add (format nil "layouts/~A.md" stem)
               (clempo.contract:layout-markdown contract))
          (add (format nil "abi/~A.json" stem)
               (format nil "~A~%"
                       (clempo.json:to-json-string (clempo.contract:contract-abi contract))))))
      (dolist (fork clempo.forks:+forks+)
        (add (format nil "forks/~A.md" (spec-file-stem fork))
             (spec-fork-markdown fork))))
    (nreverse outputs)))

(defun generate-spec (&optional (root (asdf:system-relative-pathname "clempo-dev" "../../spec/")))
  "Write deterministic compiled-form documentation and remove stale owned outputs.
ROOT is a directory, including when its command-line spelling lacks a trailing slash.
Only immediate layouts/*.md, forks/*.md and abi/*.json outputs are reconciled."
  (let* ((root (uiop:ensure-directory-pathname root))
         (outputs (spec-outputs)))
    (dolist (entry outputs)
      (let ((path (merge-pathnames (car entry) root)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
          (write-string (cdr entry) out))))
    (dolist (owned '(("layouts/" . "md") ("forks/" . "md") ("abi/" . "json")))
      (let ((directory (merge-pathnames (car owned) root)))
        (dolist (path (directory (merge-pathnames
                                 (make-pathname :name :wild :type (cdr owned)) directory)))
          (let ((relative (concatenate 'string (car owned) (file-namestring path))))
            (unless (assoc relative outputs :test #'string=)
              (delete-file path))))))
    (length outputs)))

(defcommand "spec" (args)
  "generate compiled-form specification: spec [-out directory]"
  (let ((out (arg args :out)))
    (if out (generate-spec out) (generate-spec)))
  0)
