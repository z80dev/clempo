;;; Every .lisp file in this directory is a component, main.lisp and score.lisp first, the
;;; rest in alphabetical order, so command files can be added without editing this file.
;;; The system depends on every layer listed in layers.sexp that has a system, so new
;;; layers are picked up without editing this file either.

(defsystem "clempo-dev"
  :description "clempo-dev measurement and development commands"
  :depends-on #.(let* ((here (make-pathname :name nil :type nil :defaults *load-truename*))
                       (root (merge-pathnames "../../" here))
                       (layers (with-open-file (in (merge-pathnames "layers.sexp" root))
                                 (let ((*read-eval* nil)) (read in)))))
                  (loop for l in layers
                        for dir = (getf l :dir)
                        for asd = (merge-pathnames (format nil "src/~A/clempo-~A.asd" dir dir) root)
                        unless (string= dir "dev")
                          when (probe-file asd)
                            collect (format nil "clempo-~A" dir)))
  :serial t
  :components #.(let* ((here (make-pathname :name nil :type nil :defaults *load-truename*))
                       (names (sort (mapcar #'pathname-name (directory (merge-pathnames "*.lisp" here)))
                                    #'string<)))
                  (mapcar (lambda (n) (list :file n))
                          (list* "main" "score"
                                 (remove-if (lambda (n) (member n '("main" "score") :test #'string=))
                                            names)))))
