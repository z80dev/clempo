;;; Every .lisp file in this directory is a component, main.lisp and score.lisp first, the rest in
;;; alphabetical order, so command files can be added without editing this file.

(defsystem "clempo-dev"
  :description "clempo-dev measurement and development commands"
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-types" "clempo-trie"
               "clempo-state" "clempo-evm" "clempo-tempo" "clempo-sync")
  :serial t
  :components #.(let* ((here (make-pathname :name nil :type nil :defaults *load-truename*))
                       (names (sort (mapcar #'pathname-name (directory (merge-pathnames "*.lisp" here)))
                                    #'string<)))
                  (mapcar (lambda (n) (list :file n))
                          (list* "main" "score" (remove-if (lambda (n) (member n (quote ("main" "score")) :test (function string=))) names)))))
