;;; Owner: the precompiles agent. Tempo precompiles.
;;; packages.lisp and common.lisp load first; every other .lisp file here is a component, in
;;; alphabetical order. Files may call each other's functions freely (the precompiles call
;;; each other in cycles) but must not use another file's macros or DEFSTORABLE types at
;;; compile time.

(defsystem "clempo-tempo-precompiles"
  :description "Owner: the precompiles agent. Tempo precompiles."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-tempo-forks" "clempo-tempo-contract")
  :serial t
  :components #.(let* ((here (make-pathname :name nil :type nil :defaults *load-truename*))
                       (first '("packages" "common"))
                       (names (sort (mapcar #'pathname-name (directory (merge-pathnames "*.lisp" here)))
                                    #'string<)))
                  (mapcar (lambda (n) (list :file n))
                          (append first (remove-if (lambda (n) (member n first :test #'string=)) names)))))

(defsystem "clempo-tempo-precompiles/tests"
  :depends-on ("clempo-tempo-precompiles" "clempo-tempo-contract/tests" "clempo-io")
  :pathname "../../../tests/tempo/precompiles/"
  :serial t
  :components ((:file "oracle")
               (:file "precompiles-tests")))
