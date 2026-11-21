(defpackage #:clempo.precompiles.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles
        #:clempo.oracle #:clempo.oracle.tests)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))

(in-package #:clempo.precompiles.tests)

(deftest oracle-corpus-ratchet ()
  ;; Every case of the R3p corpus must reproduce the reference, except those listed in
  ;; data/oracle_known.txt. Only regressions fail: a listed case that now passes is
  ;; reported so the precompiles owner can prune the list, without breaking anyone's gate.
  (multiple-value-bind (cases expected) (oracle-corpus)
    (is= 4992 (length cases))
    (multiple-value-bind (passed total failures) (run-oracle-lines cases expected)
      (let* ((known (known-failures))
             (unexpected (remove-if (lambda (f) (member (first f) known :test #'string=)) failures))
             (fixed (set-difference known (mapcar #'first failures) :test #'string=)))
        (format t "~&oracle: ~D/~D identical~%" passed total)
        (when fixed
          (format t "~&oracle: ~D listed case~:P now pass; prune oracle_known.txt: ~{~A~^ ~}~%"
                  (length fixed) (subseq fixed 0 (min 20 (length fixed)))))
        (is (null unexpected) "~D unexpected differences:~%~{  ~{~A [~A]: ~A~}~%~}"
            (length unexpected) (subseq unexpected 0 (min 20 (length unexpected))))))))

(deftest precompile-set-by-fork ()
  (with-spec (:t2)
    (is (precompile-address-p +nonce-address+))
    (is (precompile-address-p (tip20-address 5)))
    (is (not (precompile-address-p #x1234)))))

(deftest t8-fork-sites ()
  ;; (fork-sites :t8) answers "what changed in T8"
  (is (listp (fork-sites :t8))))
