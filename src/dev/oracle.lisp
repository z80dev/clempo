(in-package #:clempo.dev)

;;; R3p: zempo's reference-oracle corpus of Tempo precompile calls
;;; (tests/tempo/precompiles/data/oracle.tar.gz; see the README there).

(defun oracle-data (name)
  (merge-pathnames (concatenate 'string "tests/tempo/precompiles/data/" name)
                   (asdf:system-relative-pathname "clempo-dev" "../../")))

(defun oracle-member (member)
  (multiple-value-bind (out code)
      (clempo.io:run-capture "tar" (list "-xzOf" (namestring (oracle-data "oracle.tar.gz")) member))
    (invariant (zerop code) "tar ~A failed" member)
    (remove "" (uiop:split-string (octets->string out) :separator '(#\Newline)) :test #'string=)))

(defun oracle-corpus ()
  (let ((cases '()) (expected '()))
    (dolist (set '("" "forks/account_keychain." "forks/evm." "forks/fee_manager." "forks/framework."
                   "forks/review." "forks/smoke." "forks/stablecoin_dex." "forks/system."
                   "forks/tip20." "forks/tip403." "forks/tx." "forks/validator_config."))
      (setf cases (append cases (oracle-member (concatenate 'string set "cases.jsonl")))
            expected (append expected (oracle-member (if (string= set "")
                                                          "expected.jsonl"
                                                          (concatenate 'string set "expected.jsonl"))))))
    (values cases expected)))

(defun run-oracle (&key filter target spec)
  "Run the corpus; FILTER is a name substring, TARGET a prefix of the called code address
\(e.g. \"0xdec0\"), SPEC a fork name."
  (multiple-value-bind (cases expected) (oracle-corpus)
    (clempo.oracle:run-oracle-lines
     cases expected
     :filter (lambda (name case)
               (and (or (null filter) (search filter name))
                    (or (null target)
                        (eql 0 (search (string-downcase target)
                                       (string-downcase (clempo.json:jget* case "call" "code_address")))))
                    (or (null spec) (string-equal spec (clempo.json:jget case "spec"))))))))

(defcommand "oracle" (args)
  "R3p oracle: oracle [-filter name-substring] [-target 0xdec0] [-spec T11] [-max 40] [-group] [-names]"
  (multiple-value-bind (passed total failures)
      (run-oracle :filter (arg args :filter) :target (arg args :target) :spec (arg args :spec))
    (cond
      ((arg-flag args :names)
       (dolist (f failures) (write-line (first f))))
      ((arg-flag args :group)
       (let ((groups (make-hash-table :test 'equal)) (rows '()))
         (dolist (f failures)
           (let ((g (subseq (first f) 0 (or (position #\/ (first f) :from-end t) (length (first f))))))
             (incf (gethash g groups 0))))
         (maphash (lambda (k v) (push (cons k v) rows)) groups)
         (dolist (r (sort rows #'> :key #'cdr))
           (format t "~6D ~A~%" (cdr r) (car r)))))
      (t
       (loop for (name spec diff) in failures
             repeat (arg-int args :max 40)
             do (format t "~A [~A]: ~A~%" name spec diff))))
    (format t "oracle: ~D/~D identical~%" passed total)
    0))

(defrung "R3p" (args)
  "zempo's reference-oracle corpus of Tempo precompile calls, identical/total"
  (multiple-value-bind (passed total) (run-oracle)
    (clempo.json:jobject "score" (format nil "~D/~D" passed total) "identical" passed "total" total)))
