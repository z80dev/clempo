(defpackage #:clempo.oracle.tests
  (:use #:cl #:clempo.base #:clempo.oracle)
  (:export #:oracle-corpus #:known-failures #:corpus-file))

(in-package #:clempo.oracle.tests)

(defun corpus-file (name)
  (asdf:system-relative-pathname "clempo-tempo-precompiles/tests"
                                 (concatenate 'string "../../../tests/tempo/precompiles/data/" name)))

(defun tar-member (member)
  (multiple-value-bind (out code)
      (clempo.io:run-capture "tar" (list "-xzOf" (namestring (corpus-file "oracle.tar.gz")) member))
    (invariant (zerop code) "tar ~A failed" member)
    (remove "" (uiop:split-string (octets->string out) :separator '(#\Newline)) :test #'string=)))

(defparameter *sets*
  '("cases" "forks/account_keychain." "forks/evm." "forks/fee_manager." "forks/framework."
    "forks/review." "forks/smoke." "forks/stablecoin_dex." "forks/system." "forks/tip20."
    "forks/tip403." "forks/tx." "forks/validator_config.")
  "Corpus members: X.jsonl / Xexpected.jsonl pairs (\"cases\" pairs with \"expected\").")

(defun oracle-corpus ()
  "(values case-lines expected-lines) of the whole corpus, in member order."
  (let ((cases '()) (expected '()))
    (dolist (set *sets*)
      (multiple-value-bind (c e)
          (if (string= set "cases")
              (values "cases.jsonl" "expected.jsonl")
              (values (concatenate 'string set "cases.jsonl") (concatenate 'string set "expected.jsonl")))
        (setf cases (append cases (tar-member c))
              expected (append expected (tar-member e)))))
    (values cases expected)))

(defun known-failures ()
  "Case names listed in data/oracle_known.txt (# comments)."
  (let ((path (corpus-file "oracle_known.txt")))
    (when (probe-file path)
      (loop for line in (uiop:split-string (clempo.io:read-file-string path) :separator '(#\Newline))
            for l = (string-trim " " line)
            unless (or (string= l "") (char= (char l 0) #\#)) collect l))))
