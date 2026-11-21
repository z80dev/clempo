(defpackage #:clempo.rpc.serde-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde)))
(in-package #:clempo.rpc.serde-tests)

;;; JSON numbers as serde_json reads and serde prints them (data/README.md).

(defun fixture-lines (name)
  (with-open-file (in (asdf:system-relative-pathname "clempo-rpc" (concatenate 'string "../../tests/rpc/data/" name)))
    (loop for line = (read-line in nil) while line collect (json:parse-json line))))

(deftest number-params-as-reference ()
  (let ((cases (fixture-lines "number-params.jsonl")))
    (is (> (length cases) 200))
    (dolist (c cases)
      (let ((request (json:jget c "request")))
        (is= (json:jget c "response")
             (handler-case (nth-value 1 (clempo.rpc:handle-body request))
               (error (e) (format nil "~A: ~A" (type-of e) e)))
             "~A" request)))))

(deftest serde-numbers ()
  (flet ((number (text) (serde:parse-serde-number text)))
    (is= 18446744073709551615 (number "18446744073709551615"))
    (is= -9223372036854775808 (number "-9223372036854775808"))
    (is (typep (number "18446744073709551616") 'double-float))
    (is (typep (number "-9223372036854775809") 'double-float))
    (is (minusp (float-sign (number "-0"))))
    ;; digits beyond a u64 are dropped, so this is one ulp from the nearest double
    (is= "1.2345678901234567e+20" (serde:rust-float (number "123456789012345678901")))
    (is= 0d0 (number "1e-400"))
    (signals serde:serde-error (number "1e400"))
    (signals serde:serde-error (number "-1.8e308")))
  (loop for (x text) in '((1.5d20 "1.5e+20") (1d16 "1e+16") (1d15 "1000000000000000.0")
                          (1d-6 "1e-6") (2.5d-5 "0.000025") (1d-5 "0.00001") (100d0 "100.0")
                          (-0d0 "-0.0") (0.1d0 "0.1") (123456789012345.6d0 "123456789012345.6")
                          (-9.223372036854776d18 "-9.223372036854776e+18"))
        do (is= text (serde:rust-float x) "~S" x)))
