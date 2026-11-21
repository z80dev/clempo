(defpackage #:clempo.json.tests
  (:use #:cl #:clempo.base #:clempo.json))

(in-package #:clempo.json.tests)

(deftest json-round-trip ()
  (let* ((text "{\"a\":[1,2,{\"b\":null}],\"c\":\"x\\ny\\u00e9\",\"d\":true,\"e\":-1.5,\"f\":{}}")
         (v (parse-json text)))
    (is= 2 (jget* v "a" 1))
    (is= :null (jget* v "a" 2 "b"))
    (is= (format nil "x~%y~C" (code-char #xe9)) (jget v "c"))
    (is= -1.5d0 (jget v "e"))
    (is (json-equal v (parse-json (to-json-string v))))
    (is= (format nil "{\"a\":[1,2,{\"b\":null}],\"c\":\"x\\ny~C\",\"d\":true,\"e\":-1.5,\"f\":{}}"
                 (code-char #xe9))
         (to-json-string v))))

(deftest json-equal-ignores-key-order ()
  (is (json-equal (parse-json "{\"a\":1,\"b\":[]}") (parse-json "{\"b\":[],\"a\":1}")))
  (is (not (json-equal (parse-json "[]") (parse-json "{}")))))

(deftest json-rejects-garbage ()
  (signals json-error (parse-json "{\"a\":1,}"))
  (signals json-error (parse-json "[1 2]"))
  (signals json-error (parse-json "\"open")))

(deftest json-control-escapes-are-lowercase ()
  ;; The reference (serde_json) writes \u001f, not \u001F.
  (is= "\"\\u001f\"" (to-json-string (string (code-char 31)))))
