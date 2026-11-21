(defpackage #:clempo.io.tests
  (:use #:cl #:clempo.base #:clempo.io))

(in-package #:clempo.io.tests)

(deftest manual-clock-drives-now ()
  (multiple-value-bind (clock handle) (make-manual-clock 5000)
    (let ((*clock* clock))
      (is= 5 (now-seconds))
      (manual-clock-advance handle 2500)
      (is= 7500 (now-ms)))))

(deftest seeded-entropy-is-reproducible ()
  (let ((a (let ((*entropy* (make-seeded-entropy 42))) (random-octets 16)))
        (b (let ((*entropy* (make-seeded-entropy 42))) (random-octets 16))))
    (is-equalp a b)
    (is (< (let ((*entropy* (make-seeded-entropy 1))) (random-below 10)) 10))))

(deftest run-capture-pipes-stdin ()
  (multiple-value-bind (out code) (run-capture "cat" '() :input (string->octets "hello"))
    (is= 0 code)
    (is= "hello" (octets->string out))))
