(defpackage #:clempo.cw-limit.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:limit #:clempo.cw-limit)))
(in-package #:clempo.cw-limit.tests)

(defconstant +second+ 1000000000)

(deftest gcra-quota-shapes ()
  (multiple-value-bind (interval burst) (limit:quota-per-second 8)
    (is= (/ +second+ 8) interval) (is= 8 burst))
  (multiple-value-bind (interval burst) (limit:quota-per-second 128)
    (is= 7812500 interval) (is= 128 burst))
  (multiple-value-bind (interval burst) (limit:quota-with-period 5)
    (is= (* 5 +second+) interval) (is= 1 burst))
  (multiple-value-bind (interval burst) (limit:quota-with-period 0.062)
    (is= 62000000 interval) (is= 1 burst))
  (signals error (limit:quota-per-second 0))
  (signals error (limit:quota-with-period 0)))

(deftest gcra-burst-then-refill ()
  (let ((limiter (multiple-value-call #'limit:make-gcra (limit:quota-per-second 8)))
        (now (* 1000 +second+)))
    ;; A fresh limiter admits exactly one burst at one instant.
    (is (loop repeat 8 always (limit:gcra-check limiter now)))
    (multiple-value-bind (ok earliest) (limit:gcra-check limiter now)
      (is (not ok))
      (is= (+ now (/ +second+ 8)) earliest))
    ;; A refused check consumes nothing: the cell is available exactly at EARLIEST.
    (is (not (limit:gcra-check limiter (+ now (/ +second+ 8) -1))))
    (is (limit:gcra-check limiter (+ now (/ +second+ 8))))
    (is (not (limit:gcra-check limiter (+ now (/ +second+ 8)))))
    ;; One cell per interval afterwards.
    (is (limit:gcra-check limiter (+ now (/ +second+ 4))))
    (is (not (limit:gcra-check limiter (+ now (/ +second+ 4)))))
    ;; Idle time refills at most one burst.
    (let ((later (+ now (* 60 +second+))))
      (is= 8 (loop repeat 20 count (limit:gcra-check limiter later))))))

(deftest gcra-period-and-keys ()
  (let ((keyed (multiple-value-call #'limit:make-keyed-gcra (limit:quota-with-period 5)))
        (now (* 7 +second+)))
    (is (limit:keyed-gcra-check keyed (octets-of 127 0 0 1) now))
    (multiple-value-bind (ok earliest) (limit:keyed-gcra-check keyed (octets-of 127 0 0 1) now)
      (is (not ok)) (is= (+ now (* 5 +second+)) earliest))
    ;; Keys are independent and compared by content.
    (is (limit:keyed-gcra-check keyed (octets-of 127 0 0 2) now))
    (is= 2 (limit:keyed-gcra-count keyed))
    (is= 2 (limit:keyed-gcra-retain-recent keyed (+ now +second+)))
    (is= 0 (limit:keyed-gcra-retain-recent keyed (+ now (* 5 +second+))))
    (is (limit:keyed-gcra-check keyed (octets-of 127 0 0 1) (+ now (* 5 +second+))))
    (limit:keyed-gcra-forget keyed (octets-of 127 0 0 1))
    (is (limit:keyed-gcra-check keyed (octets-of 127 0 0 1) (+ now (* 5 +second+))))))
