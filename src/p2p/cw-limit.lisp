(defpackage #:clempo.cw-limit
  (:use #:cl #:clempo.base)
  (:documentation "GCRA rate limiters with the semantics of commonware-runtime's Quota and
RateLimiter (the governor crate): a quota admits BURST cells at once and refills one
cell every INTERVAL. A refused check consumes nothing and reports the earliest time a
cell becomes available. Times are monotonic integer nanoseconds (MONOTONIC-NS).")
  (:export #:monotonic-ns #:quota-per-second #:quota-with-period
           #:gcra #:make-gcra #:gcra-check #:gcra-interval #:gcra-burst
           #:keyed-gcra #:make-keyed-gcra #:keyed-gcra-check #:keyed-gcra-retain-recent
           #:keyed-gcra-count #:keyed-gcra-forget))
(in-package #:clempo.cw-limit)

(defconstant +ns-per-second+ 1000000000)

(defun monotonic-ns ()
  "A monotonic clock in integer nanoseconds."
  (floor (* (clempo.io:monotonic-ticks) +ns-per-second+) internal-time-units-per-second))

(defun quota-per-second (rate &optional (burst rate))
  "Quota::per_second(RATE): one cell every 1/RATE s, BURST (default RATE) at once.
Returns the interval in nanoseconds and the burst."
  (check-type rate (integer 1 *))
  (check-type burst (integer 1 *))
  (values (ceiling +ns-per-second+ rate) burst))

(defun quota-with-period (seconds)
  "Quota::with_period(SECONDS): one cell per period, burst 1."
  (check-type seconds (real (0)))
  (values (max 1 (ceiling (* (rationalize seconds) +ns-per-second+))) 1))

(defstruct (gcra (:constructor %make-gcra (interval burst tolerance)))
  (interval 1 :type (integer 1 *) :read-only t)
  (burst 1 :type (integer 1 *) :read-only t)
  ;; (burst - 1) * interval: how far the theoretical arrival time may run ahead.
  (tolerance 0 :type (integer 0 *) :read-only t)
  ;; Theoretical arrival time of the next cell; 0 means a full bucket.
  (tat 0 :type integer))

(defun make-gcra (interval burst)
  "A limiter from an INTERVAL in nanoseconds and a BURST, as QUOTA-PER-SECOND or
QUOTA-WITH-PERIOD return them: (multiple-value-call #'make-gcra (quota-per-second 8))."
  (check-type interval (integer 1 *))
  (check-type burst (integer 1 *))
  (%make-gcra interval burst (* (1- burst) interval)))

(defun gcra-check (limiter &optional (now (monotonic-ns)))
  "Take one cell if available and return T. Otherwise consume nothing and return NIL
and the earliest NOW (nanoseconds) at which a cell is available. Not thread-safe:
callers sharing a limiter serialize access."
  (let ((tat (max (gcra-tat limiter) now)))
    (if (<= (- tat now) (gcra-tolerance limiter))
        (progn (setf (gcra-tat limiter) (+ tat (gcra-interval limiter))) t)
        (values nil (- tat (gcra-tolerance limiter))))))

(defstruct (keyed-gcra (:constructor %make-keyed-gcra (interval burst table)))
  (interval 1 :type (integer 1 *) :read-only t)
  (burst 1 :type (integer 1 *) :read-only t)
  (table nil :type hash-table :read-only t))

(defun make-keyed-gcra (interval burst &key (test 'equalp))
  "One independent GCRA per key (KeyedRateLimiter): an unseen key starts full."
  (check-type interval (integer 1 *))
  (check-type burst (integer 1 *))
  (%make-keyed-gcra interval burst (make-hash-table :test test)))

(defun keyed-gcra-check (keyed key &optional (now (monotonic-ns)))
  "GCRA-CHECK the limiter of KEY, creating it on first use."
  (let ((table (keyed-gcra-table keyed)))
    (gcra-check (or (gethash key table)
                    (setf (gethash key table)
                          (make-gcra (keyed-gcra-interval keyed) (keyed-gcra-burst keyed))))
                now)))

(defun keyed-gcra-retain-recent (keyed &optional (now (monotonic-ns)))
  "Forget keys whose bucket has refilled completely; they behave as unseen keys."
  (let ((table (keyed-gcra-table keyed)))
    (maphash (lambda (key limiter) (when (<= (gcra-tat limiter) now) (remhash key table))) table)
    (hash-table-count table)))

(defun keyed-gcra-forget (keyed key)
  (remhash key (keyed-gcra-table keyed)))

(defun keyed-gcra-count (keyed)
  (hash-table-count (keyed-gcra-table keyed)))
