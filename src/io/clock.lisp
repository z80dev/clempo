(in-package #:clempo.io)

(export '(monotonic-ticks process-cpu-ticks))

(declaim (inline monotonic-ticks process-cpu-ticks))

(defun monotonic-ticks ()
  "Elapsed time from the runtime's monotonic clock, in internal-time-units-per-second units."
  (get-internal-real-time))

(defun process-cpu-ticks ()
  "CPU time used by this process, in internal-time-units-per-second units."
  (get-internal-run-time))
