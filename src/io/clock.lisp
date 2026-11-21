(in-package #:clempo.io)

(export '(monotonic-ticks monotonic-nanoseconds process-cpu-ticks))

(declaim (inline monotonic-ticks monotonic-nanoseconds process-cpu-ticks))

(defun monotonic-ticks ()
  "Elapsed time from the runtime's monotonic clock, in internal-time-units-per-second units."
  (get-internal-real-time))

(defun monotonic-nanoseconds ()
  "The monotonic clock in nanoseconds, for timing short operations. Darwin's
CLOCK_MONOTONIC counts whole microseconds, so Darwin reads CLOCK_MONOTONIC_RAW (4), the
hardware counter (41.67 ns ticks on Apple Silicon)."
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime #+darwin 4 #-darwin sb-unix:clock-monotonic)
    (+ (* seconds 1000000000) nanoseconds)))

(defun process-cpu-ticks ()
  "CPU time used by this process, in internal-time-units-per-second units."
  (get-internal-run-time))
