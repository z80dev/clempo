(in-package #:clempo.dev)

(defcommand "pool-fixtures" (args)
  "validate recorded-state/signed pool cases and measure actual admission (-iterations N, -json)"
  ;; Keep fixture loading explicit: the node and normal pool never depend on test support.
  (asdf:load-system "clempo-tempo-txpool/fixtures")
  (let* ((iterations (arg-int args :iterations 1))
         (run (symbol-function (find-symbol "RUN-FIXTURE-VALIDATION" "CLEMPO.TXPOOL.FIXTURES"))))
    (unless (and (integerp iterations) (plusp iterations))
      (log-line "-iterations must be a positive integer")
      (return-from command 2))
    (let ((accepted 0) (rejected 0) (selected 0)
          (start (clempo.io:monotonic-ticks)) (bytes (sb-ext:get-bytes-consed)))
      (handler-case
          (dotimes (i iterations)
            (multiple-value-bind (a r s) (funcall run)
              (incf accepted a) (incf rejected r) (incf selected s)))
        (error (condition)
          (log-line "pool fixture validation failed: ~A" condition)
          (return-from command 1)))
      (let* ((elapsed (/ (- (clempo.io:monotonic-ticks) start)
                         (float internal-time-units-per-second 1d0)))
             (allocated (- (sb-ext:get-bytes-consed) bytes))
             (report (clempo.json:jobject
                      "iterations" iterations "accepted" accepted "rejected" rejected
                      "selected" selected "seconds" elapsed "bytesAllocated" allocated
                      "validationsPerSecond" (if (plusp elapsed) (/ (+ accepted rejected) elapsed) 0))))
        (if (arg-flag args :json)
            (format t "~A~%" (clempo.json:to-json-string report))
            (format t "Pool fixtures: ~D admitted, ~D expected rejections, ~D selected; ~,3Fs, ~D bytes allocated.~%"
                    accepted rejected selected elapsed allocated)))
      0)))
