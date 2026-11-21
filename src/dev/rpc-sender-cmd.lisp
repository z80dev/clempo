(in-package #:clempo.dev)

(defcommand "rpc-sender-index" (args)
  "build the (sender, nonce) event index over the archive: rpc-sender-index [-threads N] [-to BLOCK]"
  (let ((start (clempo.io:monotonic-ticks)))
    (multiple-value-bind (events txs)
        (apply #'clempo.rpc.sender:build-sender-index :threads (arg-int args :threads 16)
               (when (arg args :to) (list :to (arg-int args :to))))
      (format t "sender index: ~D events from ~D transactions in ~,1F s -> ~A~%" events txs
              (/ (- (clempo.io:monotonic-ticks) start) internal-time-units-per-second)
              (clempo.rpc.sender:sender-index-path))
      0)))
