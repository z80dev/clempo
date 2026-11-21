(defpackage #:clempo.metrics.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:metrics #:clempo.metrics) (#:socket #:sb-bsd-sockets)))

(in-package #:clempo.metrics.tests)

(deftest prometheus-text-and-listener ()
  (let* ((metrics:*registry* (metrics:make-registry))
         (head (metrics:define-metric "t_head" :gauge "head"))
         (calls (metrics:define-metric "t_calls_total" :counter "calls"))
         (latency (metrics:define-metric "t_seconds" :summary "latency")))
    (metrics:metric-set head 7)
    (metrics:metric-inc calls :labels '(("method" . "eth_call")))
    (metrics:metric-inc calls :by 2 :labels '(("method" . "eth_call")))
    (metrics:metric-observe latency 0.5)
    (metrics:metric-observe latency 0.25)
    (is (eq head (metrics:define-metric "t_head" :gauge "head")))
    (signals error (metrics:define-metric "t_head" :counter "head"))
    (let ((text (metrics:render-metrics)))
      (is (search (format nil "# TYPE t_head gauge~%t_head 7~%") text))
      (is (search "t_calls_total{method=\"eth_call\"} 3" text))
      (is (search "t_seconds_count 2" text))
      (is (search "t_seconds_sum 0.750000" text)))
    (let ((server (metrics:start-metrics-server "127.0.0.1" 0))
          (client (make-instance 'socket:inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (socket:socket-connect client #(127 0 0 1) (metrics:metrics-server-port server))
             (let ((s (socket:socket-make-stream client :input t :output t :timeout 5
                                                        :external-format :latin-1)))
               (format s "GET /metrics HTTP/1.1~C~CHost: x~C~C~C~C" #\Return #\Newline
                       #\Return #\Newline #\Return #\Newline)
               (finish-output s)
               (let ((reply (with-output-to-string (out)
                              (loop for line = (read-line s nil) while line do (write-line line out)))))
                 (is (search "200 OK" reply))
                 (is (search "t_head 7" reply)))))
        (socket:socket-close client)
        (metrics:stop-metrics-server server)))))

(deftest cells-count-without-the-lock ()
  (let* ((metrics:*registry* (metrics:make-registry))
         (calls (metrics:define-metric "t_calls_total" :counter "calls"))
         (latency (metrics:define-metric "t_seconds" :summary "latency"))
         (labels '(("method" . "eth_call"))))
    (metrics:metric-inc calls :labels labels)            ; counted before the cell exists
    (let ((cell (metrics:metric-cell calls labels)) (time (metrics:metric-cell latency nil)))
      (is (eq cell (metrics:metric-cell calls labels)))
      (mapc #'sb-thread:join-thread
            (loop for k below 8
                  collect (let ((k k))
                            (sb-thread:make-thread
                             (lambda ()
                               (let ((metrics:*shard* (mod k 3)))
                                 (dotimes (i 1000)
                                   (metrics:cell-inc cell)
                                   (metrics:cell-observe-ticks time (floor internal-time-units-per-second 1000)))))))))
      (metrics:metric-inc calls :by 2 :labels labels)
      (is= 8003 (metrics:metric-value calls :labels labels))
      (is= 8000 (car (metrics:metric-value latency)))
      (let ((text (metrics:render-metrics)))
        (is (search "t_calls_total{method=\"eth_call\"} 8003" text))
        (is (search "t_seconds_count 8000" text))
        (is (search "t_seconds_sum 8.000000" text))))))
