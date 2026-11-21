(defpackage #:clempo.metrics
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:socket #:sb-bsd-sockets))
  (:documentation "A Prometheus registry and its text-format listener, shared by the node,
the validator and the RPC server. A metric is a counter, a gauge or a summary (count and
sum of observations, for latencies), optionally split by label sets.")
  (:export #:define-metric #:metric #:metric-set #:metric-inc #:metric-observe #:metric-value
           #:render-metrics #:start-metrics-server #:stop-metrics-server #:metrics-server-port
           #:*registry* #:make-registry))

(in-package #:clempo.metrics)

(defstruct (registry (:constructor make-registry ()))
  (lock (sb-thread:make-mutex :name "metrics"))
  (metrics (make-hash-table :test 'equal))
  (order '()))

(defvar *registry* (make-registry) "The process's metrics.")

(defstruct (metric (:constructor %make-metric (name kind help registry)))
  name kind help registry
  (series (make-hash-table :test 'equal)))   ; labels alist -> number, or (count . sum)

(defun define-metric (name kind help &key (registry *registry*))
  "Find or create the metric NAME (a string, e.g. \"clempo_head\") of KIND :COUNTER,
:GAUGE or :SUMMARY. Redefining with another kind is an error."
  (check-type kind (member :counter :gauge :summary))
  (unless (every (lambda (c) (or (alphanumericp c) (char= c #\_))) name)
    (error "Bad metric name ~S" name))
  (sb-thread:with-mutex ((registry-lock registry))
    (let ((found (gethash name (registry-metrics registry))))
      (cond (found (unless (eq kind (metric-kind found))
                     (error "Metric ~A is a ~(~A~)" name (metric-kind found)))
                   found)
            (t (push name (registry-order registry))
               (setf (gethash name (registry-metrics registry))
                     (%make-metric name kind help registry)))))))

(defmacro with-series ((metric) &body body)
  `(sb-thread:with-mutex ((registry-lock (metric-registry ,metric))) ,@body))

(defun metric-set (metric value &key labels)
  (with-series (metric) (setf (gethash labels (metric-series metric)) value)))

(defun metric-inc (metric &key (by 1) labels)
  (with-series (metric) (incf (gethash labels (metric-series metric) 0) by)))

(defun metric-observe (metric value &key labels)
  "Add one observation (e.g. seconds) to a summary."
  (with-series (metric)
    (let ((cell (or (gethash labels (metric-series metric))
                    (setf (gethash labels (metric-series metric)) (cons 0 0)))))
      (incf (car cell)) (incf (cdr cell) value))))

(defun metric-value (metric &key labels)
  (with-series (metric) (gethash labels (metric-series metric))))

(defun escape-label (value)
  (with-output-to-string (out)
    (loop for c across (princ-to-string value)
          do (case c (#\\ (write-string "\\\\" out)) (#\" (write-string "\\\"" out))
                   (#\Newline (write-string "\\n" out)) (t (write-char c out))))))

(defun write-series (out name labels value)
  (format out "~A~@[{~{~A~^,~}}~] ~A~%" name
          (and labels (mapcar (lambda (l) (format nil "~A=\"~A\"" (car l) (escape-label (cdr l))))
                              labels))
          (if (integerp value) value (format nil "~,6F" value))))

(defun render-metrics (&optional (registry *registry*))
  "The Prometheus text exposition of REGISTRY."
  (with-output-to-string (out)
    (sb-thread:with-mutex ((registry-lock registry))
      (dolist (name (reverse (registry-order registry)))
        (let ((m (gethash name (registry-metrics registry))))
          (format out "# HELP ~A ~A~%# TYPE ~A ~(~A~)~%" name (metric-help m) name (metric-kind m))
          (let ((series (sort (loop for k being the hash-keys of (metric-series m) using (hash-value v)
                                    collect (cons k v))
                              #'string< :key (lambda (e) (prin1-to-string (car e))))))
            (loop for (labels . value) in series
                  do (if (eq (metric-kind m) :summary)
                         (progn (write-series out (format nil "~A_count" name) labels (car value))
                                (write-series out (format nil "~A_sum" name) labels (cdr value)))
                         (write-series out name labels value)))))))))

;;; The listener: GET /metrics only, one short-lived connection at a time, read bounded.

(defstruct (metrics-server (:constructor %make-metrics-server (listener port registry)))
  listener port registry thread (stopped nil))

(defun read-request-line (stream)
  (let ((line (make-string-output-stream)))
    (loop repeat 1024
          for c = (read-char stream nil)
          until (or (null c) (char= c #\Newline))
          do (write-char c line))
    (get-output-stream-string line)))

(defun serve-connection (server client)
  (with-open-stream (stream (socket:socket-make-stream client :input t :output t
                                                      :external-format :latin-1 :buffering :full
                                                      :timeout 2))
    (let* ((line (read-request-line stream))
           (ok (and (>= (length line) 12) (string= "GET /metrics" line :end2 12)))
           (body (if ok (render-metrics (metrics-server-registry server)) "not found
"))
           (octets (length (string->octets body))))
      (format stream "HTTP/1.1 ~:[404 Not Found~;200 OK~]~C~CContent-Type: text/plain; version=0.0.4; charset=utf-8~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
              ok #\Return #\Newline #\Return #\Newline octets #\Return #\Newline
              #\Return #\Newline #\Return #\Newline)
      (write-string body stream)
      (finish-output stream))))

(defun run-server (server)
  (let ((listener (metrics-server-listener server)))
    (unwind-protect
         (loop until (metrics-server-stopped server)
               when (sb-sys:wait-until-fd-usable (socket:socket-file-descriptor listener) :input 0.2)
                 do (let ((client (ignore-errors (socket:socket-accept listener))))
                      (when client
                        (unwind-protect
                             (handler-case (serve-connection server client)
                               (error () nil))
                          (ignore-errors (socket:socket-close client))))))
      (socket:socket-close listener))))

(defun start-metrics-server (host port &key (registry *registry*))
  "Serve REGISTRY as GET /metrics on HOST (dotted IPv4 string) and PORT (0 picks one)."
  (let ((listener (make-instance 'socket:inet-socket :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (setf (socket:sockopt-reuse-address listener) t)
          (socket:socket-bind listener (socket:make-inet-address host) port)
          (socket:socket-listen listener 16)
          (let ((server (%make-metrics-server listener (nth-value 1 (socket:socket-name listener))
                                              registry)))
            (setf (metrics-server-thread server)
                  (sb-thread:make-thread (lambda () (run-server server)) :name "metrics listener"))
            server))
      (error (c) (socket:socket-close listener) (error c)))))

(defun stop-metrics-server (server)
  (setf (metrics-server-stopped server) t)
  (sb-thread:join-thread (metrics-server-thread server) :default nil))
