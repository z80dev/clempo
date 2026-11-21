(in-package #:clempo.dev)

(defun https-get (url post repeats)
  (multiple-value-bind (scheme host port) (clempo.http:parse-url url)
    ;; One fresh connection timed by phase (anchors, TCP connect, TLS handshake), then
    ;; REPEATS requests through the pooled client.
    (let* ((t-load (clempo.io:now-ms))
           (anchors (clempo.x509:trust-store-count (clempo.tls:system-trust-store)))
           (t0 (clempo.io:now-ms))
           (stream (clempo.http:open-connection "http" host port))
           (t1 (clempo.io:now-ms))
           (tls (when (string= scheme "https") (clempo.tls:tls-connect stream host)))
           (t2 (clempo.io:now-ms)))
      (log-line "~D trust anchors loaded in ~D ms; connect ~D ms, handshake ~D ms~@[, ~A~]~@[, alpn ~A~]"
                anchors (- t0 t-load) (- t1 t0) (- t2 t1)
                (and tls (clempo.tls::suite-name (clempo.tls:tls-suite tls)))
                (and tls (clempo.tls:tls-alpn tls)))
      (close (or tls stream)))
    (let ((status nil) (body nil) (bytes 0) (start (clempo.io:now-ms)))
      (dotimes (i repeats)
        (multiple-value-setq (status body)
          (if post
              (clempo.http:http-post url (string->octets post))
              (clempo.http:http-get url)))
        (incf bytes (length body)))
      (let ((seconds (/ (- (clempo.io:now-ms) start) 1000.0)))
        (log-line "~D request~:P on pooled connections in ~,1F ms: ~,1F ms each, ~,2F MB/s"
                  repeats (* 1000 seconds) (/ (* 1000 seconds) repeats)
                  (if (plusp seconds) (/ bytes seconds 1e6) 0)))
      (format t "~D~%" status)
      (write-line (or (ignore-errors (octets->string body :end (min (length body) 4096)))
                      (format nil "<~D bytes of binary>" (length body))))
      (if (< status 400) 0 1))))

(defcommand "https-get" (args)
  "fetch a URL over clempo's TLS: https-get <url> [-post json] [-n repeats]; prints status, body, timings"
  (let ((url (or (first (arg args :rest)) (return-from command (usage)))))
    (handler-case (https-get url (arg args :post) (arg-int args :n 1))
      ((or clempo.tls:tls-error clempo.http:http-error) (e)
        (log-line "~A" e)
        1))))
