(defpackage #:clempo.node.run-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:node #:clempo.node) (#:json #:clempo.json) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers) (#:store #:clempo.store)
                    (#:fake #:clempo.sync.fake-upstream)))

(in-package #:clempo.node.run-tests)

;;; bin/clempo end to end on mainnet blocks 0..20 (data/README.md) served by a loopback
;;; upstream: an empty datadir starts at genesis with certification on, executes the range,
;;; restarts from its own store, and stops cleanly on SIGTERM.

(defun recorded-blocks (&optional (chain "mainnet"))
  (with-open-file (in (asdf:system-relative-pathname
                       "clempo-node/tests" (format nil "../../tests/node/data/~A-0-20.rawblocks.hex" chain)))
    (coerce (loop for line = (read-line in nil) while line collect (hex->octets line)) 'simple-vector)))

(defun upstream (raws tip-cell &key fail-burst)
  "Serve RAWS as the canonical chain, with its tip at (CAR TIP-CELL). With FAIL-BURST (FROM . TO),
those requests fail, as during an upstream outage."
  (let ((count 0))
    (lambda (method params)
      (when (and fail-burst (<= (car fail-burst) (incf count) (cdr fail-burst)))
        (error 'fake:rpc-failure :code -32000 :message "upstream hiccup"))
      (flet ((number-of (tag) (if (equal tag "latest") (car tip-cell) (hex->integer tag))))
        (cond ((string= method "eth_getBlockByNumber")
               (let ((n (number-of (first params))))
                 (if (< n (length raws))
                     (let ((block (blocks:decode-block (svref raws n))))
                       (json:jobject "number" (quantity->hex n)
                                     "hash" (octets->hex (blocks:block-hash block) :prefix t)
                                     "stateRoot" (octets->hex (headers:header-state-root
                                                               (blocks:block-header block))
                                                              :prefix t)))
                     :null)))
              ((string= method "debug_getRawBlock")
               (octets->hex (svref raws (number-of (first params))) :prefix t))
              ((string= method "eth_blockNumber") (quantity->hex (car tip-cell)))
              (t (error 'fake:rpc-failure :code -32601 :message "not served")))))))

(defun temp-dir (name)
  (format nil "/tmp/clempo-node-test-~A-~A/" name (octets->hex (clempo.io:random-octets 6))))

(defmacro with-temp-dirs ((&rest vars) &body body)
  `(let ,(mapcar (lambda (v) `(,v (temp-dir ,(string-downcase v)))) vars)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (v) `(when (probe-file ,v) (sb-ext:delete-directory ,v :recursive t))) vars))))

(defun run (datadir archive url &rest options)
  ;; OPTIONS come first: the first occurrence of a keyword wins.
  (apply #'node:run-node datadir (append options (list :upstream url :cert-rpc url :archive-dir archive
                                                          :rpc-port 0 :root-workers 1 :batch-size 8
                                                          :poll-seconds 0.01))))

(deftest node-bootstraps-from-genesis-with-certification ()
  (let* ((raws (recorded-blocks)) (tip (list 20))
         (block-20 (blocks:decode-block (svref raws 20))))
    (with-temp-dirs (datadir archive)
      (fake:call-with-fake-upstream
       (upstream raws tip)
       (lambda (url)
         (let ((report (run datadir archive url :once t)))
           (is= 0 (json:jget report "anchor"))
           (is= 20 (json:jget report "head"))
           (is= 20 (json:jget report "executed"))
           ;; Mainnet serves no certificates this low: admitted on links and execution.
           (is= 20 (json:jget report "uncertified"))
           (is (string-equal (octets->hex (blocks:block-hash block-20) :prefix t)
                             (json:jget report "hash")))
           (is (string-equal (octets->hex (headers:header-state-root (blocks:block-header block-20))
                                          :prefix t)
                             (json:jget report "stateRoot"))))
         ;; Restart from the store just written; the datadir lock was released.
         (let ((report (run datadir archive url :once t)))
           (is= 20 (json:jget report "anchor"))
           (is= 0 (json:jget report "executed")))
         ;; Caches are datadir-scoped unless shared explicitly.
         (is (probe-file (merge-pathnames "receipts/" datadir))))))))

(deftest node-bootstraps-moderato-from-genesis ()
  ;; Moderato runs system transactions before :t0 (fork :genesis); this once failed block 1.
  (let ((raws (recorded-blocks "moderato")) (tip (list 20)))
    (with-temp-dirs (datadir archive)
      (fake:call-with-fake-upstream
       (upstream raws tip)
       (lambda (url)
         (let ((report (run datadir archive url :chain :moderato :once t)))
           (is (equal "moderato" (json:jget report "chain")))
           (is= 20 (json:jget report "head"))
           (is (string-equal "0xc10e8ebef1a01089addba69d5d7dadddef042569e146f2f6be065b154a2ec513"
                             (json:jget report "hash")))
           (is (string-equal "0x3952ff0c60a3f215ab11a3a2c7698126b7911da84faf5cb66bdbcb0c605738c4"
                             (json:jget report "stateRoot")))))))))

(deftest node-survives-upstream-errors ()
  (let ((raws (recorded-blocks)) (tip (list 12)))
    (with-temp-dirs (datadir archive)
      (fake:call-with-fake-upstream
       (upstream raws tip :fail-burst '(3 . 9))
       (lambda (url)
         (let ((report (run datadir archive url :once t)))
           (is= 12 (json:jget report "head"))))))))

(defun free-port ()
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect (progn (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
                           (nth-value 1 (sb-bsd-sockets:socket-name s)))
      (sb-bsd-sockets:socket-close s))))

(defun http-get-text (port path)
  (multiple-value-bind (status body) (clempo.http:http-get (format nil "http://127.0.0.1:~D~A" port path))
    (values (octets->string body) status)))

(deftest live-node-serves-status-pool-metrics-and-stops-on-sigterm ()
  (let* ((raws (recorded-blocks)) (tip (list 5)) (report nil) (forwarded '())
         (rpc-port (free-port)) (metrics-port (free-port))
         (node-url (format nil "http://127.0.0.1:~D/" rpc-port))
         (served (upstream raws tip)))
    (with-temp-dirs (datadir archive)
      (fake:call-with-fake-upstream
       (lambda (method params)
         (if (string= method "eth_sendRawTransaction")
             (progn (push (first params) forwarded) (first params))
             (funcall served method params)))
       (lambda (url)
         (let ((thread (sb-thread:make-thread
                        (lambda () (setf report (run datadir archive url :rpc-port rpc-port
                                                     :metrics-address (format nil "127.0.0.1:~D" metrics-port))))
                        :name "node under test")))
           ;; Live mode: wait until it serves the tip.
           (let ((status (loop repeat 600
                               for s = (ignore-errors (clempo.jsonrpc:rpc-call "clempo_status" '() :url node-url
                                                                                :retries 0))
                               until (and s (equal "0x5" (json:jget* s "head" "number")))
                               do (sleep 0.05)
                               finally (return s))))
             (is (equal "0x5" (json:jget* status "head" "number")))
             (is (equal "mainnet" (json:jget status "chain")))
             (is= 0 (json:jget* status "txpool" "pending")))
           ;; Admission is validated locally: an unfunded sender is refused and never forwarded.
           (let ((raw (clempo.transactions:encode-transaction (clempo.txpool.fixtures:fixture-transaction))))
             (is (typep (handler-case (clempo.jsonrpc:rpc-call "eth_sendRawTransaction"
                                                              (list (octets->hex raw :prefix t))
                                                              :url node-url :retries 0)
                          (clempo.jsonrpc:rpc-error (e) e))
                        'clempo.jsonrpc:rpc-error)))
           (is (null forwarded))
           (is (equal "0x0" (json:jget (clempo.jsonrpc:rpc-call "txpool_status" '() :url node-url :retries 0)
                                       "pending")))
           (let ((text (http-get-text metrics-port "/metrics")))
             (is (search "clempo_head 5" text))
             (is (search "clempo_blocks_executed_total" text))
             (is (search "clempo_block_execution_seconds_count" text)))
           (sb-posix:kill (sb-posix:getpid) sb-unix:sigterm)
           (sb-thread:join-thread thread :timeout 30 :default nil)
           (is report)
           (when report
             (is (eq :true (json:jget report "stopped")))
             (is (search "\"stopped\":true" (json:to-json-string report)))
             (is= 5 (json:jget report "head")))
           ;; Closed and unlocked: the store opens again, and the ports are free.
           (store:close-store (store:open-store datadir))
           (is (null (ignore-errors (clempo.jsonrpc:rpc-call "eth_blockNumber" '() :url node-url
                                                                                 :retries 0))))))))))

(deftest forwarder-drops-what-the-upstream-refuses ()
  (let* ((reader (clempo.txpool.fixtures:make-fixture-reader))
         (db (clempo.txpool:state-reader-db reader))
         (service (clempo.pool-service:make-pool-service))
         (clempo.io:*clock* (lambda () (* clempo.txpool.fixtures:+fixture-timestamp+ 1000)))
         (tx (clempo.txpool.fixtures:fixture-transaction)))
    (clempo.pool-service:pool-service-head service db (clempo.txpool:state-reader-env reader))
    (let ((hash (clempo.pool-service:pool-service-submit service tx db)))
      (fake:call-with-fake-upstream
       (lambda (method params)
         (declare (ignore method params))
         (error 'fake:rpc-failure :code -32000 :message "nonce too low"))
       (lambda (url)
         (let ((forwarder (clempo.node::start-forwarder url service)))
           (clempo.node::forward forwarder (clempo.transactions:encode-transaction tx) hash)
           (clempo.node::stop-forwarder forwarder))))
      (is= 0 (clempo.pool-service:pool-service-count service)))))
