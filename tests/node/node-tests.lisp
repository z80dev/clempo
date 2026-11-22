(defpackage #:clempo.node.run-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:node #:clempo.node) (#:json #:clempo.json) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers) (#:store #:clempo.store)
                    (#:fake #:clempo.sync.fake-upstream)))

(in-package #:clempo.node.run-tests)

(define-constant archive-chunk clempo.archive:+chunk-blocks+)

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
         ;; Caches are datadir-scoped unless shared explicitly, and the history (seeded at
         ;; genesis, appended per block) covers every followed block's state.
         (is (probe-file (merge-pathnames "receipts/" datadir)))
         (let ((history (clempo.history:open-history (merge-pathnames "history/" datadir))))
           (unwind-protect (is= 20 (clempo.history:history-height history))
             (clempo.history:close-history history))))))))

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

(deftest followed-hashes-survive-restart ()
  ;; Hashes of followed blocks resolve after a restart without --index-dir, from the
  ;; datadir's followed.log; entries past the restored head (a crash) are ignored.
  (let ((raws (recorded-blocks)))
    (with-temp-dirs (index)
      (let ((node (make-instance 'clempo.node::node-backend :archive-dir index :head 20000)))
        (clempo.node::open-hash-log node index 0)
        (loop for n from 1 to 20
              do (clempo.node::log-block-hashes node (blocks:decode-block (svref raws n)) n))
        (close (clempo.node::node-hash-log node)))
      (let ((node (make-instance 'clempo.node::node-backend :archive-dir index :head 20000))
            (block-7 (blocks:decode-block (svref raws 7)))
            (block-15 (blocks:decode-block (svref raws 15))))
        (clempo.node::open-hash-log node index 12)
        (unwind-protect
             (progn
               (is= 7 (gethash (blocks:block-hash block-7) (clempo.node::node-followed-blocks node)))
               (is= (ash 7 20)
                          (gethash (clempo.transactions:transaction-hash
                                    (first (blocks:block-transactions block-7)))
                                   (clempo.node::node-followed-txs node)))
               (is (null (gethash (blocks:block-hash block-15) (clempo.node::node-followed-blocks node)))))
          (close (clempo.node::node-hash-log node)))))))

(defun synthetic-chain (count)
  "Raw blocks 1..COUNT of a linked synthetic chain (index I holds block I+1)."
  (let ((parent (make-octets 32)) (out (make-array count)))
    (dotimes (i count out)
      (multiple-value-bind (raw hash) (clempo.follow.tests::test-block (1+ i) parent)
        (setf (svref out i) raw parent hash)))))

(deftest followed-hashes-fold-into-segments ()
  ;; At each completed archive chunk the in-memory entries fold into an on-disk segment;
  ;; memory keeps only the open chunk, and lookups and restarts still resolve folded hashes.
  (let ((chain (synthetic-chain (+ archive-chunk 5))))
    (with-temp-dirs (index)
      (let ((node (make-instance 'clempo.node::node-backend :archive-dir index :head 20000)))
        (clempo.node::open-hash-log node index 0)
        (loop for i from 0 below (length chain)
              do (clempo.node::log-block-hashes node (blocks:decode-block (svref chain i)) (1+ i)))
        ;; Blocks 1..9999 folded at block 9999; 10000..10005 remain in memory.
        (is= (- archive-chunk 1) (clempo.node::node-folded-to node))
        (is= 6 (hash-table-count (clempo.node::node-followed-blocks node)))
        (is (probe-file (format nil "~Afollowed-000000000000-~12,'0D-blocks.idx" index (1- archive-chunk))))
        (let ((hash-7 (blocks:block-hash (blocks:decode-block (svref chain 6)))))
          (is= 7 (clempo.node::segment-block-number node hash-7))
          (close (clempo.node::node-hash-log node))
          ;; Restart at head 10003: the segment answers 7; the log holds only 10000..10003.
          (let ((again (make-instance 'clempo.node::node-backend :archive-dir index :head 20000)))
            (clempo.node::open-hash-log again index 10003)
            (unwind-protect
                 (progn
                   (is= 7 (clempo.node::segment-block-number again hash-7))
                   (is= 4 (hash-table-count (clempo.node::node-followed-blocks again))))
              (close (clempo.node::node-hash-log again)))))))))

;;; Folds and merges, small: every few blocks the open entries fold into a segment and due
;;; segments merge, through the same functions the node runs at each archive chunk.

(define-condition simulated-crash (error) ((step :initarg :step :reader crash-step)))

(defun fake-tx-hash (n j) (clempo.keccak:keccak256 (integer->octets (+ (* 1000 n) j) 8)))

(defun log-followed (node block n)
  "Log block N and two transactions of it, as the follower does."
  (let ((out (clempo.node::node-hash-log node)))
    (dotimes (j 2)
      (let ((value (logior (ash n 20) j)))
        (write-byte 1 out) (write-sequence (fake-tx-hash n j) out)
        (write-sequence (integer->octets value 8) out)
        (clempo.node::note-hash node 1 (fake-tx-hash n j) value))))
  (clempo.node::log-block-hashes node block n))

(defun index-hits (node kind hash value)
  "How many places hold HASH -> VALUE: the in-memory table plus every segment."
  (+ (if (eql value (gethash hash (if (zerop kind)
                                       (clempo.node::node-followed-blocks node)
                                       (clempo.node::node-followed-txs node))))
         1 0)
     (loop for (nil blocks txs) in (clempo.node::node-segments node)
           sum (count value (clempo.rpc.chain:index-lookup (if (zerop kind) blocks txs)
                                                           (clempo.rpc.chain:hash-prefix hash))))))

(defun every-hash-once-p (node blocks head)
  (loop for n from 1 to head
        always (and (= 1 (index-hits node 0 (blocks:block-hash (svref blocks (1- n))) n))
                    (loop for j below 2 always (= 1 (index-hits node 1 (fake-tx-hash n j)
                                                                (logior (ash n 20) j)))))))

(defun files-match-segments-p (node dir)
  "DIR holds followed.log and exactly the open segments' files."
  (let ((names (mapcar #'file-namestring (directory (merge-pathnames "*.*" dir))))
        (want (cons "followed.log"
                    (loop for (to nil nil from) in (clempo.node::node-segments node)
                          append (loop for kind in '(:blocks :txs)
                                       collect (file-namestring (clempo.node::segment-path dir from to kind)))))))
    (and (= (length names) (length want)) (subsetp want names :test #'string=))))

(defun open-followed (dir head)
  (let ((node (make-instance 'clempo.node::node-backend :archive-dir dir :head 20000)))
    (clempo.node::open-hash-log node dir head)
    node))

(defun drive-followed (node blocks from to on-logged)
  "Log blocks FROM..TO, folding every 4 blocks and merging what is due."
  (loop for n from from to to
        do (log-followed node (svref blocks (1- n)) n)
           (funcall on-logged n)
           (when (zerop (mod n 4))
             (clempo.node::fold-followed node n)
             (clempo.node::merge-segments node))))

(deftest followed-index-survives-a-crash-at-every-fold-step ()
  ;; Stop the node at the Kth durable step of folding and merging, for every K: a reopen
  ;; finds every logged hash exactly once, and following on to the end keeps that true
  ;; and leaves no stray files.
  (let* ((total 24)
         (blocks (map 'vector #'blocks:decode-block (synthetic-chain total)))
         (seen '()))
    (loop for crash-at from 1
          for crashed = nil
          do (with-temp-dirs (index)
               (let ((node (open-followed index 0)) (head 0) (count 0))
                 (handler-case
                     (let ((clempo.node::*fold-step-hook*
                             (lambda (step)
                               (when (= (incf count) crash-at) (error 'simulated-crash :step step)))))
                       (drive-followed node blocks 1 total (lambda (n) (setf head n))))
                   (simulated-crash (c) (setf crashed (crash-step c)) (pushnew crashed seen)))
                 (close (clempo.node::node-hash-log node))
                 (let ((again (open-followed index head)))
                   (unwind-protect
                        (progn
                          (is (every-hash-once-p again blocks head) "crash ~D at ~S" crash-at crashed)
                          (drive-followed again blocks (1+ head) total (lambda (n) n))
                          (is (every-hash-once-p again blocks total) "crash ~D at ~S, then on" crash-at crashed)
                          (is (files-match-segments-p again index))
                          (is (null (clempo.node::due-merge (clempo.node::node-segments again)))))
                     (clempo.node::close-hash-log again)))))
          until (null crashed))
    (is (null (set-difference '(:fold-txs-written :fold-blocks-written :fold-log-written
                                :fold-log-renamed :merge-txs-written :merge-blocks-written
                                :merge-input-deleted)
                              seen)))))

(deftest followed-segments-merge-in-the-background ()
  ;; 16 folds through the background merger leave one segment per set bit of the fold
  ;; count, not 16; the merged segments resolve every hash once and survive a restart.
  (let* ((total 64)
         (blocks (map 'vector #'blocks:decode-block (synthetic-chain total))))
    (with-temp-dirs (index)
      (let ((node (open-followed index 0)))
        (loop for n from 1 to total
              do (log-followed node (svref blocks (1- n)) n)
                 (when (and (zerop (mod n 4)) (clempo.node::fold-followed node n))
                   (clempo.node::start-segment-merge node)))
        (sb-thread:join-thread (clempo.node::node-merger node))
        (let ((segments (clempo.node::node-segments node)))
          (is (null (clempo.node::due-merge segments)))
          (is (<= (length segments) 2) "~D segments" (length segments))
          (is= '(0 . 64) (cons (fourth (car (last segments))) (first (first segments)))))
        (is (every-hash-once-p node blocks total))
        (clempo.node::close-hash-log node)
        (let ((again (open-followed index total)))
          (unwind-protect
               (progn (is (every-hash-once-p again blocks total))
                      (is (files-match-segments-p again index)))
            (clempo.node::close-hash-log again)))))))

(deftest hash-index-seeded-from-a-complete-chunk ()
  (let ((chain (synthetic-chain archive-chunk)))
    (with-temp-dirs (archive index)
      ;; Chunk 0 holds blocks 0..9999: a synthetic genesis stand-in then blocks 1..9999.
      (let ((blocks (make-array archive-chunk)))
        (setf (svref blocks 0) (clempo.follow.tests::test-block 0 (make-octets 32)))
        (replace blocks chain :start1 1)
        (clempo.archive:write-chunk 0 blocks archive))
      (ensure-directories-exist index)
      (clempo.node::seed-hash-index index archive 5000)
      (let ((blocks-index (clempo.rpc.chain:open-index (format nil "~Ablocks.idx" index)))
            (hash (blocks:block-hash (blocks:decode-block (svref chain 41)))))
        (is (member 42 (clempo.rpc.chain:index-lookup blocks-index (clempo.rpc.chain:hash-prefix hash))))
        (is= 5000 (clempo.rpc.chain:index-covered-height index))))))

;;; --to, --hold, --summary and --recent.

(defun read-summary (path) (json:parse-json (clempo.io:read-file-string path)))

(deftest node-stops-at-to-and-writes-a-summary ()
  ;; The tip is past --to: the node stops after block 7, not at the tip, and writes the
  ;; report it returns to --summary. A head already at --to stops at once.
  (let ((raws (recorded-blocks)) (tip (list 20)))
    (with-temp-dirs (datadir archive out)
      (let ((summary (format nil "~Asummary.json" out)))
        (fake:call-with-fake-upstream
         (upstream raws tip)
         (lambda (url)
           (let ((report (run datadir archive url :to 7 :summary summary)))
             (is= 7 (json:jget report "head"))
             (is= 7 (json:jget report "executed"))
             (is= 7 (json:jget report "to"))
             (is= 20 (json:jget report "tip"))
             (is (plusp (json:jget report "transactions")))
             (is (null (json:jget report "failed")))
             (let ((written (read-summary summary)))
               (is= 7 (json:jget written "head"))
               (is (equal (json:jget report "hash") (json:jget written "hash")))
               (is (floatp (json:jget written "seconds")))))
           (let ((report (run datadir archive url :to 5)))
             (is= 7 (json:jget report "head"))
             (is= 0 (json:jget report "executed")))))))))

(deftest node-waits-for-the-tip-to-reach-to ()
  ;; --to past the tip: it follows the tip, waits, and stops at --to once the tip passes it.
  (let* ((raws (recorded-blocks)) (tip (list 4)) (report nil)
         (rpc-port (free-port)) (node-url (format nil "http://127.0.0.1:~D/" rpc-port)))
    (with-temp-dirs (datadir archive)
      (fake:call-with-fake-upstream
       (upstream raws tip)
       (lambda (url)
         (let ((thread (sb-thread:make-thread
                        (lambda () (setf report (run datadir archive url :rpc-port rpc-port :to 9)))
                        :name "node under test")))
           (is (loop repeat 600
                     thereis (equal "0x4" (ignore-errors (clempo.jsonrpc:rpc-call "eth_blockNumber" '()
                                                                                  :url node-url :retries 0)))
                     do (sleep 0.05)))
           (is (sb-thread:thread-alive-p thread))
           (setf (car tip) 20)
           (sb-thread:join-thread thread :timeout 60 :default nil)
           (is report)
           (when report
             (is= 9 (json:jget report "head"))
             (is (eq :false (json:jget report "stopped"))))))))))

(deftest node-holds-rpc-after-to-until-sigterm ()
  (let* ((raws (recorded-blocks)) (tip (list 20)) (report nil)
         (rpc-port (free-port)) (node-url (format nil "http://127.0.0.1:~D/" rpc-port)))
    (signals error (node:run-node "/nonexistent/clempo-hold" :hold t))
    (with-temp-dirs (datadir archive out)
      (let ((summary (format nil "~Asummary.json" out)))
        (fake:call-with-fake-upstream
         (upstream raws tip)
         (lambda (url)
           (let ((thread (sb-thread:make-thread
                          (lambda () (setf report (run datadir archive url :rpc-port rpc-port :to 6 :hold t
                                                                           :summary summary)))
                          :name "node under test")))
             (is (loop repeat 600
                       thereis (equal "0x6" (ignore-errors (clempo.jsonrpc:rpc-call "eth_blockNumber" '()
                                                                                    :url node-url :retries 0)))
                       do (sleep 0.05)))
             ;; Still serving, still at 6, though the tip is at 20.
             (sleep 0.3)
             (is (sb-thread:thread-alive-p thread))
             (is (equal "0x6" (json:jget* (clempo.jsonrpc:rpc-call "clempo_status" '() :url node-url :retries 0)
                                          "head" "number")))
             (is (not (probe-file summary)))
             (sb-posix:kill (sb-posix:getpid) sb-unix:sigterm)
             (sb-thread:join-thread thread :timeout 30 :default nil)
             (is report)
             (when report
               (is= 6 (json:jget report "head"))
               (is (eq :true (json:jget report "stopped"))))
             (is= 6 (json:jget (read-summary summary) "head"))
             (is (null (ignore-errors (clempo.jsonrpc:rpc-call "eth_blockNumber" '() :url node-url
                                                                                   :retries 0)))))))))))

(deftest node-summary-records-a-failed-run ()
  ;; A bounded run (--to) gives up after its retries; the summary says why, and the error
  ;; still reaches the caller.
  (let ((raws (recorded-blocks)) (tip (list 20)))
    (with-temp-dirs (datadir archive out)
      (let ((summary (format nil "~Asummary.json" out)))
        (fake:call-with-fake-upstream
         (upstream raws tip)
         (lambda (url)
           (signals error (run datadir archive url :to 10 :summary summary :poll-seconds 0.001
                                                   :block-source (lambda (from count)
                                                                   (declare (ignore from count))
                                                                   nil)))
           (let ((written (read-summary summary)))
             (is= 0 (json:jget written "head"))
             (is (search "wrong block batch shape" (json:jget written "failed"))))))))))

(deftest recent-ring-is-sized-and-indexes-its-hashes ()
  (let ((raws (recorded-blocks)))
    (is= 1024 (length (clempo.node::node-recent (make-instance 'clempo.node::node-backend
                                                               :archive-dir "/nonexistent/" :head 0))))
    (signals error (clempo.node::make-recent-ring 0))
    (signals error (clempo.node::make-recent-ring 65537))
    (let ((options (node:node-options '("--to" "9" "--recent" "64" "--summary" "s.json" "--hold"))))
      (is (equal '(9 64 "s.json" t) (mapcar (lambda (k) (getf options k)) '(:to :recent :summary :hold)))))
    ;; A ring of 4: block 7 is evicted by 11 and leaves the hash lookups with it.
    (let ((node (make-instance 'clempo.node::node-backend :archive-dir "/nonexistent/" :head 0
                                                          :recent (clempo.node::make-recent-ring 4)))
          (block-7 (blocks:decode-block (svref raws 7))))
      (loop for n from 7 to 11
            do (clempo.node::note-recent node (clempo.node::recent-entry (blocks:decode-block (svref raws n)) nil)))
      (is (null (clempo.node::recent-block node 7)))
      (is= 11 (clempo.node::recent-block-number (clempo.node::recent-block node 11)))
      (is (null (gethash (blocks:block-hash block-7) (clempo.node::node-recent-blocks node))))
      (is (null (gethash (clempo.transactions:transaction-hash (first (blocks:block-transactions block-7)))
                         (clempo.node::node-recent-txs node))))
      (let* ((block-10 (blocks:decode-block (svref raws 10)))
             (tx (first (blocks:block-transactions block-10))))
        (is= 10 (gethash (blocks:block-hash block-10) (clempo.node::node-recent-blocks node)))
        (is (equal '(10 0) (multiple-value-list
                            (clempo.rpc.chain:backend-tx-location node (clempo.transactions:transaction-hash tx)))))))))
