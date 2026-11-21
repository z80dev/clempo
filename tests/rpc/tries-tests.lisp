(defpackage #:clempo.rpc.tries-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:archive #:clempo.archive) (#:store #:clempo.store) (#:history #:clempo.history)
                    (#:tries #:clempo.rpc.tries) (#:executor #:clempo.executor)))
(in-package #:clempo.rpc.tries-tests)

;;; The trie-state cursor pool (clempo.rpc.tries) over mainnet's first 4,000 blocks: the
;;; archive and history of ~/cache/clempo, and a genesis checkpoint written here as the only
;;; base (every history-build checkpoint lies above the head), so every state is a cursor
;;; moved up from genesis and checked against its header.

(defparameter *head* 4000)
(defparameter *blocks* '(1 2 3 1000 2000 3000 3864 3865 3866))
(defparameter *pathusd* "0x20c0000000000000000000000000000000000000")

(defvar *history* nil "The mainnet history, opened once for every test backend.")

(defun mainnet-history ()
  (unless (archive:chunk-present-p 0 (archive:archive-dir))
    (skip-test "mainnet blocks 0..9,999 are not in the archive"))
  (or *history*
      (let ((dir (chain:history-dir)))
        (unless (probe-file dir) (skip-test "no mainnet history (clempo-dev history-build)"))
        (let ((h (history:open-history dir)))
          (unless (history:history-state h *head*) (skip-test "the history does not cover block 4,000"))
          (setf *history* h)))))

(defun call-with-genesis-base (fn)
  "Call FN with a function making archive backends at *HEAD* whose only trie base is a
genesis checkpoint (FN's argument takes the pool's size)."
  (let* ((h (mainnet-history))
         (dir (format nil "/tmp/clempo-tries-test-~D/" (random 100000000 (make-random-state t))))
         (saved tries:*trie-state-dirs*))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (store:write-checkpoint (executor:genesis-db :chain :mainnet)
                                   (format nil "~Acheckpoint-~12,'0D.state" dir 0))
           (setf tries:*trie-state-dirs* (list dir))
           (funcall fn (lambda (cursors)
                         (let ((b (chain:make-archive-backend :head *head* :history h))
                               (tries:*trie-cursors* cursors)
                               ;; these states are small: the heap bound is not under test
                               (tries:*heap-share* 1))
                           (tries::backend-tries b) ; the pool, made at this size
                           b))))
      (setf tries:*trie-state-dirs* saved)
      (sb-ext:delete-directory dir :recursive t))))

(defmacro with-genesis-base ((backend-of) &body body)
  "Run BODY with (BACKEND-OF cursors) making a backend over the genesis base whose pool holds
CURSORS states."
  (let ((make (gensym "MAKE")))
    `(call-with-genesis-base
      (lambda (,make) (flet ((,backend-of (cursors) (funcall ,make cursors))) ,@body)))))

(defun request (method params)
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":~A}" method params))

(defun answer (backend body)
  (let ((clempo.rpc.state:*eth-proof-window* most-positive-fixnum))
    (json:parse-json (chain:with-backend-read (backend) (nth-value 1 (rpc:handle-body body))))))

(defun requests (backend)
  "Every method that takes a trie state, at each of *BLOCKS*."
  (loop for n in *blocks*
        for q = (quantity->hex n)
        append (list (request "debug_intermediateRoots"
                              (format nil "[~S]" (octets->hex (chain:block-hash-of-raw (chain:backend-block-raw backend n)))))
                     (request "debug_executionWitness" (format nil "[~S]" q))
                     (request "debug_stateRootWithUpdates"
                              (format nil "[{\"accounts\":{\"0x~(~64,'0X~)\":{\"nonce\":\"0x1\",\"balance\":\"0x2\",\"bytecode_hash\":null}},\"storages\":{}},~S]"
                                      (+ n 7) q))
                     (request "eth_getProof" (format nil "[~S,[\"0x0\",\"0x1\"],~S]" *pathusd* q)))))

(defun idle-and-lent (backend)
  (let ((tries (tries::backend-tries backend)))
    (sb-thread:with-mutex ((tries::tries-lock tries))
      (list (length (tries::tries-idle tries)) (length (tries::tries-lent tries))))))

(defun run-in-threads (backend jobs threads)
  "Answer JOBS ((index . body) ...) from THREADS threads taking them in turn; returns a vector of
the answers by index."
  (let ((out (make-array (length jobs) :initial-element nil))
        (queue jobs)
        (lock (sb-thread:make-mutex :name "test jobs")))
    (mapc #'sb-thread:join-thread
          (loop repeat threads
                collect (sb-thread:make-thread
                         (lambda ()
                           (loop for job = (sb-thread:with-mutex (lock) (pop queue))
                                 while job
                                 do (setf (svref out (car job)) (answer backend (cdr job)))))
                         :name "trie pool test")))
    out))

(deftest concurrent-trie-calls-answer-as-serial-ones ()
  (with-genesis-base (backend-of)
    (let* ((serial-backend (backend-of 1))
           (bodies (requests serial-backend))
           (serial (mapcar (lambda (body) (answer serial-backend body)) bodies))
           (pooled (backend-of 3))
           ;; 12 threads on 3 states, the requests in reverse so neighbours start together
           (jobs (reverse (loop for body in bodies for i from 0 collect (cons i body))))
           (concurrent (run-in-threads pooled jobs 12)))
      (loop for body in bodies for want in serial for i from 0
            do (is (json:jget want "result") "~A: ~A" body (json:to-json-string want))
               (is (json:json-equal want (svref concurrent i)) "~A:~%  serial ~A~%  concurrent ~A"
                   body (json:to-json-string want) (json:to-json-string (svref concurrent i))))
      ;; every state came back, and the pool never held more than its three
      (destructuring-bind (idle lent) (idle-and-lent pooled)
        (is= 0 lent)
        (is (<= 1 idle 3) "~D idle states" idle)))))

(deftest a-lent-state-holds-up-only-calls-beyond-the-pool ()
  (with-genesis-base (backend-of)
    (let* ((b (backend-of 2))
           (body (request "debug_intermediateRoots"
                          (format nil "[~S]" (octets->hex (chain:block-hash-of-raw (chain:backend-block-raw b 3865))))))
           (want (answer (backend-of 1) body))
           (held (sb-thread:make-semaphore))
           (release (sb-thread:make-semaphore))
           (holders
             (loop for n in '(1000 2000)
                   collect (let ((n n))
                             (sb-thread:make-thread
                              (lambda ()
                                (chain:with-backend-read (b)
                                  (tries:call-with-trie-state
                                   n (lambda (db)
                                       (sb-thread:signal-semaphore held)
                                       (sb-thread:wait-on-semaphore release)
                                       (store:db-number db)))))
                              :name "trie state holder")))))
      (unwind-protect
           (progn
             ;; the second state was lent while the first was held
             (is (sb-thread:wait-on-semaphore held :n 2 :timeout 120) "two states lent at once")
             (let ((third (sb-thread:make-thread (lambda () (answer b body)) :name "third trie call")))
               ;; both states are lent: the third call waits for one
               (sleep 0.5)
               (is (sb-thread:thread-alive-p third) "a third call ran beyond a pool of two")
               (sb-thread:signal-semaphore release)
               (let ((got (sb-thread:join-thread third :timeout 120 :default :timeout)))
                 (is (json:json-equal want got) "~A" got))))
        (sb-thread:signal-semaphore release 2)
        (is= '(1000 2000) (mapcar #'sb-thread:join-thread holders)))
      (is= '(2 0) (idle-and-lent b)))))
