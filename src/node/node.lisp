(defpackage #:clempo.node
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:follow #:clempo.follow) (#:certs #:clempo.follow.certs)
                    (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:backend #:clempo.rpc.chain) (#:server #:clempo.rpcserver)
                    (#:archive #:clempo.archive) (#:store #:clempo.store)
                    (#:executor #:clempo.executor) (#:blocks #:clempo.blocks)
                    (#:trie #:clempo.trie) (#:workers #:clempo.node.workers)
                    (#:headers #:clempo.headers) (#:repl #:clempo.node.repl)
                    (#:eth-source #:clempo.eth-source) (#:history #:clempo.history)
                    (#:pools #:clempo.pool-service) (#:txpool #:clempo.txpool)
                    (#:metrics #:clempo.metrics) (#:chainspec #:clempo.chainspec)
                    (#:transactions #:clempo.transactions))
  (:export #:run-node #:main #:*node-backend* #:call-with-block-source
           #:node-options #:request-stop))

(in-package #:clempo.node)

(defvar *node-backend* nil "Live backend bound in the node's socket REPL.")
(define-constant +recent-count+ 256)
(define-constant +history-flush-blocks+ 100000)

;;; Metrics (Prometheus, on --metrics-address).

(defmacro defmetric (var name kind help)
  `(defvar ,var (metrics:define-metric ,name ,kind ,help)))

(defmetric *m-head* "clempo_head" :gauge "Highest executed and committed block.")
(defmetric *m-tip* "clempo_upstream_tip" :gauge "Upstream canonical tip at the last poll.")
(defmetric *m-lag* "clempo_lag_blocks" :gauge "Upstream tip minus the local head.")
(defmetric *m-executed* "clempo_blocks_executed_total" :counter "Blocks executed and committed.")
(defmetric *m-txs* "clempo_transactions_executed_total" :counter "Transactions executed.")
(defmetric *m-certified* "clempo_blocks_certified_total" :counter
  "Blocks admitted under a verified finalization certificate.")
(defmetric *m-uncertified* "clempo_blocks_uncertified_total" :counter
  "Blocks admitted below --certify-from (hash-linked and executed only).")
(defmetric *m-exec-seconds* "clempo_block_execution_seconds" :summary
  "Wall time to execute, verify and commit one block.")
(defmetric *m-retries* "clempo_follow_retries_total" :counter
  "Follow steps that failed on upstream or transport errors and were retried.")
(defmetric *m-pool* "clempo_txpool_transactions" :gauge "Pooled transactions by state.")
(defmetric *m-pool-forwarded* "clempo_txpool_forwarded_total" :counter
  "Transactions forwarded upstream, by result.")
(defmetric *m-accounts* "clempo_store_accounts" :gauge "Accounts in the live state.")
(defmetric *m-peers* "clempo_peer_sessions_total" :counter
  "Authenticated devp2p sessions (block-source peers and inbound listener peers).")

;;; The backend.

(defstruct recent-block
  number raw receipts receipts-known-p transaction-hashes)

(defclass node-backend (backend:archive-backend)
  ((store :initarg :store :reader node-store)
   (chain :initarg :chain :reader node-chain)
   (recent :initform (make-array +recent-count+ :initial-element nil) :reader node-recent)
   (server :initform nil :accessor node-server)
   (history-writer :initform nil :accessor node-history-writer)
   (pool :initarg :pool :reader node-pool)
   (archive-dir :initarg :archive-dir :reader node-archive-dir)
   (forwarder :initform nil :accessor node-forwarder)
   (status :initform (list :tip nil :retries 0 :last-error nil) :accessor node-status)
   (follower :initform nil :accessor node-follower)
   (started :initform (clempo.io:now-seconds) :reader node-started)))

(defun node-db (node) (store:store-db (node-store node)))

(defun recent-block (node number)
  (let ((entry (svref (node-recent node) (mod number +recent-count+))))
    (when (and entry (= number (recent-block-number entry))) entry)))

(defun recent-entry (block receipts &optional (known t))
  (make-recent-block
   :number (headers:header-number (blocks:block-header block))
   :raw (or (blocks:block-raw block) (blocks:encode-block block))
   :receipts-known-p known
   :receipts (mapcar #'clempo.receipts:encode-receipt receipts)
   :transaction-hashes (mapcar #'transactions:transaction-hash
                               (blocks:block-transactions block))))

(defun note-recent (node entry)
  (setf (svref (node-recent node) (mod (recent-block-number entry) +recent-count+)) entry))

(defmethod backend:backend-head ((node node-backend))
  (store:db-number (node-db node)))

(defmethod backend:backend-block-raw ((node node-backend) number)
  (let ((entry (recent-block node number)))
    (if entry (recent-block-raw entry) (call-next-method))))

(defmethod backend:backend-receipts-raw ((node node-backend) number)
  (let ((entry (recent-block node number)))
    (if (and entry (recent-block-receipts-known-p entry))
        (recent-block-receipts entry)
        (call-next-method))))

(defmethod backend:backend-block-number ((node node-backend) hash)
  (let* ((db (node-db node)) (head (store:db-number db)))
    (or (loop for n downfrom head to (max 0 (- head (1- +recent-count+)))
              for h = (store:db-block-hash db n)
              when (and h (octets= h hash)) return n)
        (call-next-method))))

(defmethod backend:backend-tx-location ((node node-backend) hash)
  (loop for entry across (node-recent node)
        for index = (and entry (position hash (recent-block-transaction-hashes entry) :test #'octets=))
        when index do (return-from backend:backend-tx-location (values (recent-block-number entry) index)))
  (call-next-method))

(defmethod backend:backend-state ((node node-backend) number)
  (let ((db (node-db node)))
    (if (= number (store:db-number db)) db (call-next-method))))

(defmethod follow:apply-follow-block ((node node-backend) block &key certificate)
  (let* ((datastore (node-store node))
         (db (store:store-db datastore))
         (header (blocks:block-header block))
         (number (headers:header-number header))
         (start (clempo.io:monotonic-ticks)))
    (backend:with-backend-write (node)
      (unwind-protect
           (let* ((result (executor:execute-block db block :chain (node-chain node)))
                  (fields (executor:block-mismatches result header)))
             (when fields
               (destructuring-bind (field expected actual) (first fields)
                 (error 'executor:block-mismatch :number number :field field :fields fields
                                                 :expected expected :actual actual :result result)))
             (let ((entry (recent-entry block (executor:block-result-receipts result))))
               (let ((diff (store:db-commit-block db number (blocks:block-hash block)))
                     (h (node-history-writer node)))
                 (store:store-commit-block datastore diff)
                 (when h
                   (history:history-record-block h db diff)
                   (when (zerop (mod number +history-flush-blocks+))
                     (history:history-flush h :db db))))
               ;; Receipts are root-checked above (BLOCK-MISMATCHES), so they are kept.
               (backend:backend-record-receipts node number (recent-block-receipts entry))
               (note-recent node entry)
               (setf (backend:archive-backend-head node) number)
               (pools:pool-service-block (node-pool node) db header)))
        (when (store:db-block-open-p db) (store:db-revert-block db))))
    (metrics:metric-set *m-head* number)
    (let ((tip (getf (node-status node) :tip)))
      (when tip (metrics:metric-set *m-lag* (max 0 (- tip number)))))
    (metrics:metric-inc *m-executed*)
    (metrics:metric-inc *m-txs* :by (length (blocks:block-transactions block)))
    (metrics:metric-observe *m-exec-seconds* (/ (- (clempo.io:monotonic-ticks) start)
                                                 internal-time-units-per-second 1.0))
    (when (node-server node)
      (server:publish-head (node-server node) number :certificate certificate))))

;;; The transaction pool: admitted locally against the head state, then forwarded to the
;;; upstream (a follower has no gossip of its own), as reth's RPC forwards raw transactions
;;; to its sequencer. A transaction the upstream refuses leaves the local pool.

(defstruct (forwarder (:constructor %make-forwarder (url pool)))
  url pool thread (queue '()) (stopped nil)
  (lock (sb-thread:make-mutex :name "tx forwarder"))
  (ready (sb-thread:make-waitqueue :name "tx forwarder")))

(defun forward-one (forwarder raw hash)
  (handler-case
      (progn (rpc:rpc-call "eth_sendRawTransaction" (list (octets->hex raw :prefix t))
                           :url (forwarder-url forwarder) :retries 2)
             (metrics:metric-inc *m-pool-forwarded* :labels '(("result" . "accepted"))))
    (rpc:rpc-error (e)
      (let ((message (princ-to-string e)))
        (cond ((search "already known" message)
               (metrics:metric-inc *m-pool-forwarded* :labels '(("result" . "known"))))
              (t (metrics:metric-inc *m-pool-forwarded* :labels '(("result" . "rejected")))
                 (log-node "node: upstream refused ~A: ~A; dropped from the pool"
                           (octets->hex hash :prefix t) message)
                 (pools:pool-service-remove (forwarder-pool forwarder) hash)))))
    (error (e)
      (metrics:metric-inc *m-pool-forwarded* :labels '(("result" . "error")))
      (log-node "node: forwarding ~A failed: ~A" (octets->hex hash :prefix t) e))))

(defun start-forwarder (url pool)
  (let ((f (%make-forwarder url pool)))
    (setf (forwarder-thread f)
          (sb-thread:make-thread
           (lambda ()
             (loop (let ((job (sb-thread:with-mutex ((forwarder-lock f))
                                (loop until (or (forwarder-queue f) (forwarder-stopped f))
                                      do (sb-thread:condition-wait (forwarder-ready f) (forwarder-lock f)))
                                (pop (forwarder-queue f)))))
                     (unless job (return))
                     (forward-one f (car job) (cdr job)))))
           :name "tx forwarder"))
    f))

(defun forward (forwarder raw hash)
  (sb-thread:with-mutex ((forwarder-lock forwarder))
    (unless (forwarder-stopped forwarder)
      (setf (forwarder-queue forwarder) (nconc (forwarder-queue forwarder) (list (cons raw hash))))
      (sb-thread:condition-notify (forwarder-ready forwarder)))))

(defun stop-forwarder (forwarder)
  "Forward what is queued, then stop."
  (sb-thread:with-mutex ((forwarder-lock forwarder))
    (setf (forwarder-stopped forwarder) t)
    (sb-thread:condition-broadcast (forwarder-ready forwarder)))
  (sb-thread:join-thread (forwarder-thread forwarder) :default nil))

(defun note-pool-metrics (node)
  (let ((all (pools:pool-service-transactions (node-pool node))))
    (metrics:metric-set *m-pool* (count-if #'third all) :labels '(("state" . "pending")))
    (metrics:metric-set *m-pool* (count-if-not #'third all) :labels '(("state" . "queued")))))

(defmethod backend:backend-submit-raw-transaction ((node node-backend) raw)
  (let ((tx (transactions:decode-transaction raw)))
    (let ((hash (pools:pool-service-submit (node-pool node) tx (node-db node))))
      (when (node-forwarder node) (forward (node-forwarder node) raw hash))
      (note-pool-metrics node)
      hash)))

(defmethod backend:backend-pool-transactions ((node node-backend))
  (pools:pool-service-transactions (node-pool node)))

(defmethod backend:backend-clear-pool ((node node-backend))
  (prog1 (pools:pool-service-clear (node-pool node)) (note-pool-metrics node)))

;;; clempo_status: the node's own view, as goempo's status command reports it.

(clempo.rpc:defrpc "clempo_status" () "the follower's head, upstream tip, certification and pool"
  (let ((node backend:*backend*))
    (if (typep node 'node-backend)
        (let* ((db (node-db node))
               (status (node-status node))
               (follower (node-follower node))
               (tip (getf status :tip))
               (pool (pools:pool-service-transactions (node-pool node))))
          (json:jobject
           "chain" (string-downcase (node-chain node))
           "head" (json:jobject "number" (quantity->hex (store:db-number db))
                                "hash" (octets->hex (store:db-hash db) :prefix t)
                                "stateRoot" (octets->hex (store:db-root-hash db) :prefix t))
           "upstreamTip" (if tip (quantity->hex tip) :null)
           "lag" (if tip (max 0 (- tip (store:db-number db))) :null)
           "executed" (if follower (follow:follower-blocks follower) 0)
           "certified" (if follower (follow:follower-certified follower) 0)
           "retries" (getf status :retries)
           "lastError" (or (getf status :last-error) :null)
           "txpool" (json:jobject "pending" (count-if #'third pool) "queued" (count-if-not #'third pool))
           "uptimeSeconds" (- (clempo.io:now-seconds) (node-started node))))
        (clempo.rpc:rpc-fail -32601 "the method clempo_status does not exist/is not available"))))

;;; Opening a datadir.

(defun reject-healed-datadir (datadir)
  (let ((path (merge-pathnames "replay.sexp" datadir)))
    (when (probe-file path)
      (let* ((*read-eval* nil)
             (history (read-from-string (clempo.io:read-file-string path))))
        (when (or (getf history :heals) (getf history :skips))
          (error "Refusing ~A: it records healed or skipped blocks; use a verified replay" datadir))))))

(defun validate-head (db upstream)
  (let ((head (follow:rpc-head (store:db-number db) :url upstream)))
    (unless (octets= (store:db-hash db) (follow:head-hash head))
      (error "Stored block ~D does not match the upstream canonical hash" (store:db-number db)))
    (unless (octets= (store:db-root-hash db) (follow:head-state-root head))
      (error 'store:store-root-mismatch :number (store:db-number db)
             :expected (follow:head-state-root head) :actual (store:db-root-hash db)))
    head))

(defun head-block (node head upstream)
  (let* ((number (follow:head-number head))
         (raw (or (backend:backend-block-raw node number)
                  (svref (archive:fetch-blocks number 1 :url upstream :batch 1) 0)))
         (block (blocks:decode-block raw)))
    (unless (and (octets= (follow:head-hash head) (blocks:block-hash block))
                 (null (blocks:block-body-mismatches block)))
      (error "Invalid raw block for the stored head at ~D" number))
    block))

(defun token-file-secret (file)
  (let* ((path (namestring (truename file)))
         (stat (sb-posix:stat path)))
    (unless (and (= (logand sb-posix:s-ifmt (sb-posix:stat-mode stat)) sb-posix:s-ifreg)
                 (= (sb-posix:geteuid) (sb-posix:stat-uid stat))
                 (zerop (logand #o077 (sb-posix:stat-mode stat))))
      (error "REPL token file must be a regular file owned by this user and private (chmod 600)"))
    (string-right-trim '(#\Newline #\Return) (clempo.io:read-file-string path))))

(defun log-node (fmt &rest args)
  (format *error-output* "~&~?~%" fmt args)
  (force-output *error-output*))

(defun follow-with-root-workers (threads follower &rest options)
  ;; The follow thread recovers senders; RPC threads keep the fresh-arena fallback.
  (let ((clempo.secp256k1:*recovery-scratch* (clempo.secp256k1:make-recovery-scratch))
        (clempo.p256:*p256-scratch* (clempo.p256:make-p256-scratch)))
    (workers:call-with-workers
     (lambda (mapper)
       (trie:call-with-trie-hashing (lambda () (apply #'follow:follow-live follower options))
                                    :parallel-map mapper))
     :threads threads :name "trie subtree hash")))

(defun start-history-writer (node)
  "Append followed blocks to the backend's history when it ends exactly at the stored head;
a gap would make later versions shadow unrecorded ones. Returns the history or NIL."
  (let* ((h (backend:backend-history node))
         (db (node-db node))
         (number (store:db-number db)))
    (cond ((null h) nil)
          ((eql (history:history-height h) number)
           (history:history-start-writer h (1+ number) db)
           (setf (node-history-writer node) h)
           (log-node "node: appending followed blocks to history ~A from ~D"
                     (history:history-dir h) (1+ number))
           h)
          (t (log-node "node: history ~A ends at ~A, not the stored head ~D; not appending"
                       (history:history-dir h) (history:history-height h) number)
             nil))))

;;; The EVM compiler thread: hot contracts compile here, never on a block or RPC thread.

(defstruct (jit-worker (:constructor %make-jit-worker ()))
  thread (queue '()) (stopped nil)
  (lock (sb-thread:make-mutex :name "jit queue"))
  (ready (sb-thread:make-waitqueue :name "jit queue")))

(defun start-jit-worker ()
  (let ((w (%make-jit-worker)))
    (setf (jit-worker-thread w)
          (sb-thread:make-thread
           (lambda ()
             (loop (let ((thunk (sb-thread:with-mutex ((jit-worker-lock w))
                                  (loop until (or (jit-worker-queue w) (jit-worker-stopped w))
                                        do (sb-thread:condition-wait (jit-worker-ready w) (jit-worker-lock w)))
                                  (pop (jit-worker-queue w)))))
                     (unless thunk (return))
                     (funcall (the function thunk)))))
           :name "evm compiler"))
    (setf clempo.evm:*jit-compile-hook*
     (lambda (thunk)
       (sb-thread:with-mutex ((jit-worker-lock w))
         (unless (jit-worker-stopped w)
           (setf (jit-worker-queue w) (nconc (jit-worker-queue w) (list thunk)))
           (sb-thread:condition-notify (jit-worker-ready w))
           t))))
    (clempo.evm:enable-jit)
    w))

(defun stop-jit-worker (w)
  (clempo.evm:disable-jit)
  (setf clempo.evm:*jit-compile-hook* nil)
  (sb-thread:with-mutex ((jit-worker-lock w))
    (setf (jit-worker-stopped w) t (jit-worker-queue w) '())
    (sb-thread:condition-broadcast (jit-worker-ready w)))
  (sb-thread:join-thread (jit-worker-thread w) :default nil))

;;; Serving the chain to devp2p peers (--p2p-listen): only local data, never the upstream.

(defun node-key (dir)
  "The node's secp256k1 key from DIR/p2p.key, created once (exclusively, mode 600)."
  (let ((path (namestring (merge-pathnames "p2p.key" dir))))
    (unless (probe-file path)
      (let ((key (1+ (clempo.io:random-below (1- clempo.secp256k1:+secp256k1-n+)))))
        (handler-case
            (let ((fd (sb-posix:open path (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl)
                                     #o600)))
              (unwind-protect
                   (let ((text (string->octets (format nil "~64,'0X~%" key))))
                     (sb-posix:write fd (sb-sys:vector-sap text) (length text))
                     (sb-posix:fsync fd))
                (sb-posix:close fd)))
          (sb-posix:syscall-error (e)
            (unless (probe-file path) (error e))))))
    (let ((stat (sb-posix:stat path)))
      (unless (zerop (logand #o077 (sb-posix:stat-mode stat)))
        (error "~A must be private (chmod 600)" path)))
    (parse-integer (string-trim '(#\Newline #\Space) (clempo.io:read-file-string path)) :radix 16)))

(defun local-block-raw (node height)
  (let ((entry (recent-block node height)))
    (cond (entry (recent-block-raw entry))
          ((archive:chunk-present-p (* archive:+chunk-blocks+ (floor height archive:+chunk-blocks+))
                                    (node-archive-dir node))
           (archive:raw-block height (node-archive-dir node))))))

(defun eth-source (node)
  (lambda (kind key)
    (ecase kind
      (:number (let* ((db (node-db node)) (head (store:db-number db)))
                 (loop for n downfrom head to (max 0 (- head (1- +recent-count+)))
                       for h = (store:db-block-hash db n)
                       when (and h (octets= h key)) return n)))
      (:block (and (<= key (store:db-number (node-db node))) (local-block-raw node key)))
      (:receipts (let ((entry (recent-block node key)))
                   (if (and entry (recent-block-receipts-known-p entry))
                       (values (recent-block-receipts entry) t)
                       (values nil nil)))))))

(defun eth-status (node)
  (lambda ()
    (let* ((db (node-db node)) (head (store:db-number db))
           (block (blocks:decode-block (local-block-raw node head))))
      (values (if (archive:chunk-present-p 0 (node-archive-dir node))
                  0 (max 0 (- head (1- +recent-count+))))
              head (store:db-hash db)
              (headers:ethereum-header-timestamp (headers:header-inner (blocks:block-header block)))))))

(defun call-with-p2p-listener (node address dir chain thunk)
  (if (null address)
      (funcall thunk)
      (multiple-value-bind (host port) (parse-host-port address)
        (progn
          (clempo.eth:call-with-eth-listener
           host port (node-key dir) (eth-source node) (eth-status node)
           (lambda (bound)
             (log-node "node: devp2p listener on ~A:~D" host bound)
             (funcall thunk))
           :chain (clempo.eth:tempo-eth-chain chain)
           :on-peer (lambda (peer version)
                      (declare (ignore peer version))
                      (metrics:metric-inc *m-peers* :labels '(("role" . "inbound"))))
           :on-error (lambda (c) (log-node "node: devp2p: ~A" c)))))))

;;; Stopping: SIGTERM and SIGINT ask the follow loop to return; RUN-NODE then shuts down.

(defvar *stop-requested* nil)

(defun request-stop () (setf *stop-requested* t))

(defun call-with-stop-signals (thunk)
  (let ((old-term (sb-sys:enable-interrupt sb-unix:sigterm
                                           (lambda (&rest args) (declare (ignore args)) (request-stop))))
        (old-int (sb-sys:enable-interrupt sb-unix:sigint
                                          (lambda (&rest args) (declare (ignore args)) (request-stop)))))
    (unwind-protect (funcall thunk)
      (sb-sys:enable-interrupt sb-unix:sigterm (or old-term #'sb-unix::sigterm-handler))
      (sb-sys:enable-interrupt sb-unix:sigint (or old-int #'sb-unix::sigint-handler)))))

;;; Datadir-scoped caches.

(defun datadir-subdir (dir name)
  (namestring (merge-pathnames (make-pathname :directory (list :relative name)) dir)))

(defun chain-keyword (name)
  (cond ((member name '("mainnet" :mainnet) :test #'equal) :mainnet)
        ((member name '("moderato" :moderato) :test #'equal) :moderato)
        (t (error "Unknown chain ~S; expected mainnet or moderato" name))))

(defun default-upstream (chain)
  (ecase chain (:mainnet (rpc:local-rpc-url)) (:moderato rpc:+moderato-rpc+)))

(defun parse-host-port (text)
  (let ((colon (position #\: text :from-end t)))
    (unless colon (error "Expected HOST:PORT, got ~A" text))
    (values (subseq text 0 colon) (parse-integer text :start (1+ colon)))))

(defun run-node (datadir &key (chain :mainnet) upstream archive-dir
                           index-dir receipts-dir history-dir finalization-dir
                           (rpc-port 8545) (bind "127.0.0.1")
                           (certify t) certify-from cert-rpc
                           repl-port repl-token-file (checkpoint-interval 0)
                           (batch-size 32) (root-workers 4) block-source once seconds
                           metrics-address p2p-listen (jit t) (forward t) (poll-seconds 0.25))
  "Open DATADIR (initialized with CHAIN's genesis when empty), follow the chain with
execution and, unless CERTIFY is NIL, finalization certificates, and serve committed state.
Caches (hash index, receipts, history, finalizations) live under DATADIR unless a directory
is given. BLOCK-SOURCE optionally provides raw blocks instead of fetching them from UPSTREAM;
UPSTREAM still supplies canonical header commitments. Returns a JSON report after ONCE,
SECONDS or a stop request (SIGTERM, SIGINT); upstream failures are retried."
  (check-type rpc-port (unsigned-byte 16))
  (check-type root-workers (integer 1 16))
  (when (and once seconds) (error "Choose --once or --seconds"))
  (unless (eq (null repl-port) (null repl-token-file))
    (error "--repl-port and --repl-token-file must be supplied together"))
  (setf *stop-requested* nil)
  (let* ((chain (chain-keyword chain))
         (name (string-downcase chain))
         (upstream (or upstream (default-upstream chain)))
         (dir (uiop:ensure-directory-pathname datadir))
         (archive-dir (namestring (uiop:ensure-directory-pathname
                                   (or archive-dir (archive:archive-dir name)))))
         (repl-token (when repl-token-file (token-file-secret repl-token-file)))
         (executor:*chain* chain))
    (reject-healed-datadir dir)
    (let* ((datastore (trie:call-with-trie-hashing
                       (lambda () (store:open-store
                                   dir :interval checkpoint-interval :log #'log-node
                                       :genesis (lambda (db)
                                                  (log-node "node: empty datadir; loading the ~A genesis" name)
                                                  (executor:load-genesis db :chain chain))))))
           (rpc-server nil) (repl-server nil) (metrics-server nil) (jit-worker nil)
           (forwarder nil) (history-writer nil))
      (unwind-protect
           (let* ((db (store:store-db datastore))
                  (head (validate-head db upstream))
                  (pool (pools:make-pool-service :chain chain))
                  (node (backend:make-archive-backend
                         :class 'node-backend :store datastore :chain chain :pool pool
                         :head (follow:head-number head) :archive-dir archive-dir :upstream upstream
                         :index-dir (or index-dir (datadir-subdir dir "index"))
                         :receipts-dir (or receipts-dir (datadir-subdir dir "receipts"))
                         :history-dir (or history-dir (datadir-subdir dir "history"))
                         :finalization-dir (or finalization-dir (datadir-subdir dir "finalizations"))))
                  (initial-number (follow:head-number head))
                  (block (head-block node head upstream))
                  (certifier (when certify
                               (certs:make-certifier
                                head :chain chain :upstream upstream
                                     :url (or cert-rpc (ecase chain (:mainnet rpc:+mainnet-rpc+)
                                                         (:moderato rpc:+moderato-rpc+)))
                                     :from (or certify-from (certs:certify-from chain))
                                     :trusted-hash (lambda (n) (store:db-block-hash db n)))))
                  (follower (follow:make-follower head :rpc-url upstream :executor node :certifier certifier
                                                       :archive-dir archive-dir :use-archive t :write-archive t
                                                       :batch-size batch-size :block-source block-source))
                  (started nil) (next-report 0) (max-lag 0) (live-base nil))
             (note-recent node (recent-entry block nil nil))
             (pools:pool-service-block pool db (blocks:block-header block))
             (setf (node-follower node) follower)
             (metrics:metric-set *m-head* initial-number)
             (metrics:metric-set *m-accounts* (store:db-account-count db))
             (when certifier
               (log-node "node: certifying from block ~D; first epoch identity from ~(~A~)"
                         (certs::certifier-from certifier) (certs:certifier-anchor certifier)))
             (setf history-writer (start-history-writer node))
             (when jit (setf jit-worker (start-jit-worker)))
             (when forward (setf forwarder (start-forwarder upstream pool)
                                 (node-forwarder node) forwarder))
             (setf rpc-server (server:start-server :host bind :port rpc-port :backend node)
                   (node-server node) rpc-server)
             (log-node "node: ~A JSON-RPC listening on ~A:~D; stored head ~D" name bind
                       (server:server-port rpc-server) initial-number)
             (when metrics-address
               (multiple-value-bind (host port) (parse-host-port metrics-address)
                 (setf metrics-server (metrics:start-metrics-server host port))
                 (log-node "node: metrics on ~A:~D/metrics" host (metrics:metrics-server-port metrics-server))))
             (when repl-port
               (setf repl-server
                     (repl:start-repl repl-port repl-token
                                      :configure (lambda (fn)
                                                   (let ((*node-backend* node)) (funcall fn)))))
               (log-node "node: Lisp REPL on 127.0.0.1:~D (token required)" (repl:repl-port repl-server)))
             (let* ((last (call-with-p2p-listener
                           node p2p-listen dir chain
                           (lambda ()
                             (call-with-stop-signals
                           (lambda ()
                             (follow-with-root-workers
                              root-workers follower :once once :seconds seconds :poll-seconds poll-seconds
                              :stop (lambda () *stop-requested*)
                              :on-retry
                              (lambda (attempt condition)
                                (metrics:metric-inc *m-retries*)
                                (setf (getf (node-status node) :retries) (1+ (getf (node-status node) :retries))
                                      (getf (node-status node) :last-error) (princ-to-string condition))
                                (log-node "node: retry ~D: ~A" attempt condition))
                              :on-target
                              (lambda (follow remote)
                                (declare (ignore follow))
                                (setf (getf (node-status node) :tip) (follow:head-number remote))
                                (metrics:metric-set *m-tip* (follow:head-number remote)))
                              :on-poll
                              (lambda (follow remote)
                                (let* ((local (follow:follower-head follow))
                                       (height (follow:head-number local))
                                       (tip (follow:head-number remote))
                                       (lag (- tip height))
                                       (now (clempo.io:monotonic-ticks)))
                                  (setf (getf (node-status node) :tip) tip)
                                  (metrics:metric-set *m-tip* tip)
                                  (metrics:metric-set *m-lag* (max 0 lag))
                                  (metrics:metric-set *m-certified* (follow:follower-certified follow))
                                  (when certifier
                                    (metrics:metric-set *m-uncertified* (certs:certifier-uncertified certifier)))
                                  (metrics:metric-set *m-accounts* (store:db-account-count (node-db node)))
                                  (when (and (not started) (<= lag 0))
                                    (setf started now live-base height))
                                  (when started (setf max-lag (max max-lag lag)))
                                  (when (>= now next-report)
                                    (log-node "node: head ~D lag ~D, executed ~D, certified ~D"
                                              height lag (follow:follower-blocks follow)
                                              (follow:follower-certified follow))
                                    (setf next-report (+ now (* 30 internal-time-units-per-second))))))))))))
                    (elapsed (if started (/ (- (clempo.io:monotonic-ticks) started)
                                             internal-time-units-per-second) 0)))
               (json:jobject "chain" name "anchor" initial-number "head" (follow:head-number last)
                             "hash" (octets->hex (follow:head-hash last) :prefix t)
                             "stateRoot" (octets->hex (follow:head-state-root last) :prefix t)
                             "executed" (follow:follower-blocks follower)
                             "certified" (follow:follower-certified follower)
                             "uncertified" (if certifier (certs:certifier-uncertified certifier) 0)
                             "archivedChunks" (follow:follower-chunks follower)
                             "stopped" (if *stop-requested* t :false)
                             "liveSeconds" (floor elapsed)
                             "liveBlocks" (if live-base (- (follow:head-number last) live-base) 0)
                             "maxLagAfterCatchup" max-lag)))
        ;; Shutdown, in order: sessions that read the store, then the threads that feed or
        ;; serve it, then the history and the store itself.
        (shutdown-steps
         (lambda () (when repl-server (repl:stop-repl repl-server)))
         (lambda () (when rpc-server (server:stop-server rpc-server)))
         (lambda () (when metrics-server (metrics:stop-metrics-server metrics-server)))
         (lambda () (when forwarder (stop-forwarder forwarder)))
         (lambda () (when jit-worker (stop-jit-worker jit-worker)))
         (lambda () (when history-writer
                      (history:history-flush history-writer :db (store:store-db datastore))))
         (lambda () (store:close-store datastore)))))))

(defun shutdown-steps (&rest steps)
  "Run every step even when an earlier one fails; then signal the first failure."
  (let ((failure nil))
    (dolist (step steps)
      (handler-case (funcall step)
        (error (c) (log-node "node: shutdown: ~A" c) (unless failure (setf failure c)))))
    (when failure (error failure))))

(defun call-with-block-source (source function &key peers (chain :mainnet))
  "Run FUNCTION with the selected raw block transport, or NIL for RPC."
  (cond ((string= source "rpc")
         (when peers (error "A peer list requires the devp2p block source"))
         (funcall function nil))
        ((string= source "devp2p")
         (flet ((on-peer (peer version)
                  (declare (ignore peer version))
                  (metrics:metric-inc *m-peers* :labels '(("role" . "source")))))
           (if peers
               (let ((explicit (eth-source:read-enodes peers)))
                 (unless explicit (error "No enodes in ~A" peers))
                 (eth-source:call-with-public-block-source
                  function :chain chain :discovery-timeout 0 :on-peer #'on-peer
                           :bootnodes (append explicit (eth-source:tempo-bootnodes chain))))
               (eth-source:call-with-public-block-source function :chain chain :on-peer #'on-peer))))
        (t (error "Unknown block source ~S; expected rpc or devp2p" source))))

(defun node-usage ()
  (format t "Usage: clempo --datadir DIR [--chain mainnet|moderato] [--upstream URL]~%
              [--archive DIR] [--index-dir DIR] [--receipts-dir DIR] [--history-dir DIR]~%
              [--finalization-dir DIR] [--source rpc|devp2p] [--peers FILE]~%
              [--rpc-port N] [--bind IP] [--metrics-address HOST:PORT] [--p2p-listen HOST:PORT]~%
              [--no-certify] [--certify-from N] [--cert-rpc URL] [--no-jit] [--no-forward]~%
              [--repl-port N --repl-token-file FILE] [--once | --seconds N]~%
              [--batch N] [--checkpoint-interval N] [--root-workers N]~%
       clempo keys ...        (clempo keys --help)~%
An empty datadir starts at the chain's genesis. Caches live under the datadir unless a~%
directory flag shares one. SIGTERM or SIGINT stops cleanly.~%"))

(defun node-options (words)
  (loop with options = nil
        while words
        for flag = (pop words)
        do (cond ((string= flag "--help") (return-from node-options :help))
                 ((string= flag "--certify") (setf (getf options :certify) t))
                 ((string= flag "--no-certify") (setf (getf options :certify) nil))
                 ((string= flag "--no-jit") (setf (getf options :jit) nil))
                 ((string= flag "--no-forward") (setf (getf options :forward) nil))
                 ((string= flag "--once") (setf (getf options :once) t))
                 (t (let* ((entry (assoc flag '(("--datadir" :datadir) ("--chain" :chain)
                                                  ("--upstream" :upstream)
                                                  ("--source" :source) ("--peers" :peers)
                                                  ("--archive" :archive-dir) ("--index-dir" :index-dir)
                                                  ("--receipts-dir" :receipts-dir)
                                                  ("--history-dir" :history-dir)
                                                  ("--finalization-dir" :finalization-dir)
                                                  ("--rpc-port" :rpc-port t)
                                                  ("--bind" :bind) ("--cert-rpc" :cert-rpc)
                                                  ("--certify-from" :certify-from t)
                                                  ("--metrics-address" :metrics-address) ("--p2p-listen" :p2p-listen)
                                                  ("--repl-port" :repl-port t) ("--repl-token-file" :repl-token-file)
                                                  ("--seconds" :seconds t) ("--batch" :batch-size t)
                                                  ("--root-workers" :root-workers t)
                                                  ("--checkpoint-interval" :checkpoint-interval t))
                                         :test #'string=))
                           (value (pop words)))
                      (unless entry (error "Unknown option ~A" flag))
                      (unless value (error "Missing value for ~A" flag))
                      (setf (getf options (second entry)) (if (third entry) (parse-integer value) value)))))
        finally (return options)))

(defun run-main (argv)
  (cond ((equal (first argv) "keys") (clempo.validator:keys-command (rest argv)))
        (t (let ((options (node-options argv)))
             (cond ((eq options :help) (node-usage) 0)
                   ((not (getf options :datadir)) (node-usage) 2)
                   (t (let ((dir (getf options :datadir))
                            (source (getf options :source "rpc"))
                            (peers (getf options :peers)))
                        (remf options :datadir)
                        (remf options :source)
                        (remf options :peers)
                        (call-with-block-source
                         source
                         (lambda (fetch)
                           (write-line (json:to-json-string
                                        (apply #'run-node dir :block-source fetch options))))
                         :peers peers :chain (chain-keyword (getf options :chain :mainnet))))
                      0))))))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  (let ((code (handler-case (run-main argv)
                (sb-sys:interactive-interrupt () 130)
                (error (c) (format *error-output* "~&clempo: ~A~%" c) 1))))
    (finish-output)
    (sb-ext:exit :code code)))
