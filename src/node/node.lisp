(defpackage #:clempo.node
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:follow #:clempo.follow) (#:certs #:clempo.follow.certs)
                    (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:backend #:clempo.rpc.chain) (#:server #:clempo.rpcserver)
                    (#:archive #:clempo.archive) (#:store #:clempo.store)
                    (#:executor #:clempo.executor) (#:blocks #:clempo.blocks)
                    (#:trie #:clempo.trie) (#:workers #:clempo.node.workers)
                    (#:headers #:clempo.headers) (#:repl #:clempo.node.repl)
                    (#:eth-source #:clempo.eth-source) (#:history #:clempo.history))
  (:export #:run-node #:main #:*node-backend* #:call-with-block-source))

(in-package #:clempo.node)

(defvar *node-backend* nil "Live backend bound in the node's socket REPL.")
(define-constant +recent-count+ 256)
(define-constant +history-flush-blocks+ 100000)

(defstruct recent-block
  number raw receipts receipts-known-p transaction-hashes)

(defclass node-backend (backend:archive-backend)
  ((store :initarg :store :reader node-store)
   (chain :initarg :chain :reader node-chain)
   (recent :initform (make-array +recent-count+ :initial-element nil) :reader node-recent)
   (server :initform nil :accessor node-server)
   (history-writer :initform nil :accessor node-history-writer)))

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
   :transaction-hashes (mapcar #'clempo.transactions:transaction-hash
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
         (number (headers:header-number header)))
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
               (note-recent node entry)
               (setf (backend:archive-backend-head node) number)))
        (when (store:db-block-open-p db) (store:db-revert-block db))))
    (when (node-server node)
      (server:publish-head (node-server node) number :certificate certificate))))

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

(defun prime-head-block (node head upstream)
  (let* ((number (follow:head-number head))
         (raw (or (backend:backend-block-raw node number)
                  (svref (archive:fetch-blocks number 1 :url upstream :batch 1) 0)))
         (block (blocks:decode-block raw)))
    (unless (and (octets= (follow:head-hash head) (blocks:block-hash block))
                 (null (blocks:block-body-mismatches block)))
      (error "Invalid raw block for the stored head at ~D" number))
    (note-recent node (recent-entry block nil nil))))

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

(defun run-node (datadir &key (upstream (rpc:local-rpc-url))
                           (archive-dir (archive:archive-dir))
                           (rpc-port 8545) (bind "127.0.0.1")
                           certify (cert-rpc rpc:+mainnet-rpc+)
                           repl-port repl-token-file (checkpoint-interval 0)
                           (batch-size 32) (root-workers 4) block-source once seconds)
  "Open a checked mainnet replay datadir, follow it with execution, and serve committed state.
BLOCK-SOURCE optionally provides raw blocks instead of fetching them from UPSTREAM;
UPSTREAM still supplies canonical header commitments.
Returns a JSON report after ONCE or SECONDS; otherwise runs until interrupted."
  (check-type rpc-port (unsigned-byte 16))
  (check-type root-workers (integer 1 16))
  (when (and once seconds) (error "Choose --once or --seconds"))
  (unless (eq (null repl-port) (null repl-token-file))
    (error "--repl-port and --repl-token-file must be supplied together"))
  (let* ((dir (uiop:ensure-directory-pathname datadir))
         (archive-dir (namestring (uiop:ensure-directory-pathname archive-dir)))
         (tip (follow:rpc-head :latest :url upstream))
         (repl-token (when repl-token-file (token-file-secret repl-token-file))))
    (reject-healed-datadir dir)
    (let* ((datastore (trie:call-with-trie-hashing
                       (lambda () (store:open-store dir :interval checkpoint-interval :log #'log-node))))
           (rpc-server nil)
           (repl-server nil)
           (history-writer nil))
      (unwind-protect
           (let* ((head (validate-head (store:store-db datastore) upstream))
                  (node (backend:make-archive-backend :class 'node-backend :store datastore
                                                     :chain :mainnet :head (follow:head-number head)
                                                     :archive-dir archive-dir :upstream upstream))
                  (initial-number (follow:head-number head))
                  (initial-target (if (> initial-number (follow:head-number tip))
                                      (follow:rpc-head :latest :url upstream) tip))
                  (certifier (when certify (certs:make-mainnet-certifier head :upstream upstream
                                                                             :url cert-rpc)))
                  (follower (follow:make-follower head :rpc-url upstream :executor node :certifier certifier
                                                       :archive-dir archive-dir :use-archive t :write-archive t
                                                       :batch-size batch-size :block-source block-source))
                  (started nil)
                  (next-report 0)
                  (max-lag 0)
                  (live-base nil))
             (prime-head-block node head upstream)
             (setf history-writer (start-history-writer node))
             (setf rpc-server (server:start-server :host bind :port rpc-port :backend node)
                   (node-server node) rpc-server)
             (log-node "node: JSON-RPC listening on ~A:~D; stored head ~D, upstream tip ~D" bind
                       (server:server-port rpc-server)
                       initial-number (follow:head-number initial-target))
             (when repl-port
               (setf repl-server
                     (repl:start-repl repl-port repl-token
                                      :configure (lambda (fn)
                                                   (let ((*node-backend* node)) (funcall fn)))))
               (log-node "node: Lisp REPL on 127.0.0.1:~D (token required)" (repl:repl-port repl-server)))
             (let* ((last (follow-with-root-workers
                           root-workers follower :tip initial-target :once once :seconds seconds
                           :on-poll
                           (lambda (follow remote)
                             (let* ((local (follow:follower-head follow))
                                    (height (follow:head-number local))
                                    (lag (- (follow:head-number remote) height))
                                    (now (get-internal-real-time)))
                               (when (and (not started) (zerop lag))
                                 (setf started now live-base height))
                               (when started (setf max-lag (max max-lag lag)))
                               (when (>= now next-report)
                                 (log-node "node: head ~D lag ~D, executed ~D, certified ~D"
                                           height lag (follow:follower-blocks follow)
                                           (follow:follower-certified follow))
                                 (setf next-report (+ now (* 30 internal-time-units-per-second))))))))
                    (elapsed (if started (/ (- (get-internal-real-time) started)
                                             internal-time-units-per-second) 0)))
               (json:jobject "anchor" initial-number "head" (follow:head-number last)
                             "hash" (octets->hex (follow:head-hash last) :prefix t)
                             "stateRoot" (octets->hex (follow:head-state-root last) :prefix t)
                             "executed" (follow:follower-blocks follower)
                             "certified" (follow:follower-certified follower)
                             "archivedChunks" (follow:follower-chunks follower)
                             "liveSeconds" (floor elapsed)
                             "liveBlocks" (if live-base (- (follow:head-number last) live-base) 0)
                             "maxLagAfterCatchup" max-lag)))
        (unwind-protect
             (unwind-protect (when repl-server (repl:stop-repl repl-server))
               (when rpc-server (server:stop-server rpc-server)))
          (unwind-protect
               (when history-writer (history:history-flush history-writer :db (store:store-db datastore)))
            (store:close-store datastore)))))))

(defun call-with-block-source (source function &key peers)
  "Run FUNCTION with the selected raw block transport, or NIL for RPC."
  (cond ((string= source "rpc")
         (when peers (error "A peer list requires the devp2p block source"))
         (funcall function nil))
        ((string= source "devp2p")
         (if peers
             (let ((explicit (eth-source:read-enodes peers)))
               (unless explicit (error "No enodes in ~A" peers))
               (eth-source:call-with-public-block-source
                function :discovery-timeout 0 :bootnodes (append explicit (eth-source:tempo-bootnodes))))
             (eth-source:call-with-public-block-source function)))
        (t (error "Unknown block source ~S; expected rpc or devp2p" source))))

(defun node-usage ()
  (format t "Usage: clempo --datadir DIR [--upstream URL] [--archive DIR]~%
              [--source rpc|devp2p] [--peers FILE]~%
              [--rpc-port N] [--bind IP] [--certify] [--cert-rpc URL]~%
              [--repl-port N --repl-token-file FILE] [--once | --seconds N]~%
              [--batch N] [--checkpoint-interval N] [--root-workers N]~%"))

(defun node-options (words)
  (loop with options = nil
        while words
        for flag = (pop words)
        do (cond ((string= flag "--help") (return-from node-options :help))
                 ((string= flag "--certify") (setf (getf options :certify) t))
                 ((string= flag "--once") (setf (getf options :once) t))
                 (t (let* ((entry (assoc flag '(("--datadir" :datadir) ("--upstream" :upstream)
                                                  ("--source" :source) ("--peers" :peers)
                                                  ("--archive" :archive-dir) ("--rpc-port" :rpc-port t)
                                                  ("--bind" :bind) ("--cert-rpc" :cert-rpc)
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

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  (let ((code
          (handler-case
              (let ((options (node-options argv)))
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
                            :peers peers))
                         0)))
            (sb-sys:interactive-interrupt () 130)
            (error (c) (format *error-output* "~&clempo: ~A~%" c) 1))))
    (finish-output)
    (sb-ext:exit :code code)))
