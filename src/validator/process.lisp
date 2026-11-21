(in-package #:clempo.validator)

;;; `clempo validator`: a Tempo validator process. Its datadir holds the state store, the
;;; finalized block log, the Simplex vote journal and the DKG state; restarting on it resumes
;;; where it stopped (re-executing logged blocks the state store had not synced), and the
;;; journals keep it from signing anything that conflicts with what it signed before.

(defvar *stop-requested* nil "Set by SIGTERM/SIGINT; the event loop stops after its turn.")

(defun datadir-path (datadir name)
  (merge-pathnames name (uiop:ensure-directory-pathname datadir)))

(defun genesis-block-raw (chain db)
  (let ((header (clempo.headers:decode-header
                 (clempo.chainspec:genesis-header-rlp chain :state-root (clempo.store:db-root-hash db)))))
    (clempo.blocks:encode-block
     (clempo.blocks:make-block
      :header header
      :withdrawals-present-p (and (clempo.headers:ethereum-header-withdrawals-root
                                   (clempo.headers:header-inner header))
                                  t)))))

(defun recover-state (chain store log reporter)
  "Bring the state store up to the block log and return the head header: on a fresh datadir,
log the genesis block; after a restart, re-execute the logged blocks the state store had not
synced."
  (let ((db (clempo.store:store-db store)))
    (when (minusp (block-log-head log))
      (invariant (zerop (clempo.store:db-number db)) "a state store without a block log")
      (block-log-append log 0 (genesis-block-raw chain db) nil nil))
    (let ((stored (clempo.store:db-number db)) (logged (block-log-head log)))
      (invariant (<= stored logged) "the state store (~D) is ahead of the block log (~D)" stored logged)
      (when (< stored logged)
        (funcall reporter (json:jobject "event" "replaying" "from" (1+ stored) "to" logged)))
      (loop for height from (1+ stored) to logged
            for block = (clempo.blocks:decode-block (log-record-block (block-log-record log height)))
            do (let* ((result (clempo.executor:execute-block db block :chain chain))
                      (mismatches (clempo.executor:block-mismatches result (clempo.blocks:block-header block))))
                 (when mismatches
                   (error "logged block ~D re-executes differently (~{~A~^, ~})" height (mapcar #'first mismatches)))
                 (clempo.store:store-commit-block
                  store (clempo.store:db-commit-block db height (clempo.blocks:block-hash block)))))
      (clempo.store:store-sync store)
      (clempo.blocks:block-header (clempo.blocks:decode-block (log-record-block (block-log-record log logged)))))))

(defun logged-dkg-block (log height)
  (let ((raw (log-record-block (block-log-record log height))))
    (dkg-block-from-header (clempo.blocks:block-header (clempo.blocks:decode-block raw))
                           (clempo.rpc.chain:block-hash-of-raw raw))))

(defun load-finalized-headers (app)
  "The DKG actor reads the finalized blocks of the current and previous epoch."
  (let* ((log (validator-app-log app))
         (head (head-number app))
         (length (validator-app-epoch-length app))
         (epoch (clempo.dkg:height-epoch (1+ head) length))
         (from (max 0 (1- (* (max 0 (1- epoch)) length)))))
    (setf (gethash 0 (validator-app-finalized-headers app)) (logged-dkg-block log 0))
    (loop for height from from to head
          do (setf (gethash height (validator-app-finalized-headers app)) (logged-dkg-block log height)))))

(defun open-consensus (app share datadir)
  "Start the DKG actor on the datadir's DKG state and enter the current epoch."
  (let ((chain (validator-app-chain app)))
    (multiple-value-bind (dkg actions)
        (clempo.dkg:open-dkg-manager
         :seed (validator-app-seed app)
         :store (make-file-dkg-store (datadir-path datadir "dkg/"))
         :epoch-length (validator-app-epoch-length app)
         :t12-time (clempo.chainspec:fork-activation :t12 chain)
         :t15-time (clempo.chainspec:fork-activation :t15 chain)
         :header-at (lambda (height) (gethash height (validator-app-finalized-headers app)))
         :last-finalized-height (head-number app) :initial-share share)
      (setf (validator-app-dkg app) dkg)
      (let* ((enter (find :enter actions :key #'first))
             (boundary (boundary-dkg-block app (second enter))))
        (invariant boundary "no finalized boundary for epoch ~D" (second enter))
        (apply-dkg-actions app actions (clempo.dkg:decode-onchain-dkg-outcome
                                        (clempo.dkg:dkg-block-extra-data boundary)))))))

(defun open-validator-app (&key chain datadir seed share network (fee-recipient 0) timeouts
                                (backfill-frequency 8) (reporter (constantly nil)) before-consensus)
  "Open (or resume) the validator on DATADIR for CHAIN, a chain spec, with ed25519 SEED on
NETWORK and enter the current epoch. BEFORE-CONSENSUS, when given, is called with the app
before it enters its epoch. CLOSE-VALIDATOR-APP closes the datadir."
  (let* ((length (clempo.chainspec:chain-epoch-length chain))
         (store (clempo.store:open-store (datadir-path datadir "state/")
                                         :genesis (lambda (db) (clempo.executor:load-genesis db :chain chain))))
         (log nil) (app nil))
    (unwind-protect
         (progn
           (invariant (> length 1) "the chain's epoch length must exceed 1")
           (setf log (open-block-log (datadir-path datadir "blocks/")))
           (let ((head (recover-state chain store log reporter)))
             (setf app (%make-app :network network :store store :db (clempo.store:store-db store)
                                  :chain chain :seed seed :key (clempo.ed25519:ed25519-public-key seed)
                                  :epoch-length length :head head :log log
                                  :backend (make-instance 'validator-backend :chain chain :log log :store store)
                                  :fee-recipient fee-recipient :timeouts timeouts
                                  :backfill-frequency backfill-frequency :report reporter
                                  :journal (make-file-journal (datadir-path datadir "simplex/"))))
             (load-finalized-headers app)
             (when before-consensus (funcall before-consensus app))
             (open-consensus app share datadir)
             app))
      (unless (and app (validator-app-voter app))
        (when log (close-block-log log))
        (clempo.store:close-store store)))))

(defun close-validator-app (app)
  (close-block-log (validator-app-log app))
  (clempo.store:close-store (validator-app-store app)))

(defun call-with-validator-app (function &rest options)
  "Call FUNCTION with the app OPEN-VALIDATOR-APP opens with OPTIONS, then close it."
  (let ((app (apply #'open-validator-app options)))
    (unwind-protect (funcall function app)
      (close-validator-app app))))

(defun run-validator (&key chain datadir seed share (listen-host "127.0.0.1") (listen-port 8000)
                           (fee-recipient 0) http-host http-port network-options timeouts
                           (backfill-frequency 8) (seconds 0) (reporter (constantly nil)))
  "Run a Tempo validator with ed25519 SEED on DATADIR (created or resumed) for the chain
CHAIN (a genesis path or LOAD-CHAINSPEC designator). SHARE is the BLS share for the first
epoch it signs in (NIL: the datadir's DKG state, or an observer until a ceremony deals one).
Peers come from ValidatorConfigV2. HTTP-PORT serves JSON-RPC. Stops on SIGTERM/SIGINT or
after SECONDS (0: never) and returns the finalized head. REPORTER receives each JSON event."
  (let ((chain (clempo.chainspec:load-chainspec chain))
        (deadline (when (plusp seconds) (+ (io:now-ms) (* 1000 seconds))))
        (server nil))
    (unwind-protect
         (apply #'call-with-validator-network
                (lambda (network)
                  (call-with-validator-app
                   (lambda (app)
                     (unwind-protect
                          (progn
                            (loop until (or *stop-requested* (and deadline (>= (io:now-ms) deadline)))
                                  do (step-validator app))
                            (funcall reporter (json:jobject "event" "stopped" "head" (head-number app)))
                            (head-number app))
                       ;; JSON-RPC readers stop before the store closes.
                       (when server (clempo.rpcserver:stop-server (shiftf server nil)))))
                   :chain chain :datadir datadir :seed seed :share share :network network
                   :fee-recipient fee-recipient :timeouts timeouts
                   :backfill-frequency backfill-frequency :reporter reporter
                   :before-consensus
                   (lambda (app)
                     (when http-port
                       (setf server (clempo.rpcserver:start-server :host (or http-host "127.0.0.1")
                                                                   :port http-port
                                                                   :backend (validator-app-backend app))
                             (validator-app-server app) server))
                     (funcall reporter (json:jobject "event" "started"
                                                     "key" (octets->hex (validator-app-key app))
                                                     "head" (head-number app)
                                                     "listen" (network-listen-port network)
                                                     "rpc" (if server (clempo.rpcserver:server-port server) :null))))))
                :seed seed :listen-host listen-host :listen-port listen-port
                :channel-quotas (tempo-channel-quotas :backfill-frequency backfill-frequency)
                network-options)
      (when server (clempo.rpcserver:stop-server server)))))

;;; The command line, with Tempo's flag names.

(defparameter +validator-usage+
  "usage: clempo validator --chain GENESIS.json|mainnet|moderato --datadir DIR
    --consensus.signing-key PATH [--consensus.secret PASSPHRASE-FILE]
    [--consensus.signing-share PATH] [--consensus.listen-address HOST:PORT]
    [--consensus.fee-recipient ADDR] [--http --http.addr HOST --http.port PORT]
    [--consensus.use-local-defaults] [--consensus.allow-private-ips]
    [--consensus.backfill-frequency N] [--consensus.wait-for-proposal DURATION]
    [--consensus.wait-for-notarizations DURATION]
    [--consensus.wait-to-rebroadcast-nullify DURATION] [--seconds N]
Peers are the validators ValidatorConfigV2 registers (their ingress addresses). Events are
written to standard output as JSON lines.
")

(defparameter +validator-flags+
  '("chain" "datadir" "consensus.signing-key" "consensus.secret" "consensus.signing-share"
    "consensus.listen-address" "consensus.fee-recipient" "http" "http.addr" "http.port"
    "consensus.use-local-defaults" "consensus.allow-private-ips" "consensus.bypass-ip-check"
    "consensus.backfill-frequency" "consensus.wait-for-proposal" "consensus.wait-for-notarizations"
    "consensus.wait-to-rebroadcast-nullify" "seconds"))

(defparameter +validator-boolean-flags+
  '("http" "consensus.use-local-defaults" "consensus.allow-private-ips" "consensus.bypass-ip-check"))

(defun parse-duration-ms (text name)
  "Tempo's humantime durations as the command takes them: Nms, Ns or Nm."
  (let* ((unit (cond ((uiop:string-suffix-p text "ms") "ms") ((uiop:string-suffix-p text "s") "s")
                     ((uiop:string-suffix-p text "m") "m")))
         (digits (and unit (subseq text 0 (- (length text) (length unit))))))
    (unless (and digits (plusp (length digits)) (every #'digit-char-p digits))
      (keys-fail "--~A must be a duration such as 500ms or 2s" name))
    (* (parse-integer digits) (cond ((string= unit "ms") 1) ((string= unit "s") 1000) (t 60000)))))

(defun parse-listen-address (text)
  (multiple-value-bind (host port) (parse-socket-address text)
    (unless host (keys-fail "--consensus.listen-address must be IP:PORT, got ~S" text))
    (values host port)))

(defun validator-options (args)
  "The keyword arguments of RUN-VALIDATOR from the command line ARGS."
  (let* ((flags (parse-keys-flags args +validator-flags+ :booleans +validator-boolean-flags+))
         (chain (flag flags "chain" t))
         (secret (flag flags "consensus.secret"))
         (share-path (flag flags "consensus.signing-share")))
    (multiple-value-bind (listen-host listen-port)
        (parse-listen-address (or (flag flags "consensus.listen-address") "127.0.0.1:8000"))
      (flet ((integer-flag (name default bits)
               (if (flag flags name) (flag-integer flags name bits) default))
             (duration (name) (let ((v (flag flags name))) (when v (parse-duration-ms v name)))))
        (let ((timeouts (append (let ((ms (duration "consensus.wait-for-proposal"))) (when ms (list :leader ms)))
                                (let ((ms (duration "consensus.wait-for-notarizations"))) (when ms (list :certification ms)))
                                (let ((ms (duration "consensus.wait-to-rebroadcast-nullify"))) (when ms (list :retry ms))))))
          (list :chain (cond ((string= chain "mainnet") :mainnet) ((string= chain "moderato") :moderato)
                             (t (pathname chain)))
                :datadir (pathname (flag flags "datadir" t))
                :seed (read-signing-key (flag flags "consensus.signing-key" t) :passphrase-file secret)
                :share (when share-path (read-signing-share share-path))
                :listen-host listen-host :listen-port listen-port
                :fee-recipient (if (flag flags "consensus.fee-recipient") (flag-address flags "consensus.fee-recipient") 0)
                :http-host (when (flag flags "http") (or (flag flags "http.addr") "127.0.0.1"))
                :http-port (when (flag flags "http") (integer-flag "http.port" 8545 16))
                :backfill-frequency (max 1 (integer-flag "consensus.backfill-frequency" 8 32))
                :seconds (integer-flag "seconds" 0 32)
                :timeouts (when timeouts (append timeouts clempo.simplex:*tempo-timeouts*))
                :network-options (append (when (flag flags "consensus.use-local-defaults") (local-network-options))
                                         (when (flag flags "consensus.allow-private-ips") (list :allow-private-ips t))
                                         (when (flag flags "consensus.bypass-ip-check") (list :bypass-ip-check t)))))))))

(defun call-with-stop-signals (function)
  "Call FUNCTION with SIGTERM and SIGINT asking the event loop to stop cleanly."
  (let ((*stop-requested* nil))
    (flet ((stop (signal info context)
             (declare (ignore signal info context))
             (setf *stop-requested* t)))
      (sb-sys:enable-interrupt sb-unix:sigterm #'stop)
      (sb-sys:enable-interrupt sb-unix:sigint #'stop)
      (unwind-protect (funcall function)
        (sb-sys:enable-interrupt sb-unix:sigterm :default)
        (sb-sys:enable-interrupt sb-unix:sigint :default)))))

(defun validator-command (args)
  "Run `clempo validator ARGS...`; returns the process exit code."
  (when (or (null args) (member (first args) '("help" "-h" "--help") :test #'string=))
    (write-string +validator-usage+ *standard-output*)
    (return-from validator-command (if args 0 2)))
  (let ((options (handler-case (validator-options args)
                   (keys-error (e)
                     (format *error-output* "clempo validator: ~A~%~A" e +validator-usage+)
                     (return-from validator-command 2)))))
    (handler-case
        (call-with-stop-signals
         (lambda ()
           (apply #'run-validator
                  :reporter (lambda (row)
                              (write-line (json:to-json-string row) *standard-output*)
                              (force-output *standard-output*))
                  options)
           0))
      (error (e)
        (format *error-output* "clempo validator: ~A~%" e)
        1))))
