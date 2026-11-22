(in-package #:clempo.validator)

;;; The validator's chain: what consensus reads from ValidatorConfigV2, the log of finalized
;;; blocks the validator keeps in its datadir, and the JSON-RPC backend over both.

;;; ValidatorConfigV2 at a state (tempo consensus/src/validators.rs read_active_peers,
;;; application/parent_state.rs). Consensus reads the post-state of a block, committed or
;;; speculative, with no gas: a storage backend that only loads slots from the state DB.

(defclass state-storage (clempo.contract:storage-backend)
  ((db :initarg :db :reader state-storage-db))
  (:documentation "Read-only, unmetered precompile storage over a state DB."))

(defmethod clempo.contract:backend-sload ((b state-storage) address slot)
  (clempo.store:db-storage (state-storage-db b) address slot))

(defmacro with-state-storage ((db) &body body)
  `(clempo.contract:with-storage ((make-instance 'state-storage :db ,db)) ,@body))

(defstruct (registered-validator (:conc-name registered-))
  "A decoded active ValidatorConfigV2 entry."
  public-key host port egress fee-recipient index address)

(defun ipv4-text (text)
  "TEXT as four dotted decimal octets (Rust's Ipv4Addr::from_str: no leading zeros), or NIL."
  (let ((parts (uiop:split-string text :separator ".")))
    (when (and (= 4 (length parts))
               (every (lambda (p) (and (<= 1 (length p) 3) (every #'digit-char-p p)
                                       (or (= 1 (length p)) (char/= #\0 (char p 0)))
                                       (<= (parse-integer p) 255)))
                      parts))
      text)))

(defun ip-text (text)
  "An IPv4 or bracketless IPv6 address string, or NIL (Rust's IpAddr::from_str)."
  (or (ipv4-text text)
      (and (find #\: text) (every (lambda (c) (or (digit-char-p c 16) (member c '(#\: #\.)))) text)
           (<= 2 (length text) 45) text)))

(defun parse-socket-address (text)
  "IP:PORT (IPv6 as [IP]:PORT) as (values host port), or NIL (Rust's SocketAddr::from_str)."
  (let ((colon (position #\: text :from-end t)))
    (when (and colon (< (1+ colon) (length text)) (<= (- (length text) colon 1) 5)
               (every #'digit-char-p (subseq text (1+ colon))))
      (let ((port (parse-integer text :start (1+ colon)))
            (host (subseq text 0 colon)))
        (when (<= port 65535)
          (if (and (plusp (length host)) (char= #\[ (char host 0)) (char= #\] (char host (1- (length host)))))
              (let ((inner (subseq host 1 (1- (length host)))))
                (when (and (find #\: inner) (ip-text inner)) (values inner port)))
              (when (ipv4-text host) (values host port))))))))

(defun decode-registered (tuple)
  "DecodedValidatorV2::decode_from_contract: NIL when the key is not an Ed25519 point or an
address does not parse."
  (destructuring-bind (key address ingress egress fee-recipient index added deactivated) tuple
    (declare (ignore added deactivated))
    (let ((public-key (integer->octets key 32)))
      (multiple-value-bind (host port) (parse-socket-address ingress)
        (when (and host (ip-text egress) (clempo.ed25519:ed25519-public-key-valid-p public-key))
          (make-registered-validator :public-key public-key :host host :port port :egress egress
                                     :fee-recipient fee-recipient :index index :address address))))))

(defun active-validators (db)
  "The decodable active ValidatorConfigV2 validators at DB's state, one per public key (a
later duplicate replaces an earlier one, as Rust's map insert does)."
  (let ((out '()))
    (dolist (tuple (with-state-storage (db) (clempo.validator-config-v2:get-active-validators)))
      (let ((v (decode-registered tuple)))
        (when v
          (setf out (cons v (remove (registered-public-key v) out
                                    :key #'registered-public-key :test #'octets=))))))
    (nreverse out)))

(defun next-full-dkg-epoch (db)
  "getNextNetworkIdentityRotationEpoch at DB's state."
  (with-state-storage (db) (clempo.validator-config-v2:get-next-network-identity-rotation-epoch)))

(defun registered-by-key (db key)
  "Every ValidatorConfigV2 entry ever recorded for KEY (deactivated ones included), decoded,
or NIL: Rust resolves addresses of players that are no longer active this way."
  (handler-case
      (decode-registered (with-state-storage (db) (clempo.validator-config-v2:validator-by-public-key
                                                   (octets->integer key))))
    (error () nil)))

(defun sorted-keys (keys)
  (sort (remove-duplicates (copy-list keys) :test #'octets=) #'clempo.cwcodec:cw-order<))

;;; The finalized block log: one record per finalized height, appended in height order and
;;; made durable before the block's state is committed, so a restart re-executes from it any
;;; block the state store had not synced. A record holds the block, its receipts (for RPC)
;;; and the finalization certificate when this height had its own (for marshal Finalized
;;; requests). Records are framed as the vote journal's: u32 length, payload, SHA-256.

(clempo.cwcodec:defcodec log-record
  (height :varint)
  (block (:bytes :max (* 64 1024 1024)))
  (receipts (:vec (:bytes :max (* 64 1024 1024)) :max 100000))
  (finalization (:option clempo.consensus:finalization)))

(defconstant +block-log-retained+ 512 "Records kept decoded in memory.")

(clempo.guard:define-shared-struct (block-log (:constructor %make-block-log))
  (path nil :guard :immutable)
  (output nil :guard (:lock lock))                                    ; append fd-stream
  (offsets (make-array 0 :adjustable t :fill-pointer t) :guard (:lock lock)) ; height -> file offset of its frame
  (hashes (make-hash-table :test #'equalp) :guard (:lock lock))       ; block hash -> height
  (txs (make-hash-table :test #'equalp) :guard (:lock lock))          ; tx hash -> (height . index)
  (recent (make-hash-table) :guard (:lock lock))                      ; height -> log-record, the last few
  (lock (thr:make-mutex :name "block log") :guard :immutable))

(defun logged-head (log)
  (clempo.guard:with-held ((block-log-lock log))
    (1- (fill-pointer (block-log-offsets log)))))

(defun block-log-head (log)
  "The highest logged height, or -1."
  (clempo.lock:with-lock ((block-log-lock log)) (logged-head log)))

(defun frame-record (octets)
  (let ((w (clempo.cwcodec:make-cw-writer (+ 36 (length octets)))))
    (clempo.cwcodec:write-uint w (length octets) 4)
    (clempo.cwcodec:write-raw w octets)
    (clempo.cwcodec:write-raw w (clempo.sha2:sha256 octets))
    (clempo.cwcodec:cw-writer-octets w)))

(defun index-record (log record offset)
  (clempo.guard:with-held ((block-log-lock log))
    (let* ((height (log-record-height record))
           (raw (log-record-block record)))
      (invariant (= height (fill-pointer (block-log-offsets log))) "block log heights are contiguous")
      (vector-push-extend offset (block-log-offsets log))
      (multiple-value-bind (hash tx-hashes) (clempo.rpc.chain:scan-block-hashes raw)
        (setf (gethash hash (block-log-hashes log)) height)
        (loop for tx in tx-hashes for i from 0 do (setf (gethash tx (block-log-txs log)) (cons height i))))
      (setf (gethash height (block-log-recent log)) record)
      (remhash (- height +block-log-retained+) (block-log-recent log)))))

(defun read-frame (in)
  "The next record payload, NIL at a clean end, or :TORN for a final frame cut short or not
matching its checksum (an append a crash interrupted). Damage before the end signals."
  (let* ((start (file-position in)) (available (- (file-length in) start)))
    (cond ((zerop available) nil)
          ((< available 36) :torn)
          (t (let ((size (loop repeat 4 for n = (read-byte in) then (logior (ash n 8) (read-byte in))
                               finally (return n))))
               (if (> (+ 36 size) available)
                   :torn
                   (let ((data (clempo.p2p-wire:read-octets in size))
                         (sum (clempo.p2p-wire:read-octets in 32)))
                     (cond ((octets= sum (clempo.sha2:sha256 data)) data)
                           ((= (+ 36 size) available) :torn)
                           (t (error "corrupt block log record at offset ~D" start))))))))))

(defun open-block-log (directory)
  "Open (creating) the block log in DIRECTORY and index it. A torn final record, the trace
of a crash during an append, is cut off; any other damage signals an error."
  (let* ((dir (uiop:ensure-directory-pathname directory))
         (path (merge-pathnames "finalized.log" dir))
         (log (%make-block-log :path path))
         (good 0))
    (ensure-directories-exist dir)
    (clempo.lock:with-lock ((block-log-lock log))
      (with-open-file (in path :element-type 'octet :if-does-not-exist nil)
        (when in
          (loop
            (let* ((offset (file-position in)) (data (read-frame in)))
              (when (member data '(nil :torn)) (return))
              (index-record log (clempo.cwcodec:cw-decode 'log-record data) offset)
              (setf good (file-position in))))))
      (let ((fd (sb-posix:open (namestring path) (logior sb-posix:o-wronly sb-posix:o-creat) #o600)))
        (sb-posix:ftruncate fd good)
        (sb-posix:lseek fd good sb-posix:seek-set)
        (setf (block-log-output log) (sb-sys:make-fd-stream fd :element-type 'octet :output t
                                                               :buffering :full :auto-close t))))
    log))

(defun close-block-log (log)
  (clempo.lock:with-lock ((block-log-lock log))
    (when (block-log-output log)
      (close (block-log-output log))
      (setf (block-log-output log) nil))))

(defun block-log-append (log height raw receipts finalization)
  "Append HEIGHT's record and make it durable. RECEIPTS are EIP-2718 envelopes."
  (let ((record (make-log-record :height height :block raw :receipts receipts :finalization finalization)))
    (clempo.lock:with-lock ((block-log-lock log))
      (let* ((out (block-log-output log)) (offset (file-position out)))
        (write-sequence (frame-record (clempo.cwcodec:cw-encode 'log-record record)) out)
        (finish-output out)
        (sb-posix:fsync (sb-sys:fd-stream-fd out))
        (index-record log record offset)))
    record))

(defun block-log-record (log height)
  "HEIGHT's LOG-RECORD, or NIL."
  (clempo.lock:with-lock ((block-log-lock log))
    (when (and (integerp height) (<= 0 height (logged-head log)))
      (or (gethash height (block-log-recent log))
          (with-open-file (in (block-log-path log) :element-type 'octet)
            (file-position in (aref (block-log-offsets log) height))
            (clempo.cwcodec:cw-decode 'log-record (read-frame in)))))))

(defun block-log-height (log hash)
  (clempo.lock:with-lock ((block-log-lock log)) (gethash hash (block-log-hashes log))))

(defun block-log-transaction (log hash)
  (clempo.lock:with-lock ((block-log-lock log)) (gethash hash (block-log-txs log))))

;;; JSON-RPC over the validator's chain: blocks and receipts from the log, state at the
;;; head, and the transaction pool consensus proposes from.

(defclass validator-backend (clempo.rpc.chain:backend)
  ((log :initarg :log :reader backend-log)
   (store :initarg :store :reader backend-store)
   (pool :initarg :pool :initform nil :reader backend-pool-service)
   ;; The event loop's latest status (a JSON object it replaces whole), for
   ;; clempo_validatorStatus; NIL until the loop first publishes.
   (status :initform nil :accessor backend-validator-status)))

(defmethod clempo.rpc.chain:backend-head ((b validator-backend))
  (clempo.store:db-number (clempo.store:store-db (backend-store b))))

(defmethod clempo.rpc.chain:backend-block-raw ((b validator-backend) number)
  (let ((record (block-log-record (backend-log b) number)))
    (when record (log-record-block record))))

(defmethod clempo.rpc.chain:backend-receipts-raw ((b validator-backend) number)
  (let ((record (block-log-record (backend-log b) number)))
    (when record (log-record-receipts record))))

(defmethod clempo.rpc.chain:backend-block-number ((b validator-backend) hash)
  (block-log-height (backend-log b) hash))

(defmethod clempo.rpc.chain:backend-tx-location ((b validator-backend) hash)
  (let ((location (block-log-transaction (backend-log b) hash)))
    (when location (values (car location) (cdr location)))))

(defmethod clempo.rpc.chain:backend-state ((b validator-backend) number)
  (let ((db (clempo.store:store-db (backend-store b))))
    (when (eql number (clempo.store:db-number db)) db)))

;;; The transaction pool consensus proposes from (clempo.txpool, Tempo's pool rules). JSON-RPC
;;; submissions are admitted against the head state; every finalized block revalidates it.

(clempo.guard:define-shared-struct (validator-pool (:constructor make-validator-pool ()))
  (pool (clempo.txpool:make-pool) :guard :immutable) ; its contents are touched under LOCK only
  (lock (thr:make-mutex :name "validator pool") :guard :immutable))

(defmacro with-validator-pool ((pool) &body body)
  `(clempo.lock:with-lock ((validator-pool-lock ,pool)) ,@body))

(defun pool-snapshot-reader (backend)
  "A txpool state reader over the head state, for the next block's environment, with the fee
tokens the last ten finalized blocks' beneficiaries prefer (the pool's validator-token window)."
  (let* ((db (clempo.store:store-db (backend-store backend)))
         (log (backend-log backend))
         (head-number (clempo.store:db-number db))
         (head (clempo.blocks:block-header
                (clempo.blocks:decode-block (log-record-block (block-log-record log head-number)))))
         (chain (clempo.rpc.chain:backend-chain backend))
         (env (clempo.builder:next-block-env
               db head (clempo.builder:make-payload-attributes
                        :timestamp (max (1+ (clempo.headers:header-timestamp head)) (io:now-seconds)))
               :chain chain)))
    (clempo.txpool:make-state-reader
     db env :recent-validator-tokens
     (loop for height from (max 0 (- head-number 9)) to head-number
           for header = (clempo.blocks:block-header
                         (clempo.blocks:decode-block (log-record-block (block-log-record log height))))
           collect (clempo.txpool::validator-fee-token
                    db env (clempo.headers:ethereum-header-beneficiary (clempo.headers:header-inner header)))))))

(defmethod clempo.rpc.chain:backend-submit-raw-transaction ((b validator-backend) raw)
  (let ((pool (backend-pool-service b)))
    (unless pool (return-from clempo.rpc.chain:backend-submit-raw-transaction (call-next-method)))
    (let ((tx (clempo.transactions:decode-transaction raw)))
      (with-validator-pool (pool)
        (clempo.txpool:submit-transaction (validator-pool-pool pool) tx (pool-snapshot-reader b)))
      (clempo.transactions:transaction-hash tx))))

(defmethod clempo.rpc.chain:backend-pool-transactions ((b validator-backend))
  (let ((pool (backend-pool-service b)))
    (when pool (with-validator-pool (pool) (clempo.txpool:pool-snapshot (validator-pool-pool pool))))))

(defmethod clempo.rpc.chain:backend-clear-pool ((b validator-backend))
  (let ((pool (backend-pool-service b)))
    (if pool (with-validator-pool (pool) (clempo.txpool:pool-clear (validator-pool-pool pool))) 0)))

(defun revalidate-pool (backend)
  "After a finalized block: drop what it mined or made invalid."
  (let ((pool (backend-pool-service backend)))
    (when pool
      (with-validator-pool (pool)
        (clempo.txpool:pool-revalidate (validator-pool-pool pool) (pool-snapshot-reader backend))))))
