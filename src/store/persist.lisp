(in-package #:clempo.store)

;;; A datadir: the KV store of a backed state database and the write-ahead log of the
;;; blocks committed since its last flush.
;;;
;;;   LOCK                 flock(2)ed while a store has the datadir open
;;;   MANIFEST             the KV store's live segments (kv.lisp)
;;;   seg-<n>.kv           KV segments; the head record names the flushed block
;;;   wal-<first>.log      one record per block committed after the flush that started it
;;;
;;; STORE-COMMIT-BLOCK appends the block's diff to the log and, under the default :BLOCK
;;; policy, syncs it to stable storage (F_FULLFSYNC on macOS) before returning: a block the
;;; store has acknowledged survives a crash or power loss. Under (:GROUP N) the log is synced
;;; every N blocks, under (:EVERY-MS T) once T milliseconds have passed since the last sync;
;;; both also sync at every flush and on close. STORE-DURABLE-NUMBER is then the last block a
;;; crash cannot lose, and a caller acknowledges nothing past it. Every INTERVAL blocks, and whenever the database holds
;;; its cache limit's worth of unflushed writes, the store flushes them as one KV segment and
;;; starts a new log.
;;;
;;; Opening replays the log on the flushed state with every block's root checked. A record
;;; that does not parse or fails its checksum ends the log only when nothing valid follows
;;; it in the last log file (a write cut short by a crash): that tail is cut. Anywhere else it
;;; is corruption and STORE-CORRUPT-LOG is signalled; nothing is truncated.
;;;
;;; Format 1 datadirs (checkpoint-<n>.state, log-<n>.wal, MANIFEST "clempo-store 1 n") are
;;; imported into a KV store when opened.
;;;
;;; Checkpoints (WRITE-CHECKPOINT, READ-CHECKPOINT) remain a portable snapshot of a whole
;;; state: history build jobs keep their resume states in them and the archive RPC backend
;;; loads trie states from them.

(define-constant +checkpoint-magic+ (string->octets "CLEMPOCK"))
(define-constant +checkpoint-end+ (string->octets "CLEMPOEN"))
;;; Checkpoint versions: 1 has no account commitments; 2 adds each account's T14 commitment.
(defconstant +checkpoint-version+ 2)
(define-constant +wal-magic+ (string->octets "CLWR"))

(define-condition store-root-mismatch (error)
  ((number :initarg :number :reader store-root-mismatch-number)
   (expected :initarg :expected :reader store-root-mismatch-expected)
   (actual :initarg :actual :reader store-root-mismatch-actual))
  (:report (lambda (c s)
             (format s "store: state root after block ~D is ~A, recorded ~A"
                     (store-root-mismatch-number c)
                     (octets->hex (store-root-mismatch-actual c))
                     (octets->hex (store-root-mismatch-expected c))))))

(define-condition store-corrupt-log (error)
  ((path :initarg :path :reader store-corrupt-log-path)
   (offset :initarg :offset :reader store-corrupt-log-offset)
   (reason :initarg :reason :reader store-corrupt-log-reason))
  (:report (lambda (c s)
             (format s "store: block log ~A is corrupt at byte ~D (~A); valid records follow, so ~
                        it was not truncated"
                     (store-corrupt-log-path c) (store-corrupt-log-offset c) (store-corrupt-log-reason c)))))

;;; Checkpoints

(defun checkpoint-path (dir n) (dir-file dir "checkpoint-~12,'0D.state" n))

(defun write-checkpoint-to (db out)
  (let ((sink (make-sink out))
        (codes 0))
    (map-codes (lambda (k c) (declare (ignore k c)) (incf codes)) db)
    (put-raw sink +checkpoint-magic+)
    (put-u32 sink +checkpoint-version+)
    (put-u64 sink (db-number db))
    (put-raw sink (db-hash db))
    (put-raw sink (db-root-hash db))
    (put-u32 sink (hash-table-count (db-recent db)))
    (map-recent-hashes (lambda (n h) (put-u64 sink n) (put-raw sink h)) db)
    (put-u64 sink codes)
    (map-codes (lambda (key code) (put-raw sink (integer->octets key 32)) (put-bytes sink code)) db)
    (put-u64 sink (db-account-count db))
    (map-accounts (lambda (address a)
                    (put-raw sink (address-octets address))
                    (put-int sink (account-nonce a))
                    (put-int sink (account-balance a))
                    (put-raw sink (account-code-hash a))
                    (put-int sink (account-commitment a))
                    (put-raw sink (flush-storage a))
                    (put-u64 sink (account-slot-count a))
                    (map-account-storage (lambda (slot value) (put-int sink slot) (put-int sink value)) a))
                  db)
    (put-raw sink +checkpoint-end+)
    (sink-flush sink)))

(defun write-checkpoint (db path)
  "Write DB (no open block, root computed) to PATH atomically."
  (invariant (not (db-open-p db)) "checkpoint inside a block")
  (invariant (octets= (db-state-root db) (db-root-hash db)) "checkpoint of an uncommitted state")
  (write-durably path (lambda (out) (write-checkpoint-to db out))))

(defun read-checkpoint-header (src path)
  "(values version number hash root) after reading a checkpoint's magic."
  (unless (octets= (get-raw src 8) +checkpoint-magic+) (format-error "~A: not a checkpoint" path))
  (let ((version (get-u32 src)))
    (unless (member version '(1 2)) (format-error "~A: unknown version" path))
    (values version (get-u64 src) (get-raw src 32) (get-raw src 32))))

(defun read-checkpoint-account (src version)
  "(values address nonce balance code-hash commitment storage-root slot-count) of the next
account of a checkpoint; its slots follow."
  (values (octets->integer (get-raw src 20)) (get-int src) (get-int src) (get-raw src 32)
          (if (= version 1) 0 (get-int src))
          (get-raw src 32) (get-u64 src)))

(defun read-checkpoint (path &key (check-root t))
  "Load a checkpoint into a fresh in-RAM state database; with CHECK-ROOT rebuild the account
trie and compare its root with the recorded one."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((src (make-source in))
          (db (make-statedb)))
      (multiple-value-bind (version number hash root) (read-checkpoint-header src path)
        (loop repeat (get-u32 src) do (let ((n (get-u64 src))) (db-note-block-hash db n (get-raw src 32))))
        (let ((codes (db-codes db)))
          (loop repeat (get-u64 src)
                do (let ((key (octets->integer (get-raw src 32))))
                     (setf (gethash key codes) (get-bytes src)))))
        (let ((accounts (db-accounts db)) (dirty (db-dirty db)))
          (loop repeat (get-u64 src)
                do (multiple-value-bind (address nonce balance code-hash commitment storage-root n)
                       (read-checkpoint-account src version)
                     (let ((a (%make-account address)))
                       (setf (account-nonce a) nonce (account-balance a) balance
                             (account-code-hash a) code-hash (account-commitment a) commitment
                             (account-storage-root a) storage-root (account-slot-count a) n)
                       (when (plusp n)
                         (let ((s (make-hash-table :size n)))
                           (loop repeat n do (let ((slot (get-int src))) (setf (gethash slot s) (get-int src))))
                           (setf (account-storage a) s)))
                       (setf (gethash address accounts) a
                             (gethash address dirty) t)))))
        (unless (octets= (get-raw src 8) +checkpoint-end+) (format-error "~A: bad trailer" path))
        (setf (db-number db) number (db-hash db) hash)
        ;; Without CHECK-ROOT the account trie is built by the first DB-STATE-ROOT.
        (when check-root
          (let ((actual (db-state-root db)))
            (unless (octets= actual root)
              (error 'store-root-mismatch :number number :expected root :actual actual))))
        (setf (db-root-hash db) root)
        db))))

;;; Block diffs

(defun encode-block-diff (diff)
  (let ((sink (make-sink)))
    (put-u64 sink (block-diff-number diff))
    (put-raw sink (block-diff-hash diff))
    (put-raw sink (block-diff-root diff))
    (put-u32 sink (length (block-diff-codes diff)))
    (dolist (code (block-diff-codes diff)) (put-bytes sink code))
    (put-u32 sink (length (block-diff-accounts diff)))
    (dolist (d (block-diff-accounts diff))
      (put-raw sink (address-octets (account-diff-address d)))
      (put-u8 sink (logior (if (account-diff-deleted-p d) 1 0) (if (account-diff-wiped-p d) 2 0)
                           (if (zerop (account-diff-commitment d)) 0 4)))
      (unless (account-diff-deleted-p d)
        (put-int sink (account-diff-nonce d))
        (put-int sink (account-diff-balance d))
        (put-raw sink (account-diff-code-hash d))
        (unless (zerop (account-diff-commitment d)) (put-int sink (account-diff-commitment d)))
        (put-u32 sink (length (account-diff-slots d)))
        (loop for (slot . value) in (account-diff-slots d)
              do (put-int sink slot) (put-int sink value))))
    (sink-octets sink)))

(defun decode-block-diff (octets)
  (let* ((src (make-source octets))
         (number (get-u64 src)) (hash (get-raw src 32)) (root (get-raw src 32))
         (codes (loop repeat (get-u32 src) collect (get-bytes src)))
         (accounts
           (loop repeat (get-u32 src)
                 collect (let* ((address (octets->integer (get-raw src 20)))
                                (flags (get-u8 src)))
                           (if (logbitp 0 flags)
                               (make-account-diff :address address :deleted-p t)
                               (make-account-diff
                                :address address :wiped-p (logbitp 1 flags)
                                :nonce (get-int src) :balance (get-int src) :code-hash (get-raw src 32)
                                :commitment (if (logbitp 2 flags) (get-int src) 0)
                                :slots (loop repeat (get-u32 src)
                                             collect (let ((slot (get-int src))) (cons slot (get-int src))))))))))
    (unless (source-exhausted-p src) (format-error "trailing bytes in block record ~D" number))
    (make-block-diff :number number :hash hash :root root :accounts accounts :codes codes)))

;;; The write-ahead log: per record "CLWR", payload length u32, payload, CRC-32C u32 of the
;;; length and payload.

(defun wal-path (dir first) (dir-file dir "wal-~12,'0D.log" first))

(defun wal-files (dir)
  "The log files of DIR, oldest first: ((first . path) ...)."
  (sort (loop for p in (directory (dir-file dir "wal-*.log"))
              for name = (pathname-name p)
              for n = (and (> (length name) 4) (every #'digit-char-p (subseq name 4))
                           (parse-integer name :start 4))
              when n collect (cons n p))
        #'< :key #'car))

(defun put-wal-record (sink payload)
  (let ((len (integer->octets (length payload) 4)))
    (put-raw sink +wal-magic+)
    (put-raw sink len)
    (put-raw sink payload)
    (put-u32 sink (crc32c payload 0 (length payload) (crc32c len)))))

(defun wal-record-at (bytes pos)
  "(values payload-start payload-end next) of a valid record at POS, or NIL and a reason."
  (let ((total (length bytes)))
    (cond ((> (+ pos 8) total) (values nil "header past the end"))
          ((mismatch +wal-magic+ bytes :start2 pos :end2 (+ pos 4)) (values nil "bad record magic"))
          (t (let* ((len (octets->integer bytes :start (+ pos 4) :end (+ pos 8)))
                    (start (+ pos 8)) (end (+ start len)))
               (cond ((> (+ end 4) total) (values nil "record past the end"))
                     ((/= (octets->integer bytes :start end :end (+ end 4))
                          (crc32c bytes start end (crc32c bytes (+ pos 4) (+ pos 8))))
                      (values nil "checksum mismatch"))
                     (t (values start end (+ end 4)))))))))

(defun valid-record-after-p (bytes pos)
  "True when a valid record starts anywhere after POS."
  (loop for q = (search +wal-magic+ bytes :start2 (1+ pos)) then (search +wal-magic+ bytes :start2 (1+ q))
        while q
        thereis (wal-record-at bytes q)))

(defun scan-wal (path fn &key last-p)
  "Call FN with each record payload of the log at PATH. Returns the byte length of the valid
records and whether a torn tail follows them. A bad record with a valid one after it, or a
bad record in a log that is not the LAST-P one, signals STORE-CORRUPT-LOG."
  (let ((bytes (clempo.io:read-file-octets path)) (pos 0))
    (loop
      (when (= pos (length bytes)) (return (values pos nil)))
      (multiple-value-bind (start end next) (wal-record-at bytes pos)
        (cond (start (funcall fn (subseq bytes start end))
                     (setf pos next))
              ((and last-p (not (valid-record-after-p bytes pos)))
               (return (values pos t)))
              (t (error 'store-corrupt-log :path path :offset pos :reason end)))))))

;;; Legacy format 1 log records: length u32, payload, low 32 bits of the payload's keccak.

(defun read-legacy-records (path fn)
  "Call FN with each intact record payload of the format 1 log at PATH. Returns the byte
offset after the last intact record and whether a torn tail followed it. As in SCAN-WAL a
bad record with valid ones after it signals STORE-CORRUPT-LOG."
  (let ((bytes (clempo.io:read-file-octets path)) (pos 0))
    (flet ((record-at (p)
             (when (<= (+ p 4) (length bytes))
               (let* ((len (octets->integer bytes :start p :end (+ p 4))) (end (+ p 4 len)))
                 (when (and (<= (+ end 4) (length bytes))
                            (= (octets->integer bytes :start end :end (+ end 4))
                               (ldb (byte 32 0) (octets->integer (keccak256 (subseq bytes (+ p 4) end))))))
                   (values (+ p 4) end (+ end 4)))))))
      (loop
        (when (= pos (length bytes)) (return (values pos nil)))
        (multiple-value-bind (start end next) (record-at pos)
          (cond (start (funcall fn (subseq bytes start end)) (setf pos next))
                ((loop for q from (1+ pos) below (length bytes) thereis (record-at q))
                 (error 'store-corrupt-log :path path :offset pos :reason "bad legacy record"))
                (t (return (values pos t)))))))))

;;; The datadir lock: an exclusive flock(2) on DIR/LOCK for as long as the store is open, so
;;; two processes (or two opens in one process) never write one datadir. The kernel drops
;;; the lock when the process dies, so a crash leaves nothing to clean up.

(define-condition store-locked (error)
  ((dir :initarg :dir :reader store-locked-dir))
  (:report (lambda (c s) (format s "store: datadir ~A is in use by another store (LOCK is held)"
                                 (store-locked-dir c)))))

(defconstant +lock-ex+ 2)
(defconstant +lock-nb+ 4)

(defun lock-datadir (dir)
  "Take DIR's lock; returns the descriptor that holds it. Signals STORE-LOCKED when held."
  (let ((fd (sb-posix:open (namestring (dir-file dir "LOCK"))
                           (logior sb-posix:o-rdwr sb-posix:o-creat) #o644)))
    (if (zerop (sb-alien:alien-funcall
                (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                fd (logior +lock-ex+ +lock-nb+)))
        fd
        (let ((errno (sb-alien:get-errno)))
          (sb-posix:close fd)
          (if (= errno sb-posix:ewouldblock)
              (error 'store-locked :dir dir)
              (error "store: flock on ~A failed: errno ~D" dir errno))))))

(defun unlock-datadir (fd)
  (when fd (sb-posix:close fd)))

;;; The store

(defstruct (store (:constructor %make-store (dir kv db interval sync-policy)))
  dir kv db
  (interval 0 :type integer)          ; blocks between flushes; 0 = only on the cache limit
  (sync-policy :block)                ; :BLOCK, (:GROUP n) or (:EVERY-MS t)
  wal-stream wal-sink
  (logged-number -1 :type integer)    ; last block appended to the log
  (durable-number -1 :type integer)   ; last block a crash cannot lose
  (unsynced 0 :type fixnum)
  (synced-at 0 :type integer)         ; internal real time of the last sync
  (lock-fd nil))

(defun check-sync-policy (policy)
  (unless (or (eq policy :block)
              (and (consp policy) (member (first policy) '(:group :every-ms))
                   (typep (second policy) '(integer 1)) (null (cddr policy))))
    (error "store: sync policy must be :BLOCK, (:GROUP n) or (:EVERY-MS t), not ~S" policy))
  policy)

(defun sync-due-p (store)
  "Whether the sync policy wants the log synced after the block just appended."
  (let ((policy (store-sync-policy store)))
    (cond ((eq policy :block) t)
          ((eq (first policy) :group) (>= (store-unsynced store) (second policy)))
          (t (>= (- (clempo.io:monotonic-ticks) (store-synced-at store))
                 (* (second policy) (/ internal-time-units-per-second 1000)))))))

(defun open-wal (store path)
  (let ((stream (open path :element-type '(unsigned-byte 8) :direction :output
                           :if-exists :append :if-does-not-exist :create)))
    (setf (store-wal-stream store) stream
          (store-wal-sink store) (make-sink stream))))

(defun start-wal (store)
  "Start a new, empty log after the flushed head. Returns the older logs, which the caller
deletes once the flushed state is durable in the KV store."
  (let* ((dir (store-dir store))
         (first (1+ (db-number (store-db store))))
         (path (wal-path dir first)))
    (when (store-wal-stream store) (close (store-wal-stream store)))
    (write-durably path (lambda (out) (declare (ignore out))))
    (open-wal store path)
    (loop for (n . p) in (wal-files dir) when (< n first) collect p)))

(defun normalize-dir (dir)
  (let ((p (pathname dir)))
    (if (or (pathname-name p) (pathname-type p))
        (make-pathname :directory (append (or (pathname-directory p) (list :relative))
                                          (list (file-namestring p)))
                       :name nil :type nil :defaults p)
        p)))

(defun open-store (dir &key genesis (interval 0) (log (constantly nil)) (sync :block)
                         (cache (make-cache-limits)))
  "Open the datadir DIR (created if absent) and hold its lock until CLOSE-STORE.
An empty datadir is initialized by calling GENESIS with the fresh state database, which must
commit block 0 in it. Otherwise the flushed state is opened and the log replayed on it with
every block's root checked; a format 1 datadir is imported first. SYNC is the log's sync
policy (:BLOCK, (:GROUP n) or (:EVERY-MS t)); INTERVAL the blocks between flushes (0: flush only when the
unflushed writes reach CACHE's limit); CACHE the database's cache limits. LOG is called with
a format string and arguments for progress notes."
  (check-sync-policy sync)
  (let* ((dir (normalize-dir dir))
         (fd (progn (ensure-directories-exist dir) (lock-datadir dir)))
         (store nil))
    (unwind-protect
         (progn (setf store (open-locked-store dir genesis interval log sync cache))
                (setf (store-lock-fd store) fd))
      (unless store (unlock-datadir fd)))
    store))

(defun open-datadir-snapshot (dir &key (cache (make-cache-limits)))
  "A state database over the flushed state of the datadir DIR, or NIL when it holds none.
It takes no lock, replays no log and changes no file, so it may be opened beside the store
that owns DIR: the segments it maps stay readable after the owner merges them away. Its
height is the owner's last flush (DB-NUMBER), not the owner's head. Blocks may be executed
and committed on it (WITH-SPECULATIVE-BLOCKS, or plain commits that move it); they stay in
its RAM and never reach DIR."
  (let ((dir (normalize-dir dir)))
    (when (kv-manifest-p dir)
      ;; a merge may delete a listed segment between the manifest read and its open
      (loop repeat 8
            do (let ((kv (handler-case (open-kv dir :read-only t)
                           (file-error () nil)
                           (sb-posix:syscall-error () nil))))
                 (when kv
                   (return (when (kv-get kv (key-head))
                             (open-backed-statedb kv cache)))))
            finally (format-error "~A: its segments kept changing while being opened" dir)))))

(defun legacy-manifest (dir)
  "The checkpoint number a format 1 MANIFEST names, or NIL."
  (let ((path (dir-file dir "MANIFEST")))
    (when (probe-file path)
      (let ((text (string-trim '(#\Newline #\Space) (clempo.io:read-file-string path))))
        (when (eql 0 (search "clempo-store " text))
          (let ((words (let ((*read-eval* nil))
                         (with-input-from-string (s text)
                           (loop for w = (read s nil) while w collect w)))))
            (unless (and (= (length words) 3) (eql (second words) 1) (integerp (third words)))
              (format-error "bad MANIFEST ~S" text))
            (third words)))))))

(defun open-locked-store (dir genesis interval log sync cache)
  (let ((legacy (legacy-manifest dir)))
    (when legacy (import-legacy-datadir dir legacy log)))
  (let* ((kv (open-kv dir))
         (db (open-backed-statedb kv cache))
         (store (%make-store dir kv db interval sync)))
    (cond ((kv-get kv (key-head))
           (funcall log "store: block ~D flushed, root ~A" (db-number db) (octets->hex (db-root-hash db)))
           (replay-wal store log))
          (t
           (invariant genesis "empty datadir ~A and no genesis" dir)
           (funcall genesis db)
           (invariant (zerop (db-number db)) "genesis must commit block 0")
           (flush-db store)))
    (setf (store-logged-number store) (db-number db)
          (store-durable-number store) (db-number db))
    (unless (store-wal-stream store) (start-wal store))
    store))

(defun replay-wal (store log)
  "Apply the logged blocks after the flushed head; continue appending to the last log."
  (let* ((db (store-db store))
         (files (wal-files (store-dir store)))
         (count 0))
    (loop for ((nil . path) . more) on files
          do (multiple-value-bind (good torn)
                 (scan-wal path
                           (lambda (payload)
                             (let ((diff (decode-block-diff payload)))
                               (when (> (block-diff-number diff) (db-number db))
                                 (unless (= (block-diff-number diff) (1+ (db-number db)))
                                   (error 'store-corrupt-log :path path :offset 0
                                                             :reason (format nil "block ~D follows block ~D"
                                                                             (block-diff-number diff) (db-number db))))
                                 (let ((actual (db-apply-diff db diff)))
                                   (unless (octets= actual (block-diff-root diff))
                                     (error 'store-root-mismatch :number (block-diff-number diff)
                                                                 :expected (block-diff-root diff) :actual actual)))
                                 (incf count))))
                           :last-p (null more))
               (declare (type (or null fixnum) good))
               (when torn
                 (funcall log "store: cutting a torn log tail at byte ~D of ~A" good (file-namestring path))
                 (sb-posix:truncate (namestring path) good)
                 (with-open-file (out path :direction :output :if-exists :append :element-type '(unsigned-byte 8))
                   (sync-stream out)))
               (unless more (open-wal store path))))
    (funcall log "store: replayed ~D logged blocks to ~D" count (db-number db))
    count))

(defun store-sync (store)
  "Write buffered log records and sync them to stable storage."
  (sink-flush (store-wal-sink store))
  (sync-stream (store-wal-stream store))
  (setf (store-unsynced store) 0
        (store-synced-at store) (clempo.io:monotonic-ticks)
        (store-durable-number store) (store-logged-number store)))

(defun flush-db (store)
  "Hand the database's unflushed state to the KV store and start a new log. The batch is
written as a segment on a background thread while blocks go on; until then it is read from
RAM, and the old logs, synced first, still hold its blocks (they are deleted once the
segment is in MANIFEST)."
  (let ((db (store-db store)))
    (when (store-wal-stream store) (store-sync store))
    (let ((batch (db-flush-entries db)))
      (db-after-flush db)
      (let ((old (start-wal store)))
        (kv-write-async (store-kv store) batch
                        (lambda () (dolist (p old) (when (probe-file p) (delete-file p)))))))
    (setf (store-logged-number store) (db-number db)
          (store-durable-number store) (db-number db)
          (store-synced-at store) (clempo.io:monotonic-ticks)
          (store-unsynced store) 0)
    (db-number db)))

(defun store-flush (store)
  "Flush now (between blocks, outside speculation). Returns the flushed block."
  (flush-db store))

(defun store-checkpoint (store)
  "Make the current block the store's flushed state (the log restarts after it)."
  (store-flush store))

(defun store-commit-block (store diff)
  "Append a committed block's DIFF to the log, sync it as the policy says, and flush when
the interval or the cache limit says so."
  (let ((db (store-db store)))
    (put-wal-record (store-wal-sink store) (encode-block-diff diff))
    (setf (store-logged-number store) (block-diff-number diff))
    (incf (store-unsynced store))
    (when (sync-due-p store) (store-sync store))
    (let ((k (store-interval store)))
      (when (and (db-flush-ready-p db)
                 (or (and (plusp k) (zerop (mod (block-diff-number diff) k)))
                     (> (db-flush-weight db) (cache-limits-dirty (db-limits db)))))
        (flush-db store))))
  diff)

(defun close-store (store &key checkpoint)
  "Sync the log (and with CHECKPOINT flush), close the files and release the lock."
  (unwind-protect
       (when (store-wal-stream store)
         (if (and checkpoint (db-flush-ready-p (store-db store)))
             (flush-db store)
             (store-sync store))
         (close (store-wal-stream store))
         (setf (store-wal-stream store) nil)
         (kv-close (store-kv store)))
    (unlock-datadir (shiftf (store-lock-fd store) nil)))
  store)

;;; Importing a format 1 datadir: its checkpoint streams into KV segments (flat entries and
;;; trie nodes, storage tries built one account at a time), checked root by root; then its
;;; log is replayed on the backed database and flushed. The KV MANIFEST replaces the old one
;;; only when the import is whole, so an interrupted import starts over.

(defconstant +import-batch+ 2000000)

(defun import-checkpoint (kv path log)
  "Stream the checkpoint at PATH into KV's segments (no manifest). Returns its head record."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((src (make-source in))
           (nodes (make-node-store kv t))   ; every node the import hashes is kept
           (entries (make-hash-table :size 65536))
           (leaves '())
           (head-db (make-statedb))
           (accounts 0))
      (flet ((spill (&optional force)
               (when (or force (> (hash-table-count entries) +import-batch+))
                 (kv-write kv (sorted-entries entries) :manifest nil)
                 (clrhash entries))))
        (multiple-value-bind (version number hash root) (read-checkpoint-header src path)
          (loop repeat (get-u32 src) do (let ((n (get-u64 src))) (db-note-block-hash head-db n (get-raw src 32))))
          (loop repeat (get-u64 src)
                do (let ((key (octets->integer (get-raw src 32))))
                     (setf (gethash (key-code key) entries) (get-bytes src))))
          (loop repeat (get-u64 src)
                do (multiple-value-bind (address nonce balance code-hash commitment storage-root n)
                       (read-checkpoint-account src version)
                     (let ((a (%make-account address)))
                       (setf (account-nonce a) nonce (account-balance a) balance
                             (account-code-hash a) code-hash (account-commitment a) commitment
                             (account-storage-root a) storage-root (account-slot-count a) n)
                       (setf (gethash (key-account address) entries) (encode-account-record a)))
                     (when (plusp n)
                       (let ((trie (make-secure-trie :store nodes)))
                         (loop repeat n
                               do (let* ((slot (get-int src)) (value (get-int src)))
                                    (setf (gethash (key-slot address slot) entries) (encode-slot-value value))
                                    (trie-put trie (slot-octets slot) (rlp-encode value))))
                         (let ((actual (trie-root trie)))
                           (unless (octets= actual storage-root)
                             (format-error "~A: storage of ~40,'0X hashes to ~A, recorded ~A" path address
                                           (octets->hex actual) (octets->hex storage-root))))
                         (write-nodes nodes entries :visits (list (cons (octets->integer storage-root) nil)))))
                     (push (cons (address-octets address)
                                 (encode-account nonce balance storage-root code-hash commitment))
                           leaves)
                     (incf accounts)
                     (spill)))
          (unless (octets= (get-raw src 8) +checkpoint-end+) (format-error "~A: bad trailer" path))
          (funcall log "store: imported ~D accounts of checkpoint ~D; building the account trie" accounts number)
          (let ((trie (make-secure-trie :store nodes)))
            (dolist (l leaves) (trie-put trie (car l) (cdr l)))
            (let ((actual (trie-root trie)))
              (unless (octets= actual root)
                (error 'store-root-mismatch :number number :expected root :actual actual))
              (unless (octets= root +empty-trie-root+)
                (write-nodes nodes entries :refs (list (cons (octets->integer root) t))))))
          (setf (db-number head-db) number (db-hash head-db) hash (db-root-hash head-db) root
                (db-account-total head-db) accounts)
          (setf (gethash (key-head) entries) (encode-head head-db))
          (spill t)
          number)))))

(defun import-legacy-datadir (dir n log)
  "Import the format 1 datadir DIR (checkpoint N and its log) into a KV store."
  (funcall log "store: importing format 1 checkpoint ~D of ~A" n dir)
  (dolist (p (directory (dir-file dir "seg-*.kv"))) (delete-file p))
  (let ((kv (%make-kv dir)))
    (setf (kv-background kv) nil)
    (import-checkpoint kv (checkpoint-path dir n) log)
    (write-kv-manifest kv (mapcar #'kseg-name (kv-segments kv)))
    (kv-merge-tiers kv))
  ;; the old log, replayed on the imported state and flushed
  (let* ((kv (open-kv dir))
         (db (open-backed-statedb kv (make-cache-limits)))
         (path (dir-file dir "log-~12,'0D.wal" n))
         (count 0))
    (when (probe-file path)
      (read-legacy-records path
                           (lambda (payload)
                             (let* ((diff (decode-block-diff payload))
                                    (actual (db-apply-diff db diff)))
                               (unless (octets= actual (block-diff-root diff))
                                 (error 'store-root-mismatch :number (block-diff-number diff)
                                                             :expected (block-diff-root diff) :actual actual))
                               (incf count))))
      (kv-write kv (sorted-entries (db-flush-entries db)))
      (db-after-flush db))
    (kv-close kv)                       ; its merges finish before the store reopens DIR
    (funcall log "store: imported checkpoint ~D and ~D logged blocks" n count)
    (dolist (p (list (checkpoint-path dir n) path))
      (when (probe-file p) (delete-file p)))))
