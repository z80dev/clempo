(in-package #:clempo.store)

;;; On disk a datadir holds one checkpoint of the whole state and the log of block diffs
;;; committed after it:
;;;
;;;   MANIFEST                 "clempo-store 1 <N>": the live checkpoint
;;;   checkpoint-<N>.state     the state after block N, written to a temp file, fsynced, renamed
;;;   log-<N>.wal              one framed record per block after N
;;;
;;; Opening loads the checkpoint, checks its root, replays the log (checking every block's
;;; root), cuts a torn tail record, and continues appending. A new checkpoint every K blocks
;;; replaces the old pair only after MANIFEST names it.


(define-constant +checkpoint-magic+ (string->octets "CLEMPOCK"))
(define-constant +checkpoint-end+ (string->octets "CLEMPOEN"))
(defconstant +format-version+ 1)

(define-condition store-root-mismatch (error)
  ((number :initarg :number :reader store-root-mismatch-number)
   (expected :initarg :expected :reader store-root-mismatch-expected)
   (actual :initarg :actual :reader store-root-mismatch-actual))
  (:report (lambda (c s)
             (format s "store: state root after block ~D is ~A, recorded ~A"
                     (store-root-mismatch-number c)
                     (octets->hex (store-root-mismatch-actual c))
                     (octets->hex (store-root-mismatch-expected c))))))

;;; Files

(defun fsync-stream (stream)
  (finish-output stream)
  (sb-posix:fsync (sb-sys:fd-stream-fd stream)))

(defun fsync-directory (dir)
  (let ((fd (sb-posix:open (namestring dir) sb-posix:o-rdonly)))
    (unwind-protect (sb-posix:fsync fd) (sb-posix:close fd))))

(defun rename-file* (from to)
  (sb-posix:rename (namestring from) (namestring to)))

(defun dir-file (dir fmt &rest args)
  (merge-pathnames (apply #'format nil fmt args) dir))

(defun checkpoint-path (dir n) (dir-file dir "checkpoint-~12,'0D.state" n))
(defun log-path (dir n) (dir-file dir "log-~12,'0D.wal" n))
(defun manifest-path (dir) (dir-file dir "MANIFEST"))

(defun write-durably (path writer)
  "Call WRITER with an octet output stream on PATH.tmp, fsync, rename over PATH."
  (let ((tmp (make-pathname :type (format nil "~A-tmp" (pathname-type path)) :defaults path)))
    (with-open-file (out tmp :element-type '(unsigned-byte 8) :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
      (funcall writer out)
      (fsync-stream out))
    (rename-file* tmp path)
    (fsync-directory (make-pathname :name nil :type nil :defaults path))
    path))

(defun read-manifest (dir)
  (let ((path (manifest-path dir)))
    (when (probe-file path)
      (let* ((text (string-trim '(#\Newline #\Space) (clempo.io:read-file-string path)))
             (words (let ((*read-eval* nil))
                      (with-input-from-string (s text)
                        (loop for w = (read s nil) while w collect w)))))
        (unless (and (= (length words) 3) (string-equal (symbol-name (first words)) "CLEMPO-STORE")
                     (eql (second words) +format-version+) (integerp (third words)))
          (format-error "bad MANIFEST ~S" text))
        (third words)))))

(defun write-manifest (dir n)
  (write-durably (manifest-path dir)
                 (lambda (out)
                   (write-sequence (string->octets (format nil "clempo-store ~D ~D~%" +format-version+ n))
                                   out))))

;;; Checkpoints

(defun write-checkpoint-to (db out)
  (let ((sink (make-sink out)))
    (put-raw sink +checkpoint-magic+)
    (put-u32 sink +format-version+)
    (put-u64 sink (db-number db))
    (put-raw sink (db-hash db))
    (put-raw sink (db-root-hash db))
    (put-u32 sink (hash-table-count (db-recent db)))
    (map-recent-hashes (lambda (n h) (put-u64 sink n) (put-raw sink h)) db)
    (put-u64 sink (hash-table-count (db-codes db)))
    (map-codes (lambda (key code) (put-raw sink (integer->octets key 32)) (put-bytes sink code)) db)
    (put-u64 sink (db-account-count db))
    (map-accounts (lambda (address a)
                    (put-raw sink (address-octets address))
                    (put-int sink (account-nonce a))
                    (put-int sink (account-balance a))
                    (put-raw sink (account-code-hash a))
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

(defun read-checkpoint (path &key (check-root t))
  "Load a checkpoint into a fresh state database; with CHECK-ROOT rebuild the account trie and
compare its root with the recorded one."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((src (make-source in))
          (db (make-statedb)))
      (unless (octets= (get-raw src 8) +checkpoint-magic+) (format-error "~A: not a checkpoint" path))
      (unless (= (get-u32 src) +format-version+) (format-error "~A: unknown version" path))
      (let ((number (get-u64 src)) (hash (get-raw src 32)) (root (get-raw src 32)))
        (loop repeat (get-u32 src) do (let ((n (get-u64 src))) (db-note-block-hash db n (get-raw src 32))))
        (let ((codes (db-codes db)))
          (loop repeat (get-u64 src)
                do (let ((key (octets->integer (get-raw src 32))))
                     (setf (gethash key codes) (get-bytes src)))))
        (let ((accounts (db-accounts db)) (dirty (db-dirty db)))
          (loop repeat (get-u64 src)
                do (let* ((address (octets->integer (get-raw src 20)))
                          (a (%make-account address)))
                     (setf (account-nonce a) (get-int src)
                           (account-balance a) (get-int src)
                           (account-code-hash a) (get-raw src 32)
                           (account-storage-root a) (get-raw src 32))
                     (let ((n (get-u64 src)))
                       (when (plusp n)
                         (let ((s (make-hash-table :size n)))
                           (loop repeat n do (let ((slot (get-int src))) (setf (gethash slot s) (get-int src))))
                           (setf (account-storage a) s))))
                     (setf (gethash address accounts) a
                           (gethash address dirty) t))))
        (unless (octets= (get-raw src 8) +checkpoint-end+) (format-error "~A: bad trailer" path))
        (setf (db-number db) number (db-hash db) hash)
        ;; Without CHECK-ROOT the account trie is built by the first DB-STATE-ROOT.
        (when check-root
          (let ((actual (db-state-root db)))
            (unless (octets= actual root)
              (error 'store-root-mismatch :number number :expected root :actual actual))))
        (setf (db-root-hash db) root)
        db))))

;;; The block log

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
      (put-u8 sink (logior (if (account-diff-deleted-p d) 1 0) (if (account-diff-wiped-p d) 2 0)))
      (unless (account-diff-deleted-p d)
        (put-int sink (account-diff-nonce d))
        (put-int sink (account-diff-balance d))
        (put-raw sink (account-diff-code-hash d))
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
                                :slots (loop repeat (get-u32 src)
                                             collect (let ((slot (get-int src))) (cons slot (get-int src))))))))))
    (unless (source-exhausted-p src) (format-error "trailing bytes in block record ~D" number))
    (make-block-diff :number number :hash hash :root root :accounts accounts :codes codes)))

(defun record-checksum (payload)
  (ldb (byte 32 0) (octets->integer (keccak256 payload))))

(defun put-record (sink payload)
  (put-u32 sink (length payload))
  (put-raw sink payload)
  (put-u32 sink (ldb (byte 32 0) (record-checksum payload))))

(defun read-records (path fn)
  "Call FN with each intact record payload of the log at PATH. Returns the byte offset after
the last intact record and whether a torn or corrupt tail followed it."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((src (make-source in)) (good 0) (total (file-length in)))
      (loop
        (when (source-exhausted-p src) (return (values good nil)))
        (unless (source-available-p src 4) (return (values good t)))
        (let ((len (get-u32 src)))
          (unless (and (<= (+ good 8 len) total) (source-available-p src (+ len 4)))
            (return (values good t)))
          (let ((payload (get-raw src len)) (sum (get-u32 src)))
            (unless (= sum (ldb (byte 32 0) (record-checksum payload)))
              (return (values good t)))
            (funcall fn payload)
            (incf good (+ 8 len))))))))

;;; The store

(defstruct (store (:constructor %make-store (dir db interval)))
  dir db
  (interval 0 :type integer)          ; blocks between checkpoints; 0 = only on request/close
  (checkpoint-number 0 :type integer)
  log-stream log-sink
  (unsynced 0 :type fixnum)
  (lock-fd nil))                       ; descriptor holding DIR/LOCK

(defun open-log (store)
  (let ((stream (open (log-path (store-dir store) (store-checkpoint-number store))
                      :element-type '(unsigned-byte 8) :direction :output
                      :if-exists :append :if-does-not-exist :create)))
    (setf (store-log-stream store) stream
          (store-log-sink store) (make-sink stream))))

(defun truncate-file (path length)
  (sb-posix:truncate (namestring path) length))

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

(defun open-store (dir &key genesis (interval 0) (log (constantly nil)))
  "Open the datadir DIR (created if absent) and hold its lock until CLOSE-STORE."
  (let* ((dir (let ((p (pathname dir)))
                (if (or (pathname-name p) (pathname-type p))
                    (make-pathname :directory (append (or (pathname-directory p) (list :relative))
                                                      (list (file-namestring p)))
                                   :name nil :type nil :defaults p)
                    p)))
         (fd (progn (ensure-directories-exist dir) (lock-datadir dir)))
         (store nil))
    (unwind-protect
         (progn (setf store (open-locked-store dir genesis interval log))
                (setf (store-lock-fd store) fd))
      (unless store (unlock-datadir fd)))
    store))

(defun open-locked-store (dir genesis interval log)
  "Open the datadir DIR (created if absent). An empty one is initialized by calling GENESIS
with a fresh state database, which must commit block 0 in it; the result is checkpoint 0.
Otherwise the live checkpoint is loaded, its root checked, and the log replayed with every
block's root checked. LOG is called with a format string and arguments for progress notes."
  (let ((n (read-manifest dir)))
    (if (null n)
        (let ((db (make-statedb)))
          (invariant genesis "empty datadir ~A and no genesis" dir)
          (funcall genesis db)
          (let ((store (%make-store dir db interval)))
            (setf (store-checkpoint-number store) (db-number db))
            (write-checkpoint db (checkpoint-path dir (db-number db)))
            (write-durably (log-path dir (db-number db)) (lambda (out) (declare (ignore out))))
            (write-manifest dir (db-number db))
            (open-log store)
            store))
        (let* ((db (read-checkpoint (checkpoint-path dir n)))
               (store (%make-store dir db interval))
               (path (log-path dir n))
               (count 0))
          (funcall log "store: checkpoint ~D loaded (~D accounts), root ~A" n (db-account-count db)
                   (octets->hex (db-root-hash db)))
          (setf (store-checkpoint-number store) n)
          (when (probe-file path)
            (multiple-value-bind (good torn)
                (read-records path
                              (lambda (payload)
                                (let* ((diff (decode-block-diff payload))
                                       (expected (block-diff-root diff)))
                                  (invariant (= (block-diff-number diff) (1+ (db-number db)))
                                             "log record ~D after block ~D" (block-diff-number diff) (db-number db))
                                  (let ((actual (db-apply-diff db diff)))
                                    (unless (octets= actual expected)
                                      (error 'store-root-mismatch :number (block-diff-number diff)
                                                                  :expected expected :actual actual)))
                                  (incf count))))
              (when torn
                (funcall log "store: cutting a torn log tail at byte ~D" good)
                (truncate-file path good))))
          (funcall log "store: replayed ~D logged blocks to ~D" count (db-number db))
          (open-log store)
          store))))

(defun store-db-number (store) (db-number (store-db store)))

(defun store-sync (store)
  "Write buffered log records and fsync them."
  (sink-flush (store-log-sink store))
  (fsync-stream (store-log-stream store))
  (setf (store-unsynced store) 0))

(defun store-commit-block (store diff)
  "Append a committed block's DIFF to the log; checkpoint when the interval says so."
  (let ((sink (store-log-sink store)))
    (put-record sink (encode-block-diff diff))
    (when (>= (incf (store-unsynced store)) 4096) (store-sync store)))
  (let ((k (store-interval store)))
    (when (and (plusp k) (zerop (mod (block-diff-number diff) k)))
      (store-checkpoint store)))
  diff)

(defun store-checkpoint (store)
  "Write a checkpoint at the current block and start a new log after it."
  (let* ((db (store-db store))
         (dir (store-dir store))
         (old (store-checkpoint-number store))
         (n (db-number db)))
    (unless (= n old)
      (store-sync store)
      (write-checkpoint db (checkpoint-path dir n))
      (close (store-log-stream store))
      (write-durably (log-path dir n) (lambda (out) (declare (ignore out))))
      (write-manifest dir n)
      (setf (store-checkpoint-number store) n)
      (open-log store)
      (dolist (path (list (checkpoint-path dir old) (log-path dir old)))
        (when (probe-file path) (delete-file path))))
    n))

(defun close-store (store &key checkpoint)
  (unwind-protect
       (progn
         (if checkpoint
             (store-checkpoint store)
             (store-sync store))
         (close (store-log-stream store))
         (setf (store-log-stream store) nil))
    (unlock-datadir (shiftf (store-lock-fd store) nil)))
  store)
