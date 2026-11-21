(in-package #:clempo.store.tests)

;;; Backed state databases and datadirs: the same random blocks as the in-RAM tests, run on a
;;; database over the KV store with caches far smaller than the state (so every read goes
;;; through eviction and reloads), checked against an in-RAM mirror and a root built from
;;; scratch; restarts, torn and corrupt logs, damaged segments, format 1 imports, and
;;; processes killed mid-write.

(defparameter *tiny-cache*
  (make-cache-limits :accounts 3 :slots 6 :trie-nodes 8 :code-bytes 4 :dirty 60)
  "Limits well below the test state: every flush evicts.")

(defmacro with-temp-dir ((dir) &body body)
  `(let ((,dir (temp-dir)))
     (unwind-protect (progn ,@body)
       (when (probe-file ,dir) (sb-ext:delete-directory ,dir :recursive t)))))

(defun open-test-store (dir &rest args &key (genesis #'genesis) (cache *tiny-cache*)
                                            (sync '(:group 1000)) &allow-other-keys)
  (apply #'open-store dir :genesis genesis :cache cache :sync sync
         (loop for (k v) on args by #'cddr unless (member k '(:genesis :cache :sync)) append (list k v))))

(defun mirror-db ()
  "An in-RAM database at the test genesis."
  (let ((db (make-statedb))) (genesis db) db))

(defun commit-random (store rng n &optional mirror)
  "Run random block N on STORE (and the same diff on MIRROR); returns the diff."
  (let ((db (store-db store)))
    (random-block db rng n)
    (let ((diff (db-commit-block db n (block-hash n))))
      (store-commit-block store diff)
      (when mirror (is-equalp (db-root-hash db) (db-apply-diff mirror diff) "mirror at ~D" n))
      diff)))

(defun node-census (db)
  "(values stored reachable): trie node records in DB's store, and the nodes reachable from its
head root (through storage roots too)."
  (let ((kv (clempo.store::db-backing db)) (stored 0) (seen (make-hash-table)))
    (clempo.store::kv-map-range kv (clempo.store::key-node 0) (clempo.store::key-node (ash 1 256))
                                (lambda (k v) (declare (ignore k v)) (incf stored)))
    (labels ((walk (h account-p)
               (unless (gethash h seen)
                 (setf (gethash h seen) t)
                 (let ((v (clempo.store::kv-get kv (clempo.store::key-node h))))
                   (is v "reachable node ~X missing" h)
                   (when v
                     (dolist (c (clempo.store::node-references
                                 (subseq v (nth-value 1 (clempo.store::node-value-split v))) account-p))
                       (walk (car c) (cdr c))))))))
      (unless (octets= (db-root-hash db) +empty-trie-root+)
        (walk (octets->integer (db-root-hash db)) t)))
    (values stored (hash-table-count seen))))

(deftest backed-state-matches-memory-under-tiny-caches ()
  (with-temp-dir (dir)
    (let* ((rng (sb-ext:seed-random-state 31))
           (store (open-test-store dir :interval 3))
           (db (store-db store))
           (mirror (mirror-db)))
      (is (db-backed-p db))
      (loop for n from 1 to 45
            do (commit-random store rng n mirror)
               (is-equalp (reference-root db) (db-root-hash db) "fresh root at ~D" n)
               (when (zerop (mod n 3))
                 ;; just flushed and trimmed
                 (is (<= (getf (db-cache-stats db) :accounts) 3) "~S" (db-cache-stats db))
                 (is (zerop (getf (db-cache-stats db) :pending-nodes)))))
      (is (equal (snapshot mirror) (snapshot db)))
      (is= (db-account-count mirror) (db-account-count db))
      (check-proofs db)
      ;; the store holds exactly the nodes of the head state: nothing leaked, nothing missing
      (store-flush store)
      (multiple-value-bind (stored reachable) (node-census db)
        (is= reachable stored "stored trie nodes"))
      (close-store store))))

(deftest backed-reverts-and-speculation-leave-no-trace ()
  (with-temp-dir (dir)
    (let* ((rng (sb-ext:seed-random-state 32))
           (store (open-test-store dir :interval 4))
           (db (store-db store))
           (mirror (mirror-db)))
      (loop for n from 1 to 10 do (commit-random store rng n mirror))
      (loop repeat 6
            do (let ((before (snapshot db)) (root (db-state-root db)))
                 (random-block db rng 11 :writes 60)
                 (db-state-root db)
                 (db-revert-block db)
                 (is (equal before (snapshot db)))
                 (is-equalp root (db-state-root db))))
      (let ((before (snapshot db)) (root (db-root-hash db)))
        (with-speculative-blocks (db)
          (loop for n from 11 to 16
                do (random-block db rng n :writes 50)
                   (db-commit-block db n (block-hash n))
                   (is-equalp (reference-root db) (db-root-hash db) "speculative ~D" n)))
        (is= 10 (db-number db))
        (is (equal before (snapshot db)))
        (is-equalp root (db-root-hash db)))
      ;; what the store holds after more blocks and a flush is the mirror's state
      (loop for n from 11 to 14 do (commit-random store rng n mirror))
      (close-store store :checkpoint t)
      (let ((again (open-test-store dir)))
        (is= 14 (db-number (store-db again)))
        (is (equal (snapshot mirror) (snapshot (store-db again))))
        (multiple-value-bind (stored reachable) (node-census (store-db again))
          (is= reachable stored "stored trie nodes after speculation"))
        (close-store again)))))

(deftest store-survives-restarts-with-a-root-check ()
  (with-temp-dir (dir)
    (let* ((rng (sb-ext:seed-random-state 3))
           (mirror (mirror-db))
           (store (open-test-store dir :interval 8)))
      (loop for n from 1 to 20 do (commit-random store rng n mirror))
      (close-store store)
      ;; flushed at 16, blocks 17..20 replayed from the log
      (let ((again (open-test-store dir)))
        (is= 20 (db-number (store-db again)))
        (is-equalp (db-root-hash mirror) (db-root-hash (store-db again)))
        (is (equal (snapshot mirror) (snapshot (store-db again))))
        (is-equalp (block-hash 19) (db-block-hash (store-db again) 19))
        (commit-random again rng 21 mirror)
        (close-store again :checkpoint t))
      (let ((third (open-test-store dir)))
        (is= 21 (db-number (store-db third)))
        (is (equal (snapshot mirror) (snapshot (store-db third))))
        (close-store third)))))

(defun last-wal (dir) (cdr (car (last (clempo.store::wal-files (pathname dir))))))

(deftest store-cuts-only-a-torn-last-record ()
  (with-temp-dir (dir)
    (let ((rng (sb-ext:seed-random-state 9)))
      (let ((store (open-test-store dir :cache (make-cache-limits)))) ; no flush: one log
        (loop for n from 1 to 5 do (commit-random store rng n))
        (close-store store))
      ;; chop the last record in half: a write a crash cut short
      (let* ((wal (last-wal dir)) (bytes (clempo.io:read-file-octets wal)))
        (clempo.io:write-file-octets wal (subseq bytes 0 (- (length bytes) 7))))
      (let ((again (open-test-store dir)))
        (is= 4 (db-number (store-db again)))
        (random-block (store-db again) rng 5)
        (store-commit-block again (db-commit-block (store-db again) 5 (block-hash 5)))
        (close-store again))
      (let ((third (open-test-store dir)))
        (is= 5 (db-number (store-db third)))
        (close-store third)))))

(deftest store-refuses-a-log-corrupt-before-its-end ()
  ;; A damaged record with valid records after it is corruption: opening signals and the
  ;; log keeps every byte (a torn-tail cut would silently drop blocks 3..5).
  (with-temp-dir (dir)
    (let ((rng (sb-ext:seed-random-state 10)))
      (let ((store (open-test-store dir :cache (make-cache-limits))))
        (loop for n from 1 to 5 do (commit-random store rng n))
        (close-store store))
      (let* ((wal (last-wal dir))
             (bytes (clempo.io:read-file-octets wal))
             (second (search (string->octets "CLWR") bytes :start2 1)))
        (is second)
        ;; flip a payload byte of the second record, then a length byte of it
        (dolist (offset (list (+ second 20) (+ second 5)))
          (let ((damaged (copy-seq bytes)))
            (setf (aref damaged offset) (logxor #x40 (aref damaged offset)))
            (clempo.io:write-file-octets wal damaged)
            (signals store-corrupt-log (open-test-store dir))
            (is= (length bytes) (length (clempo.io:read-file-octets wal)) "log truncated")))
        (clempo.io:write-file-octets wal bytes))
      (let ((again (open-test-store dir)))
        (is= 5 (db-number (store-db again)))
        (close-store again)))))

(deftest store-recovers-from-a-flush-cut-short ()
  (with-temp-dir (dir)
    (let ((rng (sb-ext:seed-random-state 11)) (mirror (mirror-db)) (old-wal nil))
      (let ((store (open-test-store dir)))
        (loop for n from 1 to 6 do (commit-random store rng n mirror))
        (let ((wal (last-wal dir)))
          (setf old-wal (cons wal (clempo.io:read-file-octets wal))))
        (commit-random store rng 7 mirror)
        (store-flush store)
        (commit-random store rng 8 mirror)
        (close-store store))
      ;; a crash after the segment and MANIFEST but before the old log was deleted, and a
      ;; segment file a later flush wrote but never named
      (clempo.io:write-file-octets (car old-wal) (cdr old-wal))
      (clempo.io:write-file-octets (format nil "~Aseg-000000009999.kv" dir) (octets-of 1 2 3))
      (let ((again (open-test-store dir)))
        (is= 8 (db-number (store-db again)))
        (is (equal (snapshot mirror) (snapshot (store-db again))))
        (is (not (probe-file (format nil "~Aseg-000000009999.kv" dir))))
        (close-store again)))))

(deftest damaged-segments-signal ()
  (with-temp-dir (dir)
    (let ((rng (sb-ext:seed-random-state 12)))
      (let ((store (open-test-store dir)))
        (loop for n from 1 to 6 do (commit-random store rng n))
        (store-flush store)
        (close-store store))
      (let* ((seg (first (directory (format nil "~Aseg-*.kv" dir))))
             (bytes (clempo.io:read-file-octets seg)))
        ;; a byte inside the first data block: its checksum no longer matches
        (setf (aref bytes 30) (logxor 1 (aref bytes 30)))
        (clempo.io:write-file-octets seg bytes)
        (signals store-format-error
          (let ((s (open-test-store dir)))
            (unwind-protect (snapshot (store-db s)) (close-store s))))
        ;; a footer pointing outside the file: the segment does not open
        (let ((n (length bytes)))
          (replace bytes (integer->octets (* 2 n) 8) :start1 (- n 40))
          (clempo.io:write-file-octets seg bytes))
        (signals store-format-error (open-test-store dir))))))

(defun legacy-record (payload)
  (concatenate 'octets (integer->octets (length payload) 4) payload
               (integer->octets (ldb (byte 32 0) (octets->integer (clempo.keccak:keccak256 payload))) 4)))

(deftest format-1-datadirs-are-imported ()
  (with-temp-dir (dir)
    (let ((rng (sb-ext:seed-random-state 13)) (mirror (mirror-db)) (log '()))
      ;; a format 1 datadir: checkpoint 10, blocks 11..15 in its log
      (loop for n from 1 to 10 do (random-block mirror rng n) (db-commit-block mirror n (block-hash n)))
      (ensure-directories-exist dir)
      (write-checkpoint mirror (format nil "~Acheckpoint-~12,'0D.state" dir 10))
      (loop for n from 11 to 15
            do (random-block mirror rng n)
               (push (legacy-record (encode-block-diff (db-commit-block mirror n (block-hash n)))) log))
      (clempo.io:write-file-octets (format nil "~Alog-~12,'0D.wal" dir 10) (apply #'concatenate 'octets (reverse log)))
      (clempo.io:write-file-octets (format nil "~AMANIFEST" dir) (string->octets (format nil "clempo-store 1 10~%")))
      (let ((store (open-test-store dir :genesis nil)))
        (is= 15 (db-number (store-db store)))
        (is-equalp (db-root-hash mirror) (db-root-hash (store-db store)))
        (is (equal (snapshot mirror) (snapshot (store-db store))))
        (is-equalp (block-hash 7) (db-block-hash (store-db store) 7))
        (multiple-value-bind (stored reachable) (node-census (store-db store))
          (is= reachable stored "stored trie nodes after the import"))
        (close-store store))
      (is (not (probe-file (format nil "~Acheckpoint-~12,'0D.state" dir 10))))
      (is (not (probe-file (format nil "~Alog-~12,'0D.wal" dir 10)))))))

;;; Processes killed mid-write. A child process commits random blocks with the :BLOCK sync
;;; policy and reports each block STORE-COMMIT-BLOCK acknowledged; the parent kills it with
;;; SIGKILL at a random point (in a block, a log write or a flush), reopens the datadir and
;;; checks that every acknowledged block is there and the state is exactly the blocks'.

(defun read-acks (fd)
  "Block numbers (u64 each) the child wrote to FD until it closed."
  (let ((buf (make-octets 8)) (acks '()))
    (sb-sys:with-pinned-objects (buf)
      (loop (let ((got 0))
              (loop while (< got 8)
                    do (let ((n (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf) got) (- 8 got))))
                         (if (and n (plusp n)) (incf got n) (return))))
              (if (= got 8) (push (octets->integer buf) acks) (return)))))
    (nreverse acks)))

(defun crash-child (dir seed write-fd)
  "The child: commit blocks forever, acknowledging each on WRITE-FD."
  (handler-case
      (let ((store (open-test-store dir :sync :block :interval 5 :cache *tiny-cache*))
            (rng (sb-ext:seed-random-state seed))
            (buf (make-octets 8)))
        (loop for n from 1 to 100000
              do (random-block (store-db store) rng n)
                 (store-commit-block store (db-commit-block (store-db store) n (block-hash n)))
                 (replace buf (integer->octets n 8))
                 (sb-sys:with-pinned-objects (buf)
                   (sb-unix:unix-write write-fd buf 0 8))))
    (error () nil))
  (sb-ext:exit :code 1 :abort t))

(deftest killed-writers-lose-no-acknowledged-block ()
  (let ((kill-rng (sb-ext:seed-random-state 77)))
    (dotimes (round 4)
      (with-temp-dir (dir)
        (let ((seed (+ 100 round)))
          (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
            (let ((pid (sb-posix:fork)))
              (when (zerop pid)
                (sb-posix:close read-fd)
                (crash-child dir seed write-fd))
              (sb-posix:close write-fd)
              ;; let it acknowledge a few blocks, then kill it wherever it is
              (let ((want (+ 3 (random 12 kill-rng))) (buf (make-octets 8)) (seen 0))
                (sb-sys:with-pinned-objects (buf)
                  (loop while (< seen want)
                        do (if (plusp (or (sb-unix:unix-read read-fd (sb-sys:vector-sap buf) 8) 0))
                               (incf seen)
                               (return))))
                (sleep (/ (random 30 kill-rng) 1000))
                (sb-posix:kill pid sb-posix:sigkill)
                (sb-posix:waitpid pid 0)
                (let* ((rest (read-acks read-fd))
                       (acked (if rest (car (last rest)) seen)))
                  (sb-posix:close read-fd)
                  (is (>= acked want) "round ~D: ~D acknowledged" round acked)
                  (let* ((store (open-test-store dir))
                         (db (store-db store))
                         (mirror (mirror-db))
                         (rng (sb-ext:seed-random-state seed)))
                    (unwind-protect
                         (progn
                           (is (>= (db-number db) acked) "round ~D: head ~D below acknowledged ~D"
                               round (db-number db) acked)
                           (loop for n from 1 to (db-number db)
                                 do (random-block mirror rng n) (db-commit-block mirror n (block-hash n)))
                           (is-equalp (db-root-hash mirror) (db-root-hash db) "round ~D" round)
                           (is (equal (snapshot mirror) (snapshot db)) "round ~D state" round))
                      (close-store store))))))))))))

;;; Bounded RAM over a state far larger than the caches: thousands of accounts and slots,
;;; replayed on a backed database whose caches hold a few hundred, every root checked
;;; against an in-RAM mirror.

(deftest a-large-state-replays-under-small-caches ()
  (with-temp-dir (dir)
    (let* ((cache (make-cache-limits :accounts 64 :slots 256 :trie-nodes 512 :code-bytes 256 :dirty 4000))
           (store (open-test-store dir :cache cache :interval 0))
           (db (store-db store))
           (mirror (mirror-db))
           (rng (sb-ext:seed-random-state 41))
           (peak-accounts 0))
      (loop for n from 1 to 60
            do (random-block db rng n :accounts 3000 :writes 400)
               (let ((diff (db-commit-block db n (block-hash n))))
                 (store-commit-block store diff)
                 (is-equalp (db-root-hash db) (db-apply-diff mirror diff) "root at ~D" n))
               (setf peak-accounts (max peak-accounts (getf (db-cache-stats db) :accounts))))
      (is (> (db-account-count db) 1000) "~D accounts" (db-account-count db))
      (store-flush store)
      (is (<= (getf (db-cache-stats db) :accounts) 64))
      (is (equal (snapshot mirror) (snapshot db)))
      (close-store store))))
