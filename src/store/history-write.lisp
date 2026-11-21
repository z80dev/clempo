(in-package #:clempo.history)

;;; Writing a history: a job appends committed blocks to the tail and flushes it into
;;; segments; its own segments merge in tiers of +MERGE-FAN-IN+ so a lookup crosses few files.

(defconstant +merge-fan-in+ 8)

(defun fsync-output (stream)
  (finish-output stream)
  (sb-posix:fsync (sb-sys:fd-stream-fd stream)))

(defun fsync-dir (dir)
  (let ((fd (sb-posix:open (namestring dir) sb-posix:o-rdonly)))
    (unwind-protect (sb-posix:fsync fd) (sb-posix:close fd))))

(defun write-file-durably (path writer)
  "Call WRITER with an octet stream on a temporary file, fsync it and rename it to PATH."
  (let ((tmp (make-pathname :type (format nil "~A-tmp" (pathname-type path)) :defaults path)))
    (with-open-file (out tmp :element-type 'octet :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
      (funcall writer out)
      (fsync-output out))
    (sb-posix:rename (namestring tmp) (namestring path))
    (fsync-dir (make-pathname :name nil :type nil :defaults path))
    path))

;;; The segment writer: pages of prefix-compressed records, then the index, wipes and footer.

(defstruct (segment-writer (:constructor make-segment-writer (stream)))
  stream
  (page (make-obuf 8192) :type obuf)
  (offset 0 :type index)                ; file offset of the page being filled
  (index '())                           ; ((offset first-block key) ...) newest first
  (prev (make-octets 53) :type octets) (prev-len 0 :type fixnum)
  (key (make-octets 53) :type octets)
  (versions 0 :type index))

(defun writer-flush-page (w)
  (let ((page (segment-writer-page w)))
    (when (plusp (obuf-pos page))
      (write-sequence (obuf-bytes page) (segment-writer-stream w) :end (obuf-pos page))
      (incf (segment-writer-offset w) (obuf-pos page))
      (setf (obuf-pos page) 0))))

(defun writer-record (w key first count body &key (body-start 0) (body-end (length body)))
  "Append a record of KEY (an integer) whose COUNT versions start at block FIRST; BODY holds
the encoded versions."
  (let ((page (segment-writer-page w)))
    (when (>= (obuf-pos page) +page-size+) (writer-flush-page w))
    (let* ((kbuf (segment-writer-key w))
           (klen (key-into key kbuf))
           (start-p (zerop (obuf-pos page)))
           (prev (segment-writer-prev w))
           (shared (if start-p 0
                       (loop for i below (min klen (segment-writer-prev-len w))
                             while (= (aref kbuf i) (aref prev i)) count t))))
      (when start-p (push (list (segment-writer-offset w) first key) (segment-writer-index w)))
      (put8 page shared)
      (put8 page (- klen shared))
      (put-octets page kbuf :start shared :end klen)
      (put-varint page first)
      (put-varint page count)
      (put-varint page (- body-end body-start))
      (put-octets page body :start body-start :end body-end)
      (replace prev kbuf :end2 klen)
      (setf (segment-writer-prev-len w) klen)
      (incf (segment-writer-versions w) count))))

(defun writer-finish (w &key from to full-p level wipes)
  "Write the index, wipes and footer after the last record."
  (writer-flush-page w)
  (let ((b (make-obuf)) (index (reverse (segment-writer-index w)))
        (index-offset (segment-writer-offset w)))
    (put-u64 b (length index))
    (loop for (offset first key) in index
          do (put-u64 b offset) (put-u64 b first)
             (let ((n (key-length key))) (put8 b n) (put-be b key n)))
    (let ((wipes-offset (+ index-offset (obuf-pos b))))
      (put-u64 b (length wipes))
      (loop for (address . block) in wipes do (put-be b address 20) (put-u64 b block))
      (put-octets b +magic+)
      (dolist (n (list from to (if full-p 1 0) level index-offset wipes-offset (segment-writer-versions w)))
        (put-u64 b n))
      (put-octets b +magic+))
    (write-sequence (obuf-bytes b) (segment-writer-stream w) :end (obuf-pos b))))

(defun encode-versions (body versions start end accountp)
  "Encode VERSIONS[START, END) (conses (block . value)) into BODY (reset first)."
  (setf (obuf-pos body) 0)
  (loop with prev = (car (aref versions start))
        for i from start below end
        for (block . value) = (aref versions i)
        do (put-varint body (- block prev))
           (setf prev block)
           (if accountp (encode-aver body value) (put-word body value))))

(defun write-tail-segment (dir job level tail &key full-p)
  "Write TAIL as a segment file in DIR; returns its path."
  (let* ((keys (sort (loop for k being the hash-keys of (tail-keys tail) collect k) #'<))
         (path (dir-path dir (segment-name job level (tail-from tail) (tail-to tail))))
         (body (make-obuf 4096)))
    (write-file-durably
     path
     (lambda (out)
       (let ((w (make-segment-writer out)))
         (dolist (key keys)
           (let* ((versions (gethash key (tail-keys tail)))
                  (n (fill-pointer versions))
                  (accountp (< key +slot-tag+)))
             (loop for start from 0 below n by +chunk-versions+
                   for end = (min n (+ start +chunk-versions+))
                   do (encode-versions body versions start end accountp)
                      (writer-record w key (car (aref versions start)) (- end start)
                                     (obuf-bytes body) :body-end (obuf-pos body)))))
         (writer-finish w :from (tail-from tail) :to (tail-to tail) :full-p full-p :level level
                          :wipes (reverse (tail-wipes tail))))))
    path))

;;; The writer API

(defun account-aver (db address &optional wiped-p)
  (let ((a (store:db-account db address)))
    (if a
        (make-aver nil wiped-p (store:account-nonce a) (store:account-balance a)
                   (store:account-code-hash a) (plusp (store:account-slot-count a)))
        (make-aver t wiped-p 0 0 +empty-code-hash+ nil))))

(defun open-code-output (history job)
  (open (code-file-path (history-dir history) job) :element-type 'octet :direction :output
                                                    :if-exists :append :if-does-not-exist :create))

(defun history-start-writer (history job db)
  "Make HISTORY's tail writable for build job JOB, continuing from DB (the state after the
last block already in the history, or the state a full segment is about to record)."
  (setf (history-job history) job
        (history-tail history) (make-tail (1+ (store:db-number db)))
        (history-code-out history) (open-code-output history job)
        (history-last-accounts history) (make-hash-table :size (max 1024 (store:db-account-count db))))
  (let ((last (history-last-accounts history)))
    (store:map-accounts (lambda (address a)
                          (declare (ignore a))
                          (setf (gethash address last) (account-aver db address)))
                        db))
  history)

(defun write-code (history code)
  "Append CODE to the job's code file (unless known) and make it readable at once."
  (let* ((hash (keccak256 code)) (key (octets->integer hash)))
    (unless (gethash key (history-codes history))
      (let ((out (history-code-out history)) (len (make-octets 4)))
        (write-sequence hash out)
        (setf (aref len 0) (ldb (byte 8 24) (length code)) (aref len 1) (ldb (byte 8 16) (length code))
              (aref len 2) (ldb (byte 8 8) (length code)) (aref len 3) (ldb (byte 8 0) (length code)))
        (write-sequence len out)
        (write-sequence code out))
      (setf (gethash key (history-codes history)) code))))

(defun note-account (history tail address block v)
  "Record account version V unless it repeats the last one (a wipe always records)."
  (let* ((last (history-last-accounts history))
         (old (gethash address last)))
    (when (or (null old) (aver-wiped-p v) (aver-deleted-p v) (not (aver-same-p old v)))
      (tail-add tail address block v)
      (setf (gethash address last) v))
    (when (or (aver-wiped-p v) (aver-deleted-p v))
      (push (cons address block) (tail-wipes tail)))))

(defun history-record-block (history db diff)
  "Append the committed block DIFF (from clempo.store:db-commit-block) of DB, which must
still be at that block, to the tail."
  (let ((tail (history-tail history))
        (block (store:block-diff-number diff)))
    (invariant (= block (1+ (tail-to* tail))) "history: block ~D after ~D" block (tail-to* tail))
    (dolist (code (store:block-diff-codes diff)) (write-code history code))
    (dolist (d (store:block-diff-accounts diff))
      (let ((address (store:account-diff-address d)))
        (note-account history tail address block
                      (account-aver db address (or (store:account-diff-wiped-p d) (store:account-diff-deleted-p d))))
        (loop for (slot . value) in (store:account-diff-slots d)
              do (tail-add tail (slot-key address slot) block value))))
    (setf (tail-to tail) block)
    history))

(defun tail-to* (tail) (if (< (tail-to tail) (tail-from tail)) (1- (tail-from tail)) (tail-to tail)))

(defun history-record-state (history db)
  "Write DB's whole state as a full segment at its block (a genesis or a seed checkpoint);
the tail must be empty and start at that block."
  (let ((tail (history-tail history)) (block (store:db-number db)))
    (invariant (zerop (tail-versions tail)) "history: full state into a non-empty tail")
    (setf (tail-from tail) block (tail-to tail) block)
    (store:map-codes (lambda (key code) (declare (ignore key)) (write-code history code)) db)
    (store:map-accounts
     (lambda (address a)
       (tail-add tail address block (account-aver db address))
       (store:map-account-storage (lambda (slot value) (tail-add tail (slot-key address slot) block value)) a))
     db)
    (history-flush history :full-p t :db db)))

(defun history-tail-blocks (history)
  (let ((tail (history-tail history)))
    (if tail (max 0 (1+ (- (tail-to tail) (tail-from tail)))) 0)))

(defun history-tail-versions (history)
  (let ((tail (history-tail history))) (if tail (tail-versions tail) 0)))

(defun job-segments (history job)
  (remove-if-not (lambda (s) (eql (segment-job* s) job)) (history-segments history)))

(defun segment-job* (seg)
  (nth-value 0 (parse-segment-name (file-namestring (segment-path seg)))))

(defun history-flush (history &key full-p db)
  "Write the tail as a segment (nothing when it is empty), sync the job's codes, then, with
DB (the state after the tail's last block), the job's resume checkpoint; merge the job's
segments when a tier is full. Starts a new tail after it."
  (let* ((tail (history-tail history))
         (job (history-job history))
         (dir (history-dir history)))
    (when (>= (tail-to tail) (tail-from tail))
      (fsync-output (history-code-out history))
      (write-tail-segment dir job 0 tail :full-p full-p)
      (when db
        (invariant (= (store:db-number db) (tail-to tail)) "history: checkpoint at ~D for a tail ending at ~D"
                   (store:db-number db) (tail-to tail))
        (store:write-checkpoint db (job-state-path dir job)))
      (setf (history-tail history) (make-tail (1+ (tail-to tail))))
      (history-refresh history)
      (merge-tiers history job))
    history))

;;; Merging

(defstruct (cursor (:constructor make-cursor (seg pos end)))
  seg (pos 0 :type fixnum) (end 0 :type fixnum)
  (kbuf (make-octets 53) :type octets) (klen 0 :type fixnum)
  (key 0) (first 0) (count 0) (body-start 0 :type fixnum) (body-end 0 :type fixnum))

(defun cursor-next (c)
  "Advance to the next record; NIL at the end."
  (let ((sap (segment-sap (cursor-seg c))) (pos (cursor-pos c)))
    (declare (type fixnum pos))
    (when (< pos (cursor-end c))
      (with-sap-reader (sap pos)
        (let* ((shared (u8)) (suffix (u8)) (kbuf (cursor-kbuf c)))
          (dotimes (i suffix) (setf (aref kbuf (+ shared i)) (sb-sys:sap-ref-8 sap (+ pos i))))
          (incf pos suffix)
          (setf (cursor-klen c) (+ shared suffix)
                (cursor-key c) (let ((k (octets->integer kbuf :start 1 :end (+ shared suffix))))
                                 (if (= (+ shared suffix) 53) (logior +slot-tag+ k) k))
                (cursor-first c) (varint)
                (cursor-count c) (varint))
          (let ((len (varint)))
            (setf (cursor-body-start c) pos (cursor-body-end c) (+ pos len)
                  (cursor-pos c) (+ pos len)))))
      c)))

(defun merge-segments (history segments)
  "Merge SEGMENTS (contiguous, oldest first, of one job) into one segment; returns its path."
  (let* ((dir (history-dir history))
         (first (first segments)) (last (car (last segments)))
         (job (segment-job* first))
         (level (1+ (reduce #'max segments :key #'segment-level)))
         (path (dir-path dir (segment-name job level (segment-from first) (segment-to last))))
         (cursors (loop for s in segments
                        for c = (cursor-next (make-cursor s 0 (segment-index-offset s)))
                        when c collect c)))
    (write-file-durably
     path
     (lambda (out)
       (let ((w (make-segment-writer out)) (body (make-obuf 4096)))
         (loop while cursors
               do (let ((best (first cursors)))
                    (dolist (c (rest cursors))
                      (when (key-before-p (cursor-key c) (cursor-first c) (cursor-key best) (cursor-first best))
                        (setf best c)))
                    (let* ((n (- (cursor-body-end best) (cursor-body-start best))))
                      (setf (obuf-pos body) 0)
                      (obuf-room body n)
                      (let ((sap (segment-sap (cursor-seg best))) (bytes (obuf-bytes body)) (s (cursor-body-start best)))
                        (dotimes (i n) (setf (aref bytes i) (sb-sys:sap-ref-8 sap (+ s i)))))
                      (writer-record w (cursor-key best) (cursor-first best) (cursor-count best)
                                     (obuf-bytes body) :body-end n))
                    (unless (cursor-next best) (setf cursors (remove best cursors)))))
         (writer-finish w :from (segment-from first) :to (segment-to last) :full-p (segment-full-p first)
                          :level level :wipes (loop for s in segments append (segment-wipes s))))))
    ;; The merged file replaces its inputs; a crash before these deletes leaves inputs that
    ;; HISTORY-REFRESH ignores as contained in it.
    (dolist (s segments) (delete-file (segment-path s)))
    path))

(defun merge-tiers (history job)
  "While the job's newest +MERGE-FAN-IN+ non-full segments share a level, merge them."
  (loop
    (let* ((own (sort (remove-if #'segment-full-p (job-segments history job)) #'< :key #'segment-from))
           (n (length own)))
      (unless (>= n +merge-fan-in+) (return))
      (let* ((group (last own +merge-fan-in+))
             (level (segment-level (first group))))
        (unless (every (lambda (s) (= (segment-level s) level)) group) (return))
        (merge-segments history group)
        (history-refresh history)))))

(defun history-compact (history &key (job nil job-p))
  "Merge every run of contiguous non-full segments (of JOB, or of every job) into one
segment each. Do not run it on a job that is still writing."
  (let* ((segments (sort (remove-if #'segment-full-p
                                    (if job-p (job-segments history job) (history-segments history)))
                         #'< :key #'segment-from))
         (runs '()))
    (dolist (s segments)
      (let ((run (first runs)))
        (if (and run (= (segment-from s) (1+ (segment-to (car (last run)))))
                 (eql (segment-job* s) (segment-job* (first run))))
            (setf (first runs) (append run (list s)))
            (push (list s) runs))))
    (dolist (run runs)
      (when (rest run) (merge-segments history run)))
    (history-refresh history)))

(defun history-job-resume (history job)
  "The resume checkpoint of build JOB (a statedb at the end of the job's last flushed
segment) after deleting the job's segments written past it, or NIL for a job not started."
  (let ((path (job-state-path (history-dir history) job)))
    (when (probe-file path)
      (let ((db (store:read-checkpoint path)))
        (dolist (s (job-segments history job))
          (when (> (segment-to s) (store:db-number db))
            (delete-file (segment-path s))
            (setf (history-segments history) (remove s (history-segments history)))))
        db))))
