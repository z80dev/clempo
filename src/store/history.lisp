(defpackage #:clempo.history
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:store #:clempo.store))
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.trie #:+empty-code-hash+)
  (:documentation "Historical state: every account and storage slot as it was after any block.

A history directory holds immutable segment files and append-only code files:

  seg-<job>-<level>-<from>-<to>.hist   the versions written by blocks FROM..TO
  codes-<job>.dat                      code blobs by hash (hash, u32 length, bytes)*
  job-<job>.state                      a build job's resume checkpoint (clempo.store format)

A segment maps each key (an account, or a slot of an account) to the values it took in the
segment's blocks: post-block values, so the value after block N is the last version at or
before N. A FULL segment holds the whole state after its first block (a genesis or a seed
checkpoint); a lookup stops there. Keys are sorted, so a lookup is a binary search of the
segment's page index (in RAM) and a scan of one page of the memory-mapped file, newest
segment first. Storage wipes (a destroyed or re-created account) are kept per account so a
slot last written before a wipe reads as zero.

The tail is the in-RAM segment being written: HISTORY-RECORD-BLOCK appends a committed
block, HISTORY-FLUSH writes the tail out as a segment. Reads see the tail too, so a node can
append each block it follows and serve its states at once; appends must not run while views
read (the RPC backend's write lock).")
  (:export
   #:history #:open-history #:close-history #:history-dir #:history-refresh
   #:history-height #:history-ranges #:history-segment-count #:history-seeds
   #:history-state #:history-state-p #:history-state-number #:history-state-history
   #:history-account #:history-storage #:history-code #:history-storage-empty-p
   #:history-code-by-hash #:history-map-changes #:history-gap-seeds
   ;; writing
   #:history-start-writer #:history-record-block #:history-record-state #:history-flush
   #:history-tail-blocks #:history-tail-versions #:history-job #:history-job-resume
   #:history-compact
   #:job-state-path #:history-file-error))

(in-package #:clempo.history)

(deftype index () '(integer 0 #.(ash 1 61)))

(define-condition history-file-error (error)
  ((message :initarg :message :reader history-file-error-message))
  (:report (lambda (c s) (format s "history: ~A" (history-file-error-message c)))))

(defun file-error* (fmt &rest args)
  (error 'history-file-error :message (apply #'format nil fmt args)))

;;; Keys. An account is its address; a slot is 2^416 + address * 2^256 + slot. As bytes a key
;;; is a tag (0 account, 1 slot), the address and for slots the slot word, so integer order
;;; and byte order agree.

(defconstant +slot-tag+ (ash 1 416))

(declaim (inline slot-key))
(defun slot-key (address slot) (logior +slot-tag+ (ash address 256) slot))

(defun key-length (key) (if (< key +slot-tag+) 21 53))

(defun key-into (key buffer)
  "Write KEY's bytes into BUFFER; returns their count."
  (declare (type octets buffer) (type integer key))
  (let ((n (key-length key)))
    (setf (aref buffer 0) (if (= n 21) 0 1))
    (loop for i from (1- n) downto 1
          for k = key then (ash k -8)
          do (setf (aref buffer i) (ldb (byte 8 0) k)))
    n))

;;; Account versions

(defstruct (aver (:constructor make-aver (deleted-p wiped-p nonce balance code-hash storage-p
                                           &optional (commitment 0))))
  "An account after a block. DELETED-P: absent. WIPED-P: its storage was cleared in that block.
STORAGE-P: it has nonzero storage. COMMITMENT: its T14 configuration commitment (0: none)."
  deleted-p wiped-p (nonce 0) (balance 0) code-hash storage-p (commitment 0))

(defun aver-same-p (a b)
  "Equal fields (the wipe flag aside)."
  (and (eq (aver-deleted-p a) (aver-deleted-p b))
       (or (aver-deleted-p a)
           (and (eql (aver-nonce a) (aver-nonce b))
                (eql (aver-balance a) (aver-balance b))
                (octets= (aver-code-hash a) (aver-code-hash b))
                (eql (aver-commitment a) (aver-commitment b))
                (eq (aver-storage-p a) (aver-storage-p b))))))

;;; Byte output: a growable buffer

(defstruct (obuf (:constructor make-obuf (&optional (size 8192)
                                          &aux (bytes (make-octets size)))))
  (bytes (make-octets 0) :type octets)
  (pos 0 :type index))

(defun obuf-room (b n)
  (declare (type obuf b) (type index n))
  (let ((need (+ (obuf-pos b) n)))
    (when (> need (length (obuf-bytes b)))
      (let ((new (make-octets (max need (* 2 (length (obuf-bytes b)))))))
        (replace new (obuf-bytes b) :end2 (obuf-pos b))
        (setf (obuf-bytes b) new)))))

(defun put8 (b n)
  (obuf-room b 1)
  (setf (aref (obuf-bytes b) (obuf-pos b)) n)
  (incf (obuf-pos b)))

(defun put-varint (b n)
  (declare (type (integer 0) n))
  (loop (if (< n 128)
            (return (put8 b n))
            (progn (put8 b (logior 128 (ldb (byte 7 0) n)))
                   (setf n (ash n -7))))))

(defun put-be (b n width)
  (obuf-room b width)
  (loop with bytes = (obuf-bytes b) and pos = (obuf-pos b)
        for i from (1- width) downto 0
        do (setf (aref bytes (+ pos i)) (ldb (byte 8 0) n) n (ash n -8)))
  (incf (obuf-pos b) width))

(defun put-u64 (b n) (put-be b n 8))

(defun put-word (b n)
  "A non-negative integer as a length byte and its minimal big-endian bytes."
  (let ((len (ceiling (integer-length n) 8)))
    (invariant (< len 256))
    (put8 b len)
    (put-be b n len)))

(defun put-octets (b octets &key (start 0) (end (length octets)))
  (obuf-room b (- end start))
  (replace (obuf-bytes b) octets :start1 (obuf-pos b) :start2 start :end2 end)
  (incf (obuf-pos b) (- end start)))

(defun encode-aver (b v)
  (let ((live (not (aver-deleted-p v))))
    (put8 b (logior (if (aver-deleted-p v) 1 0) (if (aver-wiped-p v) 2 0)
                    (if (aver-storage-p v) 4 0)
                    (if (and live (not (octets= (aver-code-hash v) +empty-code-hash+))) 8 0)
                    (if (and live (plusp (aver-commitment v))) 16 0)))
    (when live
      (put-varint b (aver-nonce v))
      (put-word b (aver-balance v))
      (unless (octets= (aver-code-hash v) +empty-code-hash+)
        (put-octets b (aver-code-hash v)))
      (when (plusp (aver-commitment v))
        (put-word b (aver-commitment v))))))

;;; Byte input from a mapped file

(defun corrupt-segment (what)
  (file-error* "corrupt segment: ~A" what))

(defmacro with-sap-reader ((sap pos limit) &body body)
  "Local readers over SAP at the place POS (a fixnum variable), never past LIMIT (the end of
the mapped bytes the caller may read): U8, VARINT, BE, WORD, SKIP. A read past LIMIT, an
overlong varint or word signals HISTORY-FILE-ERROR, so a damaged file cannot make a lookup
read outside its mapping (the readers run at the hot policy's safety)."
  `(flet ((need (n)
            (declare (type fixnum n))
            (unless (<= (+ ,pos n) ,limit) (corrupt-segment "read past the end of a region"))))
     (declare (ignorable #'need))
     (flet ((u8 () (need 1) (prog1 (sb-sys:sap-ref-8 ,sap ,pos) (incf ,pos)))
            (varint ()
              (let ((n 0) (shift 0))
                (declare (type (unsigned-byte 62) n) (type (integer 0 63) shift))
                (loop (need 1)
                      (let ((byte (sb-sys:sap-ref-8 ,sap ,pos)))
                        (incf ,pos)
                        (when (> shift 54) (corrupt-segment "overlong varint"))
                        (setf n (logior n (the (unsigned-byte 62) (ash (logand byte 127) shift))))
                        (when (< byte 128) (return n))
                        (incf shift 7)))))
            (be (width)
              (declare (type fixnum width))
              (need width)
              (let ((n 0))
                (dotimes (i width) (setf n (logior (ash n 8) (sb-sys:sap-ref-8 ,sap (+ ,pos i)))))
                (incf ,pos width)
                n))
            (skip (n)
              (declare (type fixnum n))
              (need n)
              (incf ,pos n)))
       (declare (ignorable #'u8 #'varint #'be #'skip))
       (flet ((word ()
                (let ((len (u8)))
                  (when (> len 32) (corrupt-segment "word longer than 32 bytes"))
                  (be len))))
         (declare (ignorable #'word))
         ,@body))))

(defun sap-octets (sap start n limit)
  (declare (type fixnum start n limit))
  (unless (<= 0 start (+ start n) limit) (corrupt-segment "read past the end of a region"))
  (let ((o (make-octets n)))
    (dotimes (i n o) (setf (aref o i) (sb-sys:sap-ref-8 sap (+ start i))))))

(defun decode-aver (sap pos limit)
  (declare (type fixnum pos limit))
  (with-sap-reader (sap pos limit)
    (let ((flags (u8)))
      (if (logbitp 0 flags)
          (make-aver t (logbitp 1 flags) 0 0 +empty-code-hash+ nil)
          (let* ((nonce (varint)) (balance (word))
                 (code (if (logbitp 3 flags)
                           (prog1 (sap-octets sap pos 32 limit) (incf pos 32))
                           +empty-code-hash+))
                 (commitment (if (logbitp 4 flags) (word) 0)))
            (make-aver nil (logbitp 1 flags) nonce balance code (logbitp 2 flags) commitment))))))

(defun skip-aver (sap pos limit)
  (declare #.*hot* (type fixnum pos limit) (type sb-sys:system-area-pointer sap))
  (with-sap-reader (sap pos limit)
    (let ((flags (u8)))
      (unless (logbitp 0 flags)
        (varint)
        (let ((len (u8)))
          (when (> len 32) (corrupt-segment "word longer than 32 bytes"))
          (skip len))
        (when (logbitp 3 flags) (skip 32))
        (when (logbitp 4 flags)
          (let ((len (u8)))
            (when (> len 32) (corrupt-segment "word longer than 32 bytes"))
            (skip len)))))
    pos))

;;; Segment files
;;;
;;;   pages    records sorted by (key, first block); a page starts with a full key
;;;   record   shared:u8 suffix-len:u8 suffix first-block:varint count:varint body-len:varint body
;;;            body: per version, block delta from the previous (0 for the first), then the
;;;            value: a slot word (length byte, bytes) or an account (flags, nonce, balance, code hash)
;;;   index    pages:u64, then per page offset:u64 first-block:u64 key-length:u8 key
;;;   wipes    count:u64, then per wipe address:20 block:u64
;;;   footer   magic from to full level index-offset wipes-offset versions magic (u64 each)

(define-constant +magic+ (string->octets "CLHIST01"))
(defconstant +footer-size+ 72)
(defconstant +page-size+ 4096)
(defconstant +chunk-versions+ 64)

(defstruct (segment (:constructor %make-segment))
  path (job 0) (level 0) (from 0) (to 0) full-p
  (sap nil) (length 0 :type fixnum) (box nil)
  (page-offsets (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (page-blocks (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (page-keys #() :type simple-vector)
  (index-offset 0 :type fixnum)
  (versions 0)
  (wipes '()))                          ; ((address . block) ...)

(defmethod print-object ((s segment) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~D..~D~:[~; full~] job ~D level ~D" (segment-from s) (segment-to s)
            (segment-full-p s) (segment-job s) (segment-level s))))

(defun segment-name (job level from to)
  (format nil "seg-~12,'0D-~D-~12,'0D-~12,'0D.hist" job level from to))

(defun parse-segment-name (name)
  "(values job level from to) of a segment file name, or NIL."
  (let ((parts (and (> (length name) 9) (string= "seg-" name :end2 4)
                    (string= ".hist" name :start2 (- (length name) 5))
                    (split-dashes (subseq name 4 (- (length name) 5))))))
    (when (and (= (length parts) 4) (every (lambda (p) (and (plusp (length p)) (every #'digit-char-p p))) parts))
      (values-list (mapcar #'parse-integer parts)))))

(defun split-dashes (s)
  (loop with start = 0
        for dash = (position #\- s :start start)
        collect (subseq s start dash)
        while dash do (setf start (1+ dash))))

(defun map-file (path)
  "(values sap length) of PATH mapped read-only."
  (let ((fd (sb-posix:open (namestring path) sb-posix:o-rdonly)))
    (unwind-protect
         (let ((length (sb-posix:stat-size (sb-posix:fstat fd))))
           (if (zerop length)
               (values nil 0)
               (values (sb-posix:mmap nil length sb-posix:prot-read sb-posix:map-shared fd 0) length)))
      (sb-posix:close fd))))

(defun open-segment (path)
  "Map the segment file PATH and read its footer, index and wipes, checking that every offset
and count they hold lies inside the file."
  (multiple-value-bind (sap length) (map-file path)
    (unless (and sap (>= length +footer-size+))
      (when sap (sb-posix:munmap sap length))
      (file-error* "~A: too short" path))
    (let ((box (cons sap length)))
      ;; A segment a refresh drops may still be read by a view: it is unmapped only when
      ;; nothing refers to it any more (its finalizer), or here if it never opens.
      (handler-bind ((error (lambda (c) (declare (ignore c)) (unmap-box box))))
        (let ((seg (read-segment-footer path sap length)))
          (setf (segment-box seg) box)
          (sb-ext:finalize seg (lambda () (unmap-box box)) :dont-save t)
          seg)))))

(defun read-segment-footer (path sap length)
  (declare (type fixnum length))
  (let* ((footer (- length +footer-size+))
         (pos footer))
    (declare (type fixnum footer pos))
    (flet ((magic-at (p) (octets= (sap-octets sap p 8 length) +magic+))
           (bad (what) (file-error* "~A: ~A" path what)))
      (unless (and (magic-at pos) (magic-at (- length 8))) (bad "bad footer"))
      (incf pos 8)
      (with-sap-reader (sap pos length)
        (let* ((from (be 8)) (to (be 8)) (full (be 8)) (level (be 8))
               (index-offset (be 8)) (wipes-offset (be 8)) (versions (be 8)))
          (unless (and (<= index-offset wipes-offset footer) (<= from to) (typep to 'fixnum)
                       (typep versions 'fixnum) (typep level 'fixnum))
            (bad "footer offsets outside the file"))
          (let ((seg (%make-segment :path path :from from :to to :full-p (= full 1) :level level
                                    :sap sap :length length :index-offset index-offset
                                    :versions versions)))
            (setf pos index-offset)
            (with-sap-reader (sap pos wipes-offset)
              (let ((pages (be 8)))
                ;; each index entry takes at least 8 + 8 + 1 + 21 bytes
                (unless (<= (* pages 38) (- wipes-offset pos)) (bad "page count exceeds the index"))
                (let ((offsets (make-array pages :element-type 'fixnum))
                      (blocks (make-array pages :element-type 'fixnum))
                      (keys (make-array pages))
                      (prev 0))
                  (dotimes (i pages)
                    (let ((offset (be 8)) (block (be 8)) (n (u8)))
                      (unless (and (<= prev offset) (< offset index-offset) (typep block 'fixnum)
                                   (or (= n 21) (= n 53)))
                        (bad "bad page index entry"))
                      (setf prev offset
                            (aref offsets i) offset (aref blocks i) block
                            (svref keys i) (let ((k (be n))) (if (= n 53) k (ldb (byte 160 0) k))))))
                  (setf (segment-page-offsets seg) offsets (segment-page-blocks seg) blocks
                        (segment-page-keys seg) keys))))
            (setf pos wipes-offset)
            (with-sap-reader (sap pos footer)
              (let ((count (be 8)))
                (unless (<= (* count 28) (- footer pos)) (bad "wipe count exceeds the file"))
                (setf (segment-wipes seg) (loop repeat count collect (let ((a (be 20))) (cons a (be 8)))))))
            seg))))))

(defun unmap-box (box)
  "Unmap BOX's mapping once: the first caller to clear the address unmaps it (the finalizer
thread and a closing thread may race here)."
  (let ((sap (car box)))
    (when (and sap (eq sap (sb-ext:compare-and-swap (car box) sap nil)))
      (sb-posix:munmap sap (cdr box)))))

(defun close-segment (seg)
  "Unmap SEG now. Only for a segment no view can hold (one a refresh opened and dropped at
once); a published segment is unmapped by its finalizer."
  (when (segment-box seg) (unmap-box (segment-box seg))))

;;; Lookup in a segment

(defun key-before-p (ka ba kb bb)
  "(KA, BA) < (KB, BB) in key-then-block order."
  (or (< ka kb) (and (= ka kb) (< ba bb))))

(defun find-page (seg key n)
  "The last page whose first record is at or before (KEY, N), or NIL."
  (let* ((keys (segment-page-keys seg)) (blocks (segment-page-blocks seg))
         (lo 0) (hi (length keys)))
    (declare (type fixnum lo hi))
    ;; invariant: pages below LO start at or before the target; pages at HI and above after it
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (key-before-p key n (svref keys mid) (aref blocks mid))
                   (setf hi mid)
                   (setf lo (1+ mid)))))
    (when (plusp lo) (1- lo))))

(defun segment-floor (seg key target tlen n)
  "Find KEY's record holding its last version at or before block N in SEG. TARGET holds KEY's
TLEN bytes. Returns (values body-start body-end first-block count) or NIL."
  (declare #.*hot* (type octets target) (type fixnum tlen n))
  (let ((page (find-page seg key n)))
    (when page
      (let* ((sap (segment-sap seg))
             (limit (segment-index-offset seg))
             (pos (aref (segment-page-offsets seg) page))
             (end (if (< (1+ page) (length (segment-page-offsets seg)))
                      (aref (segment-page-offsets seg) (1+ page))
                      limit))
             (buf (make-octets 53))
             (klen 0)
             (found nil) (fstart 0) (fend 0) (ffirst 0) (fcount 0))
        (declare (type fixnum pos end limit klen) (type sb-sys:system-area-pointer sap)
                 (dynamic-extent buf))
        (with-sap-reader (sap pos limit)
          (loop while (< pos end)
                do (let* ((shared (u8)) (suffix (u8)))
                     (unless (and (<= shared klen) (<= (+ shared suffix) 53))
                       (corrupt-segment "bad key prefix"))
                     (need suffix)
                     (dotimes (i suffix) (setf (aref buf (+ shared i)) (sb-sys:sap-ref-8 sap (+ pos i))))
                     (incf pos suffix)
                     (setf klen (+ shared suffix))
                     (let* ((first (varint)) (count (varint)) (body-len (varint))
                            (cmp (octets-compare buf klen target tlen)))
                       (cond ((minusp cmp))
                             ((and (zerop cmp) (<= first n))
                              (setf found t fstart pos fend (+ pos body-len) ffirst first fcount count))
                             (t (loop-finish)))
                       (skip body-len)))))
        (when found (values fstart fend ffirst fcount))))))

(defun octets-compare (a alen b blen)
  (declare #.*hot* (type octets a b) (type fixnum alen blen))
  (loop for i below (min alen blen)
        do (let ((x (aref a i)) (y (aref b i)))
             (cond ((< x y) (return-from octets-compare -1))
                   ((> x y) (return-from octets-compare 1)))))
  (cond ((< alen blen) -1) ((> alen blen) 1) (t 0)))

(defun last-slot-version (sap start end first n)
  "(values block value) of the last slot version at or before N in a record body that
SEGMENT-FLOOR bounded to START..END."
  (declare #.*hot* (type fixnum start end first n) (type sb-sys:system-area-pointer sap))
  (let ((pos start) (block first) (vblock first) (vpos start))
    (declare (type fixnum pos block vblock vpos))
    (with-sap-reader (sap pos end)
      (loop while (< pos end)
            do (let ((b (+ block (varint))))
                 (when (> b n) (loop-finish))
                 (setf block b vblock b vpos pos)
                 (let ((len (u8)))
                   (when (> len 32) (corrupt-segment "word longer than 32 bytes"))
                   (skip len))))
      (setf pos vpos)
      (values vblock (word)))))

(defun last-account-version (sap start end first n)
  (declare #.*hot* (type fixnum start end first n) (type sb-sys:system-area-pointer sap))
  (let ((pos start) (block first) (vblock first) (vpos start))
    (declare (type fixnum pos block vblock vpos))
    (with-sap-reader (sap pos end)
      (loop while (< pos end)
            do (let ((b (+ block (varint))))
                 (when (> b n) (loop-finish))
                 (setf block b vblock b vpos pos)
                 (setf pos (skip-aver sap pos end)))))
    (values vblock (decode-aver sap vpos end))))

;;; The tail: versions not yet in a segment. key -> VERSIONS, the key's (block . value) in
;;; block order, value an integer (slot) or an AVER (account).
;;;
;;; One writer appends while views read without a lock. A reader sees a prefix: the writer
;;; stores an entry (in a new, larger vector when full, published first) before it raises
;;; the count, and a reader reads the count before the vector. The one entry a writer
;;; changes in place belongs to the block being appended, which no view can see yet (a view
;;; is made only up to the tail's last complete block).

(defstruct (versions (:constructor make-versions ()))
  (vec (make-array 2) :type simple-vector)
  (count 0 :type fixnum))

(defun versions-entry (v i) (svref (versions-vec v) i))

(defstruct (tail (:constructor make-tail (from)))
  (from 0) (to -1)
  (keys (make-hash-table :size 65536 :synchronized t) :type hash-table) ; the follower writes while RPC threads read
  (versions 0 :type fixnum)
  (wipes '()))

(defun tail-add (tail key block value)
  (let* ((keys (tail-keys tail))
         (v (or (gethash key keys) (make-versions)))
         (n (versions-count v))
         (vec (versions-vec v)))
    (if (and (plusp n) (= (car (svref vec (1- n))) block))
        (setf (svref vec (1- n)) (cons block value)) ; written twice in one block: keep the last
        (progn
          (when (= n (length vec))
            (let ((bigger (make-array (* 2 n))))
              (replace bigger vec)
              (setf vec bigger)
              (sb-thread:barrier (:write))
              (setf (versions-vec v) bigger)))
          (setf (svref vec n) (cons block value))
          (sb-thread:barrier (:write))
          (setf (versions-count v) (1+ n))
          (incf (tail-versions tail))))
    (when (zerop n) (setf (gethash key keys) v))))

(defun tail-floor (tail key n)
  "The last (block . value) of KEY at or before N in the tail, or NIL."
  (let ((v (gethash key (tail-keys tail))))
    (when v
      (let ((hi (versions-count v)))
        (sb-thread:barrier (:read))
        (let ((vec (versions-vec v)) (lo 0))
          (loop while (< lo hi)
                do (let ((mid (ash (+ lo hi) -1)))
                     (if (> (car (svref vec mid)) n) (setf hi mid) (setf lo (1+ mid)))))
          (when (plusp lo) (svref vec (1- lo))))))))

;;; Codes: append-only files of (hash:32 length:u32 bytes)*, indexed at open and read on use.

;;; The history

(defstruct (history (:constructor %make-history (dir)))
  dir
  (segments '() :type list)              ; newest (largest TO) first
  (wipes (make-hash-table :synchronized t) :type hash-table) ; address -> sorted simple-vector of blocks
  (codes (make-hash-table :synchronized t) :type hash-table) ; hash integer -> (path . offset) or octets
  (code-files (make-hash-table :test 'equal :synchronized t) :type hash-table) ; path -> bytes scanned
  (lock (sb-thread:make-mutex :name "history"))
  (tail nil)                             ; writer only
  (job nil)
  (last-accounts nil)                    ; writer: address -> AVER, the latest version
  (code-out nil))                        ; writer: stream of codes-<job>.dat

(defun dir-path (dir name) (merge-pathnames name dir))

(defun directory-pathname (dir)
  (let ((p (pathname dir)))
    (if (or (pathname-name p) (pathname-type p))
        (make-pathname :directory (append (or (pathname-directory p) (list :relative))
                                          (list (file-namestring p)))
                       :name nil :type nil :defaults p)
        p)))

(defun job-state-path (dir job) (dir-path (directory-pathname dir) (format nil "job-~12,'0D.state" job)))
(defun code-file-path (dir job) (dir-path (directory-pathname dir) (format nil "codes-~12,'0D.dat" job)))

(defun segment-files (dir)
  (remove-if-not (lambda (p) (parse-segment-name (file-namestring p)))
                 (directory (dir-path dir "seg-*.hist"))))

(defun contained-p (a b)
  "Segment A's blocks lie inside B's and B is a different file (a merge left A behind)."
  (and (not (eq a b)) (>= (segment-from a) (segment-from b)) (<= (segment-to a) (segment-to b))
       (or (/= (segment-from a) (segment-from b)) (/= (segment-to a) (segment-to b))
           (segment-full-p b))))

(defun scan-codes (history path)
  "Index the intact records of the code file PATH not yet seen."
  (let ((start (gethash (namestring path) (history-code-files history) 0)))
    (with-open-file (in path :element-type 'octet)
      (let ((len (file-length in)) (pos start) (head (make-octets 36)))
        (file-position in pos)
        (loop while (<= (+ pos 36) len)
              do (read-sequence head in)
                 (let ((n (octets->integer head :start 32 :end 36)))
                   (when (> (+ pos 36 n) len) (loop-finish))
                   (setf (gethash (octets->integer head :start 0 :end 32) (history-codes history))
                         (cons (namestring path) (+ pos 36)))
                   (incf pos (+ 36 n))
                   (file-position in pos)))
        (setf (gethash (namestring path) (history-code-files history)) pos)))))

(defun add-wipe (table address block)
  (let ((v (gethash address table)))
    (unless (and v (find block v))
      (setf (gethash address table)
            (sort (concatenate 'simple-vector (or v #()) (vector block)) #'<)))))

(defun history-refresh (history)
  "Pick up segments and codes written since the history was opened (a running build job)."
  (sb-thread:with-mutex ((history-lock history))
    (let* ((dir (history-dir history))
           (files (segment-files dir))
           ;; a file gone from the directory (a merge input, a removed job) is dropped
           (known (remove-if-not (lambda (s) (find (namestring (segment-path s)) files
                                                   :key #'namestring :test #'string=))
                                 (history-segments history)))
           (fresh (loop for p in files
                        unless (find (namestring p) known :key (lambda (s) (namestring (segment-path s)))
                                                         :test #'string=)
                          collect (handler-case (open-segment p)
                                    (history-file-error () nil))))
           (all (append (remove nil fresh) known))
           (live (remove-if (lambda (s) (some (lambda (o) (contained-p s o)) all)) all)))
      ;; Dropped segments (inputs of a merge) are unmapped by their finalizers.
      (dolist (s (set-difference all live))
        (when (find s fresh) (close-segment s)))
      (dolist (s (remove nil fresh))
        (when (member s live)
          (loop for (address . block) in (segment-wipes s)
                do (add-wipe (history-wipes history) address block))))
      (setf (history-segments history)
            (sort (copy-list live) (lambda (a b) (or (> (segment-to a) (segment-to b))
                                                     (and (= (segment-to a) (segment-to b))
                                                          (> (segment-from a) (segment-from b)))))))
      (dolist (p (directory (dir-path dir "codes-*.dat"))) (scan-codes history p))
      history)))

(defun open-history (dir)
  "Open the history directory DIR (created if absent) for reading."
  (let ((dir (directory-pathname dir)))
    (ensure-directories-exist dir)
    (history-refresh (%make-history dir))))

(defun close-history (history)
  "Stop writing and forget the segments. Their mappings go with their finalizers once no
view holds them, so a view still in use never reads an unmapped page."
  (sb-thread:with-mutex ((history-lock history))
    (setf (history-segments history) '())
    (when (history-code-out history)
      (close (history-code-out history))
      (setf (history-code-out history) nil)))
  history)

(defun history-segment-count (history) (length (history-segments history)))

;;; Coverage: the block ranges a history answers. A run starts at a full segment and goes on
;;; through segments whose ranges follow one another without a gap.

(defun history-ranges (history)
  "The block ranges ((from . to) ...) whose states HISTORY can answer, oldest first."
  (let ((segments (sort (copy-list (history-segments history)) #'< :key #'segment-from))
        (tail (history-tail history))
        (ranges '()))
    (dolist (s segments)
      (let ((run (first ranges)))
        (cond ((and run (<= (segment-from s) (1+ (cdr run))))
               (setf (cdr run) (max (cdr run) (segment-to s))))
              ((segment-full-p s) (push (cons (segment-from s) (segment-to s)) ranges)))))
    (when (and tail (>= (tail-to tail) (tail-from tail)) ranges
               (<= (tail-from tail) (1+ (cdr (first ranges)))))
      (setf (cdr (first ranges)) (max (cdr (first ranges)) (tail-to tail))))
    (nreverse ranges)))

(defun history-height (history)
  "The highest block whose state the history answers, or NIL."
  (let ((ranges (history-ranges history)))
    (when ranges (reduce #'max ranges :key #'cdr))))

;;; Views

(defstruct (history-state (:constructor %make-history-state (history number probes tail)))
  "The state after block NUMBER: an execution backend (its backend methods live in the
executor layer, which may see this one)."
  history (number 0 :type fixnum)
  (probes '() :type list)               ; ((segment . bound) ...) newest first, ending at a full one
  ;; The tail the view reads, captured when it was made: a flush replaces the history's tail
  ;; with an empty one and leaves this one intact, so the view keeps every version it saw.
  tail)

(defmethod print-object ((v history-state) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "after block ~D (~D segments)" (history-state-number v) (length (history-state-probes v)))))

(defun history-state (history n)
  "The state after block N, or NIL when HISTORY does not cover N."
  (let* ((tail (history-tail history))
         (tail-p (and tail (<= (tail-from tail) n) (>= (tail-to tail) (tail-from tail))))
         (probes '())
         (reach (if tail-p (1- (tail-from tail)) n)))
    ;; HISTORY-FLUSH publishes a tail's segment before it replaces the tail: a reader that
    ;; sees the new tail must also see the segment list holding the old one.
    (sb-thread:barrier (:read))
    (when (and tail-p (> n (tail-to tail))) (return-from history-state nil))
    ;; Walk back from N through segments that end where the previous probe began.
    (loop for s in (history-segments history)
          do (when (and (<= (segment-from s) reach) (>= (segment-to s) reach))
               (push (cons s (min n (segment-to s))) probes)
               (when (segment-full-p s)
                 (return-from history-state
                   (%make-history-state history n (nreverse probes) (when tail-p tail))))
               (setf reach (1- (segment-from s)))))
    nil))

(defun probe (view key fn-tail fn-seg)
  "The newest version of KEY visible from VIEW: (FN-TAIL entry) for a tail entry or
(FN-SEG segment start end first count bound) for a segment record; NIL when none."
  (let ((n (history-state-number view))
        (tail (history-state-tail view)))
    (when tail
      (let ((e (tail-floor tail key n)))
        (when e (return-from probe (funcall fn-tail e)))))
    (let* ((target (make-octets 53))
           (tlen (key-into key target)))
      (declare (dynamic-extent target))
      (loop for (seg . bound) in (history-state-probes view)
            do (multiple-value-bind (start end first count) (segment-floor seg key target tlen bound)
                 (when start (return-from probe (funcall fn-seg seg start end first count bound))))))
    nil))

(defun account-version (view address)
  "(values aver block) of ADDRESS's latest version visible from VIEW, or NIL."
  (probe view address
         (lambda (e) (values (cdr e) (car e)))
         (lambda (seg start end first count bound)
           (declare (ignore count))
           (multiple-value-bind (block v) (last-account-version (segment-sap seg) start end first bound)
             (values v block)))))

(defun history-account (view address)
  "(values found-p nonce balance code-hash commitment) of ADDRESS in VIEW."
  (let ((v (account-version view address)))
    (if (and v (not (aver-deleted-p v)))
        (values t (aver-nonce v) (aver-balance v) (aver-code-hash v) (aver-commitment v))
        (values nil 0 0 +empty-code-hash+ 0))))

(defun history-storage-empty-p (view address)
  (let ((v (account-version view address)))
    (or (null v) (aver-deleted-p v) (not (aver-storage-p v)))))

(defun last-wipe (history address n)
  (let ((v (gethash address (history-wipes history))))
    (when v
      (let ((best nil))
        (loop for b across v while (<= b n) do (setf best b))
        best))))

(defun history-storage (view address slot)
  "The value of SLOT of ADDRESS in VIEW (0 when never written or wiped since)."
  (multiple-value-bind (block value)
      (probe view (slot-key address slot)
             (lambda (e) (values (car e) (cdr e)))
             (lambda (seg start end first count bound)
               (declare (ignore count))
               (last-slot-version (segment-sap seg) start end first bound)))
    (if (null block)
        0
        (let* ((history (history-state-history view))
               (n (history-state-number view))
               (wipe (let ((w (last-wipe history address n)))
                       (let ((tail (history-state-tail view)))
                         (when tail
                           (loop for (a . b) in (tail-wipes tail)
                                 when (and (= a address) (<= b n) (or (null w) (> b w))) do (setf w b))))
                       w)))
          (if (and wipe (> wipe block)) 0 value)))))

(defun history-code-by-hash (history hash)
  "The code whose keccak is HASH (32 octets), or NIL. Codes are never removed."
  (let* ((key (octets->integer hash))
         (entry (gethash key (history-codes history))))
    (cond ((octets= hash +empty-code-hash+) (make-octets 0))
          ((null entry) nil)
          ((typep entry 'octets) entry)
          (t (destructuring-bind (path . offset) entry
               (let ((code (with-open-file (in path :element-type 'octet)
                             (file-position in (- offset 4))
                             (let ((len (make-octets 4)))
                               (read-sequence len in)
                               (let ((o (make-octets (octets->integer len))))
                                 (read-sequence o in)
                                 o)))))
                 (setf (gethash key (history-codes history)) code)))))))

(defun history-code (view hash)
  (or (history-code-by-hash (history-state-history view) hash)
      (error "history: no code for hash ~A" (octets->hex hash))))

;;; Changes between two blocks: the keys a state must rewrite to move from one block's state to
;;; another's (both directions), e.g. a state database with tries moved to an older block.

(defun segment-map-changes (seg from to fn)
  "Call (FN address slot-or-nil) for each key of SEG with a version in blocks FROM+1..TO."
  (declare #.*hot* (type fixnum from to) (type function fn))
  (let* ((sap (segment-sap seg))
         (pos (if (plusp (length (segment-page-offsets seg))) (aref (segment-page-offsets seg) 0) 0))
         (end (segment-index-offset seg))
         (buf (make-octets 53))
         (klen 0))
    (declare (type fixnum pos end klen) (type sb-sys:system-area-pointer sap))
    (with-sap-reader (sap pos end)
      (loop while (< pos end)
            do (let* ((shared (u8)) (suffix (u8)))
                 (declare (type fixnum shared suffix))
                 (unless (and (<= shared klen) (<= (+ shared suffix) 53))
                   (corrupt-segment "bad key prefix"))
                 (need suffix)
                 (dotimes (i suffix) (setf (aref buf (+ shared i)) (sb-sys:sap-ref-8 sap (+ pos i))))
                 (incf pos suffix)
                 (setf klen (+ shared suffix))
                 (let* ((first (varint)) (count (varint)) (body-len (varint))
                        (body-end (progn (need body-len) (+ pos body-len)))
                        (hit (cond ((> first to) nil)
                                   ((> first from) t)
                                   ;; walk the versions to the first one after FROM
                                   (t (let ((block first) (slot-p (= klen 53)))
                                        (declare (type fixnum block))
                                        (with-sap-reader (sap pos body-end)
                                          (loop repeat count
                                                do (incf block (varint))
                                                   (when (> block from) (return (<= block to)))
                                                   (if slot-p
                                                       (let ((len (u8)))
                                                         (when (> len 32) (corrupt-segment "word longer than 32 bytes"))
                                                         (skip len))
                                                       (setf pos (skip-aver sap pos body-end))))))))))
                   (declare (type fixnum first count body-len body-end))
                   (when hit
                     (when (< klen 21) (corrupt-segment "short key"))
                     (funcall fn (octets->integer buf :start 1 :end 21)
                              (when (= klen 53) (octets->integer buf :start 21 :end 53))))
                   (setf pos body-end)))))))

(defun history-gap-seeds (history)
  "The first blocks of full segments that no other segment covers: a job seeded there from a
checkpoint, so that block's deletions (accounts and slots it removed) are written nowhere;
HISTORY-MAP-CHANGES across such a block misses them."
  (loop for s in (history-segments history)
        when (and (segment-full-p s) (plusp (segment-from s))
                  (notany (lambda (o) (and (not (segment-full-p o))
                                           (<= (segment-from o) (segment-from s) (segment-to o))))
                          (history-segments history)))
          collect (segment-from s)))

(defun history-map-changes (history from to fn)
  "Call (FN address slot-or-nil) for every account (slot NIL) and storage slot with a version
written by blocks FROM+1..TO: a superset of the keys whose values differ between the states
after FROM and after TO (a full segment in the range lists its whole state), unless the range
holds one of HISTORY-GAP-SEEDS. A key may be given more than once. Returns the addresses with
a storage wipe in the range."
  (let ((from (min from to)) (to (max from to)) (wiped '()))
    (dolist (seg (history-segments history))
      (when (and (<= (segment-from seg) to) (> (segment-to seg) from))
        (segment-map-changes seg from to fn)
        (loop for (address . block) in (segment-wipes seg)
              when (and (> block from) (<= block to)) do (pushnew address wiped))))
    (let ((tail (history-tail history)))
      (when tail
        (maphash (lambda (key versions)
                   (when (loop for i below (versions-count versions)
                               for e = (versions-entry versions i)
                               thereis (and (> (car e) from) (<= (car e) to)))
                     (if (< key +slot-tag+)
                         (funcall fn key nil)
                         (funcall fn (ldb (byte 160 256) key) (ldb (byte 256 0) key)))))
                 (tail-keys tail))
        (loop for (address . block) in (tail-wipes tail)
              when (and (> block from) (<= block to)) do (pushnew address wiped))))
    wiped))

(defun history-seeds (history)
  "The first blocks of the full segments (the jobs' starting states), ascending."
  (sort (loop for s in (history-segments history) when (segment-full-p s) collect (segment-from s)) #'<))

(defun sample-keys (history n)
  "N keys drawn with clempo.io's entropy from the segments' page indexes (for benchmarks)."
  (let ((all (coerce (loop for s in (history-segments history) append (coerce (segment-page-keys s) 'list))
                     'simple-vector)))
    (when (plusp (length all))
      (let ((out (make-array n)))
        (dotimes (i n out) (setf (svref out i) (svref all (clempo.io:random-below (length all)))))))))
