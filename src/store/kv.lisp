(in-package #:clempo.store)

;;; The on-disk key-value store under a backed state database: a log-structured merge tree
;;; without a memtable. The state database keeps a block's writes in RAM until the store
;;; flushes them; a flush hands KV-WRITE one sorted batch, which becomes an immutable segment
;;; file named in MANIFEST (replaced by rename). Lookups go newest segment first. Segments
;;; merge in tiers: whenever the newest four are within a factor of four in size they become
;;; one, and a merge that reaches the oldest segment drops tombstones.
;;;
;;; Keys are 53-byte big-endian integers (KEY-* below), so integer order is byte order. A
;;; value is a non-empty octet vector; the empty vector is a tombstone.
;;;
;;; Segment file:
;;;   blocks   per block: payload length u32, payload, CRC-32C of the payload u32. A payload
;;;            holds records shared:u8 suffix:u8 suffix-bytes value-length:varint value,
;;;            the key sharing SHARED leading bytes with the record before (0 for a block's
;;;            first record); a block closes once it passes 4 KiB
;;;   index    blocks:u64, per block offset:u64 first-key:53
;;;   bloom    bits:u64 hashes:u8 bit-bytes
;;;   footer   magic index-offset:u64 bloom-offset:u64 records:u64 crc:u32 0:u32 magic
;;;            (CRC-32C of index and bloom)
;;;
;;; Every read of a mapped segment is checked against the region it reads, and each block's
;;; checksum is verified the first time the block is read, so a damaged file signals
;;; STORE-FORMAT-ERROR instead of returning wrong bytes.

(defconstant +key-bytes+ 53)
(defconstant +block-target+ 4096)
(define-constant +segment-magic+ (string->octets "CLKVSEG1"))
(defconstant +segment-footer+ 48)
(defconstant +bloom-hashes+ 7)

;;; Keys

(defun key-account (address) (logior #.(ash 1 416) (ash address 256)))
(defun key-slot (address slot) (logior #.(ash 2 416) (ash address 256) slot))
(defun key-code (hash) (logior #.(ash 3 416) (ash hash 160)))
(defun key-node (hash) (logior #.(ash 4 416) (ash hash 160)))
(defun key-head () #.(ash 7 416))

(defun key-range-accounts () (values #.(ash 1 416) #.(ash 2 416)))
(defun key-range-slots (address)
  (values (key-slot address 0) (key-slot (1+ address) 0)))
(defun key-range-codes () (values #.(ash 3 416) #.(ash 4 416)))

(defun key-slot-number (key) (ldb (byte 256 0) key))
(defun key-address (key) (ldb (byte 160 256) key))
(defun key-hash (key) (ldb (byte 256 160) key))

(defun key-octets (key &optional (out (make-octets +key-bytes+)))
  (declare (type octets out))
  (loop for i from (1- +key-bytes+) downto 0
        for k = key then (ash k -8)
        do (setf (aref out i) (ldb (byte 8 0) k)))
  out)

;;; Bloom filter hashes: FNV-1a over the key bytes, the second hash a SplitMix64 finish.

(defun key-hashes (kb)
  (declare #.*hot* (type octets kb))
  (let ((h 14695981039346656037))
    (declare (type (unsigned-byte 64) h))
    (dotimes (i +key-bytes+)
      (setf h (ldb (byte 64 0) (* (logxor h (aref kb i)) 1099511628211))))
    (let ((z (ldb (byte 64 0) (+ h #x9E3779B97F4A7C15))))
      (declare (type (unsigned-byte 64) z))
      (setf z (ldb (byte 64 0) (* (logxor z (ash z -30)) #xBF58476D1CE4E5B9))
            z (ldb (byte 64 0) (* (logxor z (ash z -27)) #x94D049BB133111EB))
            z (logxor z (ash z -31)))
      (values h (logior z 1)))))

(defun bloom-bit (h1 h2 j bits)
  (declare #.*hot* (type (unsigned-byte 64) h1 h2) (type fixnum j)
           (type (integer 64 #.(ash 1 50)) bits))
  (mod (ldb (byte 64 0) (+ h1 (ldb (byte 64 0) (* j h2)))) bits))

;;; Writing a segment

(defun write-segment (path next count)
  "Write the records NEXT returns ((values key value), NIL at the end, ascending keys) to
PATH durably; COUNT bounds their number (it sizes the bloom filter)."
  (let* ((bits (* 64 (max 1 (ceiling (* 10 count) 64))))
         (bloom (make-octets (/ bits 8)))
         (records 0))
    (write-durably
     path
     (lambda (out)
       (let ((sink (make-sink out))
             (block (make-sink nil (+ +block-target+ 512)))
             (offset 0) (index '())
             (kb (make-octets +key-bytes+)) (prev (make-octets +key-bytes+)))
         (flet ((finish-block ()
                  (let ((n (sink-pos block)))
                    (when (plusp n)
                      (put-u32 sink n)
                      (put-raw-range sink (sink-buffer block) 0 n)
                      (put-u32 sink (crc32c (sink-buffer block) 0 n))
                      (incf offset (+ 8 n))
                      (sink-reset block)))))
           (loop
             (multiple-value-bind (key value) (funcall next)
               (unless key (return))
               (when (>= (sink-pos block) +block-target+) (finish-block))
               (key-octets key kb)
               (multiple-value-bind (h1 h2) (key-hashes kb)
                 (dotimes (j +bloom-hashes+)
                   (let ((bit (bloom-bit h1 h2 j bits)))
                     (setf (aref bloom (ash bit -3))
                           (logior (aref bloom (ash bit -3)) (ash 1 (logand bit 7)))))))
               (let ((shared (if (zerop (sink-pos block))
                                 (progn (push (cons offset key) index) 0)
                                 (or (mismatch prev kb) +key-bytes+))))
                 (when (= shared +key-bytes+) (invariant nil "duplicate key in a segment batch"))
                 (put-u8 block shared)
                 (put-u8 block (- +key-bytes+ shared))
                 (put-raw-range block kb shared +key-bytes+)
                 (put-varint block (length value))
                 (put-raw block value))
               (replace prev kb)
               (incf records)))
           (finish-block))
         (invariant (<= records count) "segment of ~D records sized for ~D" records count)
         (let ((tail (make-sink nil 4096))
               (index-offset offset))
           (put-u64 tail (length index))
           (dolist (e (nreverse index))
             (put-u64 tail (car e))
             (put-raw tail (key-octets (cdr e))))
           (let ((bloom-offset (+ index-offset (sink-pos tail))))
             (put-u64 tail bits)
             (put-u8 tail +bloom-hashes+)
             (put-raw tail bloom)
             (let ((crc (crc32c (sink-buffer tail) 0 (sink-pos tail))))
               (put-raw tail +segment-magic+)
               (put-u64 tail index-offset)
               (put-u64 tail bloom-offset)
               (put-u64 tail records)
               (put-u32 tail crc)
               (put-u32 tail 0)
               (put-raw tail +segment-magic+)))
           (put-raw-range sink (sink-buffer tail) 0 (sink-pos tail))
           (sink-flush sink)))))
    records))

;;; Reading a segment

(defstruct (kseg (:constructor %make-kseg))
  path name (sap nil) (length 0 :type fixnum) box
  (data-end 0 :type fixnum)                              ; index offset: blocks end here
  (keys #() :type simple-vector)                         ; first key of each block
  (offsets (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (verified (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (bloom (make-octets 0) :type octets)
  (bits 64 :type fixnum)
  (records 0 :type fixnum))

(defun segment-damaged (seg what)
  (format-error "~A: ~A" (if seg (kseg-name seg) "segment") what))

(defun sap-be (sap pos width)
  (declare #.*hot* (type sb-sys:system-area-pointer sap) (type fixnum pos) (type (integer 0 8) width))
  (let ((n 0))
    (dotimes (i width n) (setf n (logior (ash n 8) (sb-sys:sap-ref-8 sap (+ pos i)))))))

(defun sap-octets* (sap start n)
  (declare #.*hot* (type sb-sys:system-area-pointer sap) (type fixnum start n))
  (let ((o (make-octets n)))
    (dotimes (i n o) (setf (aref o i) (sb-sys:sap-ref-8 sap (+ start i))))))

(defun crc32c-sap (sap start end)
  (declare #.*hot* (type sb-sys:system-area-pointer sap) (type fixnum start end))
  (let ((c #xFFFFFFFF) (table +crc32c-table+))
    (declare (type (unsigned-byte 32) c))
    (loop for i of-type fixnum from start below end
          do (setf c (logxor (aref table (logand (logxor c (sb-sys:sap-ref-8 sap i)) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun map-segment-file (path)
  (let ((fd (sb-posix:open (namestring path) sb-posix:o-rdonly)))
    (unwind-protect
         (let ((length (sb-posix:stat-size (sb-posix:fstat fd))))
           (if (< length +segment-footer+)
               (values nil length)
               (values (sb-posix:mmap nil length sb-posix:prot-read sb-posix:map-shared fd 0) length)))
      (sb-posix:close fd))))

(defun release-mapping (box)
  "Unmap BOX's mapping once (finalizer and closer may race)."
  (let ((sap (car box)))
    (when (and sap (eq sap (sb-ext:compare-and-swap (car box) sap nil)))
      (sb-posix:munmap sap (cdr box)))))

(defun open-kseg (path)
  (multiple-value-bind (sap length) (map-segment-file path)
    (unless sap (format-error "~A: too short for a segment" path))
    (let ((box (cons sap length)))
      (handler-bind ((error (lambda (c) (declare (ignore c)) (release-mapping box))))
        (let ((seg (read-kseg-footer path sap length)))
          (setf (kseg-box seg) box)
          ;; a merge drops segments a reader may still be using: unmap on collection
          (sb-ext:finalize seg (lambda () (release-mapping box)) :dont-save t)
          seg)))))

(defun read-kseg-footer (path sap length)
  (declare (type fixnum length))
  (let* ((name (file-namestring path))
         (foot (- length +segment-footer+)))
    (flet ((bad (what) (format-error "~A: ~A" name what)))
      (unless (and (octets= (sap-octets* sap foot 8) +segment-magic+)
                   (octets= (sap-octets* sap (- length 8) 8) +segment-magic+))
        (bad "bad segment footer"))
      (let ((index-offset (sap-be sap (+ foot 8) 8))
            (bloom-offset (sap-be sap (+ foot 16) 8))
            (records (sap-be sap (+ foot 24) 8))
            (crc (sap-be sap (+ foot 32) 4)))
        (unless (and (<= index-offset bloom-offset) (<= (+ bloom-offset 9) foot)
                     (typep records 'fixnum))
          (bad "segment footer offsets outside the file"))
        (unless (= crc (crc32c-sap sap index-offset foot)) (bad "segment index checksum"))
        (let ((blocks (sap-be sap index-offset 8))
              (pos (+ index-offset 8)))
          (unless (= (+ pos (* blocks (+ 8 +key-bytes+))) bloom-offset) (bad "segment index size"))
          (let ((keys (make-array blocks)) (offsets (make-array blocks :element-type 'fixnum)))
            (dotimes (i blocks)
              (let ((offset (sap-be sap pos 8)))
                (unless (and (< offset index-offset) (or (zerop i) (> offset (aref offsets (1- i)))))
                  (bad "segment index offset"))
                (setf (aref offsets i) offset
                      (svref keys i) (octets->integer (sap-octets* sap (+ pos 8) +key-bytes+)))
                (incf pos (+ 8 +key-bytes+))))
            (let ((bits (sap-be sap bloom-offset 8)))
              (unless (and (plusp bits) (zerop (mod bits 64)) (= (sap-be sap (+ bloom-offset 8) 1) +bloom-hashes+)
                           (= (+ bloom-offset 9 (/ bits 8)) foot))
                (bad "segment bloom filter size"))
              (%make-kseg :path path :name name :sap sap :length length :data-end index-offset
                          :keys keys :offsets offsets
                          :verified (make-array blocks :element-type 'bit :initial-element 0)
                          :bloom (sap-octets* sap (+ bloom-offset 9) (/ bits 8))
                          :bits bits :records records))))))))

(defun kseg-maybe-p (seg kb)
  "False when SEG's bloom filter rules out the key whose bytes are KB."
  (declare #.*hot* (type octets kb))
  (let ((bloom (kseg-bloom seg)) (bits (kseg-bits seg)))
    (multiple-value-bind (h1 h2) (key-hashes kb)
      (dotimes (j +bloom-hashes+ t)
        (let ((bit (bloom-bit h1 h2 j bits)))
          (unless (logbitp (logand bit 7) (aref bloom (ash bit -3))) (return nil)))))))

(defun kseg-block-floor (seg key)
  "The index of the last block whose first key is at or before KEY, or NIL."
  (let ((keys (kseg-keys seg)) (lo 0) (hi (length (kseg-keys seg))))
    (declare (type fixnum lo hi))
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (< key (svref keys mid)) (setf hi mid) (setf lo (1+ mid)))))
    (when (plusp lo) (1- lo))))

(defun kseg-block (seg i)
  "(values payload-start payload-end) of block I, checked against the data region and, the
first time, against its checksum."
  (declare #.*hot* (type fixnum i))
  (let* ((sap (kseg-sap seg))
         (off (aref (kseg-offsets seg) i))
         (data-end (kseg-data-end seg)))
    (declare (type fixnum off data-end))
    (unless (<= (+ off 4) data-end) (segment-damaged seg "block header past the data"))
    (let* ((len (sap-be sap off 4)) (start (+ off 4)) (end (+ start len)))
      (declare (type fixnum len start end))
      (unless (<= (+ end 4) data-end) (segment-damaged seg "block past the data"))
      (when (zerop (sbit (kseg-verified seg) i))
        (unless (= (sap-be sap end 4) (crc32c-sap sap start end))
          (segment-damaged seg (format nil "checksum of block ~D" i)))
        (setf (sbit (kseg-verified seg) i) 1))
      (values start end))))

(defmacro with-record-reader ((seg sap pos end) &body body)
  "Readers over a block payload SAP[POS, END): BYTE, VARINT, NEED."
  `(flet ((need (n) (declare (type fixnum n))
            (unless (<= (+ ,pos n) ,end) (segment-damaged ,seg "record past its block"))))
     (declare (ignorable #'need))
     (flet ((byte* () (need 1) (prog1 (sb-sys:sap-ref-8 ,sap ,pos) (incf ,pos)))
            (varint ()
              (let ((n 0) (shift 0))
                (declare (type (unsigned-byte 62) n) (type (integer 0 63) shift))
                (loop (need 1)
                      (let ((b (sb-sys:sap-ref-8 ,sap ,pos)))
                        (incf ,pos)
                        (when (> shift 54) (segment-damaged ,seg "overlong varint"))
                        (setf n (logior n (the (unsigned-byte 62) (ash (logand b 127) shift))))
                        (when (< b 128) (return n))
                        (incf shift 7))))))
       (declare (ignorable #'byte* #'varint))
       ,@body)))

(defun kseg-read-key (seg sap pos end buf first-p)
  "Read a record's key into BUF; returns the position after it."
  (declare #.*hot* (type fixnum pos end) (type octets buf) (type sb-sys:system-area-pointer sap))
  (with-record-reader (seg sap pos end)
    (let* ((shared (byte*)) (suffix (byte*)))
      (unless (and (= (+ shared suffix) +key-bytes+) (or (not first-p) (zerop shared)))
        (segment-damaged seg "bad key prefix"))
      (need suffix)
      (dotimes (i suffix) (setf (aref buf (+ shared i)) (sb-sys:sap-ref-8 sap (+ pos i))))
      (+ pos suffix))))

(defun kseg-get (seg key kb)
  "The value of KEY (bytes KB) in SEG: octets (empty for a tombstone), or NIL when absent."
  (declare #.*hot* (type octets kb))
  (when (kseg-maybe-p seg kb)
    (let ((i (kseg-block-floor seg key)))
      (when i
        (multiple-value-bind (pos end) (kseg-block seg i)
          (declare (type fixnum pos end))
          (let ((sap (kseg-sap seg)) (buf (make-octets +key-bytes+)) (first-p t))
            (declare (dynamic-extent buf))
            (loop while (< pos end)
                  do (setf pos (kseg-read-key seg sap pos end buf first-p) first-p nil)
                     (with-record-reader (seg sap pos end)
                       (let ((vlen (varint))
                             (cmp (let ((d (mismatch buf kb)))
                                    (cond ((null d) 0) ((< (aref buf d) (aref kb d)) -1) (t 1)))))
                         (declare (type fixnum vlen))
                         (need vlen)
                         (cond ((zerop cmp) (return (sap-octets* sap pos vlen)))
                               ((plusp cmp) (return nil)))
                         (incf pos vlen))))))))))

;;; Cursors: one segment's records in key order from a start key.

(defstruct (kcursor (:constructor make-kcursor (seg)))
  seg (block 0 :type fixnum) (pos 0 :type fixnum) (end 0 :type fixnum)
  (buf (make-octets +key-bytes+) :type octets)
  (first-p t)
  key value)

(defun kcursor-load-block (c i)
  (let ((seg (kcursor-seg c)))
    (if (>= i (length (kseg-keys seg)))
        (progn (setf (kcursor-key c) nil) nil)
        (multiple-value-bind (start end) (kseg-block seg i)
          (setf (kcursor-block c) i (kcursor-pos c) start (kcursor-end c) end (kcursor-first-p c) t)
          t))))

(defun kcursor-next (c)
  "Step to the next record; its key and value in KCURSOR-KEY/VALUE, KEY NIL at the end."
  (let ((seg (kcursor-seg c)))
    (loop while (>= (kcursor-pos c) (kcursor-end c))
          do (unless (kcursor-load-block c (1+ (kcursor-block c))) (return-from kcursor-next nil)))
    (let ((sap (kseg-sap seg)) (end (kcursor-end c)) (buf (kcursor-buf c)))
      (let ((pos (kseg-read-key seg sap (kcursor-pos c) end buf (kcursor-first-p c))))
        (declare (type fixnum pos))
        (with-record-reader (seg sap pos end)
          (let ((vlen (varint)))
            (need vlen)
            (setf (kcursor-key c) (octets->integer buf)
                  (kcursor-value c) (sap-octets* sap pos vlen)
                  (kcursor-first-p c) nil
                  (kcursor-pos c) (+ pos vlen))))
        c))))

(defun kcursor-seek (c lo)
  "Position C at its segment's first record with a key at or after LO."
  (let ((i (or (kseg-block-floor (kcursor-seg c) lo) 0)))
    (setf (kcursor-block c) (1- i) (kcursor-pos c) 0 (kcursor-end c) 0)
    (loop (unless (kcursor-next c) (return nil))
          (when (>= (kcursor-key c) lo) (return c)))))

(defun merged-records (segments lo hi &key keep-tombstones)
  "A generator over the records of SEGMENTS (newest first) with keys in [LO, HI) (HI NIL: no
bound), merged in key order, the newest version of each key winning; tombstones are
skipped unless KEEP-TOMBSTONES."
  (let ((cursors (loop for s in segments
                       for c = (kcursor-seek (make-kcursor s) lo)
                       when c collect c)))
    (lambda ()
      (loop
        (setf cursors (delete-if (lambda (c) (or (null (kcursor-key c))
                                                  (and hi (>= (kcursor-key c) hi))))
                                 cursors))
        (when (null cursors) (return nil))
        (let ((best nil))
          ;; CURSORS keep the segments' newest-first order: the first minimum is the newest
          (dolist (c cursors) (when (or (null best) (< (kcursor-key c) (kcursor-key best))) (setf best c)))
          (let ((key (kcursor-key best)) (value (kcursor-value best)))
            (dolist (c cursors) (when (eql (kcursor-key c) key) (kcursor-next c)))
            (when (or keep-tombstones (plusp (length value)))
              (return (values key value)))))))))

;;; The store

(defstruct (kv (:constructor %make-kv (dir)))
  dir
  (segments '() :type list)             ; newest first; replaced whole, read without a lock
  (seq 0 :type integer)
  (merges 0 :type integer)
  ;; Merges run on a background thread, one at a time. The lock orders every change of
  ;; SEGMENTS (a flush adds one at the front, a merge replaces a run) with its MANIFEST.
  (lock (sb-thread:make-mutex :name "kv"))
  (merged (sb-thread:make-waitqueue :name "kv merge done"))
  (merger nil)                          ; the running merge thread
  (merge-error nil)                     ; a merge's failure, signalled to the next writer
  (background t))                       ; NIL: merge on the writer's thread

(defun kv-segment-name (n) (format nil "seg-~12,'0D.kv" n))

(defun kv-manifest-text (kv names)
  (let ((body (format nil "clempo-kv 1~%seq ~D~%~{~A~%~}" (kv-seq kv) names)))
    (format nil "~Acrc ~D~%" body (crc32c (string->octets body)))))

(defun parse-kv-manifest (text)
  "(values seq names) of a KV manifest TEXT, or signal STORE-FORMAT-ERROR."
  (let* ((lines (loop with start = 0
                      for nl = (position #\Newline text :start start)
                      while nl collect (subseq text start nl) do (setf start (1+ nl))))
         (body-end (let ((p (search (format nil "~%crc ") text :from-end t))) (and p (1+ p)))))
    (unless (and (>= (length lines) 3) (string= (first lines) "clempo-kv 1") body-end
                 (eql 0 (search "seq " (second lines)))
                 (eql 0 (search "crc " (car (last lines))))
                 (= (crc32c (string->octets (subseq text 0 body-end)))
                    (or (ignore-errors (parse-integer (car (last lines)) :start 4)) -1)))
      (format-error "damaged KV MANIFEST"))
    (values (parse-integer (second lines) :start 4)
            (butlast (cddr lines)))))

(defun write-kv-manifest (kv names)
  (write-durably (dir-file (kv-dir kv) "MANIFEST")
                 (lambda (out) (write-sequence (string->octets (kv-manifest-text kv names)) out))))

(defun kv-manifest-p (dir)
  "True when DIR's MANIFEST is a KV manifest."
  (let ((path (dir-file dir "MANIFEST")))
    (and (probe-file path)
         (eql 0 (search "clempo-kv " (clempo.io:read-file-string path))))))

(defun open-kv (dir &key read-only)
  "Open the KV store in DIR (an empty one when DIR has no KV manifest), removing segment
files the manifest does not list (a write or merge cut short). READ-ONLY opens what the
manifest names and touches nothing, for a reader beside the datadir's owner; a segment a
concurrent merge deleted between the manifest read and its open signals FILE-ERROR or
SB-POSIX:SYSCALL-ERROR (read again)."
  (let ((kv (%make-kv dir))
        (names '()))
    (when (kv-manifest-p dir)
      (multiple-value-bind (seq listed) (parse-kv-manifest (clempo.io:read-file-string (dir-file dir "MANIFEST")))
        (setf (kv-seq kv) seq names listed)))
    (unless read-only
      (dolist (p (directory (dir-file dir "seg-*.kv")))
        (unless (member (file-namestring p) names :test #'string=) (delete-file p)))
      (dolist (p (directory (dir-file dir "seg-*.kv-tmp"))) (delete-file p)))
    (setf (kv-segments kv) (mapcar (lambda (n) (open-kseg (dir-file dir "~A" n))) names))
    kv))

(defun kv-get (kv key)
  "The value of KEY, or NIL when absent or deleted. Safe from any thread."
  (let ((kb (key-octets key)))
    (dolist (seg (kv-segments kv) nil)
      (let ((v (kseg-get seg key kb)))
        (when v (return (if (plusp (length v)) v nil)))))))

(defun kv-map-range (kv lo hi fn)
  "Call FN with (key value) for every live key in [LO, HI), ascending."
  (let ((next (merged-records (kv-segments kv) lo hi)))
    (loop (multiple-value-bind (key value) (funcall next)
            (unless key (return))
            (funcall fn key value)))))

(defun kv-segment-bytes (seg) (kseg-length seg))

(defconstant +merge-width+ 4)
(defconstant +segments-max+ 24
  "Segments a writer may leave before it waits for the running merge (backpressure: lookups
cost one probe per segment).")

(defun kv-write (kv entries &key (manifest t))
  "Write ENTRIES, a vector of (key . value) with distinct keys in ascending order (an empty
value deletes), as the newest segment, and start a merge if a tier is full. Waits while
+SEGMENTS-MAX+ segments are live and a merge is running. Without MANIFEST the segment is
readable through KV but not yet named in MANIFEST (an import publishes them all at once)."
  (kv-check-merge kv)
  (when (plusp (length entries))
    (let* ((seq (sb-thread:with-mutex ((kv-lock kv)) (incf (kv-seq kv))))
           (name (kv-segment-name seq))
           (path (dir-file (kv-dir kv) "~A" name))
           (i 0))
      (write-segment path
                     (lambda ()
                       (when (< i (length entries))
                         (let ((e (aref entries i)))
                           (incf i)
                           (values (car e) (cdr e)))))
                     (length entries))
      (let ((seg (open-kseg path)))
        (sb-thread:with-mutex ((kv-lock kv))
          (let ((segments (cons seg (kv-segments kv))))
            (when manifest (write-kv-manifest kv (mapcar #'kseg-name segments)))
            (setf (kv-segments kv) segments))))
      (when manifest
        (kv-merge-tiers kv)
        (sb-thread:with-mutex ((kv-lock kv))
          (loop while (and (kv-merger kv) (> (length (kv-segments kv)) +segments-max+))
                do (sb-thread:condition-wait (kv-merged kv) (kv-lock kv)))))))
  kv)

(defun kv-check-merge (kv)
  (let ((e (shiftf (kv-merge-error kv) nil)))
    (when e (error e))))

(defun merge-group (segments)
  "The first run of +MERGE-WIDTH+ adjacent segments, newest first, within a factor of four
in size, or NIL."
  (loop for tail on segments
        for group = (and (nthcdr (1- +merge-width+) tail) (subseq tail 0 +merge-width+))
        while group
        do (let ((sizes (mapcar #'kseg-length group)))
             (when (<= (reduce #'max sizes) (* 4 (reduce #'min sizes)))
               (return group)))))

(defun kv-merge-tiers (kv)
  "Merge full tiers: on a background thread (one at a time), or here without BACKGROUND."
  (if (kv-background kv)
      (sb-thread:with-mutex ((kv-lock kv))
        (unless (kv-merger kv)
          (let ((group (merge-group (kv-segments kv))))
            (when group
              (setf (kv-merger kv)
                    (sb-thread:make-thread
                     (lambda ()
                       (handler-case (loop for g = group then (sb-thread:with-mutex ((kv-lock kv))
                                                                (merge-group (kv-segments kv)))
                                           while g do (kv-merge kv g))
                         (error (e) (setf (kv-merge-error kv) e)))
                       (sb-thread:with-mutex ((kv-lock kv))
                         (setf (kv-merger kv) nil)
                         (sb-thread:condition-broadcast (kv-merged kv))))
                     :name "kv merge"))))))
      (loop for group = (merge-group (kv-segments kv))
            while group do (kv-merge kv group))))

(defun kv-merge (kv group)
  "Replace GROUP, a run of adjacent segments (newest first), by one segment. The run stays
adjacent while it merges: flushes only add segments at the front."
  (let* ((older-p (sb-thread:with-mutex ((kv-lock kv))
                    (rest (member (car (last group)) (kv-segments kv)))))
         (seq (sb-thread:with-mutex ((kv-lock kv)) (incf (kv-seq kv))))
         (path (dir-file (kv-dir kv) "~A" (kv-segment-name seq)))
         ;; tombstones go only when no older segment holds what they delete
         (next (merged-records group 0 nil :keep-tombstones older-p)))
    (write-segment path next (reduce #'+ group :key #'kseg-records))
    (let ((merged (open-kseg path)))
      (sb-thread:with-mutex ((kv-lock kv))
        (let* ((segments (kv-segments kv))
               (at (search group segments)))
          (invariant at "merged segments are no longer adjacent")
          (let ((new (append (subseq segments 0 at) (list merged)
                             (nthcdr (+ at (length group)) segments))))
            (write-kv-manifest kv (mapcar #'kseg-name new))
            (setf (kv-segments kv) new)
            (incf (kv-merges kv))
            (sb-thread:condition-broadcast (kv-merged kv)))))
      ;; Readers holding the old list keep their mappings until the segments are collected.
      (dolist (s group) (delete-file (kseg-path s))))))

(defun kv-wait-merges (kv)
  "Wait for the running merge, then signal its failure if it failed."
  (let ((thread (sb-thread:with-mutex ((kv-lock kv)) (kv-merger kv))))
    (when thread (sb-thread:join-thread thread :default nil)))
  (kv-check-merge kv))

(defun kv-compact (kv)
  "Merge every segment into one (tombstones dropped)."
  (kv-wait-merges kv)
  (when (rest (kv-segments kv)) (kv-merge kv (kv-segments kv)))
  kv)

(defun kv-close (kv)
  (kv-wait-merges kv)
  (setf (kv-segments kv) '())
  kv)
