(defpackage #:clempo.archive
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:rpc #:clempo.jsonrpc) (#:io #:clempo.io))
  (:documentation "The raw block archive: mainnet blocks as received from debug_getRawBlock,
in chunk files of +CHUNK-BLOCKS+ consecutive blocks.

Chunk file layout (all integers big-endian):
  \"CLBK\" | u64 first-block | u32 count | (count+1) x u64 offsets | block bytes...
Offsets are relative to the start of the data section, so block i is
data[offset(i), offset(i+1)). Files are written once, atomically, and never modified.")
  (:export #:+chunk-blocks+ #:*archive-dir* #:archive-dir #:chunk-path #:chunk-present-p
           #:write-chunk #:read-chunk #:chunk-block #:chunk-first #:chunk-count
           #:raw-block #:fetch-blocks #:fetch-chunk #:fetch-range #:archive-contiguous-height))

(in-package #:clempo.archive)

(define-constant +chunk-blocks+ 10000)

(defvar *archive-dir* nil "Directory of chunk files; NIL means (archive-dir \"mainnet\").")

(defun archive-dir (&optional (chain "mainnet"))
  (or *archive-dir*
      (format nil "~A/cache/clempo/~A/" (io:getenv "HOME") chain)))

(defun chunk-path (first &optional (dir (archive-dir)))
  (format nil "~Achunk-~10,'0D.blk" dir first))

(defun chunk-present-p (first &optional (dir (archive-dir)))
  (probe-file (chunk-path first dir)))

(defstruct (chunk (:constructor %make-chunk))
  (first 0 :type (unsigned-byte 64))
  (count 0 :type fixnum)
  (bytes nil :type octets)
  (data-start 0 :type fixnum))

(defun put-u (o pos n bytes)
  (loop for i from 0 below bytes
        do (setf (aref o (+ pos i)) (ldb (byte 8 (* 8 (- bytes i 1))) n))))

(defun write-chunk (first blocks &optional (dir (archive-dir)))
  "BLOCKS is a vector of octet vectors, block FIRST first."
  (let* ((count (length blocks))
         (header (+ 4 8 4 (* 8 (1+ count))))
         (total (+ header (loop for b across blocks sum (length b))))
         (o (make-octets total))
         (pos header)
         (rel 0))
    (replace o (string->octets "CLBK"))
    (put-u o 4 first 8)
    (put-u o 12 count 4)
    (loop for b across blocks
          for i from 0
          do (put-u o (+ 16 (* 8 i)) rel 8)
             (replace o b :start1 pos)
             (incf pos (length b))
             (incf rel (length b)))
    (put-u o (+ 16 (* 8 count)) rel 8)
    (ensure-directories-exist dir)
    (io:write-file-atomically (chunk-path first dir) o)))

(defun read-chunk (first &optional (dir (archive-dir)))
  (let* ((o (io:read-file-octets (chunk-path first dir)))
         (count (octets->integer o :start 12 :end 16)))
    (invariant (string= "CLBK" (octets->string o :start 0 :end 4)) "bad chunk magic")
    (invariant (= first (octets->integer o :start 4 :end 12)) "chunk ~D has wrong first block" first)
    (%make-chunk :first first :count count :bytes o :data-start (+ 16 (* 8 (1+ count))))))

(defun chunk-block (chunk n)
  "Raw block N (absolute number) from CHUNK, as a fresh octet vector."
  (let* ((i (- n (chunk-first chunk)))
         (o (chunk-bytes chunk))
         (base (chunk-data-start chunk)))
    (invariant (< -1 i (chunk-count chunk)) "block ~D not in chunk ~D" n (chunk-first chunk))
    (let ((a (octets->integer o :start (+ 16 (* 8 i)) :end (+ 24 (* 8 i))))
          (b (octets->integer o :start (+ 24 (* 8 i)) :end (+ 32 (* 8 i)))))
      (subseq o (+ base a) (+ base b)))))

(defun raw-block (n &optional (dir (archive-dir)))
  "One raw block from the archive (reads its whole chunk; use READ-CHUNK for runs)."
  (chunk-block (read-chunk (* +chunk-blocks+ (floor n +chunk-blocks+)) dir) n))

;;; Fetching from an upstream node.

(defun fetch-blocks (from count &key url (batch 500))
  "Raw blocks FROM..FROM+COUNT-1 via debug_getRawBlock, as a vector."
  (let ((out (make-array count)))
    (loop for start from 0 below count by batch
          do (let* ((n (min batch (- count start)))
                    (results (rpc:rpc-batch
                              (loop for i from 0 below n
                                    collect (list "debug_getRawBlock"
                                                  (list (quantity->hex (+ from start i)))))
                              :url (or url (rpc:local-rpc-url)))))
               (loop for r in results
                     for i from start
                     do (invariant (stringp r) "block ~D: ~A" (+ from i) r)
                        (setf (aref out i) (hex->octets r)))))
    out))

(defun fetch-chunk (first &key url (dir (archive-dir)) (last nil))
  "Fetch and write the chunk starting at FIRST. A chunk ending past LAST (the upstream
head) is not written: chunks are always complete."
  (invariant (zerop (mod first +chunk-blocks+)))
  (when (and last (> (+ first +chunk-blocks+ -1) last))
    (return-from fetch-chunk nil))
  (write-chunk first (fetch-blocks first +chunk-blocks+ :url url) dir)
  t)

(defun fetch-range (from to &key url (threads 8) (dir (archive-dir)) (log t))
  "Fetch every missing complete chunk covering FROM..TO with THREADS workers."
  (let* ((head (hex->integer (rpc:rpc-call "eth_blockNumber" '() :url (or url (rpc:local-rpc-url)))))
         (to (min to head))
         (firsts (loop for c from (* +chunk-blocks+ (floor from +chunk-blocks+)) to to by +chunk-blocks+
                       unless (chunk-present-p c dir) collect c))
         (queue firsts)
         (lock (sb-thread:make-mutex :name "fetch queue"))
         (done 0)
         (errors '())
         (start (clempo.io:monotonic-ticks)))
    (flet ((worker ()
             (loop
               (let ((c (clempo.lock:with-lock (lock) (pop queue))))
                 (unless c (return))
                 (handler-case
                     (progn
                       (fetch-chunk c :url url :dir dir :last to)
                       (clempo.lock:with-lock (lock)
                         (incf done)
                         (when (and log (zerop (mod done 10)))
                           (let ((secs (/ (- (clempo.io:monotonic-ticks) start)
                                          internal-time-units-per-second)))
                             (format *error-output* "~&archive: ~D/~D chunks, ~,0F blocks/s~%"
                                     done (length firsts)
                                     (/ (* done +chunk-blocks+) (max secs 1/1000)))
                             (force-output *error-output*)))))
                   (error (e)
                     (clempo.lock:with-lock (lock) (push (cons c (princ-to-string e)) errors))))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (max 1 threads) collect (sb-thread:make-thread #'worker :name "fetch"))))
    (values done (nreverse errors))))

(defun archive-contiguous-height (&optional (dir (archive-dir)))
  "The last block N such that every chunk from 0 through N's is present."
  (loop for c from 0 by +chunk-blocks+
        while (chunk-present-p c dir)
        finally (return (1- c))))
