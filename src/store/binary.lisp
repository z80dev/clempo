(in-package #:clempo.store)

;;; Little binary codec for checkpoints and the block log: big-endian fixed-width integers,
;;; length-prefixed integers and byte strings, written through a buffer that drains to a
;;; stream (or grows in memory) and read from a buffer refilled from a stream.

(defconstant +buffer-size+ (* 1024 1024))

(defstruct (sink (:constructor %make-sink (stream buffer)))
  stream                                   ; NIL: grow BUFFER in memory
  (buffer (make-octets 0) :type octets)
  (pos 0 :type fixnum))

(defun make-sink (&optional stream (size 256))
  "A sink draining to STREAM, or growing in memory from SIZE bytes."
  (%make-sink stream (make-octets (if stream +buffer-size+ size))))

(defun sink-reset (sink)
  "Empty an in-memory sink for reuse."
  (setf (sink-pos sink) 0)
  sink)

(defun sink-flush (sink)
  (when (sink-stream sink)
    (write-sequence (sink-buffer sink) (sink-stream sink) :end (sink-pos sink))
    (setf (sink-pos sink) 0))
  sink)

(defun sink-octets (sink)
  "The bytes of an in-memory sink."
  (subseq (sink-buffer sink) 0 (sink-pos sink)))

(declaim (inline sink-room))
(defun sink-room (sink n)
  (declare (type fixnum n))
  (let ((need (+ (sink-pos sink) n)))
    (when (> need (length (sink-buffer sink)))
      (if (sink-stream sink)
          (progn (sink-flush sink)
                 (when (> n (length (sink-buffer sink)))
                   (setf (sink-buffer sink) (make-octets n))))
          (let ((new (make-octets (max need (* 2 (length (sink-buffer sink)))))))
            (replace new (sink-buffer sink) :end2 (sink-pos sink))
            (setf (sink-buffer sink) new))))))

(defun put-uint (sink n width)
  (declare (type (integer 0) n) (type (integer 1 8) width))
  (sink-room sink width)
  (let ((buf (sink-buffer sink)) (pos (sink-pos sink)))
    (loop for i from (1- width) downto 0
          do (setf (aref buf (+ pos i)) (ldb (byte 8 0) n)
                   n (ash n -8)))
    (setf (sink-pos sink) (+ pos width))))

(defun put-u8 (sink n) (put-uint sink n 1))
(defun put-u32 (sink n) (put-uint sink n 4))
(defun put-u64 (sink n) (put-uint sink n 8))

(defun put-raw (sink octets)
  (declare (type octets octets))
  (sink-room sink (length octets))
  (replace (sink-buffer sink) octets :start1 (sink-pos sink))
  (incf (sink-pos sink) (length octets)))

(defun put-int (sink n)
  "A non-negative integer as one length byte and its minimal big-endian bytes."
  (declare (type (integer 0) n))
  (let ((len (ceiling (integer-length n) 8)))
    (invariant (< len 256))
    (put-u8 sink len)
    (sink-room sink len)
    (let ((buf (sink-buffer sink)) (pos (sink-pos sink)))
      (loop for i from (1- len) downto 0
            do (setf (aref buf (+ pos i)) (ldb (byte 8 0) n)
                     n (ash n -8)))
      (setf (sink-pos sink) (+ pos len)))))

(defun put-bytes (sink octets)
  (put-u32 sink (length octets))
  (put-raw sink octets))

(define-condition store-format-error (error)
  ((message :initarg :message :reader store-format-error-message))
  (:report (lambda (c s) (format s "store: ~A" (store-format-error-message c)))))

(defun format-error (fmt &rest args)
  (error 'store-format-error :message (apply #'format nil fmt args)))

(defstruct (source (:constructor %make-source (stream buffer end)))
  stream
  (buffer (make-octets 0) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-source (stream-or-octets)
  (if (streamp stream-or-octets)
      (%make-source stream-or-octets (make-octets +buffer-size+) 0)
      (%make-source nil stream-or-octets (length stream-or-octets))))

(defun source-available-p (source n)
  "Refill from the stream if needed; true when N bytes are available at POS."
  (declare (type fixnum n))
  (when (and (source-stream source) (> (+ (source-pos source) n) (source-end source)))
    (let* ((old (source-buffer source))
           (left (- (source-end source) (source-pos source)))
           (buf (if (> n (length old)) (make-octets (max n +buffer-size+)) old)))
      ;; REPLACE copies overlapping regions of one vector as if through a temporary.
      (replace buf old :start2 (source-pos source) :end2 (source-end source))
      (setf (source-buffer source) buf
            (source-pos source) 0
            (source-end source) (read-sequence buf (source-stream source) :start left))))
  (<= (+ (source-pos source) n) (source-end source)))

(defun source-exhausted-p (source)
  (not (source-available-p source 1)))

(defun source-fill (source n)
  (unless (source-available-p source n)
    (format-error "truncated input"))
  source)

(defun get-uint (source width)
  (source-fill source width)
  (let ((buf (source-buffer source)) (pos (source-pos source)) (n 0))
    (dotimes (i width) (setf n (logior (ash n 8) (aref buf (+ pos i)))))
    (setf (source-pos source) (+ pos width))
    n))

(defun get-u8 (source) (get-uint source 1))
(defun get-u32 (source) (get-uint source 4))
(defun get-u64 (source) (get-uint source 8))

(defun get-raw (source n)
  (source-fill source n)
  (let ((o (subseq (source-buffer source) (source-pos source) (+ (source-pos source) n))))
    (incf (source-pos source) n)
    o))

(defun get-int (source)
  (let ((len (get-u8 source)))
    (source-fill source len)
    (let ((buf (source-buffer source)) (pos (source-pos source)) (n 0))
      (dotimes (i len) (setf n (logior (ash n 8) (aref buf (+ pos i)))))
      (setf (source-pos source) (+ pos len))
      n)))

(defun get-bytes (source) (get-raw source (get-u32 source)))

(defun put-raw-range (sink octets start end)
  (declare (type octets octets) (type fixnum start end))
  (sink-room sink (- end start))
  (replace (sink-buffer sink) octets :start1 (sink-pos sink) :start2 start :end2 end)
  (incf (sink-pos sink) (- end start)))

(defun put-varint (sink n)
  "N (below 2^63) in 7-bit groups, low first."
  (declare (type (unsigned-byte 63) n))
  (loop (if (< n 128)
            (return (put-u8 sink n))
            (progn (put-u8 sink (logior 128 (ldb (byte 7 0) n)))
                   (setf n (ash n -7))))))

(defun get-varint (source)
  (let ((n 0) (shift 0))
    (loop (let ((byte (get-u8 source)))
            (when (> shift 56) (format-error "overlong varint"))
            (setf n (logior n (ash (logand byte 127) shift)))
            (when (< byte 128) (return n))
            (incf shift 7)))))

;;; CRC-32C (Castagnoli), the checksum of log records and segment blocks.

(declaim (type (simple-array (unsigned-byte 32) (256)) +crc32c-table+))
(sb-ext:defglobal +crc32c-table+
    (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
      (dotimes (i 256 table)
        (let ((c i))
          (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #x82F63B78 (ash c -1)) (ash c -1))))
          (setf (aref table i) c)))))

(defun crc32c (octets &optional (start 0) (end (length octets)) (crc 0))
  "CRC-32C of OCTETS[START, END), continuing from CRC."
  (declare #.*hot* (type octets octets) (type fixnum start end) (type (unsigned-byte 32) crc))
  (invariant (<= 0 start end (length octets)))
  (let ((c (logxor crc #xFFFFFFFF))
        (table +crc32c-table+))
    (declare (type (unsigned-byte 32) c))
    (loop for i of-type fixnum from start below end
          do (setf c (logxor (aref table (logand (logxor c (aref octets i)) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

;;; Files

(defun dir-file (dir fmt &rest args)
  (merge-pathnames (apply #'format nil fmt args) dir))

;;; Durable writes. On macOS fsync(2) leaves data in the drive's cache; F_FULLFSYNC flushes it
;;; to stable storage, which is what a durability promise needs. Elsewhere fsync does that.

(defvar *full-sync* t
  "NIL makes macOS syncs plain fsync(2): safe against a process crash (the kernel keeps the
data) but not a power loss. The store's tests bind it, since a killed process is what they
can simulate; nothing else should.")

(defun sync-fd (fd)
  #+darwin (if *full-sync*
               (handler-case (sb-posix:fcntl fd 51) ; F_FULLFSYNC
                 ;; a file system without it (some network mounts): plain fsync
                 (sb-posix:syscall-error () (sb-posix:fsync fd)))
               (sb-posix:fsync fd))
  #-darwin (sb-posix:fsync fd)
  fd)

(defun sync-stream (stream)
  (finish-output stream)
  (sync-fd (sb-sys:fd-stream-fd stream)))

(defun sync-directory (dir)
  (let ((fd (sb-posix:open (namestring dir) sb-posix:o-rdonly)))
    (unwind-protect (sync-fd fd) (sb-posix:close fd))))

(defun write-durably (path writer)
  "Call WRITER with an octet output stream on PATH.tmp, sync it, rename it over PATH and
sync the directory."
  (let ((tmp (make-pathname :type (format nil "~A-tmp" (pathname-type path)) :defaults path)))
    (with-open-file (out tmp :element-type '(unsigned-byte 8) :direction :output
                             :if-exists :supersede :if-does-not-exist :create)
      (funcall writer out)
      (sync-stream out))
    (sb-posix:rename (namestring tmp) (namestring path))
    (sync-directory (make-pathname :name nil :type nil :defaults path))
    path))
