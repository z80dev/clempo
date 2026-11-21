(in-package #:clempo.store)

;;; Little binary codec for checkpoints and the block log: big-endian fixed-width integers,
;;; length-prefixed integers and byte strings, written through a buffer that drains to a
;;; stream (or grows in memory) and read from a buffer refilled from a stream.

(defconstant +buffer-size+ (* 1024 1024))

(defstruct (sink (:constructor %make-sink (stream buffer)))
  stream                                   ; NIL: grow BUFFER in memory
  (buffer (make-octets 0) :type octets)
  (pos 0 :type fixnum))

(defun make-sink (&optional stream)
  (%make-sink stream (make-octets (if stream +buffer-size+ 4096))))

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
