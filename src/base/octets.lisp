(in-package #:clempo.base)

(deftype octet () '(unsigned-byte 8))
(deftype octets (&optional (n '*)) `(simple-array (unsigned-byte 8) (,n)))
(deftype u64 () '(unsigned-byte 64))
(deftype u160 () '(unsigned-byte 160))
(deftype u256 () '(unsigned-byte 256))

(declaim (inline make-octets))
(defun make-octets (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun octets-of (&rest bytes)
  (let ((o (make-octets (length bytes))))
    (loop for b in bytes for i from 0 do (setf (aref o i) b))
    o))

(defun copy-octets (o)
  (declare (type octets o))
  (copy-seq o))

(defun octets-slice (o start &optional (end (length o)))
  (declare (type octets o) (type fixnum start end))
  (subseq o start end))

(defun octets-concat (&rest parts)
  (let* ((n (loop for p in parts sum (length (the octets p))))
         (out (make-octets n))
         (pos 0))
    (declare (type fixnum pos))
    (dolist (p parts out)
      (declare (type octets p))
      (replace out p :start1 pos)
      (incf pos (length p)))))

(defun octets= (a b)
  (declare (type octets a b))
  (and (= (length a) (length b))
       (loop for x across a for y across b always (= x y))))

;;; Hex

(declaim (inline hex-digit-value))
(defun hex-digit-value (ch)
  (let ((v (digit-char-p ch 16)))
    (invariant v "bad hex digit ~S" ch)
    v))

(defun strip-0x (s)
  (if (and (>= (length s) 2) (char= (char s 0) #\0) (char-equal (char s 1) #\x))
      (subseq s 2)
      s))

(defun hex->octets (s)
  "Decode hex (with or without 0x). Odd lengths get a leading zero nibble."
  (let* ((h (strip-0x s))
         (h (if (oddp (length h)) (concatenate 'string "0" h) h))
         (o (make-octets (/ (length h) 2))))
    (dotimes (i (length o) o)
      (setf (aref o i) (+ (* 16 (hex-digit-value (char h (* 2 i))))
                          (hex-digit-value (char h (1+ (* 2 i)))))))))

(defun octets->hex (o &key (prefix t) (start 0) (end (length o)))
  (declare (type octets o))
  (let ((s (make-string (+ (if prefix 2 0) (* 2 (- end start)))))
        (digits "0123456789abcdef"))
    (when prefix (setf (char s 0) #\0 (char s 1) #\x))
    (loop for i from start below end
          for j from (if prefix 2 0) by 2
          do (setf (char s j) (char digits (ash (aref o i) -4))
                   (char s (1+ j)) (char digits (logand (aref o i) 15))))
    s))

(defun hex->integer (s)
  (let ((h (strip-0x s)))
    (if (zerop (length h)) 0 (parse-integer h :radix 16))))

(defun integer->hex (n &key (bytes nil))
  "0x-prefixed lowercase hex. With BYTES, left-padded to that many bytes."
  (if bytes
      (format nil "0x~(~v,'0x~)" (* 2 bytes) n)
      (format nil "0x~(~x~)" n)))

(defun quantity->hex (n)
  "JSON-RPC quantity encoding: minimal hex, 0 is 0x0."
  (format nil "0x~(~x~)" n))

;;; Integers <-> big-endian octets

(defun integer->octets (n len)
  (declare (type unsigned-byte n) (type fixnum len))
  (invariant (<= (integer-length n) (* 8 len)) "~D does not fit in ~D bytes" n len)
  (let ((o (make-octets len)))
    (loop for i from (1- len) downto 0
          for shift from 0 by 8
          do (setf (aref o i) (ldb (byte 8 shift) n)))
    o))

(defun integer->octets-minimal (n)
  "Big-endian with no leading zeros; 0 is the empty vector."
  (integer->octets n (ceiling (integer-length n) 8)))

(defun octets->integer (o &key (start 0) (end (length o)))
  (declare (type octets o) (type fixnum start end))
  (let ((n 0))
    (loop for i from start below end
          do (setf n (logior (ash n 8) (aref o i))))
    n))

;;; Strings

(defun string->octets (s)
  (coerce (sb-ext:string-to-octets s :external-format :utf-8) '(simple-array (unsigned-byte 8) (*))))

(defun octets->string (o &key (start 0) (end (length o)))
  (sb-ext:octets-to-string o :external-format :utf-8 :start start :end end))
