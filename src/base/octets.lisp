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
  "Whether A and B hold the same octets: a word at a time (hashes, roots and 256-byte
blooms are compared in every block), then the tail."
  (declare (type octets a b) (optimize (speed 3)))
  (let ((n (length a)))
    (and (= n (length b))
         (let ((words (floor n 8)))
           (and (loop for i of-type fixnum below words
                      always (= (sb-kernel:%vector-raw-bits a i) (sb-kernel:%vector-raw-bits b i)))
                (loop for i of-type fixnum from (* 8 words) below n
                      always (= (aref a i) (aref b i))))))))

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
  (let* ((s (if (simple-string-p s) s (coerce s 'simple-string)))
         (start (if (and (>= (length s) 2) (char= (schar s 0) #\0) (char-equal (schar s 1) #\x)) 2 0))
         (digits (- (length s) start))
         (o (make-octets (ceiling digits 2))))
    (declare (type simple-string s) (type fixnum start digits))
    (flet ((nibble (i) (hex-digit-value (schar s i))))
      (declare (inline nibble))
      ;; an odd count: the first digit is the low nibble of the first octet
      (let ((j start) (k 0))
        (declare (type fixnum j k))
        (when (oddp digits)
          (setf (aref o 0) (nibble j)) (incf j) (incf k))
        (loop while (< j (length s))
              do (setf (aref o k) (+ (* 16 (nibble j)) (nibble (1+ j))))
                 (incf j 2) (incf k))))
    o))

(defun octets->hex (o &key (prefix t) (start 0) (end (length o)))
  (declare (type octets o) (type fixnum start end))
  (invariant (<= 0 start end (length o)) "octets->hex: bad bounds ~D..~D" start end)
  (let ((s (make-string (+ (if prefix 2 0) (* 2 (- end start)))))
        (digits "0123456789abcdef"))
    (declare (type simple-string digits))
    (when prefix (setf (schar s 0) #\0 (schar s 1) #\x))
    (locally (declare #.*hot*)
      (loop for i of-type fixnum from start below end
            for j of-type fixnum from (if prefix 2 0) by 2
            do (let ((b (aref o i)))
                 (setf (schar s j) (schar digits (ash b -4))
                       (schar s (1+ j)) (schar digits (logand b 15))))))
    s))

(defun hex->integer (s)
  (let ((h (strip-0x s)))
    (if (zerop (length h)) 0 (parse-integer h :radix 16))))

(defun hex-into (s n end)
  "Write the non-negative integer N's hex digits into S ending at END-1, leftward to index 2
\(after \"0x\"); positions N does not reach keep their contents."
  (declare #.*hot* (type (simple-array character (*)) s) (type unsigned-byte n) (type fixnum end))
  (let ((digits "0123456789abcdef") (j end))
    (declare (type fixnum j) (type simple-string digits))
    (flet ((word (w count)
             (declare (type (unsigned-byte 64) w) (type fixnum count))
             (dotimes (k count)
               (decf j)
               (setf (schar s j) (schar digits (logand w 15))
                     w (ash w -4)))))
      (if (typep n 'fixnum)
          (word n (- end 2))
          (dotimes (i (sb-bignum:%bignum-length n))
            (when (<= j 2) (return))
            (word (sb-bignum:%bignum-ref n i) (min 16 (- j 2))))))
    s))

(defun integer->hex (n &key (bytes nil))
  "0x-prefixed lowercase hex. With BYTES, left-padded to that many bytes."
  (if (and (integerp n) (>= n 0))
      (let* ((width (max (if bytes (* 2 bytes) 1) (ceiling (integer-length n) 4)))
             (s (make-string (+ 2 width) :initial-element #\0)))
        (setf (schar s 1) #\x)
        (hex-into s n (+ 2 width)))
      (if bytes
          (format nil "0x~(~v,'0x~)" (* 2 bytes) n)
          (format nil "0x~(~x~)" n))))

(defun quantity->hex (n)
  "JSON-RPC quantity encoding: minimal hex, 0 is 0x0."
  (integer->hex n))

;;; Integers <-> big-endian octets

;;; Both directions move whole 64-bit bignum digits: shifting a growing bignum one byte at
;;; a time is quadratic and allocates a bignum per byte.

(defun integer-into-octets (n o start end)
  "Write N big-endian into O[START, END), zero-padded on the left. N must fit."
  (declare #.*hot* (type unsigned-byte n) (type octets o) (type fixnum start end))
  (invariant (and (<= 0 start end (length o)) (<= (integer-length n) (* 8 (- end start))))
             "~D does not fit in ~D bytes" n (- end start))
  (if (typep n 'fixnum)
      (let ((v n))
        (declare (type (and fixnum unsigned-byte) v))
        (loop for i fixnum from (1- end) downto start
              do (setf (aref o i) (logand v 255) v (ash v -8))))
      (let ((digits (sb-bignum:%bignum-length n)))
        (declare (type fixnum digits))
        (loop for k fixnum from 0
              for hi fixnum = (- end (* 8 k))
              while (> hi start)
              do (let ((w (if (< k digits) (sb-bignum:%bignum-ref n k) 0)))
                   (declare (type (unsigned-byte 64) w))
                   (loop for i fixnum from (1- hi) downto (max start (- hi 8))
                         do (setf (aref o i) (logand w 255) w (ash w -8)))))))
  o)

(defun integer->octets (n len)
  (declare (type unsigned-byte n) (type fixnum len))
  (integer-into-octets n (make-octets len) 0 len))

(defun integer->octets-minimal (n)
  "Big-endian with no leading zeros; 0 is the empty vector."
  (integer->octets n (ceiling (integer-length n) 8)))

(defun octets->integer (o &key (start 0) (end (length o)))
  (declare #.*hot* (type octets o) (type fixnum start end))
  (invariant (<= 0 start end (length o)) "bad octet range ~D..~D of ~D" start end (length o))
  (let ((n (- end start)))
    (declare (type fixnum n))
    (if (<= n 7)
        (let ((v 0))
          (declare (type (unsigned-byte 56) v))
          (loop for i fixnum from start below end do (setf v (logior (ash v 8) (aref o i))))
          v)
        ;; One extra digit keeps the top bit clear: bignums are two's complement.
        (let* ((digits (1+ (ceiling n 8)))
               (b (sb-bignum:%allocate-bignum digits)))
          (declare (type fixnum digits))
          (dotimes (k digits)
            (let* ((hi (- end (* 8 k)))
                   (w 0))
              (declare (type fixnum hi) (type (unsigned-byte 64) w))
              (loop for i fixnum from (max start (- hi 8)) below hi
                    do (setf w (logior (ldb (byte 64 0) (ash w 8)) (aref o i))))
              (sb-bignum:%bignum-set b k w)))
          (sb-bignum::%normalize-bignum b digits)))))

;;; Strings

(defun string->octets (s)
  (coerce (sb-ext:string-to-octets s :external-format :utf-8) '(simple-array (unsigned-byte 8) (*))))

(defun octets->string (o &key (start 0) (end (length o)))
  (sb-ext:octets-to-string o :external-format :utf-8 :start start :end end))
