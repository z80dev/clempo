(defpackage #:clempo.rlp
  (:use #:cl #:clempo.base)
  (:documentation "Recursive Length Prefix encoding. Items are octet vectors and lists of
items; integers encode as minimal big-endian strings.")
  (:export #:rlp-encode #:rlp-encode-into #:rlp-encoded-length
           #:rlp-decode #:rlp-decode-integer #:rlp-item-bounds #:*rlp-max-depth*
           #:rlp-error #:rlp-raw #:make-rlp-raw #:rlp-raw-octets #:header-length #:write-header))

(in-package #:clempo.rlp)

(define-condition rlp-error (error)
  ((message :initarg :message :reader rlp-error-message))
  (:report (lambda (c s) (format s "rlp: ~A" (rlp-error-message c)))))

(defun rlp-fail (fmt &rest args)
  (error 'rlp-error :message (apply #'format nil fmt args)))

(defstruct (rlp-raw (:constructor make-rlp-raw (octets)))
  "An already-encoded item, spliced verbatim."
  (octets nil :type octets))

;;; Encoding: one pass to size, one pass to write.

(declaim (inline length-of-length))
(defun length-of-length (n)
  (declare (type fixnum n))
  (ceiling (integer-length n) 8))

(defun header-length (payload)
  (declare (type fixnum payload))
  (if (<= payload 55) 1 (1+ (length-of-length payload))))

(defun rlp-encoded-length (item)
  (etypecase item
    (octets (let ((n (length item)))
              (if (and (= n 1) (< (aref item 0) #x80)) 1 (+ n (header-length n)))))
    (integer (invariant (>= item 0) "negative integer ~D" item)
     (if (< item #x80)
         1
         (let ((n (ceiling (integer-length item) 8))) (+ n (header-length n)))))
    (list (let ((n (loop for x in item sum (rlp-encoded-length x))))
            (+ n (header-length n))))
    (rlp-raw (length (rlp-raw-octets item)))
    (string (rlp-encoded-length (string->octets item)))))

(defun write-header (out pos payload short-base)
  (declare (type octets out) (type fixnum pos payload short-base))
  (if (<= payload 55)
      (progn (setf (aref out pos) (+ short-base payload)) (1+ pos))
      (let ((ll (length-of-length payload)))
        (setf (aref out pos) (+ short-base 55 ll))
        (loop for i from 1 to ll
              do (setf (aref out (+ pos i)) (ldb (byte 8 (* 8 (- ll i))) payload)))
        (+ pos 1 ll))))

(defun rlp-encode-into (item out pos)
  "Write ITEM at POS of OUT; return the position after it."
  (declare (type octets out) (type fixnum pos))
  (etypecase item
    (octets
     (let ((n (length item)))
       (if (and (= n 1) (< (aref item 0) #x80))
           (progn (setf (aref out pos) (aref item 0)) (1+ pos))
           (let ((p (write-header out pos n #x80)))
             (replace out item :start1 p)
             (+ p n)))))
    (integer
     (if (< item #x80)
         (progn (setf (aref out pos) (if (zerop item) #x80 item)) (1+ pos))
         (let* ((n (ceiling (integer-length item) 8))
                (p (write-header out pos n #x80)))
           (loop for i from 0 below n
                 do (setf (aref out (+ p i)) (ldb (byte 8 (* 8 (- n i 1))) item)))
           (+ p n))))
    (list
     (let* ((payload (loop for x in item sum (rlp-encoded-length x)))
            (p (write-header out pos payload #xc0)))
       (dolist (x item p)
         (setf p (rlp-encode-into x out p)))))
    (rlp-raw
     (let ((o (rlp-raw-octets item)))
       (replace out o :start1 pos)
       (+ pos (length o))))
    (string (rlp-encode-into (string->octets item) out pos))))

(defun rlp-encode (item)
  (let ((out (make-octets (rlp-encoded-length item))))
    (rlp-encode-into item out 0)
    out))

;;; Decoding

(defun rlp-item-bounds (data pos end)
  "Parse the header at POS. Returns (values payload-start payload-end list-p).
Rejects non-canonical headers."
  (declare (type octets data) (type fixnum pos end))
  (when (>= pos end) (rlp-fail "unexpected end at ~D" pos))
  (let ((b (aref data pos)))
    (flet ((long-length (ll)
             (when (> (+ pos 1 ll) end) (rlp-fail "truncated length at ~D" pos))
             (when (zerop (aref data (1+ pos))) (rlp-fail "length with leading zero at ~D" pos))
             (let ((n (octets->integer data :start (1+ pos) :end (+ pos 1 ll))))
               (when (<= n 55) (rlp-fail "long form for short payload at ~D" pos))
               n)))
      (multiple-value-bind (start len list-p)
          (cond ((< b #x80) (values pos 1 nil))
                ((<= b #xb7)
                 (let ((n (- b #x80)))
                   (when (and (= n 1) (< (1+ pos) end) (< (aref data (1+ pos)) #x80))
                     (rlp-fail "single byte encoded as string at ~D" pos))
                   (values (1+ pos) n nil)))
                ((< b #xc0)
                 (let ((ll (- b #xb7)))
                   (values (+ pos 1 ll) (long-length ll) nil)))
                ((<= b #xf7) (values (1+ pos) (- b #xc0) t))
                (t (let ((ll (- b #xf7)))
                     (values (+ pos 1 ll) (long-length ll) t))))
        (when (> (+ start len) end) (rlp-fail "item overruns input at ~D" pos))
        (values start (+ start len) list-p)))))

(defvar *rlp-max-depth* 32
  "The most lists a decoded item may nest, goempo's DepthMax; Tempo's own structures nest
far less (tests/types/transactions.lisp measures them). Decoding recurses once per level, so
this also bounds the control stack on hostile input.")

(defun decode-at (data pos end depth)
  (declare (type octets data) (type fixnum pos end depth))
  (multiple-value-bind (start stop list-p) (rlp-item-bounds data pos end)
    (if list-p
        (let ((items '()) (p start))
          (when (zerop depth) (rlp-fail "lists nest too deep at ~D" pos))
          (loop while (< p stop)
                do (multiple-value-bind (item next) (decode-at data p stop (1- depth))
                     (push item items)
                     (setf p next)))
          (values (nreverse items) stop))
        (values (subseq data start stop) stop))))

(defun rlp-decode (data &key (start 0) (end (length data)) (allow-trailing nil)
                          (max-depth *rlp-max-depth*))
  "Decode one item. Returns (values item position-after). Lists may nest MAX-DEPTH deep."
  (unless (and (typep data 'octets) (<= 0 start end (length data)))
    (rlp-fail "invalid input range ~S..~S" start end))
  (multiple-value-bind (item next) (decode-at data start end max-depth)
    (unless (or allow-trailing (= next end))
      (rlp-fail "~D trailing bytes" (- end next)))
    (values item next)))

(defun rlp-decode-integer (o &key (max-bytes 32))
  "Canonical unsigned integer from a decoded string item."
  (unless (typep o 'octets) (rlp-fail "expected string, got list"))
  (when (> (length o) max-bytes) (rlp-fail "integer longer than ~D bytes" max-bytes))
  (when (and (plusp (length o)) (zerop (aref o 0))) (rlp-fail "integer with leading zero"))
  (octets->integer o))
