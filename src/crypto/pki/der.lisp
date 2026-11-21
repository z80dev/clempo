(defpackage #:clempo.der
  (:use #:cl #:clempo.base)
  (:documentation "A strict ASN.1 DER reader (X.690). An element is a TLV view into the
input octets; nothing is copied until a value is asked for.")
  (:export #:der-error #:der-error-message #:tlv #:tlv-tag #:tlv-data #:tlv-start #:tlv-end #:tlv-header-start
           #:der-parse #:der-children #:der-content #:der-raw #:der-integer #:der-oid
           #:der-bit-string #:der-boolean #:der-string #:der-time #:der-expect
           #:+sequence+ #:+set+ #:+integer+ #:+bit-string+ #:+octet-string+ #:+null+ #:+oid+
           #:+boolean+ #:+utc-time+ #:+generalized-time+))
(in-package #:clempo.der)

(define-condition der-error (error)
  ((message :initarg :message :reader der-error-message))
  (:report (lambda (c s) (format s "DER: ~A" (der-error-message c)))))

(defun der-fail (fmt &rest args) (error 'der-error :message (apply #'format nil fmt args)))

(defconstant +boolean+ #x01)
(defconstant +integer+ #x02)
(defconstant +bit-string+ #x03)
(defconstant +octet-string+ #x04)
(defconstant +null+ #x05)
(defconstant +oid+ #x06)
(defconstant +utc-time+ #x17)
(defconstant +generalized-time+ #x18)
(defconstant +sequence+ #x30)
(defconstant +set+ #x31)

(defstruct (tlv (:constructor make-tlv (tag data header-start start end)))
  "Identifier octet TAG; content is DATA[START,END); the element begins at HEADER-START."
  (tag 0 :type (unsigned-byte 8) :read-only t)
  (data nil :type octets :read-only t)
  (header-start 0 :type fixnum :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t))

(defun read-element (data pos limit)
  "The element at POS that must end by LIMIT."
  (when (>= (+ pos 2) (1+ limit)) (der-fail "truncated header at ~D" pos))
  (let ((tag (aref data pos)) (len (aref data (1+ pos))) (start (+ pos 2)))
    (when (= (logand tag #x1f) #x1f) (der-fail "multi-byte tag at ~D" pos))
    (when (>= len #x80)
      (let ((count (logand len #x7f)))
        (when (or (zerop count) (> count 4)) (der-fail "unsupported length form at ~D" pos))
        (when (> (+ start count) limit) (der-fail "truncated length at ~D" pos))
        (setf len (octets->integer data :start start :end (+ start count)))
        (when (or (< len #x80) (zerop (aref data start))) (der-fail "non-minimal length at ~D" pos))
        (incf start count)))
    (when (> (+ start len) limit) (der-fail "element at ~D overruns its container" pos))
    (make-tlv tag data pos start (+ start len))))

(defun der-parse (data &key (start 0) (end (length data)) (whole t))
  "The element at START. With WHOLE, it must fill DATA up to END."
  (let ((e (read-element data start end)))
    (when (and whole (/= (tlv-end e) end)) (der-fail "trailing bytes after element"))
    e))

(defun der-children (tlv &optional tag)
  "The elements inside constructed TLV, checking TLV's tag against TAG if given."
  (when tag (der-expect tlv tag))
  (unless (logbitp 5 (tlv-tag tlv)) (der-fail "tag ~X is not constructed" (tlv-tag tlv)))
  (loop with pos = (tlv-start tlv)
        while (< pos (tlv-end tlv))
        collect (let ((e (read-element (tlv-data tlv) pos (tlv-end tlv))))
                  (setf pos (tlv-end e))
                  e)))

(defun der-expect (tlv tag)
  (unless (and tlv (= (tlv-tag tlv) tag))
    (der-fail "expected tag ~2,'0X, got ~:[nothing~;~:*~2,'0X~]" tag (and tlv (tlv-tag tlv))))
  tlv)

(defun der-content (tlv) (subseq (tlv-data tlv) (tlv-start tlv) (tlv-end tlv)))
(defun der-raw (tlv) (subseq (tlv-data tlv) (tlv-header-start tlv) (tlv-end tlv)))

(defun der-integer (tlv &key (tag +integer+))
  "A two's-complement INTEGER."
  (der-expect tlv tag)
  (let ((d (tlv-data tlv)) (s (tlv-start tlv)) (e (tlv-end tlv)))
    (when (= s e) (der-fail "empty INTEGER"))
    (when (and (> (- e s) 1)
               (or (and (= (aref d s) 0) (< (aref d (1+ s)) #x80))
                   (and (= (aref d s) #xff) (>= (aref d (1+ s)) #x80))))
      (der-fail "non-minimal INTEGER"))
    (let ((u (octets->integer d :start s :end e)))
      (if (>= (aref d s) #x80) (- u (ash 1 (* 8 (- e s)))) u))))

(defun der-boolean (tlv)
  (der-expect tlv +boolean+)
  (unless (= (- (tlv-end tlv) (tlv-start tlv)) 1) (der-fail "BOOLEAN length"))
  (case (aref (tlv-data tlv) (tlv-start tlv))
    (0 nil) (#xff t) (t (der-fail "BOOLEAN value"))))

(defun der-oid (tlv)
  "Dotted-decimal string."
  (der-expect tlv +oid+)
  (let ((arcs '()) (value 0) (d (tlv-data tlv)))
    (when (= (tlv-start tlv) (tlv-end tlv)) (der-fail "empty OID"))
    (loop for i from (tlv-start tlv) below (tlv-end tlv)
          for b = (aref d i)
          do (when (and (= value 0) (= b #x80)) (der-fail "non-minimal OID arc"))
             (setf value (logior (ash value 7) (logand b #x7f)))
             (unless (logbitp 7 b) (push value arcs) (setf value 0))
          finally (when (logbitp 7 b) (der-fail "truncated OID")))
    (setf arcs (nreverse arcs))
    (let ((first (first arcs)))
      (format nil "~{~D~^.~}"
              (list* (min 2 (floor first 40)) (- first (* 40 (min 2 (floor first 40)))) (rest arcs))))))

(defun der-bit-string (tlv &key (allow-unused nil))
  "The bytes of a BIT STRING; second value is the unused-bit count."
  (der-expect tlv +bit-string+)
  (when (= (tlv-start tlv) (tlv-end tlv)) (der-fail "empty BIT STRING"))
  (let ((unused (aref (tlv-data tlv) (tlv-start tlv))))
    (when (or (> unused 7) (and (plusp unused) (not allow-unused))
              (and (plusp unused) (= (1+ (tlv-start tlv)) (tlv-end tlv))))
      (der-fail "BIT STRING with ~D unused bits" unused))
    (values (subseq (tlv-data tlv) (1+ (tlv-start tlv)) (tlv-end tlv)) unused)))

(defun der-string (tlv)
  "Any of the ASN.1 character string types as a Lisp string."
  (let ((d (tlv-data tlv)) (s (tlv-start tlv)) (e (tlv-end tlv)))
    (case (tlv-tag tlv)
      (#x0c (octets->string d :start s :end e))
      ((#x13 #x16 #x14 #x1a #x12) (map 'string #'code-char (subseq d s e)))
      (#x1e (let ((out (make-string (floor (- e s) 2))))
              (dotimes (i (length out) out)
                (setf (char out i) (code-char (+ (* 256 (aref d (+ s (* 2 i)))) (aref d (+ s (* 2 i) 1))))))))
      (#x1c (let ((out (make-string (floor (- e s) 4))))
              (dotimes (i (length out) out)
                (setf (char out i) (code-char (octets->integer d :start (+ s (* 4 i)) :end (+ s (* 4 i) 4)))))))
      (t (der-fail "tag ~2,'0X is not a string" (tlv-tag tlv))))))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun der-time (tlv)
  "UTCTime or GeneralizedTime (Z form, whole seconds) as Unix seconds."
  (let* ((text (map 'string #'code-char (der-content tlv)))
         (n (length text)))
    (flet ((num (a b)
             (or (and (every #'digit-char-p (subseq text a b)) (parse-integer text :start a :end b))
                 (der-fail "bad time ~S" text))))
      (multiple-value-bind (year rest)
          (case (tlv-tag tlv)
            (#.+utc-time+ (unless (= n 13) (der-fail "bad UTCTime ~S" text))
             (let ((yy (num 0 2))) (values (if (>= yy 50) (+ 1900 yy) (+ 2000 yy)) 2)))
            (#.+generalized-time+ (unless (= n 15) (der-fail "bad GeneralizedTime ~S" text))
             (values (num 0 4) 4))
            (t (der-fail "tag ~2,'0X is not a time" (tlv-tag tlv))))
        (unless (char= (char text (1- n)) #\Z) (der-fail "time ~S not in UTC" text))
        (let ((month (num rest (+ rest 2))) (day (num (+ rest 2) (+ rest 4)))
              (hour (num (+ rest 4) (+ rest 6))) (minute (num (+ rest 6) (+ rest 8)))
              (second (num (+ rest 8) (+ rest 10))))
          (unless (and (<= 1 month 12) (<= 1 day 31) (< hour 24) (< minute 60) (< second 60))
            (der-fail "bad time ~S" text))
          (- (encode-universal-time second minute hour day month year 0) +unix-epoch+))))))
