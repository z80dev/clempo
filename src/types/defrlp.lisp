(defpackage #:clempo.defrlp
  (:use #:cl #:clempo.base)
  (:export #:defrlp #:rlp-layout #:rlp-field-documentation #:decode-field #:encode-field))
(in-package #:clempo.defrlp)

(defun rlp-layout (name)
  "The declarative field schema, including codecs, trailing flags and documentation."
  (get name 'rlp-layout))

(defun rlp-field-documentation (name)
  (mapcar (lambda (field)
            (list (first field) (second field) (getf (cddr field) :doc)))
          (rlp-layout name)))

(defun decode-field (codec item)
  (cond ((eq codec :u64) (clempo.rlp:rlp-decode-integer item :max-bytes 8))
        ((eq codec :u256) (clempo.rlp:rlp-decode-integer item))
        ((eq codec :bytes) (check-type item octets) item)
        ((eq codec :address)
         (invariant (and (typep item 'octets) (= (length item) 20)) "address must be 20 bytes")
         (octets->integer item))
        ((and (consp codec) (eq (car codec) :bytes))
         (invariant (and (typep item 'octets) (= (length item) (second codec)))
                    "expected ~D bytes" (second codec)) item)
        ((and (consp codec) (eq (car codec) :struct))
         (funcall (second codec) item))
        ((eq codec :item) item)
        (t (error "Unknown RLP codec ~S" codec))))

(defun encode-field (codec value)
  (cond ((eq codec :address) (integer->octets value 20))
        ((and (consp codec) (eq (car codec) :struct)) (funcall (third codec) value))
        (t value)))

(declaim (inline decode-field-at write-address))

(defun decode-field-at (codec raw position end)
  "Decode scalar fields directly from a bounded RLP item, without a temporary vector."
  (declare (type octets raw) (type fixnum position end) #.clempo.base:*hot*)
  (multiple-value-bind (start stop list-p) (clempo.rlp:rlp-item-bounds raw position end)
    (when (eq codec :item)
      (return-from decode-field-at (clempo.rlp:rlp-decode raw :start position :end stop)))
    (invariant (not list-p) "expected scalar RLP field")
    (let ((length (- stop start)))
      (values
       (cond ((member codec '(:u64 :u256))
              (invariant (<= length (if (eq codec :u64) 8 32)) "integer field too long")
              (invariant (or (zerop length) (plusp (aref raw start))) "noncanonical integer")
              (octets->integer raw :start start :end stop))
             ((eq codec :address)
              (invariant (= length 20) "address must be 20 bytes")
              (octets->integer raw :start start :end stop))
             (t
              (when (consp codec) (invariant (= length (second codec)) "fixed byte field length"))
              (subseq raw start stop)))
       stop))))

(defun write-address (address out position)
  (declare (type (unsigned-byte 160) address) (type octets out) (type fixnum position))
  (setf (aref out position) #x94)
  (dotimes (i 20 (+ position 21))
    (setf (aref out (+ position i 1)) (ldb (byte 8 (* 8 (- 19 i))) address))))

(defmacro defrlp (name &body fields)
  "Declare a structure and its canonical RLP codec once. :TRAILING fields use NIL
for absence (zero remains present), must be a suffix, and cannot contain gaps.
The bounded decoder reads fields directly, retaining the original bytes in
NAME-RAW unless :RETAIN-RAW NIL. The encoder reconstructs into a single vector.
Generated functions: NAME-FROM-ITEMS, NAME-TO-ITEMS, DECODE-NAME, ENCODE-NAME,
NAME-ENCODED-LENGTH and NAME-ENCODE-INTO. Field schemas and :DOC strings remain queryable."
  (flet ((sym (format-string &rest args)
           (intern (apply #'format nil format-string args) (symbol-package name)))
         (nested (codec format-string)
           (let* ((from (second codec)) (text (symbol-name from))
                  (stem (subseq text 0 (- (length text) (length "-FROM-ITEMS")))))
             (intern (format nil format-string stem) (symbol-package from)))))
    (let* ((constructor (sym "MAKE-~A" name))
           (from (sym "~A-FROM-ITEMS" name)) (to (sym "~A-TO-ITEMS" name))
           (decoder (sym "DECODE-~A" name)) (encoder (sym "ENCODE-~A" name))
           (raw (sym "~A-RAW" name))
           (payload-size (sym "~A-PAYLOAD-LENGTH" name))
           (size (sym "~A-ENCODED-LENGTH" name))
           (writer (sym "~A-ENCODE-INTO" name))
           (minimum (or (position-if (lambda (f) (getf (cddr f) :trailing)) fields)
                        (length fields))))
      (assert (every (lambda (f) (getf (cddr f) :trailing)) (nthcdr minimum fields)))
      `(progn
         (defstruct ,name
           ,@(loop for (slot) in fields collect `(,slot nil))
           (raw nil :type (or null octets)))
         (setf (get ',name 'rlp-layout) ',fields)
         (defun ,from (items)
           (invariant (and (listp items) (<= ,minimum (length items) ,(length fields)))
                      "~A requires ~D..~D fields" ',name ,minimum ,(length fields))
           (,constructor
            ,@(loop for (slot codec . options) in fields for index from 0
                    append (list (intern (symbol-name slot) :keyword)
                                 (if (getf options :trailing)
                                     `(when (nthcdr ,index items) (decode-field ',codec (nth ,index items)))
                                     `(decode-field ',codec (nth ,index items)))))))
         (defun ,to (object)
           (let ((items nil) (absent nil))
             (declare (ignorable absent))
             ,@(loop for (slot codec . options) in fields
                     for access = (sym "~A-~A" name slot)
                     collect (if (getf options :trailing)
                                 `(let ((value (,access object)))
                                    (if (null value) (setf absent t)
                                        (progn (invariant (not absent) "gap in ~A trailing fields" ',name)
                                               (push (encode-field ',codec value) items))))
                                 `(push (encode-field ',codec (,access object)) items)))
             (nreverse items)))
         (defun ,decoder (octets &key (start 0) (end (length octets)) (retain-raw t) allow-trailing)
           (declare (type octets octets) (type fixnum start end))
           (multiple-value-bind (cursor stop list-p) (clempo.rlp:rlp-item-bounds octets start end)
             (invariant (and list-p (or allow-trailing (= stop end))) "expected one ~A list" ',name)
             (let ((object
                     (,constructor
                      ,@(loop for (slot codec . options) in fields
                              for read-form =
                              `(multiple-value-bind (value next)
                                   ,(if (and (consp codec) (eq (car codec) :struct))
                                        `(,(nested codec "DECODE-~A") octets :start cursor :end stop
                                          :retain-raw nil :allow-trailing t)
                                        `(decode-field-at ',codec octets cursor stop))
                                 (setf cursor next) value)
                              append (list (intern (symbol-name slot) :keyword)
                                           (if (getf options :trailing)
                                               `(when (< cursor stop) ,read-form) read-form))))))
               (invariant (= cursor stop) "extra fields in ~A" ',name)
               (when retain-raw
                 (setf (,raw object) (if (and (zerop start) (= stop (length octets)))
                                        octets (subseq octets start stop))))
               (values object stop))))
         (defun ,payload-size (object)
           (let ((size 0) (absent nil))
             (declare (type fixnum size) (ignorable absent))
             ,@(loop for (slot codec . options) in fields
                     for access = `(,(sym "~A-~A" name slot) object)
                     for length-form = (cond ((eq codec :address) 21)
                                             ((and (consp codec) (eq (car codec) :struct))
                                              `(,(nested codec "~A-ENCODED-LENGTH") ,access))
                                             (t `(clempo.rlp:rlp-encoded-length ,access)))
                     collect (if (getf options :trailing)
                                 `(if (null ,access) (setf absent t)
                                      (progn (invariant (not absent) "gap in ~A trailing fields" ',name)
                                             (incf size ,length-form)))
                                 `(incf size ,length-form)))
             size))
         (defun ,size (object)
           (let ((payload (,payload-size object)))
             (+ payload (clempo.rlp:header-length payload))))
         (defun ,writer (object out position)
           (declare (type octets out) (type fixnum position))
           (setf position (clempo.rlp:write-header out position (,payload-size object) #xc0))
           ,@(loop for (slot codec . options) in fields
                   for access = `(,(sym "~A-~A" name slot) object)
                   for write-form =
                   `(setf position
                          ,(cond ((eq codec :address) `(write-address ,access out position))
                                 ((and (consp codec) (eq (car codec) :struct))
                                  `(,(nested codec "~A-ENCODE-INTO") ,access out position))
                                 (t `(clempo.rlp:rlp-encode-into ,access out position))))
                   collect (if (getf options :trailing) `(when ,access ,write-form) write-form))
           position)
         (defun ,encoder (object)
           (let ((out (make-octets (,size object))))
             (,writer object out 0)
             out))
         ',name))))
