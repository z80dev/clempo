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

(defmacro defrlp (name &body fields)
  "Declare a structure and its canonical RLP codec once. :TRAILING fields use NIL
for absence (zero remains present), must be a suffix, and cannot contain gaps.
The decoder retains the original octets in NAME-RAW; the encoder always reads
current fields. Generated functions: NAME-FROM-ITEMS, NAME-TO-ITEMS,
DECODE-NAME, ENCODE-NAME. Field schemas and :DOC strings remain queryable."
  (flet ((sym (format-string &rest args)
           (intern (apply #'format nil format-string args) (symbol-package name))))
    (let* ((constructor (sym "MAKE-~A" name))
           (from (sym "~A-FROM-ITEMS" name)) (to (sym "~A-TO-ITEMS" name))
           (decoder (sym "DECODE-~A" name)) (encoder (sym "ENCODE-~A" name))
           (raw (sym "~A-RAW" name))
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
         (defun ,decoder (octets)
           (let ((object (,from (clempo.rlp:rlp-decode octets))))
             (setf (,raw object) octets) object))
         (defun ,encoder (object) (clempo.rlp:rlp-encode (,to object)))
         ',name))))
