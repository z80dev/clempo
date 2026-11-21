(defpackage #:clempo.gcm.tests (:use #:cl #:clempo.base #:clempo.gcm))
(in-package #:clempo.gcm.tests)

(defun data-file (name)
  (merge-pathnames (concatenate 'string "data/" name) #.(or *compile-file-truename* *load-truename*)))

(defun read-forms (name)
  (with-open-file (in (data-file name))
    (let ((*read-eval* nil)) (loop for f = (read in nil) while f collect f))))

(deftest gcm-nist-cavp ()
  (let ((n 0))
    (dolist (v (read-forms "gcm-nist.sexp"))
      (destructuring-bind (key iv pt aad ct tag authentic) v
        (let ((g (make-gcm (hex->octets key))) (iv (hex->octets iv)) (aad (hex->octets aad))
              (sealed (octets-concat (hex->octets ct) (hex->octets tag))))
          (incf n)
          (if authentic
              (progn (is-equalp sealed (gcm-encrypt g iv (hex->octets pt) :aad aad) "~A" key)
                     (is-equalp (hex->octets pt) (gcm-decrypt g iv sealed :aad aad) "~A" key))
              (is (null (gcm-decrypt g iv sealed :aad aad)) "forged ~A" key)))))
    (is (> n 250))))

(deftest gcm-rejects-flipped-bits ()
  (let* ((g (make-gcm (make-octets 32))) (iv (make-octets 12))
         (sealed (gcm-encrypt g iv (string->octets "hello, tempo") :aad (octets-of 1 2 3))))
    (dotimes (i (length sealed))
      (let ((bad (copy-seq sealed)))
        (setf (aref bad i) (logxor (aref bad i) 1))
        (is (null (gcm-decrypt g iv bad :aad (octets-of 1 2 3))))))
    (is (null (gcm-decrypt g iv sealed :aad (octets-of 1 2 4))))))
