(defpackage #:clempo.bls12-381.hash.tests
  (:use #:cl #:clempo.base #:clempo.bls12-381))
(in-package #:clempo.bls12-381.hash.tests)

(defparameter *hash-fixtures*
  (with-open-file (stream #.(merge-pathnames "hash-vectors.sexp"
                                            (or *compile-file-truename* *load-truename*)))
    (let ((*read-eval* nil)) (read stream))))

(defun fixture-integers (hex)
  (let* ((start (if (and (>= (length hex) 2) (string-equal hex "0x" :end1 2)) 2 0)))
    (loop for i from start below (length hex) by 128
          collect (parse-integer hex :start i :end (+ i 128) :radix 16))))

(deftest bls-hash-rfc9380-and-eip2537-vectors ()
  (dolist (vector *hash-fixtures*)
    (destructuring-bind (kind group &rest fields) vector
      (ecase kind
        (:hash
         (destructuring-bind (dst message expected) fields
           (is-equalp expected
                      (funcall (if (= group 1) #'hash-to-g1 #'hash-to-g2)
                               (string->octets message) :dst (string->octets dst)))))
        (:map
         (destructuring-bind (input output) fields
           (let* ((u (fixture-integers input))
                  (coordinates (fixture-integers output))
                  (expected (if (= group 1)
                                (cons (first coordinates) (second coordinates))
                                (cons (cons (first coordinates) (second coordinates))
                                      (cons (third coordinates) (fourth coordinates))))))
             (is-equalp expected
                        (if (= group 1) (map-fp-to-g1 (first u))
                            (map-fp2-to-g2 (cons (first u) (second u))))))))))))

(deftest bls-hash-subgroup-and-exceptional-inputs ()
  (dolist (u (list 0 1 (1- +p+)))
    (is (null (g1-mul (map-fp-to-g1 u) +r+)))
    (dolist (v (list 0 1 (1- +p+)))
      (is (null (g2-mul (map-fp2-to-g2 (cons u v)) +r+))))))

(deftest bls-map-precompile-encoding-and-rejection ()
  (dolist (record *hash-fixtures*)
    (when (eq (first record) :map)
      (destructuring-bind (kind group input expected) record
        (declare (ignore kind))
        (multiple-value-bind (output valid)
            (funcall (if (= group 1) #'map-g1-precompile #'map-g2-precompile)
                     (hex->octets input))
          (is valid)
          (is-equalp (hex->octets expected) output)))))
  (dolist (spec (list (cons #'map-g1-precompile 64) (cons #'map-g2-precompile 128)))
    (dolist (length (list 0 (1- (cdr spec)) (1+ (cdr spec))))
      (multiple-value-bind (output valid) (funcall (car spec) (make-octets length))
        (is (null output))
        (is (null valid))))
    (dolist (input (list (integer->octets +p+ (cdr spec))
                        (let ((bytes (make-octets (cdr spec))))
                          (setf (aref bytes 0) 1) bytes)))
      (multiple-value-bind (output valid) (funcall (car spec) input)
        (is (null output))
        (is (null valid))))))
