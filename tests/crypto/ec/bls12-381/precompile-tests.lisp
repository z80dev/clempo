(defpackage #:clempo.bls12-381.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.bls12-381.tests)

(defparameter *fixture-directory*
  #.(make-pathname :name nil :type nil :defaults
                  (or *compile-file-truename* *load-truename*)))

(defmacro define-fixture-test (name file function)
  `(deftest ,name ()
     (with-open-file (in (merge-pathnames ,file *fixture-directory*))
       (let ((*read-eval* nil))
         (dolist (row (read in))
           (destructuring-bind (label input expected valid) row
             (multiple-value-bind (actual actual-valid)
                 (,function (hex->octets input))
               (is (eql valid actual-valid) "~A: validity" label)
               (when valid
                 (is (and actual (octets= (hex->octets expected) actual))
                     "~A: output" label)))))))))

(define-fixture-test g1-add-vectors "blsG1Add.sexp" clempo.bls12-381:g1-add-precompile)
(define-fixture-test g2-add-vectors "blsG2Add.sexp" clempo.bls12-381:g2-add-precompile)
(define-fixture-test g1-mul-vectors "blsG1Mul.sexp" clempo.bls12-381:g1-msm-precompile)
(define-fixture-test g1-msm-vectors "blsG1MultiExp.sexp" clempo.bls12-381:g1-msm-precompile)
(define-fixture-test g2-msm-vectors "blsG2MultiExp.sexp" clempo.bls12-381:g2-msm-precompile)
(define-fixture-test pairing-vectors "blsPairing.sexp" clempo.bls12-381:pairing-precompile)
(define-fixture-test map-g1-vectors "blsMapG1.sexp" clempo.bls12-381:map-g1-precompile)
(define-fixture-test map-g2-vectors "blsMapG2.sexp" clempo.bls12-381:map-g2-precompile)
(define-fixture-test g1-add-invalid "fail-blsG1Add.sexp" clempo.bls12-381:g1-add-precompile)
(define-fixture-test g2-add-invalid "fail-blsG2Add.sexp" clempo.bls12-381:g2-add-precompile)
(define-fixture-test g1-msm-invalid "fail-blsG1MultiExp.sexp" clempo.bls12-381:g1-msm-precompile)
(define-fixture-test g2-msm-invalid "fail-blsG2MultiExp.sexp" clempo.bls12-381:g2-msm-precompile)
(define-fixture-test pairing-invalid "fail-blsPairing.sexp" clempo.bls12-381:pairing-precompile)
(define-fixture-test map-g1-invalid "fail-blsMapG1.sexp" clempo.bls12-381:map-g1-precompile)
(define-fixture-test map-g2-invalid "fail-blsMapG2.sexp" clempo.bls12-381:map-g2-precompile)

(deftest group-arithmetic-and-compression ()
  (dolist (group '(1 2))
    (let* ((generator (if (= group 1) #'clempo.bls12-381:g1-generator #'clempo.bls12-381:g2-generator))
           (add (if (= group 1) #'clempo.bls12-381:g1-add #'clempo.bls12-381:g2-add))
           (neg (if (= group 1) #'clempo.bls12-381:g1-neg #'clempo.bls12-381:g2-neg))
           (mul (if (= group 1) #'clempo.bls12-381:g1-mul #'clempo.bls12-381:g2-mul))
           (msm (if (= group 1) #'clempo.bls12-381:g1-msm #'clempo.bls12-381:g2-msm))
           (encode (if (= group 1) #'clempo.bls12-381:g1-compress #'clempo.bls12-381:g2-compress))
           (decode (if (= group 1) #'clempo.bls12-381:g1-decompress #'clempo.bls12-381:g2-decompress))
           (g (funcall generator)))
      (is (equal g (funcall decode (funcall encode g))))
      (is (null (funcall decode (funcall encode nil))))
      (is (null (funcall mul g clempo.bls12-381:+r+)))
      (is (null (funcall add g (funcall neg g))))
      (is (equal (funcall mul g 2) (funcall add g g)))
      (is (equal (funcall mul g -1) (funcall neg g)))
      (is (equal (funcall mul g 19) (funcall msm (list g g) '(7 12))))
      (let ((bad (funcall encode nil)))
        (setf (aref bad 0) #xe0)
        (signals clempo.bls12-381:invalid-point (funcall decode bad))))))

(deftest eip-boundaries-and-non-subgroup-addition ()
  ;; (0,2) has order three. ADD allows it; MSM must reject, even with zero scalar.
  (let* ((torsion (cons 0 2)) (encoded (clempo.bls12-381:g1-encode torsion)))
    (is (not (clempo.bls12-381:g1-subgroup-p torsion)))
    (multiple-value-bind (out valid)
        (clempo.bls12-381:g1-add-precompile (octets-concat encoded (make-octets 128)))
      (is valid) (is (octets= encoded out)))
    (is (null (nth-value 1 (clempo.bls12-381:g1-msm-precompile
                           (octets-concat encoded (make-octets 32))))))
    (signals clempo.bls12-381:invalid-point
      (clempo.bls12-381:g1-decompress (clempo.bls12-381:g1-compress torsion))))
  (let ((bad (make-octets 256)))
    (setf (aref bad 0) 1)
    (is (null (nth-value 1 (clempo.bls12-381:g1-add-precompile bad)))))
  (is (equal '(t t) (multiple-value-list (clempo.bls12-381:pairing-check nil))))
  (is (equal '(nil nil) (multiple-value-list
                         (clempo.bls12-381:pairing-check (make-octets 0))))))

(deftest pairing-relations ()
  (let ((g1 (clempo.bls12-381:g1-generator)) (g2 (clempo.bls12-381:g2-generator)))
    (is (equal '(nil t) (multiple-value-list
                        (clempo.bls12-381:pairing-check (list (cons g1 g2))))))
    (is (equal '(t t) (multiple-value-list
                      (clempo.bls12-381:pairing-check
                       (list (cons (clempo.bls12-381:g1-mul g1 7) g2)
                             (cons (clempo.bls12-381:g1-neg g1)
                                   (clempo.bls12-381:g2-mul g2 7)))))))
    (is (equal '(nil nil) (multiple-value-list
                          (clempo.bls12-381:pairing-check (list (cons '(0 . 2) g2))))))))
