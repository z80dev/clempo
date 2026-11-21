(defpackage #:clempo.rsa.tests (:use #:cl #:clempo.base #:clempo.rsa))
(in-package #:clempo.rsa.tests)

(defun data-file (name)
  (merge-pathnames (concatenate 'string "data/" name) #.(or *compile-file-truename* *load-truename*)))

(defun run-wycheproof (file pss)
  (let ((n 0))
    (with-open-file (in (data-file file))
      (let ((*read-eval* nil))
        (loop for v = (read in nil) while v
              do (destructuring-bind (id hash salt n-hex e-hex msg sig result) v
                   (let ((ok (if pss
                                 (rsa-pss-verify (hex->integer n-hex) (hex->integer e-hex) hash
                                                 (hex->octets msg) (hex->octets sig) :salt-length salt)
                                 (rsa-pkcs1-verify (hex->integer n-hex) (hex->integer e-hex) hash
                                                   (hex->octets msg) (hex->octets sig)))))
                     (incf n)
                     (ecase result
                       (:valid (is ok "~A tc ~D should verify" file id))
                       (:invalid (is (not ok) "~A tc ~D should fail" file id))
                       (:acceptable nil)))))))
    (is (> n 100) "~A: ~D vectors" file n)))

(deftest rsa-pkcs1-wycheproof ()
  (dolist (f '("rsa_signature_2048_sha256_test.sexp" "rsa_signature_3072_sha384_test.sexp"
               "rsa_signature_4096_sha512_test.sexp"))
    (run-wycheproof f nil)))

(deftest rsa-pss-wycheproof ()
  (dolist (f '("rsa_pss_2048_sha256_mgf1_32_test.sexp" "rsa_pss_2048_sha384_mgf1_48_test.sexp"
               "rsa_pss_4096_sha512_mgf1_64_test.sexp"))
    (run-wycheproof f t)))
