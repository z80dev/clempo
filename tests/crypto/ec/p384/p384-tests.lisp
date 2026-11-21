(defpackage #:clempo.p384.tests
  (:use #:cl #:clempo.base #:clempo.p384))
(in-package #:clempo.p384.tests)

(defun fixture-data (name)
  (with-open-file (in (asdf:system-relative-pathname
                      "clempo-crypto-ec" (concatenate 'string "../../../tests/crypto/ec/p384/" name)))
    (let ((*read-eval* nil)) (read in))))

(deftest p384-wycheproof ()
  (let ((rows (fixture-data "wycheproof.sexp"))
        (boundaries (fixture-data "wire-boundaries.sexp")))
    (is= 280 (length rows))
    (is= 19 (length boundaries))
    (dolist (row rows)
      (destructuring-bind (id hash r s x y expected) row
        (let* ((boundary (assoc id boundaries))
               (equivalent (third boundary))
               (actual (p384-verify (hex->octets hash) r s x y)))
          ;; Ten short encodings contain the same valid integers as a separately
          ;; valid 96-byte vector. Integers alone cannot retain that wire error.
          (when equivalent
            (let ((valid-row (assoc equivalent rows)))
              (is (seventh valid-row))
              (is (equal (subseq row 1 6) (subseq valid-row 1 6)))))
          (is= (not (null (or expected equivalent))) actual "math tcId ~D" id)
          ;; Composing the mandatory 96-byte caller check retains every original
          ;; Wycheproof wire-level rejection, with no removed/rewritten results.
          (is= expected (and (null boundary) actual) "P1363 tcId ~D" id)
          (when actual
            (is (p384-verify (hex->octets hash) r (- +p384-n+ s) x y)
                "high-/low-s twin tcId ~D" id)))))))

(deftest p384-rfc6979-nist-known-answers ()
  ;; RFC6979 A.2.6, NIST P-384, SHA-384, messages "sample" and "test".
  (let ((x #xec3a4e415b4e19a4568618029f427fa5da9a8bc4ae92e02e06aae5286b300c64def8f0ea9055866064a254515480bc13)
        (y #x8015d9b72d7d57244ea8ef9ac0c621896708a59367f9dfb9f54ca84b3f1c9db1288b231c3ae0d4fe7344fd2533264720))
    (dolist (row '(("9a9083505bc92276aec4be312696ef7bf3bf603f4bbd381196a029f340585312313bca4a9b5b890efee42c77b1ee25fe"
                    #x94edbb92a5ecb8aad4736e56c691916b3f88140666ce9fa73d64c4ea95ad133c81a648152e44acf96e36dd1e80fabe46
                    #x99ef4aeb15f178cea1fe40db2603138f130e740a19624526203b6351d0a3a94fa329c145786e679e7b82c71a38628ac8)
                   ("768412320f7b0aa5812fce428dc4706b3cae50e02a64caa16a782249bfe8efc4b7ef1ccb126255d196047dfedf17a0a9"
                    #x8203b63d3c853e8d77227fb377bcf7b7b772e97892a80f36ab775d509d7a5feb0542a7f0812998da8f1dd3ca3cf023db
                    #xddd0760448d42d8a43af45af836fce4de8be06b485e9b61b827c2f13173923e06a739f040649a667bf3b828246baa5a5)))
      (destructuring-bind (hex r s) row
        (let ((hash (hex->octets hex)))
          (is (p384-verify hash r s x y))
          (is (p384-verify hash r (- +p384-n+ s) x y))
          (is (not (p384-verify hash (1+ r) s x y)))
          (is (not (p384-verify hash r (1+ s) x y)))
          (let ((modified (copy-seq hash)))
            (setf (aref modified 0) (logxor 1 (aref modified 0)))
            (is (not (p384-verify modified r s x y)))))))))

(deftest p384-invalid-boundaries ()
  (let ((hash (make-octets 48))
        (x #xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7)
        (y #x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f))
    (dolist (bad (list -1 0 +p384-n+ (1+ +p384-n+) (ash 1 512)))
      (is (not (p384-verify hash bad 1 x y)))
      (is (not (p384-verify hash 1 bad x y))))
    (dolist (bad (list -1 +p384-p+ (1+ +p384-p+) (ash 1 512)))
      (is (not (p384-verify hash 1 1 bad y)))
      (is (not (p384-verify hash 1 1 x bad))))
    (is (not (p384-verify hash 1 1 0 0)))
    (is (not (p384-verify hash 1 1 x (1+ y))))
    (is (not (p384-verify (make-octets 47) 1 1 x y)))
    (is (not (p384-verify (make-octets 49) 1 1 x y)))))

(deftest p384-public-key-validation ()
  (let ((x #xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7)
        (y #x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f))
    (is (p384-valid-public-key-p x y))
    (is (p384-valid-public-key-p x (- +p384-p+ y)))
    (is (not (p384-valid-public-key-p 0 0)))
    (is (not (p384-valid-public-key-p x (1+ y))))
    (dolist (bad (list -1 +p384-p+ (1+ +p384-p+) (ash 1 512) nil 1.5d0))
      (is (not (p384-valid-public-key-p bad y)))
      (is (not (p384-valid-public-key-p x bad))))))
