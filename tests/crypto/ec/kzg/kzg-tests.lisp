(defpackage #:clempo.kzg.tests
  (:use #:cl #:clempo.base #:clempo.kzg))

(in-package #:clempo.kzg.tests)

(defparameter *here*
  #.(make-pathname :name nil :type nil
                   :defaults (or *compile-file-truename* *load-truename*)))

(defun fixture-lines (name)
  (with-open-file (s (merge-pathnames name *here*))
    (loop for line = (read-line s nil) while line
          when (plusp (length (string-trim " " line))) collect line)))

(defun words (line)
  (loop with start = 0 for pos = (position #\Space line :start start)
        collect (subseq line start pos) while pos do (setf start (1+ pos))))

(defparameter *expected-output*
  (hex->octets (concatenate 'string
                            "0000000000000000000000000000000000000000000000000000000000001000"
                            "73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001")))

(defparameter *valid-input*
  (hex->octets "01e798154708fe7789429634053cbf9f99b619f9f084048927333fce637f549b564c0a11a0f704f4fc3e8acfe0f8245f0ad1347b378fbf96e206da11a5d3630624d25032e67a7e6a4910df5834b8fe70e6bcfeeac0352434196bdf4b2485d5a18f59a8d2a1a625a17f3fea0fe5eb8c896db3764f3185481bc22f91b4aaffcca25f26936857bc3a7c2539ea8ec3a952b7873033e038326e87ed3e1276fd140253fa08e9fc25fb2d9a98527fc22a2c9612fbeafdad446cbc7bcdbdcd780af2c16a"))

(deftest kzg-constants ()
  (is-equalp *expected-output* (point-evaluation-return-value))
  (is= 96 (length (hex->octets +tau-g2-hex+)))
  (is (tau-g2)))

(deftest kzg-geth-point-evaluation ()
  ;; pointEvaluation.json: the single geth vector; Input repeated here verbatim.
  (let ((json (format nil "~{~A~}" (fixture-lines "pointEvaluation.json"))))
    (is (search (octets->hex *valid-input* :prefix nil) json)))
  (is-equalp *expected-output* (point-evaluation *valid-input*))
  (is= 50000 +point-evaluation-gas+))

(deftest kzg-versioned-hash ()
  (is-equalp (subseq *valid-input* 0 32)
             (kzg-to-versioned-hash (subseq *valid-input* 96 144))))

(defun mutate (input index xor)
  (let ((c (copy-seq input)))
    (setf (aref c index) (logxor (aref c index) xor))
    c))

(deftest kzg-invalid-cases ()
  (let ((in *valid-input*))
    ;; lengths
    (is (null (point-evaluation (make-octets 0))))
    (is (null (point-evaluation (subseq in 0 191))))
    (is (null (point-evaluation (octets-concat in (make-octets 1)))))
    ;; versioned hash: wrong version byte, wrong digest byte
    (is (null (point-evaluation (mutate in 0 3))))
    (is (null (point-evaluation (mutate in 31 1))))
    ;; wrong y, wrong z: well-formed but false proof
    (is (null (point-evaluation (mutate in 95 1))))
    (is (null (point-evaluation (mutate in 63 1))))
    ;; proof corrupted
    (is (null (point-evaluation (mutate in 191 1))))
    ;; non-canonical z = r and y = r signal from verify-kzg-proof
    (let ((r (integer->octets clempo.bls12-381:+r+ 32))
          (c (subseq in 96 144)) (p (subseq in 144 192)))
      (signals invalid-kzg-input (verify-kzg-proof c r (subseq in 64 96) p))
      (signals invalid-kzg-input (verify-kzg-proof c (subseq in 32 64) r p))
      (dolist (length '(0 1 31 33 64))
        (signals invalid-kzg-input (verify-kzg-proof c (make-octets length) r p))
        (signals invalid-kzg-input (verify-kzg-proof c r (make-octets length) p)))
      (is (verify-kzg-proof c (subseq in 32 64) (subseq in 64 96) p))
      ;; z = r replaced in full input also fails
      (is (null (point-evaluation (octets-concat (subseq in 0 32) r (subseq in 64))))))
    ;; compressed-point flag bits: clear compression flag on commitment/proof
    (signals invalid-kzg-input
             (verify-kzg-proof (mutate (subseq in 96 144) 0 #x80)
                               (subseq in 32 64) (subseq in 64 96) (subseq in 144 192)))
    (signals invalid-kzg-input
             (verify-kzg-proof (subseq in 96 144) (subseq in 32 64) (subseq in 64 96)
                               (mutate (subseq in 144 192) 0 #x80)))))

(deftest kzg-eest-point-evaluation ()
  ;; 125 execution-spec-tests vectors: success flag, returndatasize, input.
  (let ((n 0))
    (dolist (line (fixture-lines "eest_point_evaluation.txt"))
      (destructuring-bind (ok size hex) (words line)
        (let ((out (point-evaluation (hex->octets hex))))
          (incf n)
          (if (string= ok "1")
              (progn (is= "64" size "line ~A" n)
                     (is-equalp *expected-output* out "vector ~A ~A" n hex))
              (is (null out) "vector ~A should fail: ~A" n hex)))))
    (is= 125 n)))
