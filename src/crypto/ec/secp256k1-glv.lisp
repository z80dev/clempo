(in-package #:clempo.secp256k1)

(defun square-times (x count)
  (declare #.clempo.base:*hot*
           (type (integer 0 #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e) x)
           (type fixnum count))
  (dotimes (i count x) (setf x (secp-f* x x))))

(defun field-square-root (x)
  "x^((p+1)/4), with an addition chain of 253 squarings and 13 multiplies."
  (let* ((x2 (secp-f* (secp-f* x x) x))
         (x3 (secp-f* (secp-f* x2 x2) x))
         (x6 (secp-f* (square-times x3 3) x3))
         (x9 (secp-f* (square-times x6 3) x3))
         (x11 (secp-f* (square-times x9 2) x2))
         (x22 (secp-f* (square-times x11 11) x11))
         (x44 (secp-f* (square-times x22 22) x22))
         (x88 (secp-f* (square-times x44 44) x44))
         (x176 (secp-f* (square-times x88 88) x88))
         (x220 (secp-f* (square-times x176 44) x44))
         (x223 (secp-f* (square-times x220 3) x3)))
    ;; (((2^223-1)*2^23 + (2^22-1))*2^6 + 3)*4 = (p+1)/4.
    (square-times (secp-f* (square-times (secp-f* (square-times x223 23) x22) 6) x2) 2)))

;; GLV lattice basis (Hankerson/Menezes/Vanstone, algorithm 3.74),
;; mathematical constants also documented by RustCrypto k256 arithmetic/mul.rs.
(defconstant +glv-beta+ #x7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee)
(defconstant +glv-a1+ #x3086d221a7d46bcde86c90e49284eb15)
(defconstant +glv-minus-b1+ #xe4437ed6010e88286f547fa90abfe4c3)
(defconstant +glv-a2+ #x114ca50f7a8e2f3f657c1108d9d44cfd8)

(defun split-scalar (k)
  (declare #.clempo.base:*hot* (type integer k))
  (let* ((k (mod k +secp256k1-n+))
         (c1 (round (* k +glv-a1+) +secp256k1-n+))
         (c2 (round (* k +glv-minus-b1+) +secp256k1-n+)))
    (values (- k (* c1 +glv-a1+) (* c2 +glv-a2+))
            (- (* c1 +glv-minus-b1+) (* c2 +glv-a1+)))))

(defun endomorphism-table (table)
  (map 'simple-vector (lambda (p) (when p (cons (secp-f* +glv-beta+ (car p)) (cdr p)))) table))

(defparameter secp-endo-generator (endomorphism-table secp-generator))

(defun secp-fast-mul (k &optional (table secp-generator) (width 7))
  (multiple-value-bind (a b) (split-scalar k)
    (secp-joint a table b
                (if (eq table secp-generator) secp-endo-generator (endomorphism-table table))
                width width)))

(defun secp-fast-joint (k1 k2 rx ry)
  "k1*G + k2*R using four 128-bit GLV components and one doubling chain."
  (declare #.clempo.base:*hot* (type integer k1 k2 rx ry))
  (multiple-value-bind (a b) (split-scalar k1)
    (multiple-value-bind (c d) (split-scalar k2)
      (let* ((rt (secp-table rx ry))
             (tables (vector secp-generator secp-endo-generator rt (endomorphism-table rt)))
             (scalars (vector a b c d))
             (digits (make-array 4))
             (lengths (make-array 4 :element-type 'fixnum))
             (maxlen 0)
             (x 0) (y 1) (z 0))
        (declare (type fixnum maxlen)
                 (type (integer 0 #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e) x y z))
        (dotimes (j 4)
          (multiple-value-bind (ds len) (clempo.ec:wnaf (aref scalars j) (if (< j 2) 7 5))
            (setf (aref digits j) ds (aref lengths j) len maxlen (max maxlen len))))
        (loop for i of-type fixnum downfrom (1- maxlen) to 0 do
          (multiple-value-setq (x y z) (secp-double x y z))
          (dotimes (j 4)
            (when (< i (aref lengths j))
              (let* ((ds (the (simple-array (signed-byte 16) (*)) (aref digits j)))
                     (digit (aref ds i)))
                (unless (zerop digit)
                  (let* ((tab (the simple-vector (aref tables j)))
                         (q (aref tab (ash (abs digit) -1))))
                    (multiple-value-setq (x y z)
                      (secp-add-mixed x y z (car q)
                                      (if (minusp digit) (secp-f- 0 (cdr q)) (cdr q))))))))))
        (secp-affine x y z)))))
