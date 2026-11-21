(defpackage #:clempo.p256
  (:use #:cl)
  (:export #:p256-verify #:p256-verify-low-s #:+p256-n+ #:+p256-p+))
(in-package #:clempo.p256)

(defconstant +p256-p+ #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun p256-reduction-rows ()
    ;; B=2^32, and B^8 = B^7 - B^6 - B^3 + 1 modulo p.
    ;; Construct the signed coefficients of B^0 through B^15 in that basis.
    (let ((rows (make-array 16)))
      (dotimes (i 16 rows)
        (setf (aref rows i)
              (if (< i 8)
                  (loop for j below 8 collect (if (= i j) 1 0))
                  (mapcar (lambda (a b c d) (- (+ a b) c d))
                          (aref rows (- i 8)) (aref rows (- i 1))
                          (aref rows (- i 2)) (aref rows (- i 5)))))))))

(defmacro p256-fold-words (&rest words)
  (let ((rows (p256-reduction-rows)) (digits (loop repeat 8 collect (gensym "DIGIT")))
        (out (loop repeat 8 collect (gensym "OUT")))
        (hi (loop repeat 8 collect (gensym "HIGH"))))
    `(let* (,@(loop for word in words for i from 0 append
                    `((,(nth (* 2 i) (append digits hi)) (ldb (byte 32 0) ,word))
                      (,(nth (1+ (* 2 i)) (append digits hi)) (ash ,word -32))))
            ,@(loop for r in out for i below 8 collect
                    `(,r (+ ,(nth i digits)
                            ,@(loop for j from 8 below 16
                                    for coeff = (nth i (aref rows j))
                                    unless (zerop coeff) collect `(* ,coeff ,(nth (- j 8) hi)))))))
       (declare (type (signed-byte 40) ,@out))
       (let ((carry 0))
         (declare (type (signed-byte 8) carry))
         (loop
           ,@(loop for d in out collect
                   `(let ((n (+ ,d carry)))
                      (setf ,d (ldb (byte 32 0) n) carry (ash n -32))))
           (when (zerop carry) (return))
           ;; The first carry is small; the next has magnitude at most one.
           ;; If another wrap occurs, the following fold cannot wrap again:
           ;; its only adjustment is +/- (B^7 - B^6 - B^3 + 1).
           (incf ,(nth 0 out) carry)
           (decf ,(nth 3 out) carry)
           (decf ,(nth 6 out) carry)
           (incf ,(nth 7 out) carry)
           (setf carry 0)))
       (let ((r (clempo.secp256k1::pack-field
                 ,@(loop for (a b) on out by #'cddr collect `(logior ,a (ash ,b 32))))))
         (if (< r +p256-p+) r (- r +p256-p+))))))

(defmacro define-p256-product ()
  (let ((as (loop repeat 4 collect (gensym "A")))
        (bs (loop repeat 4 collect (gensym "B")))
        (rs (loop repeat 8 collect (gensym "R")))
        (carry (gensym "CARRY")))
    `(defun p256-field-multiply (x y)
       (declare #.clempo.base:*hot* (type (integer 0 ,(1- +p256-p+)) x y))
       (let (,@(loop for a in as for i from 0 collect `(,a (clempo.secp256k1::integer-limb x ,i)))
             ,@(loop for b in bs for i from 0 collect `(,b (clempo.secp256k1::integer-limb y ,i)))
             ,@(loop for r in rs collect `(,r 0)) (,carry 0))
         (declare (type (unsigned-byte 64) ,@as ,@bs ,@rs ,carry))
         ,@(loop for a in as for i from 0 append
                 `((setf ,carry 0)
                   ,@(loop for b in bs for j from 0 for r = (nth (+ i j) rs) collect
                           `(setf (values ,carry ,r) (clempo.secp256k1::mul-add-word ,a ,b ,r ,carry)))
                   (setf ,(nth (+ i 4) rs) ,carry)))
         (p256-fold-words ,@rs)))))
(define-p256-product)
