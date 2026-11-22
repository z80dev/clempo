(defpackage #:clempo.ripemd160
  (:use #:cl #:clempo.base)
  (:export #:ripemd160))
(in-package #:clempo.ripemd160)

(defparameter *iv* '(#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0))

(defbound +state-words+ 5
  "Words of the 160-bit chaining value, in each of the two parallel lines."
  (= (* 32 +state-words+) 160))

(defbound +block-words+ 16
  "Message words of a 64-octet block."
  (= (* 4 +block-words+) 64))

(defbound +steps+ 80
  "Steps per line: five rounds of +BLOCK-WORDS+ steps, each round reading every word once."
  (= +steps+ (* 5 +block-words+)))

(defmacro define-compressor ()
  (let ((left (loop repeat +state-words+ collect (gensym)))
        (right (loop repeat +state-words+ collect (gensym)))
        (w (loop repeat +block-words+ collect (gensym)))
        (il '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
              7 4 13 1 10 6 15 3 12 0 9 5 2 14 11 8
              3 10 14 4 9 15 8 1 2 7 0 6 13 11 5 12
              1 9 11 10 0 8 12 4 13 3 7 15 14 5 6 2
              4 0 5 9 7 12 2 10 14 1 3 8 11 6 15 13))
        (ir '(5 14 7 0 9 2 11 4 13 6 15 8 1 10 3 12
              6 11 3 7 0 13 5 10 14 15 8 12 4 9 1 2
              15 5 1 3 7 14 6 9 11 8 12 2 10 0 4 13
              8 6 4 1 3 11 15 0 5 12 2 13 9 7 10 14
              12 15 10 4 1 5 8 7 6 2 13 14 0 3 9 11))
        (sl '(11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8
              7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12
              11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5
              11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12
              9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6))
        (sr '(8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6
              9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11
              9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5
              15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8
              8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11)))
    (labels ((rol (x n) `(logior (ldb (byte 32 0) (ash ,x ,n)) (ash ,x ,(- n 32))))
             (step-form (i regs idx shift ks reversed)
               (destructuring-bind (a b c d e) (append (last regs (mod i 5)) (butlast regs (mod i 5)))
                 (let ((round (floor i 16)))
                   `(let ((x (ldb (byte 32 0)
                                  (+ ,a ,(nth idx w) ,(nth round ks)
                                     ,(case (if reversed (- 4 round) round)
                                        (0 `(logxor ,b ,c ,d))
                                        (1 `(logxor ,d (logand ,b (logxor ,c ,d))))
                                        (2 `(logxor (logior ,b (logxor #xffffffff ,c)) ,d))
                                        (3 `(logxor ,c (logand ,d (logxor ,b ,c))))
                                        (4 `(logxor ,b (logior ,c (logxor #xffffffff ,d)))))))))
                      ;; The next iteration renames (a b c d e) to (e a b c d).
                      (setf ,a (ldb (byte 32 0) (+ ,e ,(rol 'x shift))) ,c ,(rol c 10)))))))
      `(defun compress (h data offset)
         (declare #.clempo.base:*hot* (type (simple-array (unsigned-byte 32) (5)) h)
                  (type octets data) (type fixnum offset))
         (let (,@(loop for a in left for b in right for i below +state-words+ append
                       `((,a (aref h ,i)) (,b (aref h ,i))))
               ,@(loop for x in w for i below +block-words+ collect
                       `(,x (clempo.blake2f::load-le data (+ offset ,(* i 4)) 32))))
           (declare (type (unsigned-byte 32) ,@left ,@right ,@w))
           ,@(loop for i below +steps+ for l in il for r in ir for ls in sl for rs in sr append
                   (list (step-form i left l ls '(0 #x5a827999 #x6ed9eba1 #x8f1bbcdc #xa953fd4e) nil)
                         (step-form i right r rs '(#x50a28be6 #x5c4dd124 #x6d703ef3 #x7a6d76e9 0) t)))
           (let ((first (aref h 0)))
             ,@(loop for i below +state-words+ collect
                     `(setf (aref h ,i)
                            (ldb (byte 32 0) (+ ,(if (= i 4) 'first `(aref h ,(1+ i)))
                                                 ,(nth (mod (+ i 2) 5) left)
                                                 ,(nth (mod (+ i 3) 5) right)))))))
         (values)))))
(define-compressor)

(defun ripemd160 (data)
  (check-type data octets)
  (let* ((n (length data)) (tail (mod n 64)) (end (- n tail))
         (buf (make-octets 64)) (out (make-octets 20))
         (h (make-array +state-words+ :element-type '(unsigned-byte 32) :initial-contents *iv*)))
    (loop for start from 0 below end by 64 do (compress h data start))
    (replace buf data :start2 end)
    (setf (aref buf tail) #x80)
    (when (>= tail 56) (compress h buf 0) (fill buf 0))
    (dotimes (i 8) (setf (aref buf (+ 56 i)) (ldb (byte 8 (* i 8)) (* n 8))))
    (compress h buf 0)
    (dotimes (i 20 out)
      (setf (aref out i) (ldb (byte 8 (* 8 (mod i 4))) (aref h (floor i 4)))))))
