(defpackage #:clempo.p256.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.p256.tests)

(deftest p256-field-products ()
  (let* ((p clempo.p256:+p256-p+)
         (boundaries (append (list 0 1 2 (1- p) (- p 2) (ash p -1))
                             (loop for bit from 32 to 224 by 32
                                   append (list (1- (ash 1 bit)) (ash 1 bit) (1+ (ash 1 bit)))))))
    (flet ((check (a b)
             (is= (mod (* a b) p) (clempo.p256::p256-field-multiply a b))))
      (dolist (a boundaries)
        (dolist (b boundaries) (check a b)))
      (let ((state 1))
        (flet ((next ()
                 (setf state (mod (+ (* state #x5851f42d4c957f2d) #x14057b7ef767814f) p))))
          (dotimes (i 4096) (check (next) (next))))))))
