(defpackage #:clempo.p256
  (:use #:cl)
  (:export #:p256-verify #:p256-verify-low-s #:make-p256-scratch #:*p256-scratch*
           #:+p256-n+ #:+p256-p+))
(in-package #:clempo.p256)

(defconstant +p256-p+ #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff)
(defconstant +p256-n+ #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)

