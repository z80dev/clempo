;;; CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive \
;;; --load scripts/build.lisp --eval '(asdf:load-system "clempo-crypto-ec")' \
;;; --load tests/crypto/ec/bls12-381/speed.lisp
(defpackage #:clempo.bls12-381.speed
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:b #:clempo.bls12-381)))
(in-package #:clempo.bls12-381.speed)
(defun measure (name count fn)
  (funcall fn) (sb-ext:gc :full t)
  (let ((wall (get-internal-real-time)) (cpu (get-internal-run-time))
        (bytes (sb-ext:get-bytes-consed)))
    (dotimes (i count) (funcall fn))
    (let ((elapsed-wall (- (get-internal-real-time) wall))
          (elapsed-cpu (- (get-internal-run-time) cpu))
          (allocated (- (sb-ext:get-bytes-consed) bytes)))
      (format t "~&~A: ~,3F ms wall, ~,3F ms CPU, ~,1F bytes/call, ~,1F% CPU (~D calls)~%"
              name (/ (* 1000d0 elapsed-wall) count internal-time-units-per-second)
              (/ (* 1000d0 elapsed-cpu) count internal-time-units-per-second)
              (/ allocated (float count 1d0))
              (if (zerop elapsed-wall) 0d0 (* 100d0 (/ elapsed-cpu elapsed-wall))) count))))
(defparameter *g1* (b:g1-generator))
(defparameter *g2* (b:g2-generator))
(defparameter *pairs* (octets-concat (b:g1-encode *g1*) (b:g2-encode *g2*)
                                    (b:g1-encode (b:g1-neg *g1*)) (b:g2-encode *g2*)))
(defparameter *points* (loop for i from 1 to 128 collect (b:g1-mul *g1* i)))
(defparameter *scalars* (loop for i from 1 to 128 collect
                         (octets->integer (clempo.keccak:keccak256 (integer->octets i 32)))))
(defparameter *msm* (apply #'octets-concat
                          (loop for p in *points* for s in *scalars* collect
                            (octets-concat (b:g1-encode p) (integer->octets s 32)))))
(defparameter *namespace* (string->octets "tempo"))
(defparameter *message* (string->octets "benchmark"))
(defparameter *public* (b:bls-public-key :min-sig (integer->octets 11 32)))
(defparameter *signature* (b:bls-sign :min-sig (integer->octets 11 32) *message* :namespace *namespace*))
(measure "BLS pairing 2, byte API" 300 (lambda () (assert (b:pairing-check *pairs*))))
(measure "BLS G1 MSM 128, byte API" 100
         (lambda () (multiple-value-bind (result valid) (b:g1-msm-precompile *msm*)
                      (declare (ignore result)) (assert valid))))
(measure "BLS MinSig verify" 300
         (lambda () (assert (b:bls-verify :min-sig *public* *message* *signature* :namespace *namespace*))))
(measure "G1 MSM math only" 100 (lambda () (b:g1-msm *points* *scalars*)))
(measure "G1 subgroup checks, 128 points" 100 (lambda () (every #'b:g1-subgroup-p *points*)))
(defun fp-multiply-block (arena)
  (declare #.clempo.base:*hot* (type b::mont-words arena))
  (dotimes (i 1000) (b::fp-mul! arena 0 arena 0 arena 6))
  (values))
(let ((arena (make-array 12 :element-type '(unsigned-byte 64) :initial-element 0)))
  (b::fp-from-integer! arena 0 2) (b::fp-from-integer! arena 6 3)
  (measure "Fp multiplication x1000" 1000 (lambda () (fp-multiply-block arena))))
