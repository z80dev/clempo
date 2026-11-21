(defpackage #:clempo.base.tests
  (:use #:cl #:clempo.base))

(in-package #:clempo.base.tests)

(deftest hex-round-trip ()
  (is= "0x00ff10" (octets->hex (hex->octets "0x00FF10")))
  (is= "0abc" (octets->hex (hex->octets "abc") :prefix nil))
  (is-equalp (octets-of 1 2) (hex->octets "0102")))

(deftest integer-octets ()
  (is= 258 (octets->integer (integer->octets 258 2)))
  (is-equalp (octets-of) (integer->octets-minimal 0))
  (is-equalp (octets-of 1 0) (integer->octets-minimal 256))
  (signals invariant-violation (integer->octets 256 1)))

(deftest signed-views ()
  (is= -1 (u256->s256 +u256-max+))
  (is= +u256-max+ (s256->u256 -1))
  (is= 0 (wrap256 +u256-modulus+)))

(deftest octets=-compares-every-byte ()
  ;; word-at-a-time comparison: every length around a word boundary, every differing position
  (dotimes (n 20)
    (let ((a (make-octets n)))
      (dotimes (i n) (setf (aref a i) (mod (* 37 (1+ i)) 256)))
      (is (octets= a (copy-seq a)))
      (is (not (octets= a (subseq (octets-concat a (octets-of 0)) 0 (1+ n)))))
      (dotimes (i n)
        (let ((b (copy-seq a)))
          (setf (aref b i) (logxor (aref b i) 1))
          (is (not (octets= a b)) "length ~D, differing at ~D" n i))))))
