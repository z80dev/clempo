(defpackage #:clempo.keccak.tests
  (:use #:cl #:clempo.base #:clempo.keccak))

(in-package #:clempo.keccak.tests)

(deftest keccak-vectors ()
  (is= "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
       (octets->hex (keccak256 (make-octets 0))))
  (is= "0x4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
       (octets->hex (keccak256 (string->octets "abc"))))
  ;; Transfer(address,address,uint256) topic
  (is= "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
       (octets->hex (keccak256 (string->octets "Transfer(address,address,uint256)")))))

(deftest keccak-incremental-matches-one-shot ()
  ;; Lengths straddle the 136-byte rate.
  (dolist (n '(0 1 135 136 137 271 272 273 1000))
    (let ((data (make-octets n)))
      (dotimes (i n) (setf (aref data i) (mod (* i 7) 251)))
      (let ((k (make-keccak)))
        (loop for pos from 0 below n by 50
              do (keccak-update k data :start pos :end (min n (+ pos 50))))
        (is-equalp (keccak256 data) (keccak-final k) "length ~D" n)))))

(deftest keccak-fixed-size-slices ()
  (let ((data (make-octets 89)))
    (dotimes (i (length data)) (setf (aref data i) (mod (* i 19) 256)))
    (dolist (size '(32 64))
      (dolist (start '(0 3 16))
        (let ((k (make-keccak)) (end (+ start size)))
          (keccak-update k data :start start :end end)
          (is-equalp (keccak-final k) (keccak256 data :start start :end end)))))))

#+(and arm64 darwin)
(deftest keccak-arm-permutation-matches-portable ()
  (let ((a (make-array 25 :element-type '(unsigned-byte 64))))
    (dotimes (i 25)
      (setf (aref a i) (ldb (byte 64 0) (* #x9e3779b97f4a7c15 (1+ i)))))
    (let ((b (copy-seq a)))
      (dotimes (i 16)
        (clempo.keccak::keccak-f-portable a)
        (clempo.keccak::keccak-f b)
        (is-equalp a b)))))

(deftest keccak-digest-is-non-destructive ()
  (let ((k (make-keccak)) (scratch (make-keccak)) (a (string->octets "running")) (b (string->octets " mac")))
    (keccak-update k a)
    (is-equalp (keccak256 a) (keccak-digest k scratch))
    (keccak-update k b)
    (is-equalp (keccak256 (octets-concat a b)) (keccak-digest k scratch))))
