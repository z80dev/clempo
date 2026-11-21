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

(deftest keccak-fixed-size-overlapping-destination ()
  (dolist (size '(32 64))
    (let* ((data (make-octets 80)) (end (+ 3 size)))
      (dotimes (i (length data)) (setf (aref data i) (mod (* 19 i) 253)))
      (let ((k (make-keccak)))
        (keccak-update k data :start 3 :end end)
        (let ((expected (keccak-final k)))
          (is (eq data (keccak256-into data 3 end data 7)))
          (is-equalp expected (subseq data 7 39))
          (is= (mod (* 6 19) 253) (aref data 6))
          (is= (mod (* 39 19) 253) (aref data 39)))))))

(defmacro with-portable-keccak (&body body)
  "Run BODY with the arm64 instructions switched off, so it exercises the portable code."
  #+arm64 `(let ((saved clempo.keccak-arm64:**native**))
             (unwind-protect (progn (setf clempo.keccak-arm64:**native** nil) ,@body)
               (setf clempo.keccak-arm64:**native** saved)))
  #-arm64 `(progn ,@body))

(deftest keccak-portable-code-matches-vectors ()
  ;; Every host compiles and runs the portable permutation and fixed-size paths, whether or
  ;; not it uses the Arm instructions.
  (with-portable-keccak
    (is= "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
         (octets->hex (keccak256 (make-octets 0))))
    (is= "0x4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
         (octets->hex (keccak256 (string->octets "abc"))))
    ;; Storage slot hashes: keccak256 of uint256(0), and of (uint256(0), uint256(0)).
    (is= "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563"
         (octets->hex (keccak256 (make-octets 32))))
    (is= "0xad3228b676f7d3cd4284a5443f17f1962b36e491b30a40b2405849e597ba5fb5"
         (octets->hex (keccak256 (make-octets 64))))
    (let ((data (make-octets 300)))
      (dotimes (i 300) (setf (aref data i) (mod (* 31 i) 251)))
      (dolist (n '(32 64))
        (let ((k (make-keccak)))
          (keccak-update k data :start 5 :end (+ 5 n))
          (is-equalp (keccak-final k)
                     (clempo.keccak::fixed-into-portable data 5 n (make-octets 32) 0)))))))

(deftest keccak-native-matches-portable ()
  (let ((state (sb-ext:seed-random-state 99))
        (a (make-array 25 :element-type '(unsigned-byte 64))))
    (dotimes (i 25) (setf (aref a i) (random (ash 1 64) state)))
    (let ((b (copy-seq a)))
      (dotimes (i 64)
        (clempo.keccak::keccak-f-portable a)
        (clempo.keccak::keccak-f b)
        (is-equalp a b)))
    (dotimes (i 500)
      (let* ((n (random 400 state)) (data (make-octets n)))
        (dotimes (j n) (setf (aref data j) (random 256 state)))
        (is-equalp (with-portable-keccak (keccak256 data)) (keccak256 data) "length ~D" n)))))

#+arm64
(deftest keccak-native-under-concurrent-gc ()
  ;; The VOPs hold interior pointers to young vectors in raw registers; they run
  ;; pseudo-atomic, so collections forced from another thread cannot move the vectors
  ;; beneath them.
  (when clempo.keccak-arm64:**native**
    (let* ((stop nil) (mismatches 0) (lock (sb-thread:make-mutex))
           (workers
             (loop for w below 2 collect
               (let ((w w))
                 (sb-thread:make-thread
                  (lambda ()
                    (let ((state (sb-ext:seed-random-state w)))
                      (loop until stop
                            do (let* ((n (if (zerop (random 2 state)) 32 64))
                                      (data (make-octets (+ n 7))) (out (make-octets 40)))
                                 (dotimes (j (length data)) (setf (aref data j) (random 256 state)))
                                 (keccak256-into data 7 (+ 7 n) out 3)
                                 (unless (octets= (subseq out 3 35)
                                                  (clempo.keccak::fixed-into-portable
                                                   data 7 n (make-octets 32) 0))
                                   (sb-thread:with-mutex (lock) (incf mismatches))))))))))))
      (unwind-protect (dotimes (i 40) (sb-ext:gc))
        (setf stop t)
        (mapc #'sb-thread:join-thread workers))
      (is= 0 mismatches))))

(deftest keccak-reusable-destination ()
  (let ((k (make-keccak)) (data (make-octets 300)) (out (make-octets 40)))
    (dotimes (i (length data)) (setf (aref data i) (mod (* i 13) 253)))
    (dolist (n '(0 32 64 136 137 270 31))
      (fill out #xaa)
      (let ((expected (keccak256 data :start 3 :end (+ 3 n))))
        (is (eq out (keccak256-into data 3 (+ 3 n) out 4 k)))
        (is-equalp expected (subseq out 4 36))
        (is= #xaa (aref out 3))
        (is= #xaa (aref out 36))))
    (is (eq k (keccak-reset k)))
    (keccak-update k data)
    (is-equalp (keccak256 data) (keccak-final k))))

(deftest keccak-digest-is-non-destructive ()
  (let ((k (make-keccak)) (scratch (make-keccak)) (a (string->octets "running")) (b (string->octets " mac")))
    (keccak-update k a)
    (is-equalp (keccak256 a) (keccak-digest k scratch))
    (keccak-update k b)
    (is-equalp (keccak256 (octets-concat a b)) (keccak-digest k scratch))))
