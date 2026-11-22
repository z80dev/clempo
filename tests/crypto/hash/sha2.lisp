(defpackage #:clempo.sha2.tests
  (:use #:cl #:clempo.base #:clempo.sha2))
(in-package #:clempo.sha2.tests)

(deftest sha-fips-180 ()
  (is-equalp (hex->octets "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
             (sha256 (string->octets "0abc1") :start 1 :end 4))
  (let ((ctx (make-sha512)))
    (sha512-update ctx (string->octets "a"))
    (sha512-update ctx (string->octets "bc"))
    (is-equalp (hex->octets "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")
               (sha512-final ctx)))
  (let ((ctx (make-sha256)) (data (fill (make-octets 1000001) 97 :start 1)))
    (sha256-update ctx data :start 1 :end 29)
    (sha256-update ctx data :start 29)
    (is-equalp (hex->octets "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
               (sha256-final ctx)))
  (is-equalp (hex->octets "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909")
             (sha512 (string->octets "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))))

(deftest sha384-fips-180 ()
  (is-equalp (hex->octets "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7")
             (sha384 (string->octets "0abc1") :start 1 :end 4))
  (is-equalp (hex->octets "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985")
             (sha384 (fill (make-octets 1000000) 97))))

(deftest hmac-rfc-4231 ()
  (let ((key (fill (make-octets 20) #x0b)) (data (string->octets "Hi There")))
    (is-equalp (hex->octets "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
               (hmac-sha256 key data))
    (is-equalp (hex->octets "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854")
               (hmac-sha512 key data))))

(deftest hkdf-rfc-5869 ()
  (let* ((ikm (fill (make-octets 22) #x0b))
         (salt (hex->octets "000102030405060708090a0b0c"))
         (info (hex->octets "f0f1f2f3f4f5f6f7f8f9"))
         (prk (hkdf-sha256-extract salt ikm)))
    (is-equalp (hex->octets "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5") prk)
    (is-equalp (hex->octets "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865")
               (hkdf-sha256-expand prk info 42))))

(defmacro with-portable-sha256 (&body body)
  "Run BODY with the arm64 SHA-256 instructions switched off."
  #+arm64 `(let ((saved clempo.sha256-arm64:**native**))
             (unwind-protect (progn (setf clempo.sha256-arm64:**native** nil) ,@body)
               (setf clempo.sha256-arm64:**native** saved)))
  #-arm64 `(progn ,@body))

(deftest sha256-portable-code-matches-vectors ()
  ;; Every host compiles and runs the portable compression, Arm instructions or not.
  (with-portable-sha256
    (is-equalp (hex->octets "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
               (sha256 (string->octets "abc")))
    (is-equalp (hex->octets "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
               (sha256 (string->octets "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))
    (is-equalp (hex->octets "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
               (sha256 (fill (make-octets 1000000) 97)))))

(deftest sha256-native-matches-portable ()
  (let ((random (sb-ext:seed-random-state 5)))
    (dotimes (i 300)
      (let* ((data (make-octets (+ 64 9)))
             (a (make-array 8 :element-type '(unsigned-byte 32)))
             (b (make-array 8 :element-type '(unsigned-byte 32))))
        (dotimes (j (length data)) (setf (aref data j) (random 256 random)))
        (dotimes (j 8) (setf (aref a j) (random (ash 1 32) random) (aref b j) (aref a j)))
        (clempo.sha2::compress256-portable a data 9)
        (clempo.sha2::compress256 b data 9)
        (is-equalp a b)))
    (dotimes (i 200)
      (let* ((n (random 300 random)) (data (make-octets n)))
        (dotimes (j n) (setf (aref data j) (random 256 random)))
        (is-equalp (with-portable-sha256 (sha256 data)) (sha256 data) "length ~D" n)))))

#+arm64
(deftest sha256-many-buffers-under-forced-gcs ()
  ;; The FEAT_SHA256 VOP holds interior pointers to its input and round constants in raw
  ;; registers; it runs pseudo-atomic. Four threads hash fresh young vectors of one to
  ;; twenty blocks at odd offsets while the main thread forces nursery and full collections;
  ;; each digest is checked against the portable code's.
  (when clempo.sha256-arm64:**native**
    (let* ((state (sb-ext:seed-random-state 23))
           (inputs (coerce (loop repeat 64
                                 collect (let ((data (make-octets (random 1300 state))))
                                           (dotimes (j (length data)) (setf (aref data j) (random 256 state)))
                                           (cons data (with-portable-sha256 (sha256 data)))))
                           'simple-vector))
           (stop nil) (hashed (list 0)) (mismatches (list 0))
           (workers
             (loop for w below 4 collect
               (let ((w w))
                 (sb-thread:make-thread
                  (lambda ()
                    (let ((state (sb-ext:seed-random-state (+ 200 w))))
                      (loop until stop
                            do (destructuring-bind (data . want) (svref inputs (random 64 state))
                                 (let* ((offset (1+ (random 15 state)))
                                        (copy (make-octets (+ offset (length data) 3))))
                                   (replace copy data :start1 offset)
                                   (unless (octets= want (sha256 copy :start offset :end (+ offset (length data))))
                                     (sb-ext:atomic-incf (car mismatches)))
                                   (sb-ext:atomic-incf (car hashed)))))))
                  :name "sha256 gc stress")))))
      (unwind-protect (dotimes (i 60) (sb-ext:gc :full (zerop (mod i 10))))
        (setf stop t)
        (mapc #'sb-thread:join-thread workers))
      (is (> (car hashed) 1000) "~D digests" (car hashed))
      (is= 0 (car mismatches)))))
