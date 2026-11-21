(defpackage #:clempo.gcm
  (:use #:cl #:clempo.base #:clempo.aes)
  (:documentation "AES-GCM (NIST SP 800-38D) with 96-bit nonces and 128-bit tags. GHASH
is carry-less multiplication by masked integer products (no table, so no index
depends on H or the data; bit 0 of a block is the top bit of its first byte).")
  (:export #:make-gcm #:gcm-encrypt #:gcm-decrypt #:gcm-p))
(in-package #:clempo.gcm)

(deftype u64 () '(unsigned-byte 64))
(deftype hash-key () '(simple-array (unsigned-byte 64) (2)))

(defstruct (gcm (:constructor %make-gcm (cipher h)))
  (cipher nil :read-only t)
  (h nil :type hash-key :read-only t))

(declaim (inline load64 store64))
(defun load64 (octets start)
  (declare #.*hot* (type octets octets) (type fixnum start))
  (let ((x 0))
    (declare (type u64 x))
    (dotimes (i 8 x) (setf x (logior (ldb (byte 64 0) (ash x 8)) (aref octets (+ start i)))))))

(defun store64 (x octets start)
  (declare #.*hot* (type u64 x) (type octets octets) (type fixnum start))
  (dotimes (i 8) (setf (aref octets (+ start i)) (ldb (byte 8 (- 56 (* 8 i))) x))))

(defun make-gcm (key)
  "Expand an AES-128 or AES-256 KEY into a GCM context."
  (clempo.ct:with-data-independent-timing
    (let* ((cipher (make-aes key))
           (h (aes-encrypt-block cipher (make-octets 16)))
           (words (make-array 2 :element-type '(unsigned-byte 64))))
      (setf (aref words 0) (load64 h 0) (aref words 1) (load64 h 8))
      (%make-gcm cipher words))))

(declaim (inline bmul64 rev64))
(defun bmul64 (x y)
  "Low 64 bits of the carry-less product of X and Y. Integer multiplication of operands
with three-bit holes keeps every carry out of the bits that are kept (BearSSL ctmul64)."
  (declare #.*hot* (type u64 x y))
  (let ((x0 (logand x #x1111111111111111)) (x1 (logand x #x2222222222222222))
        (x2 (logand x #x4444444444444444)) (x3 (logand x #x8888888888888888))
        (y0 (logand y #x1111111111111111)) (y1 (logand y #x2222222222222222))
        (y2 (logand y #x4444444444444444)) (y3 (logand y #x8888888888888888)))
    (macrolet ((m (a b) `(ldb (byte 64 0) (* ,a ,b))))
      (logior (logand (logxor (m x0 y0) (m x1 y3) (m x2 y2) (m x3 y1)) #x1111111111111111)
              (logand (logxor (m x0 y1) (m x1 y0) (m x2 y3) (m x3 y2)) #x2222222222222222)
              (logand (logxor (m x0 y2) (m x1 y1) (m x2 y0) (m x3 y3)) #x4444444444444444)
              (logand (logxor (m x0 y3) (m x1 y2) (m x2 y1) (m x3 y0)) #x8888888888888888)))))

(defun rev64 (x)
  (declare #.*hot* (type u64 x))
  (macrolet ((swap (mask shift)
               `(setf x (logior (ldb (byte 64 0) (ash (logand x ,mask) ,shift))
                                (logand (ash x ,(- shift)) ,mask)))))
    (swap #x5555555555555555 1) (swap #x3333333333333333 2) (swap #x0f0f0f0f0f0f0f0f 4)
    (swap #x00ff00ff00ff00ff 8) (swap #x0000ffff0000ffff 16)
    (logior (ldb (byte 64 0) (ash x 32)) (ash x -32))))

(defun ghash-blocks (h state data start end)
  "Absorb DATA[START,END) zero-padded to whole blocks into STATE (two u64): Karatsuba
over BMUL64 on the bit-reflected halves, then the GCM reduction, all branch-free."
  (declare #.*hot* (type hash-key h) (type (simple-array (unsigned-byte 64) (2)) state)
           (type octets data) (type fixnum start end))
  (let* ((block (make-octets 16))
         (h1 (aref h 0)) (h0 (aref h 1)) (h0r (rev64 h0)) (h1r (rev64 h1))
         (h2 (logxor h0 h1)) (h2r (logxor h0r h1r))
         (y1 (aref state 0)) (y0 (aref state 1)))
    (declare (type u64 h0 h1 h0r h1r h2 h2r y0 y1))
    (loop for pos of-type fixnum from start below end by 16 do
      (let ((n (min 16 (- end pos))) (src data) (at pos))
        (declare (type octets src) (type fixnum at))
        (when (< n 16)
          (fill block 0)
          (replace block data :start2 pos :end2 (+ pos n))
          (setf src block at 0))
        (setf y1 (logxor y1 (load64 src at)) y0 (logxor y0 (load64 src (+ at 8))))
        (let* ((y0r (rev64 y0)) (y1r (rev64 y1)) (y2 (logxor y0 y1)) (y2r (logxor y0r y1r))
               (z0 (bmul64 y0 h0)) (z1 (bmul64 y1 h1)) (z2 (bmul64 y2 h2))
               (z0h (bmul64 y0r h0r)) (z1h (bmul64 y1r h1r)) (z2h (bmul64 y2r h2r)))
          (declare (type u64 y0r y1r y2 y2r z0 z1 z2 z0h z1h z2h))
          (setf z2 (logxor z2 z0 z1) z2h (logxor z2h z0h z1h)
                z0h (ash (rev64 z0h) -1) z1h (ash (rev64 z1h) -1) z2h (ash (rev64 z2h) -1))
          (let ((v0 z0) (v1 (logxor z0h z2)) (v2 (logxor z1 z2h)) (v3 z1h))
            (declare (type u64 v0 v1 v2 v3))
            (macrolet ((shl (x k) `(ldb (byte 64 0) (ash ,x ,k))))
              (setf v3 (logior (shl v3 1) (ash v2 -63)) v2 (logior (shl v2 1) (ash v1 -63))
                    v1 (logior (shl v1 1) (ash v0 -63)) v0 (shl v0 1))
              (setf v2 (logxor v2 v0 (ash v0 -1) (ash v0 -2) (ash v0 -7))
                    v1 (logxor v1 (shl v0 63) (shl v0 62) (shl v0 57))
                    v3 (logxor v3 v1 (ash v1 -1) (ash v1 -2) (ash v1 -7))
                    v2 (logxor v2 (shl v1 63) (shl v1 62) (shl v1 57))))
            (setf y0 v2 y1 v3)))))
    (setf (aref state 0) y1 (aref state 1) y0)
    state))

(defun counter-block (nonce n)
  (let ((b (make-octets 16)))
    (setf (aref b 12) (ldb (byte 8 24) n) (aref b 13) (ldb (byte 8 16) n)
          (aref b 14) (ldb (byte 8 8) n) (aref b 15) (ldb (byte 8 0) n))
    (replace b nonce :end2 12)
    b))

(defun ctr-xor (gcm nonce data start end out out-start)
  "XOR DATA[START,END) with the keystream from counter 2 (inc32 of J0) into OUT."
  (declare (type octets data out) (type fixnum start end out-start))
  (let ((cipher (gcm-cipher gcm)) (ks (make-octets 64)) (ctrs (make-octets 64)))
    (dotimes (b 4) (replace ctrs nonce :start1 (* 16 b) :end2 12))
    (loop for pos of-type fixnum from start below end by 64
          for c of-type fixnum from 2 by 4 do
      (let ((blocks (min 4 (ceiling (- end pos) 16))))
        (dotimes (b blocks)
          (let ((n (ldb (byte 32 0) (+ c b))) (at (* 16 b)))
            (setf (aref ctrs (+ at 12)) (ldb (byte 8 24) n) (aref ctrs (+ at 13)) (ldb (byte 8 16) n)
                  (aref ctrs (+ at 14)) (ldb (byte 8 8) n) (aref ctrs (+ at 15)) (ldb (byte 8 0) n))))
        (aes-encrypt-blocks cipher ctrs 0 ks 0 blocks)
        (loop for j of-type fixnum from 0 below (min 64 (- end pos))
              do (setf (aref out (+ out-start (- pos start) j))
                       (logxor (aref data (+ pos j)) (aref ks j))))))
    out))

(defun compute-tag (gcm nonce aad ct start end)
  (let ((state (make-array 2 :element-type '(unsigned-byte 64) :initial-element 0))
        (lens (make-octets 16)) (h (gcm-h gcm)))
    (ghash-blocks h state aad 0 (length aad))
    (ghash-blocks h state ct start end)
    (store64 (* 8 (length aad)) lens 0)
    (store64 (* 8 (- end start)) lens 8)
    (ghash-blocks h state lens 0 16)
    (let ((tag (aes-encrypt-block (gcm-cipher gcm) (counter-block nonce 1))))
      (dotimes (i 8) (setf (aref tag i) (logxor (aref tag i) (ldb (byte 8 (- 56 (* 8 i))) (aref state 0)))
                           (aref tag (+ 8 i)) (logxor (aref tag (+ 8 i))
                                                      (ldb (byte 8 (- 56 (* 8 i))) (aref state 1)))))
      tag)))

(defparameter *empty* (make-octets 0))

(defun gcm-encrypt (gcm nonce plaintext &key (aad *empty*))
  "Ciphertext followed by the 16-byte tag."
  (clempo.ct:with-data-independent-timing
    (check-type gcm gcm) (check-type nonce octets) (check-type plaintext octets) (check-type aad octets)
    (assert (= (length nonce) 12))
    (let* ((n (length plaintext)) (out (make-octets (+ n 16))))
      (ctr-xor gcm nonce plaintext 0 n out 0)
      (replace out (compute-tag gcm nonce aad out 0 n) :start1 n)
      out)))

(defun gcm-decrypt (gcm nonce ciphertext &key (aad *empty*))
  "Plaintext, or NIL when CIPHERTEXT (with its trailing tag) does not authenticate."
  (clempo.ct:with-data-independent-timing
    (check-type gcm gcm) (check-type nonce octets) (check-type ciphertext octets) (check-type aad octets)
    (assert (= (length nonce) 12))
    (let ((n (- (length ciphertext) 16)))
      (when (>= n 0)
        (let ((tag (compute-tag gcm nonce aad ciphertext 0 n)) (diff 0))
          (dotimes (i 16) (setf diff (logior diff (logxor (aref tag i) (aref ciphertext (+ n i))))))
          (when (zerop diff)
            (ctr-xor gcm nonce ciphertext 0 n (make-octets n) 0)))))))
