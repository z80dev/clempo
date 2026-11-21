(defpackage #:clempo.gcm
  (:use #:cl #:clempo.base #:clempo.aes)
  (:documentation "AES-GCM (NIST SP 800-38D) with 96-bit nonces and 128-bit tags. GHASH
uses 4-bit tables over two 64-bit halves (bit 0 of a block is the top bit of its first byte).")
  (:export #:make-gcm #:gcm-encrypt #:gcm-decrypt #:gcm-p))
(in-package #:clempo.gcm)

(deftype u64 () '(unsigned-byte 64))
(deftype table () '(simple-array (unsigned-byte 64) (32)))

(defparameter *reduce4*
    ;; Dropping four low bits at once: bit k leaves after k+1 shifts, so its reduction
    ;; term 0xE1<<56 has since moved right by the remaining 3-k shifts.
    (let ((r (make-array 16 :element-type '(unsigned-byte 64) :initial-element 0)))
      (dotimes (rem 16 r)
        (dotimes (k 4)
          (when (logbitp k rem)
            (setf (aref r rem) (logxor (aref r rem) (ash (ash #xe1 56) (- k 3)))))))))

(defstruct (gcm (:constructor %make-gcm (cipher table)))
  (cipher nil :read-only t)
  (table nil :type table :read-only t))

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
  (let* ((cipher (make-aes key))
         (h (aes-encrypt-block cipher (make-octets 16)))
         (table (make-array 32 :element-type '(unsigned-byte 64) :initial-element 0))
         (hi (load64 h 0)) (lo (load64 h 8)))
    ;; Entry i (pair 2i, 2i+1) is H times the nibble i read with 8 as the x^0 term.
    (loop for i = 8 then (ash i -1) while (plusp i)
          do (setf (aref table (* 2 i)) hi (aref table (1+ (* 2 i))) lo)
             (let ((carry (logand lo 1)))
               (setf lo (logior (ash lo -1) (ldb (byte 64 0) (ash hi 63)))
                     hi (logxor (ash hi -1) (if (= carry 1) (ash #xe1 56) 0)))))
    (dolist (i '(3 5 6 7 9 10 11 12 13 14 15))
      (let ((a (ash 1 (1- (integer-length i)))))
        (setf (aref table (* 2 i)) (logxor (aref table (* 2 a)) (aref table (* 2 (- i a))))
              (aref table (1+ (* 2 i))) (logxor (aref table (1+ (* 2 a))) (aref table (1+ (* 2 (- i a))))))))
    (%make-gcm cipher table)))

(defun ghash-blocks (table state data start end)
  "Absorb DATA[START,END) zero-padded to whole blocks into STATE (two u64)."
  (declare #.*hot* (type table table) (type (simple-array (unsigned-byte 64) (2)) state)
           (type octets data) (type fixnum start end))
  (let ((r *reduce4*) (block (make-octets 16)))
    (declare (type (simple-array (unsigned-byte 64) (16)) r))
    (loop for pos of-type fixnum from start below end by 16 do
      (let ((n (min 16 (- end pos))))
        (let ((src data) (at pos))
          (declare (type octets src) (type fixnum at))
          (when (< n 16)
            (fill block 0)
            (replace block data :start2 pos :end2 (+ pos n))
            (setf src block at 0))
        (let ((xh (logxor (aref state 0) (load64 src at)))
              (xl (logxor (aref state 1) (load64 src (+ at 8))))
              (zh 0) (zl 0))
          (declare (type u64 xh xl zh zl))
          (flet ((absorb (nib)
                   (declare (type (unsigned-byte 4) nib))
                   (let ((rem (logand zl 15)))
                     (setf zl (logior (ash zl -4) (ldb (byte 64 0) (ash zh 60)))
                           zh (logxor (ash zh -4) (aref r rem))))
                   (setf zh (logxor zh (aref table (* 2 nib)))
                         zl (logxor zl (aref table (1+ (* 2 nib)))))))
            (declare (inline absorb))
            (loop for shift of-type (integer 0 64) from 0 to 56 by 8
                  for byte of-type (unsigned-byte 8) = (ldb (byte 8 shift) xl)
                  do (absorb (logand byte 15)) (absorb (ash byte -4)))
            (loop for shift of-type (integer 0 64) from 0 to 56 by 8
                  for byte of-type (unsigned-byte 8) = (ldb (byte 8 shift) xh)
                  do (absorb (logand byte 15)) (absorb (ash byte -4))))
          (setf (aref state 0) zh (aref state 1) zl)))))
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
  (let ((cipher (gcm-cipher gcm)) (ks (make-octets 16)) (ctr (counter-block nonce 2)))
    (loop for pos of-type fixnum from start below end by 16
          for c of-type (unsigned-byte 32) from 2 do
            (setf (aref ctr 12) (ldb (byte 8 24) c) (aref ctr 13) (ldb (byte 8 16) c)
                  (aref ctr 14) (ldb (byte 8 8) c) (aref ctr 15) (ldb (byte 8 0) c))
            (aes-encrypt-block cipher ctr ks)
            (loop for j of-type fixnum from 0 below (min 16 (- end pos))
                  do (setf (aref out (+ out-start (- pos start) j))
                           (logxor (aref data (+ pos j)) (aref ks j)))))
    out))

(defun compute-tag (gcm nonce aad ct start end)
  (let ((state (make-array 2 :element-type '(unsigned-byte 64) :initial-element 0))
        (lens (make-octets 16)) (table (gcm-table gcm)))
    (ghash-blocks table state aad 0 (length aad))
    (ghash-blocks table state ct start end)
    (store64 (* 8 (length aad)) lens 0)
    (store64 (* 8 (- end start)) lens 8)
    (ghash-blocks table state lens 0 16)
    (let ((tag (aes-encrypt-block (gcm-cipher gcm) (counter-block nonce 1))))
      (dotimes (i 8) (setf (aref tag i) (logxor (aref tag i) (ldb (byte 8 (- 56 (* 8 i))) (aref state 0)))
                           (aref tag (+ 8 i)) (logxor (aref tag (+ 8 i))
                                                      (ldb (byte 8 (- 56 (* 8 i))) (aref state 1)))))
      tag)))

(defparameter *empty* (make-octets 0))

(defun gcm-encrypt (gcm nonce plaintext &key (aad *empty*))
  "Ciphertext followed by the 16-byte tag."
  (check-type gcm gcm) (check-type nonce octets) (check-type plaintext octets) (check-type aad octets)
  (assert (= (length nonce) 12))
  (let* ((n (length plaintext)) (out (make-octets (+ n 16))))
    (ctr-xor gcm nonce plaintext 0 n out 0)
    (replace out (compute-tag gcm nonce aad out 0 n) :start1 n)
    out))

(defun gcm-decrypt (gcm nonce ciphertext &key (aad *empty*))
  "Plaintext, or NIL when CIPHERTEXT (with its trailing tag) does not authenticate."
  (check-type gcm gcm) (check-type nonce octets) (check-type ciphertext octets) (check-type aad octets)
  (assert (= (length nonce) 12))
  (let ((n (- (length ciphertext) 16)))
    (when (>= n 0)
      (let ((tag (compute-tag gcm nonce aad ciphertext 0 n)) (diff 0))
        (dotimes (i 16) (setf diff (logior diff (logxor (aref tag i) (aref ciphertext (+ n i))))))
        (when (zerop diff)
          (ctr-xor gcm nonce ciphertext 0 n (make-octets n) 0))))))
