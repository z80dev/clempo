(defpackage #:clempo.aes
  (:use #:cl #:clempo.base)
  (:documentation "Bitsliced AES-128/256 (FIPS 197) and CTR mode. Constant-time: no table
lookup, branch or index depends on key or data. Up to four blocks share one pass: bit k
of every state byte lives in plane k, byte i of block l at bit 16l+i. SubBytes is GF(2^8)
inversion as x^254 over the planes followed by the affine map. On an arm64 core with
FEAT_AES the blocks run on AESE/AESMC instead (clempo.aes-arm64), also constant-time.")
  (:export #:make-aes #:aes-encrypt-block #:aes-encrypt-blocks #:make-aes-ctr #:aes-ctr-xor))
(in-package #:clempo.aes)

(deftype u64 () '(unsigned-byte 64))
(deftype planes () '(simple-array (unsigned-byte 64) (*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reduced-power (d)
    "x^D modulo the AES polynomial x^8+x^4+x^3+x+1, as a byte."
    (let ((v (ash 1 d)))
      (loop for bit from 14 downto 8
            when (logbitp bit v) do (setf v (logxor v (ash #x11b (- bit 8)))))
      v))
  (defun reduce-forms (outs terms)
    "Set OUTS[k] to the XOR of the TERMS (degree . form) whose reduced power has bit k."
    (loop for out in outs for k from 0
          collect `(setf ,out (logxor 0 ,@(loop for (d . form) in terms
                                                when (logbitp k (reduced-power d)) collect form)))))
  (defun gf-mul-forms (outs as bs)
    (let ((cs (loop repeat 15 collect (gensym "C"))))
      `((let (,@(loop for c in cs for d from 0 collect
                  `(,c (logxor 0 ,@(loop for i from (max 0 (- d 7)) to (min d 7)
                                         collect `(logand ,(nth i as) ,(nth (- d i) bs)))))))
          (declare (type u64 ,@cs))
          ,@(reduce-forms outs (loop for c in cs for d from 0 collect (cons d c)))))))
  (defun gf-square-forms (outs as)
    (reduce-forms outs (loop for a in as for i from 0 collect (cons (* 2 i) a)))))

(defmacro define-sbox ()
  ;; x^254 = x^-1 (and 0 for 0): x2 x3 x6 x12 x15 x30 x60 x120 x240 x252 x254.
  (let ((vars (make-hash-table))
        (steps '((x2 :square x) (x3 :mul x2 x) (x6 :square x3) (x12 :square x6)
                 (x15 :mul x12 x3) (x30 :square x15) (x60 :square x30) (x120 :square x60)
                 (x240 :square x120) (x252 :mul x240 x12) (x254 :mul x252 x2))))
    (flet ((v (name) (or (gethash name vars)
                         (setf (gethash name vars) (loop repeat 8 collect (gensym (string name)))))))
      (let ((x (v 'x)) (temps (loop for (name) in steps append (v name))) (y (v 'x254)))
        `(defun sbox-planes! (planes)
           "SubBytes on the eight bit PLANES, in place. The planes stay in a word vector:
returned as eight values they would be boxed, allocating for some data and not others."
           (declare #.clempo.base:*hot* (type planes planes))
           (let (,@(loop for v in x for i from 0 collect `(,v (aref planes ,i)))
                 ,@(loop for s in temps collect `(,s 0)))
             (declare (type u64 ,@x ,@temps))
             ,@(loop for (name op a b) in steps append
                 (if (eq op :square)
                     (gf-square-forms (v name) (v a))
                     (gf-mul-forms (v name) (v a) (v b))))
             ;; s_i = b_i + b_(i+4) + b_(i+5) + b_(i+6) + b_(i+7) + bit i of #x63.
             ,@(loop for i below 8 collect
                 `(setf (aref planes ,i)
                        (logxor ,@(loop for k in '(0 4 5 6 7) collect (nth (mod (+ i k) 8) y))
                                ,(if (logbitp i #x63) #xffffffffffffffff 0))))
             planes))))))
(define-sbox)

(defmacro lane-mask (pattern) (* pattern #x0001000100010001))

(declaim (inline shift-rows rotate-rows))
(defun shift-rows (x)
  "Row r of every block rotates left by r columns: byte 4c+r takes byte 4(c+r mod 4)+r."
  (declare #.clempo.base:*hot* (type u64 x))
  (logior (logand x (lane-mask #x1111))
          (logand (ash x -4) (lane-mask #x0222)) (logand (ldb (byte 64 0) (ash x 12)) (lane-mask #x2000))
          (logand (ash x -8) (lane-mask #x0044)) (logand (ldb (byte 64 0) (ash x 8)) (lane-mask #x4400))
          (logand (ash x -12) (lane-mask #x0008)) (logand (ldb (byte 64 0) (ash x 4)) (lane-mask #x8880))))

(defun rotate-rows (x k)
  "Byte 4c+r takes byte 4c+(r+K mod 4) within each column."
  (declare #.clempo.base:*hot* (type u64 x) (type (integer 1 3) k))
  (ecase k
    (1 (logior (logand (ash x -1) (lane-mask #x7777)) (logand (ldb (byte 64 0) (ash x 3)) (lane-mask #x8888))))
    (2 (logior (logand (ash x -2) (lane-mask #x3333)) (logand (ldb (byte 64 0) (ash x 2)) (lane-mask #xcccc))))
    (3 (logior (logand (ash x -3) (lane-mask #x1111)) (logand (ldb (byte 64 0) (ash x 1)) (lane-mask #xeeee))))))

(defmacro with-planes ((&rest vars) &body body)
  `(let (,@(loop for v in vars collect `(,v 0))) (declare (type u64 ,@vars)) ,@body))

(defun encrypt-planes (keys rounds state)
  "Encrypt the eight STATE planes in place with the bitsliced round KEYS."
  (declare #.clempo.base:*hot* (type planes keys state) (type (member 10 14) rounds))
  (let ((s0 (aref state 0)) (s1 (aref state 1)) (s2 (aref state 2)) (s3 (aref state 3))
        (s4 (aref state 4)) (s5 (aref state 5)) (s6 (aref state 6)) (s7 (aref state 7)))
    (declare (type u64 s0 s1 s2 s3 s4 s5 s6 s7))
    (macrolet ((add-key (round)
                 `(let ((base (* 8 ,round)))
                    ,@(loop for s in '(s0 s1 s2 s3 s4 s5 s6 s7) for i from 0
                            collect `(setf ,s (logxor ,s (aref keys (+ base ,i)))))))
               (sub-shift ()
                 `(progn
                    (setf (aref state 0) s0 (aref state 1) s1 (aref state 2) s2 (aref state 3) s3
                          (aref state 4) s4 (aref state 5) s5 (aref state 6) s6 (aref state 7) s7)
                    (sbox-planes! state)
                    (setf s0 (aref state 0) s1 (aref state 1) s2 (aref state 2) s3 (aref state 3)
                          s4 (aref state 4) s5 (aref state 5) s6 (aref state 6) s7 (aref state 7))
                    ,@(loop for s in '(s0 s1 s2 s3 s4 s5 s6 s7) collect `(setf ,s (shift-rows ,s)))))
               (mix-columns ()
                 ;; out = 2(a + rot1 a) + rot1 a + rot2 a + rot3 a, per plane except xtime.
                 (let ((ss '(s0 s1 s2 s3 s4 s5 s6 s7))
                       (ts (loop repeat 8 collect (gensym "T")))
                       (us (loop repeat 8 collect (gensym "U"))))
                   `(let (,@(loop for s in ss for u in us
                                  collect `(,u (logxor (rotate-rows ,s 1) (rotate-rows ,s 2) (rotate-rows ,s 3))))
                          ,@(loop for s in ss for tt in ts collect `(,tt (logxor ,s (rotate-rows ,s 1)))))
                      (declare (type u64 ,@ts ,@us))
                      ;; xtime: bit k <- bit k-1, plus bit 7 into bits 0, 1, 3, 4.
                      ,@(loop for s in ss for u in us for k from 0 collect
                          `(setf ,s (logxor ,u ,(if (zerop k) (nth 7 ts) (nth (1- k) ts))
                                            ,@(when (member k '(1 3 4)) (list (nth 7 ts))))))))))
      (add-key 0)
      (loop for round of-type fixnum from 1 below rounds do
        (sub-shift) (mix-columns) (add-key round))
      (sub-shift)
      (add-key rounds))
    (setf (aref state 0) s0 (aref state 1) s1 (aref state 2) s2 (aref state 3) s3
          (aref state 4) s4 (aref state 5) s5 (aref state 6) s6 (aref state 7) s7))
  state)

(defun load-planes! (state octets start count)
  "Transpose COUNT 16-octet blocks from OCTETS into the eight STATE planes."
  (declare #.clempo.base:*hot* (type planes state) (type octets octets)
           (type fixnum start) (type (integer 1 4) count))
  (fill state 0)
  (dotimes (b (* 16 count) state)
    (let ((v (aref octets (+ start b))))
      (dotimes (k 8)
        (setf (aref state k) (logior (aref state k) (ash (ldb (byte 1 k) v) b)))))))

(defun store-planes! (state octets start count)
  (declare #.clempo.base:*hot* (type planes state) (type octets octets)
           (type fixnum start) (type (integer 1 4) count))
  (dotimes (b (* 16 count) octets)
    (let ((v 0))
      (declare (type (unsigned-byte 8) v))
      (dotimes (k 8)
        (setf v (logior v (ash (ldb (byte 1 b) (aref state k)) k))))
      (setf (aref octets (+ start b)) v))))

(defun sub-word (word)
  "SubWord on a 32-bit key-schedule word (byte 0 most significant). Planes stay in a
word vector: boxing them could allocate bignums for some key bits and not others."
  (declare #.clempo.base:*hot* (type (unsigned-byte 32) word))
  (let ((planes (make-array 8 :element-type 'u64 :initial-element 0)) (out 0))
    (declare (dynamic-extent planes) (type (unsigned-byte 32) out))
    (dotimes (b 4)
      (dotimes (k 8)
        (setf (aref planes k) (logior (aref planes k)
                                      (ash (ldb (byte 1 (+ k (* 8 (- 3 b)))) word) b)))))
    (sbox-planes! planes)
    (dotimes (b 4 out)
      (dotimes (k 8)
        (setf out (logior out (ash (ldb (byte 1 b) (aref planes k)) (+ k (* 8 (- 3 b))))))))))

(defstruct (aes (:constructor %make-aes (keys bytes rounds)))
  (keys (make-array 0 :element-type 'u64) :type planes :read-only t)
  (bytes (make-octets 0) :type octets :read-only t)
  (rounds 10 :type (member 10 14) :read-only t))

(defun make-aes (key)
  (clempo.ct:with-data-independent-timing
    (check-type key octets)
    (assert (member (length key) '(16 32)))
    (let* ((nk (/ (length key) 4)) (rounds (if (= nk 4) 10 14))
           (total (* 4 (1+ rounds)))
           (w (make-array total :element-type '(unsigned-byte 32)))
           (bytes (make-octets (* 4 total)))
           (keys (make-array (* 8 (1+ rounds)) :element-type 'u64 :initial-element 0))
           (state (make-array 8 :element-type 'u64 :initial-element 0))
           (rcon 1))
      ;; Typed, so the words assemble inline: generic ASH returns early for a zero byte.
      (dotimes (i nk)
        (flet ((byte-at (j) (the (unsigned-byte 8) (aref (the octets key) (+ (* 4 i) j)))))
          (setf (aref w i) (logior (ash (byte-at 0) 24) (ash (byte-at 1) 16)
                                   (ash (byte-at 2) 8) (byte-at 3)))))
      (loop for i from nk below total do
        (let ((temp (aref w (1- i))))
          (cond ((zerop (mod i nk))
                 (setf temp (logxor (sub-word (logior (ldb (byte 32 0) (ash temp 8)) (ash temp -24)))
                                    (ash rcon 24))
                       rcon (logxor (ldb (byte 8 0) (ash rcon 1)) (if (logbitp 7 rcon) #x1b 0))))
                ((and (> nk 6) (= (mod i nk) 4))
                 (setf temp (sub-word temp))))
          (setf (aref w i) (logxor (aref w (- i nk)) temp))))
      (dotimes (i total)
        (dotimes (j 4) (setf (aref bytes (+ (* 4 i) j)) (ldb (byte 8 (* 8 (- 3 j))) (aref w i)))))
      ;; Every round key repeats across the four block lanes (by shifts: a generic
      ;; multiply here would allocate a bignum for some key bits).
      (dotimes (round (1+ rounds))
        (load-planes! state bytes (* 16 round) 1)
        (dotimes (k 8)
          (let ((lane (ldb (byte 16 0) (aref state k))))
            (setf (aref keys (+ (* 8 round) k))
                  (logior lane (ash lane 16) (ash lane 32) (ash lane 48))))))
      (%make-aes keys bytes rounds))))

(defun aes-encrypt-blocks (cipher input in-start output out-start count)
  "Encrypt COUNT (1 to 4) consecutive blocks of INPUT into OUTPUT in one pass."
  (declare (type aes cipher) (type octets input output) (type fixnum in-start out-start)
           (type (integer 1 4) count))
  (clempo.ct:with-data-independent-timing
    (assert (and (<= 0 in-start (+ in-start (* 16 count)) (length input))
                 (<= 0 out-start (+ out-start (* 16 count)) (length output))))
    #+arm64
    (when clempo.aes-arm64:**native**
      (dotimes (b count)
        (clempo.aes-arm64:encrypt-block (aes-bytes cipher) (aes-rounds cipher)
                                        input (+ in-start (* 16 b)) output (+ out-start (* 16 b))))
      (return-from aes-encrypt-blocks output))
    (let ((state (make-array 8 :element-type 'u64 :initial-element 0)))
      (declare (dynamic-extent state))
      (load-planes! state input in-start count)
      (encrypt-planes (aes-keys cipher) (aes-rounds cipher) state)
      (store-planes! state output out-start count)
      output)))

(defun aes-encrypt-block (cipher block &optional (out (make-octets 16)))
  (check-type cipher aes)
  (check-type block octets)
  (check-type out octets)
  (assert (= (length block) (length out) 16))
  (aes-encrypt-blocks cipher block 0 out 0 1))

(defstruct (aes-ctr (:constructor %make-aes-ctr (cipher counter)))
  (cipher nil :type (or null aes) :read-only t)
  (counter (make-octets 16) :type octets :read-only t)
  (counters (make-octets 64) :type octets :read-only t)
  (keystream (make-octets 64) :type octets :read-only t)
  (used 0 :type (integer 0 64))
  (available 0 :type (integer 0 64))
  (exhausted nil :type boolean))

(defun make-aes-ctr (cipher initial-counter)
  (check-type cipher aes)
  (check-type initial-counter octets)
  (assert (= (length initial-counter) 16))
  (%make-aes-ctr cipher (copy-seq initial-counter)))

(defun refill (ctx)
  "Encrypt the next four counter blocks; a 128-bit wrap ends the stream after it."
  (assert (not (aes-ctr-exhausted ctx)))
  (let ((ctr (aes-ctr-counter ctx)) (counters (aes-ctr-counters ctx)) (blocks 4))
    (dotimes (b 4)
      (when (< b blocks)
        (replace counters ctr :start1 (* 16 b))
        (unless (loop for j from 15 downto 0
                      for next = (ldb (byte 8 0) (1+ (aref ctr j)))
                      do (setf (aref ctr j) next)
                      when (plusp next) return t)
          (setf (aes-ctr-exhausted ctx) t blocks (1+ b)))))
    (aes-encrypt-blocks (aes-ctr-cipher ctx) counters 0 (aes-ctr-keystream ctx) 0 blocks)
    (setf (aes-ctr-used ctx) 0 (aes-ctr-available ctx) (* 16 blocks))))

(defun aes-ctr-xor (ctx data &key (start 0) (end (length data)) out (out-start 0))
  "XOR DATA's slice with this context's next bytes, preserving partial blocks."
  (check-type ctx aes-ctr)
  (check-type data octets)
  (assert (<= 0 start end (length data)))
  (let ((out (or out (make-octets (+ out-start (- end start)))))
        (key (aes-ctr-keystream ctx)))
    (check-type out octets)
    (assert (<= 0 out-start (+ out-start (- end start)) (length out)))
    (assert (or (not (eq out data)) (<= out-start start) (>= out-start end)))
    (loop for source from start below end for dest from out-start do
      (when (= (aes-ctr-used ctx) (aes-ctr-available ctx)) (refill ctx))
      (setf (aref out dest) (logxor (aref data source) (aref key (aes-ctr-used ctx))))
      (incf (aes-ctr-used ctx)))
    out))
