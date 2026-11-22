(defpackage #:clempo.chacha20poly1305
  (:use #:cl #:clempo.base)
  (:export #:chacha20-xor #:aead-encrypt #:aead-decrypt))
(in-package #:clempo.chacha20poly1305)

(defbound +word-octets+ 4
  "Octets in a 32-bit ChaCha20 word."
  (= (* 8 +word-octets+) 32))

(defbound +state-words+ 16
  "Words of the ChaCha20 state: four constants, eight key words, a counter and three
nonce words."
  (= +state-words+ (+ 4 (/ 32 +word-octets+) 1 (/ 12 +word-octets+))))

(defbound +block-octets+ 64
  "Keystream octets of one ChaCha20 block: the state words, serialized."
  (= +block-octets+ (* +state-words+ +word-octets+)))

(defbound +double-rounds+ 10
  "Column-and-diagonal round pairs: ChaCha20 runs 20 rounds."
  (= (* 2 +double-rounds+) 20))

(defmacro define-chacha-block ()
  (let ((initial (loop repeat +state-words+ collect (gensym)))
        (state (loop repeat +state-words+ collect (gensym))))
    (flet ((quarter (a b c d)
             (loop for x in (list a c a c) for y in (list b d b d)
                   for z in (list d b d b) for shift in '(16 12 8 7) append
                   `((setf ,x (ldb (byte 32 0) (+ ,x ,y)))
                     (let ((v (logxor ,z ,x)))
                       (setf ,z (logior (ldb (byte 32 0) (ash v ,shift)) (ash v ,(- shift 32)))))))))
      `(defun block-into (words counter out)
         (declare #.clempo.base:*hot* (type (simple-array (unsigned-byte 32) (16)) words)
                  (type (unsigned-byte 32) counter) (type octets out))
         (let* (,@(loop for x in initial for i below +state-words+
                        collect `(,x ,(if (= i 12) 'counter `(aref words ,i))))
                ,@(loop for x in state for y in initial collect `(,x ,y)))
           (declare (type (unsigned-byte 32) ,@initial ,@state))
           (dotimes (round +double-rounds+)
             ,@(loop for indices in '((0 4 8 12) (1 5 9 13) (2 6 10 14) (3 7 11 15)
                                      (0 5 10 15) (1 6 11 12) (2 7 8 13) (3 4 9 14))
                     append (apply #'quarter (mapcar (lambda (i) (nth i state)) indices))))
           ,@(loop for x in state for y in initial for i below +state-words+ collect
                   `(let ((sum (ldb (byte 32 0) (+ ,x ,y))))
                      ,@(loop for j below +word-octets+ collect
                              `(setf (aref out ,(+ (* +word-octets+ i) j)) (ldb (byte 8 ,(* j 8)) sum))))))
         (values)))))
(define-chacha-block)

(defun make-words (key nonce)
  (check-type key octets)
  (check-type nonce octets)
  (assert (= (length key) 32))
  (assert (= (length nonce) 12))
  (let ((words (make-array +state-words+ :element-type '(unsigned-byte 32))))
    (setf (aref words 0) #x61707865 (aref words 1) #x3320646e
          (aref words 2) #x79622d32 (aref words 3) #x6b206574)
    (dotimes (i 8) (setf (aref words (+ 4 i)) (clempo.blake2f::load-le key (* 4 i) 32)))
    (dotimes (i 3) (setf (aref words (+ 13 i)) (clempo.blake2f::load-le nonce (* 4 i) 32)))
    words))

(defun xor-into (words counter data length out scratch)
  (declare #.clempo.base:*hot* (type (simple-array (unsigned-byte 32) (16)) words)
           (type octets data out scratch) (type fixnum length) (type (unsigned-byte 32) counter))
  (loop for offset of-type fixnum from 0 below length by +block-octets+
        for c of-type (unsigned-byte 64) from counter do
    (block-into words (ldb (byte 32 0) c) scratch)
    (dotimes (i (min +block-octets+ (- length offset)))
      (setf (aref out (+ offset i)) (logxor (aref scratch i) (aref data (+ offset i)))))))

(defun chacha20-xor (key nonce data &key (counter 0))
  (clempo.ct:with-data-independent-timing
    (check-type data octets)
    (check-type counter (unsigned-byte 32))
    (assert (<= (+ counter (ceiling (length data) +block-octets+)) (ash 1 32)))
    (let ((out (make-octets (length data))))
      (xor-into (make-words key nonce) counter data (length data) out (make-octets +block-octets+))
      out)))

(defbound +poly-limbs+ 5
  "Radix 2^26 limbs of a Poly1305 accumulator modulo 2^130 - 5."
  (= (* +poly-limbs+ 26) 130)
  ;; A product coefficient sums +POLY-LIMBS+ terms of a 28-bit h limb times a 26-bit r
  ;; limb, scaled by 5 when it wraps: inside the (UNSIGNED-BYTE 60) accumulators, fixnums.
  (< (* +poly-limbs+ (ash 1 28) 5 (ash 1 26)) (ash 1 60))
  (typep (1- (ash 1 60)) 'fixnum))

(defbound +tag-octets+ 16
  "Octets of a Poly1305 tag: the low 128 bits of h + s, two little-endian words."
  (= (* 8 +tag-octets+) 128)
  (= (* 8 (floor +tag-octets+ 2)) 64))

(defmacro define-poly1305 ()
  ;; Radix 2^26: 2^130 = 5 in the field. All products and carries fit fixnums.
  (let ((r (loop repeat +poly-limbs+ collect (gensym))) (h (loop repeat +poly-limbs+ collect (gensym)))
        (d (loop repeat +poly-limbs+ collect (gensym))))
    `(defun auth-tag (secret aad data length out offset)
       (declare #.clempo.base:*hot* (type octets secret aad data out) (type fixnum length offset))
       (let* ((r-low (logand #x0ffffffc0fffffff (clempo.blake2f::load-le secret 0 64)))
              (r-high (logand #x0ffffffc0ffffffc (clempo.blake2f::load-le secret 8 64)))
              ;; Limbs straight from the two key words: an integer r would be a
              ;; bignum whose arithmetic takes value-dependent paths.
              ,@(loop for x in r for i below +poly-limbs+ for bit = (* i 26) collect
                  `(,x ,(cond ((<= (+ bit 26) 64) `(ldb (byte 26 ,bit) r-low))
                              ((>= bit 64) `(ldb (byte 26 ,(- bit 64)) r-high))
                              (t `(logior (ash r-low ,(- bit))
                                          (ldb (byte 26 0) (ash (ldb (byte ,(- (+ bit 26) 64) 0) r-high)
                                                                ,(- 64 bit))))))))
              ,@(loop for x in h collect `(,x 0)))
         (declare (type (unsigned-byte 26) ,@r) (type (unsigned-byte 28) ,@h))
         (labels ((fold (lo hi)
                    (declare (type (unsigned-byte 64) lo hi))
                    ,@(loop for x in h for i below +poly-limbs+ for bit = (* i 26) collect
                            `(incf ,x ,(cond ((<= (+ bit 26) 64) `(ldb (byte 26 ,bit) lo))
                                             ((>= bit 64) `(ldb (byte ,(min 26 (- 128 bit)) ,(- bit 64)) hi))
                                             (t `(logior (ash lo ,(- bit))
                                                         (ash (ldb (byte ,(- (+ bit 26) 64) 0) hi) ,(- 64 bit)))))))
                    (incf ,(nth 4 h) (ash 1 24))
                    (let (,@(loop for x in d for i below +poly-limbs+ collect
                                  `(,x (+ ,@(loop for j below +poly-limbs+ for k = (- i j) collect
                                                  `(* ,(nth j h) ,(if (minusp k) `(* 5 ,(nth (+ +poly-limbs+ k) r)) (nth k r))))))))
                      (declare (type (unsigned-byte 60) ,@d))
                      ,@(loop for x in h for y in d for i below +poly-limbs+ append
                              `((setf ,x (ldb (byte 26 0) ,y))
                                ,@(when (< i 4) `((incf ,(nth (1+ i) d) (ash ,y -26))))))
                      (let ((carry (+ ,(first h) (* 5 (ash ,(fifth d) -26)))))
                        (incf ,(second h) (ash carry -26))
                        (setf ,(first h) (ldb (byte 26 0) carry)))))
                  (padded (v n)
                    (declare (type octets v) (type fixnum n))
                    (let ((end (logandc2 n 15)))
                      (loop for i of-type fixnum from 0 below end by 16 do
                        (fold (clempo.blake2f::load-le v i 64) (clempo.blake2f::load-le v (+ i 8) 64)))
                      (unless (= end n)
                        (let ((lo 0) (hi 0))
                          (declare (type (unsigned-byte 64) lo hi))
                          (loop for i from end below n for j from 0 do
                            (if (< j 8) (setf lo (logior lo (ash (aref v i) (* j 8))))
                                (setf hi (logior hi (ash (aref v i) (* (- j 8) 8))))))
                          (fold lo hi))))))
           (declare (inline fold))
           (padded aad (length aad))
           (padded data length)
           (fold (length aad) length))
         ;; Final reduction and pad without a data-dependent branch (or bignum):
         ;; carry fully, form g = h + 5 - 2^130 and keep it when it does not borrow.
         (destructuring-bind (h0 h1 h2 h3 h4) (list ,@h)
           (declare (type (unsigned-byte 32) h0 h1 h2 h3 h4))
           (macrolet ((carry (from to &optional (scale 1))
                        `(setf ,to (+ ,to (* ,scale (ash ,from -26))) ,from (ldb (byte 26 0) ,from))))
             (carry h1 h2) (carry h2 h3) (carry h3 h4) (carry h4 h0 5) (carry h0 h1))
           (let* ((g0 (+ h0 5)) (g1 (+ h1 (ash g0 -26))) (g2 (+ h2 (ash g1 -26)))
                  (g3 (+ h3 (ash g2 -26))) (g4 (- (+ h4 (ash g3 -26)) (ash 1 26)))
                  (keep-h (ldb (byte 32 0) (ash g4 -40))))
             (declare (type (unsigned-byte 32) g0 g1 g2 g3 keep-h) (type (signed-byte 32) g4))
             (flet ((pick (h g) (logior (logand keep-h h) (logandc1 keep-h (ldb (byte 26 0) g)))))
               (declare (inline pick))
               ;; Limbs may carry one bit past 26, so they are added, not ORed.
               (let ((f0 (pick h0 g0)) (f1 (pick h1 g1)) (f2 (pick h2 g2)) (f3 (pick h3 g3)) (f4 (pick h4 g4)))
                 (declare (type (unsigned-byte 32) f0 f1 f2 f3 f4))
                 (multiple-value-bind (lo carry)
                     (clempo.ec.word:add-with-carry (+ f0 (ash f1 26)) (ldb (byte 64 0) (ash f2 52)) 0)
                   (let ((hi (ldb (byte 64 0) (+ (ash f2 -12) (ash f3 14) (ash f4 40) carry))))
                     (declare (type (unsigned-byte 64) hi))
                     (multiple-value-setq (lo carry)
                       (clempo.ec.word:add-with-carry lo (clempo.blake2f::load-le secret 16 64) 0))
                     (setf hi (clempo.ec.word:add-with-carry hi (clempo.blake2f::load-le secret 24 64) carry))
                     (dotimes (i (floor +tag-octets+ 2))
                       (setf (aref out (+ offset i)) (ldb (byte 8 (* i 8)) lo)
                             (aref out (+ offset (floor +tag-octets+ 2) i)) (ldb (byte 8 (* i 8)) hi)))))))))
       (values)))))
(define-poly1305)

(defparameter *empty-aad* (make-octets 0))

(defun aead-encrypt (key nonce plaintext &key (aad *empty-aad*))
  (clempo.ct:with-data-independent-timing
    (check-type plaintext octets)
    (check-type aad octets)
    (let* ((length (length plaintext)) (words (make-words key nonce))
           (scratch (make-octets +block-octets+)) (out (make-octets (+ length +tag-octets+))))
      (assert (< (ceiling length +block-octets+) (ash 1 32)))
      (xor-into words 1 plaintext length out scratch)
      (block-into words 0 scratch)
      (auth-tag scratch aad out length out length)
      out)))

(defun aead-decrypt (key nonce ciphertext &key (aad *empty-aad*))
  (clempo.ct:with-data-independent-timing
    (check-type ciphertext octets)
    (check-type aad octets)
    (let ((length (- (length ciphertext) +tag-octets+)))
      (when (and (>= length 0) (< (ceiling length +block-octets+) (ash 1 32)))
        (let ((words (make-words key nonce)) (scratch (make-octets +block-octets+))
              (tag (make-octets +tag-octets+)) (difference 0))
          (block-into words 0 scratch)
          (auth-tag scratch aad ciphertext length tag 0)
          (dotimes (i +tag-octets+)
            (setf difference (logior difference (logxor (aref tag i) (aref ciphertext (+ length i))))))
          (when (zerop difference)
            (let ((out (make-octets length)))
              (xor-into words 1 ciphertext length out scratch)
              out)))))))
