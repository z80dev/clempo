(defpackage #:clempo.blake2f
  (:use #:cl #:clempo.base)
  (:export #:blake2f #:blake2f-precompile))
(defpackage #:clempo.blake3
  (:use #:cl #:clempo.base)
  (:export #:blake3 #:blake3-keyed #:blake3-derive-key))
(in-package #:clempo.blake2f)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *iv32* '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                        #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  (defparameter *iv64* '(#x6a09e667f3bcc908 #xbb67ae8584caa73b #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
                        #x510e527fade682d1 #x9b05688c2b3e6c1f #x1f83d9abfb41bd6b #x5be0cd19137e2179))
  (defparameter *sigma2*
    '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
      (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
      (11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
      (7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
      (9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
      (2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
      (12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
      (13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
      (6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
      (10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0)))
  (defun mix-round (v m indices width rotations)
    (flet ((add (&rest x) `(ldb (byte ,width 0) (+ ,@x)))
           (ror (a b n) `(let ((x (logxor ,a ,b)))
                           (logior (ash x ,(- n)) (ldb (byte ,width 0) (ash x ,(- width n)))))))
      (destructuring-bind (r0 r1 r2 r3) rotations
        (loop for (ai bi ci di) in '((0 4 8 12) (1 5 9 13) (2 6 10 14) (3 7 11 15)
                                    (0 5 10 15) (1 6 11 12) (2 7 8 13) (3 4 9 14))
              for (xi yi) on indices by #'cddr
              for a = (nth ai v) for b = (nth bi v) for c = (nth ci v) for d = (nth di v)
              append `((setf ,a ,(add a b (nth xi m)) ,d ,(ror d a r0)
                             ,c ,(add c d) ,b ,(ror b c r1)
                             ,a ,(add a b (nth yi m)) ,d ,(ror d a r2)
                             ,c ,(add c d) ,b ,(ror b c r3))))))))

(defbound +state-words+ 8
  "Words of the BLAKE2b chaining value h; the work vector's upper half starts as the IV."
  (= (length *iv64*) +state-words+))

(defbound +block-words+ 16
  "Message words of a 128-octet BLAKE2b block: each round's schedule permutes all of them."
  (= (* 8 +block-words+) 128)
  (every (lambda (row) (equal (sort (copy-list row) #'<) (loop for i below +block-words+ collect i)))
         *sigma2*))

(defbound +work-words+ 16
  "Words of the work vector v: the chaining value and the IV."
  (= +work-words+ (* 2 +state-words+)))

(defmacro define-blake2f ()
  (let ((v (loop repeat +work-words+ collect (gensym))) (m (loop repeat +block-words+ collect (gensym))))
    `(defun compress2 (rounds h words t0 t1 final out)
       (declare #.clempo.base:*hot* (type (unsigned-byte 32) rounds)
                (type (simple-array (unsigned-byte 64) (8)) h out)
                (type (simple-array (unsigned-byte 64) (16)) words)
                (type (unsigned-byte 64) t0 t1) (type boolean final))
       (let (,@(loop for x in v for i below +work-words+
                     for initial = (if (< i 8) `(aref h ,i) (nth (- i 8) *iv64*))
                     collect `(,x ,(case i (12 `(logxor t0 ,initial)) (13 `(logxor t1 ,initial))
                                        (14 `(if final (logxor #xffffffffffffffff ,initial) ,initial))
                                        (t initial))))
             ,@(loop for x in m for i below +block-words+ collect `(,x (aref words ,i))))
         (declare (type (unsigned-byte 64) ,@v ,@m))
         (loop with remaining of-type (unsigned-byte 32) = rounds while (plusp remaining) do
           ,@(loop for sigma in *sigma2* append
                   `((when (zerop remaining) (return)) (decf remaining)
                     ,@(mix-round v m sigma 64 '(32 24 16 63)))))
         ,@(loop for i below +state-words+ collect
                 `(setf (aref out ,i) (logxor (aref h ,i) ,(nth i v) ,(nth (+ i 8) v)))))
       out)))
(define-blake2f)

(defun blake2f (rounds h m counters final)
  "Compress parsed EIP-152 words. FINAL is a boolean; COUNTERS contains two u64 words."
  (check-type rounds (unsigned-byte 32))
  (check-type h (simple-array (unsigned-byte 64) (8)))
  (check-type m (simple-array (unsigned-byte 64) (16)))
  (check-type counters (simple-array (unsigned-byte 64) (2)))
  (check-type final boolean)
  (compress2 rounds h m (aref counters 0) (aref counters 1) final
             (make-array +state-words+ :element-type '(unsigned-byte 64))))

(defmacro load-le (data start width)
  (let ((v (gensym)) (s (gensym)))
    `(let ((,v ,data) (,s ,start))
       (declare (type octets ,v) (type fixnum ,s))
       (logior ,@(loop for i below (/ width 8) collect `(ash (aref ,v (+ ,s ,i)) ,(* i 8)))))))

(defun blake2f-precompile (input)
  (check-type input octets)
  (when (and (= (length input) 213) (< (aref input 212) 2))
    (let ((h (make-array +state-words+ :element-type '(unsigned-byte 64)))
          (m (make-array +block-words+ :element-type '(unsigned-byte 64)))
          (out (make-octets 64)))
      (dotimes (i +state-words+) (setf (aref h i) (load-le input (+ 4 (* 8 i)) 64)))
      (dotimes (i +block-words+) (setf (aref m i) (load-le input (+ 68 (* 8 i)) 64)))
      (compress2 (logior (ash (aref input 0) 24) (ash (aref input 1) 16)
                         (ash (aref input 2) 8) (aref input 3))
                 h m (load-le input 196 64) (load-le input 204 64) (= (aref input 212) 1) h)
      (dotimes (i 64 out)
        (setf (aref out i) (ldb (byte 8 (* 8 (mod i 8))) (aref h (floor i 8))))))))

(in-package #:clempo.blake3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *permutation* '(2 6 3 10 7 0 4 13 1 11 12 5 9 14 15 8)
    "BLAKE3's message permutation between rounds."))

(defbound +cv-words+ 8
  "Words of a BLAKE3 chaining value; the work vector's third quarter starts as the IV."
  (= (length clempo.blake2f::*iv32*) +cv-words+)
  (= (* 4 +cv-words+) 32))

(defbound +block-words+ 16
  "Message words of a 64-octet BLAKE3 block, all of them moved by the permutation."
  (= (* 4 +block-words+) 64)
  (equal (sort (copy-list *permutation*) #'<) (loop for i below +block-words+ collect i)))

(defbound +work-words+ 16
  "Words of the work vector: the chaining value, four IV words, counter, size and flags;
a wide output's words."
  (= +work-words+ (+ +cv-words+ 4 2 1 1))
  (= +work-words+ (* 2 +cv-words+)))

(defbound +rounds+ 7
  "BLAKE3's rounds: BLAKE2s's mixing with three of its ten rounds removed."
  (= (+ +rounds+ 3) (length clempo.blake2f::*sigma2*)))

(defmacro define-blake3-compressor ()
  (let ((v (loop repeat +work-words+ collect (gensym))) (m (loop repeat +block-words+ collect (gensym)))
        (perm *permutation*))
    `(defun compress3 (cv data start counter size flags out wide)
       (declare #.clempo.base:*hot* (type octets data) (type fixnum start)
                (type (simple-array (unsigned-byte 32) (*)) cv out)
                (type (unsigned-byte 64) counter) (type (unsigned-byte 32) size flags))
       (let (,@(loop for x in v for i below +work-words+ collect
                     `(,x ,(cond ((< i 8) `(aref cv ,i))
                                 ((< i 12) (nth (- i 8) clempo.blake2f::*iv32*))
                                 (t (nth (- i 12) '((ldb (byte 32 0) counter) (ash counter -32) size flags))))))
             ,@(loop for x in m for i below +block-words+ collect
                     `(,x (clempo.blake2f::load-le data (+ start ,(* 4 i)) 32))))
         (declare (type (unsigned-byte 32) ,@v ,@m))
         ,@(loop repeat +rounds+ for indices = (loop for i below +block-words+ collect i)
                 then (mapcar (lambda (i) (nth i indices)) perm)
                 append (clempo.blake2f::mix-round v m indices 32 '(16 12 8 7)))
         (when wide
           ,@(loop for i below +cv-words+ collect `(setf (aref out ,(+ i 8)) (logxor (aref cv ,i) ,(nth (+ i 8) v)))))
         ,@(loop for i below +cv-words+ collect `(setf (aref out ,i) (logxor ,(nth i v) ,(nth (+ i 8) v)))))
       (values))))
(define-blake3-compressor)

(defun new-cv () (make-array +cv-words+ :element-type '(unsigned-byte 32)))

(defun serialize-words (words dst offset count)
  (declare #.clempo.base:*hot* (type (simple-array (unsigned-byte 32) (*)) words)
           (type octets dst) (type fixnum offset) (type (integer 0 64) count))
  (dotimes (i count)
    (setf (aref dst (+ offset i)) (ldb (byte 8 (ash (logand i 3) 3)) (aref words (ash i -2))))))

(defun key-words (data)
  (assert (= (length data) 32))
  (let ((w (new-cv)))
    (dotimes (i +cv-words+ w) (setf (aref w i) (clempo.blake2f::load-le data (* i 4) 32)))))

(defparameter *iv* (make-array +cv-words+ :element-type '(unsigned-byte 32)
                               :initial-contents clempo.blake2f::*iv32*))

(defun hash-tree (data key flags length)
  (check-type data octets)
  (check-type length (integer 0 #.array-dimension-limit))
  (let* ((n (length data)) (chunks (max 1 (ceiling n 1024)))
         (stack (make-array (integer-length chunks) :initial-element nil)) (depth 0)
         (cv (new-cv)) (scratch (make-octets 64)) (root-words (make-array +work-words+ :element-type '(unsigned-byte 32)))
         (last-data data) (last-start 0) (last-size 0) (last-flags 0) (last-key cv))
    (labels ((parent (left)
               (serialize-words left scratch 0 32)
               (serialize-words cv scratch 32 32)
               (setf last-key key last-data scratch last-start 0 last-size 64 last-flags (logior flags 4))))
      (dotimes (chunk chunks)
        (replace cv key)
        (let* ((start (* chunk 1024)) (end (min n (+ start 1024)))
               (blocks (max 1 (ceiling (- end start) 64))))
          (dotimes (block (1- blocks))
            (compress3 cv data (+ start (* block 64)) chunk 64 (logior flags (if (zerop block) 1 0)) cv nil))
          (setf last-start (+ start (* (1- blocks) 64)) last-size (- end last-start)
                last-data data last-flags (logior flags 2 (if (= blocks 1) 1 0)) last-key cv)
          (when (< last-size 64)
            (fill scratch 0 :start last-size)
            (replace scratch data :start2 last-start :end2 end)
            (setf last-data scratch last-start 0))
          (cond ((= chunk (1- chunks))
                 (unless (zerop depth)
                   (compress3 cv last-data last-start chunk last-size last-flags cv nil)
                   (loop do (parent (aref stack (decf depth))) while (plusp depth)
                         do (compress3 key scratch 0 0 64 last-flags cv nil))))
                (t
                 (compress3 cv last-data last-start chunk last-size last-flags cv nil)
                 (loop for count = (1+ chunk) then (ash count -1) while (evenp count) do
                   (parent (aref stack (decf depth)))
                   (compress3 key scratch 0 0 64 last-flags cv nil))
                 (unless (aref stack depth) (setf (aref stack depth) (new-cv)))
                 (replace (aref stack depth) cv)
                 (incf depth))))))
    (let ((out (make-octets length)))
      (loop for start from 0 below length by 64 for counter from 0 do
        (compress3 last-key last-data last-start counter last-size (logior last-flags 8) root-words t)
        (serialize-words root-words out start (min 64 (- length start))))
      out)))

(defun blake3 (data &key (length 32)) (hash-tree data *iv* 0 length))
(defun blake3-keyed (key data &key (length 32)) (hash-tree data (key-words key) 16 length))
(defun blake3-derive-key (context data &key (length 32))
  (hash-tree data (key-words (hash-tree (string->octets context) *iv* 32 32)) 64 length))
