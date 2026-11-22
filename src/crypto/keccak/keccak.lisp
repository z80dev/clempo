(defpackage #:clempo.keccak
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak-arm64
                #:+lane-bytes+ #:+lanes+ #:+plane-lanes+ #:+rounds+ #:+digest-lanes+ #:+rate-lanes+)
  (:documentation "Keccak-256 (the pre-SHA3 padding Ethereum uses).")
  (:export #:keccak256 #:keccak256-integer #:keccak256-concat
           #:make-keccak #:keccak-update #:keccak-final
           #:keccak-reset #:keccak256-into
           #:copy-keccak-into #:keccak-digest))

(in-package #:clempo.keccak)

(deftype lane () '(unsigned-byte 64))
(deftype keccak-state () `(simple-array (unsigned-byte 64) (,+lanes+)))

(define-constant +rate+ 136)
(static-assert (= +rate+ (* +rate-lanes+ +lane-bytes+)))

(define-constant +round-constants+
  (make-array +rounds+ :element-type '(unsigned-byte 64)
                 :initial-contents
                 '(#x0000000000000001 #x0000000000008082 #x800000000000808A #x8000000080008000
                   #x000000000000808B #x0000000080000001 #x8000000080008081 #x8000000000008009
                   #x000000000000008A #x0000000000000088 #x0000000080008009 #x000000008000000A
                   #x000000008000808B #x800000000000008B #x8000000000008089 #x8000000000008003
                   #x8000000000008002 #x8000000000000080 #x000000000000800A #x800000008000000A
                   #x8000000080008081 #x8000000000008080 #x0000000080000001 #x8000000080008008)))
(static-assert (= (length +round-constants+) +rounds+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Rotation offsets indexed by x + 5y.
  (defparameter *rotations*
    #(0 1 62 28 27  36 44 6 55 20  3 10 43 25 39  41 45 15 21 8  18 2 61 56 14))
  (static-assert (= (length *rotations*) +lanes+))

  (defun lane-symbols (prefix n)
    (loop for i below n collect (intern (format nil "~A~D" prefix i))))

  (defun rol-form (x n)
    (if (zerop n)
        x
        `(logior (ldb (byte 64 0) (ash ,x ,n)) (ash ,x ,(- n 64))))))

(defmacro keccak-rounds (state &optional (words nil) data start out out-start)
  "Expands to 24 rounds of Keccak-f[1600] over 25 lane variables, fully unrolled
within a round: theta, rho, pi and chi become straight-line register code."
  (let ((a (lane-symbols "A" +lanes+))
        (b (lane-symbols "B" +lanes+))
        (c (lane-symbols "C" +plane-lanes+))
        (d (lane-symbols "D" +plane-lanes+)))
    (flet ((a (x y) (nth (+ x (* +plane-lanes+ y)) a))
           (b (x y) (nth (+ x (* +plane-lanes+ y)) b)))
      `(let (,@(loop for reg in a for i from 0
                     collect `(,reg ,(cond ((not words) `(aref ,state ,i))
                                           ((< i words) `(load-lane ,data (+ ,start ,(* i +lane-bytes+))))
                                           ((= i words) 1)
                                           ((= i (1- +rate-lanes+)) #x8000000000000000)
                                           (t 0)))))
         (declare (type lane ,@a))
         (dotimes (round +rounds+)
           (let* (,@(loop for x below +plane-lanes+
                          collect `(,(nth x c) (logxor ,@(loop for y below +plane-lanes+ collect (a x y)))))
                  ,@(loop for x below +plane-lanes+
                          collect `(,(nth x d)
                                    (logxor ,(nth (mod (+ x 4) 5) c)
                                            ,(rol-form (nth (mod (1+ x) 5) c) 1))))
                  ,@(loop for x below +plane-lanes+
                          nconc (loop for y below +plane-lanes+
                                      collect `(,(b y (mod (+ (* 2 x) (* 3 y)) 5))
                                                ,(rol-form `(logxor ,(a x y) ,(nth x d))
                                                           (aref *rotations* (+ x (* +plane-lanes+ y))))))))
             (declare (type lane ,@c ,@d ,@b))
             (setf ,@(loop for y below +plane-lanes+
                           nconc (loop for x below +plane-lanes+
                                       nconc `(,(a x y)
                                               (logxor ,(b x y)
                                                       (logandc1 ,(b (mod (1+ x) 5) y)
                                                                 ,(b (mod (+ x 2) 5) y)))))))
             (setf ,(a 0 0) (logxor ,(a 0 0) (aref +round-constants+ round)))))
         ,@(if words
               (loop for reg in a for i below +digest-lanes+ append
                     (loop for j below +lane-bytes+ collect
                           `(setf (aref ,out (+ ,out-start ,(+ (* +lane-bytes+ i) j)))
                                  (ldb (byte 8 ,(* 8 j)) ,reg))))
               `((setf ,@(loop for reg in a for i from 0 nconc `((aref ,state ,i) ,reg)))
                 ,state))))))

;;; The portable permutation and fixed-size hashes are compiled on every host, so every
;;; host's tests run them against the vectors; arm64 cores with FEAT_SHA3 use the
;;; instructions instead (clempo.keccak-arm64:**native**).
(defun keccak-f-portable (state)
  (declare (type keccak-state state) #.*hot*)
  (keccak-rounds state))

#+arm64
(clempo.keccak-arm64:define-arm64-keccak *rotations*)

(defun keccak-f (state)
  (declare (type keccak-state state) #.*hot*)
  #+arm64 (if clempo.keccak-arm64:**native**
              (progn (clempo.keccak-arm64:permute state +round-constants+) state)
              (keccak-f-portable state))
  #-arm64 (keccak-f-portable state))

(defstruct (keccak (:constructor %make-keccak))
  (state (make-array +lanes+ :element-type '(unsigned-byte 64) :initial-element 0) :type keccak-state)
  (buffer (make-octets +rate+) :type (octets 136))
  (fill 0 :type fixnum))

(defun make-keccak () (%make-keccak))

(defun keccak-reset (k)
  "Reset K to its initial state without replacing its storage. Return K."
  (declare (type keccak k) #.*hot*)
  (fill (keccak-state k) 0)
  (setf (keccak-fill k) 0)
  k)

(defmacro load-lane (data start)
  (let ((offset (gensym)))
    `(let ((,offset ,start))
       (declare (type fixnum ,offset))
       (logior ,@(loop for i below +lane-bytes+ collect `(ash (aref ,data (+ ,offset ,i)) ,(* i 8)))))))

(declaim (inline absorb-block))
(defun absorb-block (state data start)
  (declare (type keccak-state state) (type octets data) (type fixnum start) #.*hot*)
  (dotimes (lane +rate-lanes+)
    (setf (aref state lane) (logxor (aref state lane) (load-lane data (+ start (* lane +lane-bytes+))))))
  (keccak-f state))

(defun keccak-update (k data &key (start 0) (end (length data)))
  (declare (type keccak k) (type octets data) (type fixnum start end) #.*hot*)
  (let ((state (keccak-state k))
        (buf (keccak-buffer k))
        (used (keccak-fill k))
        (pos start))
    (declare (type fixnum used pos))
    (when (plusp used)
      (let ((n (min (- 136 used) (- end pos))))
        (replace buf data :start1 used :start2 pos :end2 (+ pos n))
        (incf used n)
        (incf pos n)
        (when (= used 136)
          (absorb-block state buf 0)
          (setf used 0))))
    (loop while (<= (+ pos 136) end)
          do (absorb-block state data pos)
             (incf pos 136))
    (when (< pos end)
      (replace buf data :start1 0 :start2 pos :end2 end)
      (setf used (- end pos)))
    (setf (keccak-fill k) used)
    k))

(defun squeeze-into (state out out-start)
  (declare (type keccak-state state) (type octets out) (type fixnum out-start) #.*hot*)
  (dotimes (lane +digest-lanes+ out)
    (let ((w (aref state lane)))
      (dotimes (j +lane-bytes+)
        (setf (aref out (+ out-start (* lane +lane-bytes+) j)) (ldb (byte 8 (* 8 j)) w))))))

(defun finalize-into (k out out-start)
  (declare (type keccak k) (type octets out) (type fixnum out-start) #.*hot*)
  (let ((buf (keccak-buffer k))
        (used (keccak-fill k))
        (state (keccak-state k)))
    (fill buf 0 :start used)
    (setf (aref buf used) (logxor (aref buf used) #x01))
    (setf (aref buf 135) (logxor (aref buf 135) #x80))
    (absorb-block state buf 0)
    (squeeze-into state out out-start)))

(defun keccak-final (k)
  "Pad, squeeze 32 bytes. Reset K before reusing it."
  (finalize-into k (make-octets 32) 0))

(declaim (inline fixed-into-portable))
(defun fixed-into-portable (data start size out out-start)
  (declare (type octets data out) (type fixnum start out-start) (type (member 32 64) size) #.*hot*)
  (if (= size 32)
      (keccak-rounds nil 4 data start out out-start)
      (keccak-rounds nil 8 data start out out-start))
  out)

(defun fixed-into (data start size out out-start)
  (declare (type octets data out) (type fixnum start out-start) (type (member 32 64) size) #.*hot*)
  #+arm64 (if clempo.keccak-arm64:**native**
              (progn (if (= size 32)
                         (clempo.keccak-arm64:hash32-into data start out out-start +round-constants+)
                         (clempo.keccak-arm64:hash64-into data start out out-start +round-constants+))
                     out)
              (fixed-into-portable data start size out out-start))
  #-arm64 (fixed-into-portable data start size out out-start))

(defun keccak256-into (data start end out out-start &optional k)
  "Hash DATA[START,END) into OUT at OUT-START and return OUT.
32- and 64-byte inputs need no scratch. Other lengths allocate nothing with reusable K.
K's previous digest state is discarded."
  (check-type data octets)
  (check-type out octets)
  (check-type k (or null keccak))
  (assert (and (typep start 'fixnum) (typep end 'fixnum) (<= 0 start end (length data))))
  (assert (and (typep out-start 'fixnum) (<= 0 out-start (- (length out) 32))))
  (let ((size (- end start)))
    (cond ((or (= size 32) (= size 64))
           (when k (keccak-reset k))
           (fixed-into data start size out out-start))
          (k
           (keccak-reset k)
           (keccak-update k data :start start :end end)
           (finalize-into k out out-start))
          (t
           ;; a context on the stack: receipts, headers and trie nodes hash here every block
           (let* ((state (make-array +lanes+ :element-type '(unsigned-byte 64) :initial-element 0))
                  (buffer (make-array +rate+ :element-type '(unsigned-byte 8) :initial-element 0))
                  (k (%make-keccak :state state :buffer buffer)))
             (declare (dynamic-extent state buffer k))
             (keccak-update k data :start start :end end)
             (finalize-into k out out-start))))))

(defun keccak256 (data &key (start 0) (end (length data)))
  "Keccak-256 of DATA[START,END) as 32 octets."
  (declare (type octets data))
  (keccak256-into data start end (make-octets 32) 0))

(defun keccak256-concat (&rest parts)
  (let ((k (%make-keccak)))
    (dolist (p parts) (keccak-update k p))
    (keccak-final k)))

(defun keccak256-integer (data &key (start 0) (end (length data)))
  (octets->integer (keccak256 data :start start :end end)))

(defun copy-keccak-into (from to)
  "Overwrite TO with FROM's absorbed state. Returns TO."
  (declare (type keccak from to) #.*hot*)
  (replace (keccak-state to) (keccak-state from))
  (replace (keccak-buffer to) (keccak-buffer from))
  (setf (keccak-fill to) (keccak-fill from))
  to)

(defun keccak-digest (k &optional (scratch (%make-keccak)))
  "The digest of everything absorbed into K so far, leaving K usable (running MACs).
SCRATCH is overwritten; pass a reusable one to avoid allocating per call."
  (declare (type keccak k scratch))
  (keccak-final (copy-keccak-into k scratch)))
