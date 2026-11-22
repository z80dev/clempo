(defpackage #:clempo.sha2
  (:use #:cl #:clempo.base)
  (:export #:sha256 #:sha384 #:sha512 #:make-sha256 #:make-sha512 #:sha256-update
           #:sha512-update #:sha256-final #:sha512-final #:hmac-sha256 #:hmac-sha512
           #:hkdf-sha256 #:hkdf-sha256-extract #:hkdf-sha256-expand))
(in-package #:clempo.sha2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIPS 180-4 constants are the fractional square/cube roots of primes.
  ;; Compute them exactly at macro expansion, not with host floating point.
  (defun root-constants (count degree)
    (loop for p from 2
          when (loop for d from 2 to (isqrt p) never (zerop (rem p d)))
            collect (let ((n (ash p (* degree 64))) (lo 0) (hi (ash 1 72)))
                      (loop while (> (- hi lo) 1) for mid = (ash (+ hi lo) -1)
                            do (if (<= (expt mid degree) n) (setf lo mid) (setf hi mid)))
                      (ldb (byte 64 0) lo)) into constants
          when (= (length constants) count) return constants)))

(defbound +state-words+ 8
  "Words of the SHA-2 state: a SHA-256 or SHA-512 digest of eight words."
  (= (* 32 +state-words+) 256)
  (= (* 64 +state-words+) 512))

(defbound +block-words+ 16
  "Words of a message block (64 or 128 octets); the schedule's first words are these."
  (= (* 4 +block-words+) 64)
  (= (* 8 +block-words+) 128))

(defmacro define-sha (make update final hash compress context width rounds length-size
                      big0 big1 small0 small1)
  "The portable compression is COMPRESS-PORTABLE on every host (tests run it directly);
for SHA-256 on an arm64 core with FEAT_SHA256, COMPRESS runs clempo.sha256-arm64 instead."
  (let* ((word `(unsigned-byte ,width)) (bytes (/ width 8)) (block (* +block-words+ bytes))
         (init (mapcar (lambda (n) (ash n (- width 64))) (root-constants +state-words+ 2)))
         (ks (mapcar (lambda (n) (ash n (- width 64))) (root-constants rounds 3)))
         (st (intern (format nil "~A-STATE" context)))
         (buf (intern (format nil "~A-BUFFER" context)))
         (total (intern (format nil "~A-TOTAL" context)))
         (done (intern (format nil "~A-DONE" context)))
         (constructor (intern (format nil "%~A" make)))
         (final-size (intern (format nil "%~A" final)))
         (portable (intern (format nil "~A-PORTABLE" compress)))
         (regs (loop repeat +state-words+ collect (gensym)))
         (ws (loop repeat +block-words+ collect (gensym))))
    (labels ((add (&rest args) `(ldb (byte ,width 0) (+ ,@args)))
             (sigma (x shifts shift-p)
               `(logxor ,@(loop for n in shifts for i from 1
                                collect (if (and shift-p (= i 3)) `(ash ,x ,(- n))
                                            `(logior (ash ,x ,(- n))
                                                     (ldb (byte ,width 0) (ash ,x ,(- width n)))))))))
      `(progn
         (defstruct (,context (:constructor ,constructor (&optional state)))
           (state (make-array +state-words+ :element-type ',word :initial-contents ',init)
                  :type (simple-array ,word (8)))
           (buffer (make-octets ,block) :type octets)
           (total 0 :type (integer 0))
           (done nil :type boolean))
         (defun ,make () (,constructor))
         (declaim (inline ,portable))
         (defun ,portable (state data offset)
           (declare #.clempo.base:*hot* (type octets data) (type fixnum offset)
                    (type (simple-array ,word (8)) state))
           (let (,@(loop for r in regs for i from 0 collect `(,r (aref state ,i)))
                 ,@(loop for w in ws for i from 0
                         collect `(,w (logior ,@(loop for j below bytes
                                                     collect `(ash (aref data (+ offset ,(+ (* bytes i) j)))
                                                                   ,(* 8 (- bytes j 1))))))))
             (declare (type ,word ,@regs ,@ws))
             ,@(loop for k in ks for i from 0
                     for w = (nth (mod i 16) ws)
                     for cur = (append (last regs (mod i 8)) (butlast regs (mod i 8)))
                     append
                     (destructuring-bind (a b c d e f g h) cur
                       (let ((u (gensym)) (v (gensym)))
                         `(,@(when (>= i 16)
                               `((setf ,w ,(add w (sigma (nth (mod (- i 15) 16) ws) small0 t)
                                                (nth (mod (- i 7) 16) ws)
                                                (sigma (nth (mod (- i 2) 16) ws) small1 t)))))
                           (let ((,u ,(add (add h k w)
                                           (add (sigma e big1 nil)
                                                `(logior (logand ,e ,f) (logandc2 ,g ,e)))))
                                 (,v ,(add (sigma a big0 nil) `(logxor (logand ,a ,b) (logand ,c (logxor ,a ,b))))))
                             (setf ,d ,(add d u) ,h ,(add u v)))))))
             ,@(loop for r in regs for i from 0 collect `(setf (aref state ,i) ,(add `(aref state ,i) r))))
           (values))
         (defun ,compress (state data offset)
           (declare #.clempo.base:*hot* (type octets data) (type fixnum offset)
                    (type (simple-array ,word (8)) state))
           ,(if (= width 32)
                #+arm64
                `(if clempo.sha256-arm64:**native**
                     (clempo.sha256-arm64:compress
                      state data offset
                      (load-time-value
                       (make-array ,rounds :element-type '(unsigned-byte 32) :initial-contents ',ks) t))
                     (,portable state data offset))
                #-arm64
                `(,portable state data offset)
                `(,portable state data offset)))
         (defun ,update (ctx data &key (start 0) (end (length data)))
           (declare (type octets data))
           (check-type ctx ,context)
           (assert (and (not (,done ctx)) (typep start 'fixnum) (typep end 'fixnum)
                        (<= 0 start end (length data))))
           (let* ((used (mod (,total ctx) ,block)) (buffer (,buf ctx)) (state (,st ctx)))
             (incf (,total ctx) (- end start))
             (unless (zerop used)
               (let ((n (min (- end start) (- ,block used))))
                 (replace buffer data :start1 used :start2 start :end2 (+ start n))
                 (incf start n)
                 (unless (= (+ used n) ,block) (return-from ,update ctx))
                 (,compress state buffer 0)))
             (loop while (<= (+ start ,block) end)
                   do (,compress state data start) (incf start ,block))
             (replace buffer data :start2 start :end2 end))
           ctx)
         (defun ,final-size (ctx words)
           (check-type ctx ,context)
           (assert (not (,done ctx)))
           (let* ((total (,total ctx)) (used (mod total ,block)) (buffer (,buf ctx))
                  (state (,st ctx)) (out (make-octets (* words ,bytes))))
             (setf (,done ctx) t (aref buffer used) #x80)
             (fill buffer 0 :start (1+ used))
             (when (>= used ,(- block length-size))
               (,compress state buffer 0) (fill buffer 0))
             (let ((bits (* total 8)))
               (loop for i below ,length-size do
                 (setf (aref buffer (- ,(1- block) i)) (ldb (byte 8 (* i 8)) bits))))
             (,compress state buffer 0)
             (dotimes (i words out)
               (dotimes (j ,bytes)
                 (setf (aref out (+ (* i ,bytes) j))
                       (ldb (byte 8 (* 8 (- ,(1- bytes) j))) (aref state i)))))))
         (defun ,final (ctx) (,final-size ctx 8))
         (defun ,hash (data &key (start 0) (end (length data)))
           (,final (,update (,make) data :start start :end end)))))))

(define-sha make-sha256 sha256-update sha256-final sha256 compress256 sha256-context
  32 64 8 (2 13 22) (6 11 25) (7 18 3) (17 19 10))
(define-sha make-sha512 sha512-update sha512-final sha512 compress512 sha512-context
  64 80 16 (28 34 39) (14 18 41) (1 8 7) (19 61 6))

(defun sha384 (data &key (start 0) (end (length data)))
  "FIPS 180-4 SHA-384: SHA-512 compression with a distinct IV and six output words."
  (let ((ctx (%make-sha512
              (make-array 8 :element-type '(unsigned-byte 64)
                          :initial-contents '(#xcbbb9d5dc1059ed8 #x629a292a367cd507
                                              #x9159015a3070dd17 #x152fecd8f70e5939
                                              #x67332667ffc00b31 #x8eb44a8768581511
                                              #xdb0c2e0d64f98fa7 #x47b5481dbefa4fa4)))))
    (%sha512-final (sha512-update ctx data :start start :end end) 6)))

(defmacro define-hmac (name make update final hash block)
  `(defun ,name (key data &rest more)
     (declare (dynamic-extent more))
     (let* ((key (if (> (length key) ,block) (,hash key) key))
            (pad (fill (make-octets ,block) #x36))
            (inner (,make)) (outer (,make)))
       (dotimes (i (length key)) (setf (aref pad i) (logxor #x36 (aref key i))))
       (,update inner pad)
       (,update inner data)
       (dolist (part more) (,update inner part))
       (dotimes (i ,block) (setf (aref pad i) (logxor #x6a (aref pad i))))
       (,update outer pad)
       ;; The pads and the inner digest derive from the key: zero them once used.
       (let ((digest (,final inner)))
         (fill pad 0)
         (prog1 (,final (,update outer digest)) (fill digest 0))))))

(define-hmac hmac-sha256 make-sha256 sha256-update sha256-final sha256 64)
(define-hmac hmac-sha512 make-sha512 sha512-update sha512-final sha512 128)

(defun hkdf-sha256-extract (salt ikm)
  (hmac-sha256 salt ikm))

(defun hkdf-sha256-expand (prk info length)
  (check-type length (integer 0 8160))
  (let ((out (make-octets length)) (previous (make-octets 0)) (counter (make-octets 1)))
    (loop for offset from 0 below length by 32 for i from 1 do
      (setf (aref counter 0) i previous (hmac-sha256 prk previous info counter))
      (replace out previous :start1 offset))
    out))

(defun hkdf-sha256 (salt ikm info length)
  (hkdf-sha256-expand (hkdf-sha256-extract salt ikm) info length))
