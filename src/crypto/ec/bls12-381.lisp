;;; BLS12-381 package, integer boundaries, canonical encodings and precompile wrappers.
(defpackage #:clempo.bls12-381
  (:use #:cl)
  (:import-from #:clempo.base #:make-octets #:integer->octets #:octets->integer
                #:octets-concat)
  (:export #:+p+ #:+r+ #:invalid-point #:g1-generator #:g2-generator
           #:g1-add #:g2-add #:g1-neg #:g2-neg #:g1-mul #:g2-mul
           #:g1-msm #:g2-msm #:g1-on-curve-p #:g2-on-curve-p
           #:g1-subgroup-p #:g2-subgroup-p #:g1-compress #:g2-compress
           #:g1-decompress #:g2-decompress #:g1-encode #:g2-encode
           #:g1-decode #:g2-decode #:pairing-check
           #:g1-add-precompile #:g2-add-precompile #:g1-msm-precompile
           #:g2-msm-precompile #:pairing-precompile
           #:expand-message-xmd #:map-fp-to-g1 #:map-fp2-to-g2 #:hash-to-g1 #:hash-to-g2
           #:map-g1-precompile #:map-g2-precompile
           #:bls-public-key #:bls-sign #:bls-verify #:bls-pop-prove #:bls-pop-verify
           #:bls-aggregate #:bls-aggregate-verify #:bls-fast-aggregate-verify
           #:bls-batch-verify #:bls-batch-verify-same-message #:bls-union-unique
           #:bls-batch-verify-same-signer
           #:bls-participant-scalar #:bls-partial-public-key #:bls-threshold-sign
           #:bls-threshold-verify #:bls-threshold-recover #:bls-threshold-pop-prove
           #:bls-threshold-pop-verify))
(in-package #:clempo.bls12-381)

(defconstant +p+ #x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab)
(defconstant +r+ #x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001)
(define-condition invalid-point (error) ())
(defun invalid () (error 'invalid-point))

(defun fp-pow (base exponent)
  ;; Integer boundary for the declarative hash-to-curve formulas. The exponent
  ;; loop itself uses the raw Montgomery kernel and a stack-local arena.
  (let ((arena (make-array 30 :element-type '(unsigned-byte 64) :initial-element 0)))
    (declare (dynamic-extent arena))
    (fp-from-integer! arena 0 base)
    (fp-pow! arena 0 arena 0 exponent arena 6)
    (fp-to-integer arena 0)))
(defun fp-inv (n)
  (let ((arena (make-array 30 :element-type '(unsigned-byte 64) :initial-element 0)))
    (declare (dynamic-extent arena))
    (fp-from-integer! arena 0 n)
    (fp-inv! arena 0 arena 0 arena 6)
    (fp-to-integer arena 0)))
(defun fp-sqrt (n)
  (let ((root (fp-pow n (ash (1+ +p+) -2))))
    (when (= (mod (* root root) +p+) (mod n +p+)) root)))
(defun fp+ (a b) (mod (+ a b) +p+))
(defun fp- (a b) (mod (- a b) +p+))
(defun fp* (a b) (mod (* a b) +p+))
(defun fp-neg (a) (mod (- a) +p+))
(defun f2+ (a b) (cons (fp+ (car a) (car b)) (fp+ (cdr a) (cdr b))))
(defun f2- (a b) (cons (fp- (car a) (car b)) (fp- (cdr a) (cdr b))))
(defun f2-neg (a) (cons (fp-neg (car a)) (fp-neg (cdr a))))
(defun f2* (a b)
  (let ((ac (* (car a) (car b))) (bd (* (cdr a) (cdr b))))
    (cons (mod (- ac bd) +p+)
          (mod (- (* (+ (car a) (cdr a)) (+ (car b) (cdr b))) ac bd) +p+))))
(defun f2-scale (a n) (cons (fp* (car a) n) (fp* (cdr a) n)))
(defun f2-inv (a)
  (let ((inv (fp-inv (+ (* (car a) (car a)) (* (cdr a) (cdr a))))))
    (cons (fp* (car a) inv) (fp* (- (cdr a)) inv))))
(defun f2/ (a b) (f2* a (f2-inv b)))
(defun f2-pow (base exponent)
  (let ((result (cons 1 0)))
    (loop while (plusp exponent) do
      (when (oddp exponent) (setf result (f2* result base)))
      (setf exponent (ash exponent -1))
      (when (plusp exponent) (setf base (f2* base base))))
    result))
(defun f2-integer-zero-p (a) (and (zerop (car a)) (zerop (cdr a))))
(defun f2-sqrt (a)
  (cond ((f2-integer-zero-p a) (cons 0 0))
        ((zerop (cdr a))
         (let ((r (fp-sqrt (car a))))
           (if r (cons r 0) (let ((i (fp-sqrt (- (car a)))))
                             (when i (cons 0 i))))))
        (t (let ((norm (fp-sqrt (+ (* (car a) (car a)) (* (cdr a) (cdr a))))))
             (when norm
               (let ((x (or (fp-sqrt (fp* (+ (car a) norm) (ash (1+ +p+) -1)))
                            (fp-sqrt (fp* (- (car a) norm) (ash (1+ +p+) -1))))))
                 (when (and x (not (zerop x)))
                   (let ((root (cons x (fp* (cdr a) (fp-inv (* 2 x))))))
                     (when (equal (f2* root root) a) root)))))))))


(defun g1-generator ()
  (cons #x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
        #x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1))
(defun g2-generator ()
  (cons (cons #x024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
              #x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
        (cons #x0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
              #x0606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)))

(defun fp-canonical-p (x) (and (integerp x) (<= 0 x) (< x +p+)))
(defun f2-canonical-p (x)
  (and (consp x) (fp-canonical-p (car x)) (fp-canonical-p (cdr x))))
(defun g1-valid-p (p &optional subgroup)
  (and (or (null p) (and (consp p) (fp-canonical-p (car p))
                         (fp-canonical-p (cdr p)) (g1-on-curve-p p)))
       (or (not subgroup) (g1-subgroup-p p))))
(defun g2-valid-p (p &optional subgroup)
  (and (or (null p) (and (consp p) (f2-canonical-p (car p))
                         (f2-canonical-p (cdr p)) (g2-on-curve-p p)))
       (or (not subgroup) (g2-subgroup-p p))))
(defun expect-length (bytes n)
  (unless (and (typep bytes '(array (unsigned-byte 8) (*))) (= (length bytes) n)) (invalid)))
(defmacro define-bls-integer-reader (name size)
  `(defun ,name (bytes start)
     (declare #.clempo.base:*hot* (type clempo.base:octets bytes) (type fixnum start))
     (unless (<= 0 start (+ start ,size) (length bytes)) (invalid))
     (let ((out (sb-bignum::%allocate-bignum ,(1+ (/ size 8)))))
       ,@(loop for limb below (/ size 8)
               for offset = (- size (* (1+ limb) 8))
               collect
          `(setf (sb-bignum::%bignum-ref out ,limb)
                 (logior ,@(loop for j below 8 collect
                             `(ash (aref bytes (+ start ,(+ offset j))) ,(* (- 7 j) 8))))))
       (setf (sb-bignum::%bignum-ref out ,(/ size 8)) 0)
       (sb-bignum::%normalize-bignum out ,(1+ (/ size 8))))))
(define-bls-integer-reader read-bls-integer32 32)
(define-bls-integer-reader read-bls-integer48 48)

(defun decode-fp (bytes start &optional (width 64))
  (unless (<= 0 start (+ start width) (length bytes)) (invalid))
  (ecase width
    (64 (dotimes (i 16) (unless (zerop (aref bytes (+ start i))) (invalid)))
        (incf start 16))
    (48))
  (let ((x (read-bls-integer48 bytes start)))
    (unless (< x +p+) (invalid)) x))
(defun g1-encode (p)
  (if p (octets-concat (integer->octets (car p) 64) (integer->octets (cdr p) 64))
      (make-octets 128)))
(defun g2-encode (p)
  (if p (octets-concat (integer->octets (caar p) 64) (integer->octets (cdar p) 64)
                       (integer->octets (cadr p) 64) (integer->octets (cddr p) 64))
      (make-octets 256)))
(defun g1-decode (bytes &key (subgroup t) (start 0))
  (unless (<= (+ start 128) (length bytes)) (invalid))
  (let* ((x (decode-fp bytes start)) (y (decode-fp bytes (+ start 64)))
         (p (unless (and (zerop x) (zerop y)) (cons x y))))
    (unless (g1-valid-p p subgroup) (invalid)) p))
(defun g2-decode (bytes &key (subgroup t) (start 0))
  (unless (<= (+ start 256) (length bytes)) (invalid))
  (let* ((x (cons (decode-fp bytes start) (decode-fp bytes (+ start 64))))
         (y (cons (decode-fp bytes (+ start 128)) (decode-fp bytes (+ start 192))))
         (p (unless (and (f2-integer-zero-p x) (f2-integer-zero-p y)) (cons x y))))
    (unless (g2-valid-p p subgroup) (invalid)) p))
(defun f2-large-p (a)
  (> (if (zerop (cdr a)) (car a) (cdr a)) (ash +p+ -1)))
(defun g1-compress (p)
  (let ((out (if p (integer->octets (car p) 48) (make-octets 48))))
    (setf (aref out 0) (logior (aref out 0) #x80
                              (if p (if (> (cdr p) (ash +p+ -1)) #x20 0) #x40))) out))
(defun g2-compress (p)
  (let ((out (if p (octets-concat (integer->octets (cdar p) 48)
                                  (integer->octets (caar p) 48)) (make-octets 96))))
    (setf (aref out 0) (logior (aref out 0) #x80
                              (if p (if (f2-large-p (cdr p)) #x20 0) #x40))) out))
(defun compressed-x (bytes n)
  (expect-length bytes n)
  (unless (logbitp 7 (aref bytes 0)) (invalid))
  (let ((payload (copy-seq bytes)))
    (setf (aref payload 0) (logand (aref payload 0) #x1f))
    (if (logbitp 6 (aref bytes 0))
        (progn (unless (and (not (logbitp 5 (aref bytes 0)))
                            (every #'zerop payload)) (invalid))
               (values nil t))
        (values payload nil))))
(defun g1-decompress (bytes)
  (multiple-value-bind (payload infinity) (compressed-x bytes 48)
    (unless infinity
      (let* ((x (decode-fp payload 0 48)) (y (fp-sqrt (+ (* x x x) 4))))
        (unless y (invalid))
        (unless (eql (> y (ash +p+ -1)) (logbitp 5 (aref bytes 0)))
          (setf y (fp-neg y)))
        (let ((p (cons x y))) (unless (g1-subgroup-p p) (invalid)) p)))))
(defun g2-decompress (bytes)
  (multiple-value-bind (payload infinity) (compressed-x bytes 96)
    (unless infinity
      (let* ((x (cons (decode-fp payload 48 48) (decode-fp payload 0 48)))
             (y (f2-sqrt (f2+ (f2* x (f2* x x)) '(4 . 4)))))
        (unless y (invalid))
        (unless (eql (f2-large-p y) (logbitp 5 (aref bytes 0))) (setf y (f2-neg y)))
        (let ((p (cons x y))) (unless (g2-subgroup-p p) (invalid)) p)))))


(defmacro define-precompile-add (name size decode encode add)
  `(defun ,name (input)
     (handler-case
         (progn (expect-length input (* 2 ,size))
                (values (,encode (,add (,decode input :subgroup nil)
                                       (,decode input :start ,size :subgroup nil))) t))
       (invalid-point () (values nil nil)))))
(define-precompile-add g1-add-precompile 128 g1-decode g1-encode g1-add)
(define-precompile-add g2-add-precompile 256 g2-decode g2-encode g2-add)
(defmacro define-precompile-msm (name size decode encode msm)
  `(defun ,name (input)
     (handler-case
         (let ((stride (+ ,size 32)) (points nil) (scalars nil))
           (when (or (zerop (length input)) (not (zerop (mod (length input) stride)))) (invalid))
           (loop for i from 0 below (length input) by stride do
             (push (,decode input :start i) points)
             (push (read-bls-integer32 input (+ i ,size)) scalars))
           (values (,encode (,msm points scalars)) t))
       (invalid-point () (values nil nil)))))
(define-precompile-msm g1-msm-precompile 128 g1-decode g1-encode g1-msm)
(define-precompile-msm g2-msm-precompile 256 g2-decode g2-encode g2-msm)
(defun pairing-precompile (input)
  (multiple-value-bind (result valid-p) (pairing-check input)
    (values (when valid-p (integer->octets (if result 1 0) 32)) valid-p)))
