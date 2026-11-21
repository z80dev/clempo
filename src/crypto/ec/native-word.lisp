;;; One word contract for all EC kernels. Optional SBCL intrinsics are found,
;;; never read as package-qualified symbols that could intern into a locked package.
(defpackage #:clempo.ec.word
  (:use #:cl)
  (:export #:add-with-carry #:subtract-with-borrow #:multiply-and-add))
(in-package #:clempo.ec.word)

(deftype word () '(unsigned-byte 64))
(declaim (inline portable-add-with-carry portable-subtract-with-borrow
                 portable-multiply-and-add))

(defun portable-add-with-carry (a b carry)
  (declare #.clempo.base:*hot* (type word a b) (type bit carry))
  (let* ((low (+ (ldb (byte 32 0) a) (ldb (byte 32 0) b) carry))
         (high (+ (ldb (byte 32 32) a) (ldb (byte 32 32) b) (ash low -32))))
    (declare (type (unsigned-byte 33) low high))
    (values (the word (logior (ash (logand high #xffffffff) 32)
                             (logand low #xffffffff)))
            (the bit (ash high -32)))))

(defun portable-subtract-with-borrow (a b no-borrow)
  "Return a-b-(1-NO-BORROW), and 1 for no outgoing borrow, 0 for a borrow."
  (declare #.clempo.base:*hot* (type word a b) (type bit no-borrow))
  (let* ((low (- (ldb (byte 32 0) a) (ldb (byte 32 0) b) (- 1 no-borrow)))
         (high (- (ldb (byte 32 32) a) (ldb (byte 32 32) b)
                  (if (minusp low) 1 0))))
    (declare (type (signed-byte 33) low high))
    (values (the word (logior (ash (logand high #xffffffff) 32)
                             (logand low #xffffffff)))
            (if (minusp high) 0 1))))

(defun portable-multiply-and-add (a b c)
  "Return high,low for a*b+c. Half-word products never exceed one word."
  (declare #.clempo.base:*hot* (type word a b c))
  (let* ((a0 (ldb (byte 32 0) a)) (a1 (ldb (byte 32 32) a))
         (b0 (ldb (byte 32 0) b)) (b1 (ldb (byte 32 32) b))
         (product (the word (* a0 b0)))
         (middle (the word (+ (* a1 b0) (ash product -32))))
         (cross (the word (+ (* a0 b1) (logand middle #xffffffff))))
         (high (the word (+ (* a1 b1) (ash middle -32) (ash cross -32))))
         (low (the word (logior (ash (logand cross #xffffffff) 32)
                                (logand product #xffffffff)))))
    (declare (type (unsigned-byte 32) a0 a1 b0 b1)
             (type word product middle cross high low))
    (multiple-value-bind (result carry) (portable-add-with-carry low c 0)
      (values (ldb (byte 64 0) (+ high carry)) result))))

;; These macros expand straight to the native primitive, without runtime dispatch,
;; on the two supported 64-bit backends when that primitive actually exists.
;; FBOUNDP is not a capability test: compiler-only SBCL intrinsics are not functions.
(defmacro add-with-carry (a b carry)
  (list '#.(or (and (member :64-bit *features*)
                   (or (member :arm64 *features*) (member :x86-64 *features*))
                   (find-symbol "%ADD-WITH-CARRY" "SB-BIGNUM"))
              'portable-add-with-carry)
        a b carry))

(defmacro subtract-with-borrow (a b no-borrow)
  (list '#.(or (and (member :64-bit *features*)
                   (or (member :arm64 *features*) (member :x86-64 *features*))
                   (find-symbol "%SUBTRACT-WITH-BORROW" "SB-BIGNUM"))
              'portable-subtract-with-borrow)
        a b no-borrow))

(defmacro multiply-and-add (a b c)
  (list '#.(or (and (member :64-bit *features*)
                   (or (member :arm64 *features*) (member :x86-64 *features*))
                   (find-symbol "%MULTIPLY-AND-ADD" "SB-BIGNUM"))
              'portable-multiply-and-add)
        a b c))
