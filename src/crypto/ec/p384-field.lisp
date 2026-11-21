;;; P-384 field/scalar engine: six little-endian UB64 Montgomery words, R=2^384.
;;; Every residue is canonical (< its modulus). Offsets count words. Exact
;;; output/input aliases are supported; partial overlaps are not. INV! alone
;;; needs caller workspace: twelve words disjoint from all input/output words.
;;; Parameter vectors below are immutable after load; hot calls allocate nothing.
(in-package #:clempo.p384)

(deftype p384-arena () '(simple-array (unsigned-byte 64) (*)))

(defmacro p384-integer-limb (integer index)
  ;; A positive SBCL bignum may include a leading sign word. Missing words are
  ;; zero. Read the object directly rather than allocating shifted integers.
  `(if (typep ,integer 'fixnum) ,(if (zerop index) integer 0)
       (if (< ,index (sb-bignum:%bignum-length ,integer))
           (sb-bignum::%bignum-ref ,integer ,index) 0)))

;; Keep the ARM64 assembler references inside reader guards. Other targets use
;; the shared word macros below, including their portable arithmetic fallback.
#+arm64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %p384-word-mac
      ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
      (values (unsigned-byte 64) (unsigned-byte 64))
      (sb-c:movable sb-c:flushable) :overwrite-fndb-silently t)
  (sb-c:define-vop (%p384-word-mac)
    (:translate %p384-word-mac)
    (:policy :fast-safe)
    ;; Inputs remain live until both results exist. In particular UMULH cannot
    ;; overwrite an operand before MUL reads it (the stock four-arg VOP bug).
    (:args (a :scs (sb-vm::unsigned-reg) :to :result)
           (b :scs (sb-vm::unsigned-reg) :to :result)
           (c :scs (sb-vm::unsigned-reg) :to :result)
           (d :scs (sb-vm::unsigned-reg) :to :result))
    (:arg-types sb-vm::unsigned-num sb-vm::unsigned-num
                sb-vm::unsigned-num sb-vm::unsigned-num)
    (:results (hi :scs (sb-vm::unsigned-reg) :from :load)
              (lo :scs (sb-vm::unsigned-reg) :from :load))
    (:result-types sb-vm::unsigned-num sb-vm::unsigned-num)
    (:generator 1
      ;; All six registers are in the unboxed bank; no descriptor scratch,
      ;; allocation, raw heap pointer or pseudo-atomic guard is needed.
      (sb-assem:inst sb-arm64-asm::mul lo a b)
      (sb-assem:inst sb-arm64-asm::umulh hi a b)
      (sb-assem:inst sb-arm64-asm::adds lo lo c)
      (sb-assem:inst sb-arm64-asm::adc hi hi sb-vm::zr-tn)
      (sb-assem:inst sb-arm64-asm::adds lo lo d)
      (sb-assem:inst sb-arm64-asm::adc hi hi sb-vm::zr-tn))))

(declaim (inline p384-mul-add-word))
(defun p384-mul-add-word (a b c d)
  (declare #.clempo.base:*hot* (type (unsigned-byte 64) a b c d))
  #+arm64 (%p384-word-mac a b c d)
  #-arm64
  (multiple-value-bind (hi lo) (clempo.ec.word:multiply-and-add a b c)
    (multiple-value-bind (low carry) (clempo.ec.word:add-with-carry lo d 0)
      (values (ldb (byte 64 0) (+ hi carry)) low))))

#+arm64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %p384-word-mac-minus-one
      ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
      (values (unsigned-byte 64) (unsigned-byte 64))
      (sb-c:movable sb-c:flushable) :overwrite-fndb-silently t)
  (sb-c:define-vop (%p384-word-mac-minus-one)
    (:translate %p384-word-mac-minus-one)
    (:policy :fast-safe)
    (:args (m :scs (sb-vm::unsigned-reg) :to :result)
           (c :scs (sb-vm::unsigned-reg) :to :result)
           (d :scs (sb-vm::unsigned-reg) :to :result))
    (:arg-types sb-vm::unsigned-num sb-vm::unsigned-num sb-vm::unsigned-num)
    (:results (hi :scs (sb-vm::unsigned-reg) :from :load)
              (lo :scs (sb-vm::unsigned-reg) :from :load))
    (:result-types sb-vm::unsigned-num sb-vm::unsigned-num)
    (:generator 1
      ;; m*(2^64-1)+c+d = m*2^64 + (c-m+d).
      (sb-assem:inst sb-arm64-asm::subs lo c m)
      (sb-assem:inst sb-arm64-asm::sbc hi m sb-vm::zr-tn)
      (sb-assem:inst sb-arm64-asm::adds lo lo d)
      (sb-assem:inst sb-arm64-asm::adc hi hi sb-vm::zr-tn))))

(declaim (inline p384-mul-add-minus-one))
(defun p384-mul-add-minus-one (m c d)
  (declare #.clempo.base:*hot* (type (unsigned-byte 64) m c d))
  #+arm64 (%p384-word-mac-minus-one m c d)
  #-arm64
  (multiple-value-bind (lo carry) (clempo.ec.word:subtract-with-borrow c m 1)
    (let ((hi (ldb (byte 64 0) (+ m carry -1))))
      (declare (type (unsigned-byte 64) hi))
      (multiple-value-bind (low carry) (clempo.ec.word:add-with-carry lo d 0)
        (values (ldb (byte 64 0) (+ hi carry)) low)))))

(defmacro define-p384-raw-operations ()
  `(progn
     (defun p384-zero! (out o)
       (declare #.clempo.base:*hot* (type p384-arena out) (type fixnum o))
       ,@(loop for i below 6 collect `(setf (aref out (+ o ,i)) 0))
       (values))
     (defun p384-copy! (out o a ao)
       (declare #.clempo.base:*hot* (type p384-arena out a) (type fixnum o ao))
       ,@(loop for i below 6 collect `(setf (aref out (+ o ,i)) (aref a (+ ao ,i))))
       (values))
     (defun p384-zero-p (a ao)
       (declare #.clempo.base:*hot* (type p384-arena a) (type fixnum ao))
       (zerop (logior ,@(loop for i below 6 collect `(aref a (+ ao ,i))))))
     (defun p384-equal-p (a ao b bo)
       (declare #.clempo.base:*hot* (type p384-arena a b) (type fixnum ao bo))
       (and ,@(loop for i below 6 collect `(= (aref a (+ ao ,i)) (aref b (+ bo ,i))))))))

(define-p384-raw-operations)

(defparameter *p384-integer-one*
  (make-array 6 :element-type '(unsigned-byte 64) :initial-contents '(1 0 0 0 0 0)))

(defmacro define-p384-residue (prefix modulus &key digest)
  (let* ((p (symbol-value modulus))
         (radix (ash 1 384))
         (ps (loop for i below 6 collect (ldb (byte 64 (* i 64)) p)))
         ;; Constant generation only; no generic integer arithmetic at runtime.
         (factor (mod (- (clempo.ec:invert (first ps) (ash 1 64))) (ash 1 64))))
    (labels ((name (suffix) (intern (format nil "~A-~A" prefix suffix) '#:clempo.p384))
             (ge (words i)
               (if (zerop i) `(>= ,(nth i words) ,(nth i ps))
                   `(or (> ,(nth i words) ,(nth i ps))
                        (and (= ,(nth i words) ,(nth i ps)) ,(ge words (1- i))))))
             (subtract (words carry)
               (loop for word in words for pword in ps collect
                 `(setf (values ,word ,carry)
                        (clempo.ec.word:subtract-with-borrow ,word ,pword ,carry))))
             (store (words)
               (loop for word in words for i from 0 collect `(setf (aref out (+ o ,i)) ,word))))
      `(progn
         (defparameter ,(name "R2")
           (make-array 6 :element-type '(unsigned-byte 64)
                       :initial-contents ',(loop for i below 6 collect
                                             (ldb (byte 64 (* i 64)) (mod (* radix radix) p)))))
         (defun ,(name "ONE!") (out o)
           (declare #.clempo.base:*hot* (type p384-arena out) (type fixnum o))
           ,@(loop for i below 6 collect
               `(setf (aref out (+ o ,i)) ,(ldb (byte 64 (* i 64)) (mod radix p))))
           (values))
         ,@(loop for addp in '(t nil) collect
             (let ((words (loop repeat 6 collect (gensym "W"))))
               `(defun ,(name (if addp "ADD!" "SUB!")) (out o a ao b bo)
                  (declare #.clempo.base:*hot* (type p384-arena out a b)
                           (type fixnum o ao bo))
                  (let (,@(loop for word in words collect `(,word 0)) (carry ,(if addp 0 1)))
                    (declare (type (unsigned-byte 64) ,@words) (type bit carry))
                    ,@(loop for word in words for i from 0 collect
                        `(setf (values ,word carry)
                               (,(if addp 'clempo.ec.word:add-with-carry 'clempo.ec.word:subtract-with-borrow)
                                (aref a (+ ao ,i)) (aref b (+ bo ,i)) carry)))
                    ,(if addp
                         `(when (or (= carry 1) ,(ge words 5))
                            ;; With a high carry, subtract modulo R. The borrow
                            ;; consumes that carry since a+b < 2p < 2R.
                            (setf carry 1) ,@(subtract words 'carry))
                         `(when (zerop carry)
                            ,@(loop for word in words for pword in ps collect
                                `(setf (values ,word carry)
                                       (clempo.ec.word:add-with-carry ,word ,pword carry)))))
                    ,@(store words))
                  (values))))
         ,(let ((words (loop repeat 6 collect (gensym "W"))))
            `(defun ,(name "NEG!") (out o a ao)
               (declare #.clempo.base:*hot* (type p384-arena out a) (type fixnum o ao))
               (if (p384-zero-p a ao)
                   (p384-zero! out o)
                   (let (,@(loop for word in words collect `(,word 0)) (carry 1))
                     (declare (type (unsigned-byte 64) ,@words) (type bit carry))
                     ,@(loop for word in words for pword in ps for i from 0 collect
                         `(setf (values ,word carry)
                                (clempo.ec.word:subtract-with-borrow ,pword (aref a (+ ao ,i)) carry)))
                     ,@(store words)))
               (values)))
         ,(let ((rs (loop repeat 7 collect (gensym "R"))))
            `(defun ,(name "MUL!") (out o a ao b bo)
               (declare #.clempo.base:*hot* (type p384-arena out a b)
                        (type fixnum o ao bo))
               (let (,@(loop for r in rs collect `(,r 0))
                     (carry 0) (m 0) (bword 0) (bit 0) (top 0))
                 (declare (type (unsigned-byte 64) ,@rs carry m bword)
                          (type bit bit top))
                 ;; Full-width CIOS: seven accumulator words, not twelve product
                 ;; words plus twelve live operands. Keep both top carry bits
                 ;; before shifting; the BLS381 headroom shortcut is invalid here.
                 ,@(loop for i below 6 append
                     `((setf bword (aref b (+ bo ,i)) carry 0)
                       ,@(loop for j below 6 collect
                           `(setf (values carry ,(nth j rs))
                                  (p384-mul-add-word
                                   (aref a (+ ao ,j)) bword ,(nth j rs) carry)))
                       (setf (values ,(nth 6 rs) top)
                             (clempo.ec.word:add-with-carry ,(nth 6 rs) carry 0))
                       (setf m (ldb (byte 64 0) (* ,(first rs) ,factor)) carry 0)
                       ,@(loop for pword in ps for j from 0 collect
                           `(setf (values carry ,(nth j rs))
                                  ,(if (= pword #xffffffffffffffff)
                                       `(p384-mul-add-minus-one m ,(nth j rs) carry)
                                       `(p384-mul-add-word m ,pword ,(nth j rs) carry))))
                       (setf (values ,(nth 6 rs) bit)
                             (clempo.ec.word:add-with-carry ,(nth 6 rs) carry 0))
                       ;; If the first addition overflowed, its new top is zero,
                       ;; so the second cannot also overflow: TOP+BIT is a bit.
                       (setf top (+ top bit))
                       ,@(loop for j below 6 collect
                           `(setf ,(nth j rs) ,(nth (1+ j) rs)))
                       (setf ,(nth 6 rs) top)))
                 ;; Each round T < A+p; at the boundary T < 2p < 2R.
                 (when (or (plusp ,(nth 6 rs)) ,(ge (subseq rs 0 6) 5))
                   (setf bit 1) ,@(subtract (subseq rs 0 6) 'bit))
                 ,@(store (subseq rs 0 6)))
               (values)))
         (defun ,(name "SQUARE!") (out o a ao)
           (declare #.clempo.base:*hot* (type p384-arena out a) (type fixnum o ao))
           (,(name "MUL!") out o a ao a ao))
         (defun ,(name "FROM-INTEGER!") (out o n)
           "Import a canonical nonnegative integer; the caller checks n < modulus."
           (declare #.clempo.base:*hot* (type p384-arena out) (type fixnum o)
                    (type (integer 0 ,(1- p)) n))
           ,@(loop for i below 6 collect
               `(setf (aref out (+ o ,i)) (p384-integer-limb n ,i)))
           (,(name "MUL!") out o out o ,(name "R2") 0))
         (defun ,(name "TO-ORDINARY!") (out o a ao)
           "Remove Montgomery R using the immutable ordinary-one vector."
           (declare #.clempo.base:*hot* (type p384-arena out a) (type fixnum o ao))
           (,(name "MUL!") out o a ao *p384-integer-one* 0))
         (declaim (inline ,(name "RAW-HALF!")))
         ,(let ((words (loop repeat 6 collect (gensym "W"))))
            `(defun ,(name "RAW-HALF!") (out o)
               "Halve a canonical ordinary coefficient modulo this odd modulus."
               (declare #.clempo.base:*hot* (type p384-arena out) (type fixnum o))
               (let (,@(loop for w in words for i from 0 collect `(,w (aref out (+ o ,i))))
                     (carry 0))
                 (declare (type (unsigned-byte 64) ,@words) (type bit carry))
                 (when (oddp ,(first words))
                   ,@(loop for w in words for pword in ps collect
                       `(setf (values ,w carry)
                              (clempo.ec.word:add-with-carry ,w ,pword carry))))
                 ,@(loop for w in words for next in (append (rest words) '(nil))
                         for i from 0 collect
                     `(setf (aref out (+ o ,i))
                            (logior (ash ,w -1)
                                    (ash ,(if next `(logand ,next 1) 'carry) 63)))))
               (values)))
         ,(let ((us (loop repeat 6 collect (gensym "U")))
                (vs (loop repeat 6 collect (gensym "V"))))
            (labels ((onep (words)
                       `(and (= ,(first words) 1) (zerop (logior ,@(rest words)))))
                     (ge-words (a b i)
                       (if (zerop i) `(>= ,(first a) ,(first b))
                           `(or (> ,(nth i a) ,(nth i b))
                                (and (= ,(nth i a) ,(nth i b))
                                     ,(ge-words a b (1- i))))))
                     (shift-words (words)
                       (loop for w in words for next in (append (rest words) '(nil)) collect
                         `(setf ,w ,(if next
                                        `(logior (ash ,w -1) (ash (logand ,next 1) 63))
                                        `(ash ,w -1))))))
              `(defun ,(name "INV!") (out o a ao work wo)
                 "Native binary inverse; twelve disjoint scratch words; zero -> zero."
                 (declare #.clempo.base:*hot* (type p384-arena out a work)
                          (type fixnum o ao wo))
                 (when (p384-zero-p a ao)
                   (p384-zero! out o) (return-from ,(name "INV!") (values)))
                 (,(name "TO-ORDINARY!") work wo a ao)
                 (let (,@(loop for u in us for i from 0 collect `(,u (aref work (+ wo ,i))))
                       ,@(loop for v in vs for pword in ps collect `(,v ,pword))
                       (carry 1))
                   (declare (type (unsigned-byte 64) ,@us ,@vs) (type bit carry))
                   ;; U,V are raw GCD words. Coefficients X,Y are ordinary
                   ;; canonical residues in OUT and WORK[wo+6], respectively.
                   (p384-zero! out o) (setf (aref out o) 1)
                   (p384-zero! work (+ wo 6))
                   (loop until (or ,(onep us) ,(onep vs)) do
                     (loop while (evenp ,(first us)) do
                       ,@(shift-words us) (,(name "RAW-HALF!") out o))
                     (loop while (evenp ,(first vs)) do
                       ,@(shift-words vs) (,(name "RAW-HALF!") work (+ wo 6)))
                     (if ,(ge-words us vs 5)
                         (progn
                           (setf carry 1)
                           ,@(loop for u in us for v in vs collect
                               `(setf (values ,u carry)
                                      (clempo.ec.word:subtract-with-borrow ,u ,v carry)))
                           (,(name "SUB!") out o out o work (+ wo 6)))
                         (progn
                           (setf carry 1)
                           ,@(loop for v in vs for u in us collect
                               `(setf (values ,v carry)
                                      (clempo.ec.word:subtract-with-borrow ,v ,u carry)))
                           (,(name "SUB!") work (+ wo 6) work (+ wo 6) out o))))
                   (unless ,(onep us) (p384-copy! out o work (+ wo 6))))
                 (,(name "MUL!") out o out o ,(name "R2") 0))))
         ,@(when digest
             (let ((words (loop repeat 6 collect (gensym "W"))))
               `((defun ,(name "FROM-DIGEST!") (out o hash48)
                   "Import all 48 big-endian bytes, reducing once modulo N."
                   (declare #.clempo.base:*hot* (type p384-arena out) (type fixnum o)
                            (type (simple-array (unsigned-byte 8) (*)) hash48))
                   (let (,@(loop for word in words for i from 0 collect
                             `(,word (logior ,@(loop for j below 8 collect
                                                `(ash (the (unsigned-byte 64)
                                                           (aref hash48 ,(+ (* (- 5 i) 8) j)))
                                                      ,(* (- 7 j) 8))))))
                         (carry 1))
                     (declare (type (unsigned-byte 64) ,@words) (type bit carry))
                     ;; Both moduli are above R/2. For this scalar-only entry,
                     ;; every 384-bit digest is therefore below 2N.
                     (when ,(ge words 5) ,@(subtract words 'carry))
                     ,@(store words))
                   (,(name "MUL!") out o out o ,(name "R2") 0)))))))))

(define-p384-residue pf +p384-p+)
(define-p384-residue ps +p384-n+ :digest t)
