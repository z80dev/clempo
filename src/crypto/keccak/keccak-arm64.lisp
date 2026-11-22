(defpackage #:clempo.keccak-arm64
  (:use #:cl #:clempo.base)
  (:export #:define-arm64-keccak #:permute #:hash32-into #:hash64-into #:**native**
           #:+lane-bytes+ #:+lanes+ #:+plane-lanes+ #:+rounds+ #:+digest-lanes+ #:+rate-lanes+))
(in-package #:clempo.keccak-arm64)

;;; The Keccak-f[1600] shape for Keccak-256, shared with keccak.lisp (loaded after this
;;; file, on every host).
(defbound +lane-bytes+ 8
  "Bytes in one 64-bit lane, loaded and stored little-endian."
  (= (* 8 +lane-bytes+) 64))
(defbound +lanes+ 25
  "Lanes in the 1600-bit state."
  (= (* 8 +lane-bytes+ +lanes+) 1600))
(defbound +plane-lanes+ 5
  "Lanes in one plane (fixed y); the state is a plane-lanes square of lanes."
  (= (* +plane-lanes+ +plane-lanes+) +lanes+))
(defbound +rounds+ 24
  "Rounds of Keccak-f[1600]: 12 + 2l for lanes of 2^l bits."
  (= +rounds+ (+ 12 (* 2 (1- (integer-length (* 8 +lane-bytes+)))))))
(defbound +digest-lanes+ 4
  "Lanes squeezed into a 256-bit digest."
  (= (* 8 +lane-bytes+ +digest-lanes+) 256))
(defbound +rate-lanes+ 17
  "Lanes absorbed per block: the state less a capacity of twice the digest."
  (= (- +lanes+ +rate-lanes+) (* 2 +digest-lanes+))
  ;; The digest squeezes from one block.
  (<= +digest-lanes+ +rate-lanes+)
  ;; Fixed 32- and 64-byte inputs leave room for both padding bits in one block.
  (< (floor 64 +lane-bytes+) (1- +rate-lanes+)))

;;; Keccak-f[1600] on the Arm FEAT_SHA3 instructions (EOR3, RAX1, XAR, BCAX). Every Apple
;;; Silicon core has them; an arm64 Linux core may not, so callers branch on **NATIVE**,
;;; which asks the OS at load and again whenever a saved image starts.
;;; Each SIMD register holds one lane in its low half. The high halves never
;;; affect the low halves. Pi only renames the register map; nothing moves.
#+arm64
(progn
  (sb-ext:defglobal **native** nil)
  (defun detect-native () (setf **native** (clempo.io:cpu-feature-p :arm64-sha3)))
  (detect-native)
  (pushnew 'detect-native sb-ext:*init-hooks*))

;;; GC soundness: the words entries hold raw interior pointers (vector + offset) in
;;; non-descriptor registers for all 24 rounds. The body runs pseudo-atomic, so no
;;; stop-the-world GC can move a vector under those pointers, and every descriptor input
;;; stays live in its register to the end (:to :result), as in secp256k1-native-field.lisp.
;;; Resolve backend-only symbols by name: the layer checker reads inactive branches too.
#+arm64
(defbound +instruction-bytes+ 4
  "Bytes in one A64 instruction word, emitted low byte first."
  ;; The EOR3, RAX1, XAR and BCAX encodings fill the word exactly.
  (= (integer-length #xce800000) (* 8 +instruction-bytes+)))

#+arm64
(defmacro define-permutation-entry (name intrinsic rotations &optional words)
  (let ((lanes (loop repeat +lanes+ collect (gensym "A")))
        (parities (loop repeat +plane-lanes+ collect (gensym "C")))
        (rotation-table (symbol-value rotations)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (sb-c:defknown ,intrinsic (,@(if words
                                         '(octets (integer 0 #.most-positive-fixnum)
                                           octets (integer 0 #.most-positive-fixnum))
                                         `((simple-array (unsigned-byte 64) (,+lanes+))))
                                     (simple-array (unsigned-byte 64) (,+rounds+)))
             (values) () :overwrite-fndb-silently t)
         (sb-c:define-vop (,name)
           (:translate ,intrinsic)
           (:policy :fast-safe)
           ,@(if words
                 '((:args (data :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                          (start :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM")) :to :result)
                          (out :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                          (out-start :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM")) :to :result)
                          (constants :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result))
                   (:arg-types * #.(find-symbol "UNSIGNED-NUM" "SB-VM") * #.(find-symbol "UNSIGNED-NUM" "SB-VM") *)
                   (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) src)
                   (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) dst))
                 '((:args (state :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                          (constants :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result))
                   (:arg-types * *)))
           ,@(loop for r in (append lanes parities '(delta)) collect
                   `(:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) ,r))
           (:generator 1
            (#.(find-symbol "PSEUDO-ATOMIC" "SB-VM") (#.(find-symbol "TMP-TN" "SB-VM") :sync nil)
             (let* ((header (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                               sb-vm:other-pointer-lowtag))
                    (layout (vector ,@lanes))
                    (next (make-array +lanes+))
                    (c (vector ,@parities)))
               (labels ((op (encoding dst a b &optional (extra 0))
                          ;; Rd[4:0], Rn[9:5], Rm[20:16]. EOR3/BCAX use Ra[14:10],
                          ;; XAR uses imm6[15:10].
                          (let ((opcode (logior encoding (sb-c:tn-offset dst)
                                               (ash (sb-c:tn-offset a) 5)
                                               (ash (sb-c:tn-offset b) 16)
                                               (ash extra 10))))
                            (dotimes (i +instruction-bytes+)
                              (sb-assem:inst #.(find-symbol "BYTE" "SB-ARM64-ASM") (ldb (byte 8 (* 8 i)) opcode)))))
                        (lane (x y) (aref layout (+ x (* +plane-lanes+ y)))))
                 ,@(if words
                       `((sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") src data start)
                         (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") dst out out-start)
                         (dotimes (i +lanes+)
                           (let ((reg (aref layout i)))
                             (cond ((< i ,words)
                                    (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") reg
                                                   (#.(find-symbol "@" "SB-ARM64-ASM") src (+ header (* +lane-bytes+ i))) :d))
                                   ((= i ,words)
                                    (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") reg
                                                   (#.(find-symbol "@" "SB-ARM64-ASM") constants header) :d))
                                   ((= i (1- +rate-lanes+))
                                    (sb-assem:inst #.(find-symbol "SHL" "SB-ARM64-ASM") reg (aref layout ,words) 63 :2d))
                                   (t (sb-assem:inst #.(find-symbol "MOVI" "SB-ARM64-ASM") reg 0 :16b))))))
                       '((dotimes (i +lanes+)
                           (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") (aref layout i)
                                          (#.(find-symbol "@" "SB-ARM64-ASM") state (+ header (* +lane-bytes+ i))) :d))))
                 (dotimes (round +rounds+)
                   (dotimes (x +plane-lanes+)
                     (let ((p (aref c x)))
                       (op #xce000000 p (lane x 0) (lane x 1) (sb-c:tn-offset (lane x 2)))
                       (op #xce000000 p p (lane x 3) (sb-c:tn-offset (lane x 4)))))
                   (dotimes (x +plane-lanes+)
                     ;; RAX1 computes C[x-1] XOR ROL1(C[x+1]). XAR applies
                     ;; the correction and rotates in one instruction.
                     (op #xce608c00 delta (aref c (mod (+ x 4) 5)) (aref c (mod (1+ x) 5)))
                     (dotimes (y +plane-lanes+)
                       (let ((r (lane x y)))
                         (op #xce800000 r r delta
                             (mod (- (aref ,rotation-table (+ x (* +plane-lanes+ y)))) 64))
                         (setf (aref next (+ y (* +plane-lanes+ (mod (+ (* 2 x) (* 3 y)) 5)))) r))))
                   (rotatef layout next)
                   (dotimes (y +plane-lanes+)
                     (let ((a (lane 0 y)) (b (lane 1 y)) (d (lane 2 y))
                           (e (lane 3 y)) (f (lane 4 y)) (a0 (aref c 0)) (a1 (aref c 1)))
                       (sb-assem:inst #.(find-symbol "MOV" "SB-ARM64-ASM") a0 a :16b)
                       (sb-assem:inst #.(find-symbol "MOV" "SB-ARM64-ASM") a1 b :16b)
                       (op #xce200000 a a d (sb-c:tn-offset b))
                       (op #xce200000 b b e (sb-c:tn-offset d))
                       (op #xce200000 d d f (sb-c:tn-offset e))
                       (op #xce200000 e e a0 (sb-c:tn-offset f))
                       (op #xce200000 f f a1 (sb-c:tn-offset a0))))
                   (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") delta
                                  (#.(find-symbol "@" "SB-ARM64-ASM") constants (+ header (* +lane-bytes+ round))) :d)
                   (sb-assem:inst #.(find-symbol "EOR" "SB-ARM64-ASM") (lane 0 0) (lane 0 0) delta :16b))
                 ,(if words
                      '(dotimes (i +digest-lanes+)
                         (sb-assem:inst #.(find-symbol "STR" "SB-ARM64-ASM") (aref layout i)
                                        (#.(find-symbol "@" "SB-ARM64-ASM") dst (+ header (* +lane-bytes+ i))) :d))
                      '(dotimes (i +lanes+)
                         (sb-assem:inst #.(find-symbol "STR" "SB-ARM64-ASM") (aref layout i)
                                        (#.(find-symbol "@" "SB-ARM64-ASM") state (+ header (* +lane-bytes+ i))) :d)))))))))
       ,(if words
            `(defun ,name (data start out out-start constants)
               (declare #.*hot* (type octets data out)
                        (type (integer 0 #.most-positive-fixnum) start out-start)
                        (type (simple-array (unsigned-byte 64) (,+rounds+)) constants))
               (,intrinsic data start out out-start constants))
            `(defun ,name (state constants)
               (declare #.*hot* (type (simple-array (unsigned-byte 64) (,+lanes+)) state)
                        (type (simple-array (unsigned-byte 64) (,+rounds+)) constants))
               (,intrinsic state constants))))))

#+arm64
(defmacro define-arm64-keccak (rotations)
  `(progn
     (define-permutation-entry permute %permute ,rotations)
     (define-permutation-entry hash32-into %hash32-into ,rotations 4)
     (define-permutation-entry hash64-into %hash64-into ,rotations 8)))
