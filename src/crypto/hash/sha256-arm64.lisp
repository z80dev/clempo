(defpackage #:clempo.sha256-arm64
  (:use #:cl #:clempo.base)
  (:export #:compress #:**native**))
(in-package #:clempo.sha256-arm64)

;;; One SHA-256 block on the Armv8 FEAT_SHA256 instructions; the caller owns bounds
;;; checks, buffering and padding. Every Apple Silicon core has them; an arm64 Linux core
;;; may not, so sha2.lisp branches on **NATIVE**, which asks the OS at load and again
;;; whenever a saved image starts, and otherwise runs its portable scalar core (compiled,
;;; and tested, on every host).
#+arm64
(progn
  (sb-ext:defglobal **native** nil)
  (defun detect-native () (setf **native** (clempo.io:cpu-feature-p :arm64-sha256)))
  (detect-native)
  (pushnew 'detect-native sb-ext:*init-hooks*))

;;; GC soundness: PTR and KS hold raw interior pointers in non-descriptor registers. The
;;; body runs pseudo-atomic, so no stop-the-world GC can move a vector under them, and the
;;; descriptor inputs stay live to the end (:to :result), as in secp256k1-native-field.lisp.
;;; Resolve backend-only symbols by name: the layer checker reads inactive branches too.
#+arm64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %compress ((simple-array (unsigned-byte 32) (8)) octets (integer 0 #.most-positive-fixnum)
                            (simple-array (unsigned-byte 32) (64)))
      (values) () :overwrite-fndb-silently t)

  (sb-c:define-vop (sha256-compress)
    (:translate %compress)
    (:policy :fast-safe)
    (:args (state :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
           (data :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
           (offset :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM")) :to :result)
           (constants :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result))
    (:arg-types * * #.(find-symbol "UNSIGNED-NUM" "SB-VM") *)
    (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) ptr)
    (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) ks)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) abcd)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) efgh)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) saved-a)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) saved-e)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) old-a)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) wk)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) w0)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) w1)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) w2)
    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) w3)
    (:generator 1
     (#.(find-symbol "PSEUDO-ATOMIC" "SB-VM") (#.(find-symbol "TMP-TN" "SB-VM") :sync nil)
      (let ((header (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes) sb-vm:other-pointer-lowtag))
            (schedule (vector w0 w1 w2 w3)))
        (flet ((sha (encoding dst left &optional right)
                 ;; Arm DDI 0602: Rd[4:0], Rn[9:5], Rm[20:16] (Rm absent for SU0).
                 (let ((opcode (logior encoding (sb-c:tn-offset dst)
                                       (ash (sb-c:tn-offset left) 5)
                                       (if right (ash (sb-c:tn-offset right) 16) 0))))
                   (dotimes (i 4)
                     (sb-assem:inst #.(find-symbol "BYTE" "SB-ARM64-ASM") (ldb (byte 8 (* 8 i)) opcode))))))
          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr data offset)
          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr ptr header)
          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ks constants header)
          (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") abcd (#.(find-symbol "@" "SB-ARM64-ASM") state header))
          (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") efgh (#.(find-symbol "@" "SB-ARM64-ASM") state (+ header 16)))
          (sb-assem:inst #.(find-symbol "MOV" "SB-ARM64-ASM") saved-a abcd :16b)
          (sb-assem:inst #.(find-symbol "MOV" "SB-ARM64-ASM") saved-e efgh :16b)
          (dotimes (i 4)
            (let ((w (aref schedule i)))
              (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") w (#.(find-symbol "@" "SB-ARM64-ASM") ptr (* 16 i)))
              (sb-assem:inst #.(find-symbol "REV32" "SB-ARM64-ASM") w w :16b)))
          (dotimes (i 16)
            (let ((w (aref schedule (mod i 4))))
              (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") wk (#.(find-symbol "@" "SB-ARM64-ASM") ks (* 16 i)))
              (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") wk wk w :4s)
              (sb-assem:inst #.(find-symbol "MOV" "SB-ARM64-ASM") old-a abcd :16b)
              (sha #x5e004000 abcd efgh wk)  ; SHA256H
              (sha #x5e005000 efgh old-a wk) ; SHA256H2
              (when (< i 12)
                (sha #x5e282800 w (aref schedule (mod (+ i 1) 4))) ; SHA256SU0
                (sha #x5e006000 w (aref schedule (mod (+ i 2) 4)) ; SHA256SU1
                     (aref schedule (mod (+ i 3) 4))))))
          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") abcd abcd saved-a :4s)
          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") efgh efgh saved-e :4s)
          (sb-assem:inst #.(find-symbol "STR" "SB-ARM64-ASM") abcd (#.(find-symbol "@" "SB-ARM64-ASM") state header))
          (sb-assem:inst #.(find-symbol "STR" "SB-ARM64-ASM") efgh (#.(find-symbol "@" "SB-ARM64-ASM") state (+ header 16)))))))))

#+arm64
(defun compress (state data offset constants)
  (declare #.clempo.base:*hot* (type (simple-array (unsigned-byte 32) (8)) state)
           (type (simple-array (unsigned-byte 32) (64)) constants) (type octets data)
           (type (integer 0 #.most-positive-fixnum) offset))
  (%compress state data offset constants))
