(defpackage #:clempo.aes-arm64
  (:use #:cl #:clempo.base)
  (:export #:encrypt-block #:**native**))
(in-package #:clempo.aes-arm64)

;;; One AES block on the Armv8 FEAT_AES instructions (AESE, AESMC), which run in constant
;;; time. KEYS holds the expanded key in FIPS 197 byte order, 16 octets per round key.
;;; aes.lisp branches on **NATIVE**, asked of the OS at load and whenever a saved image
;;; starts, and otherwise runs its bitsliced portable core (compiled, and tested, on
;;; every host).
#+arm64
(progn
  (sb-ext:defglobal **native** nil)
  (defun detect-native () (setf **native** (clempo.io:cpu-feature-p :arm64-aes)))
  (detect-native)
  (pushnew 'detect-native sb-ext:*init-hooks*))

;;; GC soundness as in sha256-arm64.lisp: raw interior pointers live only inside a
;;; pseudo-atomic body and every descriptor input stays live to the result.
#+arm64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((define-block-vop (name rounds)
               `(progn
                  (sb-c:defknown ,name (octets octets (integer 0 #.most-positive-fixnum)
                                               octets (integer 0 #.most-positive-fixnum))
                      (values) () :overwrite-fndb-silently t)
                  (sb-c:define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (keys :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                           (in :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                           (in-offset :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM")) :to :result)
                           (out :scs (#.(find-symbol "DESCRIPTOR-REG" "SB-VM")) :to :result)
                           (out-offset :scs (#.(find-symbol "UNSIGNED-REG" "SB-VM")) :to :result))
                    (:arg-types * * #.(find-symbol "UNSIGNED-NUM" "SB-VM")
                                * #.(find-symbol "UNSIGNED-NUM" "SB-VM"))
                    (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) ptr)
                    (:temporary (:sc #.(find-symbol "NON-DESCRIPTOR-REG" "SB-VM") :from :argument :to :result) kp)
                    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) state)
                    (:temporary (:sc #.(find-symbol "COMPLEX-DOUBLE-REG" "SB-VM")) key)
                    (:generator 1
                     (#.(find-symbol "PSEUDO-ATOMIC" "SB-VM") (#.(find-symbol "TMP-TN" "SB-VM") :sync nil)
                      (let ((header (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                       sb-vm:other-pointer-lowtag)))
                        (flet ((raw (encoding dst left &optional right)
                                 ;; Arm DDI 0602: Rd[4:0], Rn[9:5], Rm[20:16].
                                 (let ((opcode (logior encoding (sb-c:tn-offset dst)
                                                       (ash (sb-c:tn-offset left) 5)
                                                       (if right (ash (sb-c:tn-offset right) 16) 0))))
                                   (dotimes (i 4)
                                     (sb-assem:inst #.(find-symbol "BYTE" "SB-ARM64-ASM")
                                                    (ldb (byte 8 (* 8 i)) opcode)))))
                               (load-key (round)
                                 (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") key
                                                (#.(find-symbol "@" "SB-ARM64-ASM") kp (* 16 round)))))
                          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") kp keys header)
                          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr in in-offset)
                          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr ptr header)
                          (sb-assem:inst #.(find-symbol "LDR" "SB-ARM64-ASM") state
                                         (#.(find-symbol "@" "SB-ARM64-ASM") ptr 0))
                          (dotimes (round ,(1- rounds))
                            (load-key round)
                            (raw #x4e284800 state key)    ; AESE: AddRoundKey, SubBytes, ShiftRows
                            (raw #x4e286800 state state)) ; AESMC: MixColumns
                          (load-key ,(1- rounds))
                          (raw #x4e284800 state key)
                          (load-key ,rounds)
                          (raw #x6e201c00 state state key) ; EOR: the final AddRoundKey
                          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr out out-offset)
                          (sb-assem:inst #.(find-symbol "ADD" "SB-ARM64-ASM") ptr ptr header)
                          (sb-assem:inst #.(find-symbol "STR" "SB-ARM64-ASM") state
                                         (#.(find-symbol "@" "SB-ARM64-ASM") ptr 0))))))))))
    (define-block-vop %encrypt-128 10)
    (define-block-vop %encrypt-256 14)))

#+arm64
(defun encrypt-block (keys rounds in in-offset out out-offset)
  "Encrypt the block at IN-OFFSET of IN into OUT at OUT-OFFSET; the caller checks bounds."
  (declare #.clempo.base:*hot* (type octets keys in out) (type (member 10 14) rounds)
           (type (integer 0 #.most-positive-fixnum) in-offset out-offset))
  (if (= rounds 10)
      (%encrypt-128 keys in in-offset out out-offset)
      (%encrypt-256 keys in in-offset out out-offset))
  (values))
