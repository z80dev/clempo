(in-package #:clempo.evm)

;;; Ethereum precompiles. DEFPRECOMPILE declares the address, the fork it appears in, the
;;; gas as a function of the input, and a body returning the output octets or NIL for a
;;; failure (which, like running out of gas, consumes all the gas given).
;;;
;;; A precompile is a function of the frame (FRAME-INPUT, FRAME-GAS, FRAME-CALLER, ...)
;;; returning (values status output) with the gas left in FRAME-GAS, the same protocol
;;; as the interpreter, so the tempo layer's precompiles plug into the same providers.

(defvar *ethereum-precompiles* '()
  "(address since name function), one per DEFPRECOMPILE.")

(defmacro defprecompile (name address (&key (since :istanbul)) (input) gas &body body)
  "Define the Ethereum precompile NAME at ADDRESS. GAS is a form of INPUT (and of the
running fork via FORK>=); BODY returns the output or NIL on failure."
  (let ((frame (gensym "FRAME")) (cost (gensym "COST")) (out (gensym "OUT")))
    `(progn
       (defun ,name (,frame)
         (declare (type frame ,frame))
         (let* ((,input (frame-input ,frame))
                (,cost ,gas))
           (declare (type octets ,input) (ignorable ,input))
           (if (> ,cost (frame-gas ,frame))
               (progn (setf (frame-gas ,frame) 0) (values :out-of-gas +empty-code+))
               (let ((,out (progn ,@body)))
                 (if ,out
                     (progn (decf (frame-gas ,frame) ,cost) (values :return ,out))
                     (progn (setf (frame-gas ,frame) 0) (values :precompile-failure +empty-code+)))))))
       (setf *ethereum-precompiles*
             (cons (list ,address ,since ',name (function ,name))
                   (remove ,address *ethereum-precompiles* :key #'first)))
       (clrhash *precompile-tables*)
       ',name)))

(defvar *precompile-tables* (make-hash-table :synchronized t) "fork index -> address table (lazy; shared across threads)")

(defun ethereum-precompiles (fork)
  "A hash table address -> PRECOMPILE of Ethereum's precompiles active in FORK."
  (let ((f (fork-index fork)))
    (or (gethash f *precompile-tables*)
        (setf (gethash f *precompile-tables*)
              (let ((table (make-hash-table)))
                (loop for (address since name fn) in *ethereum-precompiles*
                      when (<= (fork-index since) f)
                        do (setf (gethash address table) (make-precompile name address fn)))
                table)))))

(declaim (inline words))
(defun words (n) (ceiling n 32))

(defun input-slice (input start size)
  "SIZE bytes of INPUT from START, zero-padded (precompile inputs are implicitly padded)."
  (padded-slice input start size))

(defprecompile pc-identity #x04 () (input)
  (+ 15 (* 3 (words (length input))))
  (copy-seq input))

(defprecompile pc-sha256 #x02 () (input)
  (+ 60 (* 12 (words (length input))))
  (clempo.sha2:sha256 input))

(defprecompile pc-ecrecover #x01 () (input)
  3000
  (let* ((in (input-slice input 0 128))
         (v (octets->integer in :start 32 :end 64))
         (r (octets->integer in :start 64 :end 96))
         (s (octets->integer in :start 96 :end 128))
         (public (and (or (= v 27) (= v 28))
                      (clempo.secp256k1:ecrecover (subseq in 0 32) r s (- v 27)))))
    (if public
        (integer->octets (clempo.secp256k1:pubkey->address public) 32)
        +empty-code+)))

(defprecompile pc-ripemd160 #x03 () (input)
  (+ 600 (* 120 (words (length input))))
  (octets-concat (make-octets 12) (clempo.ripemd160:ripemd160 input)))

;;; MODEXP: EIP-198 pricing, EIP-2565 from Berlin, EIP-7883 and the EIP-7823 size limit
;;; from Osaka.

(defun modexp-lengths (input)
  (values (octets->integer (input-slice input 0 32))
          (octets->integer (input-slice input 32 32))
          (octets->integer (input-slice input 64 32))))

(defun modexp-gas (input)
  (multiple-value-bind (base-len exp-len mod-len) (modexp-lengths input)
    (let* ((max-len (max base-len mod-len))
           (head-len (min exp-len 32))
           (head (if (and (< base-len (ash 1 32)) (plusp head-len))
                     (octets->integer (input-slice input (+ 96 base-len) head-len))
                     0))
           (osaka (fork>= :osaka))
           (multiplier (if osaka 16 8))
           (iterations (max 1 (if (<= exp-len 32)
                                  (max 0 (1- (integer-length head)))
                                  (+ (* multiplier (- exp-len 32))
                                     (1- (max 1 (integer-length head))))))))
      (cond (osaka
             (max 500 (* (if (<= max-len 32) 16 (* 2 (expt (ceiling max-len 8) 2))) iterations)))
            ((fork>= :berlin)
             (max 200 (floor (* (expt (ceiling max-len 8) 2) iterations) 3)))
            (t
             (floor (* (cond ((<= max-len 64) (* max-len max-len))
                             ((<= max-len 1024) (+ (floor (* max-len max-len) 4) (* 96 max-len) -3072))
                             (t (+ (floor (* max-len max-len) 16) (* 480 max-len) -199680)))
                       iterations)
                    20))))))

(defprecompile pc-modexp #x05 () (input)
  (modexp-gas input)
  (multiple-value-bind (base-len exp-len mod-len) (modexp-lengths input)
    (cond ((and (fork>= :osaka) (or (> base-len 1024) (> exp-len 1024) (> mod-len 1024))) nil)
          ((and (zerop base-len) (zerop mod-len)) +empty-code+)
          (t (clempo.modexp:modexp (input-slice input 96 base-len)
                                   (input-slice input (+ 96 base-len) exp-len)
                                   (input-slice input (+ 96 base-len exp-len) mod-len))))))

(defprecompile pc-blake2f #x09 () (input)
  (if (= (length input) 213) (octets->integer input :end 4) 0)
  (clempo.blake2f:blake2f-precompile input))

(defprecompile pc-p256verify #x100 (:since :osaka) (input)
  6900
  (if (and (= (length input) 160)
           (clempo.p256:p256-verify (subseq input 0 32)
                                    (octets->integer input :start 32 :end 64)
                                    (octets->integer input :start 64 :end 96)
                                    (octets->integer input :start 96 :end 128)
                                    (octets->integer input :start 128 :end 160)))
      (integer->octets 1 32)
      +empty-code+))
