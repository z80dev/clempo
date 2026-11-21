(in-package #:clempo.evm)

;;; Code analysis: valid jump destinations and the value of every PUSH, computed once per
;;; code vector and cached by identity.

(defstruct (analysis (:constructor %make-analysis (jumpdests pushes)))
  (jumpdests #* :type simple-bit-vector)
  (pushes #() :type simple-vector))

(defun analyze-code (code)
  (declare #.*hot* (type octets code))
  (let* ((len (length code))
         (jumpdests (make-array len :element-type 'bit :initial-element 0))
         (pushes (make-array len :initial-element 0))
         (pc 0))
    (declare (type fixnum pc len))
    (loop while (< pc len)
          do (let ((op (aref code pc)))
               (cond ((= op #x5b) (setf (sbit jumpdests pc) 1) (incf pc))
                     ((<= #x60 op #x7f)
                      (let* ((n (- op #x5f))
                             (end (min len (+ pc 1 n)))
                             (v 0))
                        (declare (type fixnum n end))
                        (loop for i fixnum from (1+ pc) below (+ pc 1 n)
                              do (setf v (logior (ash v 8) (if (< i end) (aref code i) 0))))
                        (setf (svref pushes pc) v)
                        (incf pc (1+ n))))
                     (t (incf pc)))))
    (%make-analysis jumpdests pushes)))

(defvar *analysis-cache* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "code vector -> ANALYSIS. Rebind per thread for lock-free use.")

(defun code-analysis (code)
  (declare (type octets code))
  (let ((cache *analysis-cache*))
    (or (gethash code cache)
        (setf (gethash code cache) (analyze-code code)))))

;;; A frame is one running message: code, context, gas and memory.

(defstruct (frame (:constructor %make-frame))
  (code +empty-code+ :type octets)
  (address 0 :type integer)          ; storage and balance context (ADDRESS)
  (code-address 0 :type integer)     ; where the code came from
  (caller 0 :type integer)
  (value 0 :type integer)
  (input +empty-code+ :type octets)
  (gas 0 :type gas)
  (depth 0 :type fixnum)
  (static-p nil :type boolean)
  (memory (make-octets 0) :type octets)
  (mem-size 0 :type fixnum)
  (return-data +empty-code+ :type octets)
  (account nil)
  (kind :call :type keyword))

;;; Memory: MEMORY is a buffer at least MEM-SIZE long; MEM-SIZE is the active size in
;;; bytes, a multiple of 32.

(define-constant +max-memory+ (ash 1 32)
  "Memory beyond 4 GiB is out of gas for any reachable gas limit.")

(declaim (inline memory-cost mem-end))
(defun memory-cost (bytes gp)
  (declare (type fixnum bytes) (type gas-params gp))
  (let ((words (ceiling bytes 32)))
    (+ (* (gas-params-memory-word gp) words)
       (floor (* words words) (gas-params-memory-quad-divisor gp)))))

(defun mem-end (offset size)
  "OFFSET+SIZE as a fixnum when the region is addressable, else NIL. SIZE is nonzero."
  (and (typep offset 'fixnum) (typep size 'fixnum)
       (let ((end (+ offset size)))
         (and (<= end +max-memory+) end))))

(defun expansion-cost (frame end gp)
  "Gas to grow FRAME's memory to cover END bytes (0 when it already does)."
  (declare (type frame frame) (type fixnum end))
  (let ((cur (frame-mem-size frame)))
    (if (<= end cur)
        0
        (- (memory-cost (* 32 (ceiling end 32)) gp) (memory-cost cur gp)))))

(defun grow-memory (frame end)
  (declare #.*hot* (type frame frame) (type fixnum end))
  (let ((new-size (* 32 (ceiling end 32)))
        (mem (frame-memory frame)))
    (when (> new-size (frame-mem-size frame))
      (when (> new-size (length mem))
        (let ((new (make-octets (max new-size (* 2 (length mem)) 1024))))
          (replace new mem :end2 (frame-mem-size frame))
          (setf (frame-memory frame) new)))
      (setf (frame-mem-size frame) new-size))
    frame))

(defun memory-slice (frame offset size)
  "A fresh copy of SIZE bytes of memory at OFFSET (already expanded)."
  (declare (type frame frame) (type fixnum size))
  (if (zerop size)
      +empty-code+
      (subseq (frame-memory frame) offset (+ offset size))))

;;; Stacks are 1024-slot simple-vectors reused by depth.

(defvar *stacks* (make-array 1026 :initial-element nil)
  "Stack per call depth; per thread (see WITH-EVM-THREAD).")

(defvar *keccak-scratch* (clempo.keccak:make-keccak)
  "Reusable Keccak state for KECCAK256; per thread (see WITH-EVM-THREAD).")
(defvar *keccak-out* (make-octets 32)
  "Reusable digest buffer for KECCAK256; per thread.")

(defmacro with-evm-thread (&body body)
  "Run BODY with this thread's own interpreter scratch: operand stacks, the code-analysis
cache and Keccak buffers. Every thread that executes EVM code besides the main one must
run inside this."
  `(let ((*stacks* (make-array 1026 :initial-element nil))
         (*analysis-cache* (make-hash-table :test 'eq :weakness :key))
         (*keccak-scratch* (clempo.keccak:make-keccak))
         (*keccak-out* (make-octets 32)))
     ,@body))

(declaim (inline acquire-stack))
(defun acquire-stack (depth)
  (declare (type fixnum depth))
  (let ((stacks *stacks*))
    (declare (type simple-vector stacks))
    (or (svref stacks depth)
        (setf (svref stacks depth) (make-array 1024 :initial-element 0)))))
