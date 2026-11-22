(in-package #:clempo.evm)

;;; Code analysis: valid jump destinations and the value of every PUSH, computed once per
;;; code vector and cached by identity.

(defstruct (analysis (:constructor %make-analysis (code jumpdests pushes)))
  (code +empty-code+ :type octets)     ; the code vector analysed: the caches' key
  (jumpdests #* :type simple-bit-vector)
  (pushes #() :type simple-vector)
  (jit nil))                         ; compile-cache.lisp's per-code state

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
    (%make-analysis code jumpdests pushes)))

;;; Analyses are cached by code vector in two layers.
;;;
;;; The store, shared by every thread, keeps them in two generations of weak tables. A
;;; lookup that finds an analysis in the old generation moves it into the young one;
;;; once the young one has taken in *ANALYSIS-CACHE-CODE-BYTES* bytes of code it becomes
;;; the old one and the previous old one is dropped. The store therefore holds the
;;; analyses of at most twice that much code (about 9 bytes per code byte, plus the
;;; JIT state) however many code vectors the world keeps alive, while a code in steady use
;;; is promoted every generation and never recomputed. Weakness also drops an analysis
;;; as soon as its code vector is garbage (an eth_call's overridden code).
;;;
;;; In front of the store, a direct-mapped L1 of whole analyses keyed by the code
;;; vector's address answers the frame-by-frame lookup without a lock. A slot holds one
;;; analysis, which names its code, so the global L1 may be shared by threads outside
;;; WITH-EVM-THREAD: a racing write costs a miss, never a wrong answer. A collection that
;;; moves a code vector also only costs a miss.

(defparameter *analysis-cache-code-bytes* (* 4 1024 1024)
  "Bytes of code analysed into the young generation before it ages (see above).")

(defun make-analysis-table ()
  (make-hash-table :test 'eq :weakness :key :synchronized t))

(defstruct (analysis-store (:constructor make-analysis-store ()))
  (young (make-analysis-table) :type hash-table)
  (old (make-analysis-table) :type hash-table)
  (bytes 0 :type sb-ext:word))          ; code bytes taken into YOUNG

(defvar *analysis-store* (make-analysis-store)
  "The analyses of every thread.")

(defbound +analysis-l1-size+ 64
  "Slots of the direct-mapped analysis L1."
  ;; CACHED-ANALYSIS indexes it by masking an address with (1- size).
  (= (logcount +analysis-l1-size+) 1))

(defun make-analysis-l1 () (make-array +analysis-l1-size+ :initial-element nil))

(defvar *analysis-l1* (make-analysis-l1)
  "Direct-mapped analysis cache in front of *ANALYSIS-STORE*; per thread (see
WITH-EVM-THREAD).")

(defun store-analysis (store a)
  "Put the analysis A into STORE's young generation; the insert that carries it over its
budget ages it, once."
  (let* ((code (analysis-code a))
         (cap *analysis-cache-code-bytes*)
         (len (length code)))
    (setf (gethash code (analysis-store-young store)) a)
    (let ((before (sb-ext:atomic-incf (analysis-store-bytes store) len)))
      (when (and (<= before cap) (> (+ before len) cap))
        (setf (analysis-store-old store) (analysis-store-young store)
              (analysis-store-young store) (make-analysis-table)
              (analysis-store-bytes store) 0)))
    a))

(defun stored-analysis (code evicted)
  "CODE's analysis from *ANALYSIS-STORE*, computed and stored on a miss. EVICTED, the
analysis an L1 slot is giving up for it (or NIL), goes back into the young generation:
an analysis in steady use in an L1 never meets the store, so it is kept alive there."
  (declare (type octets code))
  (let ((store *analysis-store*))
    (when (and evicted (not (gethash (analysis-code evicted) (analysis-store-young store))))
      (store-analysis store evicted))
    (or (gethash code (analysis-store-young store))
        (store-analysis store (or (gethash code (analysis-store-old store))
                                  (analyze-code code))))))

(defun code-analysis (code)
  (declare #.*hot* (type octets code))
  (let* ((l1 *analysis-l1*)
         (i (logand (ash (sb-kernel:get-lisp-obj-address code) -4) (1- +analysis-l1-size+)))
         (a (svref l1 i)))
    (declare (type simple-vector l1) (type fixnum i))
    (if (and a (eq (analysis-code (the analysis a)) code))
        a
        (setf (svref l1 i) (stored-analysis code a)))))

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
  (kind :call :type keyword)
  ;; Gas the frame's child messages took and did not give back (stipends aside): what the
  ;; JIT subtracts from the frame's own spending when it measures how hot its code is.
  (child-gas 0 :type fixnum))

;;; Memory: MEMORY is a buffer at least MEM-SIZE long; MEM-SIZE is the active size in
;;; bytes, a multiple of 32.

(defbound +max-memory+ (ash 1 32)
  "Memory beyond 4 GiB is out of gas for any reachable gas limit."
  ;; MEMORY-COST squares the word count in fixnum arithmetic.
  (typep (* (ceiling +max-memory+ 32) (ceiling +max-memory+ 32)) 'fixnum)
  ;; GROW-MEMORY at most doubles a buffer of this size.
  (typep (* 2 +max-memory+) 'fixnum)
  ;; Out of gas indeed: at the memory schedule (spec.lisp: 3 per word, quadratic
  ;; divisor 512) this much memory costs over 2^40 gas, beyond any block gas limit
  ;; and the RPC gas cap.
  (> (+ (* 3 (ceiling +max-memory+ 32)) (floor (expt (ceiling +max-memory+ 32) 2) 512))
     (ash 1 40)))

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

;;; Stacks are +STACK-LIMIT+-slot simple-vectors, one per call depth, reused.

(defbound +max-depth+ 1024
  "Call depth at which a further CALL or CREATE fails (call.lisp).")

(defbound +stack-limit+ 1024
  "Operand stack slots of a frame; an instruction that would push past them halts with
:STACK-OVERFLOW before it writes (interpreter.lisp, compile.lisp)."
  ;; Stack pointers are declared (INTEGER 0 +STACK-LIMIT+) and must stay fixnums.
  (typep +stack-limit+ 'fixnum)
  ;; The widest instruction (a DUP16 or SWAP16) fits in a stack.
  (>= +stack-limit+ 17))

(defbound +depth-stacks+ (+ +max-depth+ 2)
  "Stacks kept per thread, indexed by the depth of the frame that uses one."
  ;; Frames run at depths 0 through +MAX-DEPTH+: ACQUIRE-STACK's index stays inside.
  (> +depth-stacks+ +max-depth+))

(defvar *stacks* (make-array +depth-stacks+ :initial-element nil)
  "Stack per call depth; per thread (see WITH-EVM-THREAD).")

(defvar *keccak-scratch* (clempo.keccak:make-keccak)
  "Reusable Keccak state for KECCAK256; per thread (see WITH-EVM-THREAD).")
(defvar *keccak-out* (make-octets 32)
  "Reusable digest buffer for KECCAK256; per thread.")

(defmacro with-evm-thread (&body body)
  "Run BODY with this thread's own interpreter scratch: operand stacks, the code-analysis
L1 and Keccak buffers. Every thread that executes EVM code besides the main one must
run inside this."
  `(let ((*stacks* (make-array +depth-stacks+ :initial-element nil))
         (*analysis-l1* (make-analysis-l1))
         (*keccak-scratch* (clempo.keccak:make-keccak))
         (*keccak-out* (make-octets 32)))
     ,@body))

(declaim (inline acquire-stack))
(defun acquire-stack (depth)
  (declare (type fixnum depth))
  (let ((stacks *stacks*))
    (declare (type simple-vector stacks))
    (or (svref stacks depth)
        (setf (svref stacks depth) (make-array +stack-limit+ :initial-element 0)))))
