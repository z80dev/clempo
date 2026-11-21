(in-package #:clempo.p256)

(defvar *p256-scratch* nil
  "Optional worker-local verification arena. Bind a separate arena per worker;
do not share an arena between concurrent or reentrant verification calls.")

(defun make-p256-scratch ()
  "Allocate a reusable P256 verification arena; contents are overwritten per call."
  (make-array 352 :element-type '(unsigned-byte 64) :initial-element 0))

(defun p256-verify (hash32 r s qx qy &optional (scratch *p256-scratch*))
  "EIP-7951 P256VERIFY semantics. Verifies an already computed 32-byte digest.
High-s signatures are valid here. Invalid scalar/coordinate ranges, infinity,
off-curve public keys and an infinity verification result return NIL.
SCRATCH is NIL or an arena from MAKE-P256-SCRATCH, exclusively owned during this
call. NIL allocates a fresh arena; supply or bind scratch to reuse storage."
  (declare #.clempo.base:*hot*)
  (unless (and (typep hash32 'clempo.base:octets) (= (length hash32) 32)
               (integerp r) (< 0 r +p256-n+)
               (integerp s) (< 0 s +p256-n+)
               (integerp qx) (<= 0 qx) (< qx +p256-p+)
               (integerp qy) (<= 0 qy) (< qy +p256-p+))
    (return-from p256-verify nil))
  (when (and scratch (not (typep scratch '(simple-array (unsigned-byte 64) (352)))))
    (error 'type-error :datum scratch :expected-type '(simple-array (unsigned-byte 64) (352))))
  ;; No integer products, inverses, digest conversion, point pairs or digit
  ;; arrays are allocated. The NIL fallback allocates only the operation arena.
  (let ((arena (or scratch (make-p256-scratch))))
    (declare (type p256-arena arena))
    (pf-from-integer! arena 32 qx) (pf-from-integer! arena 36 qy)
    (pf-one! arena 40)
    (unless (p256-affine-on-curve-p arena 32 arena 236)
      (return-from p256-verify nil))
    (ps-from-integer! arena 0 r) (ps-from-integer! arena 4 s)
    (ps-inv! arena 4 arena 4 arena 236)
    (dotimes (i 4)
      (let ((word 0))
        (declare (type (unsigned-byte 64) word))
        (dotimes (j 8)
          (setf word (ldb (byte 64 0)
                          (logior (ash word 8)
                                  (aref (the clempo.base:octets hash32) (+ (* (- 3 i) 8) j))))))
        (setf (aref arena (+ 8 i)) word)))
    ;; REDC also accepts the unreduced digest (< R) times R² mod n (< n).
    (ps-mul! arena 8 arena 8 ps-r2 0)
    (ps-mul! arena 12 arena 8 arena 4)
    (ps-mul! arena 16 arena 0 arena 4)
    (ps-mul! arena 12 arena 12 *p256-integer-one* 0)
    (ps-mul! arena 16 arena 16 *p256-integer-one* 0)
    (p256-table! arena 44 arena 32 arena 236)
    (p256-joint! arena 20 arena 12 arena 16 arena 44 arena 236)
    (when (p256-zero-p arena 28) (return-from p256-verify nil))
    ;; Compare X = r Z², and (only if r+n<p) X = (r+n) Z².
    ;; This preserves EIP-7951's x mod n rule without an affine inversion.
    (pf-from-integer! arena 0 r)
    (pf-square! arena 236 arena 28)
    (pf-mul! arena 240 arena 0 arena 236)
    (or (p256-equal-p arena 20 arena 240)
        (and (< r #.(- +p256-p+ +p256-n+))
             (progn
               (pf-add! arena 0 arena 0 *p256-constants* 16)
               (pf-mul! arena 240 arena 0 arena 236)
               (p256-equal-p arena 20 arena 240))))))

(defun p256-verify-low-s (hash32 r s qx qy &optional (scratch *p256-scratch*))
  "Tempo P256/WebAuthn transaction signature rule, including low-s validation.
The caller supplies the prehashed or WebAuthn digest, not the raw challenge.
SCRATCH has the same ownership and reuse contract as P256-VERIFY."
  (declare #.clempo.base:*hot*)
  (and (integerp s) (<= 1 s #.(ash +p256-n+ -1))
       (p256-verify hash32 r s qx qy scratch)))
