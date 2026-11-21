(defpackage #:clempo.modexp
  (:use #:cl)
  (:export #:modular-expt #:modexp))
(in-package #:clempo.modexp)

(defun modular-expt (base exponent modulus)
  "BASE^EXPONENT mod positive MODULUS, using a left-to-right sliding window."
  (declare #.clempo.base:*hot*
           (type integer base)
           (type (integer 0 *) exponent)
           (type (integer 1 *) modulus))
  (when (= modulus 1) (return-from modular-expt 0))
  (when (zerop exponent) (return-from modular-expt 1))
  (let* ((bits (integer-length exponent))
         (width (cond ((< bits 24) 1) ((< bits 80) 3)
                      ((< bits 240) 4) ((< bits 672) 5) (t 6)))
         (table (make-array (ash 1 (1- width))))
         (b (mod base modulus))
         (b2 (mod (* b b) modulus))
         (result 1)
         (i (1- bits)))
    (declare (type fixnum bits width i) (type integer b b2 result) (type simple-vector table))
    (setf (aref table 0) b)
    (loop for j of-type fixnum from 1 below (length table)
          do (setf (aref table j) (mod (* (aref table (1- j)) b2) modulus)))
    (loop while (>= i 0) do
      (if (not (logbitp i exponent))
          (setf result (mod (* result result) modulus) i (1- i))
          (let ((lo (max 0 (- i width -1))))
            (declare (type fixnum lo))
            (loop until (logbitp lo exponent) do (incf lo))
            (loop repeat (1+ (- i lo)) do
              (setf result (mod (* result result) modulus)))
            (setf result (mod (* result (aref table (ash (ldb (byte (1+ (- i lo)) lo) exponent) -1)))
                              modulus)
                  i (1- lo)))))
    result))

(defun modexp (base exponent modulus)
  "EIP-198 arithmetic over big-endian octets. Result has MODULUS's byte length.
Input parsing, zero padding and gas charging belong to the precompile wrapper."
  (declare (type clempo.base:octets base exponent modulus))
  (let ((m (clempo.base:octets->integer modulus)))
    (if (zerop m)
        (clempo.base:make-octets (length modulus))
        (clempo.base:integer->octets
         (modular-expt (clempo.base:octets->integer base)
                       (clempo.base:octets->integer exponent) m)
         (length modulus)))))
