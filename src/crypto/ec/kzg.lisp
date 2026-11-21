;;; KZG proof verification and the EIP-4844 point evaluation precompile (0x0a).
;;; Spec: c-kzg-4844 2.1.8 verify_kzg_proof (src/eip4844), revm precompile kzg_point_evaluation.
;;; BLS12-381 arithmetic comes from clempo.bls12-381; this file holds no field code.

(defpackage #:clempo.kzg
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:bls #:clempo.bls12-381))
  (:export #:+versioned-hash-version-kzg+ #:+field-elements-per-blob+
           #:+point-evaluation-input-length+ #:+point-evaluation-gas+
           #:+tau-g2-hex+ #:tau-g2
           #:invalid-kzg-input #:kzg-to-versioned-hash #:verify-kzg-proof
           #:point-evaluation-return-value #:point-evaluation))

(in-package #:clempo.kzg)

(defconstant +versioned-hash-version-kzg+ 1)
(defconstant +field-elements-per-blob+ 4096)
(defconstant +point-evaluation-input-length+ 192)
(defconstant +point-evaluation-gas+ 50000)

;;; [tau]G2, compressed: line 4100 of c-kzg-2.1.8 src/trusted_setup.txt
;;; (mainnet KZG ceremony output; line 1 = 4096 G1 points, line 2 = 65 G2 points,
;;; lines 3-4098 G1 Lagrange, 4099 = G2 generator, 4100 = [tau]G2).
(define-constant +tau-g2-hex+
  "b5bfd7dd8cdeb128843bc287230af38926187075cbfbefa81009a2ce615ac53d2914e5870cb452d2afaaab24f3499f72185cbfee53492714734429b7b38608e23926c911cceceac9a36851477ba4c60b087041de621000edc98edada20c1def2")

(defvar *tau-g2* nil)

(define-condition invalid-kzg-input (error) ()
  (:report "invalid KZG input: non-canonical scalar or invalid G1 point"))

(defun tau-g2 ()
  "Decompressed, subgroup-checked [tau]G2."
  (or *tau-g2* (setf *tau-g2* (bls:g2-decompress (hex->octets +tau-g2-hex+)))))

(defun kzg-to-versioned-hash (commitment)
  "0x01 || sha256(commitment)[1:]"
  (let ((h (clempo.sha2:sha256 commitment)))
    (setf (aref h 0) +versioned-hash-version-kzg+)
    h))

(defun %scalar (octets start)
  "Big-endian 32-byte field element at START, or NIL if not canonical (>= r)."
  (let ((v (octets->integer octets :start start :end (+ start 32))))
    (and (< v bls:+r+) v)))

(defun %point (decompress octets)
  (handler-case (values (funcall decompress octets) t)
    (bls:invalid-point () (values nil nil))))

(defun verify-kzg-proof (commitment z y proof)
  "COMMITMENT, PROOF: 48-octet compressed G1. Z, Y: 32-octet big-endian scalars.
Returns T if the proof is valid, NIL if well-formed but wrong. Signals
INVALID-KZG-INPUT for non-canonical scalars or invalid points (c-kzg C_KZG_BADARGS)."
  (unless (and (typep commitment 'octets) (= (length commitment) 48)
               (typep proof 'octets) (= (length proof) 48)
               (typep z 'octets) (= (length z) 32)
               (typep y 'octets) (= (length y) 32))
    (error 'invalid-kzg-input))
  (let ((zi (%scalar z 0)) (yi (%scalar y 0)))
    (unless (and zi yi)
      (error 'invalid-kzg-input))
    (multiple-value-bind (c cok) (%point #'bls:g1-decompress commitment)
      (multiple-value-bind (p pok) (%point #'bls:g1-decompress proof)
        (unless (and cok pok) (error 'invalid-kzg-input))
        ;; e(C - [y]G1, G2) == e(P, [tau]G2 - [z]G2)
        ;; <=> e(C - [y]G1, -G2) * e(P, [tau - z]G2) == 1
        (let ((c-y (bls:g1-add c (bls:g1-neg (bls:g1-mul (bls:g1-generator) yi))))
              (x-z (bls:g2-add (tau-g2) (bls:g2-neg (bls:g2-mul (bls:g2-generator) zi)))))
          (values (bls:pairing-check
                   (list (cons c-y (bls:g2-neg (bls:g2-generator)))
                         (cons p x-z)))))))))

(defun point-evaluation-return-value ()
  "FIELD_ELEMENTS_PER_BLOB || BLS_MODULUS, 64 octets."
  (octets-concat (integer->octets +field-elements-per-blob+ 32)
                 (integer->octets bls:+r+ 32)))

(defun point-evaluation (input)
  "EIP-4844 precompile body. INPUT octets: versioned_hash(32) z(32) y(32)
commitment(48) proof(48). Returns the 64-octet output on success, NIL on any failure
(wrong length, hash mismatch, bad encoding, or false proof). Gas is +point-evaluation-gas+."
  (when (/= (length input) +point-evaluation-input-length+)
    (return-from point-evaluation nil))
  (let ((vh (subseq input 0 32))
        (z (subseq input 32 64))
        (y (subseq input 64 96))
        (commitment (subseq input 96 144))
        (proof (subseq input 144 192)))
    (unless (equalp vh (kzg-to-versioned-hash commitment))
      (return-from point-evaluation nil))
    (and (handler-case (verify-kzg-proof commitment z y proof)
           (invalid-kzg-input () nil))
         (point-evaluation-return-value))))
