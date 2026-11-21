(in-package #:clempo.simplex)

;;; Leader election (commonware-consensus 2026.10.1 simplex/elector.rs) with rotating terms
;;; (one view per term), the only shape Tempo uses. ELECT returns a participant index.
;;;
;;;   round robin  (epoch + view) mod n through a permutation, optionally shuffled once by
;;;                sorting indices on sha256(seed || varint(index)); ignores certificates.
;;;   random       view 1: (epoch + view) mod n. Later views: the seed signature (48-byte
;;;                compressed G1) of the certificate that unlocked the view, as a big-endian
;;;                integer mod n (V0), or sha256 of it mod n (V1). Tempo picks V0 for epochs
;;;                whose preceding boundary block is before T12 and V1 from T12 on.

(defstruct (elector (:constructor nil)))

(defstruct (round-robin-elector (:include elector)
                                (:constructor %make-round-robin-elector (seed)))
  (seed nil :type (or null octets))
  (permutations (make-hash-table) :type hash-table)) ; n -> simple-vector, built lazily

(defun make-round-robin-elector (&key seed)
  "commonware's RoundRobin: rotation by (epoch + view), shuffled by SEED (octets) if given."
  (%make-round-robin-elector seed))

(defstruct (random-elector (:include elector)
                           (:constructor make-random-elector (&optional (version :v1))))
  (version :v1 :type (member :v0 :v1)))

(defun elector-from-designator (designator)
  "An elector from a make-voter :ELECTOR argument: an elector, :ROUND-ROBIN, :RANDOM-V0 or
:RANDOM-V1."
  (etypecase designator
    (elector designator)
    ((eql :round-robin) (make-round-robin-elector))
    ((eql :random-v0) (make-random-elector :v0))
    ((eql :random-v1) (make-random-elector :v1))))

(defun elector-needs-certificate-p (elector)
  "True for electors that derive the leader from the unlocking certificate (commonware's
Dynamic mode): their leader is known only once that certificate is."
  (random-elector-p elector))

(defgeneric elect (elector epoch view n seed)
  (:documentation "The leader index in [0, N) for (EPOCH, VIEW). SEED is the seed signature
\(48 octets) of the certificate that unlocked VIEW, NIL for view 1."))

(defun octets-mod (octets n)
  "commonware_utils::modulo: OCTETS as a big-endian integer, mod N."
  (mod (octets->integer octets) n))

(defun round-robin-permutation (elector n)
  (let ((table (round-robin-elector-permutations elector)))
    (or (gethash n table)
        (setf (gethash n table)
              (let ((indices (loop for i below n collect i))
                    (seed (round-robin-elector-seed elector)))
                (coerce (if seed
                            (let ((keyed (mapcar (lambda (i)
                                                   (cons (clempo.sha2:sha256
                                                          (octets-concat seed (varint-octets i)))
                                                         i))
                                                 indices)))
                              (mapcar #'cdr (stable-sort keyed #'octets< :key #'car)))
                            indices)
                        'simple-vector))))))

(defun wrapping-add (epoch view) (ldb (byte 64 0) (+ epoch view)))

(defun octets< (a b)
  (loop for x across a for y across b
        when (< x y) return t
        when (> x y) return nil
        finally (return (< (length a) (length b)))))

(defmethod elect ((elector round-robin-elector) epoch view n seed)
  (declare (ignore seed))
  (invariant (plusp n) "no participants")
  (svref (round-robin-permutation elector n) (mod (wrapping-add epoch view) n)))

(defmethod elect ((elector random-elector) epoch view n seed)
  (invariant (plusp n) "no participants")
  (invariant (or seed (= view 1)) "random election of view ~D needs a seed" view)
  (cond ((null seed) (mod (wrapping-add epoch view) n))
        ((eq (random-elector-version elector) :v0) (octets-mod seed n))
        (t (octets-mod (clempo.sha2:sha256 seed) n))))
