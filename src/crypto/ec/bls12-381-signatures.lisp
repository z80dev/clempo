;;;; Commonware 2026.10.1 BLS POP signatures. All wire values are compressed octets.
(in-package #:clempo.bls12-381)


(defun bls-octets (value)
  (unless (typep value '(vector (unsigned-byte 8)))
    (error "Expected an octet vector, got ~S" value))
  value)

(defun bls-union-unique (namespace message)
  "Commonware unsigned LEB128 namespace length, namespace, then message."
  (bls-octets namespace)
  (bls-octets message)
  (let* ((n (length namespace))
         (prefix (max 1 (ceiling (integer-length n) 7)))
         (out (make-array (+ prefix n (length message)) :element-type '(unsigned-byte 8))))
    (loop for i below prefix for x = n then (ash x -7)
          do (setf (aref out i) (logior (logand x 127) (if (< i (1- prefix)) 128 0))))
    (replace out namespace :start1 prefix)
    (replace out message :start1 (+ prefix n))
    out))

(defun bls-min-sig-p (variant)
  (ecase variant (:min-sig t) (:min-pk nil)))

(defun bls-group-function (variant public g1 g2)
  (if (eq (bls-min-sig-p variant) (not public)) g1 g2))

(defun bls-encode (variant public point)
  (funcall (bls-group-function variant public #'g1-compress #'g2-compress) point))

(defun bls-decode (variant public bytes &optional allow-infinity)
  (bls-octets bytes)
  (let ((point (funcall (bls-group-function variant public #'g1-decompress #'g2-decompress) bytes)))
    (unless (or point allow-infinity) (error 'invalid-point))
    point))

(defun bls-add (variant public a b)
  (funcall (bls-group-function variant public #'g1-add #'g2-add) a b))

(defun bls-mul (variant public point scalar)
  (funcall (bls-group-function variant public #'g1-mul #'g2-mul) point scalar))

(defun bls-mul-secret (variant public point scalar)
  "BLS-MUL for a secret SCALAR: constant-time in SCALAR."
  (funcall (bls-group-function variant public #'g1-mul-secret #'g2-mul-secret) point scalar))

(defun bls-generator (variant public)
  (funcall (bls-group-function variant public #'g1-generator #'g2-generator)))

(defun bls-secret (secret)
  (unless (and (integerp secret) (< 0 secret +r+))
    (error "BLS secret must be an integer in [1,r)."))
  secret)

(defun bls-dst (variant proof)
  (clempo.base:string->octets
   (if (bls-min-sig-p variant)
       (if proof "BLS_POP_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_" "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_")
       (if proof "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_" "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_"))))

(defun bls-hash (variant message namespace proof dst)
  (when proof (bls-octets namespace))
  (funcall (if (bls-min-sig-p variant) #'hash-to-g1 #'hash-to-g2)
           (if namespace (bls-union-unique namespace message) (bls-octets message))
           :dst (if dst (bls-octets dst) (bls-dst variant proof))))

(defun bls-public-key (variant secret)
  (bls-encode variant t (bls-mul-secret variant t (bls-generator variant t) (bls-secret secret))))

(defun bls-sign (variant secret message &key namespace dst)
  "NIL namespace signs raw bytes; an empty octet namespace includes its zero prefix."
  (bls-encode variant nil (bls-mul-secret variant nil (bls-hash variant message namespace nil dst)
                                                (bls-secret secret))))

(defun bls-pair (variant public hash)
  (if (bls-min-sig-p variant) (cons hash public) (cons public hash)))

(defun bls-check (variant terms signature)
  ;; Private boundary: callers supply subgroup-checked decompressions, hashes
  ;; cleared into the subgroup, or sums/scalar multiples of those points.
  ;; The public PAIRING-CHECK still validates arbitrary caller points.
  (pairing-product-p
   (cons (if (bls-min-sig-p variant)
             (cons (g1-neg signature) (g2-generator))
             (cons (g1-neg (g1-generator)) signature)) terms)))

(defun bls-verify (variant public message signature &key namespace dst)
  (handler-case
      (bls-check variant
                 (list (bls-pair variant (bls-decode variant t public)
                                 (bls-hash variant message namespace nil dst)))
                 (bls-decode variant nil signature))
    (invalid-point () nil)))

(defun bls-pop-prove (variant secret namespace)
  (let ((public (bls-public-key variant secret)))
    (bls-encode variant nil (bls-mul-secret variant nil (bls-hash variant public namespace t nil)
                                                 (bls-secret secret)))))

(defun bls-pop-verify (variant public namespace proof)
  (handler-case
      (bls-check variant (list (bls-pair variant (bls-decode variant t public)
                                             (bls-hash variant public namespace t nil)))
                 (bls-decode variant nil proof))
    (invalid-point () nil)))

(defun bls-nonempty (values)
  (unless (and (typep values 'sequence) (plusp (length values)))
    (error "Expected a nonempty sequence."))
  values)

(defun bls-aggregate (variant signatures &key public-keys)
  (let ((sum nil))
    (map nil (lambda (bytes) (setf sum (bls-add variant public-keys sum
                                                (bls-decode variant public-keys bytes))))
         (bls-nonempty signatures))
    (bls-encode variant public-keys sum)))

(defun bls-aggregate-verify (variant entries signature)
  "ENTRIES contains (public namespace message). Keys require previously checked POPs."
  (handler-case
      (let ((terms nil))
        (map nil (lambda (entry)
                   (destructuring-bind (public namespace message) entry
                     (push (bls-pair variant (bls-decode variant t public)
                                     (bls-hash variant message namespace nil nil)) terms)))
             (bls-nonempty entries))
        (bls-check variant terms (bls-decode variant nil signature)))
    (invalid-point () nil)))

(defun bls-fast-aggregate-verify (variant publics namespace message signature)
  "Caller must have verified a POP for every unique public key."
  (handler-case
      (let ((seen (make-hash-table :test 'equalp)))
        (map nil (lambda (p)
                   (when (gethash p seen) (error "Duplicate aggregate public key."))
                   (setf (gethash p seen) t)) (bls-nonempty publics))
        (bls-verify variant (bls-aggregate variant publics :public-keys t) message signature
                    :namespace namespace))
    (invalid-point () nil)))

(defun bls-batch-verify (variant entries scalar-source)
  "ENTRIES: (public namespace message signature). SOURCE returns independent uniform
128-bit integers, including zero, once per entry. Never obtains entropy itself."
  (check-type scalar-source function)
  (handler-case
      (let ((terms nil) (sum nil))
        (map nil
             (lambda (entry)
               (destructuring-bind (public namespace message signature) entry
                 (let ((pk (bls-decode variant t public))
                       (sig (bls-decode variant nil signature))
                       (weight (funcall scalar-source)))
                   (unless (typep weight '(unsigned-byte 128))
                     (error "Batch source must return an unsigned 128-bit scalar."))
                   (push (bls-pair variant (bls-mul variant t pk weight)
                                   (bls-hash variant message namespace nil nil)) terms)
                   (setf sum (bls-add variant nil sum (bls-mul variant nil sig weight))))))
             (bls-nonempty entries))
        (bls-check variant terms sum))
    (invalid-point () nil)))

(defun bls-batch-verify-same-signer (variant public entries scalar-source)
  "ENTRIES contains (namespace message signature). Check one random linear
combination with two pairing terms, regardless of the number of entries.
SOURCE must return independent uniform unsigned 128-bit scalars, including zero."
  (check-type scalar-source function)
  (handler-case
      (let ((pk (bls-decode variant t public)) (hash-sum nil) (signature-sum nil))
        (map nil
             (lambda (entry)
               (destructuring-bind (namespace message signature) entry
                 (bls-octets namespace)
                 (let ((sig (bls-decode variant nil signature))
                       (hm (bls-hash variant message namespace nil nil))
                       (weight (funcall scalar-source)))
                   (unless (typep weight '(unsigned-byte 128))
                     (error "Batch source must return an unsigned 128-bit scalar."))
                   (setf hash-sum
                         (bls-add variant nil hash-sum (bls-mul variant nil hm weight))
                         signature-sum
                         (bls-add variant nil signature-sum (bls-mul variant nil sig weight))))))
             (bls-nonempty entries))
        (bls-check variant (list (bls-pair variant pk hash-sum)) signature-sum))
    (invalid-point () nil)))

(defun bls-batch-verify-same-message (variant entries namespace message scalar-source)
  "ENTRIES contains (public signature); returns invalid zero-based entry positions."
  (let ((expanded (map 'list (lambda (entry) (list (first entry) namespace message (second entry)))
                       (bls-nonempty entries))))
    (unless (bls-batch-verify variant expanded scalar-source)
      (loop for (public ns msg signature) in expanded for i from 0
            unless (bls-verify variant public msg signature :namespace ns) collect i))))

(defun bls-scalar-power (base exponent)
  (loop with result = 1 for e = exponent then (ash e -1) while (plusp e)
        when (oddp e) do (setf result (mod (* result base) +r+))
        do (setf base (mod (* base base) +r+)) finally (return result)))

(defun bls-participant-scalar (index total &key (mode :nonzero-counter))
  (unless (and (typep total '(integer 1 4294967295))
               (typep index '(unsigned-byte 32)) (< index total))
    (error "Invalid BLS participant index or total."))
  (ecase mode
    (:nonzero-counter (1+ index))
    (:roots-of-unity
     (let ((size (ash 1 (integer-length (1- total)))))
       (bls-scalar-power 7 (* (/ (1- +r+) size) index))))))

(defun bls-partial-public-key (variant coefficients index total &key (mode :nonzero-counter))
  "COEFFICIENTS are compressed public polynomial coefficients, constant first."
  (let ((x (bls-participant-scalar index total :mode mode)) (sum nil))
    (loop for i downfrom (1- (length (bls-nonempty coefficients))) to 0
          do (setf sum (bls-add variant t (bls-mul variant t sum x)
                               (bls-decode variant t (elt coefficients i) t))))
    (bls-encode variant t sum)))

(defun bls-threshold-sign (variant secret index total namespace message &key (mode :nonzero-counter))
  (bls-participant-scalar index total :mode mode)
  (cons index (bls-sign variant secret message :namespace namespace)))

(defun bls-threshold-verify (variant coefficients total namespace message partial &key (mode :nonzero-counter))
  (bls-verify variant (bls-partial-public-key variant coefficients (car partial) total :mode mode)
              message (cdr partial) :namespace namespace))

(defun bls-threshold-pop-prove (variant secret index total group-public namespace &key (mode :nonzero-counter))
  (bls-participant-scalar index total :mode mode)
  (bls-decode variant t group-public)
  (cons index (bls-encode variant nil
                          (bls-mul-secret variant nil (bls-hash variant group-public namespace t nil)
                                          (bls-secret secret)))))

(defun bls-threshold-pop-verify (variant coefficients total namespace partial &key (mode :nonzero-counter))
  (handler-case
      (bls-check variant
                 (list (bls-pair variant
                                 (bls-decode variant t (bls-partial-public-key variant coefficients
                                                                              (car partial) total :mode mode))
                                 (bls-hash variant (elt coefficients 0) namespace t nil)))
                 (bls-decode variant nil (cdr partial)))
    (invalid-point () nil)))

(defun bls-threshold-recover (variant threshold total partials &key (mode :nonzero-counter))
  "Return an UNVERIFIED candidate; select the lowest THRESHOLD distinct indices.
Partials are (zero-based-participant . compressed-signature). Duplicate indices
are rejected, as required by Rust's recovery contract."
  (unless (and (integerp threshold) (<= 1 threshold total)) (error "Invalid threshold."))
  (let ((sorted (sort (map 'list #'identity partials) #'< :key #'car)) (seen (make-hash-table)))
    (dolist (partial sorted)
      (unless (typep (car partial) '(unsigned-byte 32)) (error "Invalid participant."))
      (when (gethash (car partial) seen) (error "Duplicate participant."))
      (setf (gethash (car partial) seen) t))
    (when (< (length sorted) threshold) (error "Not enough partial signatures."))
    (let* ((selected (subseq sorted 0 threshold))
           (xs (mapcar (lambda (p) (bls-participant-scalar (car p) total :mode mode)) selected))
           (sum nil))
      (loop for partial in selected for x in xs
            do (let ((numerator 1) (denominator 1))
                 (dolist (y xs)
                   (unless (= x y)
                     (setf numerator (mod (* numerator (- y)) +r+)
                           denominator (mod (* denominator (- x y)) +r+))))
                 (setf sum (bls-add variant nil sum
                                    (bls-mul variant nil (bls-decode variant nil (cdr partial))
                                             (mod (* numerator (bls-scalar-power denominator (- +r+ 2))) +r+))))))
      (bls-encode variant nil sum))))
