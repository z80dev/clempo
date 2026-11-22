;;; Cache only the pure mathematical ECDSA predicate, never certificate validity,
;;; pinned roots, policy, parsing or gas. Context storage is private to one thread.
(in-package #:clempo.p384)

(clempo.base:defbound +p384-digest-bytes+ 48
  "Bytes of a cached message digest: the whole scalar PS-FROM-DIGEST! imports."
  (= +p384-digest-bytes+ (* +p384-limbs+ +p384-limb-bytes+)))

(defconstant +p384-cache-capacity+ 64)
(defstruct (p384-verification-cache (:constructor make-p384-verification-cache ()))
  (keys (make-array (* 4 +p384-cache-capacity+) :initial-element 0)
        :type simple-vector :read-only t)
  (digests (clempo.base:make-octets (* +p384-digest-bytes+ +p384-cache-capacity+))
           :type clempo.base:octets :read-only t)
  (count 0 :type (integer 0 64))
  (next 0 :type (integer 0 63)))

(defun make-p384-scratch (&key memoize)
  "Private nonreentrant native storage. MEMOIZE adds a bounded positive-only cache.
The default verifies the arithmetic every time. No global cache is installed."
  (let ((scratch (%make-p384-scratch)))
    (when memoize
      (setf (p384-scratch-cache scratch) (make-p384-verification-cache)))
    scratch))

(defun p384-cache-hit-p (cache hash r s qx qy)
  (declare #.clempo.base:*hot* (type p384-verification-cache cache)
           (type (simple-array (unsigned-byte 8) (#.+p384-digest-bytes+)) hash)
           (type (integer 1 #.(1- +p384-n+)) r s)
           (type (integer 0 #.(1- +p384-p+)) qx qy))
  (let ((keys (p384-verification-cache-keys cache))
        (digests (p384-verification-cache-digests cache)))
    (dotimes (i (p384-verification-cache-count cache) nil)
      (let ((k (* i 4)) (d (* i +p384-digest-bytes+)))
        (declare (type fixnum k d))
        (when (and (= r (svref keys k)) (= s (svref keys (+ k 1)))
                   (= qx (svref keys (+ k 2))) (= qy (svref keys (+ k 3))))
          (when (loop for j of-type fixnum below +p384-digest-bytes+
                      always (= (aref hash j) (aref digests (+ d j))))
            (return t)))))))

(defun p384-cache-store! (cache hash r s qx qy)
  "Store a proven-valid predicate. Copy mutable digest bytes, not their reference."
  (declare #.clempo.base:*hot* (type p384-verification-cache cache)
           (type (simple-array (unsigned-byte 8) (#.+p384-digest-bytes+)) hash))
  (let* ((i (p384-verification-cache-next cache)) (k (* i 4)) (d (* i +p384-digest-bytes+))
         (keys (p384-verification-cache-keys cache))
         (digests (p384-verification-cache-digests cache)))
    (declare (type fixnum i k d))
    (setf (svref keys k) r (svref keys (+ k 1)) s
          (svref keys (+ k 2)) qx (svref keys (+ k 3)) qy)
    ;; Keep this fixed digest copy inline, without a bulk-copy helper call.
    (dotimes (j +p384-digest-bytes+) (setf (aref digests (+ d j)) (aref hash j)))
    (setf (p384-verification-cache-next cache) (if (= i 63) 0 (1+ i)))
    (when (< (p384-verification-cache-count cache) +p384-cache-capacity+)
      (incf (p384-verification-cache-count cache))))
  (values))
