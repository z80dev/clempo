;;; ct-timing: a dudect-style leakage test (Reparaz, Balasch and Verbauwhede, "Dude, is
;;; my code constant time?", 2017) for every secret-key operation in src/crypto.
;;;
;;; Each sample runs one operation on a secret drawn from one of two classes, chosen at
;;; random per sample: a fixed secret (sparse, low Hamming weight, so a variable-time
;;; implementation does visibly less work) or a fresh random one. All inputs exist
;;; before timing starts, each a separate object. Welch's t-test then compares the two classes' timings, over
;;; all samples and over samples cropped at several percentiles (which drops GC pauses
;;; and interrupts). |t| above 4.5 rejects "the timing is independent of the secret".
;;; A variable-time control is measured the same way to show the test can see a leak.
(in-package #:clempo.dev)

(defstruct (ct-case (:constructor ct-case (name samples batch prepare run &optional control)))
  name samples batch prepare run control)

(defvar *ct-a/a* nil
  "When true, both classes draw random secrets: an A/A run that measures the test's own
false-positive rate on this host.")

(defun ct-random-scalar (limit) (1+ (clempo.io:random-below (1- limit))))

(defun ct-sparse-octets (n)
  "N octets with only the top and bottom bits set."
  (let ((out (make-octets n)))
    (setf (aref out 0) #x40 (aref out (1- n)) 1)
    out))

(defun ct-cases ()
  (let* ((n clempo.secp256k1:+secp256k1-n+)
         (r clempo.bls12-381:+r+)
         (p256-n #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)
         (sparse (+ (ash 1 254) 1))
         (hash (clempo.keccak:keccak256 (string->octets "clempo ct-timing")))
         (message (string->octets "clempo ct-timing message"))
         (secp-peer (clempo.secp256k1:private->public (ct-random-scalar n)))
         (p256-peer (clempo.ecdh:p256-ecdh-public-key (integer->octets (ct-random-scalar p256-n) 32)))
         (x-peer (clempo.x25519:x25519-public-key (clempo.io:random-octets 32)))
         (fixed-aes (clempo.aes:make-aes (clempo.io:random-octets 32)))
         (plain (clempo.io:random-octets 256)) (block (clempo.io:random-octets 16))
         (nonce (make-octets 12)))
;; Every fixed-class input is a fresh object, allocated in the same interleaved order as
    ;; the random ones: one shared object would stay cached and look faster.
    (flet ((scalar (limit) (lambda (class) (if (zerop class) (+ (ash 1 254) 1) (ct-random-scalar limit))))
           (octets (k) (lambda (class) (if (zerop class) (ct-sparse-octets k) (clempo.io:random-octets k)))))
      (list
       (ct-case "secp256k1-sign" 20000 1 (scalar n)
                (lambda (key) (clempo.secp256k1:sign hash key)))
       (ct-case "secp256k1-public-key" 20000 1 (scalar n)
                (lambda (key) (clempo.secp256k1:private->public key)))
       (ct-case "secp256k1-ecdh" 10000 1 (scalar n)
                (lambda (key) (clempo.secp256k1:ecdh key secp-peer)))
       (ct-case "ed25519-sign" 20000 1 (octets 32)
                (lambda (seed) (clempo.ed25519:ed25519-sign seed message)))
       (ct-case "x25519" 10000 1 (octets 32)
                (lambda (secret) (clempo.x25519:x25519 secret x-peer)))
       (ct-case "p256-ecdh" 10000 1 (lambda (class) (integer->octets (funcall (scalar p256-n) class) 32))
                (lambda (secret) (clempo.ecdh:p256-ecdh-shared secret p256-peer)))
       (ct-case "bls-sign-g1" 3000 1 (scalar r)
                (lambda (secret) (clempo.bls12-381:bls-sign :min-sig secret message)))
       (ct-case "bls-public-key-g2" 2000 1 (scalar r)
                (lambda (secret) (clempo.bls12-381:bls-public-key :min-sig secret)))
       (ct-case "dkg-share-mul" 20000 50 (scalar r)
                (lambda (a) (clempo.bls12-381:fr-mul a sparse)))
       (ct-case "aes-key-schedule-and-block" 20000 1 (octets 32)
                (lambda (key) (clempo.aes:aes-encrypt-block (clempo.aes:make-aes key) block)))
       (ct-case "aes-256-block" 20000 64 (octets 16)
                (lambda (block) (clempo.aes:aes-encrypt-block fixed-aes block)))
       #+arm64
       (ct-case "aes-256-block-bitsliced" 20000 16 (octets 16)
                (lambda (block)
                  (let ((saved clempo.aes-arm64:**native**))
                    (setf clempo.aes-arm64:**native** nil)
                    (unwind-protect (clempo.aes:aes-encrypt-block fixed-aes block)
                      (setf clempo.aes-arm64:**native** saved)))))
       (ct-case "aes-128-gcm" 20000 1 (octets 16)
                (lambda (key) (clempo.gcm:gcm-encrypt (clempo.gcm:make-gcm key) nonce plain)))
       (ct-case "chacha20-poly1305" 20000 1 (octets 32)
                (lambda (key) (clempo.chacha20poly1305:aead-encrypt key nonce plain)))
       (ct-case "control-variable-time-wnaf" 20000 1 (scalar n)
                (lambda (k) (clempo.secp256k1::native-generator-mul k))
                t)))))

(defun ct-welch (times classes keep)
  "Welch's t for the samples whose time is below KEEP."
  (let ((count (vector 0 0)) (sum (vector 0d0 0d0)) (squares (vector 0d0 0d0)))
    (loop for x across times for c across classes
          when (< x keep) do (incf (aref count c)) (incf (aref sum c) x) (incf (aref squares c) (* x x)))
    (if (or (< (aref count 0) 2) (< (aref count 1) 2))
        0d0
        (flet ((mean (c) (/ (aref sum c) (aref count c)))
               (var (c) (/ (- (aref squares c) (/ (expt (aref sum c) 2) (aref count c)))
                           (1- (aref count c)))))
          (let ((se (sqrt (+ (/ (var 0) (aref count 0)) (/ (var 1) (aref count 1))))))
            (if (zerop se) 0d0 (/ (- (mean 0) (mean 1)) se)))))))

(defun ct-run-case (case samples)
  (let* ((classes (make-array samples))
         (inputs (make-array samples))
         (times (make-array samples :element-type 'double-float))
         (prepare (ct-case-prepare case)) (run (ct-case-run case)) (batch (ct-case-batch case)))
    (dotimes (i samples)
      (let ((class (ldb (byte 1 0) (aref (clempo.io:random-octets 1) 0))))
        (setf (aref classes i) class (aref inputs i) (funcall prepare (if *ct-a/a* 1 class)))))
    ;; Copy every input once more, in sample order, so both classes sit in memory
    ;; the same way: generating random inputs leaves garbage between them, fixed
    ;; ones do not, and denser inputs run faster from the cache.
    (dotimes (i samples)
      (let ((x (aref inputs i)))
        (setf (aref inputs i) (typecase x
                                (bignum (sb-bignum::copy-bignum x))
                                (octets (copy-seq x))
                                (t x)))))
    (dotimes (i (min samples 200)) (funcall run (aref inputs i)))   ; warm up
    (dotimes (i samples)
      (let ((input (aref inputs i)) (start (clempo.io:monotonic-nanoseconds)))
        (dotimes (k batch) (funcall run input))
        (setf (aref times i) (float (- (clempo.io:monotonic-nanoseconds) start) 1d0))))
    (let* ((sorted (sort (copy-seq times) #'<))
           (crops (cons most-positive-double-float
                        (loop for q in '(0.99 0.95 0.9 0.75 0.5)
                              collect (aref sorted (floor (* q (1- samples)))))))
           (ts (mapcar (lambda (keep) (ct-welch times classes keep)) crops))
           (worst (reduce #'max ts :key #'abs)))
      (values worst (/ (aref sorted (floor samples 2)) batch) ts))))

(defcommand "ct-timing" (args)
  "ct-timing [-only NAME] [-scale X] [-verbose] [-aa]: dudect-style Welch t-test of every secret-key operation (fixed vs random secrets); |t| > 4.5 is a leak"
  (let ((*ct-a/a* (arg-flag args :aa)) (only (arg args :only)) (scale (let ((s (arg args :scale))) (if s (read-from-string s) 1)))
        (failures 0))
    (format t "~&~28A ~9A ~12A ~8A ~A~%" "operation" "samples" "median ns" "max |t|" "verdict")
    (dolist (case (ct-cases))
      (when (or (null only) (string= only (ct-case-name case)))
        (let ((samples (max 100 (round (* scale (ct-case-samples case))))))
          (multiple-value-bind (worst median ts) (ct-run-case case samples)
            (when (arg-flag args :verbose)
              (format t "~&  t by crop (all, 99%, 95%, 90%, 75%, 50%): ~{~,2F~^ ~}~%" ts))
            (let ((leak (> (abs worst) 4.5)))
              (when (and leak (not (ct-case-control case))) (incf failures))
              (format t "~28A ~9D ~12,1F ~8,2F ~A~%" (ct-case-name case) samples median (abs worst)
                      (cond ((ct-case-control case) (if leak "leak (expected: control)" "no leak seen (control)"))
                            (leak "LEAK")
                            (t "ok")))
              (finish-output))))))
    (if (zerop failures) 0 1)))
