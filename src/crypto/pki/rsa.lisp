(defpackage #:clempo.rsa
  (:use #:cl #:clempo.base)
  (:documentation "RSA signature verification (RFC 8017): RSASSA-PKCS1-v1_5 and RSASSA-PSS
with MGF1, over SHA-256, SHA-384 and SHA-512. Public operations only.")
  (:export #:rsa-pkcs1-verify #:rsa-pss-verify #:hash-function #:hash-length))
(in-package #:clempo.rsa)

(defun hash-function (name)
  (ecase name
    (:sha256 #'clempo.sha2:sha256) (:sha384 #'clempo.sha2:sha384) (:sha512 #'clempo.sha2:sha512)))

(defun hash-length (name) (ecase name (:sha256 32) (:sha384 48) (:sha512 64)))

(defparameter *digest-info*
  '((:sha256 . "3031300d060960864801650304020105000420")
    (:sha384 . "3041300d060960864801650304020205000430")
    (:sha512 . "3051300d060960864801650304020305000440"))
  "The DER DigestInfo prefix in front of each hash in an EMSA-PKCS1-v1_5 encoding.")

(defun public-op (n e signature)
  "The integer s^e mod n as a k-byte string, or NIL when SIGNATURE is not an
in-range k-byte representative."
  (let ((k (ceiling (integer-length n) 8)))
    (when (and (= (length signature) k) (> n 3) (oddp n) (> e 1))
      (let ((s (octets->integer signature)))
        (when (< s n)
          (integer->octets (clempo.modexp:modular-expt s e n) k))))))

(defun rsa-pkcs1-verify (n e hash-name message signature)
  "RSASSA-PKCS1-v1_5 verification of MESSAGE under public key (N, E)."
  (let ((em (public-op n e signature)))
    (when em
      (let* ((k (length em))
             (t-bytes (octets-concat (hex->octets (cdr (assoc hash-name *digest-info*)))
                                     (funcall (hash-function hash-name) message)))
             (pad (- k (length t-bytes) 3)))
        (when (>= pad 8)
          (let ((expected (fill (make-octets k) #xff)))
            (setf (aref expected 0) 0 (aref expected 1) 1 (aref expected (+ 2 pad)) 0)
            (replace expected t-bytes :start1 (+ 3 pad))
            (octets= em expected)))))))

(defun mgf1 (hash-name seed length)
  (let ((h (hash-function hash-name)) (out (make-octets length)))
    (loop for counter from 0
          for pos from 0 below length by (hash-length hash-name)
          do (replace out (funcall h (octets-concat seed (integer->octets counter 4))) :start1 pos))
    out))

(defun rsa-pss-verify (n e hash-name message signature &key (salt-length (hash-length hash-name)))
  "RSASSA-PSS verification (RFC 8017 9.1.2) with MGF1 over the same hash."
  (let* ((mod-bits (integer-length n))
         (em-bits (1- mod-bits))
         (em-len (ceiling em-bits 8))
         (h-len (hash-length hash-name))
         (full (public-op n e signature)))
    (when (and full (>= em-len (+ h-len salt-length 2)))
      ;; EM is the last emLen bytes of the k-byte integer; any leading byte must be zero.
      (let ((em (subseq full (- (length full) em-len))))
        (when (and (every #'zerop (subseq full 0 (- (length full) em-len)))
                   (= (aref em (1- em-len)) #xbc))
          (let* ((db-len (- em-len h-len 1))
                 (masked (subseq em 0 db-len))
                 (h (subseq em db-len (+ db-len h-len)))
                 (top-bits (- (* 8 em-len) em-bits)))
            (when (zerop (ash (aref masked 0) (- top-bits 8)))
              (let ((db (mgf1 hash-name h db-len)))
                (dotimes (i db-len) (setf (aref db i) (logxor (aref db i) (aref masked i))))
                (setf (aref db 0) (logand (aref db 0) (ash #xff (- top-bits))))
                (let ((ps-len (- em-len h-len salt-length 2)))
                  (when (and (every #'zerop (subseq db 0 ps-len))
                             (= (aref db ps-len) 1))
                    (let* ((salt (subseq db (- db-len salt-length)))
                           (m-prime (octets-concat (make-octets 8)
                                                   (funcall (hash-function hash-name) message)
                                                   salt)))
                      (octets= h (funcall (hash-function hash-name) m-prime)))))))))))))
