(defpackage #:clempo.age
  (:use #:cl #:clempo.base)
  (:documentation "age v1 (https://age-encryption.org/v1) passphrase files: the scrypt recipient
only, as Tempo's SigningKey::read_from_file_encrypted / write_to_file_encrypted use it
\(crates/consensus-config, age 0.11 with_user_passphrase). Header MAC, HKDF-SHA256 file-key
wrap under ChaCha20-Poly1305, and the 64 KiB STREAM payload are checked in full.")
  (:export #:age-encrypt #:age-decrypt #:age-error #:+age-max-work-factor+
           #:+age-default-work-factor+))
(in-package #:clempo.age)

(define-condition age-error (error)
  ((message :initarg :message :reader age-error-message))
  (:report (lambda (c s) (format s "age: ~A" (age-error-message c)))))

(defun fail (control &rest args)
  (error 'age-error :message (apply #'format nil control args)))

(defconstant +age-max-work-factor+ 20
  "Largest scrypt log2(N) accepted when decrypting (1 GiB of scrypt memory).")
(defconstant +age-default-work-factor+ 18
  "log2(N) used when encrypting; age calibrates to about one second, typically 18.")
(defconstant +chunk+ 65536)
(defparameter +version-line+ "age-encryption.org/v1")
(defparameter +scrypt-label+ "age-encryption.org/v1/scrypt")

;;; Canonical unpadded standard base64 (RFC 4648 section 4), as age requires.

(defparameter +alphabet+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun b64-encode (octets)
  (with-output-to-string (out)
    (loop for i from 0 below (length octets) by 3
          for n = (min 3 (- (length octets) i))
          for v = (loop for k below 3 sum (ash (if (< k n) (aref octets (+ i k)) 0) (* 8 (- 2 k))))
          do (dotimes (k (1+ n)) (write-char (char +alphabet+ (ldb (byte 6 (* 6 (- 3 k))) v)) out)))))

(defun b64-decode (string)
  "Decode canonical unpadded base64; NIL on any non-canonical input."
  (let ((length (length string)))
    (when (= (mod length 4) 1) (return-from b64-decode nil))
    (let ((out (make-octets (floor (* length 3) 4))) (acc 0) (bits 0) (j 0))
      (loop for ch across string
            for v = (position ch +alphabet+)
            do (unless v (return-from b64-decode nil))
               (setf acc (logior (ash (logand acc #xffff) 6) v)) (incf bits 6)
               (when (>= bits 8)
                 (decf bits 8)
                 (setf (aref out j) (ldb (byte 8 bits) acc)) (incf j)))
      ;; Leftover bits must be zero (canonical encoding).
      (unless (zerop (ldb (byte bits 0) acc)) (return-from b64-decode nil))
      out)))

;;; Keys.

(defun header-mac-key (file-key) (clempo.sha2:hkdf-sha256 (make-octets 0) file-key (string->octets "header") 32))
(defun payload-key (file-key nonce) (clempo.sha2:hkdf-sha256 nonce file-key (string->octets "payload") 32))

(defun scrypt-key (passphrase salt log-n)
  (clempo.scrypt:scrypt passphrase (octets-concat (string->octets +scrypt-label+) salt)
                        (expt 2 log-n) 8 1 32))

(defun stream-nonce (counter last)
  (let ((nonce (make-octets 12)))
    (replace nonce (integer->octets counter 11))
    (setf (aref nonce 11) (if last 1 0))
    nonce))

;;; Encryption.

(defun age-encrypt (plaintext passphrase &key (work-factor +age-default-work-factor+))
  "Encrypt PLAINTEXT octets to PASSPHRASE octets; returns the binary age file."
  (check-type plaintext octets)
  (check-type passphrase octets)
  (check-type work-factor (integer 1 #.+age-max-work-factor+))
  (let* ((file-key (clempo.io:random-octets 16))
         (salt (clempo.io:random-octets 16))
         (wrapped (clempo.chacha20poly1305:aead-encrypt (scrypt-key passphrase salt work-factor)
                                                        (make-octets 12) file-key))
         (header (format nil "~A~%-> scrypt ~A ~D~%~A~%---" +version-line+ (b64-encode salt)
                         work-factor (b64-encode wrapped)))
         (mac (clempo.sha2:hmac-sha256 (header-mac-key file-key) (string->octets header)))
         (nonce (clempo.io:random-octets 16))
         (key (payload-key file-key nonce))
         (chunks (max 1 (ceiling (length plaintext) +chunk+))))
    (apply #'octets-concat
           (string->octets (format nil "~A ~A~%" header (b64-encode mac)))
           nonce
           (loop for i below chunks
                 collect (clempo.chacha20poly1305:aead-encrypt
                          key (stream-nonce i (= i (1- chunks)))
                          (octets-slice plaintext (* i +chunk+)
                                        (min (length plaintext) (* (1+ i) +chunk+))))))))

;;; Decryption.

(defun read-line-at (data start)
  "The ASCII line at START (without its LF) and the position after the LF."
  (let ((end (position 10 data :start start)))
    (unless end (fail "header is truncated"))
    (loop for i from start below end
          unless (<= 32 (aref data i) 126) do (fail "header contains a non-printable byte"))
    (values (octets->string data :start start :end end) (1+ end))))

(defun split-spaces (string)
  (loop with start = 0 for space = (position #\Space string :start start)
        collect (subseq string start space)
        while space do (setf start (1+ space))))

(defun parse-header (data)
  "Returns (values stanzas mac-octets header-end-for-mac payload-start). A stanza is
\(args . body-octets)."
  (multiple-value-bind (line at) (read-line-at data 0)
    (unless (string= line +version-line+) (fail "not an age v1 file"))
    (let ((stanzas nil))
      (loop
        (let ((line-start at))
          (multiple-value-bind (line next) (read-line-at data at)
            (cond
              ((and (>= (length line) 4) (string= "--- " line :end2 4))
               (let ((mac (b64-decode (subseq line 4))))
                 (unless (and mac (= (length mac) 32)) (fail "malformed header MAC"))
                 (return (values (nreverse stanzas) mac (+ line-start 3) next))))
              ((and (>= (length line) 3) (string= "-> " line :end2 3))
               (let ((args (split-spaces (subseq line 3))) (body ""))
                 (when (some (lambda (a) (zerop (length a))) args) (fail "malformed stanza"))
                 (setf at next)
                 (loop
                   (multiple-value-bind (body-line after) (read-line-at data at)
                     (when (> (length body-line) 64) (fail "stanza body line too long"))
                     (setf body (concatenate 'string body body-line) at after)
                     (when (< (length body-line) 64) (return))))
                 (let ((octets (b64-decode body)))
                   (unless octets (fail "stanza body is not canonical base64"))
                   (push (cons args octets) stanzas))))
              (t (fail "malformed header line")))))))))

(defun parse-work-factor (string)
  (unless (and (<= 1 (length string) 2) (every #'digit-char-p string)
               (char/= (char string 0) #\0))
    (fail "malformed scrypt work factor"))
  (let ((log-n (parse-integer string)))
    (unless (<= 1 log-n +age-max-work-factor+)
      (fail "scrypt work factor ~D exceeds the limit ~D" log-n +age-max-work-factor+))
    log-n))

(defun age-decrypt (data passphrase)
  "Decrypt a binary age file DATA with PASSPHRASE octets; returns the plaintext octets or
signals AGE-ERROR (wrong passphrase, forged header, truncated or altered payload)."
  (check-type data octets)
  (check-type passphrase octets)
  (multiple-value-bind (stanzas mac mac-end payload-start) (parse-header data)
    (unless (and (= (length stanzas) 1) (equal (first (first (first stanzas))) "scrypt"))
      (fail "a passphrase file must hold exactly one scrypt stanza"))
    (destructuring-bind ((type salt-text work-text &rest extra) . body) (first stanzas)
      (declare (ignore type))
      (when extra (fail "malformed scrypt stanza"))
      (let ((salt (b64-decode salt-text)) (log-n (parse-work-factor work-text)))
        (unless (and salt (= (length salt) 16)) (fail "malformed scrypt salt"))
        (unless (= (length body) 32) (fail "malformed scrypt wrapped key"))
        (let ((file-key (clempo.chacha20poly1305:aead-decrypt
                         (scrypt-key passphrase salt log-n) (make-octets 12) body)))
          (unless file-key (fail "wrong passphrase"))
          (unless (octets= mac (clempo.sha2:hmac-sha256 (header-mac-key file-key)
                                                        (octets-slice data 0 mac-end)))
            (fail "header MAC mismatch"))
          (decrypt-payload data payload-start file-key))))))

(defun decrypt-payload (data start file-key)
  (when (< (- (length data) start) 16) (fail "payload is truncated"))
  (let* ((key (payload-key file-key (octets-slice data start (+ start 16))))
         (sealed (+ +chunk+ 16)) (at (+ start 16)) (parts nil))
    (loop for counter from 0 do
      (let* ((remaining (- (length data) at))
             (last (<= remaining sealed))
             (size (min remaining sealed)))
        (when (< size 16) (fail "payload is truncated"))
        (let ((plain (clempo.chacha20poly1305:aead-decrypt
                      key (stream-nonce counter last) (octets-slice data at (+ at size)))))
          (unless plain (fail "payload chunk ~D failed authentication" counter))
          (when (and last (zerop (length plain)) (plusp counter))
            (fail "empty final chunk"))
          (push plain parts)
          (incf at size)
          (when last (return)))))
    (apply #'octets-concat (nreverse parts))))
