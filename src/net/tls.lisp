(defpackage #:clempo.tls
  (:use #:cl #:clempo.base)
  (:documentation "A TLS 1.3 client (RFC 8446) over any binary stream. Key exchange X25519,
or P-256 when a HelloRetryRequest asks for it; suites TLS_AES_128_GCM_SHA256,
TLS_AES_256_GCM_SHA384 and TLS_CHACHA20_POLY1305_SHA256; server authentication by
certificate (clempo.x509) and CertificateVerify. The connection is a Gray binary stream.")
  (:export #:tls-error #:tls-error-alert #:tls-connect #:tls-stream #:tls-alpn #:tls-suite
           #:tls-close #:*trust-store* #:system-trust-store #:hkdf-expand-label))
(in-package #:clempo.tls)

(define-condition tls-error (error)
  ((message :initarg :message :reader tls-error-message)
   (alert :initarg :alert :initform nil :reader tls-error-alert))
  (:report (lambda (c s) (format s "TLS: ~A" (tls-error-message c)))))

(defun tls-fail (alert fmt &rest args)
  (error 'tls-error :alert alert :message (apply #'format nil fmt args)))

(defvar *trust-store* nil
  "The trust anchors (a clempo.x509 trust store), or NIL for the system store.")

;;; Alert descriptions we send.
(defconstant +unexpected-message+ 10)
(defconstant +bad-record-mac+ 20)
(defconstant +record-overflow+ 22)
(defconstant +handshake-failure+ 40)
(defconstant +bad-certificate+ 42)
(defconstant +certificate-expired+ 45)
(defconstant +unknown-ca+ 48)
(defconstant +illegal-parameter+ 47)
(defconstant +decode-error+ 50)
(defconstant +decrypt-error+ 51)
(defconstant +protocol-version+ 70)
(defconstant +missing-extension+ 109)
(defconstant +no-application-protocol+ 120)

;;; Hashes and the key schedule (RFC 8446 section 7).

(defstruct (suite (:constructor make-suite (id name hash key-length)))
  id name hash key-length)

(defparameter *suites*
  (list (make-suite #x1301 "TLS_AES_128_GCM_SHA256" :sha256 16)
        (make-suite #x1302 "TLS_AES_256_GCM_SHA384" :sha384 32)
        (make-suite #x1303 "TLS_CHACHA20_POLY1305_SHA256" :sha256 32)))

(defun digest (hash data) (funcall (clempo.rsa:hash-function hash) data))
(defun hash-size (hash) (clempo.rsa:hash-length hash))
(defun block-size (hash) (if (eq hash :sha256) 64 128))

(defun hmac (hash key data)
  (let* ((b (block-size hash))
         (k (if (> (length key) b) (digest hash key) key))
         (ipad (make-octets b)) (opad (make-octets b)))
    (dotimes (i b)
      (let ((x (if (< i (length k)) (aref k i) 0)))
        (setf (aref ipad i) (logxor x #x36) (aref opad i) (logxor x #x5c))))
    (digest hash (octets-concat opad (digest hash (octets-concat ipad data))))))

(defun hkdf-extract (hash salt ikm) (hmac hash salt ikm))

(defun hkdf-expand (hash prk info length)
  (let ((out (make-octets length)) (prev (make-octets 0)))
    (loop for i from 1 for pos from 0 below length by (hash-size hash)
          do (setf prev (hmac hash prk (octets-concat prev info (octets-of i))))
             (replace out prev :start1 pos))
    out))

(defun hkdf-expand-label (hash secret label context length)
  (let ((full (string->octets (concatenate 'string "tls13 " label))))
    (hkdf-expand hash secret
                 (octets-concat (integer->octets length 2) (octets-of (length full)) full
                                (octets-of (length context)) context)
                 length)))

(defun derive-secret (hash secret label transcript-hash)
  (hkdf-expand-label hash secret label transcript-hash (hash-size hash)))

;;; Byte building and parsing.

(defun u8 (n) (octets-of n))
(defun u16 (n) (integer->octets n 2))
(defun u24 (n) (integer->octets n 3))
(defun vec8 (&rest parts) (let ((b (apply #'octets-concat parts))) (octets-concat (u8 (length b)) b)))
(defun vec16 (&rest parts) (let ((b (apply #'octets-concat parts))) (octets-concat (u16 (length b)) b)))
(defun vec24 (&rest parts) (let ((b (apply #'octets-concat parts))) (octets-concat (u24 (length b)) b)))

(defstruct (reader (:constructor make-reader (data &optional (pos 0) (end (length data)))))
  data pos end)

(defun take (r n)
  (when (> (+ (reader-pos r) n) (reader-end r)) (tls-fail +decode-error+ "truncated message"))
  (prog1 (subseq (reader-data r) (reader-pos r) (+ (reader-pos r) n)) (incf (reader-pos r) n)))
(defun take-int (r n) (octets->integer (take r n)))
(defun take-vec (r n) (take r (take-int r n)))
(defun sub-reader (r n) (make-reader (take-vec r n)))
(defun done-p (r) (>= (reader-pos r) (reader-end r)))
(defun expect-done (r) (unless (done-p r) (tls-fail +decode-error+ "trailing bytes in message")))

(defun read-extensions (r)
  "Alist (type . body) of a 16-bit extension block; duplicates are an error."
  (let ((block (sub-reader r 2)) (exts '()))
    (loop until (done-p block)
          do (let ((type (take-int block 2)) (body (take-vec block 2)))
               (when (assoc type exts) (tls-fail +illegal-parameter+ "duplicate extension ~D" type))
               (push (cons type body) exts)))
    (nreverse exts)))

;;; Record protection.

(defstruct (direction (:constructor make-direction (suite secret)))
  suite secret aead iv (seq 0))

(defun install-keys (dir)
  (let* ((suite (direction-suite dir)) (hash (suite-hash suite)) (secret (direction-secret dir))
         (key (hkdf-expand-label hash secret "key" (make-octets 0) (suite-key-length suite))))
    (setf (direction-iv dir) (hkdf-expand-label hash secret "iv" (make-octets 0) 12)
          (direction-seq dir) 0
          (direction-aead dir) (if (= (suite-id suite) #x1303) key (clempo.gcm:make-gcm key)))
    dir))

(defun new-direction (suite secret) (install-keys (make-direction suite secret)))

(defun next-nonce (dir)
  (let ((nonce (copy-seq (direction-iv dir))) (seq (direction-seq dir)))
    (when (>= seq (1- (ash 1 64))) (tls-fail nil "sequence number exhausted"))
    (dotimes (i 8) (setf (aref nonce (- 11 i)) (logxor (aref nonce (- 11 i)) (ldb (byte 8 (* 8 i)) seq))))
    (incf (direction-seq dir))
    nonce))

(defun seal (dir aad plaintext)
  (let ((aead (direction-aead dir)) (nonce (next-nonce dir)))
    (if (typep aead 'octets)
        (clempo.chacha20poly1305:aead-encrypt aead nonce plaintext :aad aad)
        (clempo.gcm:gcm-encrypt aead nonce plaintext :aad aad))))

(defun unseal (dir aad ciphertext)
  (let ((aead (direction-aead dir)) (nonce (next-nonce dir)))
    (if (typep aead 'octets)
        (clempo.chacha20poly1305:aead-decrypt aead nonce ciphertext :aad aad)
        (clempo.gcm:gcm-decrypt aead nonce ciphertext :aad aad))))

;;; The connection.

(defclass tls-stream (sb-gray:fundamental-binary-input-stream sb-gray:fundamental-binary-output-stream)
  ((stream :initarg :stream :reader transport)
   (host :initarg :host :initform nil)
   (suite :initform nil :reader tls-suite)
   (alpn :initform nil :reader tls-alpn)
   (reader :initform nil)                ; direction or NIL while plaintext
   (writer :initform nil)
   (handshake-buffer :initform (make-octets 0))
   (in :initform (make-octets 0))        ; decrypted application data
   (in-pos :initform 0)
   (out :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
   (eof :initform nil)
   (closed :initform nil)
   (transcript :initform '())))

(defconstant +max-plaintext+ 16384)

(defun write-record (conn type data &optional (version #x0303))
  (with-slots (stream writer) conn
    (loop for start from 0 by +max-plaintext+
          for end = (min (length data) (+ start +max-plaintext+))
          do (let ((fragment (subseq data start end)))
               (if writer
                   (let* ((inner (octets-concat fragment (u8 type)))
                          (header (octets-concat (u8 23) (u16 #x0303) (u16 (+ (length inner) 16)))))
                     (write-sequence (octets-concat header (seal writer header inner)) stream))
                   (write-sequence (octets-concat (u8 type) (u16 version) (u16 (length fragment)) fragment)
                                   stream)))
          while (< end (length data)))
    (force-output stream)))

(defun read-full (stream n)
  (let* ((buf (make-octets n)) (got (read-sequence buf stream)))
    (if (= got n) buf (and (plusp got) (tls-fail nil "connection closed mid-record")))))

(defun send-alert (conn description)
  (ignore-errors (write-record conn 21 (octets-of (if (zerop description) 1 2) description))))

(defun read-record (conn)
  "Values content-type and plaintext of the next record, or NIL at a clean transport EOF."
  (with-slots (stream reader) conn
    (loop
      (let ((header (read-full stream 5)))
        (unless header (return nil))
        (let ((type (aref header 0)) (len (octets->integer header :start 3)))
          (when (> len (+ +max-plaintext+ 256)) (tls-fail +record-overflow+ "record of ~D bytes" len))
          (let ((body (or (read-full stream len) (tls-fail nil "connection closed mid-record"))))
            (cond ((= type 20)
                   ;; Middlebox-compatibility change_cipher_spec: ignored during the handshake.
                   (unless (equalp body #(1)) (tls-fail +unexpected-message+ "bad change_cipher_spec")))
                  ((and reader (= type 23))
                   (let ((inner (or (unseal reader header body)
                                    (tls-fail +bad-record-mac+ "record failed authentication"))))
                     (let ((end (position-if #'plusp inner :from-end t)))
                       (unless end (tls-fail +unexpected-message+ "record without a content type"))
                       (when (> end +max-plaintext+) (tls-fail +record-overflow+ "inner plaintext too long"))
                       (return (values (aref inner end) (subseq inner 0 end))))))
                  (reader (tls-fail +unexpected-message+ "plaintext record type ~D after keys" type))
                  (t (return (values type body))))))))))

(defun handle-alert (conn body)
  (unless (= (length body) 2) (tls-fail +decode-error+ "malformed alert"))
  (if (= (aref body 1) 0)
      (setf (slot-value conn 'eof) t)
      (tls-fail nil "server sent alert ~D~@[ (~A)~]" (aref body 1)
                (case (aref body 1)
                  ((40 70) (unless (slot-value conn 'writer)
                             "handshake refused: the server may not speak TLS 1.3"))))))

(defun next-handshake (conn &optional (during-handshake t))
  "Values type, body and the full encoded message of the next handshake message."
  (with-slots (handshake-buffer) conn
    (loop
      (when (>= (length handshake-buffer) 4)
        (let ((len (octets->integer handshake-buffer :start 1 :end 4)))
          (when (>= (length handshake-buffer) (+ 4 len))
            (let ((msg (subseq handshake-buffer 0 (+ 4 len))))
              (setf handshake-buffer (subseq handshake-buffer (+ 4 len)))
              (return (values (aref msg 0) (subseq msg 4) msg))))))
      (multiple-value-bind (type body) (read-record conn)
        (case type
          ((nil) (tls-fail nil "connection closed during the handshake"))
          (22 (setf handshake-buffer (octets-concat handshake-buffer body)))
          (21 (handle-alert conn body) (tls-fail nil "connection closed during the handshake"))
          (t (when during-handshake
               (tls-fail +unexpected-message+ "record type ~D during the handshake" type))))))))

(defun transcript-hash (conn hash)
  (digest hash (apply #'octets-concat (reverse (slot-value conn 'transcript)))))

(defun add-transcript (conn msg) (push msg (slot-value conn 'transcript)))

;;; ClientHello.

(defparameter *hello-retry-random*
  (hex->octets "cf21ad74e59a6111be1d8c021e65b891c2a211167abb8c5e079e09e2c8a8339c"))

(defparameter *signature-schemes*
  '(#x0403 #x0503 #x0804 #x0805 #x0806 #x0807 #x0401 #x0501 #x0601)
  "ecdsa P-256/P-384, rsa_pss_rsae, ed25519; rsa_pkcs1 is offered for certificates only.")

(defun extension (type &rest parts) (octets-concat (u16 type) (apply #'vec16 parts)))

(defun ip-literal-p (host) (or (find #\: host) (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)))

(defun key-share-entry (group public) (octets-concat (u16 group) (vec16 public)))

(defun client-hello (random session-id host alpn group public &optional cookie)
  (let ((body (octets-concat
               (u16 #x0303) random (vec8 session-id)
               (vec16 (apply #'octets-concat (mapcar (lambda (s) (u16 (suite-id s))) *suites*)))
               (vec8 (u8 0))
               (vec16
                (if (and host (not (ip-literal-p host)))
                    (extension 0 (vec16 (u8 0) (vec16 (string->octets host))))
                    (make-octets 0))
                (extension 10 (vec16 (u16 #x001d) (u16 #x0017)))
                (extension 13 (vec16 (apply #'octets-concat (mapcar #'u16 *signature-schemes*))))
                (if alpn
                    (extension 16 (vec16 (apply #'octets-concat
                                                (mapcar (lambda (p) (vec8 (string->octets p))) alpn))))
                    (make-octets 0))
                (extension 43 (vec8 (u16 #x0304)))
                (extension 45 (vec8 (u8 1)))
                (extension 51 (vec16 (key-share-entry group public)))
                (if cookie (extension 44 (vec16 cookie)) (make-octets 0))))))
    (octets-concat (u8 1) (vec24 body))))

(defun make-key-share (group)
  "Values secret and public key share for GROUP."
  (ecase group
    (#x001d (let ((secret (clempo.io:random-octets 32)))
              (values secret (clempo.x25519:x25519-public-key secret))))
    (#x0017 (let ((secret (integer->octets (1+ (clempo.io:random-below
                                                 (1- #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)))
                                           32)))
              (values secret (clempo.ecdh:p256-ecdh-public-key secret))))))

(defun shared-secret (group secret peer)
  (let ((shared (ecase group
                  (#x001d (and (= (length peer) 32) (clempo.x25519:x25519 secret peer)))
                  (#x0017 (clempo.ecdh:p256-ecdh-shared secret peer)))))
    (when (or (null shared) (every #'zerop shared))
      (tls-fail +illegal-parameter+ "invalid key share"))
    shared))

;;; Server messages.

(defun parse-server-hello (body session-id)
  "Values random, suite, extensions."
  (let ((r (make-reader body)))
    (let ((version (take-int r 2)) (random (take r 32)) (echo (take-vec r 1))
          (suite-id (take-int r 2)) (compression (take-int r 1)) (exts (read-extensions r)))
      (expect-done r)
      (let ((versions (cdr (assoc 43 exts))))
        (unless (and versions (equalp versions (u16 #x0304)))
          (tls-fail +protocol-version+ "server does not speak TLS 1.3 (legacy version ~4,'0X)" version)))
      (unless (equalp echo session-id) (tls-fail +illegal-parameter+ "session id not echoed"))
      (unless (zerop compression) (tls-fail +illegal-parameter+ "compression selected"))
      (values random
              (or (find suite-id *suites* :key #'suite-id)
                  (tls-fail +illegal-parameter+ "server chose suite ~4,'0X" suite-id))
              exts))))

(defparameter *verify-schemes*
  '((#x0403 :ecdsa :sha256 :p256) (#x0503 :ecdsa :sha384 :p384)
    (#x0804 :rsa-pss :sha256 :rsa) (#x0805 :rsa-pss :sha384 :rsa) (#x0806 :rsa-pss :sha512 :rsa)
    (#x0807 :ed25519 nil :ed25519)))

(defun verify-certificate-verify (body key hash transcript)
  (let* ((r (make-reader body)) (scheme (take-int r 2)) (signature (take-vec r 2))
         (entry (assoc scheme *verify-schemes*)))
    (expect-done r)
    (unless (and entry (eq (fourth entry) (first key)))
      (tls-fail +illegal-parameter+ "CertificateVerify scheme ~4,'0X does not fit the key" scheme))
    (let ((content (octets-concat (fill (make-octets 64) 32)
                                  (string->octets "TLS 1.3, server CertificateVerify")
                                  (u8 0) (digest hash transcript)))
          (algorithm (ecase (second entry)
                       (:ecdsa (list :ecdsa (third entry)))
                       (:rsa-pss (list :rsa-pss (third entry) (hash-size (third entry))))
                       (:ed25519 (list :ed25519)))))
      (unless (clempo.x509:verify-signature algorithm key content signature)
        (tls-fail +decrypt-error+ "CertificateVerify signature is invalid")))))

(defun parse-certificates (body)
  (let ((r (make-reader body)))
    (take-vec r 1)                      ; certificate_request_context, empty for server auth
    (let ((list (sub-reader r 3)) (certs '()))
      (expect-done r)
      (loop until (done-p list)
            do (push (take-vec list 3) certs)
               (take-vec list 2))
      (when (null certs) (tls-fail +decode-error+ "server sent no certificate"))
      (nreverse certs))))

(defun x509-alert (e)
  (case (clempo.x509:x509-error-reason e)
    (:expired +certificate-expired+) (:unknown-ca +unknown-ca+) (t +bad-certificate+)))

(defun authenticate (conn ders host verify)
  "The leaf's public key, after validating the chain when VERIFY."
  (handler-case
      (let ((chain (mapcar #'clempo.x509:parse-certificate ders)))
        (when verify
          (clempo.x509:verify-server-chain chain (or *trust-store* (system-trust-store))
                                           :host host :now (clempo.io:now-seconds)))
        (let ((key (clempo.x509:cert-public-key (first chain))))
          (when (eq (first key) :unsupported) (tls-fail +bad-certificate+ "unsupported server key"))
          key))
    (clempo.x509:x509-error (e)
      (send-alert conn (x509-alert e))
      (tls-fail (x509-alert e) "~A: ~A" host e))))

;;; The handshake.

(defun handshake (conn &key host alpn (verify t) replay (session-id nil session-id-p))
  "Run the client side of a full handshake on CONN. REPLAY, a list of (client-hello group
secret) per flight, replays fixed hellos (the RFC 8448 traces); normally they are generated."
  (let* ((session-id (if session-id-p session-id (clempo.io:random-octets 32)))
         (random (clempo.io:random-octets 32))
         (group #x001d) (cookie nil) (retried nil) hello secret)
    (flet ((next-hello ()
             (if replay
                 (destructuring-bind (h g s) (pop replay)
                   (setf hello h group g secret s))
                 (multiple-value-bind (s p) (make-key-share group)
                   (setf secret s
                         hello (client-hello random session-id host alpn group p cookie))))))
      (next-hello)
    (loop
      ;; The first ClientHello carries legacy record version 0x0301 (RFC 8446 5.1).
      (write-record conn 22 hello (if retried #x0303 #x0301))
      (add-transcript conn hello)
      (multiple-value-bind (type body msg) (next-handshake conn)
        (unless (= type 2) (tls-fail +unexpected-message+ "expected ServerHello, got ~D" type))
        (multiple-value-bind (server-random suite exts) (parse-server-hello body session-id)
          (cond ((equalp server-random *hello-retry-random*)
                 (when retried (tls-fail +unexpected-message+ "second HelloRetryRequest"))
                 (setf retried t)
                 (let* ((hash (suite-hash suite))
                        (ch1-hash (digest hash (first (slot-value conn 'transcript))))
                        (share (cdr (assoc 51 exts))) (new-group (and share (octets->integer share))))
                   (setf (slot-value conn 'transcript)
                         (list msg (octets-concat (u8 254) (u24 (hash-size hash)) ch1-hash))
                         cookie (let ((c (cdr (assoc 44 exts)))) (and c (take-vec (make-reader c) 2))))
                   (when new-group
                     (unless (and (member new-group '(#x001d #x0017)) (/= new-group group))
                       (tls-fail +illegal-parameter+ "HelloRetryRequest for group ~4,'0X" new-group))
                     (setf group new-group))
                   (next-hello)
                   ;; The retried hello goes out under the same (plaintext) record state.
                   (setf (slot-value conn 'suite) suite)))
                (t
                 (add-transcript conn msg)
                 (when (and retried (not (eq suite (slot-value conn 'suite))))
                   (tls-fail +illegal-parameter+ "suite changed after HelloRetryRequest"))
                 (let* ((share (or (cdr (assoc 51 exts)) (tls-fail +missing-extension+ "no key share")))
                        (r (make-reader share)) (server-group (take-int r 2)) (peer (take-vec r 2)))
                   (expect-done r)
                   (unless (= server-group group) (tls-fail +illegal-parameter+ "key share for another group"))
                   (return (finish-handshake conn suite (shared-secret group secret peer)
                                             host alpn verify (plusp (length session-id))))))))))))
  conn)

(defun finish-handshake (conn suite shared host alpn verify compatibility)
  (let* ((hash (suite-hash suite)) (size (hash-size hash)) (zeros (make-octets size))
         (empty-hash (digest hash (make-octets 0)))
         (early (hkdf-extract hash zeros zeros))
         (hs (hkdf-extract hash (derive-secret hash early "derived" empty-hash) shared))
         (th (transcript-hash conn hash))
         (c-hs (derive-secret hash hs "c hs traffic" th))
         (s-hs (derive-secret hash hs "s hs traffic" th))
         (master (hkdf-extract hash (derive-secret hash hs "derived" empty-hash) zeros))
         (key nil) (certificate-requested nil))
    (setf (slot-value conn 'suite) suite
          (slot-value conn 'reader) (new-direction suite s-hs))
    (flet ((expect (want)
             (multiple-value-bind (type body msg) (next-handshake conn)
               (unless (member type want)
                 (tls-fail +unexpected-message+ "expected handshake ~A, got ~D" want type))
               (values type body msg))))
      ;; EncryptedExtensions
      (multiple-value-bind (type body msg) (expect '(8))
        (declare (ignore type))
        (let* ((r (make-reader body)) (exts (read-extensions r)) (chosen (cdr (assoc 16 exts))))
          (expect-done r)
          (when chosen
            (let* ((list (sub-reader (make-reader chosen) 2)) (proto (octets->string (take-vec list 1))))
              (unless (and (done-p list) (member proto alpn :test #'string=))
                (tls-fail +no-application-protocol+ "server chose protocol ~S" proto))
              (setf (slot-value conn 'alpn) proto))))
        (add-transcript conn msg))
      ;; CertificateRequest?, Certificate
      (loop
        (multiple-value-bind (type body msg) (expect '(13 11))
          (add-transcript conn msg)
          (if (= type 13)
              (setf certificate-requested (take-vec (make-reader body) 1))
              (progn (setf key (authenticate conn (parse-certificates body) host verify))
                     (return)))))
      ;; CertificateVerify
      (multiple-value-bind (type body msg) (expect '(15))
        (declare (ignore type))
        (verify-certificate-verify body key hash (apply #'octets-concat (reverse (slot-value conn 'transcript))))
        (add-transcript conn msg))
      ;; Finished
      (multiple-value-bind (type body msg) (expect '(20))
        (declare (ignore type))
        (let ((expected (hmac hash (hkdf-expand-label hash s-hs "finished" (make-octets 0) size)
                              (transcript-hash conn hash))))
          (unless (and (= (length body) size)
                       (zerop (reduce #'logior (map 'octets #'logxor expected body))))
            (send-alert conn +decrypt-error+)
            (tls-fail +decrypt-error+ "server Finished does not verify")))
        (add-transcript conn msg)))
    (let* ((th (transcript-hash conn hash))
           (c-ap (derive-secret hash master "c ap traffic" th))
           (s-ap (derive-secret hash master "s ap traffic" th)))
      ;; Middlebox compatibility mode (RFC 8446 D.4): a client that sent a session id sends
      ;; change_cipher_spec before its first encrypted record.
      (when compatibility (write-record conn 20 (u8 1)))
      (setf (slot-value conn 'writer) (new-direction suite c-hs))
      (when certificate-requested
        (let ((msg (octets-concat (u8 11) (vec24 (vec8 certificate-requested) (vec24)))))
          (write-record conn 22 msg)
          (add-transcript conn msg)))
      (let ((finished (octets-concat (u8 20) (vec24 (hmac hash (hkdf-expand-label hash c-hs "finished"
                                                                                  (make-octets 0) size)
                                                        (transcript-hash conn hash))))))
        (write-record conn 22 finished))
      (setf (slot-value conn 'reader) (new-direction suite s-ap)
            (slot-value conn 'writer) (new-direction suite c-ap)
            (slot-value conn 'transcript) '()))))

;;; Post-handshake traffic.

(defun key-update (dir)
  (let ((hash (suite-hash (direction-suite dir))))
    (setf (direction-secret dir) (hkdf-expand-label hash (direction-secret dir) "traffic upd"
                                                    (make-octets 0) (hash-size hash)))
    (install-keys dir)))

(defun handle-post-handshake (conn)
  (loop while (>= (length (slot-value conn 'handshake-buffer)) 4)
        do (let ((len (octets->integer (slot-value conn 'handshake-buffer) :start 1 :end 4)))
             (when (< (length (slot-value conn 'handshake-buffer)) (+ 4 len)) (return))
             (multiple-value-bind (type body) (next-handshake conn nil)
               (case type
                 (4 nil)                ; NewSessionTicket: no resumption
                 (24 (unless (and (= (length body) 1) (<= (aref body 0) 1))
                       (tls-fail +decode-error+ "malformed KeyUpdate"))
                     (key-update (slot-value conn 'reader))
                     (when (= (aref body 0) 1)
                       (flush conn)
                       (write-record conn 22 (octets-concat (u8 24) (vec24 (u8 0))))
                       (key-update (slot-value conn 'writer))))
                 (t (tls-fail +unexpected-message+ "post-handshake message ~D" type)))))))

(defun fill-input (conn)
  "Read records until application data arrives. NIL at EOF."
  (with-slots (in in-pos eof handshake-buffer) conn
    (loop
      (when eof (return nil))
      (multiple-value-bind (type body) (read-record conn)
        (case type
          ((nil) (tls-fail nil "connection closed without close_notify"))
          (23 (when (plusp (length body)) (setf in body in-pos 0) (return t)))
          (22 (setf handshake-buffer (octets-concat handshake-buffer body))
              (handle-post-handshake conn))
          (21 (handle-alert conn body))
          (t (tls-fail +unexpected-message+ "record type ~D" type)))))))

(defun flush (conn)
  (with-slots (out) conn
    (when (plusp (length out))
      (write-record conn 23 (coerce out 'octets))
      (setf (fill-pointer out) 0))))

(defmethod stream-element-type ((s tls-stream)) '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((s tls-stream))
  (with-slots (in in-pos) s
    (if (or (< in-pos (length in)) (fill-input s))
        (prog1 (aref in in-pos) (incf in-pos))
        :eof)))

(defmethod sb-gray:stream-read-sequence ((s tls-stream) seq &optional (start 0) end)
  (let ((end (or end (length seq))) (pos start))
    (with-slots (in in-pos) s
      (loop while (< pos end)
            do (unless (or (< in-pos (length in)) (fill-input s)) (return))
               (let ((n (min (- end pos) (- (length in) in-pos))))
                 (replace seq in :start1 pos :end1 (+ pos n) :start2 in-pos)
                 (incf pos n) (incf in-pos n))))
    pos))

(defmethod sb-gray:stream-listen ((s tls-stream))
  (with-slots (in in-pos) s (< in-pos (length in))))

(defmethod sb-gray:stream-write-byte ((s tls-stream) byte)
  (vector-push-extend byte (slot-value s 'out))
  byte)

(defmethod sb-gray:stream-write-sequence ((s tls-stream) seq &optional (start 0) end)
  (let ((out (slot-value s 'out)))
    (loop for i from start below (or end (length seq)) do (vector-push-extend (aref seq i) out)))
  seq)

(defmethod sb-gray:stream-force-output ((s tls-stream)) (flush s) nil)
(defmethod sb-gray:stream-finish-output ((s tls-stream)) (flush s) nil)

(defun tls-close (s)
  "Send close_notify and close the transport."
  (with-slots (closed stream) s
    (unless closed
      (setf closed t)
      (ignore-errors (flush s))
      (send-alert s 0)
      (ignore-errors (close stream)))))

(defmethod close ((s tls-stream) &key abort)
  (if abort (ignore-errors (close (transport s) :abort t)) (tls-close s))
  (call-next-method)
  t)

(defun tls-connect (stream host &key (alpn '("http/1.1")) (verify t))
  "Run a TLS 1.3 handshake over binary STREAM to HOST; returns the TLS-STREAM."
  (let ((conn (make-instance 'tls-stream :stream stream :host host)))
    (handler-bind ((tls-error (lambda (e)
                                (when (tls-error-alert e) (send-alert conn (tls-error-alert e))))))
      (handshake conn :host host :alpn alpn :verify verify))))
