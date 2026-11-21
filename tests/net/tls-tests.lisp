(defpackage #:clempo.tls.tests (:use #:cl #:clempo.base #:clempo.tls))
(in-package #:clempo.tls.tests)

(defun data-file (name)
  (merge-pathnames (concatenate 'string "data/" name) #.(or *compile-file-truename* *load-truename*)))

;;; In-memory transport: reads from fixed octets, collects what is written.

(defclass octet-pipe (sb-gray:fundamental-binary-input-stream sb-gray:fundamental-binary-output-stream)
  ((input :initarg :input) (pos :initform 0)
   (output :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))))

(defmethod stream-element-type ((s octet-pipe)) '(unsigned-byte 8))
(defmethod sb-gray:stream-read-byte ((s octet-pipe))
  (with-slots (input pos) s
    (if (< pos (length input)) (prog1 (aref input pos) (incf pos)) :eof)))
(defmethod sb-gray:stream-read-sequence ((s octet-pipe) seq &optional (start 0) end)
  (with-slots (input pos) s
    (let ((n (min (- (or end (length seq)) start) (- (length input) pos))))
      (replace seq input :start1 start :end1 (+ start n) :start2 pos)
      (incf pos n)
      (+ start n))))
(defmethod sb-gray:stream-write-sequence ((s octet-pipe) seq &optional (start 0) end)
  (loop for i from start below (or end (length seq)) do (vector-push-extend (aref seq i) (slot-value s 'output)))
  seq)
(defmethod sb-gray:stream-force-output ((s octet-pipe)) nil)
(defun pipe-output (s) (coerce (slot-value s 'output) 'octets))

(defun traces ()
  (with-open-file (in (data-file "rfc8448.sexp"))
    (let ((*read-eval* nil)) (loop for f = (read in nil) while f collect f))))

(defun trace-named (name) (find name (traces) :key (lambda (x) (getf x :name)) :test #'string=))

(defun replay (trace)
  "Run the client against TRACE's server records. Values the TLS stream and the pipe."
  (let* ((pipe (make-instance 'octet-pipe
                              :input (apply #'octets-concat (mapcar #'hex->octets (getf trace :server)))))
         (flights (loop for (hello group secret) in (getf trace :flights)
                        collect (list (hex->octets hello) group (hex->octets secret))))
         (hello (first (first flights)))
         ;; legacy_session_id follows the 4-byte header, version and random.
         (session-id (subseq hello 39 (+ 39 (aref hello 38))))
         (conn (make-instance 'tls-stream :stream pipe :host "server")))
    (clempo.tls::handshake conn :host "server" :alpn nil :verify nil
                                :replay flights :session-id session-id)
    (values conn pipe)))

(defun expected-client-output (trace)
  (apply #'octets-concat (mapcar #'hex->octets (getf trace :client))))

(deftest rfc8448-simple-1rtt ()
  ;; Full handshake (RSA-PSS CertificateVerify, AES-128-GCM), a NewSessionTicket,
  ;; one application data exchange each way, then close_notify both ways.
  (let ((trace (trace-named "simple-1rtt")))
    (multiple-value-bind (conn pipe) (replay trace)
      (let ((data (hex->octets (getf trace :client-data))) (echo (make-octets 50)))
        (write-sequence data conn)
        (force-output conn)
        (is= 50 (read-sequence echo conn))
        (is-equalp data echo "the server echoes the client's application data")
        (is= nil (read-byte conn nil nil) "close_notify is a clean end of stream")
        (close conn)
        (is-equalp (expected-client-output trace) (pipe-output pipe)
                   "client records byte for byte: hello, Finished, data, alert")))))

(deftest rfc8448-hello-retry-request ()
  ;; The server answers an X25519 share with a HelloRetryRequest for P-256 and a cookie.
  (let ((trace (trace-named "hello-retry-request")))
    (multiple-value-bind (conn pipe) (replay trace)
      (is= nil (read-byte conn nil nil))
      (close conn)
      (is-equalp (expected-client-output trace) (pipe-output pipe)))))

(deftest rfc8448-compatibility-mode ()
  ;; A 32-byte session id: both sides send change_cipher_spec.
  (let ((trace (trace-named "compatibility-mode")))
    (multiple-value-bind (conn pipe) (replay trace)
      (is= nil (read-byte conn nil nil))
      (close conn)
      (is-equalp (expected-client-output trace) (pipe-output pipe)))))

(deftest rfc8448-client-authentication ()
  ;; The server asks for a certificate; this client answers with an empty one, so its
  ;; Finished differs from the trace, but the server's flight must authenticate.
  (let ((trace (trace-named "client-authentication")))
    (multiple-value-bind (conn pipe) (replay trace)
      (declare (ignore conn))
      (let ((out (pipe-output pipe)) (hello (hex->octets (first (getf trace :client)))))
        (is-equalp hello (subseq out 0 (length hello)))
        (is (> (length out) (length hello)))))))

(deftest rfc8448-tampering-is-detected ()
  ;; One flipped bit anywhere in the server's encrypted flight must fail the handshake.
  (let* ((trace (trace-named "simple-1rtt"))
         (records (mapcar #'hex->octets (getf trace :server)))
         (flight (second records)))
    (dolist (offset (list 5 40 300 (- (length flight) 1)))
      (let ((bad (copy-seq flight)))
        (setf (aref bad offset) (logxor (aref bad offset) 1))
        (signals tls-error
          (replay (list* :server (list (octets->hex (first records)) (octets->hex bad))
                         (remove-from-plist trace :server))))))))

(defun remove-from-plist (plist key)
  (loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(deftest hkdf-expand-label-rfc8448 ()
  ;; RFC 8448 section 3: "tls13 derived" from the early secret.
  (is-equalp (hex->octets "6f2615a108c702c5678f54fc9dbab69716c076189c48250cebeac3576c3611ba")
             (hkdf-expand-label :sha256
                                (hex->octets "33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a")
                                "derived"
                                (hex->octets "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                32)))

(defun peer-record (dir type data)
  "A record as the peer holding DIR would send it."
  (let* ((inner (octets-concat data (octets-of type)))
         (header (octets-concat (octets-of 23 3 3) (integer->octets (+ (length inner) 16) 2))))
    (octets-concat header (clempo.tls::seal dir header inner))))

(defun copy-direction (dir)
  (let ((copy (clempo.tls::new-direction (clempo.tls::direction-suite dir) (clempo.tls::direction-secret dir))))
    (setf (clempo.tls::direction-seq copy) (clempo.tls::direction-seq dir))
    copy))

(deftest key-update-both-ways ()
  ;; After the RFC 8448 handshake, the server sends KeyUpdate(update_requested), then data
  ;; under its next key. The client must read it and answer with its own KeyUpdate.
  (let ((trace (trace-named "simple-1rtt")))
    (multiple-value-bind (conn pipe) (replay trace)
      (let ((echo (make-octets 50)))
        (write-sequence (hex->octets (getf trace :client-data)) conn)
        (force-output conn)
        (read-sequence echo conn))
      (let* ((server (copy-direction (slot-value conn 'clempo.tls::reader)))
             (client (copy-direction (slot-value conn 'clempo.tls::writer)))
             (sent (length (pipe-output pipe)))
             (update (peer-record server 22 (octets-of 24 0 0 1 1))))
        (clempo.tls::key-update server)
        (setf (slot-value pipe 'input) (octets-concat update (peer-record server 23 (string->octets "after")))
              (slot-value pipe 'pos) 0)
        (let ((got (make-octets 5)))
          (read-sequence got conn)
          (is= "after" (octets->string got)))
        ;; The client's answer: KeyUpdate(update_not_requested) under its old key.
        (let* ((out (subseq (pipe-output pipe) sent))
               (inner (clempo.tls::unseal client (subseq out 0 5) (subseq out 5))))
          (is-equalp (octets-of 24 0 0 1 0 22) inner))
        ;; Its next record uses the next key.
        (clempo.tls::key-update client)
        (let ((before (length (pipe-output pipe))))
          (write-sequence (string->octets "next") conn)
          (force-output conn)
          (let ((out (subseq (pipe-output pipe) before)))
            (is-equalp (octets-concat (string->octets "next") (octets-of 23))
                       (clempo.tls::unseal client (subseq out 0 5) (subseq out 5)))))))))
