(defpackage #:clempo.x509
  (:use #:cl #:clempo.base #:clempo.der)
  (:documentation "X.509 certificates (RFC 5280): parsing, signature verification, path
building to a set of trust anchors, and server hostname checks (RFC 6125). Pure: the
caller passes the time and the anchors.")
  (:export #:x509-error #:x509-error-reason #:parse-certificate #:pem-certificates
           #:certificate #:cert-der #:cert-subject #:cert-issuer #:cert-subject-name
           #:cert-not-before #:cert-not-after #:cert-public-key #:cert-dns-names #:cert-ip-addresses
           #:cert-ca-p #:cert-path-length #:cert-key-usage #:cert-extended-key-usage
           #:cert-signature-algorithm #:cert-serial
           #:make-trust-store #:trust-store-add #:trust-store-count
           #:verify-signature #:verify-certificate-signature #:verify-server-chain
           #:hostname-matches-p #:parse-algorithm))
(in-package #:clempo.x509)

(define-condition x509-error (error)
  ((reason :initarg :reason :reader x509-error-reason)
   (message :initarg :message :reader x509-error-message))
  (:report (lambda (c s) (format s "certificate ~(~A~): ~A" (x509-error-reason c) (x509-error-message c)))))

(defun x509-fail (reason fmt &rest args)
  (error 'x509-error :reason reason :message (apply #'format nil fmt args)))

(defstruct (certificate (:conc-name cert-))
  der tbs signature-algorithm signature serial
  issuer subject subject-name not-before not-after public-key
  (ca-p nil) (path-length nil) (key-usage nil) (extended-key-usage nil)
  (dns-names '()) (ip-addresses '()) subject-key-id authority-key-id
  (permitted-dns '()) (excluded-dns '()) (unknown-critical '()))

;;; Algorithms. A signature algorithm is (:rsa-pkcs1 hash), (:rsa-pss hash salt-length),
;;; (:ecdsa hash) or (:ed25519). A public key is (:rsa n e), (:p256 x y), (:p384 x y)
;;; or (:ed25519 octets).

(defparameter *signature-oids*
  '(("1.2.840.113549.1.1.11" :rsa-pkcs1 :sha256) ("1.2.840.113549.1.1.12" :rsa-pkcs1 :sha384)
    ("1.2.840.113549.1.1.13" :rsa-pkcs1 :sha512) ("1.2.840.10045.4.3.2" :ecdsa :sha256)
    ("1.2.840.10045.4.3.3" :ecdsa :sha384) ("1.2.840.10045.4.3.4" :ecdsa :sha512)
    ("1.3.101.112" :ed25519)))

(defparameter *hash-oids*
  '(("2.16.840.1.101.3.4.2.1" . :sha256) ("2.16.840.1.101.3.4.2.2" . :sha384)
    ("2.16.840.1.101.3.4.2.3" . :sha512)))

(defun hash-oid (algid)
  (or (cdr (assoc (der-oid (first (der-children algid +sequence+))) *hash-oids* :test #'string=))
      (x509-fail :unsupported "hash algorithm")))

(defun parse-pss-params (params)
  "RSASSA-PSS-params; MGF1 must use the message hash and the trailer must be 1."
  (let ((hash nil) (mgf-hash nil) (salt 20))
    (dolist (field (der-children params +sequence+))
      (let ((inner (first (der-children field))))
        (case (tlv-tag field)
          (#xa0 (setf hash (hash-oid inner)))
          (#xa1 (let ((parts (der-children inner +sequence+)))
                  (unless (string= (der-oid (first parts)) "1.2.840.113549.1.1.8")
                    (x509-fail :unsupported "PSS mask generation"))
                  (setf mgf-hash (hash-oid (second parts)))))
          (#xa2 (setf salt (der-integer inner)))
          (#xa3 (unless (= (der-integer inner) 1) (x509-fail :unsupported "PSS trailer"))))))
    (unless (and hash (eq hash mgf-hash)) (x509-fail :unsupported "PSS hash parameters"))
    (list :rsa-pss hash salt)))

(defun parse-algorithm (algid)
  (destructuring-bind (oid-tlv &optional params) (der-children algid +sequence+)
    (let ((oid (der-oid oid-tlv)))
      (cond ((string= oid "1.2.840.113549.1.1.10") (parse-pss-params params))
            ((assoc oid *signature-oids* :test #'string=)
             (rest (assoc oid *signature-oids* :test #'string=)))
            (t (list :unsupported oid))))))

(defun parse-public-key (spki)
  (destructuring-bind (algid key-bits) (der-children spki +sequence+)
    (destructuring-bind (oid-tlv &optional params) (der-children algid +sequence+)
      (let ((oid (der-oid oid-tlv)) (bits (der-bit-string key-bits)))
        (cond ((member oid '("1.2.840.113549.1.1.1" "1.2.840.113549.1.1.10") :test #'string=)
               (destructuring-bind (n e) (der-children (der-parse bits) +sequence+)
                 (list :rsa (der-integer n) (der-integer e))))
              ((string= oid "1.2.840.10045.2.1")
               (let* ((curve (der-oid params))
                      (kind (cond ((string= curve "1.2.840.10045.3.1.7") :p256)
                                  ((string= curve "1.3.132.0.34") :p384)
                                  (t (return-from parse-public-key (list :unsupported curve)))))
                      (size (if (eq kind :p256) 32 48)))
                 (unless (and (= (length bits) (1+ (* 2 size))) (= (aref bits 0) 4))
                   (x509-fail :malformed "EC point encoding"))
                 (list kind (octets->integer bits :start 1 :end (1+ size))
                       (octets->integer bits :start (1+ size)))))
              ((string= oid "1.3.101.112")
               (unless (= (length bits) 32) (x509-fail :malformed "Ed25519 key length"))
               (list :ed25519 bits))
              (t (list :unsupported oid)))))))

;;; Names.

(defun name-common-name (name)
  (dolist (rdn (der-children name +sequence+))
    (dolist (atv (der-children rdn +set+))
      (destructuring-bind (type value) (der-children atv +sequence+)
        (when (string= (der-oid type) "2.5.4.3")
          (return-from name-common-name (ignore-errors (der-string value))))))))

(defun general-names (seq)
  "Values (dns-names ip-addresses) of a GeneralNames sequence."
  (let ((dns '()) (ips '()))
    (dolist (gn (der-children seq +sequence+))
      (case (tlv-tag gn)
        (#x82 (push (string-downcase (map 'string #'code-char (der-content gn))) dns))
        (#x87 (push (der-content gn) ips))))
    (values (nreverse dns) (nreverse ips))))

;;; Extensions.

(defun parse-extension (cert oid critical value)
  (let ((v (der-parse value)))
    (macrolet ((case-oid (&rest clauses)
                 `(cond ,@(loop for (o . body) in clauses
                                collect (if (eq o t) `(t ,@body) `((string= oid ,o) ,@body))))))
      (case-oid
       ("2.5.29.19" (let ((fields (der-children v +sequence+)))
                      (when (and fields (= (tlv-tag (first fields)) +boolean+))
                        (setf (cert-ca-p cert) (der-boolean (pop fields))))
                      (when fields (setf (cert-path-length cert) (der-integer (first fields))))))
       ("2.5.29.15" (multiple-value-bind (bytes) (der-bit-string v :allow-unused t)
                      (setf (cert-key-usage cert)
                            (loop for i below (* 8 (length bytes))
                                  when (logbitp (- 7 (mod i 8)) (aref bytes (floor i 8)))
                                    collect (nth i '(:digital-signature :content-commitment
                                                     :key-encipherment :data-encipherment
                                                     :key-agreement :key-cert-sign :crl-sign
                                                     :encipher-only :decipher-only))))))
       ("2.5.29.37" (setf (cert-extended-key-usage cert)
                          (mapcar #'der-oid (der-children v +sequence+))))
       ("2.5.29.17" (multiple-value-bind (dns ips) (general-names v)
                      (setf (cert-dns-names cert) dns (cert-ip-addresses cert) ips)))
       ("2.5.29.14" (setf (cert-subject-key-id cert) (der-content (der-expect v +octet-string+))))
       ("2.5.29.35" (dolist (f (der-children v +sequence+))
                      (when (= (tlv-tag f) #x80) (setf (cert-authority-key-id cert) (der-content f)))))
       ("2.5.29.30" (dolist (subtrees (der-children v +sequence+))
                      (dolist (subtree (der-children subtrees))
                        (let ((base (first (der-children subtree +sequence+))))
                          (if (= (tlv-tag base) #x82)
                              (let ((dns (string-downcase (map 'string #'code-char (der-content base)))))
                                (if (= (tlv-tag subtrees) #xa0)
                                    (push dns (cert-permitted-dns cert))
                                    (push dns (cert-excluded-dns cert))))
                              ;; A constraint on a name form we do not check makes the
                              ;; extension one we cannot honor.
                              (when critical (push (format nil "~A/~2,'0X" oid (tlv-tag base))
                                                   (cert-unknown-critical cert))))))))
       ;; Policies (2.5.29.32/36/54) are not processed: no policy is required of a server.
       (t (when (and critical (not (member oid '("2.5.29.32" "2.5.29.36" "2.5.29.54")
                                           :test #'string=)))
            (push oid (cert-unknown-critical cert))))))))

(defun parse-certificate (der)
  "Parse one DER certificate; signals X509-ERROR (or DER-ERROR) when malformed."
  (handler-case
      (destructuring-bind (tbs alg sig) (der-children (der-parse der) +sequence+)
        (let* ((fields (der-children tbs +sequence+))
               (version (if (= (tlv-tag (first fields)) #xa0)
                            (der-integer (first (der-children (pop fields))))
                            0))
               (cert (make-certificate :der der :tbs (der-raw tbs)
                                       :signature-algorithm (parse-algorithm alg)
                                       :signature (der-bit-string sig))))
          (unless (<= 0 version 2) (x509-fail :malformed "version ~D" version))
          (destructuring-bind (serial inner-alg issuer validity subject spki &rest more) fields
            (unless (octets= (der-raw inner-alg) (der-raw alg))
              (x509-fail :malformed "signature algorithm fields differ"))
            (setf (cert-serial cert) (der-integer serial)
                  (cert-issuer cert) (der-raw (der-expect issuer +sequence+))
                  (cert-subject cert) (der-raw (der-expect subject +sequence+))
                  (cert-subject-name cert) (name-common-name subject)
                  (cert-public-key cert) (parse-public-key spki))
            (destructuring-bind (nb na) (der-children validity +sequence+)
              (setf (cert-not-before cert) (der-time nb) (cert-not-after cert) (der-time na)))
            (dolist (f more)
              (when (= (tlv-tag f) #xa3)
                (dolist (ext (der-children (first (der-children f)) +sequence+))
                  (let* ((parts (der-children ext +sequence+))
                         (oid (der-oid (first parts)))
                         (critical (and (= (length parts) 3) (der-boolean (second parts))))
                         (value (der-content (der-expect (car (last parts)) +octet-string+))))
                    (parse-extension cert oid critical value))))))
          cert))
    (der-error (e) (x509-fail :malformed "~A" (der-error-message e)))
    ((or type-error sb-kernel::arg-count-error sb-kernel::defmacro-lambda-list-bind-error) ()
      (x509-fail :malformed "unexpected certificate structure"))))

(defun pem-certificates (text)
  "The DER bodies of every CERTIFICATE block in PEM TEXT."
  (let ((begin "-----BEGIN CERTIFICATE-----") (end "-----END CERTIFICATE-----"))
    (loop for start = (search begin text) then (search begin text :start2 stop)
          for stop = (and start (search end text :start2 start))
          while stop
          collect (base64-decode (subseq text (+ start (length begin)) stop))
          do (setf stop (+ stop (length end))))))

(defun base64-decode (text)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (acc 0) (bits 0))
    (loop for ch across text
          for v = (cond ((char<= #\A ch #\Z) (- (char-code ch) 65))
                        ((char<= #\a ch #\z) (+ 26 (- (char-code ch) 97)))
                        ((char<= #\0 ch #\9) (+ 52 (- (char-code ch) 48)))
                        ((char= ch #\+) 62) ((char= ch #\/) 63))
          when v do (setf acc (logior (ash (logand acc #xffff) 6) v)) (incf bits 6)
                    (when (>= bits 8)
                      (decf bits 8)
                      (vector-push-extend (ldb (byte 8 bits) acc) out)))
    (coerce out 'octets)))

;;; Signatures.

(defun ecdsa-signature (der)
  (handler-case
      (destructuring-bind (r s) (der-children (der-parse der) +sequence+)
        (values (der-integer r) (der-integer s)))
    (error () (values 0 0))))

(defun fit-digest (digest size)
  "ECDSA's leftmost-bits rule for a byte-aligned order: truncate or left-pad to SIZE."
  (if (>= (length digest) size)
      (subseq digest 0 size)
      (octets-concat (make-octets (- size (length digest))) digest)))

(defun verify-signature (algorithm key message signature)
  "True when SIGNATURE over MESSAGE verifies under KEY with ALGORITHM."
  (let ((kind (first key)))
    (ecase (first algorithm)
      (:rsa-pkcs1 (and (eq kind :rsa)
                       (clempo.rsa:rsa-pkcs1-verify (second key) (third key) (second algorithm)
                                                    message signature)))
      (:rsa-pss (and (eq kind :rsa)
                     (clempo.rsa:rsa-pss-verify (second key) (third key) (second algorithm)
                                                message signature :salt-length (third algorithm))))
      (:ecdsa (when (member kind '(:p256 :p384))
                (multiple-value-bind (r s) (ecdsa-signature signature)
                  (let ((digest (funcall (clempo.rsa:hash-function (second algorithm)) message)))
                    (if (eq kind :p256)
                        (clempo.p256:p256-verify (fit-digest digest 32) r s (second key) (third key))
                        (clempo.p384:p384-verify (fit-digest digest 48) r s (second key) (third key)))))))
      (:ed25519 (and (eq kind :ed25519) (= (length signature) 64)
                     (clempo.ed25519:ed25519-verify (second key) message signature)))
      (:unsupported nil))))

(defun verify-certificate-signature (cert issuer-key)
  (verify-signature (cert-signature-algorithm cert) issuer-key (cert-tbs cert) (cert-signature cert)))

;;; Trust anchors.

(defstruct (trust-store (:constructor %make-trust-store))
  (by-subject (make-hash-table :test 'equalp)) (count 0))

(defun make-trust-store (&optional certificates)
  (let ((store (%make-trust-store)))
    (dolist (c certificates store) (trust-store-add store c))))

(defun trust-store-add (store cert)
  (push cert (gethash (cert-subject cert) (trust-store-by-subject store)))
  (incf (trust-store-count store))
  store)

;;; Hostnames.

(defun ipv4-octets (host)
  (let ((parts (loop with start = 0 for dot = (position #\. host :start start)
                     collect (subseq host start dot) while dot do (setf start (1+ dot)))))
    (when (and (= (length parts) 4)
               (every (lambda (p) (and (<= 1 (length p) 3) (every #'digit-char-p p)
                                       (<= (parse-integer p) 255)))
                      parts))
      (coerce (mapcar #'parse-integer parts) 'octets))))

(defun hostname-matches-p (pattern host)
  "RFC 6125 matching of one SAN dNSName: exact, or a wildcard that is the whole leftmost
label of a pattern with at least two further labels, standing for one non-empty label."
  (let ((pattern (string-right-trim "." (string-downcase pattern)))
        (host (string-right-trim "." (string-downcase host))))
    (if (and (> (length pattern) 2) (string= "*." pattern :end2 2))
        (let ((suffix (subseq pattern 1)) (dot (position #\. host)))
          (and dot (plusp dot)
               (>= (count #\. suffix) 2)
               (not (find #\* suffix))
               (string= suffix (subseq host dot))))
        (and (not (find #\* pattern)) (string= pattern host)))))

(defun dns-within-p (name base)
  "NAME is BASE or a subdomain of it (name constraints, RFC 5280 4.2.1.10)."
  (let ((base (string-left-trim "." base)))
    (or (string= name base)
        (and (> (length name) (length base))
             (string= base name :start2 (- (length name) (length base)))
             (char= #\. (char name (- (length name) (length base) 1)))))))

;;; Path validation.

(defparameter *server-auth* "1.3.6.1.5.5.7.3.1")
(defparameter *any-eku* "2.5.29.37.0")

(defun check-time (cert now)
  (cond ((< now (cert-not-before cert))
         (x509-fail :not-yet-valid "~A is not valid before ~D" (cert-subject-name cert) (cert-not-before cert)))
        ((> now (cert-not-after cert))
         (x509-fail :expired "~A expired at ~D" (cert-subject-name cert) (cert-not-after cert)))))

(defun check-critical (cert)
  (when (cert-unknown-critical cert)
    (x509-fail :unsupported-critical "~A has unhandled critical extensions ~A"
               (cert-subject-name cert) (cert-unknown-critical cert))))

(defun check-leaf (leaf host now)
  (check-time leaf now)
  (check-critical leaf)
  (when (and (cert-key-usage leaf) (not (member :digital-signature (cert-key-usage leaf))))
    (x509-fail :bad-usage "leaf key usage lacks digitalSignature"))
  (let ((eku (cert-extended-key-usage leaf)))
    (when (and eku (not (member *server-auth* eku :test #'string=))
               (not (member *any-eku* eku :test #'string=)))
      (x509-fail :bad-usage "leaf is not for server authentication")))
  (when (eq (first (cert-public-key leaf)) :unsupported)
    (x509-fail :unsupported "leaf public key ~A" (second (cert-public-key leaf))))
  (when host
    (let ((ip (ipv4-octets host)))
      (unless (if ip
                  (member ip (cert-ip-addresses leaf) :test #'equalp)
                  (some (lambda (p) (hostname-matches-p p host)) (cert-dns-names leaf)))
        (x509-fail :hostname-mismatch "certificate for ~{~A~^, ~} does not name ~A"
                   (or (cert-dns-names leaf) (list (cert-subject-name leaf))) host)))))

(defun check-name-constraints (ca leaf)
  (let ((names (cert-dns-names leaf)))
    (dolist (name names)
      (let ((name (string-left-trim "*." name)))
        (when (and (cert-permitted-dns ca)
                   (notany (lambda (b) (dns-within-p name b)) (cert-permitted-dns ca)))
          (x509-fail :name-constraint "~A is outside ~A's permitted names" name (cert-subject-name ca)))
        (when (some (lambda (b) (dns-within-p name b)) (cert-excluded-dns ca))
          (x509-fail :name-constraint "~A is excluded by ~A" name (cert-subject-name ca)))))))

(defun issued-by-p (cert issuer)
  (and (equalp (cert-issuer cert) (cert-subject issuer))
       (or (null (cert-authority-key-id cert)) (null (cert-subject-key-id issuer))
           (equalp (cert-authority-key-id cert) (cert-subject-key-id issuer)))))

(defun verify-server-chain (chain store &key host now (max-depth 8))
  "Validate CHAIN (parsed certificates, leaf first, as a server sent them) for HOST at
Unix time NOW against trust STORE. Returns the validated path, leaf to anchor, or
signals X509-ERROR."
  (let ((leaf (first chain)) (intermediates (rest chain)) (last-error nil))
    (check-leaf leaf host now)
    (labels ((fail (e) (setf last-error e) nil)
             (anchored (cert)
               ;; Anchors are trusted as configured: their own validity and constraints apply,
               ;; basicConstraints may be absent on v1 roots.
               (dolist (root (gethash (cert-issuer cert) (trust-store-by-subject store)))
                 (when (issued-by-p cert root)
                   (handler-case
                       (progn (check-time root now)
                              (unless (verify-certificate-signature cert (cert-public-key root))
                                (x509-fail :bad-signature "~A's signature does not verify under ~A"
                                           (cert-subject-name cert) (cert-subject-name root)))
                              (check-name-constraints root leaf)
                              (return root))
                     (x509-error (e) (fail e))))))
             (extend (path depth)
               (let ((cert (first path)))
                 ;; A chain may carry the anchor itself; trust it by exact match.
                 (when (some (lambda (r) (equalp (cert-der r) (cert-der cert)))
                             (gethash (cert-subject cert) (trust-store-by-subject store)))
                   (return-from extend (reverse path)))
                 (let ((root (anchored cert)))
                   (when root (return-from extend (reverse (cons root path)))))
                 (when (< depth max-depth)
                   (dolist (ca intermediates)
                     (when (and (not (member ca path)) (issued-by-p cert ca))
                       (handler-case
                           (progn
                             (check-time ca now)
                             (check-critical ca)
                             (unless (cert-ca-p ca)
                               (x509-fail :not-ca "~A is not a CA" (cert-subject-name ca)))
                             (when (and (cert-key-usage ca) (not (member :key-cert-sign (cert-key-usage ca))))
                               (x509-fail :bad-usage "~A may not sign certificates" (cert-subject-name ca)))
                             (when (and (cert-path-length ca) (> (1- depth) (cert-path-length ca)))
                               (x509-fail :path-length "~A's path length is exceeded" (cert-subject-name ca)))
                             (let ((eku (cert-extended-key-usage ca)))
                               (when (and eku (not (member *server-auth* eku :test #'string=))
                                          (not (member *any-eku* eku :test #'string=)))
                                 (x509-fail :bad-usage "~A may not issue server certificates"
                                            (cert-subject-name ca))))
                             (unless (verify-certificate-signature cert (cert-public-key ca))
                               (x509-fail :bad-signature "~A's signature does not verify under ~A"
                                          (cert-subject-name cert) (cert-subject-name ca)))
                             (check-name-constraints ca leaf)
                             (let ((found (extend (cons ca path) (1+ depth))))
                               (when found (return-from extend found))))
                         (x509-error (e) (fail e))))))
                 nil)))
      (or (extend (list leaf) 1)
          (if last-error
              (error last-error)
              (x509-fail :unknown-ca "no trusted issuer for ~A (issued by ~A)"
                         (cert-subject-name leaf)
                         (let ((last (car (last chain))))
                           (handler-case (name-common-name (der-parse (cert-issuer last)))
                             (error () "?")))))))))
