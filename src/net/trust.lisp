;;; Trust anchors: CLEMPO_CA_FILE, else the macOS system roots keychain, else the usual
;;; Linux bundles. Loaded once per process on first use.
(in-package #:clempo.tls)

(defparameter *ca-bundles*
  '("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt"
    "/etc/ssl/ca-bundle.pem" "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"
    "/etc/ssl/cert.pem")
  "PEM bundles tried in order when CLEMPO_CA_FILE is unset and there is no macOS keychain.")

(defparameter *macos-roots* "/System/Library/Keychains/SystemRootCertificates.keychain")

(defvar *system-trust-store* nil)
(defvar *trust-lock* (sb-thread:make-mutex :name "trust store"))

(defun trust-pem ()
  "Values the PEM text of the configured anchors and where it came from."
  (let ((override (clempo.io:getenv "CLEMPO_CA_FILE")))
    (cond ((and override (plusp (length override)))
           (unless (probe-file override) (tls-fail nil "CLEMPO_CA_FILE ~A does not exist" override))
           (values (clempo.io:read-file-string override) override))
          ((probe-file *macos-roots*)
           (multiple-value-bind (out code)
               (clempo.io:run-capture "/usr/bin/security"
                                      (list "find-certificate" "-a" "-p" *macos-roots*))
             (unless (zerop code) (tls-fail nil "security find-certificate exited ~D" code))
             (values (octets->string out) *macos-roots*)))
          (t (let ((bundle (find-if #'probe-file *ca-bundles*)))
               (unless bundle
                 (tls-fail nil "no CA bundle found (set CLEMPO_CA_FILE; tried ~{~A~^, ~})" *ca-bundles*))
               (values (clempo.io:read-file-string bundle) bundle))))))

(defun load-trust-store ()
  (multiple-value-bind (pem source) (trust-pem)
    (let ((store (clempo.x509:make-trust-store)))
      (dolist (der (clempo.x509:pem-certificates pem))
        ;; An anchor we cannot parse cannot anchor a chain; skip it rather than fail all TLS.
        (let ((cert (ignore-errors (clempo.x509:parse-certificate der))))
          (when cert (clempo.x509:trust-store-add store cert))))
      (when (zerop (clempo.x509:trust-store-count store))
        (tls-fail nil "no usable certificates in ~A" source))
      store)))

(defun system-trust-store ()
  (or *system-trust-store*
      (clempo.lock:with-lock (*trust-lock*)
        (or *system-trust-store* (setf *system-trust-store* (load-trust-store))))))
