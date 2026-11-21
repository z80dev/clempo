(defpackage #:clempo.x509.chain-tests (:use #:cl #:clempo.base #:clempo.x509))
(in-package #:clempo.x509.chain-tests)

;;; Real chains captured from the servers clempo talks to (data/chains/README.md), validated
;;; at a fixed time against only their own anchor.

(defparameter *now* 1795266653 "2026-11-21, inside every fixture certificate's validity.")

(defun chain-file (name)
  (merge-pathnames (concatenate 'string "data/chains/" name) #.(or *compile-file-truename* *load-truename*)))

(defun load-pem (name)
  (with-open-file (in (chain-file name) :external-format :latin-1)
    (let ((s (make-string (file-length in))))
      (mapcar #'parse-certificate (pem-certificates (subseq s 0 (read-sequence s in)))))))

(defun chain (host) (load-pem (concatenate 'string host ".pem")))
(defun anchors (host) (make-trust-store (load-pem (concatenate 'string host ".root.pem"))))

(defmacro rejects (reason form)
  `(is= ,reason (handler-case (progn ,form :accepted) (x509-error (e) (x509-error-reason e)))))

(deftest real-chains-validate ()
  (loop for (host expected) in '(("rpc.tempo.xyz" ("rpc.tempo.xyz" "WE1" "GTS Root R4"))
                                 ("github.com" ("github.com" "Sectigo Public Server Authentication CA DV E36"
                                                "Sectigo Public Server Authentication Root E46"))
                                 ("objects.githubusercontent.com" ("*.github.io" "YR1" "Root YR" "ISRG Root X1")))
        do (is= expected (mapcar #'cert-subject-name
                                 (verify-server-chain (chain host) (anchors host) :host host :now *now*))
                host)))

(deftest certificate-fields ()
  (let ((leaf (first (chain "rpc.tempo.xyz"))) (ca (second (chain "rpc.tempo.xyz"))))
    (is= '("rpc.tempo.xyz") (cert-dns-names leaf))
    (is= :p256 (first (cert-public-key leaf)))
    (is= '(:ecdsa :sha256) (cert-signature-algorithm leaf))
    (is (not (cert-ca-p leaf)))
    (is (member "1.3.6.1.5.5.7.3.1" (cert-extended-key-usage leaf) :test #'string=))
    (is (member :digital-signature (cert-key-usage leaf)))
    (is (cert-ca-p ca))
    (is (member :key-cert-sign (cert-key-usage ca)))
    (is (< (cert-not-before leaf) *now* (cert-not-after leaf))))
  (let ((root-yr (third (chain "objects.githubusercontent.com"))))
    (is (member (first (cert-signature-algorithm root-yr)) '(:rsa-pkcs1 :ecdsa)))))

(deftest wildcard-names ()
  (let ((anchors (anchors "objects.githubusercontent.com")) (chain (chain "objects.githubusercontent.com")))
    (dolist (host '("objects.githubusercontent.com" "githubusercontent.com" "api.github.com" "GitHub.COM"))
      (is (verify-server-chain chain anchors :host host :now *now*) host))
    (dolist (host '("a.b.githubusercontent.com" "githubusercontent.com.evil.example" "github.org"))
      (rejects :hostname-mismatch (verify-server-chain chain anchors :host host :now *now*))))
  (is (hostname-matches-p "*.example.com" "www.example.com"))
  (is (not (hostname-matches-p "*.example.com" "example.com")))
  (is (not (hostname-matches-p "*.com" "example.com")))
  (is (not (hostname-matches-p "w*.example.com" "www.example.com")))
  (is (not (hostname-matches-p "*.example.com" ".example.com"))))

(deftest rejects-wrong-host ()
  (rejects :hostname-mismatch
           (verify-server-chain (chain "rpc.tempo.xyz") (anchors "rpc.tempo.xyz") :host "rpc.tempo.xyz.evil.example" :now *now*))
  (rejects :hostname-mismatch
           (verify-server-chain (chain "rpc.tempo.xyz") (anchors "rpc.tempo.xyz") :host "127.0.0.1" :now *now*)))

(deftest rejects-expired-and-not-yet-valid ()
  (let* ((chain (chain "rpc.tempo.xyz")) (leaf (first chain)))
    (rejects :expired (verify-server-chain chain (anchors "rpc.tempo.xyz") :host "rpc.tempo.xyz"
                                                 :now (1+ (cert-not-after leaf))))
    (rejects :not-yet-valid (verify-server-chain chain (anchors "rpc.tempo.xyz") :host "rpc.tempo.xyz"
                                                       :now (1- (cert-not-before leaf))))))

(deftest rejects-bad-signature ()
  (dolist (host '("rpc.tempo.xyz" "objects.githubusercontent.com"))
    (let* ((chain (chain host))
           (der (copy-seq (cert-der (first chain)))))
      ;; The signature BIT STRING ends the certificate: corrupt its last byte.
      (setf (aref der (1- (length der))) (logxor (aref der (1- (length der))) #x55))
      (rejects :bad-signature
               (verify-server-chain (cons (parse-certificate der) (rest chain)) (anchors host)
                                    :host host :now *now*)))))

(deftest rejects-unknown-ca ()
  ;; Anchored elsewhere, no anchors at all, and a chain missing its intermediate.
  (rejects :unknown-ca (verify-server-chain (chain "rpc.tempo.xyz") (anchors "github.com")
                                            :host "rpc.tempo.xyz" :now *now*))
  (rejects :unknown-ca (verify-server-chain (chain "rpc.tempo.xyz") (make-trust-store)
                                            :host "rpc.tempo.xyz" :now *now*))
  (rejects :unknown-ca (verify-server-chain (list (first (chain "github.com"))) (anchors "github.com")
                                            :host "github.com" :now *now*)))

(deftest rejects-leaf-as-issuer ()
  ;; A leaf (not a CA) presented as the intermediate of another certificate.
  (let ((leaf (first (chain "rpc.tempo.xyz"))))
    (rejects :unknown-ca (verify-server-chain (list leaf leaf) (make-trust-store) :host "rpc.tempo.xyz" :now *now*))))

(deftest malformed-certificates-are-errors ()
  (let ((der (cert-der (first (chain "github.com")))))
    (dolist (cut (list 1 10 100 (- (length der) 1)))
      (is (typep (handler-case (parse-certificate (subseq der 0 cut)) (x509-error (e) e)) 'x509-error)))))
