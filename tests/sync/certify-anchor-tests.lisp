(defpackage #:clempo.follow.certs.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:certs #:clempo.follow.certs) (#:follow #:clempo.follow)
                    (#:json #:clempo.json) (#:consensus #:clempo.consensus)
                    (#:blocks #:clempo.blocks) (#:fake #:clempo.sync.fake-upstream)))

(in-package #:clempo.follow.certs.tests)

;;; Mainnet boundaries (data/README.md): 44,107,199 closes epoch 2041 and names epoch 2042,
;;; the network identity's from-epoch; 44,128,799 closes epoch 2042, so its certificate
;;; verifies under the network identity. A follower anchored just after either needs it.

(define-constant +before-network+ 44107199)
(define-constant +certified+ 44128799)

(defun data-path (name)
  (asdf:system-relative-pathname "clempo-sync/tests" (format nil "../../tests/sync/data/~A" name)))

(defun recorded-raw (boundary)
  (hex->octets (string-trim '(#\Newline #\Space)
                            (clempo.io:read-file-string
                             (data-path (format nil "boundary-~D.rawblock.hex" boundary))))))

(defun recorded-finalization (boundary)
  (json:parse-json (clempo.io:read-file-string
                    (data-path (format nil "boundary-~D.finalization.json" boundary)))))

(defun forge-identity (raw)
  "RAW with the DKG outcome naming another valid group key (moderato's), one the
mainnet committee never produced."
  (let* ((raw (copy-seq raw))
         (at (search consensus:+mainnet-network-identity+ raw)))
    (invariant at "recorded boundary carries the network identity")
    (replace raw (certs:network-identity :moderato) :start1 at)
    raw))

(defun upstream-handler (raw finalization)
  "Serve RAW as the boundary block (its own hash as canonical, as a lying upstream would)
and FINALIZATION (or none) as its certificate."
  (let ((hash (octets->hex (blocks:block-hash (blocks:decode-block raw)) :prefix t)))
    (lambda (method params)
      (cond ((string= method "debug_getRawBlock") (octets->hex raw :prefix t))
            ((string= method "eth_getBlockByNumber")
             (json:jobject "number" (first params) "hash" hash
                           "stateRoot" (octets->hex (make-octets 32) :prefix t)))
            ((string= method "consensus_getFinalization")
             (or finalization (error 'fake:rpc-failure :code 503 :data "certificate")))
            (t (error 'fake:rpc-failure :code -32601))))))

(defun certifier-with (boundary raw finalization &rest options)
  (fake:call-with-fake-upstream
   (upstream-handler raw finalization)
   (lambda (url)
     (apply #'certs:make-certifier (follow:make-head :number (1+ boundary) :hash (make-octets 32))
            :upstream url :url url options))))

(defun epoch-identity (certifier epoch)
  (consensus:scheme-identity
   (consensus:verifier-identity-for (clempo.follow.certs::certifier-verifier certifier) epoch)))

(deftest anchor-boundary-verified-by-its-certificate ()
  (let ((certifier (certifier-with +certified+ (recorded-raw +certified+)
                                   (recorded-finalization +certified+))))
    (is (eq :certificate (certs:certifier-anchor certifier)))
    (is-equalp consensus:+mainnet-network-identity+ (epoch-identity certifier 2043))))

(deftest anchor-boundary-names-the-network-identity ()
  (let ((certifier (certifier-with +before-network+ (recorded-raw +before-network+) nil)))
    (is (eq :network-identity (certs:certifier-anchor certifier)))
    (is-equalp consensus:+mainnet-network-identity+ (epoch-identity certifier 2042))))

(deftest forged-anchor-identity-rejected ()
  ;; The old certifier registered whatever boundary the upstream called canonical.
  (let ((forged (forge-identity (recorded-raw +certified+))))
    ;; The real certificate finalizes the real block, not the forgery...
    (signals consensus:certificate-error
      (certifier-with +certified+ forged (recorded-finalization +certified+)))
    ;; ...and without one the upstream's word is never enough past the network identity.
    (signals simple-error (certifier-with +certified+ forged nil))
    (signals simple-error (certifier-with +certified+ (recorded-raw +certified+) nil)))
  ;; Before the from-epoch, only the published identity itself is accepted.
  (signals simple-error
    (certifier-with +before-network+ (forge-identity (recorded-raw +before-network+)) nil)))

(deftest forged-anchor-certificate-rejected ()
  (let* ((finalization (recorded-finalization +certified+))
         (forged (hex->octets (json:jget finalization "certificate"))))
    ;; Flip a bit of the last signature octet: the aggregate no longer verifies.
    (setf (aref forged (1- (length forged))) (logxor 1 (aref forged (1- (length forged)))))
    (signals error
      (certifier-with +certified+ (recorded-raw +certified+)
                      (json:jobject "epoch" (json:jget finalization "epoch")
                                    "view" (json:jget finalization "view")
                                    "digest" (json:jget finalization "digest")
                                    "certificate" (octets->hex forged :prefix t))))))

(deftest anchor-boundary-from-executed-chain ()
  (let ((hash (blocks:block-hash (blocks:decode-block (recorded-raw +certified+)))))
    ;; The caller's own executed block vouches for the boundary without a certificate...
    (is (eq :executed (certs:certifier-anchor
                       (certifier-with +certified+ (recorded-raw +certified+) nil
                                       :trusted-hash (lambda (n) (and (= n +certified+) hash))))))
    ;; ...and a different upstream block is refused.
    (signals error (certifier-with +certified+ (forge-identity (recorded-raw +certified+))
                                   (recorded-finalization +certified+)
                                   :trusted-hash (lambda (n) (declare (ignore n)) hash)))))

(deftest anchor-below-certify-from-is-hash-linked ()
  (is (eq :hash-linked
          (certs:certifier-anchor
           (certifier-with +certified+ (recorded-raw +certified+) nil :from (1+ +certified+))))))
