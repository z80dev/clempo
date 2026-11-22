(in-package #:clempo.consensus.tests)

;;; Mainnet finalizations recorded by goempo (see data/README.md), verified under the
;;; identities the DKG outcomes in mainnet boundary blocks install.

(defun recorded-finalizations ()
  (with-open-file (in (data-path "mainnet_finalizations.jsonl"))
    (loop for line = (read-line in nil)
          while line
          collect (clempo.json:parse-json line))))

(defun boundary-extra (height)
  (hex->octets (string-trim '(#\Newline #\Space #\Return)
                            (clempo.io:read-file-string
                             (data-path (format nil "mainnet_block_~D_extradata.hex" height))))))

(defun jnum (j key) (clempo.json:jget j key))

(deftest mainnet-finalizations-verify-under-their-epochs ()
  (let ((v (make-finalization-verifier)))
    (dolist (b '(42098399 44085599 44107199))
      (verifier-register-boundary-outcome v (boundary-extra b)))
    ;; 44107199's outcome is the published network identity
    (is-equalp +mainnet-network-identity+ (scheme-identity (verifier-identity-for v 2042)))
    (is (not (octets= +mainnet-network-identity+ (scheme-identity (verifier-identity-for v 2041)))))
    (is-equalp (scheme-identity (verifier-identity-for v 1949))
               (scheme-identity (verifier-identity-for v 2041)))
    (dolist (j (recorded-finalizations))
      (let* ((height (jnum j "height"))
             (f (decode-finalization (jnum j "certificate")))
             (epoch (proposal-epoch (finalization-proposal f))))
        (is= (jnum j "epoch") epoch)
        (is= (jnum j "view") (proposal-view (finalization-proposal f)))
        (is-equalp (hex->octets (jnum j "digest")) (proposal-payload (finalization-proposal f)))
        (is-equalp (hex->octets (jnum j "certificate")) (cw-encode 'finalization f))
        (check-finalization-block v f height (proposal-payload (finalization-proposal f)))
        (is (verify-artifact (verifier-identity-for v epoch) f) "block ~D" height)
        ;; and not under the other identity (1949 and 2041 share one; 2042 began a full DKG)
        (is (not (verify-artifact (verifier-identity-for v (if (= epoch 2042) 1949 2042)) f))
            "block ~D under a foreign identity" height)))))

(defun recorded-block (height)
  (let ((entry (find height (clempo.json:parse-json
                             (clempo.io:read-file-string (data-path "mainnet_finalized_blocks.json")))
                     :key (lambda (j) (jnum j "height")))))
    (values (clempo.blocks:decode-block (hex->octets (jnum entry "block")))
            (jnum entry "finalization"))))

(defun block-row (block certificate)
  (let* ((h (clempo.blocks:block-header block))
         (inner (clempo.headers:header-inner h)))
    (list (clempo.headers:ethereum-header-number inner) (clempo.headers:header-hash h)
          (clempo.headers:ethereum-header-parent-hash inner)
          (clempo.headers:ethereum-header-extra-data inner) certificate)))

(deftest certify-across-the-2041-2042-boundary ()
  ;; Anchored only at epoch 2041 (block 44085599's outcome): the boundary block 44107199
  ;; verifies under 2041's identity, its outcome installs 2042's, and 44107200 verifies.
  (multiple-value-bind (b200 c200) (recorded-block 44107200)
    (let* ((j199 (find 44107199 (recorded-finalizations) :key (lambda (j) (jnum j "height"))))
           (digest199 (hex->octets (jnum j199 "digest")))
           (row199 (list 44107199 digest199 (make-octets 32) (boundary-extra 44107199)
                         (jnum j199 "certificate")))
           (row200 (block-row b200 c200)))
      (is-equalp digest199 (third row200))
      (let ((v (make-finalization-verifier)))
        (verifier-register-boundary-outcome v (boundary-extra 44085599))
        (signals certificate-error (certify-blocks v (list row200)))
        (let ((r (certify-blocks v (list row199 row200))))
          (is= 2 (certification-verified r))
          (is= '(2041 2042) (mapcar #'first (certification-epochs r)))))
      ;; a wrong block hash, a wrong parent, a wrong height, a missing identity
      (flet ((reason (rows &rest keys)
               (let ((v (make-finalization-verifier)))
                 (verifier-register-boundary-outcome v (boundary-extra 44085599))
                 (handler-case (progn (apply #'certify-blocks v rows keys) :ok)
                   (certificate-error (c) (certificate-error-reason c))))))
        (is= :block-digest-mismatch
             (reason (list (list* 44107199 (make-octets 32) (cddr row199)))))
        (is= :parent-mismatch (reason (list row199 (list* 44107200 (second row200) (make-octets 32)
                                                          (cdddr row200)))))
        (is= :epoch-mismatch (reason (list (list* 44107200 (cdr row199)))))
        (is= :identity-unavailable (reason (list row200)))
        (is= :gap (reason (list row199 (list* 44107201 (cdr row200)))))
        ;; a boundary block cannot borrow the next epoch's certificate
        (is= :uncertified (reason (list (append (butlast row199) (list nil)) row200)))))))

(deftest certify-under-the-network-identity ()
  (multiple-value-bind (b c) (recorded-block 44107200)
    (let ((v (make-finalization-verifier :network-identity +mainnet-network-identity+
                                         :network-from-epoch +mainnet-network-from-epoch+)))
      (is= 1 (certification-verified (certify-blocks v (list (block-row b c))))))
    (multiple-value-bind (b1949 c1949) (recorded-block 42100000)
      (let ((v (make-finalization-verifier :network-identity +mainnet-network-identity+
                                           :network-from-epoch +mainnet-network-from-epoch+)))
        (signals certificate-error (certify-blocks v (list (block-row b1949 c1949))))))))

(deftest network-identity-outranks-registered-epochs ()
  ;; A scheme registered from block data for an epoch the trusted network identity covers
  ;; never replaces it; epochs before the identity's start still use their registrations.
  (let ((v (make-finalization-verifier :network-identity +mainnet-network-identity+
                                       :network-from-epoch +mainnet-network-from-epoch+))
        (foreign (clempo.dkg:outcome-network-identity
                  (clempo.dkg:decode-onchain-dkg-outcome (boundary-extra 44085599)))))
    (is (not (octets= foreign +mainnet-network-identity+)))
    (verifier-register-epoch v +mainnet-network-from-epoch+ foreign)
    (verifier-register-epoch v (1- +mainnet-network-from-epoch+) foreign)
    (is-equalp +mainnet-network-identity+
               (scheme-identity (verifier-identity-for v +mainnet-network-from-epoch+)))
    (is-equalp foreign (scheme-identity (verifier-identity-for v (1- +mainnet-network-from-epoch+))))
    (signals certificate-error (verifier-identity-for v (- +mainnet-network-from-epoch+ 2)))))
