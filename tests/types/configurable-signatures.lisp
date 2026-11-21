(defpackage #:clempo.configurable-signatures.tests
  (:use #:cl #:clempo.base #:clempo.signatures))

(in-package #:clempo.configurable-signatures.tests)

;;; T14 ConfigurableAccount signatures (0x05 || RLP) built here from secp256k1 owner keys.
;;; Transactions signed this way and executed by the Rust reference are checked end to end
;;; in tests/tempo/executor (the T14 devnet blocks).

(defun owner-address (key) (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public key)))

(defun sign-digest (key digest)
  (multiple-value-bind (r s recid) (clempo.secp256k1:sign digest key)
    (octets-concat (integer->octets r 32) (integer->octets s 32) (octets-of recid))))

(defparameter *keys* '(#x1111 #x2222 #x3333))

(defun owners (&optional (keys *keys*) (weights '(1 1 1)))
  (sort (mapcar (lambda (k w) (list (owner-address k) w)) keys weights) #'< :key #'first))

(defun key-of (address) (find address *keys* :key #'owner-address))

(defun build (inner-digest &key (threshold 2) (owners (owners)) (salt 7) (version 0) account
                                signers)
  "A ConfigurableAccountSignature over INNER-DIGEST; SIGNERS (owner addresses, in approval
order) default to the first THRESHOLD owners."
  (let* ((unsigned (clempo.signatures::make-configurable-signature
                    :salt salt :version version :threshold threshold :owners owners))
         (account (or account (configurable-account-address unsigned)))
         (template (clempo.signatures::make-configurable-signature
                    :account account :salt salt :version version :threshold threshold
                    :owners owners))
         (digest (clempo.signatures::configurable-digest template inner-digest))
         (signers (or signers (subseq (mapcar #'first owners) 0 threshold))))
    (values (clempo.signatures::encode-configurable-signature
             (clempo.signatures::make-configurable-signature
              :account account :salt salt :version version :threshold threshold :owners owners
              :signatures (mapcar (lambda (o) (sign-digest (key-of o) digest)) signers)))
            account)))

(defparameter *hash* (clempo.keccak:keccak256 (string->octets "a transaction")))

(deftest configurable-signature-recovers-its-account ()
  (multiple-value-bind (signature account) (build *hash*)
    (is (eq :configurable-account (signature-kind signature)))
    (is (eq :configurable-account (signature-inner-kind signature)))
    (multiple-value-bind (sender key-id) (recover-signature signature *hash*)
      (is= account sender)
      (is (null key-id)))
    (let ((parsed (configurable-signature signature)))
      (is= account (configurable-signature-account parsed))
      (is (null (configurable-commitment-error parsed 0)))
      (is (null (configurable-commitment-error parsed (configurable-commitment parsed))))
      (is (eq :commitment-mismatch (configurable-commitment-error parsed 1))))
    ;; another digest recovers other owners, who sign nothing here
    (signals signature-error
      (recover-signature signature (clempo.keccak:keccak256 (string->octets "another"))))))

(deftest configurable-keychain-v2-delegate ()
  (let* ((user #xabcdef0000000000000000000000000000000001)
         (delegated (clempo.keccak:keccak256
                     (octets-concat (octets-of 4) *hash* (integer->octets user 20)))))
    (multiple-value-bind (inner account) (build delegated)
      (let ((v2 (octets-concat (octets-of 4) (integer->octets user 20) inner))
            (v1 (octets-concat (octets-of 3) (integer->octets user 20) inner)))
        (is (eq :keychain-v2 (signature-kind v2)))
        (is (eq :configurable-account (signature-inner-kind v2)))
        (multiple-value-bind (sender key-id) (recover-signature v2 *hash*)
          (is= user sender)
          (is= account key-id))
        ;; configurable_account access keys require keychain V2
        (signals signature-error (signature-kind v1))))))

(deftest configurable-quorum-rules ()
  (let ((sorted (mapcar #'first (owners))))
    ;; weight below the threshold, a signer twice, descending signers, an excess approval
    (dolist (signers (list (list (first sorted))
                           (list (first sorted) (first sorted))
                           (list (second sorted) (first sorted))
                           (list (first sorted) (second sorted) (third sorted))))
      (signals signature-error
        (recover-signature (build *hash* :signers signers) *hash*)))
    ;; a non-owner signer
    (let ((owners (owners)) (*keys* (cons #x4444 *keys*)))  ; parallel: owners exclude #x4444
      (signals signature-error
        (recover-signature (build *hash* :threshold 1 :owners owners
                                         :signers (list (owner-address #x4444)))
                           *hash*)))
    ;; threshold 2 with weights (2 1 1): one heavy approval suffices
    (let ((owners (owners *keys* '(2 1 1))))
      (multiple-value-bind (signature account)
          (build *hash* :threshold 2 :owners owners
                        :signers (list (first (find 2 owners :key #'second))))
        (is= account (recover-signature signature *hash*))))))

(deftest configurable-shape-rules ()
  (flet ((rejects (signature) (signals signature-error (signature-kind signature))))
    (let ((good (build *hash*)))
      (rejects (octets-concat good (octets-of 0)))  ; decode_exact: no trailing byte
      (rejects (subseq good 0 (1- (length good)))))
    ;; configuration rules (validate_for_account): zero threshold, unreachable threshold,
    ;; owners out of order, too many approvals
    (rejects (build *hash* :threshold 0 :signers (list (first (first (owners))))))
    (rejects (build *hash* :threshold 4 :signers (mapcar #'first (owners))))
    (rejects (build *hash* :owners (reverse (owners))))
    (rejects (clempo.signatures::encode-configurable-signature
              (clempo.signatures::make-configurable-signature
               :account 1 :salt 0 :threshold 1 :owners (owners)
               :signatures (make-list 9 :initial-element (make-octets 65)))))
    ;; an owner approval must be primitive
    (rejects (clempo.signatures::encode-configurable-signature
              (clempo.signatures::make-configurable-signature
               :account 1 :salt 0 :threshold 1 :owners (owners)
               :signatures (list (octets-concat (octets-of 4) (make-octets 20) (make-octets 65))))))
    ;; the zero account
    (rejects (build *hash* :account 0))))

(deftest configurable-verification-gas-parts ()
  "verification_gas: owner approvals (3000 each for secp256k1), the witness calldata and the
two keccaks; the transaction pays it less the 3000 its base covers (signature_gas.rs)."
  (let* ((signature (build *hash*))
         (parsed (configurable-signature signature))
         (gas (clempo.signatures:configurable-verification-gas parsed)))
    (is (> gas 6000))
    (is= (- gas 3000) (clempo.transactions:tempo-signature-verification-gas signature))))
