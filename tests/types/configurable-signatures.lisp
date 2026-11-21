(defpackage #:clempo.configurable-signatures.tests
  (:use #:cl #:clempo.base #:clempo.signatures))

(in-package #:clempo.configurable-signatures.tests)

;;; T14 ConfigurableAccount signatures (0x05 || RLP) built here from secp256k1 owner keys.
;;; Transactions signed this way and executed by the Rust reference are checked end to end
;;; in tests/tempo/executor (the T14 devnet blocks).

(defun owner-key (key) "The private key of the small scalar KEY." (integer->octets key 32))

(defun owner-address (key)
  (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public (owner-key key))))

(defun sign-digest (key digest)
  (multiple-value-bind (r s recid) (clempo.secp256k1:sign digest (owner-key key))
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

(deftest configurable-grant-names-its-account ()
  "SignedKeyAuthorization::authorizing_account: a configurable-signed grant names its account
without recovering the quorum, which the executor verifies over key-authorization-hash."
  (let* ((fields (list 1337 3 (make-octets 20)))
         (hash (clempo.keccak:keccak256 (clempo.rlp:rlp-encode fields))))
    (multiple-value-bind (signature account) (build hash)
      (let ((grant (list fields signature)))
        (is-equalp hash (key-authorization-hash grant))
        (is= account (key-authorization-signer grant))
        (is= account (verify-configurable-approvals (configurable-signature signature)
                                                    (key-authorization-hash grant))))
      (let ((other (list (list 1 3 (make-octets 20)) signature)))
        (is= account (key-authorization-signer other))
        (signals signature-error
          (verify-configurable-approvals (configurable-signature signature)
                                         (key-authorization-hash other)))))))

(deftest reference-devnet-t14-transactions ()
  "Transactions a T15 reference devnet accepted and sealed (data/README.md): clempo decodes
each and recovers the sender the Rust node recovered, through the owner quorum where the
signature is a ConfigurableAccount one."
  (let ((records (with-open-file (in (merge-pathnames "data/t14-devnet-transactions.json"
                                                      (asdf:system-relative-pathname
                                                       "clempo-types" "../../tests/types/")))
                   (let ((text (make-string (file-length in))))
                     (clempo.json:parse-json (subseq text 0 (read-sequence text in))))))
        (configurable 0))
    (is= 7 (length records))
    (loop for record across records
          for tx = (clempo.transactions:decode-transaction
                    (hex->octets (clempo.json:jget record "raw")))
          for signature = (car (last (clempo.transactions:transaction-fields tx)))
          do (is= (hex->integer (clempo.json:jget record "from"))
                  (clempo.transactions:transaction-sender tx))
             (when (member (signature-inner-kind signature) '(:configurable-account))
               (incf configurable)))
    ;; owner-signed, derived first use, and the V2 keychain delegating to the 2-of-2 quorum
    (is= 3 configurable)))

(deftest reference-rehashes-key-authorization-parity ()
  "A key-type-3 grant whose key authorization signature carries parity 0 (not 27): the
reference refused it ('admin-signed key authorization account mismatch') because it hashes its
re-encoding, v = 27, and so recovered sender 0x5f52... (tempo-primitives decode of these
octets, 2026-11-21), not the grantor. clempo recovers the same sender."
  (let ((tx (clempo.transactions:decode-transaction
             (hex->octets "0x76f8d18205398085174876e800832dc6c0d8d79420c00000000000000000000000000000000000008080c0808080808080c0f85dd9820539039473abdfff0489cf370e06cb1d403777e60d6aeaecb8417c8619c41d74228bba06d42ad2b572fe7ec3dbc7f715bc90086d26a8262a91f338f849bcf09ab0598a84ffd7554c30089cb1b1c487936fafebc470e77d26962d00b841eeccb5da7c6848935b9ee1d6a028e54755b376a4de79e37f6c02ed4014dca65d7719245ff0dcd2bd1a9009fb773a2f8b002622d8cbf8a965ecaf005aae608ca800"))))
    (is (octets= (hex->octets "0x3b14b220e96b647cca5c53c6d42f8b7114e4fc26091ee61a62fdc9ddff6fa7cf")
                 (clempo.transactions:transaction-signing-hash tx)))
    (is= #x5f526a65359ea8f44a320415a744095a5a12993d (clempo.transactions:transaction-sender tx))))
