(defpackage #:clempo.rpc.witness
  (:use #:cl #:clempo.base #:clempo.rpc)
  (:local-nicknames (#:sig #:clempo.signatures) (#:rlp #:clempo.rlp))
  (:documentation "The mock signatures a Tempo request is simulated with (tempo
crates/alloy/src/rpc/reth_compat.rs): create_mock_primitive_signature and
mock_webauthn_signature, and simulated_block_signature's configurable-account witnesses built
from configurableAccountSimulation and keyAuthorizationSimulation
(ConfigurableAccountSimulationSpec::mock_signature). eth_call, eth_estimateGas and
eth_createAccessList (TryIntoTxEnv) and eth_fillTransaction and eth_simulateV1 (TryIntoSimTx)
share them; a rejected witness signals WITNESS-ERROR, which TryIntoSimTx's callers report with
their own prefix.")
  (:export #:mock-webauthn-signature #:mock-primitive-signature #:simulated-signature
           #:witness-error))

(in-package #:clempo.rpc.witness)

(define-condition witness-error (rpc-error) ()
  (:documentation "A simulated witness the reference refuses: -32602 with its message."))

(defun witness-fail (message)
  (error 'witness-error :code +invalid-params+ :message message))

;;; Primitive mock signatures

(defun mock-webauthn-signature (size)
  "mock_webauthn_signature: authenticator data and a clientDataJSON padded to SIZE bytes."
  (let* ((base "{\"type\":\"webauthn.get\",\"challenge\":\"\",\"origin\":\"\"}")
         (size (max size (+ 37 (length base))))
         (auth (fill (make-octets 37) #xff)))
    (setf (aref auth 32) 1)
    (octets-concat (octets-of 2) auth
                   (string->octets (format nil "{\"type\":\"webauthn.get\",\"challenge\":\"\",\"origin\":\"~A\"}"
                                           (make-string (- size 37 (length base)) :initial-element #\x)))
                   (make-octets 128))))

(defun mock-primitive-signature (type key-data &optional (max-webauthn-data 8192))
  "create_mock_primitive_signature as wire bytes; KEY-DATA of 1, 2 or 4 bytes sizes WebAuthn
data, at most MAX-WEBAUTHN-DATA (MAX_MOCK_WEBAUTHN_DATA_LEN for a transaction's own signature)."
  (ecase type
    (:secp256k1 (octets-concat (make-octets 64) (octets-of 27)))
    (:p256 (octets-concat (octets-of 1) (make-octets 129)))
    (:webauthn (mock-webauthn-signature
                (min max-webauthn-data (if (and key-data (member (length key-data) '(1 2 4)))
                                           (octets->integer key-data)
                                           800))))))

;;; Configurable-account witnesses

(define-constant +max-owner-webauthn-data+ (- 2048 128)
  "MAX_OWNER_WEBAUTHN_DATA_LEN: a WebAuthn signature's limit without r, s and the public key.")

(defparameter *config-errors*
  '((:empty-owners . "configurable account owners cannot be empty")
    (:too-many-owners . "too many configurable account owners")
    (:zero-threshold . "configurable account threshold cannot be zero")
    (:zero-owner . "configurable account owner cannot be zero")
    (:zero-weight . "configurable account owner weight cannot be zero")
    (:duplicate-owner . "configurable account owners cannot contain duplicates")
    (:owners-not-ascending . "configurable account owners must be strictly ascending")
    (:total-weight-exceeds-max . "configurable account total owner weight exceeds u8::MAX")
    (:threshold-exceeds-weight . "configurable account threshold cannot be reached within the owner approval limit")
    (:account-is-owner . "configurable account cannot own itself"))
  "ConfigurableAccountConfigError's messages by clempo.signatures:configurable-config-error keyword.")

(defun owner-weight (owners owner)
  "ConfigurableAccountConfig::owner_weight: Rust's binary_search_by_key over OWNERS, which
the request need not have sorted (an unsorted list can miss an owner it holds)."
  (let ((v (coerce owners 'simple-vector)) (base 0))
    (unless (zerop (length v))
      (loop with size = (length v)
            while (> size 1)
            do (let ((mid (+ base (floor size 2))))
                 (unless (> (first (svref v mid)) owner) (setf base mid))
                 (decf size (floor size 2))))
      (when (= (first (svref v base)) owner) (second (svref v base))))))

(defun owner-approval (approval)
  "An owner's mock approval: create_mock_primitive_signature for its key type, else a
WebAuthn one of the largest data, WebAuthn data capped at +MAX-OWNER-WEBAUTHN-DATA+."
  (destructuring-bind (&key key-type key-data &allow-other-keys) approval
    (if key-type
        (mock-primitive-signature key-type key-data +max-owner-webauthn-data+)
        (mock-webauthn-signature +max-owner-webauthn-data+))))

(defun mock-witness (spec account)
  "ConfigurableAccountSimulationSpec::mock_signature for ACCOUNT: the claimed quorum checked
as ConfigurableAccountWeightAccumulator does (without recovery), then try_new's checks; the
0x05 signature octets."
  (destructuring-bind (&key salt version threshold owners) (getf spec :config)
    (when (zerop threshold) (witness-fail "configurable account threshold cannot be zero"))
    (let ((count 0) (weight 0) (min-weight nil) (previous nil))
      (dolist (approval (getf spec :approvals))
        (let* ((owner (getf approval :owner))
               (w (or (owner-weight owners owner) (witness-fail "configurable account signer is not an owner"))))
          (when (and (plusp count) (>= weight threshold)) (witness-fail "excess configurable account owner signatures"))
          (when (zerop w) (witness-fail "configurable account signer is not an owner"))
          (when (> (incf count) 8) (witness-fail "too many configurable account signatures"))
          (when (and previous (>= previous owner))
            (witness-fail "configurable account recovered owners must be strictly ascending"))
          (setf previous owner weight (+ weight w) min-weight (min w (or min-weight w)))))
      (cond ((zerop count) (witness-fail "configurable account signatures cannot be empty"))
            ((< weight threshold) (witness-fail "configurable account signature weight below threshold"))
            ((>= weight (+ threshold min-weight)) (witness-fail "excess configurable account owner signatures"))))
    (when (zerop account) (witness-fail "configurable account cannot be zero"))
    (let ((e (sig:configurable-config-error account version threshold owners)))
      (when e (witness-fail (cdr (assoc e *config-errors*)))))
    (sig:encode-configurable-signature
     (sig:make-configurable-signature :account account :salt (octets->integer salt) :version version
                                       :threshold threshold :owners owners
                                       :signatures (mapcar #'owner-approval (getf spec :approvals))))))

(defun simulated-signature (&key from key-id key-authorization key-authorization-simulation
                                 account-simulation)
  "simulated_block_signature for a request's FROM, KEY-ID, KEY-AUTHORIZATION (its RLP items
and signature) and simulation specs: the key authorization re-signed with
KEY-AUTHORIZATION-SIMULATION's mock witness (or its own configurable witness verified), and
ACCOUNT-SIMULATION's mock witness, in a V2 keychain for a delegate (KEY-ID). Returns (values
signature-or-nil key-authorization); a refused witness signals WITNESS-ERROR."
  (let ((from (and from (plusp from) from))
        (ka key-authorization) (ka-spec key-authorization-simulation) (spec account-simulation))
    (flet ((parent () (or from (witness-fail "configurable account simulation requires `from`"))))
      (cond ((and ka-spec ka)
             (setf ka (list (first ka) (mock-witness ka-spec (or (getf ka-spec :signer) (parent))))))
            ((and ka (eq (sig:signature-kind (second ka)) :configurable-account))
             (handler-case (sig:verify-configurable-approvals
                            (sig:parse-configurable-signature (second ka) 1)
                            (clempo.keccak:keccak256 (rlp:rlp-encode (first ka))))
               (sig:signature-error (e) (witness-fail (sig:signature-error-reason e))))))
      (if (null spec)
          (values nil ka)
          (let ((parent (parent)))
            (when (eql key-id parent) (witness-fail "a configurable delegate cannot be its own parent"))
            (let ((account (or key-id parent)))
              (when (and (getf spec :signer) (/= (getf spec :signer) account))
                (witness-fail "configurable account simulation signer does not match the role account"))
              (let ((witness (mock-witness spec account)))
                (values (if key-id (octets-concat (octets-of 4) (integer->octets parent 20) witness) witness)
                        ka))))))))
