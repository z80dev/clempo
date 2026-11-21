(defpackage #:clempo.rpc.fill
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:calls #:clempo.rpc.calls)
                    (#:rlp #:clempo.rlp) (#:tx #:clempo.transactions) (#:state #:clempo.state)
                    (#:contract #:clempo.contract) (#:sig #:clempo.signatures))
  (:documentation "eth_fillTransaction (reth EthTransactions::fill_transaction with Tempo's
LoadState::next_available_nonce_for and TryIntoSimTx): the request's defaults filled in (value,
nonce, chain id, gas from eth_estimateGas's search at the head, fees from the head's base fee)
and built into an unsigned transaction with the simulation signature (the mock
configurable-account witnesses of configurableAccountSimulation and keyAuthorizationSimulation,
else TempoSignature::default), returned as its raw EIP-2718 encoding and its primitive JSON.
clempo keeps no transaction pool, so the next nonce is the head state's.")
  (:export #:aa-envelope #:+aa-default-signature+))

(in-package #:clempo.rpc.fill)

(define-constant +expiring-nonce-key+ (1- (ash 1 256)) "TEMPO_EXPIRING_NONCE_KEY")
(define-constant +nonce-precompile+ #x4e4f4e4345000000000000000000000000000000 "NONCE_PRECOMPILE_ADDRESS")

(define-constant +aa-default-signature+
  (hex->octets (concatenate 'string
                            "840cfc572845f5786e702984c2a582528cad4b49b2a10b9db1be7fca90058565"
                            "25e7109ceb98168d95b09b18bbf6b685130e0562f233877d492b94eee0c5b6d1"
                            "1b"))
  "TempoSignature::default: alloy's Signature::test_signature as a secp256k1 primitive
\(r || s || 27 + parity).")

(define-constant +simulated-legacy-signature+ '(1 1)
  "Tempo's SIMULATED_LEGACY_SIGNATURE (r 1, s 1, parity false); typed transactions keep alloy's
all-zero placeholder.")

;;; Defaults

(defun latest-state () (state-at :latest))

(defun next-nonce (r)
  "Tempo next_available_nonce_for: a 2D lane's nonce from the nonce precompile, the expiring
lane's 0, else the account nonce (no pool adds pending transactions)."
  (let ((key (calls:req-nonce-key r)) (from (calls:req-from r)))
    (flet ((no-account () (rpc-fail +invalid-params+ "unknown account")))
      (cond ((and key (= key +expiring-nonce-key+)) 0)
            ((and key (plusp key))
             (unless from (no-account))
             ;; NonceManager::nonces[account][key], mapping slot 0, saturated to u64
             (min +u64-max+ (state:backend-storage (latest-state) +nonce-precompile+
                                                   (contract:mapping-slot key (contract:mapping-slot from 0)))))
            (t (unless from (no-account))
               (nth-value 1 (state:backend-account (latest-state) from)))))))

(defun fill-request (request)
  "reth fill_transaction's defaults, in its order; returns the filled copy of REQUEST."
  (let ((r (calls:copy-call-request request))
        (chain-id (chain:backend-chain-id chain:*backend*)))
    (when (and (calls:req-gas-price r) (or (calls:req-max-fee r) (calls:req-max-priority r)))
      (rpc-fail +invalid-params+ "both gasPrice and (maxFeePerGas or maxPriorityFeePerGas) specified"))
    (unless (calls:req-value r) (setf (calls:req-value r) 0))
    (unless (calls:req-nonce r) (setf (calls:req-nonce r) (next-nonce r)))
    (let ((asked (calls:req-chain-id r)))
      (when (and asked (/= asked chain-id))
        (rpc-fail +invalid-params+ (format nil "chainId does not match node's (have=~D, want=~D)" asked chain-id))))
    (setf (calls:req-chain-id r) chain-id)
    (unless (calls:req-gas r)
      (setf (calls:req-gas r) (calls:estimate-request-gas r :pending)))
    (unless (calls:req-gas-price r)
      ;; the suggested tip is eth_maxPriorityFeePerGas's
      (let ((tip (or (calls:req-max-priority r) (setf (calls:req-max-priority r) 0))))
        (unless (calls:req-max-fee r)
          (setf (calls:req-max-fee r) (+ (* 2 (calls:header-base-fee (chain-block chain:*head*))) tip)))))
    r))

;;; Building the simulated transaction

(defun to-octets (to) (if (integerp to) (integer->octets to 20) (make-octets 0)))

(defun access-list-items (r)
  (mapcar (lambda (item) (list (integer->octets (getf item :address) 20) (coerce (getf item :storage-keys) 'list)))
          (calls:req-access-list r)))

(defun ethereum-envelope (r)
  "alloy build_typed_simulate_transaction for an Ethereum request, as raw EIP-2718 octets."
  (let ((type (calls:request-type r))
        (chain-id (calls:req-chain-id r)) (nonce (calls:req-nonce r)) (gas (calls:req-gas r))
        (to (to-octets (calls:req-to r))) (value (calls:req-value r))
        (input (or (calls:request-input r) (make-octets 0))))
    (flet ((typed (type fields) (octets-concat (octets-of type) (rlp:rlp-encode (append fields '(0 0 0))))))
      (ecase type
        (0 (rlp:rlp-encode (list* nonce (calls:req-gas-price r) gas to value input (+ 35 (* 2 chain-id))
                                  +simulated-legacy-signature+)))
        (1 (typed 1 (list chain-id nonce (calls:req-gas-price r) gas to value input (access-list-items r))))
        (2 (typed 2 (list chain-id nonce (calls:req-max-priority r) (calls:req-max-fee r) gas to value input
                          (access-list-items r))))
        (4 (typed 4 (list chain-id nonce (calls:req-max-priority r) (calls:req-max-fee r) gas to value input
                          (access-list-items r)
                          (mapcar (lambda (a) (list (getf a :chain-id) (integer->octets (getf a :address) 20)
                                                    (getf a :nonce) (getf a :y-parity) (getf a :r) (getf a :s)))
                                  (calls:req-authorizations r)))))))))

(defun aa-fail (message)
  "build_simulate_v1_transaction's conversion error."
  (rpc-fail +invalid-params+ (format nil "Failed to convert transaction into RPC response: ~A" message)))

;;; Simulated configurable-account witnesses (tempo crates/alloy/src/rpc/reth_compat.rs
;;; simulated_block_signature and ConfigurableAccountSimulationSpec::mock_signature)

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
  "An owner's mock approval: create_mock_primitive_signature for its key type, a WebAuthn one
of the largest data otherwise, WebAuthn data capped at +MAX-OWNER-WEBAUTHN-DATA+."
  (destructuring-bind (&key key-type key-data &allow-other-keys) approval
    (case key-type
      ((nil) (calls:mock-webauthn-signature +max-owner-webauthn-data+))
      (:webauthn (calls:mock-webauthn-signature
                  (min +max-owner-webauthn-data+
                       (if (and key-data (member (length key-data) '(1 2 4))) (octets->integer key-data) 800))))
      (t (calls:mock-primitive-signature key-type key-data)))))

(defun mock-witness (spec account)
  "ConfigurableAccountSimulationSpec::mock_signature for ACCOUNT: the claimed quorum checked
as ConfigurableAccountWeightAccumulator does (without recovery), then try_new's checks; the
0x05 signature octets."
  (destructuring-bind (&key salt version threshold owners) (getf spec :config)
    (when (zerop threshold) (aa-fail "configurable account threshold cannot be zero"))
    (let ((count 0) (weight 0) (min-weight nil) (previous nil))
      (dolist (approval (getf spec :approvals))
        (let* ((owner (getf approval :owner))
               (w (or (owner-weight owners owner) (aa-fail "configurable account signer is not an owner"))))
          (when (and (plusp count) (>= weight threshold)) (aa-fail "excess configurable account owner signatures"))
          (when (zerop w) (aa-fail "configurable account signer is not an owner"))
          (when (> (incf count) 8) (aa-fail "too many configurable account signatures"))
          (when (and previous (>= previous owner))
            (aa-fail "configurable account recovered owners must be strictly ascending"))
          (setf previous owner weight (+ weight w) min-weight (min w (or min-weight w)))))
      (cond ((zerop count) (aa-fail "configurable account signatures cannot be empty"))
            ((< weight threshold) (aa-fail "configurable account signature weight below threshold"))
            ((>= weight (+ threshold min-weight)) (aa-fail "excess configurable account owner signatures"))))
    (when (zerop account) (aa-fail "configurable account cannot be zero"))
    (let ((e (sig:configurable-config-error account version threshold owners)))
      (when e (aa-fail (cdr (assoc e *config-errors*)))))
    (sig::encode-configurable-signature
     (sig::make-configurable-signature :account account :salt (octets->integer salt) :version version
                                       :threshold threshold :owners owners
                                       :signatures (mapcar #'owner-approval (getf spec :approvals))))))

(defun simulated-signature (r)
  "simulated_block_signature: a key authorization re-signed with keyAuthorizationSimulation's
mock witness (or its own configurable witness verified), and the transaction's mock witness
from configurableAccountSimulation, a V2 keychain one for a delegate (keyId). Returns (values
signature-or-nil key-authorization)."
  (let* ((from (let ((f (calls:req-from r))) (and f (plusp f) f)))
         (ka (calls:req-key-authorization r))
         (ka-spec (calls:req-key-authorization-simulation r))
         (spec (calls:req-account-simulation r))
         (key-id (calls:req-key-id r)))
    (flet ((parent () (or from (aa-fail "configurable account simulation requires `from`"))))
      (cond ((and ka-spec ka)
             (setf ka (list (first ka) (mock-witness ka-spec (or (getf ka-spec :signer) (parent))))))
            ((and ka (eq (sig:signature-kind (second ka)) :configurable-account))
             (handler-case (sig:verify-configurable-approvals
                            (sig:parse-configurable-signature (second ka) 1)
                            (clempo.keccak:keccak256 (rlp:rlp-encode (first ka))))
               (sig:signature-error (e) (aa-fail (sig::signature-error-reason e))))))
      (if (null spec)
          (values nil ka)
          (let ((parent (parent)))
            (when (eql key-id parent) (aa-fail "a configurable delegate cannot be its own parent"))
            (let ((account (or key-id parent)))
              (when (and (getf spec :signer) (/= (getf spec :signer) account))
                (aa-fail "configurable account simulation signer does not match the role account"))
              (let ((witness (mock-witness spec account)))
                (values (if key-id (octets-concat (octets-of 4) (integer->octets parent 20) witness) witness)
                        ka))))))))

(defun aa-envelope (r)
  "Tempo TryIntoSimTx for an AA request: build_aa signed with the simulated witness, else
TempoSignature::default, as raw 0x76 octets."
  (multiple-value-bind (signature key-authorization) (simulated-signature r)
    (when (and (null signature) (eq (calls:req-key-type r) :configurableaccount))
      (aa-fail "configurable account simulation requires a configuration witness"))
    (let ((calls (calls:req-calls r)) (to (calls:req-to r)))
      (when (and calls (eq to :create)) (setf to nil))
      (when (and (null calls) (null to)) (aa-fail "Missing 'calls' or 'to' field for Tempo transaction."))
      (unless (calls:req-max-fee r) (aa-fail "Missing 'max_fee_per_gas' field for Tempo transaction."))
      (unless (calls:req-max-priority r) (aa-fail "Missing 'max_priority_fee_per_gas' field for Tempo transaction."))
      (let* ((calls (append calls (when to (list (list to (calls:req-value r)
                                                       (or (calls:request-input r) (make-octets 0)))))))
             (sponsor (calls:req-fee-payer-signature r)))
        (octets-concat
         (octets-of #x76)
         (rlp:rlp-encode
          (append
           (list (calls:req-chain-id r) (calls:req-max-priority r) (calls:req-max-fee r) (calls:req-gas r)
                 (mapcar (lambda (c) (list (to-octets (first c)) (or (second c) 0) (third c))) calls)
                 (access-list-items r)
                 (or (calls:req-nonce-key r) 0) (calls:req-nonce r)
                 (or (calls:req-valid-before r) (make-octets 0)) (or (calls:req-valid-after r) (make-octets 0))
                 (to-octets (calls:req-fee-token r))
                 (if sponsor
                     (list (if (getf sponsor :y-parity) 1 0) (getf sponsor :r) (getf sponsor :s))
                     (make-octets 0))
                 (calls:req-tempo-authorizations r))
           (when key-authorization (list key-authorization))
           (list (or signature +aa-default-signature+)))))))))

(defrpc "eth_fillTransaction" ((request calls:tx-request))
  "the request with its defaults filled in, as an unsigned transaction and its raw encoding"
  (let* ((r (fill-request request))
         (raw (if (calls:aa-request-p r) (aa-envelope r) (ethereum-envelope r)))
         (tx (tx:decode-transaction raw))
         (head (chain-block chain:*head*))
         (b (make-cblock :number (cblock-number head) :hash (cblock-hash head) :header (cblock-header head)
                         :raw (cblock-raw head) :txs (vector tx) :tx-hashes (vector (tx:transaction-hash tx))
                         :senders (vector (list (or (calls:req-from r) 0) nil)))))
    (json:jobject "raw" (hexbytes raw) "tx" (primitive-tx-json b 0))))
