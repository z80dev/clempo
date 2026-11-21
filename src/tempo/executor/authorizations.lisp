(in-package #:clempo.executor)

;;; Authorization lists. Type-4 transactions run evm2's EIP-7702 handler
;;; (evm2 crates/evm2/src/ethereum/eip7702.rs validate_one_auth, apply_auth_list, AuthRefunds;
;;; lazy_eip7702.rs recover_authority) as tempo crates/evm/src/handler/config.rs
;;; prepare_eip7702 configures it; 0x76 transactions run tempo crates/evm/src/handler.rs
;;; apply_one_authorization and apply_authorization_list. EIP-8037 and EIP-2780 are never
;;; enabled on Tempo mainnet, so only the regular refund (GasId::TxEip7702AuthRefund: 12500
;;; before T1, 0 from T1, crates/hardfork/src/gas_params.rs) exists.
;;; Both run against EVM:*WORLD* under EVM:*SPEC*, after the sender's nonce bump and warming.

(defun authorization-authority (auth)
  "The recovered authority of decoded authorization AUTH, or NIL when it does not recover
\(clempo.signatures:authorization-signer signals SIGNATURE-ERROR for every failure)."
  (handler-case (values (clempo.signatures:authorization-signer auth))
    (clempo.signatures:signature-error () nil)))

(defun apply-one-authorization (world chain-id auth-chain-id address nonce authority-fn)
  "validate_one_auth followed by set_delegation. AUTHORITY-FN recovers the authority lazily
\(after the chain id and nonce checks). Returns (values applied-p refund-eligible-p)."
  (declare (type integer chain-id auth-chain-id address nonce) (type function authority-fn))
  (unless (or (zerop auth-chain-id) (= auth-chain-id chain-id))
    (return-from apply-one-authorization (values nil nil)))
  (when (= nonce #xffffffffffffffff)
    (return-from apply-one-authorization (values nil nil)))
  (let ((authority (funcall authority-fn)))
    (unless authority (return-from apply-one-authorization (values nil nil)))
    (state:warm-address world authority)
    (let* ((account (state:get-account world authority))
           (eligible (or (state:account-exists account) (state:account-touched account)))
           (code (state:account-code world account)))
      (unless (or (zerop (length code)) (state:delegation-target code))
        (return-from apply-one-authorization (values nil nil)))
      (unless (= nonce (state:account-nonce account))
        (return-from apply-one-authorization (values nil nil)))
      ;; Account::set_delegation: designator (or empty code for the zero address), nonce + 1.
      (state:set-code world account (if (zerop address)
                                        state:+empty-code+
                                        (state:delegation-designator address)))
      (state:increment-nonce world account)
      (values t eligible))))

(defun auth-refund-per-authorization ()
  (evm:gas-params-auth-refund (evm:spec-gas evm:*spec*)))

(defun apply-ethereum-authorizations (authorizations chain-id)
  "Apply a type-4 transaction's AUTHORIZATIONS, the raw RLP items of
TRANSACTION-AUTHORIZATION-LIST, each (chain-id address nonce y-parity r s). Returns the
refund to add: TxEip7702AuthRefund per applied authorization whose authority existed or
was touched."
  (let ((world evm:*world*)
        (per-auth (auth-refund-per-authorization))
        (refund 0))
    (dolist (auth authorizations refund)
      (destructuring-bind (auth-chain-id address nonce &rest signature) auth
        (declare (ignore signature))
        (multiple-value-bind (applied eligible)
            (apply-one-authorization world chain-id
                                     (rlp:rlp-decode-integer auth-chain-id)
                                     (octets->integer address)
                                     (rlp:rlp-decode-integer nonce)
                                     (lambda () (authorization-authority auth)))
          (when (and applied eligible) (incf refund per-auth)))))))

(defun keychain-authorization-p (auth)
  (member (clempo.signatures:signature-kind (fourth auth)) '(:keychain-v1 :keychain-v2)))

(defun apply-tempo-authorizations (authorizations chain-id fork)
  "Apply a 0x76 transaction's AUTHORIZATIONS, the raw RLP items of
TRANSACTION-TEMPO-AUTHORIZATION-LIST, each (chain-id address nonce signature). From T0
keychain-signed authorizations are skipped entirely. Returns the regular refund:
TxEip7702AuthRefund per applied authorization whose authority existed or was touched."
  (let ((world evm:*world*)
        (per-auth (auth-refund-per-authorization))
        (refund 0))
    (dolist (auth authorizations refund)
      (unless (and (forks:fork>= fork :t0) (keychain-authorization-p auth))
        (destructuring-bind (auth-chain-id address nonce signature) auth
          (declare (ignore signature))
          (multiple-value-bind (applied eligible)
              (apply-one-authorization world chain-id
                                       (rlp:rlp-decode-integer auth-chain-id)
                                       (octets->integer address)
                                       (rlp:rlp-decode-integer nonce)
                                       (lambda () (authorization-authority auth)))
            (when (and applied eligible) (incf refund per-auth))))))))
