(in-package #:clempo.receive-policy-guard)

;;; ReceivePolicyGuard (crates/precompiles/src/receive_policy_guard, TIP-1028; active from
;;; T6): holds blocked inbound TIP-20 transfers and mints, keyed by the keccak of the
;;; ABI-encoded ClaimReceiptV1, until the recovery authority claims or an issuer burns them.

(defcontract receive-policy-guard (:address +receive-policy-guard-address+)
  (nonce u64)
  (balances (mapping b256 u256)))

(deferror invalid-receipt "InvalidReceipt()")
(deferror invalid-claim-address "InvalidClaimAddress()")
(deferror unauthorized-claimer "UnauthorizedClaimer()")
(deferror address-reserved "AddressReserved()")
;; TIP20Error::InvalidToken, raised by TIP20Token::from_address.
(deferror invalid-token "InvalidToken()")

(defevent transfer-blocked "TransferBlocked(address indexed token, address indexed receiver, uint64 indexed blockedNonce, uint256 amount, uint8 receiptVersion, bytes receipt)")
(defevent receipt-claimed "ReceiptClaimed(address indexed token, address indexed receiver, uint64 indexed blockedNonce, uint64 blockedAt, uint8 receiptVersion, address originator, address recipient, address recoveryAuthority, address caller, address to, uint256 amount)")
(defevent receipt-burned "ReceiptBurned(address indexed token, address indexed receiver, uint64 indexed blockedNonce, uint64 blockedAt, uint8 receiptVersion, address originator, address recipient, address recoveryAuthority, address caller, uint256 amount)")

(defconstant +blocked-receipt-version+ 1)
(defconstant +recovery-originator+ 0 "Recovery-authority sentinel: the originator claims.")
(defconstant +inbound-kind-invalid+ 255 "alloy's InboundKind::__Invalid discriminant.")

;;; ClaimReceiptV1: a static 10-word tuple (version, token, recoveryAuthority, originator,
;;; recipient, blockedAt, blockedNonce, blockedReason, kind, memo).

(defstruct (receipt (:constructor make-receipt
                        (token recovery-authority originator recipient blocked-at blocked-nonce
                         blocked-reason kind memo &optional (version +blocked-receipt-version+))))
  version token recovery-authority originator recipient blocked-at blocked-nonce
  blocked-reason kind memo)

(defun word (octets index)
  (octets->integer (subseq octets (* 32 index) (* 32 (1+ index)))))

(defun decode-receipt (octets)
  "ClaimReceiptV1::try_from: alloy's non-validating abi_decode (each field keeps its low
bits, an out-of-range kind is __Invalid, trailing bytes are ignored); short data is
InvalidReceipt."
  (when (< (length octets) 320) (revert 'invalid-receipt))
  (flet ((low (i bits) (ldb (byte bits 0) (word octets i))))
    (let ((kind (low 8 8)))
      (make-receipt (low 1 160) (low 2 160) (low 3 160) (low 4 160) (low 5 64) (low 6 64)
                    (low 7 8) (if (> kind 1) +inbound-kind-invalid+ kind) (word octets 9)
                    (low 0 8)))))

(defun encode-receipt (r)
  (let ((out (make-octets 320)))
    (loop for v in (list (receipt-version r) (receipt-token r) (receipt-recovery-authority r)
                         (receipt-originator r) (receipt-recipient r) (receipt-blocked-at r)
                         (receipt-blocked-nonce r) (receipt-blocked-reason r) (receipt-kind r)
                         (receipt-memo r))
          for i from 0
          do (replace out (integer->octets v 32) :start1 (* 32 i)))
    out))

(defun receipt-key (r)
  "Content hash over every receipt field (charges keccak gas)."
  (keccak (encode-receipt r)))

(defun receipt-balance (r)
  (balances +receive-policy-guard-address+ (receipt-key r)))

;;; RecoveryMode, as keywords :originator / :receiver / :third-party.

(defun recovery-mode (r receiver)
  "RecoveryMode::from."
  (let ((authority (receipt-recovery-authority r)))
    (cond ((= authority +recovery-originator+) :originator)
          ((= authority receiver) :receiver)
          (t :third-party))))

(defun recovery-authority (mode r)
  (if (eq mode :originator) (receipt-originator r) (receipt-recovery-authority r)))

(defun policy-subject (mode originator receiver)
  (if (eq mode :originator) originator receiver))

(defun resolve-receipt (octets)
  "resolve_receipt: decode, resolve the recipient (any failure is InvalidClaimAddress) and
classify. Returns (values receipt receiver mode)."
  (let* ((r (decode-receipt octets))
         (receiver (handler-case (funcall 'clempo.address-registry:resolve-recipient (receipt-recipient r))
                     (precompile-condition () (revert 'invalid-claim-address)))))
    (values r receiver (recovery-mode r receiver))))

(defun check-tip20 (token)
  "TIP20Token::from_address."
  (unless (tip20-address-p token) (revert 'invalid-token)))

;;; Operations

(defun balance-of (receipt)
  "ReceivePolicyGuard::balance_of: the unclaimed amount for RECEIPT (octets), or zero."
  (read (receipt-balance (decode-receipt receipt))))

(defun next-receipt-nonce ()
  (let* ((place (nonce +receive-policy-guard-address+))
         (n (max (read place) 1)))
    (when (= n u64-max) (panic +panic-under-overflow+))
    (write place (1+ n))
    n))

(defun store-blocked (token originator receiver recipient recovery-address amount reason kind memo)
  "ReceivePolicyGuard::store_blocked: record a blocked inbound transfer/mint (the caller has
already moved AMOUNT of TOKEN into the guard). RECEIVER is Recipient.target, RECIPIENT the
virtual address or the target; REASON the BlockedReason discriminant (1 TOKEN_FILTER,
2 RECEIVE_POLICY), KIND 0 transfer / 1 mint. Returns (values blocked-nonce blocked-at)."
  (unless (and (member reason '(1 2)) (member kind '(0 1)))
    (revert 'invalid-receipt))
  (let* ((blocked-nonce (next-receipt-nonce))
         (blocked-at (min (block-timestamp) u64-max))
         (r (make-receipt token recovery-address originator recipient blocked-at blocked-nonce
                          reason kind memo)))
    (write (receipt-balance r) amount)
    (emit +receive-policy-guard-address+ 'transfer-blocked token receiver blocked-nonce amount
          (receipt-version r) (encode-receipt r))
    (values blocked-nonce blocked-at)))

(defun claim (sender to receipt)
  "ReceivePolicyGuard::claim: release RECEIPT's funds to TO."
  (when (= to +receive-policy-guard-address+) (revert 'invalid-claim-address))
  (multiple-value-bind (r receiver mode) (resolve-receipt receipt)
    (let ((authority (recovery-authority mode r)))
      (unless (= authority sender) (revert 'unauthorized-claimer))
      (let* ((place (receipt-balance r))
             (amount (read place)))
        (when (zerop amount) (revert 'invalid-receipt))
        (write place 0)
        (check-tip20 (receipt-token r))
        (funcall 'clempo.tip20:release-blocked-funds (receipt-token r) (receipt-originator r) receiver to
                                            amount mode authority)
        (emit +receive-policy-guard-address+ 'receipt-claimed (receipt-token r) receiver
              (receipt-blocked-nonce r) (receipt-blocked-at r) (receipt-version r)
              (receipt-originator r) (receipt-recipient r) (receipt-recovery-authority r)
              sender to amount)
        nil))))

(defun burn-blocked-receipt (sender receipt)
  "ReceivePolicyGuard::burn_blocked_receipt: burn RECEIPT's funds from their effective owner."
  (multiple-value-bind (r receiver mode) (resolve-receipt receipt)
    (let* ((place (receipt-balance r))
           (amount (read place)))
      (when (zerop amount) (revert 'invalid-receipt))
      (check-tip20 (receipt-token r))
      (funcall 'clempo.tip20:burn-blocked (receipt-token r) sender
                                 (policy-subject mode (receipt-originator r) receiver)
                                 amount nil)
      (write place 0)
      (emit +receive-policy-guard-address+ 'receipt-burned (receipt-token r) receiver
            (receipt-blocked-nonce r) (receipt-blocked-at r) (receipt-version r)
            (receipt-originator r) (receipt-recipient r) (receipt-recovery-authority r)
            sender amount)
      nil)))

;;; ABI

(defcall receive-policy-guard balance-of* (receipt)
    (:sol "balanceOf(bytes receipt)" :returns "uint256")
  (balance-of receipt))

(defcall receive-policy-guard claim* (to receipt)
    (:sol "claim(address to, bytes receipt)" :mutates t)
  (claim sender to receipt)
  (values))

(defcall receive-policy-guard burn-blocked-receipt* (receipt)
    (:sol "burnBlockedReceipt(bytes receipt)" :mutates t)
  (burn-blocked-receipt sender receipt)
  (values))

(defdispatch receive-policy-guard (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +receive-policy-guard-address+ 'receive-policy-guard :t6)
