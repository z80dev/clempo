(in-package #:clempo.nonce)

;;; NonceManager (crates/precompiles/src/nonce): 2D nonces (keys 1..N; key 0 is the
;;; account nonce) and the TIP-1009 expiring-nonce replay set, a ring buffer of hashes.

(defcontract nonce-manager (:address +nonce-address+)
  (nonces (mapping address (mapping u256 u64)))
  (expiring-nonce-seen (mapping b256 u64))
  (expiring-nonce-ring (mapping u32 b256))
  (expiring-nonce-ring-ptr u32))

(deferror protocol-nonce-not-supported "ProtocolNonceNotSupported()")
(deferror invalid-nonce-key "InvalidNonceKey()")
(deferror nonce-overflow "NonceOverflow()")
(deferror expiring-nonce-replay "ExpiringNonceReplay()")
(deferror expiring-nonce-set-full "ExpiringNonceSetFull()")
(deferror invalid-expiring-nonce-expiry "InvalidExpiringNonceExpiry()")

(defevent nonce-incremented "NonceIncremented(address indexed account, uint256 indexed nonceKey, uint64 newNonce)")

(defun get-nonce (account nonce-key)
  "NonceManager::get_nonce: the nonce of ACCOUNT's 2D key NONCE-KEY (key 0 is refused)."
  (when (zerop nonce-key) (revert 'protocol-nonce-not-supported))
  (read (nonces +nonce-address+ account nonce-key)))

(defun increment-nonce (account nonce-key)
  "NonceManager::increment_nonce: bump ACCOUNT's 2D nonce and log it. Returns the new nonce."
  (when (zerop nonce-key) (revert 'invalid-nonce-key))
  (let* ((place (nonces +nonce-address+ account nonce-key))
         (current (read place)))
    (when (= current u64-max) (revert 'nonce-overflow))
    (write place (1+ current))
    (emit +nonce-address+ 'nonce-incremented account nonce-key (1+ current))
    (1+ current)))

(defun expiring-nonce-seen-p (hash now)
  (let ((expiry (read (expiring-nonce-seen +nonce-address+ hash))))
    (and (/= expiry 0) (> expiry now))))

(defun check-and-mark-expiring-nonce (hash valid-before)
  "NonceManager::check_and_mark_expiring_nonce: validate VALID-BEFORE against the fork's
window, reject a live replay, and record HASH in the ring, evicting an expired entry."
  (let* ((now (min (block-timestamp) u64-max))
         (max-expiry (expiring-nonce-max-expiry-secs *spec*))
         (capacity (expiring-nonce-set-capacity *spec*)))
    (when (or (<= valid-before now) (> valid-before (min u64-max (+ now max-expiry))))
      (revert 'invalid-expiring-nonce-expiry))
    (let ((seen (read (expiring-nonce-seen +nonce-address+ hash))))
      (when (and (/= seen 0) (> seen now)) (revert 'expiring-nonce-replay)))
    (let* ((ptr (read (expiring-nonce-ring-ptr +nonce-address+)))
           (old (read (expiring-nonce-ring +nonce-address+ ptr))))
      (unless (zerop old)
        (let ((old-expiry (read (expiring-nonce-seen +nonce-address+ old))))
          (when (and (/= old-expiry 0) (> old-expiry now)) (revert 'expiring-nonce-set-full))
          (write (expiring-nonce-seen +nonce-address+ old) 0)))
      (write (expiring-nonce-ring +nonce-address+ ptr) hash)
      (write (expiring-nonce-seen +nonce-address+ hash) valid-before)
      (write (expiring-nonce-ring-ptr +nonce-address+) (if (>= (1+ ptr) capacity) 0 (1+ ptr))))
    nil))

(defcall nonce-manager get-nonce* (account nonce-key)
    (:sol "getNonce(address account, uint256 nonceKey)" :returns "uint64")
  (get-nonce account nonce-key))

(defdispatch nonce-manager (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +nonce-address+ 'nonce-manager :genesis)
