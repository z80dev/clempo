(in-package #:clempo.executor)

;;; Tempo transactions (type 0x76): tempo crates/evm/src/handler.rs prepare_aa / execute_aa /
;;; execute_batch, with intrinsic_gas, nonce_intrinsic_gas, calculate_key_authorization_gas,
;;; apply_nonce, prepare_keychain and apply_key_authorization. T14 configurable accounts are not
;;; on mainnet yet and stop in UNSUPPORTED-TRANSACTION.

(define-constant +key-auth-base-gas+ 27000)
(define-constant +key-auth-per-limit-gas+ 22000)
(define-constant +key-auth-extra-event-buffer+ 1500)
(define-constant +expiring-nonce-gas+ (+ (* 2 2100) 100 (* 3 2900)))

(defun signature-gas (signature)
  "signature_gas.rs tempo_signature_verification_gas."
  (tx:tempo-signature-verification-gas signature))

(defun keychain-signature-p (signature)
  (member (clempo.signatures:signature-kind signature) '(:keychain-v1 :keychain-v2)))

;;; Key authorizations

(defun key-auth-view (v)
  "The parsed KeyAuthorization of V's SignedKeyAuthorization, and its signature."
  (let ((raw (txv-key-authorization v)))
    (when raw
      (values (clempo.signatures:parse-key-authorization raw) (second raw)))))

(defun call-scope-storage-slots (auth fork)
  "handler.rs call_scope_storage_slots."
  (let ((scopes (clempo.signatures:key-authorization-allowed-calls auth)))
    (cond ((eq scopes :absent) 0)
          ((null scopes) 1)
          (t (let ((t4 (forks:fork>= fork :t4)) (selector-sets 0) (selectors 0)
                   (constrained 0) (recipients 0))
               (dolist (scope scopes)
                 (let ((rules (clempo.signatures:key-authorization-call-scope-selector-rules scope)))
                   (when (and t4 rules) (incf selector-sets))
                   (incf selectors (length rules))
                   (dolist (rule rules)
                     (let ((r (clempo.signatures:key-authorization-selector-rule-recipients rule)))
                       (when r (incf constrained) (incf recipients (length r)))))))
               (if t4
                   (+ 1 (* 2 (length scopes)) 1 (* 2 selectors) selector-sets constrained (* 2 recipients))
                   (+ 1 (* 3 (length scopes)) (* 3 selectors) constrained (* 2 recipients))))))))

(defun call-scope-extra-gas (auth)
  "handler.rs call_scope_extra_gas."
  (let ((scopes (clempo.signatures:key-authorization-allowed-calls auth)))
    (if (eq scopes :absent)
        5000
        (+ 5000 (* 7000 (length scopes))
           (* 7000 (clempo.signatures:key-authorization-selector-count auth))
           (* 5000 (clempo.signatures:key-authorization-recipient-count auth))))))

(defun key-authorization-gas (auth signature gp fork)
  "handler.rs calculate_key_authorization_gas (EIP-8037 off: no state gas)."
  ;; A key authorization pays the full verification (PrimitiveSignature::verification_gas): the
  ;; intrinsic gas above the base plus the secp256k1 verification the base already covers.
  (let* ((sig-gas (+ 3000 (signature-gas signature)))
         (limits (clempo.signatures:key-authorization-limits auth))
         (num-limits (if (eq limits :absent) 0 (length limits))))
    (if (forks:fork>= fork :t1b)
        (let* ((sload (+ (evm:gas-params-warm-read gp) (- (evm:gas-params-cold-sload gp) (evm:gas-params-warm-read gp))))
               (limit-slots (if (forks:fork>= fork :t3) (* 2 num-limits) num-limits))
               (sstores (+ 1 limit-slots (if (forks:fork>= fork :t3) (call-scope-storage-slots auth fork) 0)))
               ;; evm2's SstoreSetWithoutLoadCost: clempo.evm's sstore-set less the warm read.
               (sstore (+ (- (evm:gas-params-sstore-set gp) (evm:gas-params-warm-read gp))
                          (if (forks:fork>= fork :t7) forks:+storage-credit-value+ 0)))
               (gas (+ sig-gas sload (* sstore sstores) 2000)))
          (when (clempo.signatures:key-authorization-witness auth)
            (incf gas (+ sload +key-auth-extra-event-buffer+)))
          (when (and (forks:fork>= fork :t6) (clempo.signatures:key-authorization-is-admin auth))
            (incf gas +key-auth-extra-event-buffer+))
          (when (forks:fork>= fork :t4) (incf gas (call-scope-extra-gas auth)))
          gas)
        (+ +key-auth-base-gas+ sig-gas (* num-limits +key-auth-per-limit-gas+)))))

;;; Intrinsic gas

(defun aa-intrinsic-gas (v gp fork)
  "handler.rs intrinsic_gas (pre-T14): (values regular floor)."
  (let ((regular (+ (evm:gas-params-tx-base gp) (signature-gas (txv-signature v))))
        (tokens 0))
    (when (forks:fork>= fork :t14) (fail-tx 'unsupported-transaction *tx-index* "T14 AA intrinsic gas"))
    (incf regular (* (evm:gas-params-cold-account gp) (max 0 (1- (length (txv-calls v))))))
    (dolist (a (txv-authorizations v))
      (incf regular (+ (evm:gas-params-auth-empty-account gp) (signature-gas (fourth a))))
      (when (and (forks:fork>= fork :t1) (zerop (rlp:rlp-decode-integer (third a) :max-bytes 8)))
        (incf regular (evm:gas-params-new-account gp))))
    (multiple-value-bind (auth signature) (key-auth-view v)
      (when auth (incf regular (key-authorization-gas auth signature gp fork))))
    (dolist (call (txv-calls v))
      (unless (zerop (second call)) (invalid "value transfer not allowed in AA transactions"))
      (incf tokens (calldata-tokens (third call)))
      (when (null (first call))
        (incf regular (+ (evm:gas-params-create gp)
                         (* (evm:gas-params-initcode-word gp) (ceiling (length (third call)) 32))))))
    (incf regular (* 4 tokens))
    (multiple-value-bind (accounts keys) (access-list-counts (txv-access-list v))
      (incf regular (+ (* accounts (evm:gas-params-tx-access-address gp))
                       (* keys (evm:gas-params-tx-access-slot gp)))))
    (values regular (floor-gas gp tokens))))

(defun nonce-intrinsic-gas (v gp fork)
  "handler.rs nonce_intrinsic_gas."
  (cond ((and (= (txv-nonce-key v) +expiring-nonce-key+) (forks:fork>= fork :t1)) +expiring-nonce-gas+)
        ((and (forks:fork>= fork :t1) (zerop (txv-nonce v))) (evm:gas-params-new-account gp))
        ((not (zerop (txv-nonce-key v)))
         (if (zerop (txv-nonce v)) (forks:gas-new-nonce-key fork) (forks:gas-existing-nonce-key fork)))
        (t 0)))

;;; Nonces

(defun apply-aa-nonce (v)
  "handler.rs apply_nonce. Returns the protocol nonce (the CREATE nonce)."
  (let* ((world evm:*world*) (fork forks:*spec*)
         (caller (txv-sender v))
         (account (state:get-account world caller))
         (code (state:account-code world account)))
    (when (and (plusp (length code)) (not (state:delegation-target code)))
      (invalid "sender has code"))
    (state:touch-account world account)
    (let ((protocol-nonce (state:account-nonce account)))
      (cond
        ((zerop (txv-nonce-key v))
         (when (and (not (ctx-skip-nonce-check *ctx*)) (/= protocol-nonce (txv-nonce v)))
           (if (< (txv-nonce v) protocol-nonce)
               (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-low :reason "nonce too low")
               (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-high :reason "nonce too high")))
         (when (first (first (txv-calls v)))   ; a CALL first: bump now; CREATE bumps in the batch
           (state:increment-nonce world account)))
        ((and (= (txv-nonce-key v) +expiring-nonce-key+) (forks:fork>= fork :t1))
         (when (and (forks:fork< fork :t12) (/= (txv-nonce v) 0)) (invalid "expiring nonce must be zero"))
         (unless (txv-valid-before v) (invalid "expiring nonce without valid_before"))
         (let ((replay (if (forks:fork>= fork :t1b) (txv-unique-id v) (txv-hash v))))
           (handler-case
               (with-protocol-storage ()
                 (clempo.nonce:check-and-mark-expiring-nonce replay (txv-valid-before v)))
             (contract:precompile-revert (c)
               (error 'invalid-transaction :tx-index *tx-index*
                                           :kind (if (search "REPLAY" (string (contract:precompile-revert-name c)))
                                                     :expiring-nonce-replay :invalid)
                                           :reason (format nil "expiring nonce: ~A" c))))))
        (t
         (handler-case
             (with-protocol-storage ()
               (let ((state (clempo.nonce:get-nonce caller (txv-nonce-key v))))
                 (unless (or (ctx-skip-nonce-check *ctx*) (= state (txv-nonce v)))
                   (if (< (txv-nonce v) state)
                       (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-low :reason "nonce too low")
                       (error 'invalid-transaction :tx-index *tx-index* :kind :nonce-too-high :reason "nonce too high")))
                 (clempo.nonce:increment-nonce caller (txv-nonce-key v))))
           (contract:precompile-revert (c) (invalid "nonce manager: ~A" c)))))
      protocol-nonce)))

;;; Keychain (prepare_keychain, apply_key_authorization)

(defstruct (keychain-state (:conc-name kc-)) access-key same-tx-p fee-key loaded-key)

(defun signature-type-code (kind)
  (ecase kind (:secp256k1 0) (:p256 1) (:webauthn 2)))

(defun check-t6-key-authorization-signer (v kc)
  "prepare_keychain, T6: a key authorization signed by someone other than the root must be
signed by the transaction's own access key, an active admin key of the same signature type."
  (let ((raw (txv-key-authorization v)))
    (when (and raw (forks:spec>= :t6))
      (let ((signer (handler-case (clempo.signatures:key-authorization-signer raw)
                      (clempo.signatures:signature-error () (invalid "key authorization signature recovery failed")))))
        (unless (= signer (txv-sender v))
          (let ((key (kc-loaded-key kc)))
            (unless (and key (eql (kc-access-key kc) signer)
                         (= (clempo.keychain:authorized-key-signature-type key)
                            (signature-type-code (clempo.signatures:signature-kind (second raw))))
                         (clempo.keychain:authorized-key-is-admin key))
              (invalid "key authorization not signed by root"))))))
    kc))

(defun prepare-keychain (v fee)
  (let ((signature (txv-signature v)))
    (if (not (keychain-signature-p signature))
        (make-keychain-state)
        (let* ((user (octets->integer signature :start 1 :end 21))
               (access-key (txv-key-id v))
               (auth (key-auth-view v))
               (same (and auth (eql (clempo.signatures:key-authorization-key-id auth) access-key))))
          (unless (= user (txv-sender v)) (invalid "keychain user address mismatch"))
          (if same
              (let* ((limits (clempo.signatures:key-authorization-limits auth))
                     (fee-key (when (and (plusp (fee-collected fee)) (= (fee-payer fee) (txv-sender v))
                                         (not (eq limits :absent)))
                                (let ((limit (find (fee-token fee) (reverse limits)
                                                   :key #'clempo.signatures:key-authorization-limit-token)))
                                  (when (> (fee-collected fee)
                                           (if limit (clempo.signatures:key-authorization-limit-amount limit) 0))
                                    (invalid "SpendingLimitExceeded")))
                                access-key)))
                (check-t6-key-authorization-signer
                 v (make-keychain-state :access-key access-key :same-tx-p t :fee-key fee-key)))
              (let* ((expected-type (when (or auth (forks:spec>= :t1))
                                      (ecase (clempo.signatures:signature-inner-kind signature)
                                        (:secp256k1 0) (:p256 1) (:webauthn 2))))
                     (key (handler-case
                              (with-protocol-storage ()
                                (prog1 (clempo.keychain:validate-keychain-authorization
                                        user access-key (block-env-timestamp (ctx-env *ctx*)) expected-type)
                                  (clempo.keychain:set-transaction-key access-key)))
                            (contract:precompile-condition (c) (invalid "keychain validation failed: ~A" c)))))
                (when (and auth (not (clempo.keychain:authorized-key-is-admin key)))
                  (invalid "access key cannot authorize other keys"))
                (setf (ctx-key-expiry *ctx*) (clempo.keychain:authorized-key-expiry key))
                (check-t6-key-authorization-signer
                 v (make-keychain-state :access-key access-key :loaded-key key
                                        :fee-key (when (clempo.keychain:authorized-key-enforce-limits key)
                                                   access-key)))))))))

(defun authorize-inline-key (v auth)
  "AccountKeychain::authorize_key / authorize_admin_key for the transaction's inline key."
  (if (clempo.signatures:key-authorization-is-admin auth)
      (clempo.keychain:authorize-admin-key
       (txv-sender v) (clempo.signatures:key-authorization-key-id auth)
       (clempo.signatures:key-authorization-key-type auth)
       (clempo.signatures:key-authorization-witness auth))
      (let* ((limits (clempo.signatures:key-authorization-limits auth))
             (calls (clempo.signatures:key-authorization-allowed-calls auth))
             (expiry (clempo.signatures:key-authorization-expiry auth)))
        (clempo.keychain:authorize-key
         (txv-sender v) (clempo.signatures:key-authorization-key-id auth)
         (clempo.signatures:key-authorization-key-type auth)
         (or expiry +max-u64+)
         (not (eq limits :absent))
         (if (eq limits :absent) '()
             (mapcar (lambda (l) (list (clempo.signatures:key-authorization-limit-token l)
                                       (clempo.signatures:key-authorization-limit-amount l)
                                       (clempo.signatures:key-authorization-limit-period l)))
                     limits))
         (eq calls :absent)
         (if (eq calls :absent) '()
             (mapcar (lambda (s)
                       (list (clempo.signatures:key-authorization-call-scope-target s)
                             (mapcar (lambda (r)
                                       (list (clempo.signatures:key-authorization-selector-rule-selector r)
                                             (clempo.signatures:key-authorization-selector-rule-recipients r)))
                                     (clempo.signatures:key-authorization-call-scope-selector-rules s))))
                     calls))
         (clempo.signatures:key-authorization-witness auth)))))

(defun apply-key-authorization (v fee kc remaining)
  "handler.rs apply_key_authorization: register the inline key. On T1/T1A it is metered on
the transaction's REMAINING gas with only the SSTORE-set and warm-read costs; elsewhere it
runs unmetered. Returns the gas it used, or :OUT-OF-GAS (its writes rolled back)."
  (multiple-value-bind (auth) (key-auth-view v)
    (if (null auth)
        0
        (let* ((metered (and (forks:spec>= :t1) (forks:spec< :t1b)))
               (cp (state:checkpoint evm:*world*))
               (storage (contract:make-metered-storage *host* :gas-limit (if metered remaining +max-u64+)
                                                              :tip1060 nil))
               (used
                 (handler-case
                     (let ((contract:*storage* storage))
                       (if metered
                           (forks:with-gas-parameters-only (:sstore-set-without-load-cost :warm-storage-read-cost)
                             (authorize-inline-key v auth))
                           (authorize-inline-key v auth))
                       (if metered (contract:metered-storage-spent storage) 0))
                   (contract:precompile-halt (c)
                     (if (and metered (eq (contract:precompile-halt-reason c) :out-of-gas))
                         (progn (state:rollback evm:*world* cp) :out-of-gas)
                         (invalid "keychain precompile: ~A" c)))
                   (contract:precompile-condition (c) (invalid "keychain precompile: ~A" c)))))
          (when (and (clempo.signatures:key-authorization-is-admin auth) (forks:spec< :t6))
            (invalid "T6 key authorization fields are not active before T6"))
          (setf (ctx-key-expiry *ctx*) (clempo.signatures:key-authorization-expiry auth))
          (when (kc-same-tx-p kc)
            (handler-case
                (with-protocol-storage ()
                  (clempo.keychain:set-transaction-key (clempo.signatures:key-authorization-key-id auth))
                  (unless (zerop (fee-collected fee))
                    (clempo.keychain:authorize-transfer (fee-payer fee) (fee-token fee) (fee-collected fee))))
              (contract:precompile-condition (c) (invalid "fee authorization: ~A" c))))
          used))))

;;; The batch

(defun execute-batch (v caller access-key create-nonce price gas)
  "handler.rs execute_batch: all calls or none. Returns (values success-p gas-left)."
  (let* ((world evm:*world*)
         (cp (state:checkpoint world))
         (remaining gas))
    (when (and (forks:spec>= :t3) access-key)
      (let ((storage (contract:make-metered-storage *host* :gas-limit remaining :tip1060 t
                                                           :non-creditable *non-creditable*)))
        (handler-case
            (let ((contract:*storage* storage))
              (dolist (call (txv-calls v))
                (clempo.keychain:validate-call-scope caller access-key (first call) (third call)))
              (setf remaining (- remaining (contract:metered-storage-spent storage))))
          (contract:precompile-revert ()
            (state:rollback world cp)
            (return-from execute-batch (values nil (- remaining (contract:metered-storage-spent storage)))))
          (contract:precompile-halt ()
            (state:rollback world cp)
            (return-from execute-batch (values nil 0))))))
    (let ((evm:*tx-env* (evm:make-tx-env :origin caller :gas-price price)))
      (loop for (to value input) in (txv-calls v)
            do (when to (state:warm-address world to))
               (multiple-value-bind (status gas-left)
                   (if (and (null to)
                            (let ((a (state:get-account world caller)))
                              (if (= (state:account-nonce a) +max-u64+)
                                  t
                                  (progn (state:increment-nonce world a) nil))))
                       (values :return remaining)   ; exhausted creator nonce: succeeds, runs nothing
                       (run-initial-frame caller create-nonce to input value remaining))
                 (unless (evm:success-p status)
                   (state:rollback world cp)
                   (when (and (zerop (txv-nonce-key v)) (null (first (first (txv-calls v)))))
                     (state:increment-nonce world (state:get-account world caller)))
                   (return-from execute-batch (values nil gas-left)))
                 (setf remaining gas-left))))
    (values t remaining)))


(defun execute-aa-transaction (ctx v)
  "prepare_aa + execute_aa."
  (let* ((world evm:*world*) (gp (evm:spec-gas evm:*spec*)) (env (ctx-env ctx)) (fork forks:*spec*)
         (caller (txv-sender v))
         (gas-limit (txv-gas-limit v))
         (base-fee (block-env-base-fee env))
         (price (txv-effective-gas-price v base-fee))
         (timestamp (block-env-timestamp env))
         (*ctx* ctx))
    (setf (ctx-key-expiry ctx) nil)
    (when (forks:fork>= fork :t14) (fail-tx 'unsupported-transaction *tx-index* "T14 AA transactions"))
    (when (and (txv-authorizations v) (null (first (first (txv-calls v)))))
      (invalid "calls cannot contain CREATE when the authorization list is non-empty"))
    (let ((kind (clempo.signatures:signature-kind (txv-signature v))))
      (when (or (and (forks:fork>= fork :t1c) (eq kind :keychain-v1))
                (and (forks:fork< fork :t1c) (eq kind :keychain-v2)))
        (invalid "keychain signature version")))
    ;; handler.rs validate_key_authorization (stateless grant rules)
    (when (txv-key-authorization v)
      (let ((verdict (tx:validate-key-authorization (txv-key-authorization v) caller
                                                    (txv-key-id v)
                                                    (block-env-chain-id (ctx-env ctx)) fork
                                                    :transaction-signature (txv-signature v))))
        (unless (eq verdict t) (invalid "key authorization: ~(~A~)" verdict))))
    (when (and (forks:fork>= fork :t3) (keychain-signature-p (txv-signature v)) (null (first (first (txv-calls v)))))
      (invalid "access-key transactions cannot use CREATE as the first call"))
    (when (some (lambda (c) (/= 0 (second c))) (txv-calls v)) (invalid "value transfer not allowed in AA"))
    ;; Only a sponsored transaction has a separate fee payer to recover.
    (let ((fee-payer (txv-fee-payer v)))
      (when (and (forks:fork>= fork :t2) (txv-sponsored-p v) (= fee-payer caller))
        (invalid "self-sponsored fee payer"))
      (when (> (txv-max-priority v) (txv-max-fee v)) (invalid "priority fee greater than max fee"))
      (when (and (not (ctx-skip-base-fee-check ctx)) (< price base-fee)) (invalid "fee cap below base fee"))
      (let ((chain-id (txv-chain-id v)))  ; a request may leave it out
        (when (and chain-id (/= chain-id (block-env-chain-id env))) (invalid "wrong chain id")))
      (when (> gas-limit (evm:gas-params-tx-gas-cap gp)) (invalid "gas limit above the transaction cap"))
      (when (> gas-limit (block-env-gas-limit env)) (invalid "gas limit above the block's"))
      (unless (and (forks:fork>= fork :t1) (= (txv-nonce-key v) +expiring-nonce-key+))
        (when (= (txv-nonce v) +max-u64+) (invalid "nonce overflow")))
      (dolist (c (txv-calls v))
        (when (and (null (first c)) (> (length (third c)) (evm:gas-params-max-initcode-size gp)))
          (invalid "initcode too large")))
      (let ((after (and (not (ctx-skip-valid-after-check ctx)) (txv-valid-after v)))
            (before (txv-valid-before v)))
        (when (and after (< timestamp after)) (invalid "not valid before ~D" after))
        (when (and before (>= timestamp before)) (invalid "expired at ~D" before)))
      (multiple-value-bind (intrinsic floor) (aa-intrinsic-gas v gp fork)
        (let ((nonce-gas (nonce-intrinsic-gas v gp fork)))
          (when (forks:fork>= fork :t0) (incf intrinsic nonce-gas))
          (when (< gas-limit intrinsic) (invalid "intrinsic gas too low"))
          (when (< gas-limit floor) (invalid "gas limit below the calldata floor"))
          (unless (forks:fork>= fork :t0) (incf intrinsic nonce-gas))
          (when (and (/= 0 (txv-nonce-key v)) (null (first (first (txv-calls v))))
                     (multiple-value-bind (found nonce) ; account_info_untracked: the committed state
                         (state:backend-account (state:world-backend world) caller)
                       (or (not found) (zerop nonce))))
            (incf intrinsic (evm:gas-params-new-account gp))
            (when (< gas-limit intrinsic) (invalid "intrinsic gas too low")))
          ;; warm_aa_accounts, warm_beneficiary
          (state:warm-address world caller)
          (let ((first-to (first (first (txv-calls v))))) (when first-to (state:warm-address world first-to)))
          (dolist (a (evm:precompile-addresses (evm:spec-precompiles evm:*spec*))) (state:warm-address world a))
          (dolist (entry (txv-access-list v))
            (state:warm-address world (first entry))
            (dolist (slot (rest entry)) (state:warm-slot world (first entry) slot)))
          (state:warm-address world (block-env-beneficiary env))
          (let* ((fee (resolve-fee-context ctx v fee-payer))
                 (create-nonce (apply-aa-nonce v))
                 (kc (prepare-keychain v fee)))
            (collect-fee ctx fee (when (= (fee-payer fee) caller) (kc-fee-key kc)))
            (let* ((remaining (max 0 (- gas-limit intrinsic)))
                   ;; Validation-only runs stop after the inline grant has passed the keychain's
                   ;; own checks (duplicate limits, ...); the check rolls it back.
                   (key-gas (prog1 (apply-key-authorization v fee kc remaining)
                              (when (ctx-validate-only ctx)
                                (return-from execute-aa-transaction (validated-outcome fee)))))
                   (auth-refund (if (txv-authorizations v)
                                    (apply-tempo-authorizations (txv-authorizations v)
                                                                (block-env-chain-id env) fork)
                                    0)))
              (if (or (eq key-gas :out-of-gas) (> key-gas remaining))
                  ;; The metered key authorization ran out of gas: the transaction is included
                  ;; as an out-of-gas halt that spends all its execution gas and runs no call.
                  (multiple-value-bind (used validator-fee) (settle ctx v fee price 0 0 floor :status nil)
                    (make-tx-outcome nil used used (tx-logs)
                                     :fee-token (fee-token fee) :fallback-fee-token-p (fee-fallback-p fee)
                                     :fee-payer (fee-payer fee) :key-expiry (ctx-key-expiry ctx)
                                     :validator-fee validator-fee))
                  (let ((execution-gas (progn (incf intrinsic key-gas)
                                              (if (and (forks:fork< fork :t0) (> intrinsic gas-limit))
                                                  +max-u64+ (- gas-limit intrinsic)))))
                    (multiple-value-bind (ok gas-left)
                        (execute-batch v caller (kc-access-key kc) create-nonce price execution-gas)
                      (multiple-value-bind (used validator-fee)
                          (settle ctx v fee price (min gas-left gas-limit) auth-refund floor :status ok)
                        (make-tx-outcome ok used used (tx-logs)
                                         :fee-token (fee-token fee) :fallback-fee-token-p (fee-fallback-p fee)
                                         :fee-payer (fee-payer fee) :key-expiry (ctx-key-expiry ctx)
                                         :validator-fee validator-fee))))))))))))
