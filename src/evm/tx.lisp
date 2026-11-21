(in-package #:clempo.evm)

;;; Transaction-level processing: validation, intrinsic gas, gas purchase, access-list
;;; warming, EIP-7702 authorizations, the top-level message, refunds and fee payment.
;;; APPLY-TRANSACTION is a generic function of a handler so the tempo layer can extend or
;;; replace steps; ETHEREUM-HANDLER composes the exported building blocks below.

(defstruct (authorization (:constructor make-authorization
                              (&key chain-id address nonce y-parity r s signer)))
  (chain-id 0 :type integer)
  (address 0 :type integer)
  (nonce 0 :type integer)
  (y-parity 0 :type integer)
  (r 0 :type integer)
  (s 0 :type integer)
  (signer nil))

(defstruct (evm-tx (:constructor make-evm-tx
                       (&key (type 0) (nonce 0) (gas-limit 0) (gas-price 0) max-fee
                          max-priority-fee to (value 0) (data +empty-code+) (access-list '())
                          (blob-hashes '()) (max-fee-per-blob-gas 0) (authorizations '())
                          sender)))
  "A transaction as the EVM sees it. TO NIL is a creation. ACCESS-LIST is a list of
\(address slot...). MAX-FEE / MAX-PRIORITY-FEE are NIL for legacy and access-list types."
  (type 0 :type fixnum)
  (nonce 0 :type integer)
  (gas-limit 0 :type integer)
  (gas-price 0 :type integer)
  (max-fee nil)
  (max-priority-fee nil)
  (to nil)
  (value 0 :type integer)
  (data +empty-code+ :type octets)
  (access-list '() :type list)
  (blob-hashes '() :type list)
  (max-fee-per-blob-gas 0 :type integer)
  (authorizations '() :type list)
  (sender 0 :type integer))

(defstruct (tx-result (:constructor make-tx-result (status gas-used logs output contract-address)))
  status gas-used logs output contract-address)

(define-condition invalid-transaction (error)
  ((reason :initarg :reason :reader invalid-transaction-reason))
  (:report (lambda (c s) (format s "invalid transaction: ~A" (invalid-transaction-reason c)))))

(defun reject-tx (reason)
  (error 'invalid-transaction :reason reason))

(define-constant +gas-per-blob+ (ash 1 17))
(define-constant +secp256k1-n+
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(defun fake-exponential (factor numerator denominator)
  "EIP-4844 integer approximation of FACTOR * e^(NUMERATOR/DENOMINATOR)."
  (let ((i 1) (output 0) (acc (* factor denominator)))
    (loop while (plusp acc)
          do (incf output acc)
             (setf acc (floor (* acc numerator) (* denominator i)))
             (incf i))
    (floor output denominator)))

(defun blob-base-fee (excess-blob-gas update-fraction)
  (fake-exponential 1 excess-blob-gas update-fraction))

;;; Building blocks

(defun calldata-tokens (data)
  "EIP-7623 tokens: zero bytes count 1, others 4."
  (declare (type octets data))
  (let ((zeros (count 0 data)))
    (+ zeros (* 4 (- (length data) zeros)))))

(defun intrinsic-gas (tx)
  "(values intrinsic-gas floor-gas) of TX under *SPEC* (EIP-2028, 2930, 3860, 7623, 7702)."
  (let* ((gp (spec-gas *spec*))
         (data (evm-tx-data tx))
         (zeros (count 0 data))
         (nonzeros (- (length data) zeros))
         (create (null (evm-tx-to tx)))
         (gas (+ (gas-params-tx-base gp)
                 (* zeros (gas-params-tx-data-zero gp))
                 (* nonzeros (gas-params-tx-data-nonzero gp)))))
    (when create
      (incf gas (+ (gas-params-tx-create gp)
                   (* (gas-params-initcode-word gp) (words (length data))))))
    (dolist (entry (evm-tx-access-list tx))
      (incf gas (+ (gas-params-tx-access-address gp)
                   (* (gas-params-tx-access-slot gp) (length (rest entry))))))
    (incf gas (* (gas-params-auth-empty-account gp) (length (evm-tx-authorizations tx))))
    (values gas
            (if (plusp (gas-params-tx-floor-token gp))
                (+ (gas-params-tx-base gp)
                   (* (gas-params-tx-floor-token gp) (calldata-tokens data)))
                0))))

(defun effective-gas-price (tx base-fee)
  (if (evm-tx-max-fee tx)
      (min (evm-tx-max-fee tx) (+ base-fee (evm-tx-max-priority-fee tx)))
      (evm-tx-gas-price tx)))

(defun validate-tx (world tx)
  "Signal INVALID-TRANSACTION unless TX may be included under *SPEC* and *BLOCK*."
  (let* ((spec *spec*)
         (gp (spec-gas spec))
         (block *block*)
         (type (evm-tx-type tx))
         (sender (get-account world (evm-tx-sender tx)))
         (gas-limit (evm-tx-gas-limit tx))
         (base-fee (if (fork>= :london) (block-env-base-fee block) 0))
         (blobs (length (evm-tx-blob-hashes tx))))
    (case type
      (0)
      (1 (unless (fork>= :berlin) (reject-tx :type-1-pre-fork)))
      (2 (unless (fork>= :london) (reject-tx :type-2-pre-fork)))
      (3 (unless (fork>= :cancun) (reject-tx :type-3-pre-fork)))
      (4 (unless (fork>= :prague) (reject-tx :type-4-pre-fork)))
      (t (reject-tx :unknown-type)))
    (multiple-value-bind (intrinsic floor) (intrinsic-gas tx)
      (when (< gas-limit intrinsic) (reject-tx :intrinsic-gas-too-low))
      (when (and (fork>= :prague) (< gas-limit floor)) (reject-tx :intrinsic-gas-below-floor)))
    (when (and (null (evm-tx-to tx)) (fork>= :shanghai)
               (> (length (evm-tx-data tx)) (gas-params-max-initcode-size gp)))
      (reject-tx :initcode-size-exceeded))
    (when (>= (evm-tx-nonce tx) +max-nonce+) (reject-tx :nonce-is-max))
    (unless (= (evm-tx-nonce tx) (account-nonce sender))
      (reject-tx (if (< (evm-tx-nonce tx) (account-nonce sender)) :nonce-too-low :nonce-too-high)))
    (when (> gas-limit (block-env-gas-limit block)) (reject-tx :gas-allowance-exceeded))
    (let ((cap (gas-params-tx-gas-cap gp)))
      (when (and (plusp cap) (> gas-limit cap)) (reject-tx :gas-limit-exceeds-maximum)))
    (let ((max-fee (or (evm-tx-max-fee tx) (evm-tx-gas-price tx))))
      (when (evm-tx-max-fee tx)
        (when (> (evm-tx-max-priority-fee tx) max-fee) (reject-tx :priority-greater-than-max-fee)))
      (when (and (fork>= :london) (< max-fee base-fee)) (reject-tx :insufficient-max-fee-per-gas))
      (when (= type 3)
        (when (null (evm-tx-to tx)) (reject-tx :type-3-contract-creation))
        (when (zerop blobs) (reject-tx :type-3-zero-blobs))
        (when (> blobs (if (fork>= :osaka) (min 6 (spec-blob-max spec)) (spec-blob-max spec)))
          (reject-tx :type-3-blob-count-exceeded))
        (dolist (h (evm-tx-blob-hashes tx))
          (unless (= (ldb (byte 8 248) h) 1) (reject-tx :type-3-invalid-blob-versioned-hash)))
        (when (< (evm-tx-max-fee-per-blob-gas tx) (block-env-blob-base-fee block))
          (reject-tx :insufficient-max-fee-per-blob-gas)))
      (when (= type 4)
        (when (null (evm-tx-to tx)) (reject-tx :type-4-contract-creation))
        (when (null (evm-tx-authorizations tx)) (reject-tx :type-4-empty-authorization-list)))
      (let ((cost (+ (* gas-limit max-fee) (evm-tx-value tx)
                     (* blobs +gas-per-blob+ (evm-tx-max-fee-per-blob-gas tx)))))
        (when (< (account-balance sender) cost) (reject-tx :insufficient-account-funds))))
    (let ((code (account-code world sender)))
      (unless (or (zerop (length code)) (and (fork>= :prague) (delegation-target code)))
        (reject-tx :sender-not-eoa)))
    t))

(defun buy-gas (world tx)
  "Bump the sender's nonce and take the gas (and blob gas) fee up front."
  (let* ((block *block*)
         (sender (get-account world (evm-tx-sender tx)))
         (price (effective-gas-price tx (if (fork>= :london) (block-env-base-fee block) 0)))
         (blob-fee (* (length (evm-tx-blob-hashes tx)) +gas-per-blob+ (block-env-blob-base-fee block))))
    (increment-nonce world sender)
    (sub-balance world sender (+ (* (evm-tx-gas-limit tx) price) blob-fee))
    price))

(defun warm-tx-access (world tx)
  "EIP-2929/2930/3651: sender, recipient, precompiles, coinbase and the access list."
  (when (fork>= :berlin)
    (warm-address world (evm-tx-sender tx))
    (when (evm-tx-to tx) (warm-address world (evm-tx-to tx)))
    (dolist (a (precompile-addresses (spec-precompiles *spec*))) (warm-address world a))
    (when (fork>= :shanghai) (warm-address world (block-env-coinbase *block*)))
    (dolist (entry (evm-tx-access-list tx))
      (warm-address world (first entry))
      (dolist (slot (rest entry)) (warm-slot world (first entry) slot)))))

(defun authorization-hash (auth)
  "keccak256(0x05 || rlp([chain_id, address, nonce])), the EIP-7702 signing hash."
  (keccak256 (octets-concat (octets-of 5)
                            (rlp-encode (list (authorization-chain-id auth)
                                              (integer->octets (authorization-address auth) 20)
                                              (authorization-nonce auth))))))

(defun authority-of (auth)
  "The authority that signed AUTH, or NIL for an invalid signature. A SIGNER already
recovered by the caller is used as is."
  (let ((r (authorization-r auth)) (s (authorization-s auth)))
    (when (and (< 0 r +secp256k1-n+)
               (< 0 s)
               (<= s (floor +secp256k1-n+ 2))
               (<= 0 (authorization-y-parity auth) 1))
      (or (authorization-signer auth)
          (let ((public (clempo.secp256k1:ecrecover (authorization-hash auth) r s
                                                    (authorization-y-parity auth))))
            (and public (clempo.secp256k1:pubkey->address public)))))))

(defun apply-authorizations (world tx)
  "EIP-7702: install the delegations of TX's authorization list."
  (let ((gp (spec-gas *spec*))
        (chain-id (block-env-chain-id *block*)))
    (dolist (auth (evm-tx-authorizations tx))
      (block next
        (unless (member (authorization-chain-id auth) (list 0 chain-id)) (return-from next))
        (when (>= (authorization-nonce auth) +max-nonce+) (return-from next))
        (let ((authority (authority-of auth)))
          (unless authority (return-from next))
          (warm-address world authority)
          (let* ((account (get-account world authority))
                 (code (account-code world account)))
            (unless (or (zerop (length code)) (delegation-target code)) (return-from next))
            (unless (= (account-nonce account) (authorization-nonce auth)) (return-from next))
            (when (account-exists account)
              (add-refund world (gas-params-auth-refund gp)))
            (set-code world account (if (zerop (authorization-address auth))
                                        +empty-code+
                                        (delegation-designator (authorization-address auth))))
            (increment-nonce world account)))))))

(defun execute-tx-message (world tx gas)
  "Run TX's top-level call or creation with GAS. Returns (values status gas-left output
contract-address)."
  (let ((sender (evm-tx-sender tx))
        (to (evm-tx-to tx))
        (value (evm-tx-value tx)))
    (if (null to)
        (let ((address (create-address sender (evm-tx-nonce tx))))
          (when (fork>= :berlin) (warm-address world address))
          (if (collision-p world (get-account world address))
              (values :create-collision 0 +empty-code+ nil)
              (multiple-value-bind (status gas-left output)
                  (create-message sender address value (evm-tx-data tx) gas 0)
                (values status gas-left output (and (success-p status) address)))))
        (let* ((code (get-code world to))
               (target (and (fork>= :prague) (delegation-target code))))
          (when target
            (warm-address world target)
            (setf code (get-code world target)))
          (call-message :call sender to (or target to) value (evm-tx-data tx) gas 0 nil
                        :code code :disable-precompiles (and target t))))))

(defun settle-tx (world tx gas-left price floor)
  "Refund unused gas to the sender and pay the coinbase. Returns the gas used."
  (let* ((gp (spec-gas *spec*))
         (block *block*)
         (gas-limit (evm-tx-gas-limit tx))
         (used (- gas-limit gas-left))
         (refund (min (max 0 (world-refund world)) (floor used (gas-params-refund-quotient gp))))
         (used (max (- used refund) floor))
         (base-fee (if (fork>= :london) (block-env-base-fee block) 0)))
    (add-balance world (evm-tx-sender tx) (* (- gas-limit used) price))
    (add-balance world (block-env-coinbase block) (* used (- price base-fee)))
    used))

;;; The handler

(defclass ethereum-handler () ()
  (:documentation "Ethereum's transaction rules; subclass and specialise to change steps."))

(defgeneric apply-transaction (handler world tx)
  (:documentation "Validate and execute TX against WORLD under *SPEC* and *BLOCK*. Returns a
TX-RESULT and leaves the transaction finalized in WORLD, or signals INVALID-TRANSACTION
with WORLD unchanged."))

(defmethod apply-transaction ((handler ethereum-handler) world tx)
  (handler-bind ((invalid-transaction (lambda (c) (declare (ignore c)) (discard-tx world))))
    (validate-tx world tx))
  (let* ((*world* world)
         (*tx-env* (make-tx-env :origin (evm-tx-sender tx)
                                :gas-price (effective-gas-price
                                            tx (if (fork>= :london) (block-env-base-fee *block*) 0))
                                :blob-hashes (coerce (evm-tx-blob-hashes tx) 'simple-vector)))
         (tracer *tracer*)
         (price (progn (when tracer (trace-tx-start tracer world tx))
                       (buy-gas world tx))))
    (multiple-value-bind (intrinsic floor) (intrinsic-gas tx)
      (warm-tx-access world tx)
      (apply-authorizations world tx)
      (multiple-value-bind (status gas-left output address)
          (execute-tx-message world tx (- (evm-tx-gas-limit tx) intrinsic))
        (let* ((gas-used (settle-tx world tx gas-left price floor))
               (logs (if (success-p status) (logs-since world) '()))
               (result (make-tx-result status gas-used logs output address)))
          (finalize-tx world)
          (when tracer (trace-tx-end tracer world tx result))
          result)))))
