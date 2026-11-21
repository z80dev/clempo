(in-package #:clempo.fee-manager)

;;; The fee AMM half of TipFeeManager (crates/precompiles/src/tip_fee_manager/amm.rs).
;;; Storage (fee-manager contract, POOL struct), errors and events are declared in
;;; fee-manager.lisp, which loads after this file: only run-time references here.

(defconstant +fee-m+ 9970 "Fee swap multiplier M (30 bps fee), scaled by +FEE-SCALE+.")
(defconstant +rebalance-n+ 9985 "Rebalance swap multiplier N, scaled by +FEE-SCALE+.")
(defconstant +fee-scale+ 10000)
(defconstant +min-liquidity+ 1000 "LP tokens locked forever when a pool is initialized.")

(defconstant +u256-max+ (1- (ash 1 256)))

(defun u256-mul (a b)
  "U256::checked_mul: the product or NIL on overflow."
  (let ((p (* a b))) (and (<= p +u256-max+) p)))

(defun to-u128 (value &optional error)
  "U256 -> u128; arithmetic conversions panic, AMM amount conversions name their error."
  (cond ((<= value u128-max) value)
        (error (revert error))
        (t (panic +panic-under-overflow+))))

(defun compute-amount-out (amount-in)
  "compute_amount_out: AMOUNT-IN * M / SCALE, Panic(0x11) on overflow."
  (let ((p (u256-mul amount-in +fee-m+)))
    (unless p (panic +panic-under-overflow+))
    (floor p +fee-scale+)))


;;; Pools

(defun pool-id (user-token validator-token)
  "PoolKey::get_id: keccak256(abi.encode(userToken, validatorToken)) as an integer (no gas)."
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below 20
          do (setf (aref buf (- 31 i)) (ldb (byte 8 (* 8 i)) user-token)
                   (aref buf (- 63 i)) (ldb (byte 8 (* 8 i)) validator-token)))
    (clempo.keccak:keccak256-integer buf)))

(defun get-pool (user-token validator-token)
  "TipFeeManager::get_pool: the POOL of the directional pair."
  (read (pools +fee-manager-address+ (pool-id user-token validator-token))))

(defun pool-enough-reserve-p (pool amount)
  "Pool::has_enough_reserve_validator_token: empty pools are refused even for zero output."
  (let ((reserve (pool-reserve-validator-token pool)))
    (and (/= reserve 0) (>= reserve amount))))

(defun reserve-pool-liquidity (pool-id amount)
  "TipFeeManager::reserve_pool_liquidity: record a pending fee swap's need in transient storage."
  (t-write (pending-fee-swap-reservation +fee-manager-address+ pool-id) amount))

;;; LP supply and balances

(defun get-total-supply (pool-id)
  (read (total-supply +fee-manager-address+ pool-id)))

(defun set-total-supply (pool-id value)
  (write (total-supply +fee-manager-address+ pool-id) value))

(defun get-liquidity-balances (pool-id user)
  (read (liquidity-balances +fee-manager-address+ pool-id user)))

(defun set-liquidity-balances (pool-id user value)
  (write (liquidity-balances +fee-manager-address+ pool-id user) value))

;;; Operations

(defun rebalance-swap (msg-sender user-token validator-token amount-out to)
  "TipFeeManager::rebalance_swap: buy AMOUNT-OUT of USER-TOKEN from the pool for
validator token at rate N/SCALE (plus one). Returns the validator-token amount paid."
  (when (zerop amount-out) (revert 'invalid-amount))
  (let* ((pool-id (pool-id user-token validator-token))
         (pool (read (pools +fee-manager-address+ pool-id)))
         (amount-in (let ((p (u256-mul amount-out +rebalance-n+)))
                      (unless p (panic +panic-under-overflow+))
                      (checked+ (floor p +fee-scale+) 1)))
         (amount-in (to-u128 amount-in 'invalid-amount))
         (amount-out (to-u128 amount-out 'invalid-amount))
         (new-validator (+ (pool-reserve-validator-token pool) amount-in))
         (new-user (- (pool-reserve-user-token pool) amount-out)))
    (when (> new-validator u128-max) (revert 'insufficient-reserves))
    (when (minusp new-user) (revert 'invalid-amount))
    (fork-if (:since :t1c)
        (when (< new-validator
                 (t-read (pending-fee-swap-reservation +fee-manager-address+ pool-id)))
          (revert 'insufficient-liquidity))
        nil)
    (write (pools +fee-manager-address+ pool-id)
           (make-pool :reserve-user-token new-user :reserve-validator-token new-validator))
    (clempo.tip20:system-transfer-from (clempo.tip20:from-address validator-token)
                                       +fee-manager-address+ msg-sender amount-in)
    ;; collect_fee_pre_tx creates fee-manager balance slots for free: no credits for them.
    (set-tip1060-credit-minting nil)
    (clempo.tip20:transfer (clempo.tip20:from-address user-token) +fee-manager-address+ to amount-out)
    (emit +fee-manager-address+ 'rebalance-swap-event
          user-token validator-token msg-sender amount-in amount-out)
    amount-in))

(defun mint (msg-sender user-token validator-token amount-validator-token to)
  "TipFeeManager::mint: deposit validator token into a pool for LP tokens. Returns the
liquidity minted to TO."
  (when (= user-token validator-token) (revert 'identical-addresses))
  (when (zerop amount-validator-token) (revert 'invalid-amount))
  (clempo.tip20:validate-usd-currency user-token)
  (clempo.tip20:validate-usd-currency validator-token)
  (fork-if (:since :t8)
      (progn
        (clempo.tip20:ensure-authorized-as user-token
                                            (list (cons msg-sender :sender)
                                                  (cons +fee-manager-address+ :recipient)
                                                  (cons to :recipient)))
        (clempo.tip20:ensure-authorized-as validator-token (list (cons to :recipient))))
      nil)
  (let* ((pool-id (pool-id user-token validator-token))
         (pool (read (pools +fee-manager-address+ pool-id)))
         (supply (get-total-supply pool-id))
         (liquidity
           (if (and (zerop (pool-reserve-user-token pool))
                    (zerop (pool-reserve-validator-token pool)))
               (let ((half (floor amount-validator-token 2)))
                 (when (<= half +min-liquidity+) (revert 'insufficient-liquidity))
                 (setf supply (checked+ supply +min-liquidity+))
                 (set-total-supply pool-id supply)
                 (- half +min-liquidity+))
               ;; As if the caller rebalance-swapped then minted with both tokens:
               ;; amount * supply / (V + N/SCALE * U).
               (let* ((product (floor (* +rebalance-n+ (pool-reserve-user-token pool))
                                      +fee-scale+))
                      (denom (+ (pool-reserve-validator-token pool) product)))
                 (when (zerop denom) (revert 'division-by-zero))
                 (let ((num (u256-mul amount-validator-token supply)))
                   (unless num (revert 'invalid-swap-calculation))
                   (floor num denom))))))
    (when (zerop liquidity) (revert 'insufficient-liquidity))
    (clempo.tip20:system-transfer-from validator-token +fee-manager-address+ msg-sender
                                       amount-validator-token)
    (let ((reserve (+ (pool-reserve-validator-token pool)
                      (to-u128 amount-validator-token 'invalid-amount))))
      (when (> reserve u128-max) (revert 'invalid-amount))
      (write (pools +fee-manager-address+ pool-id)
             (make-pool :reserve-user-token (pool-reserve-user-token pool)
                        :reserve-validator-token reserve)))
    (set-total-supply pool-id (checked+ supply liquidity))
    (set-liquidity-balances pool-id to (checked+ (get-liquidity-balances pool-id to) liquidity))
    (emit +fee-manager-address+ 'mint-event
          msg-sender to user-token validator-token amount-validator-token liquidity)
    liquidity))

(defun calculate-burn-amounts (pool pool-id liquidity)
  "TipFeeManager::calculate_burn_amounts: the pro-rata (user validator) amounts of LIQUIDITY."
  (let ((supply (get-total-supply pool-id)))
    (flet ((share (reserve)
             (let ((p (u256-mul liquidity reserve)))
               (when (or (null p) (zerop supply)) (panic +panic-under-overflow+))
               (floor p supply))))
      (values (share (pool-reserve-user-token pool))
              (share (pool-reserve-validator-token pool))))))

(defun burn (msg-sender user-token validator-token liquidity to)
  "TipFeeManager::burn: redeem LIQUIDITY for the pro-rata share of both reserves, sent to TO.
Returns (values amount-user-token amount-validator-token)."
  (when (= user-token validator-token) (revert 'identical-addresses))
  (when (zerop liquidity) (revert 'invalid-amount))
  (clempo.tip20:validate-usd-currency user-token)
  (clempo.tip20:validate-usd-currency validator-token)
  (fork-if (:since :t8)
      (progn
        (clempo.tip20:ensure-authorized-as user-token (list (cons msg-sender :sender)))
        (clempo.tip20:ensure-authorized-as validator-token (list (cons msg-sender :sender))))
      nil)
  (let* ((pool-id (pool-id user-token validator-token))
         (balance (get-liquidity-balances pool-id msg-sender)))
    (when (< balance liquidity) (revert 'insufficient-liquidity))
    (let ((pool (read (pools +fee-manager-address+ pool-id))))
      (multiple-value-bind (amount-user amount-validator)
          (calculate-burn-amounts pool pool-id liquidity)
        (let ((available (- (pool-reserve-validator-token pool)
                            (to-u128 amount-validator 'invalid-amount))))
          (when (minusp available) (revert 'insufficient-reserves))
          (fork-if (:since :t1c)
              (when (< available
                       (t-read (pending-fee-swap-reservation +fee-manager-address+ pool-id)))
                (revert 'insufficient-liquidity))
              nil))
        (set-liquidity-balances pool-id msg-sender (checked- balance liquidity))
        (set-total-supply pool-id (checked- (get-total-supply pool-id) liquidity))
        (let ((user (- (pool-reserve-user-token pool) (to-u128 amount-user 'invalid-amount)))
              (validator (- (pool-reserve-validator-token pool)
                            (to-u128 amount-validator 'invalid-amount))))
          (when (or (minusp user) (minusp validator)) (revert 'insufficient-reserves))
          (write (pools +fee-manager-address+ pool-id)
                 (make-pool :reserve-user-token user :reserve-validator-token validator)))
        (clempo.tip20:transfer user-token +fee-manager-address+ to amount-user)
        (clempo.tip20:transfer validator-token +fee-manager-address+ to amount-validator)
        (emit +fee-manager-address+ 'burn-event
              msg-sender user-token validator-token amount-user amount-validator liquidity to)
        (values amount-user amount-validator)))))

(defun plan-fee-route (user-token validator-token max-amount)
  "TipFeeManager::plan_fee_route (read-only). Returns (values route intermediate pools):
ROUTE is :same-token, :direct, (:two-hop . INTERMEDIATE) or NIL when no path has the
liquidity; INTERMEDIATE is userToken.quoteToken() whenever it was read; POOLS lists
((user . validator) . observed-validator-reserve) for every pool read, in order."
  (when (= user-token validator-token)
    (return-from plan-fee-route (values :same-token nil nil)))
  (let ((data '())
        (amount-out (compute-amount-out max-amount)))
    (flet ((check (user validator amount)
             (let ((pool (get-pool user validator)))
               (push (cons (cons user validator) (pool-reserve-validator-token pool)) data)
               (pool-enough-reserve-p pool amount))))
      (when (check user-token validator-token amount-out)
        (return-from plan-fee-route (values :direct nil (reverse data))))
      (fork-if (:before :t5)
          (values nil nil (reverse data))
          (let ((mid (clempo.tip20:quote-token (clempo.tip20:from-address user-token))))
            (cond ((or (zerop mid) (= mid validator-token)
                       (not (check user-token mid amount-out))
                       (not (check mid validator-token (compute-amount-out amount-out))))
                   (values nil mid (reverse data)))
                  (t (values (cons :two-hop mid) mid (reverse data)))))))))

(defun execute-fee-swap (user-token validator-token amount-in)
  "TipFeeManager::execute_fee_swap: swap AMOUNT-IN of user token into validator token at
M/SCALE through the direct pool. Returns the validator-token amount out."
  (let* ((amount-out (compute-amount-out amount-in))
         (pool-id (pool-id user-token validator-token))
         (pool (read (pools +fee-manager-address+ pool-id))))
    ;; Pool::apply_swap
    (when (> amount-out (pool-reserve-validator-token pool)) (revert 'insufficient-liquidity))
    (when (or (> amount-in u128-max) (> amount-out u128-max)) (panic +panic-under-overflow+))
    (write (pools +fee-manager-address+ pool-id)
           (make-pool :reserve-user-token (checked-u128+ (pool-reserve-user-token pool) amount-in)
                      :reserve-validator-token (- (pool-reserve-validator-token pool)
                                                  amount-out)))
    amount-out))

(defun checked-u128+ (a b)
  (let ((s (+ a b)))
    (when (> s u128-max) (panic +panic-under-overflow+))
    s))
