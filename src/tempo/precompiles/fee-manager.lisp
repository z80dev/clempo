(in-package #:clempo.fee-manager)

;;; TipFeeManager (crates/precompiles/src/tip_fee_manager/{mod,dispatch}.rs): fee token
;;; preferences, fee collection around a transaction, and distribution. The fee AMM's
;;; operations (pool-id, get-pool, compute-amount-out, plan-fee-route, reserve-pool-liquidity,
;;; execute-fee-swap, mint, burn, rebalance-swap) live in fee-amm.lisp; this file owns the
;;; layout, every error and event of both interfaces, and the dispatch.

(defstorable pool
  (reserve-user-token u128)
  (reserve-validator-token u128))

(defcontract fee-manager (:address +fee-manager-address+)
  (validator-tokens (mapping address address))
  (user-tokens (mapping address address))
  (collected-fees (mapping address (mapping address u256)))
  (pools (mapping b256 pool))
  (total-supply (mapping b256 u256))
  (liquidity-balances (mapping b256 (mapping address u256)))
  (pending-fee-swap-reservation (mapping b256 u128) :since :t1c
   :doc "Transient only (t-read/t-write): validator-token liquidity reserved by collect-fee-pre-tx.")
  (two-hop-intermediate address :since :t5
   :doc "Transient only: the intermediate token of a two-hop fee route."))

;;; IFeeManager
(deferror invalid-token "InvalidToken()")
(deferror insufficient-fee-token-balance "InsufficientFeeTokenBalance()")
(deferror cannot-change-within-block "CannotChangeWithinBlock()")

(defevent user-token-set "UserTokenSet(address indexed user, address indexed token)")
(defevent validator-token-set "ValidatorTokenSet(address indexed validator, address indexed token)")
(defevent fees-distributed "FeesDistributed(address indexed validator, address indexed token, uint256 amount)")

;;; ITIPFeeAMM
(deferror identical-addresses "IdenticalAddresses()")
(deferror amm-invalid-token "InvalidToken()")
(deferror insufficient-liquidity "InsufficientLiquidity()")
(deferror insufficient-reserves "InsufficientReserves()")
(deferror invalid-amount "InvalidAmount()")
(deferror division-by-zero "DivisionByZero()")
(deferror invalid-swap-calculation "InvalidSwapCalculation()")

(defevent mint-event "Mint(address sender, address indexed to, address indexed userToken, address indexed validatorToken, uint256 amountValidatorToken, uint256 liquidity)")
(defevent burn-event "Burn(address indexed sender, address indexed userToken, address indexed validatorToken, uint256 amountUserToken, uint256 amountValidatorToken, uint256 liquidity, address to)")
(defevent rebalance-swap-event "RebalanceSwap(address indexed userToken, address indexed validatorToken, address indexed swapper, uint256 amountIn, uint256 amountOut)")

;;; Preferences

(defun validator-token (beneficiary)
  "get_validator_token: BENEFICIARY's fee token, the default fee token when unset."
  (let ((token (read (validator-tokens +fee-manager-address+ beneficiary))))
    (if (zerop token) +default-fee-token+ token)))

(defun user-token (account)
  "user_tokens: ACCOUNT's chosen fee token (0 when unset)."
  (read (user-tokens +fee-manager-address+ account)))

(defun set-validator-token (sender token beneficiary)
  (unless (clempo.tip20-factory:tip20-p token) (revert 'invalid-token))
  (when (= sender beneficiary) (revert 'cannot-change-within-block))
  (clempo.tip20:validate-usd-currency token)
  (write (validator-tokens +fee-manager-address+ sender) token)
  (emit +fee-manager-address+ 'validator-token-set sender token)
  (values))

(defun set-user-token (sender token)
  (unless (clempo.tip20-factory:tip20-p token) (revert 'invalid-token))
  (clempo.tip20:validate-usd-currency token)
  (when (fork-if (:since :t3)
            (= (read (user-tokens +fee-manager-address+ sender)) token)
            nil)
    (return-from set-user-token (values)))
  (write (user-tokens +fee-manager-address+ sender) token)
  (emit +fee-manager-address+ 'user-token-set sender token)
  (values))

;;; Fee collection (called by the executor, not via dispatch)

(defun reserve-fee-liquidity (user-token validator-token max-amount route)
  (cond ((eq route :same-token))
        ((eq route :direct)
         (fork-if (:since :t1c)
             (reserve-pool-liquidity (pool-id user-token validator-token)
                                     (to-u128 (compute-amount-out max-amount)))))
        ((and (consp route) (eq (car route) :two-hop))
         (let* ((mid (cdr route))
                (out1 (to-u128 (compute-amount-out max-amount)))
                (out2 (to-u128 (compute-amount-out out1))))
           (reserve-pool-liquidity (pool-id user-token mid) out1)
           (reserve-pool-liquidity (pool-id mid validator-token) out2)
           (t-write (two-hop-intermediate +fee-manager-address+) mid))))
  nil)

(defun collect-fee-pre-tx (fee-payer user-token max-amount beneficiary skip-liquidity-check)
  "TipFeeManager::collect_fee_pre_tx: move MAX-AMOUNT of USER-TOKEN from FEE-PAYER to the
fee manager and reserve AMM liquidity for the swap. Returns USER-TOKEN."
  (let ((validator-token (validator-token beneficiary)))
    (clempo.tip20:from-address user-token)
    (fork-if (:since :t8)
        (clempo.tip20:ensure-authorized-as user-token (list (cons fee-payer :sender)))
        (clempo.tip20:ensure-transfer-authorized user-token fee-payer +fee-manager-address+))
    (clempo.tip20:transfer-fee-pre-tx user-token fee-payer max-amount)
    (unless skip-liquidity-check
      (let ((route (plan-fee-route user-token validator-token max-amount)))
        (unless route (revert 'insufficient-liquidity))
        (reserve-fee-liquidity user-token validator-token max-amount route)))
    user-token))

(defun increment-collected-fees (validator token amount)
  (unless (zerop amount)
    (sinc (collected-fees +fee-manager-address+ validator token) amount)))

(defun collect-fee-post-tx (fee-payer actual-spending refund fee-token beneficiary)
  "TipFeeManager::collect_fee_post_tx: refund the unused fee, swap the spent part into the
validator's token and credit it to BENEFICIARY. Returns the amount credited."
  (clempo.tip20:from-address fee-token)
  (clempo.tip20:transfer-fee-post-tx fee-token fee-payer refund actual-spending)
  (let* ((hop (t-read (two-hop-intermediate +fee-manager-address+)))
         (validator-token (validator-token beneficiary))
         (amount (cond ((= fee-token validator-token) actual-spending)
                       ((zerop hop)
                        (unless (zerop actual-spending)
                          (execute-fee-swap fee-token validator-token actual-spending))
                        (compute-amount-out actual-spending))
                       (t
                        (unless (zerop actual-spending)
                          (let ((out1 (execute-fee-swap fee-token hop actual-spending)))
                            (execute-fee-swap hop validator-token out1)))
                        (compute-amount-out (compute-amount-out actual-spending))))))
    (increment-collected-fees beneficiary validator-token amount)
    amount))

(defun distribute-fees (validator token)
  (set-tip1060-credit-minting nil)
  (let* ((place (collected-fees +fee-manager-address+ validator token))
         (amount (read place)))
    (unless (zerop amount)
      (write place 0)
      (clempo.tip20:from-address token)
      (clempo.tip20:transfer token +fee-manager-address+ validator amount)
      (emit +fee-manager-address+ 'fees-distributed validator token amount))
    (values)))

;;; IFeeManager calls

(defcall fee-manager user-tokens* (user)
    (:sol "userTokens(address user)" :returns "address")
  (user-token user))

(defcall fee-manager validator-tokens* (validator)
    (:sol "validatorTokens(address validator)" :returns "address")
  (validator-token validator))

(defcall fee-manager collected-fees* (validator token)
    (:sol "collectedFees(address validator, address token)" :returns "uint256")
  (read (collected-fees self validator token)))

(defcall fee-manager set-validator-token* (token)
    (:sol "setValidatorToken(address token)" :mutates t)
  (set-validator-token sender token (block-beneficiary)))

(defcall fee-manager set-user-token* (token)
    (:sol "setUserToken(address token)" :mutates t)
  (set-user-token sender token))

(defcall fee-manager distribute-fees* (validator token)
    (:sol "distributeFees(address validator, address token)" :mutates t)
  (distribute-fees validator token))

;;; ITIPFeeAMM calls

(defcall fee-manager m () (:sol "M()" :returns "uint256") 9970)
(defcall fee-manager n () (:sol "N()" :returns "uint256") 9985)
(defcall fee-manager scale () (:sol "SCALE()" :returns "uint256") 10000)
(defcall fee-manager min-liquidity () (:sol "MIN_LIQUIDITY()" :returns "uint256") 1000)

(defcall fee-manager get-pool-id (user-token validator-token)
    (:sol "getPoolId(address userToken, address validatorToken)" :returns "bytes32")
  (pool-id user-token validator-token))

(defcall fee-manager get-pool* (user-token validator-token)
    (:sol "getPool(address userToken, address validatorToken)"
     :returns ("uint128" "uint128"))
  (let ((p (get-pool user-token validator-token)))
    (values (pool-reserve-user-token p) (pool-reserve-validator-token p))))

(defcall fee-manager pools* (pool-id)
    (:sol "pools(bytes32 poolId)" :returns ("uint128" "uint128"))
  (let ((p (read (pools self pool-id))))
    (values (pool-reserve-user-token p) (pool-reserve-validator-token p))))

(defcall fee-manager total-supply* (pool-id)
    (:sol "totalSupply(bytes32 poolId)" :returns "uint256")
  (read (total-supply self pool-id)))

(defcall fee-manager liquidity-balances* (pool-id user)
    (:sol "liquidityBalances(bytes32 poolId, address user)" :returns "uint256")
  (read (liquidity-balances self pool-id user)))

(defcall fee-manager mint* (user-token validator-token amount to)
    (:sol "mint(address userToken, address validatorToken, uint256 amountValidatorToken, address to)"
     :returns "uint256" :mutates t)
  (mint sender user-token validator-token amount to))

(defcall fee-manager burn* (user-token validator-token liquidity to)
    (:sol "burn(address userToken, address validatorToken, uint256 liquidity, address to)"
     :returns ("uint256" "uint256") :mutates t)
  (burn sender user-token validator-token liquidity to))

(defcall fee-manager rebalance-swap* (user-token validator-token amount-out to)
    (:sol "rebalanceSwap(address userToken, address validatorToken, uint256 amountOut, address to)"
     :returns "uint256" :mutates t)
  (rebalance-swap sender user-token validator-token amount-out to))

(defdispatch fee-manager (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +fee-manager-address+ 'fee-manager :genesis)
