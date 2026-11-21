(in-package #:clempo.txpool)

;; Read-only admission planner, not fee collection. Rust specifications:
;; precompiles/src/tip_fee_manager/{mod.rs,amm.rs}::TipFeeManager/Pool/PoolKey/plan_fee_route
;; and transaction-pool/src/amm.rs::has_enough_liquidity. FeeManager pools is mapping
;; slot 3; PoolKey is keccak256(abi.encode(user,validator)); reserveValidatorToken is
;; the high u128 of its packed row. TIP20 quoteToken is slot 6. No state is cached:
;; revalidation sees liquidity and quote-token changes in its actual snapshot.

(defun pool-validator-reserve (db user-token validator-token)
  (let* ((id (octets->integer
              (clempo.keccak:keccak256
               (octets-concat (integer->octets user-token 32)
                              (integer->octets validator-token 32)))))
         (word (clempo.store:db-storage db +fee-manager-address+ (state-mapping-slot id 3))))
    (ldb (byte 128 128) word)))

(defun fee-swap-output (fee)
  (let ((product (* fee 9970)))
    (when (> (integer-length product) 256) (reject :fee-overflow))
    (floor product 10000)))

(defun sufficient-reserve-p (reserve amount)
  ;; An empty pool is unusable even when rounding produces zero output.
  (and (plusp reserve) (>= reserve amount)))

(defmethod state-reader-liquidity-p ((reader database-state-reader) token fee)
  (check-type token (unsigned-byte 160))
  (check-type fee (unsigned-byte 256))
  (let ((tokens (state-reader-validator-tokens reader)))
    ;; A damaged snapshot is a provider failure, not an invalid transaction to evict.
    (when (null tokens) (error "Pool state reader lost required validator-token history"))
    (if (member token tokens :test #'=) t
        (let* ((db (state-reader-db reader))
               (spec (clempo.executor:block-env-spec (state-reader-env reader)))
               (out (fee-swap-output fee))
               (out2 (when (clempo.forks:fork>= spec :t5) (fee-swap-output out)))
               (quote nil) (quote-read-p nil))
          (and
           (some
            (lambda (validator-token)
              (or (sufficient-reserve-p (pool-validator-reserve db token validator-token) out)
                  (when out2
                    (unless quote-read-p
                      (setf quote (ldb (byte 160 0) (clempo.store:db-storage db token 6))
                            quote-read-p t))
                    (and (plusp quote) (/= quote validator-token)
                         (sufficient-reserve-p (pool-validator-reserve db token quote) out)
                         (sufficient-reserve-p (pool-validator-reserve db quote validator-token) out2)))))
            tokens)
           t)))))
