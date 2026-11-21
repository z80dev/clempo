(defpackage #:clempo.fee-manager.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))

(in-package #:clempo.fee-manager.tests)

(deftest fee-manager-layout ()
  (let ((errors (clempo.contract.layout-tests:layout-fixture-mismatches
                 "fee_manager" 'clempo.fee-manager:fee-manager
                 :exclude '(clempo.fee-manager::pending-fee-swap-reservation
                            clempo.fee-manager::two-hop-intermediate))))
    (is (null errors) "~{~A~^~%~}" errors)))

(deftest fee-manager-default-preferences ()
  (with-storage ((make-hashmap-storage))
    (is= 0 (clempo.fee-manager:user-token #x123))
    (is= +default-fee-token+ (clempo.fee-manager:validator-token #x123))))

(deftest fee-manager-fee-swap-rounding ()
  ;; The reference multiplies before dividing, with checked uint256 arithmetic.
  (is= 0 (clempo.fee-manager::compute-amount-out 1))
  (is= 9970 (clempo.fee-manager::compute-amount-out 10000))
  (is= 9940 (clempo.fee-manager::compute-amount-out
             (clempo.fee-manager::compute-amount-out 10000)))))

(deftest fee-manager-directional-pool-id ()
  (with-storage ((make-hashmap-storage))
    (is (/= (clempo.fee-manager::pool-id #x123 #x456)
            (clempo.fee-manager::pool-id #x456 #x123)))
    (is= (clempo.base:octets->integer
          (clempo.keccak:keccak256 (clempo.abi:abi-encode '("address" "address") '(#x123 #x456))))
         (clempo.fee-manager::pool-id #x123 #x456))))

(deftest fee-manager-fee-swap-reserves ()
  (with-storage ((make-hashmap-storage))
    (let ((place (clempo.fee-manager::pools
                  +fee-manager-address+ (clempo.fee-manager::pool-id #x123 #x456))))
      (write place (clempo.fee-manager::make-pool
                    :reserve-user-token 100 :reserve-validator-token 20000))
      (is= 9970 (clempo.fee-manager:execute-fee-swap #x123 #x456 10000))
      (let ((pool (read place)))
        (is= 10100 (clempo.fee-manager::pool-reserve-user-token pool))
        (is= 10030 (clempo.fee-manager::pool-reserve-validator-token pool))))))

(deftest fee-manager-precompile-genesis ()
  (with-spec (:genesis)
    (is (eq 'clempo.fee-manager:fee-manager
            (precompile-contract +fee-manager-address+)))))

(deftest fee-manager-fallback-tokens ()
  (is= (list +path-usd-address+) (fallback-fee-tokens :t12))
  (is= (list +path-usd-address+ +ousd-address+) (fallback-fee-tokens :t13)))

(deftest fee-manager-reservation-fork ()
  ;; Reservation writes must not affect persistent storage and begin at T1C.
  (dolist (fork '(:t1b :t1c))
    (with-spec (fork)
      (with-storage ((make-hashmap-storage))
        (let* ((id (clempo.fee-manager::pool-id #x123 #x456))
               (place (clempo.fee-manager::pending-fee-swap-reservation
                       +fee-manager-address+ id)))
          (clempo.fee-manager::reserve-fee-liquidity #x123 #x456 10000 :direct)
          (is= 0 (read place))
          (is= (if (eq fork :t1b) 0 9970) (t-read place)))))))
