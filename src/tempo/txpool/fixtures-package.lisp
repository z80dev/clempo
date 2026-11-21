(defpackage #:clempo.txpool.fixtures
  (:use #:cl #:clempo.base)
  (:export #:make-fixture-reader #:fixture-transaction #:fixture-address
           #:fixture-change-state #:fixture-fund #:fixture-set-key #:fixture-set-nonce
           #:recorded-transaction #:run-fixture-validation #:fixture-key-authorization
           #:fixture-eip7702-authorization
           #:+fixture-token+ #:+fixture-timestamp+ #:+fixture-base-fee+))
