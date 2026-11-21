(defpackage #:clempo.pool-service.tests
  (:use #:cl #:clempo.base #:clempo.pool-service)
  (:local-nicknames (#:pool #:clempo.txpool) (#:fx #:clempo.txpool.fixtures)
                    (#:txs #:clempo.transactions)))

(in-package #:clempo.pool-service.tests)

;;; The chain-attached pool over the recorded mainnet fee prestate (tests/tempo/txpool).

(defmacro with-service ((service db env) &body body)
  (let ((reader (gensym)))
    `(let* ((,reader (fx:make-fixture-reader))
            (,db (pool:state-reader-db ,reader))
            (,env (pool:state-reader-env ,reader))
            (,service (make-pool-service))
            (clempo.io:*clock* (lambda () (* fx:+fixture-timestamp+ 1000))))
       (declare (ignorable ,env))
       ,@body)))

(defun rejection (thunk)
  (handler-case (progn (funcall thunk) nil)
    (pool:pool-error (c) (pool:pool-error-code c))))

(deftest pool-service-admits-lists-and-clears ()
  (with-service (service db env)
    (let ((tx (fx:fixture-transaction)))
      ;; No head noted yet: nothing to validate against.
      (is= :not-ready (rejection (lambda () (pool-service-submit service tx db))))
      (pool-service-head service db env)
      (multiple-value-bind (hash status) (pool-service-submit service tx db)
        (is-equalp (txs:transaction-hash tx) hash)
        (is (eq :pending status)))
      (is= :already-imported (rejection (lambda () (pool-service-submit service tx db))))
      ;; A nonce gap is queued, not pending.
      (pool-service-submit service (fx:fixture-transaction :nonce 2) db)
      (is-equalp '(t nil) (mapcar #'third (pool-service-transactions service)))
      (is= 2 (pool-service-clear service))
      (is= 0 (pool-service-count service)))))

(deftest pool-service-revalidates-on-each-head ()
  (with-service (service db env)
    (pool-service-head service db env)
    (pool-service-submit service (fx:fixture-transaction) db)
    ;; The sender's transaction is mined: its protocol nonce advances past the pooled one.
    (let ((sender (fx:fixture-address 1)))
      (clempo.store:db-begin-block db)
      (let ((account (clempo.store:db-account db sender)))
        (clempo.store:db-set-account db sender 1 (clempo.store:account-balance account)
                                     (clempo.store:account-code-hash account)))
      (clempo.store:db-commit-block db 1 (make-octets 32)))
    (let ((removed (pool-service-head service db env)))
      (is= 1 (length removed))
      (is= 0 (pool-service-count service)))))
