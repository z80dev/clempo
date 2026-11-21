(defpackage #:clempo.executor.call-tests
  (:use #:cl #:clempo.base #:clempo.executor)
  (:local-nicknames (#:store #:clempo.store) (#:blocks #:clempo.blocks) (#:tx #:clempo.transactions)
                    (#:history #:clempo.history) (#:state #:clempo.state))
  (:import-from #:clempo.executor.mainnet-tests #:fixture-block #:prestate-db))

(in-package #:clempo.executor.call-tests)

;;; Calls over the regression fixture of block 4,711,932 (mainnet-blocks-tests.lisp): its
;;; prestate as a state database and as a clempo.history view of it, the block's prefix
;;; replays, gas estimation and access lists.

(defparameter *n* 4711932)

(defun first-request (block)
  (let ((tx (first (blocks:block-transactions block))))
    (list :from (tx:transaction-sender tx) :to (tx:transaction-to tx)
          :value (tx:transaction-value tx) :input (tx:transaction-input tx) :type 2)))

(defun fixture-history (db)
  "A history holding DB's state as a full segment; returns (values history view dir)."
  (let* ((dir (format nil "/tmp/clempo-call-test-~D/" (random 100000000 (make-random-state t))))
         (h (history:open-history dir)))
    (history:history-start-writer h 0 db)
    (history:history-record-state h db)
    (values h (history:history-state h (store:db-number db)) dir)))

(deftest prefix-replay-matches-the-block ()
  (let* ((block (fixture-block *n*))
         (db (prestate-db *n* block))
         (txs (blocks:block-transactions block))
         (full (let ((ctx (block-prefix-context db block 0 :chain :mainnet)))
                 (loop for tx in txs collect (apply-transaction ctx tx (tx:transaction-sender tx))))))
    (multiple-value-bind (h view dir) (fixture-history db)
      (unwind-protect
           (loop for k in (list 0 (floor (length txs) 2) (1- (length txs)))
                 do (dolist (backend (list db view))
                      (let* ((ctx (block-prefix-context backend block k :chain :mainnet))
                             (tx (nth k txs))
                             (outcome (apply-transaction ctx tx (tx:transaction-sender tx))))
                        (is (= (tx-outcome-gas-used outcome) (tx-outcome-gas-used (nth k full)))
                            "tx ~D on ~A" k (type-of backend)))))
        (history:close-history h)
        (sb-ext:delete-directory dir :recursive t)))))

(deftest estimate-gas-finds-a-working-limit ()
  (let* ((block (fixture-block *n*))
         (db (prestate-db *n* block))
         (env (header-block-env (blocks:block-header block) :chain :mainnet))
         (request (first-request block)))
    (flet ((run (gas) (call-request (begin-call-block db env) (list* :gas gas request))))
      (let* ((used (tx-outcome-gas-used (run 30000000)))
             (estimate (estimate-gas #'run :max-gas-limit 30000000)))
        (is (>= estimate used))
        (is (tx-outcome-status (run estimate)))
        ;; the search stops within reth's 1.5% above a limit that fails (the inner CREATE2
        ;; keeps 1/64 back, so the limit needed is above the gas used)
        (is (not (tx-outcome-status (run (floor (* estimate 98) 100)))) "estimate ~D, used ~D" estimate used)
        ;; a request gas limit below what it needs: the retry at the cap succeeds, so
        ;; "out of gas: gas required exceeds" the request's limit
        (handler-case (progn (estimate-gas #'run :max-gas-limit 30000000 :request-gas (floor used 2))
                             (is nil "no failure"))
          (estimate-failure (c)
            (is (eq :basic-out-of-gas (estimate-failure-kind c)))
            (is (= (floor used 2) (estimate-failure-gas-limit c)))))))))

(deftest access-list-names-the-touched-storage ()
  (let* ((block (fixture-block *n*))
         (db (prestate-db *n* block))
         (env (header-block-env (blocks:block-header block) :chain :mainnet))
         (request (list* :gas 30000000 (first-request block))))
    (multiple-value-bind (list gas outcome)
        (create-access-list (lambda (r) (call-request (begin-call-block db env) r)) request)
      (is (tx-outcome-status outcome))
      (is (plusp gas))
      (is (equal list (sort (copy-list list) #'< :key #'car)))
      ;; sender and target are never listed unless their storage was touched
      (let ((from (getf request :from)))
        (is (null (assoc from list))))
      ;; running with the list costs what create-access-list reported
      (is (= gas (tx-outcome-gas-used (call-request (begin-call-block db env)
                                                    (list* :access-list list request))))))))
