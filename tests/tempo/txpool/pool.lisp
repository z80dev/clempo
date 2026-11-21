(defpackage #:clempo.txpool.tests
  (:use #:cl #:clempo.base #:clempo.txpool #:clempo.txpool.fixtures))
(in-package #:clempo.txpool.tests)

(defmacro with-pool ((pool reader &rest options) &body body)
  `(let ((,pool (make-pool ,@options)) (,reader (make-fixture-reader))
         (clempo.io:*clock* (lambda () (* +fixture-timestamp+ 1000))))
     ,@body))

(defun rejection-code (thunk)
  (handler-case (progn (funcall thunk) nil)
    (pool-error (condition) (pool-error-code condition))))

(defmacro rejects (code form)
  `(is= ,code (rejection-code (lambda () ,form))))

(defun selected-nonces (pool &rest options)
  (mapcar #'clempo.transactions:transaction-nonce (apply #'pool-best-transactions pool options)))

(deftest pool-fixture-measurement-scenarios ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (multiple-value-bind (accepted rejected ready) (run-fixture-validation)
    (is= 4 accepted) (is= 5 rejected) (is= 4 ready)))

(deftest protocol-and-2d-nonce-gap-order ()
  (skip-test "needs executor skip-nonce-check handling and T7 fee slots")
  (with-pool (pool reader)
    (let ((tx2 (fixture-transaction :nonce 2 :priority-fee 2000000000))
          (tx0 (fixture-transaction)) (tx1 (fixture-transaction :nonce 1)))
      (is= :queued (nth-value 1 (submit-transaction pool tx2 reader)))
      (is (null (pool-best-transactions pool)))
      (submit-transaction pool tx0 reader)
      (is= '(0) (selected-nonces pool))
      (submit-transaction pool tx1 reader)
      (is= '(0 1 2) (selected-nonces pool))
      (pool-remove-transaction pool (clempo.transactions:transaction-hash tx1))
      (is= '(0) (selected-nonces pool))
      (submit-transaction pool (fixture-transaction :nonce-key 7) reader)
      (is= 2 (length (pool-best-transactions pool))))))

(deftest current-state-nonce-not-assumed-zero ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (fixture-change-state reader (lambda (db) (fixture-set-nonce db (fixture-address 1) 9 5)))
    (rejects :nonce-too-low (submit-transaction pool (fixture-transaction :nonce-key 9 :nonce 4) reader))
    (submit-transaction pool (fixture-transaction :nonce-key 9 :nonce 6) reader)
    (is (null (pool-best-transactions pool)))
    (submit-transaction pool (fixture-transaction :nonce-key 9 :nonce 5) reader)
    (is= '(5 6) (selected-nonces pool))))

(deftest effective-tip-priority-and-fifo-ties ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((low (fixture-transaction :private-key 1 :priority-fee 10 :fee-cap 2000000000))
          (first (fixture-transaction :private-key 2 :priority-fee 30 :fee-cap 2000000000))
          (second (fixture-transaction :private-key 3 :priority-fee 30 :fee-cap 2000000000)))
      (mapc (lambda (tx) (submit-transaction pool tx reader)) (list low first second))
      (is= (list first second low) (pool-best-transactions pool))
      (is= (list first second low) (pool-best-transactions pool)))))

(deftest priorities-recomputed-at-building-base-fee ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((a (fixture-transaction :private-key 1 :fee-cap 2000000000 :priority-fee 1000000000))
          (b (fixture-transaction :private-key 2 :fee-cap 3000000000 :priority-fee 500000000)))
      (submit-transaction pool a reader) (submit-transaction pool b reader)
      (is= (list a b) (pool-best-transactions pool :base-fee 1000000000))
      (is= (list b a) (pool-best-transactions pool :base-fee 1900000000)))))

(deftest dynamic-under-base-fee-queues-not-invalid ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (submit-transaction pool (fixture-transaction :fee-cap 700000000 :priority-fee 100) reader)
    (submit-transaction pool (fixture-transaction :nonce 1) reader)
    (is (null (pool-best-transactions pool)))
    (is= '(0 1) (selected-nonces pool :base-fee 600000000))))

(deftest replacement-bumps-both-fee-dimensions-and-is-atomic ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((old (fixture-transaction)))
      (submit-transaction pool old reader)
      (rejects :replacement-underpriced
        (submit-transaction pool (fixture-transaction :fee-cap 2200000000 :priority-fee 1099999999) reader))
      (rejects :replacement-underpriced
        (submit-transaction pool (fixture-transaction :fee-cap 2199999999 :priority-fee 1100000000) reader))
      (is= (list old) (pool-best-transactions pool))
      (let ((replacement (fixture-transaction :fee-cap 2200000000 :priority-fee 1100000000)))
        (is (eq old (nth-value 2 (submit-transaction pool replacement reader))))
        (is= (list replacement) (pool-best-transactions pool)))
      (is= 1 (pool-count pool)))))

(deftest duplicate-and-nonce-boundaries ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((tx (fixture-transaction)))
      (submit-transaction pool tx reader)
      (rejects :already-imported (submit-transaction pool tx reader)))
    (rejects :nonce-overflow (submit-transaction pool (fixture-transaction :nonce #xffffffffffffffff) reader))
    (rejects :subblock-nonce-key
      (submit-transaction pool (fixture-transaction :nonce-key (ash #x5b 248)) reader))
    (rejects :fee-cap-below-floor
      (submit-transaction pool (fixture-transaction :private-key 2 :fee-cap 599999999 :priority-fee 0) reader))))

(deftest time-boundaries-are-exclusive-before-inclusive-after ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (rejects :expired (submit-transaction pool (fixture-transaction :valid-before 1003) reader))
    (submit-transaction pool (fixture-transaction :valid-before 1004 :valid-after 1001) reader)
    (is (null (pool-best-transactions pool :timestamp 1000)))
    (is= '(0) (selected-nonces pool :timestamp 1001))
    (is= '(0) (selected-nonces pool :timestamp 1003))
    (is (null (pool-best-transactions pool :timestamp 1004)))
    (is= 1 (length (pool-sweep-expired pool 1004)))
    (is= 0 (pool-count pool))
    (submit-transaction pool (fixture-transaction :valid-after 1120) reader)
    (is (null (pool-sweep-expired pool 1100)))
    (rejects :valid-after-too-far
      (submit-transaction pool (fixture-transaction :private-key 2 :valid-after 1121) reader))))

(deftest future-head-blocks-descendants-without-eviction ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (submit-transaction pool (fixture-transaction :valid-after 1010) reader)
    (submit-transaction pool (fixture-transaction :nonce 1 :priority-fee 2000000000) reader)
    (is (null (pool-best-transactions pool :timestamp 1009)))
    (is= '(0 1) (selected-nonces pool :timestamp 1010))
    (is= 2 (pool-count pool))))

(deftest expiring-nonces-independent-dedup-and-expiry ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((a (fixture-transaction :nonce-key clempo.txpool::+expiring-nonce-key+ :valid-before 1020
                                   :sponsor-private-key 2))
          (b (fixture-transaction :nonce-key clempo.txpool::+expiring-nonce-key+ :valid-before 1021)))
      (submit-transaction pool a reader) (submit-transaction pool b reader)
      (is (not (equal (pool-transaction-lane pool a) (pool-transaction-lane pool b))))
      (rejects :already-imported
        (submit-transaction pool
                            (fixture-transaction :nonce-key clempo.txpool::+expiring-nonce-key+
                                                 :valid-before 1020 :sponsor-private-key 3) reader))
      (is= (list b) (pool-best-transactions pool :timestamp 1020))
      (is= 1 (length (pool-sweep-expired pool 1020)))
      (is= 1 (pool-count pool)))))

(deftest expiring-nonce-window-and-replay-use-real-storage ()
  (with-pool (pool reader)
    (rejects :invalid-transaction
      (submit-transaction pool (fixture-transaction :nonce-key clempo.txpool::+expiring-nonce-key+
                                                    :valid-before 1301) reader))
    (let* ((tx (fixture-transaction :nonce-key clempo.txpool::+expiring-nonce-key+ :valid-before 1020))
           (hash (octets->integer (clempo.transactions:transaction-unique-identifier tx (fixture-address 1)))))
      (fixture-change-state
       reader (lambda (db)
                (clempo.store:db-set-storage db clempo.txpool.fixtures::+nonce-manager+
                                            (clempo.txpool.fixtures::mapping-slot hash 1) 1020)))
      (rejects :invalid-transaction (submit-transaction pool tx reader)))))

(deftest sender-limits-and-gap-filler-eviction ()
  (skip-test "needs executor skip-nonce-check handling and T7 fee slots")
  (with-pool (pool reader :max-transactions-per-sender 2)
    (submit-transaction pool (fixture-transaction :nonce 1) reader)
    (let ((highest (fixture-transaction :nonce 2)))
      (submit-transaction pool highest reader)
      (rejects :sender-capacity (submit-transaction pool (fixture-transaction :nonce 3) reader))
      (submit-transaction pool (fixture-transaction) reader)
      (is= '(0 1) (selected-nonces pool))
      (is (null (pool-remove-transaction pool (clempo.transactions:transaction-hash highest)))))))

(deftest sponsor-counted-across-senders-and-replacement-frees-own-slot ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader :max-transactions-per-sender 2)
    (submit-transaction pool (fixture-transaction :private-key 1 :sponsor-private-key 4) reader)
    (submit-transaction pool (fixture-transaction :private-key 2 :sponsor-private-key 4) reader)
    (rejects :sender-capacity
      (submit-transaction pool (fixture-transaction :private-key 3 :sponsor-private-key 4) reader))
    (submit-transaction pool (fixture-transaction :private-key 1 :sponsor-private-key 4
                                                 :fee-cap 2200000000 :priority-fee 1100000000) reader)
    (is= 2 (pool-count pool))))

(deftest lane-limit-still-allows-current-nonce-gap-filler ()
  (skip-test "needs executor skip-nonce-check handling and T7 fee slots")
  (with-pool (pool reader :max-transactions-per-sender 8 :max-transactions-per-lane 2)
    (submit-transaction pool (fixture-transaction :nonce 1) reader)
    (submit-transaction pool (fixture-transaction :nonce 2) reader)
    (rejects :lane-capacity (submit-transaction pool (fixture-transaction :nonce 3) reader))
    (submit-transaction pool (fixture-transaction) reader)
    (is= '(0 1 2) (selected-nonces pool))))

(deftest ethereum-standard-types-and-system-rejection ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (dolist (type '(0 1 2))
      (submit-transaction pool (fixture-transaction :type type :private-key (1+ type)) reader))
    (is= 3 (length (pool-best-transactions pool)))
    (rejects :system-transaction (submit-transaction pool (recorded-transaction "legacy-system") reader))
    (rejects :invalid-transaction
      (submit-transaction pool (fixture-transaction :type 2 :private-key 5 :value 1) reader))))

(deftest recorded-ethereum-transaction-admitted-against-explicit-prestate ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (let* ((reader (make-fixture-reader :chain-id 42431))
         (pool (make-pool)) (tx (recorded-transaction "eth-0x2"))
         (sender (clempo.transactions:transaction-sender tx)))
    (fixture-change-state reader (lambda (db) (fixture-fund db sender 1000000000000000000
                                                           (clempo.transactions:transaction-nonce tx))))
    (let ((root (clempo.store:db-state-root (state-reader-db reader))))
      (submit-transaction pool tx reader)
      (is= sender (pool-transaction-sender pool tx))
      (is-equalp root (clempo.store:db-state-root (state-reader-db reader))))))

(deftest invalid-fee-token-sponsor-and-insolvency ()
  (with-pool (pool reader)
    (rejects :invalid-transaction (submit-transaction pool (fixture-transaction :fee-token #x1234) reader))
    (rejects :invalid-transaction (submit-transaction pool (fixture-transaction :sponsor-private-key 1) reader))
    (fixture-change-state reader (lambda (db) (fixture-fund db (fixture-address 2) 0)))
    (rejects :invalid-transaction (submit-transaction pool (fixture-transaction :sponsor-private-key 2) reader))
    (rejects :invalid-transaction (submit-transaction pool (fixture-transaction :chain-id 42431) reader))))

(deftest keychain-expiry-version-revocation-and-rollback ()
  (skip-test "needs executor T6 keychain handling")
  (with-pool (pool reader)
    (fixture-change-state reader (lambda (db) (fixture-set-key db (fixture-address 1) (fixture-address 2) 1020)))
    (let ((tx (fixture-transaction :key-private-key 2)))
      (submit-transaction pool tx reader)
      (is= (list tx) (pool-best-transactions pool :timestamp 1019))
      (is (null (pool-best-transactions pool :timestamp 1020)))
      (rejects :access-key-expired
        (submit-transaction pool (fixture-transaction :key-private-key 2 :nonce 1 :valid-after 1020) reader))
      (rejects :invalid-transaction
        (submit-transaction pool (fixture-transaction :key-private-key 2 :key-version 3 :nonce 1) reader))
      (fixture-change-state reader (lambda (db) (fixture-set-key db (fixture-address 1) (fixture-address 2) 0 :revoked t)))
      (is= 1 (length (pool-revalidate pool reader)))
      (is= 0 (pool-count pool)))))

(deftest inline-key-authorization-semantics-and-no-state-leak ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((authorization (fixture-key-authorization))
           (tx (fixture-transaction :key-private-key 2 :key-authorization authorization))
           (root (clempo.store:db-state-root (state-reader-db reader))))
      (submit-transaction pool tx reader)
      (is-equalp root (clempo.store:db-state-root (state-reader-db reader)))
      (rejects :invalid-transaction
        (submit-transaction pool (fixture-transaction :private-key 3 :key-authorization authorization) reader)))))

(deftest key-authorization-token-limits-reject-duplicates ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((limit (list (integer->octets +fixture-token+ 20) 1000000))
           (authorization (fixture-key-authorization :limits (list limit limit))))
      (rejects :invalid-transaction
        (submit-transaction pool (fixture-transaction :key-authorization authorization) reader)))))

(deftest revalidate-mined-nonce-and-fee-insolvency ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (submit-transaction pool (fixture-transaction) reader)
    (submit-transaction pool (fixture-transaction :nonce 1) reader)
    (fixture-change-state reader (lambda (db) (fixture-set-nonce db (fixture-address 1) 0 1)))
    (is= 1 (length (pool-revalidate pool reader)))
    (is= '(1) (selected-nonces pool))
    (fixture-change-state reader (lambda (db) (fixture-fund db (fixture-address 1) 0 1)))
    (is= 1 (length (pool-revalidate pool reader)))
    (is= 0 (pool-count pool))))

(deftest revalidate-paused-fee-token ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (submit-transaction pool (fixture-transaction) reader)
    (fixture-change-state reader (lambda (db) (clempo.store:db-set-storage db +fixture-token+ 12 1)))
    (is= 1 (length (pool-revalidate pool reader)))
    (is= 0 (pool-count pool))))

(deftest aa-cardinality-and-size-limits-before-state-validation ()
  (with-pool (pool reader :max-transaction-bytes 1000000)
    (let ((call (list (integer->octets #x1234 20) 0 (make-octets 0))))
      (rejects :too-many-calls
        (submit-transaction pool (fixture-transaction :calls (make-list 33 :initial-element call)) reader)))
    (rejects :too-many-storage-keys-per-account
      (submit-transaction pool
                          (fixture-transaction :access-list
                                               (list (list (integer->octets #x1234 20)
                                                           (make-list 257 :initial-element (make-octets 32))))) reader))
    (rejects :call-input-too-large
      (submit-transaction pool (fixture-transaction :calls
                                                    (list (list (integer->octets #x1234 20) 0
                                                                (make-octets 131073)))) reader))))

(deftest unavailable-state-protocol-fails-closed ()
  (with-pool (pool reader)
    (declare (ignore reader))
    (signals error (submit-transaction pool (fixture-transaction) (make-instance 'state-reader)))
    (signals type-error (make-state-reader nil nil))
    (is= 0 (pool-count pool))))

(deftest snapshot-context-cannot-silently-change-fork-or-time ()
  (with-pool (pool reader)
    (rejects :state-context (submit-transaction pool (fixture-transaction) reader :timestamp 1001))
    (rejects :state-context (submit-transaction pool (fixture-transaction) reader :fork :t1))
    (is= 0 (pool-count pool))))

(defun set-pool-reserve (db user validator reserve)
  (let ((id (octets->integer
             (clempo.keccak:keccak256
              (octets-concat (integer->octets user 32) (integer->octets validator 32))))))
    (clempo.store:db-set-storage db clempo.txpool.fixtures::+fee-manager+
                                (clempo.txpool.fixtures::mapping-slot id 3) (ash reserve 128))))

(deftest amm-direct-reserve-boundary-and-recent-alternative-token ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((db (state-reader-db reader))
           (validator (1+ +fixture-token+))
           (other (make-state-reader db (state-reader-env reader)
                                     :recent-validator-tokens (list validator)))
           (tx (fixture-transaction)))
      (rejects :insufficient-amm-liquidity (submit-transaction pool tx other))
      (fixture-change-state reader (lambda (db) (set-pool-reserve db +fixture-token+ validator 3987)))
      (rejects :insufficient-amm-liquidity (submit-transaction pool tx other))
      (fixture-change-state reader (lambda (db) (set-pool-reserve db +fixture-token+ validator 3988)))
      (submit-transaction pool tx other)
      (is= (list tx) (pool-best-transactions pool))
      ;; The pool accepts if ANY recent validator can receive this token, not just
      ;; the first recent preference or the current block beneficiary.
      (let ((history (make-state-reader db (state-reader-env reader)
                                       :recent-validator-tokens
                                       (list (+ 3 +fixture-token+) +fixture-token+))))
        (submit-transaction pool (fixture-transaction :private-key 2) history)
        (is= 2 (pool-count pool))))))

(deftest amm-two-hop-fallback-and-canonical-liquidity-revalidation ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((validator (1+ +fixture-token+)) (quote (+ 2 +fixture-token+))
           (routed (make-state-reader (state-reader-db reader) (state-reader-env reader)
                                      :recent-validator-tokens (list validator))))
      (fixture-change-state
       reader (lambda (db)
                (clempo.store:db-set-storage db +fixture-token+ 6 quote)
                (set-pool-reserve db +fixture-token+ quote 3988)
                (set-pool-reserve db quote validator 3975)))
      (rejects :insufficient-amm-liquidity
        (submit-transaction pool (fixture-transaction) routed))
      (fixture-change-state reader (lambda (db) (set-pool-reserve db quote validator 3976)))
      (submit-transaction pool (fixture-transaction) routed)
      (fixture-change-state reader (lambda (db) (set-pool-reserve db +fixture-token+ quote 0)))
      (is= 1 (length (pool-revalidate pool routed)))
      (is= 0 (pool-count pool)))))

(deftest amm-zero-fee-still-needs-nonempty-pool ()
  (let* ((reader (make-fixture-reader :base-fee 0))
         (pool (make-pool :minimum-fee-cap 0))
         (validator (1+ +fixture-token+))
         (routed (make-state-reader (state-reader-db reader) (state-reader-env reader)
                                    :recent-validator-tokens (list validator)))
         (tx (fixture-transaction :fee-cap 0 :priority-fee 0)))
    (rejects :insufficient-amm-liquidity (submit-transaction pool tx routed))
    (fixture-change-state reader (lambda (db) (set-pool-reserve db +fixture-token+ validator 1)))
    (submit-transaction pool tx routed)
    (is= 1 (pool-count pool))))

(deftest non-genesis-validator-history-is-required ()
  (let* ((reader (make-fixture-reader)) (db (state-reader-db reader))
         (env (state-reader-env reader)))
    (fixture-change-state reader (lambda (db) (fixture-set-nonce db (fixture-address 1) 0 1)))
    (rejects :missing-validator-history (make-state-reader db env))
    (rejects :missing-validator-history (make-state-reader db env :recent-validator-tokens nil))
    (is (typep (make-state-reader db env :recent-validator-tokens (list +fixture-token+)) 'state-reader))))

(deftest ethereum-eip7702-admission ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((tx (fixture-transaction :type 4
                                    :authorization-list (list (fixture-eip7702-authorization))))
           (root (clempo.store:db-state-root (state-reader-db reader))))
      (submit-transaction pool tx reader)
      (is= (list tx) (pool-best-transactions pool))
      (is-equalp root (clempo.store:db-state-root (state-reader-db reader))))))

(deftest recorded-tempo-root-signature-and-create-validated-not-executed ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (let* ((reader (make-fixture-reader :chain-id 42431))
         (pool (make-pool)) (tx (recorded-transaction "tempo/secp256k1"))
         (sender (clempo.transactions:transaction-sender tx)))
    (fixture-change-state reader (lambda (db) (fixture-fund db sender)))
    (let ((root (clempo.store:db-state-root (state-reader-db reader))))
      (submit-transaction pool tx reader)
      (is= sender (pool-transaction-sender pool tx))
      (is-equalp root (clempo.store:db-state-root (state-reader-db reader))))))

(deftest access-key-capacity-counts-across-unrelated-account-lanes ()
  (skip-test "needs executor T6 keychain handling")
  (with-pool (pool reader :max-transactions-per-sender 2)
    (fixture-change-state
     reader (lambda (db)
              (dolist (account '(1 3 4))
                (fixture-set-key db (fixture-address account) (fixture-address 2) 2000))))
    (submit-transaction pool (fixture-transaction :private-key 1 :key-private-key 2) reader)
    (submit-transaction pool (fixture-transaction :private-key 3 :key-private-key 2) reader)
    (rejects :sender-capacity
      (submit-transaction pool (fixture-transaction :private-key 4 :key-private-key 2) reader))
    (is= 2 (pool-count pool))))

(deftest replacement-with-new-saturated-sponsor-preserves-old ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader :max-transactions-per-sender 2)
    (submit-transaction pool (fixture-transaction :private-key 2 :sponsor-private-key 4) reader)
    (submit-transaction pool (fixture-transaction :private-key 3 :sponsor-private-key 4) reader)
    (let ((old (fixture-transaction)))
      (submit-transaction pool old reader)
      (rejects :sender-capacity
        (submit-transaction pool (fixture-transaction :sponsor-private-key 4
                                                     :fee-cap 2200000000
                                                     :priority-fee 1100000000) reader))
      (is (member old (pool-best-transactions pool) :test #'eq))
      (is= 3 (pool-count pool)))))

(deftest pool-only-key-expiry-propagation-buffer ()
  (skip-test "needs executor T6 keychain handling")
  (with-pool (pool reader)
    (fixture-change-state
     reader (lambda (db) (fixture-set-key db (fixture-address 1) (fixture-address 2) 1003)))
    (rejects :access-key-expired
      (submit-transaction pool (fixture-transaction :key-private-key 2) reader))))

(deftest aa-aggregate-access-and-authorization-cardinality ()
  (with-pool (pool reader :max-transaction-bytes 1000000)
    (rejects :too-many-access-list-accounts
      (submit-transaction pool
                          (fixture-transaction :access-list
                                               (make-list 257 :initial-element
                                                          (list (integer->octets #x1234 20) nil))) reader))
    (rejects :too-many-storage-keys
      (submit-transaction pool
                          (fixture-transaction
                           :access-list (loop for i below 9
                                              collect (list (integer->octets (+ #x1234 i) 20)
                                                            (make-list 256 :initial-element (make-octets 32)))))
                          reader))
    (let ((auth (list 4217 (integer->octets #x5678 20) 0 (make-octets 65))))
      (rejects :too-many-authorizations
        (submit-transaction pool (fixture-transaction :tempo-authorizations
                                                     (make-list 17 :initial-element auth)) reader)))
    (let ((limit (list (integer->octets +fixture-token+ 20) 1000000)))
      (rejects :too-many-token-limits
        (submit-transaction pool
                            (fixture-transaction :key-authorization
                                                 (fixture-key-authorization
                                                  :limits (make-list 257 :initial-element limit))) reader)))))

(deftest validated-fee-metadata-is-cached-for-builder ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let ((tx (fixture-transaction :sponsor-private-key 2)))
      (submit-transaction pool tx reader)
      (is= 4000 (pool-transaction-fee-cost pool tx))
      (is= (cons +fixture-token+
                 (clempo.txpool.fixtures::mapping-slot (fixture-address 2) 9))
           (pool-transaction-fee-balance-slot pool tx))
      (is (not (pool-transaction-fallback-fee-token-p pool tx))))))

(deftest intrinsic-gas-invalid-tip-and-oversized-data ()
  (with-pool (pool reader)
    (rejects :invalid-transaction
      (submit-transaction pool (fixture-transaction :gas-limit 1) reader))
    (rejects :invalid-transaction
      (submit-transaction pool (fixture-transaction :priority-fee 2000000001) reader))
    (let ((small (make-pool :max-transaction-bytes 1)))
      (rejects :oversized-transaction (submit-transaction small (fixture-transaction) reader)))))

(deftest priority-saturates-u64-and-fifo-remains-deterministic ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (fixture-change-state reader
                          (lambda (db)
                            (fixture-fund db (fixture-address 1) (ash 1 100))
                            (fixture-fund db (fixture-address 2) (ash 1 100))))
    (let* ((fee (ash 1 80))
           (first (fixture-transaction :fee-cap (* 3 fee) :priority-fee fee))
           (second (fixture-transaction :private-key 2 :fee-cap (* 3 fee) :priority-fee (* 2 fee))))
      (submit-transaction pool first reader)
      (submit-transaction pool second reader)
      (is= (list first second) (pool-best-transactions pool)))))

(deftest two-hop-liquidity-is-fork-gated ()
  (let* ((reader (make-fixture-reader :fork :t4))
         (pool (make-pool)) (validator (1+ +fixture-token+)) (quote (+ 2 +fixture-token+))
         (routed (make-state-reader (state-reader-db reader) (state-reader-env reader)
                                    :recent-validator-tokens (list validator))))
    (fixture-change-state
     reader (lambda (db)
              (clempo.store:db-set-storage db +fixture-token+ 6 quote)
              (set-pool-reserve db +fixture-token+ quote 100000)
              (set-pool-reserve db quote validator 100000)))
    (rejects :insufficient-amm-liquidity (submit-transaction pool (fixture-transaction) routed))))

(deftest key-authorization-scope-cardinalities ()
  (with-pool (pool reader :max-transaction-bytes 1000000)
    (let* ((selector (make-octets 4))
           (rule (list selector nil))
           (target (integer->octets #x1234 20))
           (scope (list target (list rule))))
      (labels ((grant (scopes)
                 (let ((fields (list 4217 0 (integer->octets (fixture-address 2) 20)
                                     2000 (make-octets 0) scopes)))
                   (list fields (clempo.txpool.fixtures::signature-bytes
                                 (clempo.keccak:keccak256 (clempo.rlp:rlp-encode fields)) 1)))))
        (rejects :too-many-call-scopes
          (submit-transaction pool (fixture-transaction :key-authorization
                                                       (grant (make-list 65 :initial-element scope))) reader))
        (rejects :too-many-selector-rules
          (submit-transaction pool (fixture-transaction :key-authorization
                                                       (grant (list (list target
                                                                          (make-list 65 :initial-element rule))))) reader))
        (rejects :too-many-recipients
          (submit-transaction pool
                              (fixture-transaction :key-authorization
                                                   (grant (list (list target
                                                                      (list (list selector
                                                                                  (make-list 65 :initial-element target)))))))
                              reader))))))

(deftest invalid-signatures-rejected-before-state-check ()
  (with-pool (pool reader)
    (let* ((tx (fixture-transaction))
           (fields (copy-list (clempo.transactions:transaction-fields tx))))
      (setf (car (last fields)) (make-octets 65))
      (rejects :invalid-signature
        (submit-transaction pool
                            (clempo.transactions:decode-transaction
                             (clempo.txpool.fixtures::envelope #x76 fields)) reader))
      (is= 0 (pool-count pool)))))

(deftest pre-t1-all-ones-nonce-key-is-still-sequential ()
  (let* ((reader (make-fixture-reader :fork :t0)) (pool (make-pool))
         (key clempo.txpool::+expiring-nonce-key+)
         (tx0 (fixture-transaction :nonce-key key))
         (tx1 (fixture-transaction :nonce-key key :nonce 1)))
    (submit-transaction pool tx1 reader)
    (is (null (pool-best-transactions pool)))
    (submit-transaction pool tx0 reader)
    (is= (cons (fixture-address 1) key) (pool-transaction-lane pool tx0))
    (is= '(0 1) (selected-nonces pool))))

(deftest fallback-fee-selection-metadata-not-confused-with-user-preference ()
  (skip-test "needs executor T7 non-creditable fee slots")
  (with-pool (pool reader)
    (let* ((path-usd #x20c0000000000000000000000000000000000000)
           (sender (fixture-address 1))
           (db (state-reader-db reader)))
      ;; Explicit synthetic token initialization in the real database. USD short-string
      ;; encoding is copied from the recorded token, not assumed by the state reader.
      (fixture-change-state
       reader (lambda (db)
                (clempo.store:db-set-account db path-usd 0 0
                                            (clempo.store:db-put-code db (octets-of #xef)))
                (clempo.store:db-set-storage db path-usd 4 (clempo.store:db-storage db +fixture-token+ 4))
                (clempo.store:db-set-storage db path-usd 7 (ash 1 160))
                (clempo.store:db-set-storage db path-usd
                                            (clempo.txpool.fixtures::mapping-slot sender 9)
                                            1000000000000000000)
                (clempo.store:db-set-storage db clempo.txpool.fixtures::+fee-manager+
                                            (clempo.txpool.fixtures::mapping-slot sender 1) 0)
                (clempo.store:db-set-storage db clempo.txpool.fixtures::+fee-manager+
                                            (clempo.txpool.fixtures::mapping-slot 1 0) path-usd)))
      (let* ((snapshot (make-state-reader db (state-reader-env reader)
                                         :recent-validator-tokens (list path-usd)))
             (tx (fixture-transaction :fee-token nil)))
        (submit-transaction pool tx snapshot)
        (is (pool-transaction-fallback-fee-token-p pool tx))
        (is= path-usd (car (pool-transaction-fee-balance-slot pool tx)))))))

(deftest malformed-key-authorization-scopes-rejected-before-state ()
  (with-pool (pool reader)
    (let ((empty (make-octets 0)) (address (integer->octets 7 20)))
      (dolist (scope (list (octets-of 1)
                           (list address)
                           (list address (octets-of 1))
                           (list address (list (octets-of 1)))
                           (list address (list (list (make-octets 4) (octets-of 1))))))
        (let ((authorization
                (list (list 4217 0 address empty empty (list scope)) (make-octets 65))))
          (rejects :invalid-key-authorization
            (submit-transaction pool (fixture-transaction :key-authorization authorization)
                                reader))))
      (is= 0 (pool-count pool))
      (is (not (clempo.store:db-block-open-p (state-reader-db reader)))))))
