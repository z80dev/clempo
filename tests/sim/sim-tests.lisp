(defpackage #:clempo.sim.tests
  (:use #:cl #:clempo.base #:clempo.sim))

(in-package #:clempo.sim.tests)

(deftest sim-smoke-4-finalizes-and-certifies ()
  ;; 4 validators, 2 epochs, no faults: both reshares conclude through the DKG managers, the
  ;; chain reaches the second epoch's boundary and the mainnet certificate verifier accepts
  ;; every block.
  (let ((r (run-scenario "smoke-4")))
    (is= :done (sim-result-summary r))
    (is (>= (sim-result-finalized-height r) 23))
    (is= '((0 . t) (1 . t)) (sim-result-ceremonies r))
    (let ((c (verify-sim-chain r)))
      (is= (sim-result-finalized-height r) (clempo.consensus:certification-verified c))
      (is= '(0 1) (mapcar #'first (clempo.consensus:certification-epochs c))))
    ;; the reshares kept the group key
    (is (every (lambda (i) (equalp i (sim-result-identity r))) (sim-result-identities r)))))
