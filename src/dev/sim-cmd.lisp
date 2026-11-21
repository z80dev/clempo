(in-package #:clempo.dev)

;;; R6: the validator simulation.

(defun sim-run-json (name)
  "Run scenario NAME and certify its chain; a JSON object with the result."
  (let* ((t0 (clempo.io:monotonic-ticks))
         (r (clempo.sim:run-scenario name))
         (c (handler-case (clempo.sim:verify-sim-chain r)
              (clempo.consensus:certificate-error (e)
                (log-line "sim ~A: certificate verifier rejected the chain: ~A" name e)
                nil)))
         (secs (/ (- (clempo.io:monotonic-ticks) t0) (float internal-time-units-per-second)))
         (ceremonies (clempo.sim:sim-result-ceremonies r))
         (concluded (count-if #'cdr ceremonies))
         ;; a pass needs 3 epochs, a certified chain and every reshare concluded
         (ok (and c (eq (clempo.sim:sim-result-summary r) :done)
                  (>= (clempo.sim:sim-result-epochs r) 3)
                  (>= (length ceremonies) 3)
                  (= concluded (length ceremonies)))))
    (log-line "sim ~A: ~A, finalized height ~D (~D epochs), ~D certificates verified, ~D messages (~D dropped), ~,1Fs simulated in ~,1Fs"
              name (clempo.sim:sim-result-summary r) (clempo.sim:sim-result-finalized-height r)
              (clempo.sim:sim-result-epochs r)
              (if c (clempo.consensus:certification-certificates c) 0)
              (clempo.sim:sim-result-messages r) (clempo.sim:sim-result-dropped r)
              (/ (clempo.sim:sim-result-ms r) 1000.0) secs)
    (values (clempo.json:jobject
             "scenario" name "ok" (clempo.json:jbool ok)
             "height" (clempo.sim:sim-result-finalized-height r)
             "epochs" (clempo.sim:sim-result-epochs r)
             "certificates" (if c (clempo.consensus:certification-certificates c) 0)
             "ceremonies" (format nil "~D/~D" concluded (length ceremonies))
             "proposers" (coerce (clempo.sim:sim-result-proposers r) 'simple-vector)
             "messages" (clempo.sim:sim-result-messages r)
             "dropped" (clempo.sim:sim-result-dropped r)
             "sim_ms" (clempo.sim:sim-result-ms r)
             "seconds" (round secs))
            ok)))

(defparameter *r6-scenarios* '("r6-4" "r6-5" "r6-7"))

(defrung "R6" (args)
  "simplex + DKG reshare: 4, 5 and 7 seeded validators with faults, 3 epochs each, chains certified"
  (let* ((runs (loop for name in *r6-scenarios*
                     collect (multiple-value-list (sim-run-json name))))
         (passed (count-if #'second runs)))
    (clempo.json:jobject "score" (if (= passed (length runs)) "sim yes" (format nil "sim ~D/~D" passed (length runs)))
                         "runs" (coerce (mapcar #'first runs) 'simple-vector))))

(defcommand "sim" (args)
  "run a validator simulation: sim [-scenario r6-4] (sim -list to list)"
  (if (arg-flag args :list)
      (progn (format t "~{~A~%~}" (clempo.sim:scenario-names)) 0)
      (multiple-value-bind (json ok) (sim-run-json (arg args :scenario "r6-4"))
        (write-line (clempo.json:to-json-string json))
        (if ok 0 1))))
