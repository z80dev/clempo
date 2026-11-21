(defpackage #:clempo.rpc.simulate
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state #:clempo.rpc.calls)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:h #:clempo.headers)
                    (#:executor #:clempo.executor))
  (:documentation "eth_simulateV1 and tempo_simulateV1: requests executed in simulated blocks on
top of a block's state (reth's simulate.rs over the executor's overlay call blocks), rendered
as reth renders a SimulatedBlock."))

(in-package #:clempo.rpc.simulate)

(defparameter *max-simulate-blocks* 256 "reth max_simulate_blocks default")

(defun simulate (payload block what)
  (when (> (length (getf payload :block-state-calls)) *max-simulate-blocks*)
    (rpc-fail -38026 "too many blocks"))
  (when (null (getf payload :block-state-calls)) (rpc-fail +invalid-params+ "calls are empty."))
  (let* ((n (block-id-number (or block :latest)))
         (b (and n (<= n chain:*head*) (chain-block n))))
    (unless b (rpc-fail -38020 (format nil "block not found: ~A" (block-id-text (or block :latest)))))
    (let ((state (state-at (list :hash (cblock-hash b)))))
      ;; The first call's request errors come before any block is rendered.
      (let* ((first-block (first (getf payload :block-state-calls)))
             (call (first (getf first-block :calls))))
        (when (and call (not (getf payload :validation)) (not (aa-request-p call)))
          (run-request b state call what
                       :state-overrides (getf first-block :state-overrides)
                       :on-invalid (lambda (code message)
                                     (declare (ignore code))
                                     (let ((e (find message +invalid-transaction-errors+ :key #'third :test #'string=)))
                                       (rpc-fail (fourth e) message))))))
      (internal-error (format nil "~A: rendering simulated blocks is not implemented yet" what)))))

(defrpc "eth_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "execute calls in simulated blocks on top of BLOCK"
  (simulate payload block "eth_simulateV1"))

(defrpc "tempo_simulateV1" ((payload simulate-payload) (block block-id :optional t))
  "eth_simulateV1 with the metadata of the TIP-20 tokens involved"
  (simulate payload block "tempo_simulateV1"))
