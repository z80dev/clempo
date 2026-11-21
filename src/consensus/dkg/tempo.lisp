;;; Tempo's use of the DKG (consensus/src/dkg/manager/actor): one ceremony per epoch, its
;;; round built from the previous boundary block's outcome, dealer logs carried in the
;;; extraData of the epoch's blocks, the next outcome in the epoch's last block.

(in-package #:clempo.dkg)

(define-constant +tempo-namespace+ (string->octets "TEMPO")
  "consensus/src/config.rs NAMESPACE.")

(defun round-info-from-outcome (outcome &key (namespace +tempo-namespace+) v1-active)
  "Round::from_state for the state a boundary OUTCOME installs: the ceremony run during
epoch (outcome-epoch OUTCOME). It reshares the outcome's output unless the outcome scheduled
a full DKG; the dealers are the output's players and the players are the next players.
V1-ACTIVE (T12 active at the timestamp of the boundary block holding OUTCOME) selects
Reveal::V1, otherwise Reveal::V0."
  (let ((output (outcome-output outcome)))
    (make-round-info namespace (outcome-epoch outcome)
                     :previous (unless (outcome-is-next-full-dkg outcome) output)
                     :mode :nonzero-counter
                     :reveal (if v1-active :v1 :v0)
                     :dealers (output-players output)
                     :players (outcome-next-players outcome))))
