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

;;; Epochs (commonware FixedEpocher): epoch e holds heights [e*L, (e+1)*L - 1]; its last
;;; block, the boundary, carries the outcome for epoch e+1.

(defun height-epoch (height epoch-length) (floor height epoch-length))
(defun epoch-first-height (epoch epoch-length) (* epoch epoch-length))
(defun epoch-last-height (epoch epoch-length) (1- (* (1+ epoch) epoch-length)))
(defun boundary-height-p (height epoch-length) (= (mod (1+ height) epoch-length) 0))

;;; Dealer logs in extraData.

(defun extra-data-dealer-log (octets info &key strict)
  "read_dealer_log: a non-boundary block's extraData OCTETS as a SignedDealerLog of the round
INFO, read with max_players = the round's player count and checked against the round.
STRICT (from T15) rejects trailing bytes. Returns (values dealer log); signals DKG-ERROR
:invalid-dealer-log when the bytes do not decode or the signature does not check."
  (let ((signed (handler-case (decode-signed-dealer-log octets (length (info-players info))
                                                        :strict strict)
                  (cw-error (e) (dkg-fail :invalid-dealer-log "could not decode: ~A" e)))))
    (multiple-value-bind (dealer log) (check-signed-dealer-log signed info)
      (unless dealer
        (dkg-fail :invalid-dealer-log "failed checking signed log against the round"))
      (values dealer log))))

(defun collect-dealer-logs (info extra-datas)
  "The ceremony logs of round INFO from the extraData of an epoch's finalized blocks, in
height order. Each item is octets, or (octets . strict) with strict true from T15. Empty or
invalid extraData is skipped, and a dealer's first log is kept (record_finalized_header,
append_dealer_log)."
  (let ((logs (make-logs info)))
    (dolist (item extra-datas logs)
      (destructuring-bind (octets . strict) (if (consp item) item (cons item nil))
        (when (plusp (length octets))
          (handler-case
              (multiple-value-bind (dealer log) (extra-data-dealer-log octets info :strict strict)
                (unless (assoc dealer (logs-entries logs) :test #'octets=)
                  (logs-record logs dealer log)))
            (dkg-error () nil)))))))

;;; Concluding a ceremony and the boundary outcome.

(defun conclude-ceremony (logs previous-output &key player)
  "The actor's conclude_ceremony: (values output share succeeded). With PLAYER, finalize its
share; when its local state cannot give one, observe instead. When the logs cannot give an
output, PREVIOUS-OUTPUT carries forward (and SUCCEEDED is NIL)."
  (flet ((observed ()
           (handler-case (values (observe logs) nil t)
             (dkg-failure () (values previous-output nil nil)))))
    (if player
        (handler-case (multiple-value-bind (output share) (finalize-player player logs)
                        (values output share t))
          (dkg-failure () (values previous-output nil nil))
          (dkg-error () (observed)))
        (observed))))

(defun boundary-outcome (output next-players &key epoch next-full-dkg-epoch)
  "assemble_boundary_outcome: the OnchainDkgOutcome a boundary block of epoch EPOCH - 1
carries: the ceremony OUTPUT, the NEXT-PLAYERS (the active validators at the parent, a sorted
key list), and whether EPOCH's ceremony is a full DKG (NEXT-FULL-DKG-EPOCH = EPOCH)."
  (make-onchain-dkg-outcome :epoch epoch :output output :next-players next-players
                            :is-next-full-dkg (eql next-full-dkg-epoch epoch)))
