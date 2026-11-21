(in-package #:clempo.dkg.tests)

(deftest dkg-tempo-epochs ()
  (is= 2041 (height-epoch 44107199 21600))
  (is= 2042 (height-epoch 44107200 21600))
  (is= 44107199 (epoch-last-height 2041 21600))
  (is= 44085600 (epoch-first-height 2041 21600))
  (is (boundary-height-p 44107199 21600))
  (is (not (boundary-height-p 44107200 21600))))

(deftest dkg-tempo-read-dealer-log ()
  ;; Rust's signed logs as extraData of the round they were made for
  (let* ((round (second (rust-rounds)))
         (info (rust-round-info round)))
    (loop for m across (clempo.json:jget round "messages")
          for h = (clempo.json:jget m "signed_log")
          when h
            do (let ((octets (hex->octets h))
                     (dealer (nth (clempo.json:jget m "dealer") (rust-keys))))
                 (is (octets= dealer (extra-data-dealer-log octets info :strict t)))
                 ;; before T15 trailing bytes are ignored; from T15 they are invalid
                 (let ((longer (octets-concat octets (octets-of 0))))
                   (is (octets= dealer (extra-data-dealer-log longer info)))
                   (signals dkg-error (extra-data-dealer-log longer info :strict t)))
                 ;; the wrong round, and garbage
                 (signals dkg-error (extra-data-dealer-log octets (rust-round-info
                                                             (first (rust-rounds)))))
                 (signals dkg-error (extra-data-dealer-log (subseq octets 0 100) info))))))

(defun epoch-extra-data (info dealer-seeds player-seeds shares)
  "The extraData of an epoch's blocks when every dealer's log is finalized: empty blocks,
noise, each signed log (one twice, one with trailing bytes)."
  (let ((players (mapcar (lambda (s) (make-player info s)) player-seeds))
        (blocks (list (make-octets 0) (octets-of 1 2 3))))
    (dolist (seed dealer-seeds)
      (multiple-value-bind (dealer pub-msg priv-msgs)
          (start-dealer info seed :share (cdr (assoc (seed-key seed) shares :test #'octets=)))
        (loop for player in players
              for key = (seed-key (clempo.dkg::player-seed player))
              do (dealer-receive-player-ack
                  dealer key (player-dealer-message player (seed-key seed) pub-msg
                                                    (cdr (assoc key priv-msgs :test #'octets=)))))
        (let ((octets (cw-encode 'signed-dealer-log (finalize-dealer dealer))))
          (push octets blocks)
          (push (make-octets 0) blocks)
          (push (cons (octets-concat octets (octets-of 9)) nil) blocks))))
    (values (nreverse blocks) players)))

(deftest dkg-tempo-epoch-chain ()
  ;; Genesis deals to 4 validators; epochs 1 and 2 reshare (identity kept), epoch 2's
  ;; outcome schedules a full DKG for epoch 3, whose ceremony makes a new identity. Each
  ;; boundary outcome goes through its wire encoding.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 2026))
         (seeds (make-seeds 4))
         (keys (mapcar #'seed-key seeds))
         (outcome nil)
         (shares nil))
    (multiple-value-bind (output dealt) (deal keys)
      (setf outcome (boundary-outcome output keys :epoch 1 :next-full-dkg-epoch 0)
            shares dealt))
    (let ((identity (outcome-network-identity outcome)))
      (loop for epoch from 1 to 3
            do (let* ((info (round-info-from-outcome
                             (decode-onchain-dkg-outcome (encode-onchain-dkg-outcome outcome))
                             :v1-active t)))
                 (is= epoch (clempo.dkg::info-round info))
                 (multiple-value-bind (blocks players)
                     (epoch-extra-data info seeds seeds shares)
                   (let ((logs (collect-dealer-logs info blocks))
                         (new-shares '()) (output nil))
                     (is= 4 (length (clempo.dkg::logs-entries logs)))
                     (dolist (player players)
                       (multiple-value-bind (o share ok)
                           (conclude-ceremony logs (outcome-output outcome) :player player)
                         (is ok)
                         (when output (is (equalp output o)))
                         (setf output o)
                         (push (cons (seed-key (clempo.dkg::player-seed player)) share)
                               new-shares)))
                     ;; an observer agrees
                     (is (equalp output (conclude-ceremony logs (outcome-output outcome))))
                     (setf shares (nreverse new-shares)
                           outcome (boundary-outcome output keys :epoch (1+ epoch)
                                                                 :next-full-dkg-epoch 3))
                     (check-shares output shares)
                     (if (< epoch 3)
                         (is (octets= identity (outcome-network-identity outcome)))
                         (is (not (octets= identity (outcome-network-identity outcome)))))
                     (is (eq (= epoch 2) (outcome-is-next-full-dkg outcome))))))))))

(deftest dkg-tempo-failed-ceremony-carries-forward ()
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 9))
         (seeds (make-seeds 4))
         (keys (mapcar #'seed-key seeds)))
    (multiple-value-bind (output) (deal keys)
      (let* ((outcome (boundary-outcome output keys :epoch 4))
             (info (round-info-from-outcome outcome :v1-active t))
             (logs (collect-dealer-logs info '())))
        (multiple-value-bind (o share ok) (conclude-ceremony logs output)
          (is (eq output o))
          (is (null share))
          (is (not ok)))))))
