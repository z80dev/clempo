(in-package #:clempo.dkg.tests)

;;; In-memory ceremonies: a fresh DKG, then reshares, every dealer and player in Lisp.

(defun make-seeds (n)
  "N ed25519 seeds, returned sorted by their public keys (the order of a key Set)."
  (sort (loop repeat n collect (clempo.io:random-octets 32))
        #'cw-order< :key #'clempo.ed25519:ed25519-public-key))

(defun seed-key (seed) (clempo.ed25519:ed25519-public-key seed))

(defun run-ceremony (info dealer-seeds player-seeds &key shares (drop (constantly nil))
                                                         corrupt)
  "Run one round. SHARES: ((public-key . share) ...) of the previous round. DROP (dealer
index, player index) loses that dealing, so the dealer reveals it. CORRUPT: a dealer index
whose dealings are all wrong (its players do not ack). Returns (values output shares logs)."
  (let ((players (mapcar (lambda (s) (make-player info s)) player-seeds))
        (logs (make-logs info)))
    (loop for seed in dealer-seeds
          for d from 0
          for me = (seed-key seed)
          do (multiple-value-bind (dealer pub-msg priv-msgs)
                 (start-dealer info seed
                               :share (cdr (assoc me shares :test #'octets=)))
               (loop for player in players
                     for p from 0
                     for priv = (cdr (assoc (seed-key (nth p player-seeds)) priv-msgs
                                            :test #'octets=))
                     unless (funcall drop d p)
                       do (if (eql corrupt d)
                              (signals dkg-error
                                (player-dealer-message player me pub-msg
                                                       (clempo.bls12-381:fr-add
                                                        priv (clempo.bls12-381:fr-octets-from-integer 1))))
                              (dealer-receive-player-ack
                               dealer (seed-key (nth p player-seeds))
                               (player-dealer-message player me pub-msg priv))))
               (let ((signed (finalize-dealer dealer)))
                 ;; through the wire format, as a block's extraData would carry it
                 (multiple-value-bind (dealer-key log)
                     (check-signed-dealer-log
                      (decode-signed-dealer-log (cw-encode 'signed-dealer-log signed)
                                                (length player-seeds) :strict t)
                      info)
                   (is (octets= me dealer-key))
                   (logs-record logs dealer-key log)))))
    (logs-pre-verify logs)
    (let ((output (observe logs))
          (new-shares '()))
      (loop for player in players
            for seed in player-seeds
            do (multiple-value-bind (player-output share) (finalize-player player logs)
                 (is (equalp output player-output))
                 (push (cons (seed-key seed) share) new-shares)))
      (values output (nreverse new-shares) logs))))

(defun check-shares (output shares)
  "Every share matches its partial public key; threshold signatures from two different
quorums recover the same signature, valid under the group key."
  (let* ((sharing (output-public output))
         (n (sharing-total sharing))
         (threshold (sharing-threshold sharing))
         (message (string->octets "clempo dkg ceremony"))
         (namespace (string->octets "TEMPO")))
    (is= (output-quorum output) threshold)
    (loop for (nil . share) in shares
          do (is (octets= (sharing-partial-public sharing (share-index share))
                          (clempo.bls12-381:bls-public-key :min-sig (share-private share)))))
    (flet ((recover (some)
             (clempo.bls12-381:bls-threshold-recover
              :min-sig threshold n
              (mapcar (lambda (entry)
                        (let ((share (cdr entry)))
                          (clempo.bls12-381:bls-threshold-sign
                           :min-sig (share-private share) (share-index share) n namespace message)))
                      some))))
      (let ((a (recover (subseq shares 0 threshold)))
            (b (recover (subseq shares (- (length shares) threshold)))))
        (is (octets= a b))
        (is (clempo.bls12-381:bls-verify :min-sig (sharing-public sharing) message a
                                         :namespace namespace))))))

(defun ceremony-sequence (n)
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy (+ 1000 n)))
         (seeds (make-seeds n))
         (keys (mapcar #'seed-key seeds))
         (info (make-round-info (string->octets "TEMPO") 0 :dealers keys :players keys)))
    ;; fresh DKG, one dealing lost (revealed) for player 0 from dealer 1
    (multiple-value-bind (output shares)
        (run-ceremony info seeds seeds :drop (lambda (d p) (and (= d 1) (= p 0))))
      (check-shares output shares)
      (let ((identity (sharing-public (output-public output))))
        ;; two reshares; the second with a corrupt dealer (its log reveals everything,
        ;; too many reveals, so it is omitted) and the selection works around it
        (loop for round from 1 to 2
              do (let ((info (make-round-info (string->octets "TEMPO") round
                                              :previous output :dealers keys :players keys)))
                   (multiple-value-setq (output shares)
                     (run-ceremony info seeds seeds :shares shares
                                                    :corrupt (and (= round 2) 0)))
                   (is (octets= identity (sharing-public (output-public output))))
                   (is= (quorum-of n) (length (output-dealers output)))
                   (check-shares output shares)))))))

(defun quorum-of (n) (- n (floor (1- n) 3)))

(deftest dkg-ceremony-4 () (ceremony-sequence 4))
(deftest dkg-ceremony-7 () (ceremony-sequence 7))

(deftest dkg-reshare-to-new-players ()
  ;; 4 validators reshare to a set of 7 (3 joining): the identity carries over and the new
  ;; players' shares work.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 77))
         (old (make-seeds 4))
         (old-keys (mapcar #'seed-key old))
         (joining (make-seeds 3))
         (new (sort (append old joining) #'cw-order< :key #'seed-key))
         (new-keys (mapcar #'seed-key new)))
    (multiple-value-bind (output shares) (deal old-keys)
      (let ((info (make-round-info (string->octets "TEMPO") 5 :previous output
                                   :dealers old-keys :players new-keys)))
        (multiple-value-bind (next next-shares) (run-ceremony info old new :shares shares)
          (is (octets= (sharing-public (output-public output))
                       (sharing-public (output-public next))))
          (is= 7 (sharing-total (output-public next)))
          (check-shares next next-shares))))))

(deftest dkg-insufficient-logs ()
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 5))
         (seeds (make-seeds 4))
         (keys (mapcar #'seed-key seeds))
         (info (make-round-info (string->octets "TEMPO") 0 :dealers keys :players keys))
         (logs (make-logs info)))
    ;; only two of the three required dealers publish
    (dolist (seed (subseq seeds 0 2))
      (multiple-value-bind (dealer) (start-dealer info seed)
        (multiple-value-bind (d log) (check-signed-dealer-log (finalize-dealer dealer) info)
          (logs-record logs d log))))
    (handler-case (progn (observe logs) (is nil))
      (dkg-failure (f)
        (is= 3 (dkg-failure-required f))
        ;; every dealing was revealed (no acks): 4 reveals > 1 allowed, so omitted
        (is= 0 (dkg-failure-found f))
        (is= 4 (length (dkg-failure-unavailable f)))))
    ;; a log signed for another round does not check
    (let ((other (make-round-info (string->octets "TEMPO") 1 :dealers keys :players keys)))
      (is (null (check-signed-dealer-log (finalize-dealer (start-dealer other (first seeds)))
                                         info))))))

(deftest dkg-root-of-unity ()
  ;; group.rs ROOT_OF_UNITY is 7^((r-1)/2^32), a primitive 2^32-th root of unity
  (is= (clempo.dkg::fr-expt 7 (ash (1- +group-order+) -32)) clempo.dkg::*root-of-unity*)
  (is= 1 (clempo.dkg::fr-expt clempo.dkg::*root-of-unity* (ash 1 32)))
  (is (/= 1 (clempo.dkg::fr-expt clempo.dkg::*root-of-unity* (ash 1 31))))
  (is= 4 (clempo.dkg::mode-scalar :nonzero-counter 7 3))
  (is (null (clempo.dkg::mode-scalar :nonzero-counter 7 7))))
