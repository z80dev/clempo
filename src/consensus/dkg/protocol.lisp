;;; The Feldman/Desmedt protocol (feldman_desmedt.rs): dealers, players, logs, selection,
;;; observation, reshare by Lagrange interpolation, and the trusted dealer.
;;;
;;; Dealers and players hold their ed25519 signing key as a 32-byte seed. Randomness (dealer
;;; polynomials, the trusted dealer) comes from clempo.io:*entropy*.

(in-package #:clempo.dkg)

(define-constant +sig-ack+ (string->octets "ack"))
(define-constant +sig-log+ (string->octets "log"))

(define-condition dkg-failure (dkg-error)
  ((required :initarg :required :reader dkg-failure-required)
   (found :initarg :found :reader dkg-failure-found)
   (faults :initarg :faults :reader dkg-failure-faults
           :documentation "((dealer . fault-reason) ...) for dealers whose logs prove a fault")
   (unavailable :initarg :unavailable :reader dkg-failure-unavailable
                :documentation "configured dealers with no usable log"))
  (:default-initargs :reason :insufficient-logs)
  (:report (lambda (c s)
             (format s "dkg: insufficient usable dealer logs: required ~D, found ~D, ~D faulty, ~D unavailable"
                     (dkg-failure-required c) (dkg-failure-found c)
                     (length (dkg-failure-faults c)) (length (dkg-failure-unavailable c)))))
  (:documentation "Failure::InsufficientLogs: the round cannot produce an output."))

;;; Transcripts of a round.

(defun round-transcript (info) (resume-transcript (info-summary info) :v0))

(defun ack-transcript (round-transcript dealer pub-msg)
  (let ((tr (transcript-fork round-transcript +sig-ack+)))
    (transcript-commit tr dealer)
    (transcript-commit tr (cw-encode 'dealer-pub-msg pub-msg))))

(defun log-transcript (info log)
  (transcript-commit (transcript-fork (round-transcript info) +sig-log+)
                     (cw-encode 'dealer-log log)))

;;; Checks shared by players and observers.

(defun previous-share-commitment (info dealer)
  "The dealer's public share of the previous output (Output::share_commitment)."
  (let* ((previous (info-previous info))
         (index (key-index dealer (output-players previous))))
    (invariant index "Info validates that dealers are previous players")
    (sharing-partial-public (output-public previous) index)))

(defun dealer-pub-msg-problem (info dealer pub-msg)
  "Info::check_dealer_pub_msg: NIL, or the reason PUB-MSG is unacceptable from DEALER."
  (let ((commitment (dealer-pub-msg-commitment pub-msg)))
    (cond ((/= (commitment-degree commitment) (info-degree info))
           :invalid-commitment-degree)
          ((and (info-previous info)
                (not (octets= (first commitment) (previous-share-commitment info dealer))))
           :mismatched-reshare-commitment))))

(defun dealing-matches-p (pub-msg x share)
  "commitment(X) = g * SHARE."
  (equal (commitment-eval (dealer-pub-msg-commitment pub-msg) x) (g2-base-mul share)))

(defun log-results (log) (let ((r (dealer-log-results log))) (and (consp r) (cdr r))))

(defun check-dealer-log (info dealer log)
  "Info::check_dealer_log: :available, :unavailable (too many reveals, omitted), or
\(:fault . reason), or :unexpected-dealer."
  (unless (info-dealer-index info dealer) (return-from check-dealer-log :unexpected-dealer))
  (let ((problem (dealer-pub-msg-problem info dealer (dealer-log-pub-msg log))))
    (when problem (return-from check-dealer-log (cons :fault problem))))
  (when (eq (dealer-log-results log) :too-many-reveals)
    (return-from check-dealer-log :unavailable))
  (let ((results (log-results log)))
    (unless (and (= (length results) (length (info-players info)))
                 (every (lambda (r p) (octets= (car r) p)) results (info-players info)))
      (return-from check-dealer-log '(:fault . :mismatched-log-players)))
    (let ((summary (transcript-summarize (ack-transcript (round-transcript info) dealer
                                                         (dealer-log-pub-msg log))))
          (reveals (remove :ack results :key #'cadr)))
      (when (> (length reveals) (info-max-reveals info))
        (return-from check-dealer-log '(:fault . :excessive-reveals)))
      (loop for (player kind . sig) in results
            when (and (eq kind :ack) (not (verify-summary player summary sig)))
              do (return-from check-dealer-log '(:fault . :invalid-ack)))
      (loop for (player nil . share) in reveals
            unless (dealing-matches-p (dealer-log-pub-msg log) (info-player-scalar info player)
                                      share)
              do (return-from check-dealer-log '(:fault . :invalid-reveal)))
      :available)))

(defun check-signed-dealer-log (signed info)
  "SignedDealerLog::check: (values dealer log) when the dealer's signature over the round's
log transcript verifies, else NIL."
  (let ((log (signed-dealer-log-log signed))
        (dealer (signed-dealer-log-dealer signed)))
    (when (transcript-verify (log-transcript info log) dealer (signed-dealer-log-sig signed))
      (values dealer log))))

(defun sign-dealer-log (seed info log)
  (make-signed-dealer-log :dealer (clempo.ed25519:ed25519-public-key seed)
                          :log log
                          :sig (transcript-sign (log-transcript info log) seed)))

;;; Logs: the dealer logs a round agrees on.

(defstruct (logs (:constructor make-logs (info)))
  (info nil :type info)
  (entries '() :type list)              ; ((dealer . log) ...) sorted by dealer
  (known '() :type list))               ; ((dealer . check-dealer-log outcome) ...)

(defun logs-record (logs dealer log)
  "Logs::record: add or replace DEALER's LOG (forgetting its verification). Returns true
when it replaced one."
  (let ((old (assoc dealer (logs-entries logs) :test #'octets=)))
    (setf (logs-entries logs)
          (sort (cons (cons dealer log) (remove old (logs-entries logs)))
                #'cw-order< :key #'car)
          (logs-known logs) (remove dealer (logs-known logs) :key #'car :test #'octets=))
    (and old t)))

(defun logs-check (logs dealer log)
  "check-dealer-log for a recorded log, verified once until the dealer's log is replaced."
  (let ((cached (assoc dealer (logs-known logs) :test #'octets=)))
    (if cached
        (cdr cached)
        (let ((outcome (check-dealer-log (logs-info logs) dealer log)))
          (push (cons dealer outcome) (logs-known logs))
          outcome))))

(defun logs-usable (logs)
  "The first required-commitments recorded logs (by dealer) that verify, checking no more
logs than needed; fewer when there are not enough."
  (let ((required (info-required-commitments (logs-info logs)))
        (usable '()))
    (loop for (dealer . log) in (logs-entries logs)
          while (< (length usable) required)
          when (eq (logs-check logs dealer log) :available)
            do (push (cons dealer log) usable))
    (nreverse usable)))

(defun logs-pre-verify (logs)
  "Logs::pre_verify: verify recorded logs now so that selection reuses the work."
  (logs-usable logs)
  logs)

(defun logs-select (logs)
  "Logs::select: the first required-commitments dealers (by key) whose logs are usable.
Signals DKG-FAILURE when too few are."
  (let* ((info (logs-info logs))
         (required (info-required-commitments info))
         (selected (logs-usable logs)))
    (when (< (length selected) required)
      (let ((known (logs-known logs)))
        (error 'dkg-failure
               :required required :found (length selected)
               :faults (sort (loop for (d . o) in known
                                   when (and (consp o) (eq (car o) :fault)) collect (cons d (cdr o)))
                             #'cw-order< :key #'car)
               :unavailable (remove-if (lambda (d)
                                         (let ((o (cdr (assoc d known :test #'octets=))))
                                           (or (eq o :available) (consp o))))
                                       (info-dealers info)))))
    selected))

(defun reckon (info selected)
  "Observe::reckon: the output of a round from its SELECTED logs, and the Lagrange weights
of a reshare (NIL for a fresh DKG)."
  (let* ((threshold (info-reveal-threshold info))
         (counts (make-hash-table :test 'equalp))
         (revealed '()))
    (dolist (entry selected)
      (loop for (player kind) in (log-results (cdr entry))
            when (and (eq kind :reveal) (= (incf (gethash player counts 0)) threshold))
              do (push player revealed)))
    (let* ((previous (info-previous info))
           (commitments (mapcar (lambda (e) (dealer-pub-msg-commitment
                                             (dealer-log-pub-msg (cdr e))))
                                selected))
           (weights (and previous (reshare-weights previous (mapcar #'car selected))))
           (public (if previous
                       (combine-commitments commitments weights)
                       (combine-commitments commitments (make-list (length commitments)
                                                                   :initial-element 1)))))
      (when previous
        (invariant (octets= (first public) (sharing-public (output-public previous)))
                   "selected reshare commitments must preserve the previous public key"))
      (values (make-output :summary (info-summary info)
                           :public (make-sharing :mode (info-mode info)
                                                 :total (length (info-players info))
                                                 :coefficients public)
                           :dealers (mapcar #'car selected)
                           :players (info-players info)
                           :revealed (sort revealed #'cw-order<))
              weights))))

(defun reshare-weights (previous dealers)
  "Mode::subset_interpolator over the previous players: Lagrange weights at 0 for DEALERS'
evaluation points in the previous sharing."
  (let* ((players (output-players previous))
         (mode (sharing-mode (output-public previous))))
    (lagrange-weights (mapcar (lambda (d) (mode-scalar mode (length players)
                                                       (key-index d players)))
                              dealers))))

(defun combine-commitments (commitments weights)
  "sum_i weights_i * commitments_i, coefficient by coefficient (Poly msm / sum)."
  (let ((width (reduce #'max commitments :key #'length)))
    (loop for j below width
          collect (let ((points '()) (scalars '()))
                    (loop for c in commitments for w in weights
                          for coefficient = (nth j c)
                          when coefficient do (push (g2-of coefficient) points) (push w scalars))
                    (g2-octets (g2-lincomb points scalars))))))

(defun observe (logs)
  "observe: the public output of a round from its LOGS, for anyone. Signals DKG-FAILURE
when the logs cannot produce one."
  (values (reckon (logs-info logs) (logs-select logs))))

;;; Dealer.

(defstruct (dealer (:constructor %make-dealer))
  seed public-key info pub-msg results transcript)

(defun start-dealer (info seed &key share (rng #'clempo.io:random-octets))
  "Dealer::start: deal a polynomial of the round's degree whose constant is SHARE (this
dealer's previous share, fr-octets or a SHARE struct; required for a reshare, random for a
fresh DKG). RNG (a function of a byte count) supplies the polynomial's randomness exactly as
Rust draws it from its CryptoRng. Returns (values dealer pub-msg priv-msgs), PRIV-MSGS
\((player . scalar) ...). The polynomial's random coefficients are zeroed once dealt."
  (let ((me (clempo.ed25519:ed25519-public-key seed)))
    (unless (info-dealer-index info me)
      (dkg-fail :dealer-not-in-round "~A" (octets->hex me)))
    (let* ((constant (cond ((share-p share) (share-private share))
                           (share share)
                           ((info-previous info) (dkg-fail :missing-dealer-share))
                           (t (random-scalar rng))))
           (poly (cons constant (loop repeat (info-degree info) collect (random-scalar rng))))
           (priv-msgs (mapcar (lambda (p) (cons p (scalar-poly-eval poly (info-player-scalar info p))))
                              (info-players info)))
           (pub-msg (make-dealer-pub-msg :commitment (commit-scalars poly))))
      (mapc #'clempo.ct:wipe (if share (rest poly) poly))
      (values (%make-dealer :seed seed :public-key me :info info :pub-msg pub-msg
                            :results (mapcar (lambda (m) (list* (car m) :reveal (cdr m))) priv-msgs)
                            :transcript (ack-transcript (round-transcript info) me pub-msg))
              pub-msg
              priv-msgs))))

(defun dealer-receive-player-ack (dealer player ack)
  "Dealer::receive_player_ack: record PLAYER's ACK (a signature). Signals DKG-ERROR
:unexpected-player or :invalid-ack."
  (let ((entry (assoc player (dealer-results dealer) :test #'octets=)))
    (unless entry (dkg-fail :unexpected-player))
    (unless (transcript-verify (dealer-transcript dealer) player ack) (dkg-fail :invalid-ack))
    (setf (cdr entry) (cons :ack ack))
    dealer))

(defun finalize-dealer (dealer)
  "Dealer::finalize: the signed log of this dealer's commitment and its acks or reveals;
results are omitted when there are too many reveals."
  (let* ((info (dealer-info dealer))
         (reveals (count :reveal (dealer-results dealer) :key #'cadr))
         (log (make-dealer-log :pub-msg (dealer-pub-msg dealer)
                               :results (if (> reveals (info-max-reveals info))
                                            :too-many-reveals
                                            (cons :ok (mapcar #'copy-list (dealer-results dealer)))))))
    (sign-dealer-log (dealer-seed dealer) info log)))

;;; Player.

(defstruct (player (:constructor %make-player))
  seed public-key info index transcript
  (view '()))                           ; ((dealer pub-msg . priv) ...)

(defun make-player (info seed)
  "Player::new. Signals DKG-ERROR :player-not-in-round."
  (let* ((me (clempo.ed25519:ed25519-public-key seed))
         (index (info-player-index info me)))
    (unless index (dkg-fail :player-not-in-round))
    (%make-player :seed seed :public-key me :info info :index index
                  :transcript (round-transcript info))))

(defun player-dealer-message (player dealer pub-msg priv-msg)
  "Player::dealer_message: check DEALER's PUB-MSG and private dealing PRIV-MSG (a scalar)
and return the ack signature; NIL when this dealer was already processed. Signals DKG-ERROR
:unexpected-dealer, :invalid-commitment-degree, :mismatched-reshare-commitment or
:invalid-dealer-share."
  (let ((info (player-info player)))
    (when (assoc dealer (player-view player) :test #'octets=)
      (return-from player-dealer-message nil))
    (unless (info-dealer-index info dealer) (dkg-fail :unexpected-dealer))
    (let ((problem (dealer-pub-msg-problem info dealer pub-msg)))
      (when problem (dkg-fail problem)))
    (unless (and (clempo.bls12-381:fr-octets-p priv-msg :nonzero t)
                 (dealing-matches-p pub-msg (mode-scalar (info-mode info)
                                                         (length (info-players info))
                                                         (player-index player))
                                    priv-msg))
      (dkg-fail :invalid-dealer-share))
    (let ((ack (transcript-sign (ack-transcript (player-transcript player) dealer pub-msg)
                                (player-seed player))))
      (push (list* dealer pub-msg priv-msg) (player-view player))
      ack)))

(defun log-ack (log player)
  "DealerLog::get_ack: PLAYER's ack signature in LOG, if it acked."
  (let ((entry (find player (log-results log) :key #'car :test #'octets=)))
    (and entry (eq (cadr entry) :ack) (cddr entry))))

(defun resume-player (info seed logs messages)
  "Player::resume: a player rebuilt from its persisted MESSAGES ((dealer pub-msg scalar) ...),
checked against the verified LOGS ((dealer . log) ...) published so far. Returns
\(values player acks), ACKS ((dealer . ack) ...) for the replayed messages. Signals DKG-ERROR
:invalid-persisted-dealing when a message no longer verifies, :missing-player-dealing when a
log carries a valid ack of ours for a dealing we do not have."
  (let ((player (make-player info seed))
        (acks '()))
    (loop for (dealer pub-msg scalar) in messages
          do (let ((ack (handler-case (player-dealer-message player dealer pub-msg scalar)
                          (dkg-error ()
                            (dkg-fail :invalid-persisted-dealing "~A" (octets->hex dealer))))))
               (when ack (push (cons dealer ack) acks))))
    (loop for (dealer . log) in logs
          for ack = (log-ack log (player-public-key player))
          when (and ack
                    (transcript-verify (ack-transcript (player-transcript player) dealer
                                                      (dealer-log-pub-msg log))
                                       (player-public-key player) ack)
                    (not (assoc dealer (player-view player) :test #'octets=)))
            do (dkg-fail :missing-player-dealing))
    (values player (nreverse acks))))

(defun player-dealing (player dealer log)
  "The scalar this player uses from DEALER's selected LOG: its own persisted dealing when it
matches the log's commitment, else the log's reveal of it."
  (let* ((persisted (cdr (assoc dealer (player-view player) :test #'octets=)))
         (reveal (find-if (lambda (r) (and (octets= (car r) (player-public-key player))
                                           (eq (cadr r) :reveal)))
                          (log-results log))))
    (cond ((and persisted (equalp (car persisted) (dealer-log-pub-msg log))) (cdr persisted))
          (reveal (cddr reveal))
          (persisted (dkg-fail :invalid-persisted-dealing "~A" (octets->hex dealer)))
          (t (dkg-fail :missing-player-dealing)))))

(defun finalize-player (player logs)
  "Player::finalize: (values output share) from the round's agreed LOGS. Signals DKG-ERROR
:mismatched-logs, :missing-player-dealing or :invalid-persisted-dealing, or DKG-FAILURE."
  (unless (info= (logs-info logs) (player-info player)) (dkg-fail :mismatched-logs))
  (let* ((selected (logs-select logs))
         (dealings (mapcar (lambda (e) (player-dealing player (car e) (cdr e))) selected)))
    (multiple-value-bind (output weights) (reckon (player-info player) selected)
      (values output
              (make-share :index (player-index player)
                          :private (scalar-lincomb (or weights (make-list (length dealings)
                                                                          :initial-element 1))
                                                   dealings))))))

;;; The trusted dealer.

(defun deal (players &key (mode :nonzero-counter) (rng #'clempo.io:random-octets))
  "deal::<N3f1>: shares of a random polynomial for PLAYERS (a sorted key list) without a
protocol. Returns (values output shares), SHARES ((player . share) ...)."
  (when (null players) (dkg-fail :num-players "0"))
  (let* ((n (length players))
         (poly (loop repeat (quorum n) collect (random-scalar rng)))
         (shares (loop for p in players for i from 0
                       collect (cons p (make-share :index i
                                                   :private (scalar-poly-eval poly (mode-scalar mode n i))))))
         (coefficients (commit-scalars poly)))
    (mapc #'clempo.ct:wipe poly)
    (values (make-output :summary (funcall rng 32)
                         :public (make-sharing :mode mode :total n :coefficients coefficients)
                         :dealers players :players players :revealed '())
            shares)))
