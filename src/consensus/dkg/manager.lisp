;;; Tempo's DKG manager (consensus/src/dkg/manager/actor/{mod.rs,state.rs}) as a pure state
;;; machine. The validator feeds it finalized blocks, notarized blocks and DKG channel
;;; messages; it answers with actions (messages to send, epochs to enter and exit) and the
;;; DKG data a proposal's extraData carries. Everything that must survive a restart goes
;;; through a DKG-STORE in Tempo's own encodings (State, Event).

(in-package #:clempo.dkg)

;;; Persisted forms (state.rs).

(defun write-share-state (w share)
  "ShareState::Plaintext(Option<Share>)."
  (write-u8 w 0)
  (write-bool w share)
  (when share (write-share w share)))

(defun read-share-state (r)
  (let ((tag (read-u8 r)))
    (unless (zerop tag) (cw-fail "invalid share state tag ~D" tag))
    (when (read-bool r) (read-share r))))

(defcwtype share-state :write write-share-state :read read-share-state)

(defcodec (epoch-state (:conc-name state-))
  ;; The outcome of a ceremony: the state a DKG round of EPOCH starts from.
  (epoch :varint)
  (seed summary)                        ; the dealer's randomness for this round
  (output output)
  (share share-state)                   ; this node's share of OUTPUT, or NIL
  (players (:set ed25519-public-key :min 1 :max +max-validators+))
  (syncers (:set ed25519-public-key :min 0 :max +max-validators+)) ; legacy, written empty
  (is-full-dkg :bool))

(defcwunion dkg-event
  ;; state.rs Event, one journal section per epoch.
  (0 :dealing (:tuple ed25519-public-key dealer-pub-msg dealer-priv-msg))
  (1 :ack (:tuple ed25519-public-key player-ack))
  (2 :log (:tuple ed25519-public-key dealer-log))
  (3 :finalized (:tuple (:array 32) (:array 32) :varint)))

(defmacro with-persisted-config (&body body)
  "Tempo reads its DKG state and events with MAXIMUM_VALIDATORS and ModeVersion v0."
  `(let ((*max-participants* +max-validators+) (*max-mode-version* 0)) ,@body))

(defun encode-epoch-state (state) (with-persisted-config (cw-encode 'epoch-state state)))
(defun decode-epoch-state (octets) (with-persisted-config (values (cw-decode 'epoch-state octets))))
(defun encode-dkg-event (event) (with-persisted-config (cw-encode 'dkg-event event)))
(defun decode-dkg-event (octets) (with-persisted-config (values (cw-decode 'dkg-event octets))))

;;; The store: states keyed by epoch, events appended per epoch. Every write must be durable
;;; when the call returns (Tempo syncs each one).

(defgeneric dkg-store-states (store)
  (:documentation "Every persisted state: ((epoch . state-octets) ...)."))
(defgeneric dkg-store-events (store)
  (:documentation "Every persisted event in append order: ((epoch . event-octets) ...)."))
(defgeneric dkg-store-put-state (store epoch octets))
(defgeneric dkg-store-append-event (store epoch octets))
(defgeneric dkg-store-prune (store up-to-epoch)
  (:documentation "Forget states and events of epochs below UP-TO-EPOCH."))

(defstruct memory-dkg-store
  (states '())
  (events '()))                         ; newest first

(defmethod dkg-store-states ((s memory-dkg-store)) (copy-list (memory-dkg-store-states s)))
(defmethod dkg-store-events ((s memory-dkg-store)) (reverse (memory-dkg-store-events s)))
(defmethod dkg-store-put-state ((s memory-dkg-store) epoch octets)
  (setf (memory-dkg-store-states s)
        (acons epoch (copy-seq octets) (remove epoch (memory-dkg-store-states s) :key #'car))))
(defmethod dkg-store-append-event ((s memory-dkg-store) epoch octets)
  (push (cons epoch (copy-seq octets)) (memory-dkg-store-events s)))
(defmethod dkg-store-prune ((s memory-dkg-store) up-to-epoch)
  (setf (memory-dkg-store-states s) (remove-if (lambda (e) (< (car e) up-to-epoch))
                                               (memory-dkg-store-states s))
        (memory-dkg-store-events s) (remove-if (lambda (e) (< (car e) up-to-epoch))
                                               (memory-dkg-store-events s))))

;;; Blocks as the manager sees them.

(defstruct (dkg-block (:constructor make-dkg-block
                          (&key height digest parent (timestamp 0)
                                (extra-data (make-octets 0)) parent-view)))
  "A finalized or notarized block: HEIGHT, DIGEST and PARENT digest (32 octets), header
TIMESTAMP in seconds, header EXTRA-DATA, and the view its parent was notarized in (only used
to name a missing ancestor)."
  height digest parent timestamp extra-data parent-view)

;;; Epoch arithmetic (commonware FixedEpocher).

(defun epoch-phase (height epoch-length)
  "EpochPhase of HEIGHT: :early before the midpoint, :midpoint at it, :late after."
  (let ((relative (- height (epoch-first-height (height-epoch height epoch-length) epoch-length)))
        (midpoint (floor epoch-length 2)))
    (cond ((< relative midpoint) :early) ((= relative midpoint) :midpoint) (t :late))))

(defun latest-boundary-at-or-before (height epoch-length)
  "The latest boundary height at or below HEIGHT (genesis before the first) and the epoch
of the outcome it carries."
  (let ((epoch (height-epoch height epoch-length)))
    (if (= height (epoch-last-height epoch epoch-length))
        (values height (1+ epoch))
        (values (if (zerop epoch) 0 (epoch-last-height (1- epoch) epoch-length)) epoch))))

;;; Events of one epoch, as Storage caches them.

(defstruct epoch-events
  (acks '())                            ; ((player . ack) ...)
  (dealings '())                        ; ((dealer pub-msg . scalar) ...)
  (logs '())                            ; ((dealer . log) ...), sorted by dealer
  (finalized '())                       ; ((height digest . parent) ...), newest first
  (notarized (make-hash-table :test 'equalp)) ; digest -> reduced block
  (outcomes (make-hash-table :test 'equalp))  ; parent digest -> (output . share)
  (epoch-outcome nil))                  ; (output . share) once every dealer's log is final

(defstruct reduced-block height parent parent-view digest log) ; log: (dealer . log) or NIL

(defun sorted-insert (alist key value)
  (merge 'list (list (cons key value)) alist #'cw-order< :key #'car))

;;; The round's dealer and player, as the actor wraps them.

(defstruct tempo-dealer
  crypto                                ; the protocol dealer, NIL once finalized
  pub-msg
  (unsent '())                          ; ((player . scalar) ...): resent until acked
  finalized)                            ; our signed log until it is seen on chain

(defstruct tempo-player
  crypto
  (acks '()))                           ; ((dealer . ack) ...): one ack per dealer, ever

;;; The manager.

(defstruct (dkg-manager (:constructor %make-dkg-manager))
  seed key namespace epoch-length store
  t12-time t15-time                     ; hardfork activation timestamps (seconds), NIL = never
  header-at                             ; height -> dkg-block of a finalized block
  state                                 ; the current epoch-state
  info dealer player
  (cache (make-hash-table)))            ; epoch -> epoch-events

(defun fork-active-p (time timestamp) (and time (>= timestamp time)))

(defun manager-t15-p (m timestamp) (fork-active-p (dkg-manager-t15-time m) timestamp))

(defun manager-events (m epoch)
  (or (gethash epoch (dkg-manager-cache m))
      (setf (gethash epoch (dkg-manager-cache m)) (make-epoch-events))))

(defun manager-epoch (m) (state-epoch (dkg-manager-state m)))

(defun header-at (m height)
  (or (funcall (or (dkg-manager-header-at m) (constantly nil)) height)
      (dkg-fail :missing-header "no finalized header at height ~D" height)))

(defun state-v1-active-p (m state)
  "is_state_v1_activated: T12 active at the boundary block that installed STATE (the last
block of the previous epoch, genesis for epoch 0)."
  (let ((t12 (dkg-manager-t12-time m)))
    (cond ((null t12) nil)
          ((zerop t12) t)
          (t (let ((boundary (if (zerop (state-epoch state))
                                 0
                                 (epoch-last-height (1- (state-epoch state))
                                                    (dkg-manager-epoch-length m)))))
               (fork-active-p t12 (dkg-block-timestamp (header-at m boundary))))))))

(defun state-round-info (m state)
  "Round::from_state."
  (make-round-info (dkg-manager-namespace m) (state-epoch state)
                   :previous (unless (state-is-full-dkg state) (state-output state))
                   :mode :nonzero-counter
                   :reveal (if (state-v1-active-p m state) :v1 :v0)
                   :dealers (output-players (state-output state))
                   :players (state-players state)))

;;; Storage operations (each persists before it updates the cache).

(defun append-event (m epoch event)
  (dkg-store-append-event (dkg-manager-store m) epoch (encode-dkg-event event)))

(defun set-state (m state)
  (dkg-store-put-state (dkg-manager-store m) (state-epoch state) (encode-epoch-state state))
  (setf (dkg-manager-state m) state))

(defun append-ack (m epoch player ack)
  (let ((events (manager-events m epoch)))
    (unless (assoc player (epoch-events-acks events) :test #'octets=)
      (append-event m epoch (list* :ack player (list ack)))
      (setf (epoch-events-acks events) (sorted-insert (epoch-events-acks events) player ack)))))

(defun append-dealing (m epoch dealer pub-msg scalar)
  (let ((events (manager-events m epoch)))
    (unless (assoc dealer (epoch-events-dealings events) :test #'octets=)
      (append-event m epoch (list :dealing dealer pub-msg scalar))
      (setf (epoch-events-dealings events)
            (sorted-insert (epoch-events-dealings events) dealer (cons pub-msg scalar))))))

(defun append-dealer-log (m epoch dealer log)
  (let ((events (manager-events m epoch)))
    (unless (assoc dealer (epoch-events-logs events) :test #'octets=)
      (append-event m epoch (list :log dealer log))
      (setf (epoch-events-logs events) (sorted-insert (epoch-events-logs events) dealer log)))))

(defun append-finalized (m epoch block)
  (let ((events (manager-events m epoch))
        (height (dkg-block-height block)))
    (unless (assoc height (epoch-events-finalized events))
      (append-event m epoch (list :finalized (dkg-block-digest block) (dkg-block-parent block)
                                  height))
      (push (list* height (dkg-block-digest block) (dkg-block-parent block))
            (epoch-events-finalized events))
      (setf (epoch-events-finalized events)
            (sort (epoch-events-finalized events) #'> :key #'car)))))

(defun replay-event (m epoch event)
  "Events::insert: rebuild the cache from the journal."
  (let ((events (manager-events m epoch)))
    (destructuring-bind (kind . data) event
      (ecase kind
        (:dealing (destructuring-bind (dealer pub-msg scalar) data
                    (setf (epoch-events-dealings events)
                          (sorted-insert (remove dealer (epoch-events-dealings events)
                                                 :key #'car :test #'octets=)
                                         dealer (cons pub-msg scalar)))))
        (:ack (destructuring-bind (player ack) data
                (setf (epoch-events-acks events)
                      (sorted-insert (remove player (epoch-events-acks events)
                                             :key #'car :test #'octets=)
                                     player ack))))
        (:log (destructuring-bind (dealer log) data
                (setf (epoch-events-logs events)
                      (sorted-insert (remove dealer (epoch-events-logs events)
                                             :key #'car :test #'octets=)
                                     dealer log))))
        (:finalized (destructuring-bind (digest parent height) data
                      (setf (epoch-events-finalized events)
                            (sort (acons height (cons digest parent)
                                         (remove height (epoch-events-finalized events)
                                                 :key #'car))
                                  #'> :key #'car))))))))

(defun latest-finalized-height (m epoch)
  (car (first (epoch-events-finalized (manager-events m epoch)))))

(defun prune (m up-to-epoch)
  (dkg-store-prune (dkg-manager-store m) up-to-epoch)
  (loop for epoch being the hash-keys of (dkg-manager-cache m)
        when (< epoch up-to-epoch) do (remhash epoch (dkg-manager-cache m))))

;;; Dealer and player of the round (create_dealer_for_round, create_player_for_round).

(define-constant +dealer-rng-label+ (string->octets "dealer-rng"))

(defun create-dealer (m state info)
  (let ((me (dkg-manager-key m))
        (epoch (state-epoch state)))
    (when (key-index me (info-dealers info))
      (let ((share (cond ((state-is-full-dkg state) nil)
                         ((state-share state))
                         ;; a dealer that lost its share sits the round out
                         (t (return-from create-dealer nil)))))
        (multiple-value-bind (crypto pub-msg priv-msgs)
            (start-dealer info (dkg-manager-seed m)
                          :share share
                          :rng (transcript-noise (resume-transcript (state-seed state) :v0)
                                                 +dealer-rng-label+))
          (let ((events (manager-events m epoch)))
            (if (assoc me (epoch-events-logs events) :test #'octets=)
                ;; our log is already on chain: do not deal again
                (make-tempo-dealer :crypto nil :pub-msg pub-msg)
                (let ((unsent (copy-alist priv-msgs)))
                  (loop for (player . ack) in (epoch-events-acks events)
                        when (and (assoc player unsent :test #'octets=)
                                  (handler-case (progn (dealer-receive-player-ack crypto player ack) t)
                                    (dkg-error () nil)))
                          do (setf unsent (remove player unsent :key #'car :test #'octets=)))
                  (make-tempo-dealer :crypto crypto :pub-msg pub-msg :unsent unsent)))))))))

(defun player-replay (tp dealer pub-msg scalar)
  (unless (assoc dealer (tempo-player-acks tp) :test #'octets=)
    (let ((ack (handler-case (player-dealer-message (tempo-player-crypto tp) dealer pub-msg scalar)
                 (dkg-error () nil))))
      (when ack (push (cons dealer ack) (tempo-player-acks tp))))))

(defun create-player (m state info)
  (when (key-index (dkg-manager-key m) (info-players info))
    (let ((tp (make-tempo-player :crypto (make-player info (dkg-manager-seed m)))))
      (loop for (dealer pub-msg . scalar) in (epoch-events-dealings
                                              (manager-events m (state-epoch state)))
            do (player-replay tp dealer pub-msg scalar))
      tp)))

(defun tempo-dealer-finalize (td)
  (unless (tempo-dealer-finalized td)
    (let ((crypto (tempo-dealer-crypto td)))
      (when crypto
        (setf (tempo-dealer-finalized td) (finalize-dealer crypto)
              (tempo-dealer-crypto td) nil)))))

(defun receive-dealing (m tp dealer pub-msg scalar)
  "Player::receive_dealing: the cached ack for DEALER, or a fresh one after persisting the
dealing. Signals DKG-ERROR for an invalid dealing."
  (or (cdr (assoc dealer (tempo-player-acks tp) :test #'octets=))
      (let ((ack (player-dealer-message (tempo-player-crypto tp) dealer pub-msg scalar)))
        (invariant ack "dealer message was already processed without a cached acknowledgement")
        (append-dealing m (manager-epoch m) dealer pub-msg scalar)
        (push (cons dealer ack) (tempo-player-acks tp))
        ack)))

(defun receive-ack (m td player ack)
  "Dealer::receive_ack: the first ack of each player counts, valid or not; a valid one is
persisted. Signals DKG-ERROR for an invalid ack."
  (when (assoc player (tempo-dealer-unsent td) :test #'octets=)
    (setf (tempo-dealer-unsent td)
          (remove player (tempo-dealer-unsent td) :key #'car :test #'octets=))
    (let ((crypto (tempo-dealer-crypto td)))
      (when crypto
        (dealer-receive-player-ack crypto player ack)
        (append-ack m (manager-epoch m) player ack)))))

(defun dealer-message-action (recipient epoch pub-msg scalar)
  (list :send recipient epoch (cw-encode 'dkg-message (list :dealer pub-msg scalar))))

(defun distribute-shares (m)
  "Send our dealing to every player that has not acked it; process our own directly."
  (let ((td (dkg-manager-dealer m))
        (tp (dkg-manager-player m))
        (me (dkg-manager-key m))
        (actions '()))
    (loop for (player . scalar) in (copy-alist (tempo-dealer-unsent td))
          do (if (octets= player me)
                 (when tp
                   (handler-case
                       (receive-ack m td me (receive-dealing m tp me (tempo-dealer-pub-msg td) scalar))
                     (dkg-error () nil)))
                 (push (dealer-message-action player (manager-epoch m) (tempo-dealer-pub-msg td)
                                              scalar)
                       actions)))
    (nreverse actions)))

;;; Starting a round (run_dkg_loop's prologue).

(defun start-round (m)
  (let* ((state (dkg-manager-state m))
         (epoch (state-epoch state)))
    (when (plusp epoch) (prune m (1- epoch)))
    (let ((info (state-round-info m state)))
      (setf (dkg-manager-info m) info
            (dkg-manager-dealer m) (create-dealer m state info))
      (let ((latest (latest-finalized-height m epoch)))
        (when (and (dkg-manager-dealer m) latest
                   (not (eq :early (epoch-phase latest (dkg-manager-epoch-length m)))))
          (tempo-dealer-finalize (dkg-manager-dealer m))))
      (setf (dkg-manager-player m) (create-player m state info))
      (maybe-conclude-for-epoch m)
      (list (list :enter epoch (state-output state) (state-share state))))))

;;; Concluding the ceremony.

(defun conclude-round (m logs)
  "The actor's conclude_ceremony over LOGS ((dealer . log) ...): (values output share)."
  (let ((state (dkg-manager-state m))
        (tp (dkg-manager-player m))
        (ceremony (make-logs (dkg-manager-info m))))
    (loop for (dealer . log) in logs do (logs-record ceremony dealer log))
    (multiple-value-bind (output share) (conclude-ceremony ceremony (state-output state)
                                                           :player (and tp (tempo-player-crypto tp))
                                                           :previous-share (state-share state))
      (values output share))))

(defun maybe-conclude-for-epoch (m)
  (let ((events (manager-events m (manager-epoch m))))
    (when (and (null (epoch-events-epoch-outcome events))
               (= (length (epoch-events-logs events))
                  (length (info-dealers (dkg-manager-info m)))))
      (multiple-value-bind (output share) (conclude-round m (epoch-events-logs events))
        (setf (epoch-events-epoch-outcome events) (cons output share))))))

(defun cached-outcome (m parent-digest)
  (let ((events (manager-events m (manager-epoch m))))
    (or (gethash parent-digest (epoch-events-outcomes events))
        (epoch-events-epoch-outcome events))))

;;; Finalized blocks.

(defun record-finalized (m block)
  "record_finalized_header: read a dealer log from a non-boundary block, then journal it."
  (let ((epoch (manager-epoch m))
        (extra (dkg-block-extra-data block)))
    (when (plusp (length extra))
      (handler-case
          (multiple-value-bind (dealer log)
              (extra-data-dealer-log extra (dkg-manager-info m)
                                     :strict (manager-t15-p m (dkg-block-timestamp block)))
            (append-dealer-log m epoch dealer log)
            (let ((td (dkg-manager-dealer m)))
              (when (and td (octets= dealer (dkg-manager-key m)))
                (setf (tempo-dealer-finalized td) nil))))
        (dkg-error () nil)))
    (append-finalized m epoch block)))

(defun dkg-finalized-block (m block)
  "handle_finalized_header for the next finalized BLOCK, in height order. Returns a list of
actions: (:send recipient epoch octets), (:exit epoch), (:enter epoch output share). At the
epoch's last block the manager reads the boundary outcome, concludes its own ceremony, and
moves to the next epoch. Blocks of past epochs are ignored."
  (let* ((height (dkg-block-height block))
         (length (dkg-manager-epoch-length m))
         (epoch (height-epoch height length))
         (current (manager-epoch m))
         (actions '()))
    (cond ((< current epoch)
           (dkg-fail :future-epoch "block ~D is in epoch ~D, the ceremony in ~D" height epoch current))
          ((> current epoch) (return-from dkg-finalized-block '())))
    (let ((td (dkg-manager-dealer m)))
      (when td
        (if (eq :early (epoch-phase height length))
            (setf actions (distribute-shares m))
            (tempo-dealer-finalize td))))
    (unless (= height (epoch-last-height epoch length))
      (record-finalized m block)
      (maybe-conclude-for-epoch m)
      (return-from dkg-finalized-block actions))
    (let* ((outcome (handler-case (decode-onchain-dkg-outcome (dkg-block-extra-data block))
                      (cw-error (e) (dkg-fail :missing-outcome "boundary block ~D: ~A" height e)))))
      (unless (= (outcome-epoch outcome) (1+ current))
        (dkg-fail :wrong-outcome-epoch "boundary outcome for epoch ~D, expected ~D"
                  (outcome-epoch outcome) (1+ current)))
      (multiple-value-bind (output share)
          (let ((cached (cached-outcome m (dkg-block-parent block))))
            (if cached
                (values (car cached) (cdr cached))
                (conclude-round m (epoch-events-logs (manager-events m current)))))
        (unless (equalp output (outcome-output outcome))
          (setf share nil))
        (set-state m (make-epoch-state :epoch (outcome-epoch outcome)
                                       :seed (clempo.io:random-octets 32)
                                       :output (outcome-output outcome)
                                       :share share
                                       :players (outcome-next-players outcome)
                                       :syncers '()
                                       :is-full-dkg (outcome-is-next-full-dkg outcome)))
        (append actions (list (list :exit current)) (start-round m))))))

;;; The DKG channel.

(defun dkg-receive (m from epoch octets)
  "handle_network_msg: a DKG channel message (OCTETS, after the epoch mux) from the
authenticated peer FROM. Returns the actions it causes (an ack back to a dealer). Messages
for other epochs are dropped. Signals DKG-ERROR or CW-ERROR for a message to block the peer
for: a dealing from a non-dealer, an ack from a non-player, bad bytes, an invalid dealing."
  (unless (= epoch (manager-epoch m)) (return-from dkg-receive '()))
  (let ((info (dkg-manager-info m)))
    (case (and (plusp (length octets)) (aref octets 0))
      (0 (unless (key-index from (info-dealers info)) (dkg-fail :unexpected-dealer)))
      (1 (unless (key-index from (info-players info)) (dkg-fail :unexpected-player))))
    (let ((message (let ((*max-participants* (length (info-players info))))
                     (values (cw-decode 'dkg-message octets)))))
      (ecase (car message)
        (:dealer
         (let ((tp (dkg-manager-player m)))
           (when tp
             (destructuring-bind (pub-msg scalar) (cdr message)
               (let ((ack (receive-dealing m tp from pub-msg scalar)))
                 (list (list :send from epoch (cw-encode 'dkg-message (cons :ack ack)))))))))
        (:ack
         (let ((td (dkg-manager-dealer m)))
           (when td (receive-ack m td from (cdr message)))
           '()))))))

;;; What proposals carry, and checking what others propose.

(defun dkg-dealer-log (m epoch)
  "GetDealerLog: our signed dealer log for a non-boundary proposal of EPOCH, as extraData
octets, or NIL."
  (let ((td (dkg-manager-dealer m)))
    (and td (= epoch (manager-epoch m)) (tempo-dealer-finalized td)
         (cw-encode 'signed-dealer-log (tempo-dealer-finalized td)))))

(defun dkg-verify-dealer-log (m epoch timestamp octets)
  "VerifyDealerLog: the dealer whose signed log of this round OCTETS hold (checked as
read_dealer_log, strict from T15 by TIMESTAMP), or NIL when invalid. The second value is
:unavailable when EPOCH is not the current round (Tempo abstains)."
  (if (/= epoch (manager-epoch m))
      (values nil :unavailable)
      (handler-case (values (extra-data-dealer-log octets (dkg-manager-info m)
                                                   :strict (manager-t15-p m timestamp)))
        (dkg-error () nil))))

(defun dkg-notarized-block (m block)
  "cache_notarized_block: remember a notarized BLOCK of the current epoch and its dealer log
for ceremony outputs on unfinalized branches."
  (let ((log (when (plusp (length (dkg-block-extra-data block)))
               (handler-case
                   (multiple-value-bind (dealer log)
                       (extra-data-dealer-log (dkg-block-extra-data block) (dkg-manager-info m)
                                              :strict (manager-t15-p m (dkg-block-timestamp block)))
                     (cons dealer log))
                 (dkg-error () nil)))))
    (setf (gethash (dkg-block-digest block)
                   (epoch-events-notarized (manager-events m (manager-epoch m))))
          (make-reduced-block :height (dkg-block-height block) :parent (dkg-block-parent block)
                              :parent-view (dkg-block-parent-view block)
                              :digest (dkg-block-digest block) :log log))))

(defun dkg-ceremony-output (m parent)
  "handle_subscribe_dkg_ceremony: the ceremony output for a boundary block on top of PARENT
\(a dkg-block of the current epoch, which this caches as notarized). Uses the finalized logs,
and for dealers without one the oldest log on PARENT's unfinalized branch. Returns the
output, or (values nil :needs-ancestor height digest parent-view) naming a branch block to
supply with DKG-NOTARIZED-BLOCK first."
  (let* ((epoch (manager-epoch m))
         (length (dkg-manager-epoch-length m)))
    (unless (= epoch (height-epoch (dkg-block-height parent) length))
      (dkg-fail :wrong-epoch "request is for epoch ~D, not ~D"
                (height-epoch (dkg-block-height parent) length) epoch))
    (dkg-notarized-block m parent)
    (let ((cached (cached-outcome m (dkg-block-digest parent))))
      (when cached (return-from dkg-ceremony-output (car cached))))
    (let* ((events (manager-events m epoch))
           (finalized (copy-alist (epoch-events-logs events)))
           (floor-height (latest-finalized-height m epoch))
           (first (epoch-first-height epoch length))
           (branch '())
           (height (dkg-block-height parent))
           (digest (dkg-block-digest parent))
           (view nil))
      (loop while (and (>= height first) (or (null floor-height) (>= height floor-height)))
            do (let ((block (gethash digest (epoch-events-notarized events))))
                 (unless block
                   (return-from dkg-ceremony-output (values nil :needs-ancestor height digest view)))
                 (let ((log (reduced-block-log block)))
                   (when (and log (not (assoc (car log) finalized :test #'octets=)))
                     ;; newest first, so an older log replaces a newer duplicate
                     (setf branch (acons (car log) (cdr log)
                                         (remove (car log) branch :key #'car :test #'octets=)))))
                 (when (zerop (reduced-block-height block)) (loop-finish))
                 (setf height (1- (reduced-block-height block))
                       digest (reduced-block-parent block)
                       view (reduced-block-parent-view block))))
      (loop for (dealer . log) in branch
            unless (assoc dealer finalized :test #'octets=)
              do (setf finalized (sorted-insert finalized dealer log)))
      (multiple-value-bind (output share) (conclude-round m finalized)
        (setf (gethash (dkg-block-digest parent) (epoch-events-outcomes events)) (cons output share))
        output))))

(defun dkg-boundary-extra-data (m parent next-players next-full-dkg-epoch)
  "The extraData of a boundary block on top of PARENT: the OnchainDkgOutcome of the ceremony
output with NEXT-PLAYERS (the active validators at PARENT's state, sorted) and whether the
next epoch is NEXT-FULL-DKG-EPOCH. Same second values as DKG-CEREMONY-OUTPUT when an
ancestor is missing."
  (multiple-value-bind (output missing height digest view) (dkg-ceremony-output m parent)
    (if output
        (encode-onchain-dkg-outcome
         (boundary-outcome output next-players :epoch (1+ (manager-epoch m))
                                               :next-full-dkg-epoch next-full-dkg-epoch))
        (values nil missing height digest view))))

;;; Accessors for the validator.

(defun dkg-manager-current-epoch (m) (manager-epoch m))
(defun dkg-manager-output (m) (state-output (dkg-manager-state m)))
(defun dkg-manager-share (m) (state-share (dkg-manager-state m)))
(defun dkg-manager-next-players (m) (state-players (dkg-manager-state m)))
(defun dkg-manager-dealer-p (m) (and (dkg-manager-dealer m) t))
(defun dkg-manager-player-p (m) (and (dkg-manager-player m) t))

(defun dkg-next-unrecorded-height (m)
  "The first height of the current epoch the manager has not recorded as finalized."
  (let ((latest (latest-finalized-height m (manager-epoch m))))
    (if latest
        (1+ latest)
        (epoch-first-height (manager-epoch m) (dkg-manager-epoch-length m)))))

;;; Startup (heal, establish_initial_state, prepopulate_to_last_finalized_height).

(defun share-matches-sharing-p (share sharing)
  (and share (< (share-index share) (sharing-total sharing))
       (octets= (sharing-partial-public sharing (share-index share))
                (g2-octets (g2-base-mul (share-private share))))))

(defun recover-revealed-share (m state)
  "maybe_recover_revealed_share: our share of STATE's output rebuilt from the dealer logs
finalized during the previous epoch (when our dealings were revealed), or NIL."
  (let ((me (dkg-manager-key m))
        (length (dkg-manager-epoch-length m))
        (epoch (state-epoch state)))
    (when (and (key-index me (output-players (state-output state))) (plusp epoch))
      (let* ((ceremony-epoch (1- epoch))
             (boundary (if (zerop ceremony-epoch) 0 (epoch-last-height (1- ceremony-epoch) length)))
             (ceremony-outcome (decode-onchain-dkg-outcome
                                (dkg-block-extra-data (header-at m boundary)))))
        (unless (= (outcome-epoch ceremony-outcome) ceremony-epoch)
          (dkg-fail :wrong-outcome-epoch "boundary ~D" boundary))
        (unless (equalp (outcome-output ceremony-outcome) (state-output state))
          (let* ((ceremony-state (make-epoch-state
                                  :epoch ceremony-epoch :seed (state-seed state)
                                  :output (outcome-output ceremony-outcome) :share nil
                                  :players (outcome-next-players ceremony-outcome)
                                  :syncers '()
                                  :is-full-dkg (outcome-is-next-full-dkg ceremony-outcome)))
                 (info (state-round-info m ceremony-state))
                 (selected (output-dealers (state-output state)))
                 (logs (make-logs info))
                 (found 0))
            (unless (key-index me (info-players info))
              (dkg-fail :player-not-in-round))
            (loop for height from (epoch-first-height ceremony-epoch length)
                    below (epoch-last-height ceremony-epoch length)
                  while (< found (length selected))
                  do (let ((block (header-at m height)))
                       (when (plusp (length (dkg-block-extra-data block)))
                         (handler-case
                             (multiple-value-bind (dealer log)
                                 (extra-data-dealer-log (dkg-block-extra-data block) info
                                                        :strict (manager-t15-p m (dkg-block-timestamp block)))
                               (when (and (key-index dealer selected)
                                          (not (assoc dealer (logs-entries logs) :test #'octets=)))
                                 (logs-record logs dealer log)
                                 (incf found)))
                           (dkg-error () nil)))))
            (unless (= found (length selected))
              (dkg-fail :missing-dealer-logs "found ~D of ~D selected dealer logs" found
                        (length selected)))
            (handler-case
                (multiple-value-bind (output share)
                    (finalize-player (make-player info (dkg-manager-seed m)) logs)
                  (unless (equalp output (state-output state))
                    (dkg-fail :recovered-output-mismatch))
                  share)
              (dkg-failure (f) (error f))
              (dkg-error (e) (if (eq (dkg-error-reason e) :missing-player-dealing) nil (error e))))))))))

(defun establish-initial-state (m last-finalized-height share-candidates)
  (multiple-value-bind (boundary epoch)
      (latest-boundary-at-or-before last-finalized-height (dkg-manager-epoch-length m))
    (let ((outcome (decode-onchain-dkg-outcome (dkg-block-extra-data (header-at m boundary)))))
      (unless (= (outcome-epoch outcome) epoch)
        (dkg-fail :wrong-outcome-epoch "boundary ~D holds epoch ~D, expected ~D"
                  boundary (outcome-epoch outcome) epoch))
      (let* ((share (find-if (lambda (s) (share-matches-sharing-p s (outcome-sharing outcome)))
                             (remove nil share-candidates)))
             (state (make-epoch-state :epoch epoch :seed (clempo.io:random-octets 32)
                                      :output (outcome-output outcome) :share share
                                      :players (outcome-next-players outcome) :syncers '()
                                      :is-full-dkg (outcome-is-next-full-dkg outcome))))
        (unless share
          (setf (state-share state) (ignore-errors (recover-revealed-share m state))))
        state))))

(defun open-dkg-manager (&key seed store epoch-length (namespace +tempo-namespace+)
                              t12-time t15-time header-at last-finalized-height initial-share)
  "Start (or restart) the DKG manager of the validator with ed25519 SEED.
STORE holds what survives restarts (MAKE-MEMORY-DKG-STORE, or a file-backed DKG-STORE).
HEADER-AT maps a finalized height to a DKG-BLOCK (used at startup and for the T12 check of a
round's boundary). LAST-FINALIZED-HEIGHT is the finalized floor; INITIAL-SHARE the configured
share. T12-TIME / T15-TIME are the hardfork timestamps (NIL: never, 0: from genesis).
Returns (values manager actions); the actions enter the current epoch."
  (let ((m (%make-dkg-manager :seed seed :key (clempo.ed25519:ed25519-public-key seed)
                              :namespace namespace :epoch-length epoch-length :store store
                              :t12-time t12-time :t15-time t15-time :header-at header-at))
        (states (dkg-store-states store))
        (events (dkg-store-events store)))
    (when (and (null states) events)
      (dkg-fail :corrupt-store "DKG states are empty, but the events journal holds events"))
    (loop for (epoch . octets) in events do (replay-event m epoch (decode-dkg-event octets)))
    (let* ((stored (when states
                     (decode-epoch-state (cdr (first (sort (copy-list states) #'> :key #'car))))))
           (next-epoch (height-epoch (1+ last-finalized-height) epoch-length)))
      (set-state m (if (and stored (>= (state-epoch stored) next-epoch))
                       stored
                       (establish-initial-state m last-finalized-height
                                                (list initial-share
                                                      (and stored (state-share stored))))))
      ;; prepopulate from the finalized headers of the current epoch
      (unless (> (manager-epoch m) next-epoch)
        (setf (dkg-manager-info m) (state-round-info m (dkg-manager-state m)))
        (loop for height from (dkg-next-unrecorded-height m) to last-finalized-height
              do (record-finalized m (header-at m height))))
      (values m (start-round m)))))
