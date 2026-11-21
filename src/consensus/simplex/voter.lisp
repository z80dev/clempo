(in-package #:clempo.simplex)

;;; The voter of one epoch: commonware simplex's voter state machine (actors/voter/state.rs,
;;; actor.rs), the vote assembly of its batcher (actors/batcher) and the certificate repair
;;; requests of its resolver (actors/resolver/state.rs), merged into one synchronous object.
;;;
;;; Terms are rotating (one view per term, the only shape Tempo uses), so there is no
;;; optimistic validation, no pipelined handoff and no stall timeout; the skip policy is
;;; commonware's Enabled { timeout, budget: Participants } when :SKIP is set.
;;;
;;; Every entry point (VOTER-START, VOTER-RECEIVE, VOTER-TICK, VOTER-PROPOSED, VOTER-VERIFIED,
;;; VOTER-CERTIFIED) queues an event and pumps the event loop of actor.rs until nothing is
;;; left to do: each iteration reconciles application requests, starts certifications and
;;; prunes (on_start), takes one event (a message, an application answer or a due timeout),
;;; then builds and publishes the votes and certificates that became available for the
;;; event's view (on_end). The effects produced on the way are returned in order.

;;; Application

(defgeneric app-propose (app context)
  (:documentation "Build a payload (32-octet digest) on CONTEXT's parent. Return the payload,
:PENDING to answer later with VOTER-PROPOSED, or NIL if no proposal can be made (the view
then times out as commonware's MissingProposal)."))

(defgeneric app-verify (app context payload)
  (:documentation "Verify the leader's PAYLOAD built on CONTEXT's parent: T, NIL (invalid: the
view times out), or :PENDING to answer later with VOTER-VERIFIED."))

(defgeneric app-certify (app round payload)
  (:documentation "Certify the notarized PAYLOAD of ROUND before voting to finalize it: T, NIL
\(we nullify the view and never build on it) or :PENDING (answer with VOTER-CERTIFIED).
commonware's default certifies everything.")
  (:method (app round payload)
    (declare (ignore app round payload))
    t))

(defstruct (context (:constructor make-context (round leader leader-key parent)))
  "What a proposal or verification request is about: ROUND, the LEADER's index and ed25519
LEADER-KEY, and PARENT = (parent-view . parent-payload)."
  round leader leader-key parent)

(defun context-view (context) (round-view (context-round context)))
(defun context-epoch (context) (round-epoch (context-round context)))

;;; Configuration

(defparameter *tempo-timeouts*
  '(:leader 1200 :certification 2000 :retry 10000 :skip 11200 :view-retention 256)
  "Tempo's defaults in ms (crates/consensus/src/args.rs): --consensus.wait-for-proposal,
wait-for-notarizations, wait-to-rebroadcast-nullify, inactive-time-before-leader-skip and
views-to-track. :SKIP NIL disables leader skipping; :SKIP-BUDGET defaults to the number of
participants (commonware's SkipBudget::Participants).")

(defstruct (voter (:constructor %make-voter))
  (scheme nil)
  (epoch 0 :type (integer 0))
  (participants #() :type simple-vector)
  (n 0 :type (integer 0))
  (me nil)
  (app nil)
  (elector nil)
  (leader-timeout 0) (certification-timeout 0) (retry-timeout 0)
  (skip-timeout nil) (skip-budget 0) (view-retention 0)
  (genesis nil)
  (view 0 :type (integer 0))
  (last-finalized 0 :type (integer 0))
  (rounds (make-hash-table) :type hash-table)
  (candidates '())                      ; notarized views awaiting certification, ascending
  (pending-propose nil)                 ; context awaiting the application
  (pending-verify nil)                  ; (context . proposal) awaiting the application
  (events '())                          ; FIFO
  (effects '())                         ; reversed
  ;; batcher
  (last-activity #() :type simple-vector)
  (required-active 0)
  (hinted-view nil)                     ; current view whose leader-nullify hint was sent
  ;; resolver
  (resolver-floor 0) (resolver-current 0) (resolver-cursor 1)
  (requests '())                        ; outstanding (view . kind), kind :nullification|:notarization
  (highest-finalization nil)
  (reported-finalized 0))

(defmethod print-object ((voter voter) stream)
  (print-unreadable-object (voter stream :type t :identity t)
    (format stream "~:[observer~;#~:*~D~] epoch ~D view ~D finalized ~D" (voter-me voter)
            (voter-epoch voter) (voter-view voter) (voter-last-finalized voter))))

(defun make-voter (&key scheme (epoch 0) participants me application (elector :random-v1)
                     (timeouts *tempo-timeouts*) namespace (genesis (make-octets 32)))
  "A voter for EPOCH. SCHEME is a clempo.consensus scheme (a signer scheme when ME is a
participant index, a verifier scheme for an observer, ME NIL). PARTICIPANTS are the ed25519
public keys in index order. ELECTOR is an elector or :RANDOM-V1, :RANDOM-V0, :ROUND-ROBIN.
TIMEOUTS is a plist like *TEMPO-TIMEOUTS*. GENESIS is the payload of view 0, the parent of
the first proposal."
  (let* ((participants (coerce participants 'simple-vector))
         (n (length participants))
         (timeouts (append timeouts *tempo-timeouts*)))
    (invariant (= n (scheme-total scheme)) "~D participants for a scheme of ~D" n
               (scheme-total scheme))
    (invariant (or (null namespace) (octets= namespace (scheme-namespace scheme)))
               "namespace differs from the scheme's")
    (when me
      (invariant (and (scheme-share scheme) (eql me (scheme-me scheme)))
                 "participant ~D needs its own signer scheme" me))
    (invariant (= (length genesis) +digest-size+) "genesis payload must be 32 octets")
    (invariant (> (getf timeouts :certification) (getf timeouts :leader))
               "certification timeout must exceed the leader timeout")
    (%make-voter :scheme scheme :epoch epoch :participants participants :n n :me me
                 :app application :elector (elector-from-designator elector)
                 :leader-timeout (getf timeouts :leader)
                 :certification-timeout (getf timeouts :certification)
                 :retry-timeout (getf timeouts :retry)
                 :skip-timeout (getf timeouts :skip)
                 :skip-budget (if (getf timeouts :skip) (or (getf timeouts :skip-budget) n) 0)
                 :view-retention (getf timeouts :view-retention)
                 :genesis genesis
                 :last-activity (make-array n :initial-element nil)
                 :required-active (if me (1- (scheme-threshold scheme)) (scheme-threshold scheme)))))

;;; Small helpers

(defun now () (clempo.io:now-ms))

(defun quorum (voter) (scheme-threshold (voter-scheme voter)))

(defun round-at (voter view) (gethash view (voter-rounds voter)))

(defun ensure-round (voter view)
  (or (round-at voter view)
      (setf (gethash view (voter-rounds voter)) (make-vround view (voter-n voter)))))

(defun is-me (voter index) (and index (eql index (voter-me voter))))

(defun voter-leader-of (voter view)
  "The leader index of VIEW, once known."
  (let ((r (round-at voter view))) (and r (vround-leader r))))

(defun effect (voter &rest effect) (push effect (voter-effects voter)))

(defun queue-event (voter &rest event)
  (setf (voter-events voter) (append (voter-events voter) (list event))))

(defun this-round (voter view) (make-round :epoch (voter-epoch voter) :view view))

(defun min-active (voter)
  (max 0 (- (voter-last-finalized voter) (voter-view-retention voter))))

(defun retains-p (voter view) (and (plusp view) (>= view (min-active voter))))

(defun admits-vote-p (voter view)
  ;; Rotating terms: views up to the next one, plus the next-term lane of two more.
  (and (retains-p voter view) (<= view (+ (voter-view voter) 3))))

(defun admits-outbound-p (voter view) (<= view (1+ (voter-view voter))))

(defun sign (voter kind thing)
  (when (voter-me voter) (sign-vote (voter-scheme voter) kind thing)))

(defun certificate-seed (certificate)
  (vrf-signature-seed (etypecase certificate
                        (notarization (notarization-certificate certificate))
                        (nullification (nullification-certificate certificate))
                        (finalization (finalization-certificate certificate)))))

(defun certificate-kind (certificate)
  (etypecase certificate
    (notarization :notarization) (nullification :nullification) (finalization :finalization)))

;;; State (state.rs)

(defun enter-view (voter view)
  (when (> view (voter-view voter))
    (let ((r (ensure-round voter view)) (now (now)))
      (unless (vround-entered-at r) (setf (vround-entered-at r) now))
      (when (eq (vround-decision r) :pending)
        (setf (vround-leader-deadline r) (+ now (voter-leader-timeout voter))
              (vround-certification-deadline r) (+ now (voter-certification-timeout voter))))
      (setf (voter-view voter) view)
      t)))

(defun set-leader (voter view certificate)
  (let ((r (ensure-round voter view)))
    (unless (vround-leader r)
      (setf (vround-leader r)
            (elect (voter-elector voter) (voter-epoch voter) view (voter-n voter)
                   (and certificate (certificate-seed certificate)))))))

(defun add-notarization (voter notarization)
  (let ((view (proposal-view (notarization-proposal notarization))))
    (set-leader voter (1+ view) notarization)
    (when (round-add-notarization (ensure-round voter view) notarization)
      (when (> view (voter-last-finalized voter))
        (setf (voter-candidates voter) (merge 'list (list view)
                                              (remove view (voter-candidates voter)) #'<)))
      t)))

(defun add-nullification (voter nullification)
  (let ((view (round-view (nullification-round nullification))))
    (enter-view voter (1+ view))
    (set-leader voter (1+ view) nullification)
    (round-add-nullification (ensure-round voter view) nullification)))

(defun add-finalization (voter finalization)
  (let ((view (proposal-view (finalization-proposal finalization))))
    (when (> view (voter-last-finalized voter))
      (setf (voter-last-finalized voter) view
            (voter-candidates voter) (remove-if (lambda (v) (<= v view)) (voter-candidates voter)))
      (loop for r being the hash-values of (voter-rounds voter)
            when (and (<= (vround-view r) view) (eq (vround-certify r) :outstanding))
              do (setf (vround-certify r) :aborted)))
    (enter-view voter (1+ view))
    (set-leader voter (1+ view) finalization)
    (round-add-finalization (ensure-round voter view) finalization)))

(defun explicit-ancestry-payload (voter view)
  (if (zerop view)
      (voter-genesis voter)
      (let ((r (round-at voter view))) (and r (round-certified-payload r)))))

(defun first-unnullified-view (voter after before)
  (loop for v from (1+ after) below before
        unless (let ((r (round-at voter v))) (and r (vround-nullification r)))
          return v))

(defun find-parent (voter view)
  "(values parent-view payload) of the highest certified view below VIEW with every view
between nullified, or (values nil nil missing-view)."
  (loop for v downfrom (1- view) above 0
        for r = (round-at voter v)
        do (cond ((and r (round-certified-payload r))
                  (return (values v (round-certified-payload r))))
                 ((not (and r (vround-nullification r)))
                  (return (values nil nil v))))
        finally (return (values 0 (voter-genesis voter)))))

(defun parent-payload-for (voter view parent)
  "(values payload nil) for a valid certified parent of a VIEW proposal, else
\(values nil error): error is (:invalid reason) or (:missing view kind)."
  (cond ((<= view parent) (values nil '(:invalid :parent-not-before-proposal)))
        ((< parent (voter-last-finalized voter)) (values nil '(:invalid :parent-before-finalized)))
        (t (let ((missing (first-unnullified-view voter parent view)))
             (if missing
                 (values nil (list :missing missing :nullification))
                 (let ((payload (explicit-ancestry-payload voter parent)))
                   (if payload
                       (values payload nil)
                       (values nil (list :missing parent :notarization)))))))))

(defun parent-payload (voter proposal)
  (parent-payload-for voter (proposal-view proposal) (proposal-parent proposal)))

(defun verification-matches-p (voter view)
  (let* ((r (round-at voter view))
         (binding (and r (vround-verifying r))))
    (cond ((null binding) t)
          ((not (proposal= (vround-proposal r) (car binding))) nil)
          (t (let ((proposal (vround-proposal r)))
               (or (let ((p (parent-payload voter proposal))) (and p (octets= p (cdr binding))))
                   (and (is-me voter (vround-leader r))
                        (multiple-value-bind (pv pp) (find-parent voter view)
                          (and pv (= pv (proposal-parent proposal)) (octets= pp (cdr binding)))))))))))

(defun trigger-timeout (voter view reason)
  (when (>= view (voter-view voter))
    (let ((r (round-at voter view)))
      (when (and r (not (and (eq reason :inactivity) (unequivocated-proposal-p r))))
        (round-latch-timeout r (now) reason)))))

(defun has-skip-budget-p (voter)
  (< (- (voter-view voter) (1+ (voter-last-finalized voter))) (voter-skip-budget voter)))

(defun next-timeout (voter)
  "(values deadline reason) of the current view's next timeout, or NIL."
  (let ((r (round-at voter (voter-view voter))))
    (and r (round-next-timeout r (has-skip-budget-p voter)))))

(defun construct-nullify (voter)
  "(values nullify retry-p) for the current view, unless we voted to finalize it."
  (let ((r (ensure-round voter (voter-view voter))))
    (unless (eq (vround-decision r) :finalize)
      (let ((retry (eq (vround-decision r) :nullify)))
        (setf (vround-decision r) :nullify
              (vround-retry-deadline r) (+ (now) (voter-retry-timeout voter)))
        (let ((nullify (sign voter :nullify (this-round voter (voter-view voter)))))
          (and nullify (values nullify retry)))))))

(defun best-certificate (voter)
  "The certificate that lets peers enter our view: finalization, nullification, then
notarization of the previous view."
  (let* ((prev (1- (voter-view voter)))
         (r (and (plusp prev) (round-at voter prev))))
    (and r (or (vround-finalization r) (vround-nullification r) (vround-notarization r)))))

(defun construct-notarize (voter view)
  (let ((r (round-at voter view)))
    (when (and r (admits-outbound-p voter view) (can-construct-notarize-p r)
               (verification-matches-p voter view))
      (setf (vround-broadcast-notarize r) t)
      (sign voter :notarize (vround-proposal r)))))

(defun construct-finalize (voter view)
  (let ((r (round-at voter view)))
    (when (and r (> view (voter-last-finalized voter)) (admits-outbound-p voter view))
      (let ((proposal (round-construct-finalize r)))
        (and proposal (sign voter :finalize proposal))))))

(defun try-propose (voter)
  "The context of the lowest view we lead and can propose in now, claiming its build."
  (loop for view from (voter-view voter) to (1+ (voter-view voter))
        for r = (round-at voter view)
        when (and r (plusp view) (is-me voter (vround-leader r))
                  (not (eq (vround-decision r) :nullify))
                  (not (vround-requested-build r)) (null (vround-proposal r)))
          do (multiple-value-bind (pv pp) (find-parent voter view)
               (when pv
                 (setf (vround-requested-build r) t)
                 (return (make-context (this-round voter view) (vround-leader r)
                                       (svref (voter-participants voter) (vround-leader r))
                                       (cons pv pp)))))))

(defun preferred-parent-p (voter context)
  "True if CONTEXT's parent is what FIND-PARENT selects for its view now."
  (destructuring-bind (view . payload) (context-parent context)
    (multiple-value-bind (pv pp) (find-parent voter (context-view context))
      (and pv (= pv view) (octets= pp payload)))))

(defun captured-parent-valid-p (voter context)
  "True if CONTEXT's parent is still preferred or still valid ancestry for its view."
  (or (preferred-parent-p voter context)
      (destructuring-bind (view . payload) (context-parent context)
        (let ((p (parent-payload-for voter (context-view context) view)))
          (and p (octets= p payload))))))

(defun proposed (voter context payload)
  "Record our proposal if its captured parent is still valid."
  (let* ((view (context-view context))
         (parent (context-parent context))
         (r (round-at voter view)))
    (when (and r (captured-parent-valid-p voter context)
               (not (eq (vround-decision r) :nullify)))
      (let ((proposal (make-proposal :round (context-round context) :parent (car parent)
                                     :payload payload)))
        (when (slot-record-verified r proposal)
          (setf (vround-verifying r) (cons proposal (cdr parent)))
          t)))))

(defun set-proposal (voter view proposal)
  (let ((r (ensure-round voter view)))
    (and (not (eq (vround-decision r) :nullify))
         (eq (slot-update-vote r proposal) :new))))

(defun try-verify (voter)
  "The next verification: (values :ready context proposal), (values :resolve view kind) or NIL."
  (loop for view from (voter-view voter) to (1+ (voter-view voter))
        for r = (round-at voter view)
        when (and r (vround-leader r) (not (is-me voter (vround-leader r)))
                  (not (eq (vround-decision r) :nullify))
                  (not (vround-requested-verify r)) (vround-proposal r))
          do (let ((proposal (vround-proposal r)))
               (multiple-value-bind (payload err) (parent-payload voter proposal)
                 (cond ((and err (eq (first err) :invalid))
                        (trigger-timeout voter view :invalid-proposal))
                       (err
                        (destructuring-bind (missing kind) (rest err)
                          (unless (eql (vround-last-ancestry-request r) missing)
                            (setf (vround-last-ancestry-request r) missing)
                            (return (values :resolve missing kind)))))
                       (t
                        (setf (vround-requested-verify r) t)
                        (when (eq (vround-certify r) :ready)
                          (setf (vround-verifying r) (cons proposal payload))
                          (return (values :ready
                                          (make-context (proposal-round proposal) (vround-leader r)
                                                        (svref (voter-participants voter)
                                                               (vround-leader r))
                                                        (cons (proposal-parent proposal) payload))
                                          proposal)))))))))

(defun verification-stale-p (voter view)
  (unless (verification-matches-p voter view)
    (let ((r (round-at voter view))) (when r (setf (vround-verifying r) nil)))
    t))

(defun verified (voter view)
  (unless (verification-stale-p voter view)
    (let ((r (round-at voter view)))
      (when (and r (not (eq (vround-decision r) :nullify)) (eq (vround-status r) :unverified))
        (setf (vround-status r) :verified)))))

(defun certify-candidates (voter)
  "Notarized proposals ready for certification (taking the candidates)."
  (let ((views (shiftf (voter-candidates voter) '())))
    (loop for view in views
          for r = (round-at voter view)
          for proposal = (and r (vround-proposal r))
          when (and proposal (> view (voter-last-finalized voter))
                    (> view (proposal-parent proposal))
                    (vround-notarization r) (eq (vround-certify r) :ready))
            collect proposal)))

(defun certified (voter view ok)
  "Record a certification verdict; returns the view's notarization."
  (let ((r (round-at voter view)))
    (when r
      (setf (vround-certify r) (if ok :certified :rejected))
      (if ok
          (enter-view voter (1+ view))
          (trigger-timeout voter view :failed-certification))
      (vround-notarization r))))

(defun prune (voter)
  (let ((min (min-active voter)))
    (loop for view being the hash-keys of (voter-rounds voter)
          when (< view min) collect view into dead
          finally (dolist (v dead) (remhash v (voter-rounds voter))))
    (setf (voter-requests voter)
          (remove-if (lambda (req) (resolver-settled-p voter (car req) (cdr req)))
                     (voter-requests voter)))))

;;; Resolver (resolver/state.rs): fetch certificates the voter lacks.

(defun resolver-settled-p (voter view kind)
  (or (<= view (voter-last-finalized voter))
      (let ((r (round-at voter view)))
        (and r (ecase kind
                 (:nullification (vround-nullification r))
                 (:notarization (vround-notarization r)))))))

(defun request-certificate (voter view kind)
  (unless (or (resolver-settled-p voter view kind)
              (member (cons view kind) (voter-requests voter) :test #'equal))
    (push (cons view kind) (voter-requests voter))
    (effect voter :request view)))

(defun resolver-fetch-missing (voter)
  (let ((anchor (max (voter-resolver-cursor voter) (1+ (voter-resolver-floor voter)))))
    (loop while (< anchor (voter-resolver-current voter))
          do (request-certificate voter anchor :nullification)
             (incf anchor))
    (setf (voter-resolver-cursor voter) anchor)))

(defun resolver-updated (voter certificate)
  (let ((view (round-view (certificate-round certificate))))
    (setf (voter-resolver-current voter) (max view (voter-resolver-current voter)))
    (when (typep certificate 'finalization)
      (let ((best (voter-highest-finalization voter)))
        (when (or (null best) (> view (proposal-view (finalization-proposal best))))
          (setf (voter-highest-finalization voter) certificate)))
      (setf (voter-resolver-floor voter) (max view (voter-resolver-floor voter))))
    (resolver-fetch-missing voter)))

(defun resolver-certified (voter notarization ok)
  (let ((view (proposal-view (notarization-proposal notarization))))
    (cond (ok (setf (voter-resolver-floor voter) (max view (voter-resolver-floor voter)))
              (resolver-fetch-missing voter))
          ((> view (voter-resolver-floor voter))
           (request-certificate voter view :nullification)))))

(defun repeat-requests (voter)
  "Ask again for every certificate still missing (commonware's resolver retries fetches)."
  (dolist (req (reverse (voter-requests voter)))
    (unless (resolver-settled-p voter (car req) (cdr req))
      (effect voter :request (car req)))))

(defun voter-certificate (voter view)
  "The certificate this voter serves for a request about VIEW (resolver produce): the highest
finalization if it covers VIEW, else VIEW's certified notarization or its nullification."
  (let ((f (voter-highest-finalization voter))
        (r (round-at voter view)))
    (cond ((and f (<= view (proposal-view (finalization-proposal f)))) f)
          ((and r (vround-notarization r) (eq (vround-certify r) :certified))
           (vround-notarization r))
          (r (vround-nullification r)))))

;;; Batcher (batcher/actor.rs, round.rs, verifier.rs): votes in, certificates out.

(defun record-activity (voter index)
  (when index (setf (svref (voter-last-activity voter) index) (now))))

(defun active-p (voter index)
  (let* ((min-time (- (now) (voter-skip-timeout voter)))
         (recent (lambda (at) (and at (>= at min-time)))))
    (or (< (count-if recent (voter-last-activity voter)) (voter-required-active voter))
        (funcall recent (svref (voter-last-activity voter) index)))))

(defun leader-nullified-p (voter)
  (let* ((view (voter-view voter))
         (r (round-at voter view)))
    (and r (vround-leader r) (not (eql (voter-hinted-view voter) view))
         (round-vote r :nullify (vround-leader r)))))

(defun hint-leader-nullify (voter)
  (when (and (voter-skip-timeout voter) (leader-nullified-p voter))
    (setf (voter-hinted-view voter) (voter-view voter))
    (queue-event voter :timeout-hint (voter-view voter) :leader-nullify)))

(defun batcher-update (voter)
  "The voter entered a new view: hint a timeout if its leader nullified or is inactive."
  (let* ((view (voter-view voter))
         (leader (voter-leader-of voter view)))
    (when (and (voter-skip-timeout voter) leader)
      (cond ((leader-nullified-p voter) (hint-leader-nullify voter))
            ((and (not (is-me voter leader)) (not (active-p voter leader)))
             (queue-event voter :timeout-hint view :inactivity))))
    (batcher-process voter view)))

(defun accept-vote (voter vote verified)
  "Record VOTE in its round (at most one per kind and signer, never both nullify and finalize).
True if it was new."
  (let* ((r (ensure-round voter (proposal-or-round-view vote)))
         (signer (vote-signer vote))
         (kind (vote-kind vote))
         (slots (kind-votes r kind)))
    (when (and (< signer (voter-n voter))
               (null (svref slots signer))
               (not (member signer (vround-invalid r)))
               (case kind
                 (:nullify (null (round-vote r :finalize signer)))
                 (:finalize (null (round-vote r :nullify signer)))
                 (t t)))
      (setf (svref slots signer) (cons vote verified))
      t)))

(defun proposal-or-round-view (vote) (round-view (vote-round vote)))

(defun batcher-proposal (voter r)
  "The proposal votes of R are filtered to: a certificate's, else the leader's notarize
\(known once R is the current view)."
  (cond ((vround-notarization r) (notarization-proposal (vround-notarization r)))
        ((vround-finalization r) (finalization-proposal (vround-finalization r)))
        ((and (= (vround-view r) (voter-view voter)) (vround-leader r))
         (let ((vote (round-vote r :notarize (vround-leader r))))
           (and vote (notarize-proposal vote))))))

(defun assemble-verified (voter r kind subject-test)
  "A certificate from a quorum of R's KIND votes satisfying SUBJECT-TEST, verifying
optimistically: assemble, check the certificate, and only if it fails check each vote."
  (let ((scheme (voter-scheme voter)) (q (quorum voter)) (slots (kind-votes r kind)))
    (loop
      (let* ((entries (loop for e across slots
                            when (and e (funcall subject-test (car e))) collect e))
             (chosen (subseq (stable-sort (copy-list entries)
                                          (lambda (a b) (and (cdr a) (not (cdr b)))))
                             0 (min q (length entries)))))
        (when (< (length chosen) q) (return nil))
        (setf chosen (sort chosen #'< :key (lambda (e) (vote-signer (car e)))))
        (let ((certificate (assemble-from-votes scheme (mapcar #'car chosen))))
          (when (or (every #'cdr chosen) (verify-artifact scheme certificate))
            (dolist (e chosen) (setf (cdr e) t))
            (return certificate))
          (dolist (e chosen)
            (unless (cdr e)
              (let ((signer (vote-signer (car e))))
                (if (verify-vote scheme (car e))
                    (setf (cdr e) t)
                    (setf (svref slots signer) nil
                          (vround-invalid r) (adjoin signer (vround-invalid r))))))))))))

(defun distinct-proposals (r kind)
  (let ((seen '()))
    (loop for e across (kind-votes r kind)
          when e do (pushnew (vote-proposal (car e)) seen :test #'proposal=))
    (nreverse seen)))

(defun try-construct-kind (voter r kind)
  (if (eq kind :nullify)
      (assemble-verified voter r :nullify (constantly t))
      (let ((selected (batcher-proposal voter r)))
        (loop for p in (if selected (list selected) (distinct-proposals r kind))
              for c = (assemble-verified voter r kind
                                         (lambda (vote) (proposal= (vote-proposal vote) p)))
              when c return c))))

(defun batcher-process (voter view)
  "Forward the leader's proposal and assemble any certificate VIEW's votes now allow."
  (let ((r (round-at voter view)))
    (when (and r (> view (voter-last-finalized voter)))
      (when (and (voter-me voter) (not (vround-proposal-sent r)))
        (let ((proposal (batcher-proposal voter r)))
          (when (and proposal (not (is-me voter (vround-leader r))))
            (setf (vround-proposal-sent r) t)
            (queue-event voter :proposal proposal))))
      (when (<= view (+ (voter-view voter) 3))
        (loop for (kind have) in '((:notarize vround-notarization)
                                   (:nullify vround-nullification)
                                   (:finalize vround-finalization))
              unless (or (funcall have r) (member kind (vround-assembled r)))
                do (let ((c (try-construct-kind voter r kind)))
                     (when c
                       (push kind (vround-assembled r))
                       (queue-event voter :certificate c))))))))

(defun batcher-network-vote (voter sender vote)
  (let ((view (round-view (vote-round vote))))
    (when (and sender (= (round-epoch (vote-round vote)) (voter-epoch voter)))
      (record-activity voter sender)
      (when (and (admits-vote-p voter view) (= sender (vote-signer vote))
                 (accept-vote voter vote nil))
        (hint-leader-nullify voter)
        (batcher-process voter view)))))

(defun batcher-network-certificate (voter sender certificate)
  (let* ((round (certificate-round certificate))
         (view (round-view round)))
    (when (= (round-epoch round) (voter-epoch voter))
      (record-activity voter sender)
      (let ((r (round-at voter view))
            (kind (ecase (certificate-kind certificate)
                    (:notarization :notarize) (:nullification :nullify) (:finalization :finalize))))
        (when (and (retains-p voter view)
                   (not (and r (member kind (vround-assembled r))))
                   (verify-artifact (voter-scheme voter) certificate))
          (push kind (vround-assembled (ensure-round voter view)))
          (queue-event voter :certificate certificate))))))

;;; Actor (actor.rs)

(defun handle-certificate (voter certificate)
  "Record a verified certificate; true if new."
  (let ((added (etypecase certificate
                 (notarization (add-notarization voter certificate))
                 (nullification (add-nullification voter certificate))
                 (finalization (add-finalization voter certificate)))))
    (when added (resolver-updated voter certificate))
    added))

(defun process-event (voter event)
  "Handle EVENT; returns (values view nullify certification) where VIEW is the view whose
votes and certificates to construct, or NIL when there is nothing to construct."
  (destructuring-bind (kind &rest args) event
    (ecase kind
      (:proposal
       (let* ((proposal (first args)) (view (proposal-view proposal)))
         (and (admits-vote-p voter view) (set-proposal voter view proposal) view)))
      (:certificate
       (let ((view (round-view (certificate-round (first args)))))
         (when (retains-p voter view)
           (handle-certificate voter (first args))
           view)))
      (:timeout-hint
       (destructuring-bind (view reason) args
         (trigger-timeout voter view reason)
         view))
      (:timeout
       (multiple-value-bind (nullify retry) (construct-nullify voter)
         (values (voter-view voter)
                 (and nullify (cons nullify (and retry (best-certificate voter))))
                 nil
                 retry)))
      (:proposed
       (destructuring-bind (context payload) args
         (when (eq context (voter-pending-propose voter))
           (setf (voter-pending-propose voter) nil)
           (let ((view (context-view context)))
             (cond ((null payload) (trigger-timeout voter view :missing-proposal) nil)
                   ((< view (voter-view voter)) nil)
                   ((proposed voter context payload) view))))))
      (:verified
       (destructuring-bind (context ok) args
         (when (eq context (car (voter-pending-verify voter)))
           (setf (voter-pending-verify voter) nil)
           (let ((view (context-view context)))
             (if ok
                 (verified voter view)
                 (unless (verification-stale-p voter view)
                   (trigger-timeout voter view :invalid-proposal)))
             view))))
      (:certified
       (destructuring-bind (view ok) args
         (let ((r (round-at voter view)))
           (when (and r (eq (vround-certify r) :outstanding))
             (let ((notarization (certified voter view ok)))
               (values view nil (cons ok notarization))))))))))

(defun reconcile (voter)
  "Drop obsolete application requests and issue new ones (reconcile_application_requests)."
  (let ((view (voter-view voter)))
    (let ((pending (voter-pending-propose voter)))
      (cond ((null pending))
            ((< (context-view pending) view) (setf (voter-pending-propose voter) nil))
            ;; a replacement parent is selectable: release the build latch and drop it
            ((and (find-parent voter (context-view pending))
                  (not (captured-parent-valid-p voter pending)))
             (let ((r (round-at voter (context-view pending))))
               (when r (setf (vround-requested-build r) nil)))
             (setf (voter-pending-propose voter) nil))))
    (let ((pending (voter-pending-verify voter)))
      (when (and pending
                 (let* ((pview (context-view (car pending)))
                        (r (round-at voter pview)))
                   (or (< pview view)
                       (and r (vround-verifying r)
                            (not (proposal= (vround-proposal r) (car (vround-verifying r))))))))
        (setf (voter-pending-verify voter) nil)))
    (unless (voter-pending-propose voter)
      (let ((context (try-propose voter)))
        (when context
          (setf (voter-pending-propose voter) context)
          (let ((answer (app-propose (voter-app voter) context)))
            (unless (eq answer :pending)
              (queue-event voter :proposed context answer))))))
    (unless (voter-pending-verify voter)
      (multiple-value-bind (what a b) (try-verify voter)
        (case what
          (:resolve (request-certificate voter a b))
          (:ready
           (setf (voter-pending-verify voter) (cons a b))
           (let ((answer (app-verify (voter-app voter) a (proposal-payload b))))
             (unless (eq answer :pending)
               (queue-event voter :verified a (and answer t))))))))))

(defun on-start (voter)
  (reconcile voter)
  (dolist (proposal (certify-candidates voter))
    (let ((r (round-at voter (proposal-view proposal))))
      (setf (vround-certify r) :outstanding)
      (let ((answer (app-certify (voter-app voter) (proposal-round proposal)
                                 (proposal-payload proposal))))
        (unless (eq answer :pending)
          (queue-event voter :certified (proposal-view proposal) (and answer t))))))
  (prune voter))

(defun publish-vote (voter vote)
  (accept-vote voter vote t)
  (effect voter :broadcast vote)
  (batcher-process voter (round-view (vote-round vote))))

(defun on-end (voter view nullify certification retry)
  "Construct what became available for VIEW, then publish it (construct, notify)."
  (let* ((notarize (construct-notarize voter view))
         (r (round-at voter view))
         (notarization (and r (take-once (vround-broadcast-notarization r) (vround-notarization r))))
         (nullification (and r (take-once (vround-broadcast-nullification r)
                                          (vround-nullification r))))
         (finalize (construct-finalize voter view))
         (finalization (and r (take-once (vround-broadcast-finalization r)
                                         (vround-finalization r)))))
    (reconcile voter)
    (when certification
      (resolver-certified voter (cdr certification) (car certification)))
    (when nullify
      (publish-vote voter (car nullify))
      (when (cdr nullify) (effect voter :broadcast (cdr nullify)))
      (when retry (repeat-requests voter)))
    (when notarize (publish-vote voter notarize))
    (when notarization
      (effect voter :broadcast notarization)
      (effect voter :notarized notarization))
    (when nullification
      (effect voter :broadcast nullification)
      (effect voter :nullified nullification))
    (when finalize (publish-vote voter finalize))
    (when finalization
      (effect voter :broadcast finalization)
      (when (> view (voter-reported-finalized voter))
        (setf (voter-reported-finalized voter) view)
        (effect voter :finalized finalization)))))

(defun pump (voter)
  "Run the event loop until no event is queued and no timeout is due."
  (loop
    (on-start voter)
    (let ((event (pop (voter-events voter)))
          (start (voter-view voter)))
      (unless event
        (multiple-value-bind (deadline reason) (next-timeout voter)
          (if (and deadline (<= deadline (now)))
              (setf event (list :timeout reason))
              (return))))
      (multiple-value-bind (view nullify certification retry) (process-event voter event)
        (when view
          (on-end voter view nullify certification retry)
          (when (> (voter-view voter) start)
            (batcher-update voter)))))))

(defun take-effects (voter)
  (nreverse (shiftf (voter-effects voter) '())))

;;; Interface

(defun voter-start (voter)
  "Enter view 1 on the genesis payload; returns effects."
  (enter-view voter 1)
  (set-leader voter 1 nil)
  (batcher-update voter)
  (pump voter)
  (take-effects voter))

(defun participant-index (voter sender)
  (etypecase sender
    (null nil)
    (integer (and (< -1 sender (voter-n voter)) sender))
    (vector (position sender (voter-participants voter) :test #'octets=))))

(defun voter-receive (voter sender message)
  "Deliver MESSAGE, a vote or certificate, from SENDER (a participant index or ed25519 public
key; NIL for a certificate from outside the committee). Returns effects."
  (let ((sender (participant-index voter sender)))
    (etypecase message
      ((or notarize nullify finalize) (batcher-network-vote voter sender message))
      ((or notarization nullification finalization)
       (batcher-network-certificate voter sender message))))
  (pump voter)
  (take-effects voter))

(defun voter-tick (voter)
  "Fire due timeouts at (clempo.io:now-ms); returns effects."
  (pump voter)
  (take-effects voter))

(defun voter-next-deadline (voter)
  "When VOTER-TICK next has work (ms), or NIL."
  (values (next-timeout voter)))

(defun voter-proposed (voter round payload)
  "Answer a :PENDING app-propose for ROUND with PAYLOAD (NIL: no proposal). Returns effects."
  (let ((context (voter-pending-propose voter)))
    (when (and context (round= round (context-round context)))
      (queue-event voter :proposed context payload)))
  (pump voter)
  (take-effects voter))

(defun voter-verified (voter round payload ok)
  "Answer a :PENDING app-verify of PAYLOAD in ROUND. Returns effects."
  (let ((pending (voter-pending-verify voter)))
    (when (and pending (round= round (context-round (car pending)))
               (octets= payload (proposal-payload (cdr pending))))
      (queue-event voter :verified (car pending) ok)))
  (pump voter)
  (take-effects voter))

(defun voter-certified (voter round payload ok)
  "Answer a :PENDING app-certify of PAYLOAD in ROUND. Returns effects."
  (let ((r (round-at voter (round-view round))))
    (when (and r (= (round-epoch round) (voter-epoch voter))
               (eq (vround-certify r) :outstanding)
               (octets= payload (proposal-payload (vround-proposal r))))
      (queue-event voter :certified (round-view round) ok)))
  (pump voter)
  (take-effects voter))
