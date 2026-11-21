(in-package #:clempo.simplex)

;;; One view's state: the voter's Round and proposal Slot (actors/voter/{round,slot}.rs)
;;; and the batcher's vote tracker for the same view (actors/batcher/{round,votes}.rs).
;;;
;;; Proposal status: :none, :unverified, :verified (built locally or verified by the
;;; application), :equivocated (conflicting proposals seen: no notarize or finalize vote).
;;; Decision: :pending (neither vote cast), :nullify or :finalize. No honest participant votes
;;; both nullify and finalize in a view, and neither vote is retracted.
;;; Certify: :ready, :outstanding, :certified, :rejected or :aborted (a finalization at or
;;; above the view made it moot).

(defstruct (vround (:constructor make-vround
                       (view n &aux (votes (vector (make-array n :initial-element nil)
                                                   (make-array n :initial-element nil)
                                                   (make-array n :initial-element nil))))))
  (view 0 :type (integer 0))
  (leader nil :type (or null (integer 0)))
  ;; proposal slot
  (proposal nil)
  (status :none :type (member :none :unverified :verified :equivocated))
  (requested-build nil)
  (requested-verify nil)
  ;; our votes and deadlines (absolute ms)
  (decision :pending :type (member :pending :nullify :finalize))
  (entered-at nil)
  (leader-deadline nil)
  (certification-deadline nil)
  (latch nil)                           ; (deadline . reason), first explicit timeout
  (retry-deadline nil)
  (broadcast-notarize nil)
  ;; certificates
  (notarization nil)
  (broadcast-notarization nil)
  (nullification nil)
  (broadcast-nullification nil)
  (finalization nil)
  (broadcast-finalization nil)
  (certify :ready :type (member :ready :outstanding :certified :rejected :aborted))
  (last-ancestry-request nil)
  (verifying nil)                       ; (proposal . parent-payload) the vote is bound to
  ;; batcher: at most one vote of each kind per signer, with its verification state
  (votes #() :type simple-vector)  ; per kind (notarize nullify finalize): signer -> entry
  (invalid '())                          ; signers whose attestation failed verification
  (proposal-sent nil)
  (assembled '()))                       ; vote kinds whose certificate the batcher holds

(defmethod print-object ((r vround) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "view ~D leader ~A ~(~A ~A ~A~)" (vround-view r) (vround-leader r)
            (vround-status r) (vround-decision r) (vround-certify r))))

(defun proposal= (a b)
  (and a b
       (round= (proposal-round a) (proposal-round b))
       (= (proposal-parent a) (proposal-parent b))
       (octets= (proposal-payload a) (proposal-payload b))))

;;; Slot

(defun slot-record-verified (r proposal)
  "Record a proposal that needs no verification (our own). False if the slot holds another."
  (let ((existing (vround-proposal r)))
    (cond ((null existing) (setf (vround-proposal r) proposal))
          ((not (proposal= existing proposal)) (return-from slot-record-verified nil))))
  (unless (eq (vround-status r) :equivocated)
    (setf (vround-status r) :verified))
  (setf (vround-requested-build r) t (vround-requested-verify r) t)
  t)

(defun slot-update-vote (r proposal)
  "Record a proposal seen in the leader's notarize: :new, :unchanged, :equivocated or NIL."
  (let ((existing (vround-proposal r)))
    (cond ((eq (vround-status r) :equivocated) nil)
          ((null existing)
           (setf (vround-proposal r) proposal (vround-status r) :unverified)
           :new)
          ((proposal= existing proposal) :unchanged)
          (t (setf (vround-status r) :equivocated) :equivocated))))

(defun slot-update-certificate (r proposal)
  "Record a proposal recovered from a certificate, which is authoritative."
  (let ((existing (vround-proposal r)))
    (cond ((null existing)
           (setf (vround-proposal r) proposal (vround-status r) :unverified)
           :new)
          ((proposal= existing proposal) :unchanged)
          (t (setf (vround-proposal r) proposal
                   (vround-requested-build r) t
                   (vround-requested-verify r) t
                   (vround-status r) :equivocated)
             :equivocated))))

(defun unequivocated-proposal-p (r)
  (and (vround-proposal r) (not (eq (vround-status r) :equivocated))))

;;; Certificates and ancestry

(defun round-certified-payload (r)
  "The payload this round lets children build on: finalized, or certified by us."
  (when (or (vround-finalization r) (eq (vround-certify r) :certified))
    (proposal-payload (vround-proposal r))))

(defun round-add-notarization (r notarization)
  (unless (vround-notarization r)
    (slot-update-certificate r (notarization-proposal notarization))
    (setf (vround-notarization r) notarization)
    t))

(defun round-add-nullification (r nullification)
  (unless (vround-nullification r)
    (setf (vround-nullification r) nullification)
    t))

(defun round-add-finalization (r finalization)
  (unless (vround-finalization r)
    (slot-update-certificate r (finalization-proposal finalization))
    (setf (vround-finalization r) finalization)
    t))

(defmacro take-once (flag value)
  "VALUE the first time FLAG is false (setting it), else NIL."
  `(let ((v ,value))
     (when (and v (not ,flag))
       (setf ,flag t)
       v)))

;;; Votes we may cast

(defun can-construct-notarize-p (r)
  (and (not (vround-broadcast-notarize r))
       (not (eq (vround-decision r) :nullify))
       (eq (vround-status r) :verified)))

(defun round-construct-finalize (r)
  "The proposal to finalize, marking the decision, if the round allows a finalize vote."
  (when (and (eq (vround-decision r) :pending)
             (unequivocated-proposal-p r)
             (vround-notarization r)
             (eq (vround-certify r) :certified))
    (setf (vround-decision r) :finalize)
    (vround-proposal r)))

;;; Timeouts

(defun round-latch-timeout (r now reason)
  (when (and (eq (vround-decision r) :pending) (null (vround-latch r)))
    (setf (vround-latch r) (cons now reason))))

(defun round-next-timeout (r allow-latched)
  "(values deadline reason) of the round's next local timeout, or NIL."
  (cond ((vround-finalization r) nil)
        ((eq (vround-decision r) :finalize) nil)
        ((eq (vround-decision r) :nullify) (values (vround-retry-deadline r) :retry))
        ((and (vround-latch r) allow-latched)
         (values (car (vround-latch r)) (cdr (vround-latch r))))
        ((and (vround-leader-deadline r) (null (vround-proposal r)))
         (values (vround-leader-deadline r) :leader-timeout))
        ((and (vround-certification-deadline r) (not (eq (vround-certify r) :certified)))
         (values (vround-certification-deadline r) :certification-timeout))))

;;; Batcher votes

(defun vote-kind (vote)
  (etypecase vote (notarize :notarize) (nullify :nullify) (finalize :finalize)))

(defun vote-proposal (vote)
  (etypecase vote
    (notarize (notarize-proposal vote))
    (finalize (finalize-proposal vote))
    (nullify nil)))

(defun kind-votes (r kind)
  "The vector, by signer, of (vote . verified-p) entries of KIND."
  (svref (vround-votes r) (ecase kind (:notarize 0) (:nullify 1) (:finalize 2))))

(defun round-vote (r kind signer)
  (car (svref (kind-votes r kind) signer)))
