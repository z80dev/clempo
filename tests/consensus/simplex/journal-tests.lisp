(in-package #:clempo.simplex.tests)

;;; The voter journal: artifacts encode as commonware's Artifact, and a voter restarted from its
;;; journal never votes against what it sent before the restart.

(defun golden-schemes ()
  (multiple-value-bind (coefficients total shares participants) (golden-sharing)
    (values (mapcar (lambda (share) (make-signer-scheme coefficients total share)) shares)
            participants)))

(defun test-proposal (view payload-byte &key (parent 0) (epoch 0))
  (make-proposal :round (make-round :epoch epoch :view view) :parent parent
                 :payload (make-array 32 :element-type 'octet :initial-element payload-byte)))

(deftest artifact-codec ()
  (let* ((schemes (golden-schemes))
         (p (test-proposal 17 #xab :parent 16 :epoch 3))
         (notarize (sign-vote (first schemes) :notarize p))
         (nullify (sign-vote (first schemes) :nullify (proposal-round p)))
         (finalize (sign-vote (first schemes) :finalize p))
         (certification (make-certification-record :round (make-round :epoch 3 :view 17) :ok t)))
    ;; tag then the value, as simplex::types::Artifact
    (is= "00" (subseq (octets->hex (encode-artifact notarize) :prefix nil) 0 2))
    (is-equalp (subseq (cw-encode 'vote notarize) 1) (subseq (encode-artifact notarize) 1))
    (is= "03" (subseq (octets->hex (encode-artifact nullify) :prefix nil) 0 2))
    (is= "05" (subseq (octets->hex (encode-artifact finalize) :prefix nil) 0 2))
    (is= "02031101" (octets->hex (encode-artifact certification) :prefix nil))
    (let ((cert (assemble-from-votes (first schemes)
                                     (loop for s in (subseq schemes 0 3)
                                           collect (sign-vote s :finalize p)))))
      (is= "06" (subseq (octets->hex (encode-artifact cert) :prefix nil) 0 2))
      (dolist (a (list notarize nullify finalize certification cert))
        (is-equalp a (decode-artifact (encode-artifact a)))
        (is= 17 (artifact-view a))))))

(deftest memory-journal-durability ()
  (let ((j (make-memory-journal)))
    (journal-append j 0 2 (octets-of 1))
    (journal-append j 0 1 (octets-of 2))
    (journal-append j 1 1 (octets-of 9))
    (journal-sync j 0)
    (journal-append j 0 1 (octets-of 3))
    (memory-journal-drop-unsynced j)
    ;; view order, append order within a view; unsynced appends are lost; epochs apart
    (is-equalp (list (octets-of 2) (octets-of 1)) (journal-replay j 0))
    (is= '() (journal-replay j 1))
    (journal-prune j 0 2)
    (is-equalp (list (octets-of 1)) (journal-replay j 0))))

(defclass accept-app (chain-app) ()
  (:documentation "Verifies every proposal."))

(defmethod app-verify ((app accept-app) context payload)
  (declare (ignore context payload))
  t)

(defun lone-voter (journal &key (me 0))
  (multiple-value-bind (schemes participants) (golden-schemes)
    (make-voter :scheme (nth me schemes) :participants participants :me me
                :application (make-instance 'accept-app) :journal journal
                :timeouts (list* :skip nil *tempo-timeouts*))))

(defun broadcast-votes (effects kind)
  (loop for (what message) in effects
        when (and (eq what :broadcast) (typep message kind)) collect message))

(deftest restarted-voter-keeps-its-notarize ()
  ;; View 1's leader (participant 1) equivocates around participant 0's restart.
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 7))
         (clempo.io:*clock* (clempo.io:make-manual-clock 1000000))
         (leader (second (golden-schemes)))
         (p (sign-vote leader :notarize (test-proposal 1 1)))
         (other (sign-vote leader :notarize (test-proposal 1 2)))
         (journal (make-memory-journal))
         (voter (lone-voter journal)))
    (voter-start voter)
    (is= 1 (voter-leader-of voter 1))
    (let ((votes (broadcast-votes (voter-receive voter 1 p) 'notarize)))
      (is= 1 (length votes))
      (is (octets= (proposal-payload (notarize-proposal p))
                   (proposal-payload (notarize-proposal (first votes))))))
    ;; restart from the journal: still in view 1, and the other proposal gets no vote
    (let ((restarted (lone-voter journal)))
      (is= '() (voter-start restarted))
      (is= 1 (voter-view restarted))
      (is= '() (broadcast-votes (voter-receive restarted 1 other) 'notarize)))
    ;; without the journal the same voter would have signed it
    (let ((amnesiac (lone-voter nil)))
      (voter-start amnesiac)
      (is= 1 (length (broadcast-votes (voter-receive amnesiac 1 other) 'notarize))))))

(deftest restarted-voter-keeps-its-nullify ()
  (let ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 7)))
    (multiple-value-bind (clock handle) (clempo.io:make-manual-clock 1000000)
      (let* ((clempo.io:*clock* clock)
             (p (sign-vote (second (golden-schemes)) :notarize (test-proposal 1 1)))
             (journal (make-memory-journal))
             (voter (lone-voter journal)))
        (voter-start voter)
        (clempo.io:manual-clock-advance handle 1200)
        (is= 1 (length (broadcast-votes (voter-tick voter) 'nullify)))
        (let ((restarted (lone-voter journal)))
          (voter-start restarted)
          ;; the late proposal gets no notarize, and the nullify is retried, not re-decided
          (is= '() (broadcast-votes (voter-receive restarted 1 p) 'notarize))
          (is= (+ (funcall clock) 10000) (voter-next-deadline restarted))
          (clempo.io:manual-clock-advance handle 10000)
          (is= 1 (length (broadcast-votes (voter-tick restarted) 'nullify))))))))

(defun restart-voter (net i)
  "Replace voter I by a new voter over the same journal; returns its start effects."
  (let* ((old (net-voter net i))
         (new (make-voter :scheme (clempo.simplex::voter-scheme old)
                          :participants (clempo.simplex::voter-participants old)
                          :me (voter-me old) :application (clempo.simplex::voter-app old)
                          :elector (clempo.simplex::voter-elector old)
                          :timeouts (list* :skip nil *tempo-timeouts*)
                          :journal (voter-journal old))))
    (setf (aref (net-voters net) i) new)
    (let ((clempo.io:*clock* (net-clock net)))
      (voter-start new))))

(deftest restarted-voter-resumes ()
  ;; A voter restarted mid-run recovers its view and finalized chain from its journal and
  ;; keeps finalizing with the others.
  (with-test-network (net :timeouts (list* :skip nil *tempo-timeouts*) :journals t)
    (start-network net)
    (is (run-network net (all-finalized-p net '(0 1 2 3) 2)))
    (let* ((before (voter-view (net-voter net 0)))
           (effects (restart-voter net 0))
           (voter (net-voter net 0)))
      (is= before (voter-view voter))
      (is (>= (voter-last-finalized voter) 2))
      (is= '(1 2) (subseq (loop for (kind c) in effects
                                when (eq kind :finalized)
                                  collect (round-view (certificate-round c)))
                          0 2))
      (handle-effects net 0 effects)
      (is (run-network net (all-finalized-p net '(0 1 2 3) (+ 2 before)))))))
