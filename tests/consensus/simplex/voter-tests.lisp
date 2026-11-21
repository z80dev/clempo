(in-package #:clempo.simplex.tests)

;;; A tiny network of voters exchanging messages directly, on a manual clock and seeded
;;; entropy. Threshold shares: the Rust harness's (4 participants, threshold 3).

(defun golden-sharing ()
  "(values coefficients total shares participants) of tests/consensus/data/rust_golden.json."
  (let* ((g (clempo.json:parse-json (clempo.io:read-file-string
                                     (test-path "data/rust_golden.json"))))
         (r (make-cw-reader (hex->octets (clempo.json:jget g "output")) :start 32)))
    (read-u8 r)                         ; sharing mode
    (let* ((total (read-uint r 4))
           (coefficients (loop repeat (read-length r) collect (read-raw r 96))))
      (values coefficients total
              (loop for s across (clempo.json:jget g "shares")
                    collect (let ((o (hex->octets s)))
                              (cons (aref o 0) (octets->integer o :start 1))))
              (map 'list #'hex->octets (clempo.json:jget g "participants"))))))

(defclass chain-app () ()
  (:documentation "Payloads commit to their parent and view, so every honest voter verifies
the same proposals."))

(defun chain-payload (parent-payload view)
  (clempo.sha2:sha256 (octets-concat parent-payload (integer->octets view 8))))

(defun payload-for (context)
  (chain-payload (cdr (context-parent context)) (context-view context)))

(defmethod app-propose ((app chain-app) context) (payload-for context))

(defmethod app-verify ((app chain-app) context payload)
  (octets= payload (payload-for context)))

(defstruct net
  voters
  (offline '())                         ; indices whose messages are dropped both ways
  (queue '())                           ; (to from message), FIFO
  (finalized (make-hash-table))         ; view -> payload, across voters
  (conflicts 0)
  (requests '())                        ; (voter . view) of every :request effect
  (reports '())                         ; (voter kind view parent payload), newest first
  clock                                 ; the clock function bound to clempo.io:*clock*
  clock-handle)

(defclass deferred-app (chain-app)
  ((net :accessor app-net) (index :accessor app-index))
  (:documentation "Answers verify and certify later, through the network queue."))

(defun defer (app thunk)
  (let ((net (app-net app)))
    (setf (net-queue net) (nconc (net-queue net) (list (list (app-index app) :answer thunk))))
    :pending))

(defmethod app-verify ((app deferred-app) context payload)
  (let ((ok (call-next-method)) (voter (net-voter (app-net app) (app-index app))))
    (defer app (lambda () (voter-verified voter (context-round context) payload ok)))))

(defmethod app-certify ((app deferred-app) round payload)
  (let ((voter (net-voter (app-net app) (app-index app))))
    (defer app (lambda () (voter-certified voter round payload t)))))

(defun make-network (&key (elector :random-v1) (epoch 0) timeouts (app-class 'chain-app)
                       observer journals)
  "Four participants, and with OBSERVER a fifth voter (index 4) that only follows. With
JOURNALS every voter journals to its own memory journal."
  (multiple-value-bind (coefficients total shares participants) (golden-sharing)
    (multiple-value-bind (clock handle) (clempo.io:make-manual-clock 1000000)
      (let* ((apps (loop repeat (if observer 5 4) collect (make-instance app-class)))
             (net (make-net
                   :clock clock :clock-handle handle
                   :voters (coerce
                            (loop for share in (if observer (append shares '(nil)) shares)
                                  for app in apps
                                  collect (make-voter
                                           :scheme (if share
                                                       (make-signer-scheme coefficients total share)
                                                       (make-verifier-scheme coefficients total))
                                           :epoch epoch :participants participants
                                           :me (car share) :application app :elector elector
                                           :timeouts (or timeouts *tempo-timeouts*)
                                           :journal (and journals (make-memory-journal))))
                            'vector))))
        (loop for app in apps for i from 0
              when (typep app 'deferred-app)
                do (setf (app-net app) net (app-index app) i))
        net))))

(defun net-voter (net i) (aref (net-voters net) i))

(defun offline-p (net i) (member i (net-offline net)))

(defun handle-effects (net from effects)
  (dolist (e effects)
    (ecase (first e)
      (:broadcast
       (dotimes (to (length (net-voters net)))
         (unless (= to from)
           (setf (net-queue net) (nconc (net-queue net) (list (list to from (second e))))))))
      ((:notarized :nullified :finalized)
       (let* ((c (second e))
              (round (certificate-round c))
              (proposal (etypecase c
                          (notarization (notarization-proposal c))
                          (finalization (finalization-proposal c))
                          (nullification nil))))
         (push (list from (first e) (round-view round)
                     (and proposal (proposal-parent proposal))
                     (and proposal (proposal-payload proposal)))
               (net-reports net))
         (when (eq (first e) :finalized)
           (let ((seen (gethash (round-view round) (net-finalized net))))
             (if (and seen (not (octets= seen (proposal-payload proposal))))
                 (incf (net-conflicts net))
                 (setf (gethash (round-view round) (net-finalized net))
                       (proposal-payload proposal)))))))
      (:request
       ;; the resolver: any reachable peer answers with the certificate it serves
       (push (cons from (second e)) (net-requests net))
       (loop for peer across (net-voters net)
             for i from 0
             for c = (and (/= i from) (not (offline-p net i)) (voter-certificate peer (second e)))
             when c
               do (setf (net-queue net) (nconc (net-queue net) (list (list from i c))))
                  (return))))))

(defun run-network (net until &key (max-ms 120000))
  "Deliver messages and fire timeouts until (funcall UNTIL) or MAX-MS of simulated time."
  (let* ((clock (net-clock net))
         (handle (net-clock-handle net))
         (start (funcall clock)))
    (let ((clempo.io:*clock* clock))
      (loop
        (when (funcall until) (return t))
        (let ((m (pop (net-queue net))))
          (if m
              (destructuring-bind (to from message) m
                (cond ((offline-p net to))
                      ((eq from :answer) (handle-effects net to (funcall message)))
                      ((not (offline-p net from))
                       (handle-effects net to (voter-receive (net-voter net to) from message)))))
              (let ((deadline (loop for v across (net-voters net)
                                    for d = (voter-next-deadline v)
                                    when d minimize d)))
                (when (or (null deadline) (> deadline (+ start max-ms))) (return nil))
                (when (> deadline (funcall clock))
                  (clempo.io:manual-clock-advance handle (- deadline (funcall clock))))
                (loop for v across (net-voters net)
                      for i from 0
                      do (handle-effects net i (voter-tick v))))))))))

(defun start-network (net)
  (let ((clempo.io:*clock* (net-clock net)))
    (loop for v across (net-voters net)
          for i from 0
          do (handle-effects net i (voter-start v)))))

(defun reports-of (net voter kind)
  (reverse (loop for (from k view parent payload) in (net-reports net)
                 when (and (= from voter) (eq k kind)) collect (list view parent payload))))

(defun all-finalized-p (net voters view)
  (lambda () (every (lambda (i) (>= (voter-last-finalized (net-voter net i)) view)) voters)))

(defmacro with-test-network ((net &rest options) &body body)
  `(let ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 7))
         (,net (make-network ,@options)))
     ,@body
     (is= 0 (net-conflicts ,net) "no two voters finalize different payloads in one view")))

(deftest voters-finalize-consecutive-views ()
  ;; Without leader skipping: at start-up a leader whose first vote is still in flight looks
  ;; inactive to peers that already heard from a quorum, and commonware skips its view.
  ;; Voter 4 is an observer (no share) that follows from votes and certificates.
  (with-test-network (net :timeouts (list* :skip nil *tempo-timeouts*) :observer t)
    (start-network net)
    (is (run-network net (all-finalized-p net '(0 1 2 3 4) 3)))
    (dotimes (i 5)
      (let ((finalized (reports-of net i :finalized)))
        (is= '(1 2 3) (subseq (mapcar #'first finalized) 0 3) "voter ~D" i)
        ;; each view builds on the previous one
        (is= '(0 1 2) (subseq (mapcar #'second finalized) 0 3) "voter ~D parents" i)
        (is (octets= (third (second finalized)) (chain-payload (third (first finalized)) 2)))))))

(deftest voters-answer-the-application-later ()
  ;; Verification and certification answered asynchronously (:pending, then
  ;; voter-verified / voter-certified).
  (with-test-network (net :timeouts (list* :skip nil *tempo-timeouts*) :app-class 'deferred-app)
    (start-network net)
    (is (run-network net (all-finalized-p net '(0 1 2 3) 2)))
    (dotimes (i 4)
      (is= '(1 2) (subseq (mapcar #'first (reports-of net i :finalized)) 0 2) "voter ~D" i))))

(deftest voters-finalize-with-a-silent-participant ()
  (with-test-network (net)
    (push 3 (net-offline net))
    (start-network net)
    (is (run-network net (all-finalized-p net '(0 1 2) 3)))
    (is= 0 (voter-last-finalized (net-voter net 3)))))

(deftest silent-leader-view-is-nullified ()
  ;; Round robin in epoch 0 makes participant 1 the leader of view 1.
  (with-test-network (net :elector :round-robin)
    (push 1 (net-offline net))
    (start-network net)
    (is= 1 (voter-leader-of (net-voter net 0) 1))
    (is (run-network net (all-finalized-p net '(0 2 3) 3)))
    (dolist (i '(0 2 3))
      (is (member 1 (mapcar #'first (reports-of net i :nullified))) "voter ~D nullifies view 1" i)
      (let ((first-final (first (reports-of net i :finalized))))
        ;; view 2's leader built on genesis across the nullified view
        (is= 2 (first first-final))
        (is= 0 (second first-final))))))

(deftest returning-voter-catches-up ()
  ;; Participant 3 misses several views and every message of them, then rejoins an idle
  ;; network: it learns the current view from the next certificates and votes, fetches what
  ;; it lacks below them, and finalizes with the others.
  (with-test-network (net)
    (push 3 (net-offline net))
    (start-network net)
    (is (run-network net (all-finalized-p net '(0 1 2) 3)))
    (setf (net-queue net) '())
    (let ((target (1+ (loop for i below 3 maximize (voter-last-finalized (net-voter net i))))))
      (is= 1 (voter-view (net-voter net 3)))
      (setf (net-offline net) '())
      (is (run-network net (all-finalized-p net '(0 1 2 3) target)))
      (is (>= (voter-view (net-voter net 3)) target))
      (is (find 3 (net-requests net) :key #'car) "the returning voter requested certificates"))))
