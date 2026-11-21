(in-package #:clempo.validator)

;;; Only the event-loop owner touches execution, Simplex or DKG state. Network
;;; workers hand over authenticated immutable frames via bounded mailboxes.

(defconstant +candidate-limit+ 512)
(defconstant +candidate-bytes-limit+ (* 64 1024 1024))
(defconstant +fetch-limit+ 64)

(defstruct (fetch (:constructor make-fetch (channel key kind deadline)))
  channel key kind deadline)

(defstruct (validator-app (:constructor %make-app))
  network store db chain seed key epoch-length
  validator-config-root journal
  voter scheme outcome dkg
  (finalized-headers (make-hash-table :test #'eql))
  (logged-dealers nil)
  head head-digest                       ; last executed finalized header and digest
  (finalizations nil)
  (candidate-blocks (make-hash-table :test #'equalp))
  (candidate-bytes 0)
  (recent-finalizations (make-hash-table :test #'eql))
  (certificates (make-hash-table :test #'eql)) ; current epoch notarizations by view
  pending-proposal pending-verifications pending-certifications
  (fetch-id 0) (requests (make-hash-table :test #'eql))
  (last-fetch 0) (prune-below 0)
  (report (constantly nil)))

(defun report (app kind &rest kvs)
  (funcall (validator-app-report app) (apply #'json:jobject "event" kind kvs)))

(defun epoch (app) (clempo.dkg:outcome-epoch (validator-app-outcome app)))
(defun header-of (block) (clempo.blocks:block-header block))
(defun number-of (block) (clempo.headers:header-number (header-of block)))
(defun parent-of (block) (clempo.headers:header-parent-hash (header-of block)))
(defun digest-of (block) (clempo.blocks:block-hash block))
(defun head-number (app) (clempo.headers:header-number (validator-app-head app)))
(defun head-hash (app)
  (or (validator-app-head-digest app)
      (setf (validator-app-head-digest app) (clempo.headers:header-hash (validator-app-head app)))))
(defun known-block (app digest) (gethash digest (validator-app-candidate-blocks app)))

(defun boundary-p (app number)
  (clempo.dkg:boundary-height-p number (validator-app-epoch-length app)))

(defun candidate-size (block)
  ;; Peer blocks retain their bytes. Locally built empty proposals have no raw
  ;; block allocation; header RLP plus the empty body bounds their size.
  (let* ((raw (clempo.blocks:block-raw block))
         (header (header-of block)))
    (if raw (length raw)
        (+ 8 (length (or (clempo.headers:header-raw header) (clempo.headers:encode-header header)))))))

(defun stash-block (app block)
  (let* ((hash (digest-of block)) (table (validator-app-candidate-blocks app))
         (number (number-of block)))
    (when (and (< (head-number app) number (+ (head-number app) +candidate-limit+))
               (< (hash-table-count table) +candidate-limit+)
               (not (gethash hash table))
               (not (clempo.blocks:block-body-mismatches block)))
      (let ((size (candidate-size block)))
        (when (<= (+ (validator-app-candidate-bytes app) size) +candidate-bytes-limit+)
          (incf (validator-app-candidate-bytes app) size)
          (setf (gethash hash table) block))))
    (when (gethash hash table) hash)))

(defun branch (app digest)
  "Return ordered unfinalized ancestors ending at DIGEST, success, or missing hash."
  (let ((final (head-hash app)) (blocks nil) (child-height nil))
    (loop repeat +candidate-limit+ do
      (when (octets= digest final)
        (return-from branch (values blocks (or (null child-height) (= child-height (1+ (head-number app)))) nil)))
      (let ((block (known-block app digest)))
        (unless block (return-from branch (values nil nil digest)))
        (when (or (<= (number-of block) (head-number app))
                  (and child-height (/= (1- child-height) (number-of block))))
          (return-from branch (values nil nil nil)))
        (setf digest (parent-of block) child-height (number-of block))
        (push block blocks)))
    (values nil nil nil)))

(defun get-parent (app ancestors)
  (if ancestors (header-of (car (last ancestors))) (validator-app-head app)))

(defun execute-checked (app block parent)
  ;; Never vote for a distinct chain with an execution-compatible state root.
  (let* ((db (validator-app-db app)) (header (header-of block))
         (inner (clempo.headers:header-inner header)))
    (unless (and (= (number-of block) (1+ (clempo.store:db-number db)))
                 (octets= (parent-of block) (clempo.store:db-hash db))
                 (< (clempo.headers:header-timestamp-millis parent) (clempo.headers:header-timestamp-millis header))
                 (< (clempo.headers:header-timestamp-millis-part header) 1000)
                 (zerop (clempo.headers:ethereum-header-difficulty inner))
                 (not (clempo.blocks:block-ommers block))
                 (not (clempo.blocks:block-body-mismatches block)))
      (error "invalid proposal header or body at height ~D" (number-of block)))
    (let* ((result (clempo.executor:execute-block db block :chain (validator-app-chain app)))
           (mismatches (clempo.executor:block-mismatches result header)))
      (when mismatches
        (error "proposal execution differs at height ~D (~{~A~^, ~})"
               (number-of block) (mapcar #'first mismatches)))
      (let ((origin (validator-app-validator-config-root app)))
        (when origin
          (let ((config (clempo.store:db-account db clempo.precompiles:+validator-config-v2-address+)))
            (unless (and config (octets= (clempo.store:account-storage-root config) origin))
              (error "proposal changes ValidatorConfigV2 on a fixed-membership devnet")))))
      (clempo.store:db-commit-block db (number-of block) (digest-of block)))))

(defun execute-ancestors (app ancestors)
  (loop with previous = (validator-app-head app)
        for block in ancestors
        do (execute-checked app block previous)
           (setf previous (header-of block))))

(defun dkg-block-from-header (header digest)
  (let ((context (clempo.headers:header-consensus-context header)))
    (clempo.dkg:make-dkg-block
     :height (clempo.headers:header-number header) :digest digest
     :parent (clempo.headers:header-parent-hash header) :timestamp (clempo.headers:header-timestamp header)
     :extra-data (clempo.headers:header-extra-data header)
     :parent-view (when context (clempo.headers:consensus-context-parent-view context)))))

(defun boundary-extra-data (app ancestors)
  (let ((dkg (validator-app-dkg app))
        (head (gethash (head-number app) (validator-app-finalized-headers app))))
    ;; The ceremony's branch walk includes its finalized floor.
    (clempo.dkg:dkg-notarized-block dkg head)
    (dolist (block ancestors)
      (clempo.dkg:dkg-notarized-block dkg (dkg-block-from-header (header-of block) (digest-of block))))
    (multiple-value-bind (extra missing)
        (clempo.dkg:dkg-boundary-extra-data
         dkg (if ancestors
                 (let ((block (car (last ancestors))))
                   (dkg-block-from-header (header-of block) (digest-of block)))
                 head)
         (clempo.dkg:outcome-next-players (validator-app-outcome app)) 0)
      (invariant (not missing) "complete proposal branch lacks a DKG ancestor")
      extra)))

(defun check-dkg-header (app block ancestors)
  (let* ((header (header-of block)) (extra (clempo.headers:header-extra-data header)))
    (cond ((boundary-p app (number-of block))
           (unless (octets= (boundary-extra-data app ancestors) extra)
             (error "boundary block carries a different DKG outcome at ~D" (number-of block)))
           nil)
          ((plusp (length extra))
           (let ((dealer (clempo.dkg:dkg-verify-dealer-log (validator-app-dkg app) (epoch app)
                                                          (clempo.headers:header-timestamp header) extra))
                 (claim (clempo.headers:header-consensus-context header)))
             (unless (and dealer claim (octets= dealer (clempo.headers:consensus-context-proposer claim)))
               (error "invalid DKG dealer log from proposal at height ~D" (number-of block)))
             dealer)))))

(defun app-send (app channel bytes &optional recipients)
  (network-send (validator-app-network app) channel bytes :recipients recipients))

(defun apply-dkg-actions (app actions &optional outcome)
  (dolist (action actions)
    (destructuring-bind (kind &rest data) action
      (ecase kind
        (:send
         (destructuring-bind (peer epoch payload) data
           (let ((writer (clempo.cwcodec:make-cw-writer)))
             (clempo.cwcodec:write-varint writer epoch)
             (clempo.cwcodec:write-raw writer payload)
             (app-send app 5 (clempo.cwcodec:cw-writer-octets writer) (list peer)))))
        (:exit
         (let ((finished (first data)))
           (when (plusp finished)
             (clempo.simplex:journal-prune (validator-app-journal app) (1- finished) (ash 1 64)))))
        (:enter
         (destructuring-bind (epoch output share) data
           (invariant (and outcome (= epoch (clempo.dkg:outcome-epoch outcome))
                           (equalp output (clempo.dkg:outcome-output outcome)))
                      "ceremony state differs from the finalized outcome")
           (unless (zerop epoch)
             (report app "dkg" "epoch" epoch
                     "reshared" (json:jbool (not (equalp output (clempo.dkg:outcome-output (validator-app-outcome app)))))
                     "share" (json:jbool share)
                     "dealers" (coerce (mapcar #'octets->hex (clempo.dkg:outcome-dealers outcome)) 'vector)))
           (enter-epoch app outcome share)))))))

(defun handle-dkg (app peer octets)
  (let* ((reader (clempo.cwcodec:make-cw-reader octets))
         (epoch (clempo.cwcodec:read-varint reader)))
    (when (= (epoch app) epoch)
      (apply-dkg-actions app (clempo.dkg:dkg-receive (validator-app-dkg app) peer epoch
                                                     (subseq octets (clempo.cwcodec:cw-reader-position reader)))))))

(defun proposal-extra-data (app ancestors height)
  (if (boundary-p app height)
      (boundary-extra-data app ancestors)
      (let ((key (validator-app-key app)))
        (or (and (not (member key (validator-app-logged-dealers app) :test #'octets=))
                 (not (loop for block in ancestors
                            for header = (header-of block)
                            for claim = (clempo.headers:header-consensus-context header)
                            thereis (and claim (plusp (length (clempo.headers:header-extra-data header)))
                                         (octets= key (clempo.headers:consensus-context-proposer claim)))))
                 (clempo.dkg:dkg-dealer-log (validator-app-dkg app) (epoch app)))
            (make-octets 0)))))

(defun propose (app context)
  (let ((parent (clempo.simplex:context-parent context)))
    (multiple-value-bind (ancestors available missing) (branch app (cdr parent))
      (when missing (return-from propose :pending))
      (unless available (return-from propose nil))
      (let* ((header (get-parent app ancestors)) (height (1+ (clempo.headers:header-number header)))
             (db (validator-app-db app))
             (stamp (max (1+ (clempo.headers:header-timestamp-millis header)) (io:now-ms))))
        ;; Start of the next epoch resets the voter; do not extend its boundary
        ;; using the current membership or the current epoch's share.
        (unless (= (floor height (validator-app-epoch-length app)) (epoch app))
          (return-from propose nil))
        (multiple-value-bind (seconds millis) (floor stamp 1000)
          (let* ((attrs (clempo.builder:make-payload-attributes
                         :timestamp seconds :timestamp-millis-part millis
                         :proposer-public-key (validator-app-key app)
                         :consensus-context (clempo.headers:make-consensus-context
                                             :epoch (epoch app) :view (clempo.simplex:context-view context)
                                             :parent-view (car parent) :proposer (validator-app-key app))
                         :extra-data (proposal-extra-data app ancestors height)))
                 (payload (clempo.store:with-speculative-blocks (db)
                            (execute-ancestors app ancestors)
                            (clempo.builder:build-payload db header attrs :chain (validator-app-chain app))))
                 (block (clempo.builder:built-payload-block payload)))
            (let ((digest (stash-block app block)))
              (when digest
                (app-send app 3 (clempo.validator-wire:encode-broadcast-block block))
                (report app "proposed" "height" height "hash" (octets->hex digest)))
              digest)))))))

(defmethod clempo.simplex:app-propose ((app validator-app) context)
  (let ((answer (propose app context)))
    (when (eq answer :pending) (setf (validator-app-pending-proposal app) context))
    answer))

(defun verify-proposal (app context digest)
  (let ((block (known-block app digest)))
    (unless block (return-from verify-proposal :pending))
    (let* ((parent (clempo.simplex:context-parent context))
           (claim (clempo.headers:header-consensus-context (header-of block))))
      (unless (and claim (= (clempo.headers:consensus-context-epoch claim) (epoch app))
                   (= (clempo.headers:consensus-context-view claim) (clempo.simplex:context-view context))
                   (= (clempo.headers:consensus-context-parent-view claim) (car parent))
                   (octets= (clempo.headers:consensus-context-proposer claim) (clempo.simplex:context-leader-key context))
                   (octets= (parent-of block) (cdr parent))
                   (= (floor (number-of block) (validator-app-epoch-length app)) (epoch app)))
        (return-from verify-proposal nil))
      (multiple-value-bind (ancestors available missing) (branch app (cdr parent))
        (when missing (return-from verify-proposal :pending))
        (unless available (return-from verify-proposal nil))
        (handler-case
            (let ((db (validator-app-db app)))
              (check-dkg-header app block ancestors)
              (clempo.store:with-speculative-blocks (db)
                (execute-ancestors app ancestors)
                (execute-checked app block (get-parent app ancestors)))
              t)
          (error (condition)
            (report app "proposal-rejected" "height" (number-of block) "reason" (princ-to-string condition))
            nil))))))

(defmethod clempo.simplex:app-verify ((app validator-app) context digest)
  (let ((answer (verify-proposal app context digest)))
    (when (eq answer :pending) (push (cons context digest) (validator-app-pending-verifications app)))
    answer))

(defun certify (app round digest)
  ;; A notarization authenticates the proposal but doesn't supply its block;
  ;; execute before certifying just as when verifying a leader's proposal.
  (multiple-value-bind (ancestors available missing) (branch app digest)
    (when missing (return-from certify :pending))
    (unless available (return-from certify nil))
    (if (null ancestors) t
        (handler-case
            (let* ((block (car (last ancestors))) (context (clempo.headers:header-consensus-context (header-of block)))
                   (db (validator-app-db app)))
              (unless (and context (= (clempo.headers:consensus-context-epoch context) (clempo.consensus:round-epoch round))
                           (= (clempo.headers:consensus-context-view context) (clempo.consensus:round-view round)))
                (return-from certify nil))
              (check-dkg-header app block (butlast ancestors))
              (clempo.store:with-speculative-blocks (db) (execute-ancestors app ancestors))
              t)
          (error (condition)
            (report app "certificate-rejected" "reason" (princ-to-string condition))
            nil)))))

(defmethod clempo.simplex:app-certify ((app validator-app) round digest)
  (let ((answer (certify app round digest)))
    (when (eq answer :pending) (push (cons round digest) (validator-app-pending-certifications app)))
    answer))

(defun broadcast-artifact (app message &optional recipients)
  (let* ((certificate-p (typep message '(or clempo.consensus:notarization clempo.consensus:nullification
                                            clempo.consensus:finalization)))
         (channel (if certificate-p 1 0))
         (type (if certificate-p 'clempo.consensus:certificate 'clempo.consensus:vote))
         (round (if certificate-p (clempo.consensus:certificate-round message) (clempo.consensus:vote-round message))))
    (app-send app channel (clempo.consensus:encode-epoch-mux (clempo.consensus:round-epoch round) type message)
              recipients)))

(defun resolve-request (app channel key kind &key peer)
  (let ((now (io:now-ms)) (requests (validator-app-requests app)))
    (maphash (lambda (id request)
               (when (<= (fetch-deadline request) now) (remhash id requests))) requests)
    (unless (or (> (validator-app-last-fetch app) (- now 140))
                (>= (hash-table-count requests) +fetch-limit+)
                (loop for request being the hash-values of requests
                      thereis (and (= (fetch-channel request) channel) (equalp (fetch-key request) key))))
      (let* ((id (incf (validator-app-fetch-id app)))
             (bytes (clempo.validator-wire:encode-resolver-message
                     channel (clempo.validator-wire:make-resolver-message
                              :kind :request :key key :id id :epoch (epoch app)))))
        (when (plusp (app-send app channel bytes (when peer (list peer))))
          (setf (validator-app-last-fetch app) now
                (gethash id requests) (make-fetch channel key kind (+ now 1500))))))))

(defun ask-block (app digest &optional peer)
  (resolve-request app 4 (clempo.validator-wire:make-marshal-key :kind :block :digest digest)
                   :block :peer peer))

(defun apply-effects (app effects)
  (dolist (effect effects)
    (destructuring-bind (kind &rest args) effect
      (ecase kind
        (:broadcast (broadcast-artifact app (first args)))
        (:send (broadcast-artifact app (second args) (first args)))
        (:notarized
         (let* ((notarization (first args)) (proposal (clempo.consensus:notarization-proposal notarization)))
           (setf (gethash (clempo.consensus:proposal-view proposal) (validator-app-certificates app)) notarization)
           (let ((digest (clempo.consensus:proposal-payload proposal)))
             (unless (known-block app digest) (ask-block app digest)))))
        (:nullified nil)
        (:finalized
         (when (>= (length (validator-app-finalizations app)) 128)
           (error "too many certified blocks are awaiting their bodies"))
         (setf (validator-app-finalizations app) (nconc (validator-app-finalizations app) (list (first args)))))
        (:request (resolve-request app 2 (first args) :certificate))))))

(defun enter-epoch (app outcome share)
  (let* ((players (clempo.dkg:outcome-players outcome))
         (sharing (clempo.dkg:outcome-sharing outcome))
         (index (position (validator-app-key app) players :test #'octets=))
         (v1 (clempo.chainspec:fork-active-p :t12 (clempo.headers:header-timestamp (validator-app-head app))
                                            (validator-app-chain app)))
         (scheme (if (and index share)
                     (progn
                       (invariant (= index (clempo.dkg:share-index share)) "share index differs from key index")
                       (clempo.consensus:make-signer-scheme (clempo.dkg:sharing-coefficients sharing)
                                                           (clempo.dkg:sharing-total sharing)
                                                           (cons index (clempo.dkg:share-private share))))
                     (clempo.consensus:make-verifier-scheme (clempo.dkg:sharing-coefficients sharing)
                                                           (clempo.dkg:sharing-total sharing)))))
    (setf (validator-app-outcome app) outcome
          (validator-app-scheme app) scheme
          (validator-app-logged-dealers app) nil
          (validator-app-pending-proposal app) nil
          (validator-app-pending-verifications app) nil
          (validator-app-pending-certifications app) nil
          (validator-app-finalizations app) nil
          (validator-app-prune-below app) 0
          (validator-app-voter app)
          (clempo.simplex:make-voter :scheme scheme :epoch (epoch app) :participants players
                                     :me (and share index) :application app
                                     :elector (if v1 :random-v1 :random-v0) :genesis (head-hash app)
                                     :journal (validator-app-journal app)))
    (clrhash (validator-app-certificates app))
    (clrhash (validator-app-requests app))
    (report app "epoch" "epoch" (epoch app) "signing" (json:jbool (and index share)))
    (apply-effects app (clempo.simplex:voter-start (validator-app-voter app)))))

(defun commit-finalization (app finalization branch)
  (let ((last (car (last branch))) (dealers nil))
    ;; Validate the whole branch before making any part of it durable.
    (let ((db (validator-app-db app)))
      (clempo.store:with-speculative-blocks (db) (execute-ancestors app branch)))
    (loop with ancestors = nil for block in branch
          do (push (check-dkg-header app block (when (boundary-p app (number-of block)) (reverse ancestors))) dealers)
             (push block ancestors))
    (loop for block in branch
          for dealer in (nreverse dealers)
          for height = (number-of block)
          for boundary = (boundary-p app height)
          for block-epoch = (epoch app)
          do (let* ((diff (execute-checked app block (validator-app-head app)))
                    (digest (clempo.store:block-diff-hash diff))
                    (header (header-of block))
                    (claim (clempo.headers:header-consensus-context header))
                    (dkg-block (dkg-block-from-header header digest))
                    (outcome (when boundary
                               (clempo.dkg:decode-onchain-dkg-outcome (clempo.headers:header-extra-data header)
                                                                     :strict t))))
               (clempo.store:store-commit-block (validator-app-store app) diff)
               (when boundary (clempo.store:store-sync (validator-app-store app)))
               (setf (validator-app-head app) header (validator-app-head-digest app) digest
                     (gethash height (validator-app-finalized-headers app)) dkg-block)
               (when dealer (pushnew dealer (validator-app-logged-dealers app) :test #'octets=))
               (report app "finalized" "height" height "hash" (octets->hex digest)
                       "epoch" block-epoch "view" (clempo.headers:consensus-context-view claim)
                       "proposer" (octets->hex (clempo.headers:consensus-context-proposer claim))
                       "boundary" (json:jbool boundary))
               (apply-dkg-actions app (clempo.dkg:dkg-finalized-block (validator-app-dkg app) dkg-block) outcome)
               (when boundary
                 (let ((keep (- height (validator-app-epoch-length app))))
                   (maphash (lambda (n block) (declare (ignore block))
                              (when (< n keep) (remhash n (validator-app-finalized-headers app))))
                            (validator-app-finalized-headers app))))))
    (setf (gethash (number-of last) (validator-app-recent-finalizations app)) finalization)
    ;; Retain a 64-block window for marshal replies; discard older side branches.
    (let ((below (- (head-number app) 64)))
      (maphash (lambda (hash block)
                 (when (< (number-of block) below)
                   (decf (validator-app-candidate-bytes app) (candidate-size block))
                   (remhash hash (validator-app-candidate-blocks app))))
               (validator-app-candidate-blocks app))
      (maphash (lambda (height artifact)
                 (declare (ignore artifact))
                 (when (< height below) (remhash height (validator-app-recent-finalizations app))))
               (validator-app-recent-finalizations app)))
    finalization))

(defun drive-finality (app)
  (loop while (validator-app-finalizations app)
        for artifact = (first (validator-app-finalizations app))
        for proposal = (clempo.consensus:finalization-proposal artifact)
        for digest = (clempo.consensus:proposal-payload proposal)
        do (multiple-value-bind (blocks valid missing) (branch app digest)
             (cond (missing (ask-block app missing) (return))
                   ((not valid) (error "verified finalization does not extend our finalized head"))
                   (t
                    (pop (validator-app-finalizations app))
                    (when blocks
                      (let ((context (clempo.headers:header-consensus-context (header-of (car (last blocks))))))
                        (invariant (and context
                                        (= (clempo.consensus:proposal-view proposal) (clempo.headers:consensus-context-view context))
                                        (= (clempo.consensus:proposal-epoch proposal) (clempo.headers:consensus-context-epoch context))
                                        (= (clempo.consensus:proposal-parent proposal) (clempo.headers:consensus-context-parent-view context)))
                                   "finalization proposal metadata mismatch"))
                      (commit-finalization app artifact blocks)))))))

(defun request-branch (app digest)
  (multiple-value-bind (blocks available missing) (branch app digest)
    (declare (ignore blocks available))
    (when missing (ask-block app missing))))

(defun prune-views (app)
  (let* ((view (clempo.simplex:voter-view (validator-app-voter app)))
         (keep (max 0 (- view 128))))
    (when (> keep (validator-app-prune-below app))
      (setf (validator-app-prune-below app) keep)
      (maphash (lambda (n certificate)
                 (declare (ignore certificate))
                 (when (< n keep) (remhash n (validator-app-certificates app))))
               (validator-app-certificates app))
      (setf (validator-app-pending-verifications app)
            (delete-if (lambda (item) (< (clempo.simplex:context-view (car item)) keep))
                       (validator-app-pending-verifications app))
            (validator-app-pending-certifications app)
            (delete-if (lambda (item) (< (clempo.consensus:round-view (car item)) keep))
                       (validator-app-pending-certifications app))))))

(defun drive-pending (app)
  (let ((voter (validator-app-voter app)) (effects nil))
    (when (validator-app-pending-proposal app)
      (let* ((context (validator-app-pending-proposal app))
             (round (clempo.simplex:context-round context)))
        (cond ((/= (clempo.simplex:voter-view voter) (clempo.consensus:round-view round))
               (setf (validator-app-pending-proposal app) nil))
              (t
               (let ((answer (propose app context)))
                 (if (eq answer :pending) (request-branch app (cdr (clempo.simplex:context-parent context)))
                     (progn
                       (setf (validator-app-pending-proposal app) nil)
                       (setf effects (nconc effects (clempo.simplex:voter-proposed voter round answer))))))))))
    (dolist (item (shiftf (validator-app-pending-verifications app) nil))
      (let* ((context (car item)) (digest (cdr item)) (answer (verify-proposal app context digest)))
        (if (eq answer :pending)
            (progn
              (request-branch app (if (known-block app digest) (cdr (clempo.simplex:context-parent context)) digest))
              (push item (validator-app-pending-verifications app)))
            (setf effects (nconc effects (clempo.simplex:voter-verified voter (clempo.simplex:context-round context)
                                                                        digest answer))))))
    (dolist (item (shiftf (validator-app-pending-certifications app) nil))
      (let ((answer (certify app (car item) (cdr item))))
        (if (eq answer :pending)
            (progn (request-branch app (cdr item)) (push item (validator-app-pending-certifications app)))
            (setf effects (nconc effects (clempo.simplex:voter-certified voter (car item) (cdr item) answer))))))
    (apply-effects app effects)))

(defun answer-request (app channel peer message)
  (let* ((key (clempo.validator-wire:resolver-message-key message))
         (value (if (= channel 2)
                    (let ((artifact (clempo.simplex:voter-certificate (validator-app-voter app) key)))
                      (when artifact (clempo.cwcodec:cw-encode 'clempo.consensus:certificate artifact)))
                    (case (clempo.validator-wire:marshal-key-kind key)
                      (:block
                       (let ((block (known-block app (clempo.validator-wire:marshal-key-digest key))))
                         (when block (clempo.validator-wire:encode-marshal-value key block nil))))
                      (:finalized
                       (let* ((height (clempo.validator-wire:marshal-key-height key))
                              (artifact (gethash height (validator-app-recent-finalizations app)))
                              (block (when artifact (known-block app (clempo.consensus:proposal-payload
                                                                    (clempo.consensus:finalization-proposal artifact))))))
                         (when block (clempo.validator-wire:encode-marshal-value key block artifact))))
                      (:notarized
                       (let* ((round (clempo.validator-wire:marshal-key-round key))
                              (artifact (when (= (clempo.consensus:round-epoch round) (epoch app))
                                          (gethash (clempo.consensus:round-view round) (validator-app-certificates app))))
                              (block (when artifact (known-block app (clempo.consensus:proposal-payload
                                                                    (clempo.consensus:notarization-proposal artifact))))))
                         (when block (clempo.validator-wire:encode-marshal-value key block artifact))))))))
    (app-send app channel
              (clempo.validator-wire:encode-resolver-message
               channel (clempo.validator-wire:make-resolver-message
                        :epoch (epoch app) :id (clempo.validator-wire:resolver-message-id message)
                        :kind (if value :response :error) :value value))
              (list peer))))

(defun handle-resolver (app channel peer octets)
  (let* ((message (clempo.validator-wire:decode-resolver-message channel octets))
         (kind (clempo.validator-wire:resolver-message-kind message))
         (id (clempo.validator-wire:resolver-message-id message))
         (requests (validator-app-requests app)))
    (when (or (= channel 4) (= (clempo.validator-wire:resolver-message-epoch message) (epoch app)))
      (case kind
        (:request (answer-request app channel peer message))
        (:response
         (let ((request (gethash id requests)))
           (when (and request (= channel (fetch-channel request)))
             (let ((data (clempo.validator-wire:resolver-message-value message)))
               (ecase (fetch-kind request)
                 (:block (stash-block app (clempo.validator-wire:decode-marshal-value (fetch-key request) data)))
                 (:certificate
                  (let ((artifact (clempo.cwcodec:cw-decode 'clempo.consensus:certificate data)))
                    (multiple-value-bind (effects disposition)
                        (clempo.simplex:voter-deliver (validator-app-voter app) (fetch-key request) artifact)
                      (when (eq :invalid disposition)
                        (clempo.cwcodec:cw-fail "resolver certificate does not satisfy its request"))
                      (apply-effects app effects)))))
               (remhash id requests)))))))))

(defun handle-network-event (app event)
  (let ((peer (network-event-peer event)) (kind (network-event-kind event)))
    (case kind
      (:connected (report app "connected" "peer" (octets->hex peer)))
      (:disconnected (report app "disconnected" "peer" (octets->hex peer)))
      (:data
       (let ((octets (subseq (network-event-frame event) (network-event-start event) (network-event-end event)))
             (channel (network-event-channel event)))
         (handler-case
             (case channel
               ((0 1)
                (multiple-value-bind (message e)
                    (clempo.consensus:decode-epoch-mux
                     (if (zerop channel) 'clempo.consensus:vote 'clempo.consensus:certificate) octets)
                  (when (= e (epoch app))
                    (apply-effects app (clempo.simplex:voter-receive (validator-app-voter app) peer message)))))
               ((2 4) (handle-resolver app channel peer octets))
               (3 (stash-block app (clempo.validator-wire:decode-broadcast-block octets)))
               (5 (handle-dkg app peer octets)))
           (clempo.dkg:dkg-error (condition)
             (report app "message-rejected" "channel" channel "error" (princ-to-string condition)))
           (clempo.cwcodec:cw-error (condition)
             (report app "message-rejected" "channel" channel "error" (princ-to-string condition)))
           (clempo.rlp:rlp-error (condition)
             (report app "message-rejected" "channel" channel "error" (princ-to-string condition)))))))))

(defun read-share (slot)
  (clempo.cwcodec:cw-decode 'clempo.dkg:share
                           (hex->octets (string-trim '(#\Space #\Newline #\Return #\Tab)
                                                     (io:read-file-string (slot-share-path slot))))))

(defun claim-signing-slot (slot)
  ;; An execution checkpoint does not recover old anti-equivocation state.
  ;; These short-lived devnets use a fresh identity/manifest for every run.
  (let ((marker (make-pathname :name "clempo-signing-active" :type nil :defaults (slot-seed-path slot))))
    (with-open-file (out marker :direction :output :if-exists :error :if-does-not-exist :create)
      (write-line "This devnet identity has already been activated. Provision a fresh network to sign again." out)
      (finish-output out)
      (sb-posix:fsync (sb-sys:fd-stream-fd out)))
    (fsync-validator-directory (make-pathname :name nil :type nil :defaults marker))))

(defun run-devnet-validator (manifest slot datadir reporter &key (seconds 0))
  "Run one local validator from genesis with the harness's registered identity.
The optional timeout is for controlled devnet runs. REPORTER consumes JSON events."
  (let* ((chain (clempo.chainspec:load-chainspec (manifest-genesis manifest)))
         (length (manifest-epoch-length manifest))
         (outcome (clempo.dkg:decode-onchain-dkg-outcome (clempo.chainspec:genesis-extra-data chain) :strict t))
         (share (read-share slot))
         (keys (map 'list #'slot-public-key (manifest-slots manifest))))
    (invariant (and (> length 1) (= length (clempo.chainspec:chain-epoch-length chain)))
               "manifest epoch length is not the genesis epoch length")
    (invariant (eq :t13 (clempo.chainspec:fork-at (io:now-seconds) chain))
               "devnet validator requires a T13 chainspec; later fork activation is unsupported")
    (invariant (and (= 0 (clempo.dkg:outcome-epoch outcome))
                    (null (set-exclusive-or keys (clempo.dkg:outcome-players outcome) :test #'octets=))
                    (null (set-exclusive-or keys (clempo.dkg:outcome-next-players outcome) :test #'octets=)))
               "manifest validators do not match the genesis DKG outcome")
    (call-with-validator-network
     manifest slot
     (lambda (network)
       (let* ((store (clempo.store:open-store (uiop:ensure-directory-pathname datadir)
                                             :genesis (lambda (db) (clempo.executor:load-genesis db :chain chain))))
              (db (clempo.store:store-db store))
              (head (clempo.headers:decode-header (clempo.chainspec:genesis-header-rlp chain :state-root (clempo.store:db-root-hash db))))
              (app (%make-app :network network :store store :db db :chain chain :seed (validator-network-seed network)
                              :key (slot-public-key slot) :epoch-length length :head head :report reporter))
              (deadline (when (plusp seconds) (+ (io:now-ms) (* 1000 seconds)))))
         (unwind-protect
              (progn
                (invariant (and (zerop (clempo.store:db-number db)) (octets= (head-hash app) (clempo.store:db-hash db)))
                           "the devnet validator requires fresh genesis state")
                (let ((config (clempo.store:db-account db clempo.precompiles:+validator-config-v2-address+)))
                  (invariant config "genesis lacks ValidatorConfigV2")
                  (setf (validator-app-validator-config-root app) (clempo.store:account-storage-root config)))
                (claim-signing-slot slot)
                (setf (validator-app-journal app)
                      (make-file-journal (merge-pathnames "simplex/" (clempo.store:store-dir store))))
                (setf (gethash 0 (validator-app-finalized-headers app))
                      (dkg-block-from-header head (head-hash app)))
                (multiple-value-bind (dkg actions)
                    (clempo.dkg:open-dkg-manager
                     :seed (validator-app-seed app)
                     :store (make-file-dkg-store (merge-pathnames "dkg/" (clempo.store:store-dir store)))
                     :epoch-length length
                     :t12-time (clempo.chainspec:fork-activation :t12 chain)
                     :t15-time (clempo.chainspec:fork-activation :t15 chain)
                     :header-at (lambda (height) (gethash height (validator-app-finalized-headers app)))
                     :last-finalized-height 0 :initial-share share)
                  (setf (validator-app-dkg app) dkg)
                  (apply-dkg-actions app actions outcome))
                (loop while (or (null deadline) (< (io:now-ms) deadline)) do
                  (let ((event (network-receive network :timeout 0.012)))
                    (when event (handle-network-event app event)))
                  (drive-finality app)
                  (drive-pending app)
                  (prune-views app)
                  (apply-effects app (clempo.simplex:voter-tick (validator-app-voter app))))
                (head-number app))
           (clempo.store:close-store store)))))))