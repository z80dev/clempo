(in-package #:clempo.validator)

;;; The validator's consensus application (tempo consensus/src/consensus/application, the
;;; epoch manager, marshal and DKG actor as the event loop drives them). Only the event-loop
;;; owner touches execution, Simplex or DKG state; network workers hand over authenticated
;;; immutable frames, and JSON-RPC readers are held off by the backend's write lock while the
;;; state DB runs a block (speculatively or for good).

(defconstant +candidate-limit+ 512)
(defconstant +candidate-bytes-limit+ (* 64 1024 1024))
(defconstant +fetch-limit+ 64)
(defconstant +finalization-queue-limit+ 128)
(defconstant +backfill-window+ 32 "Finalized heights requested ahead of the head while catching up.")
(defconstant +backfill-timeout-ms+ 3000)

(defstruct (fetch (:constructor make-fetch (channel key kind deadline &optional peer)))
  channel key kind deadline peer)

(defstruct (validator-app (:constructor %make-app))
  network store db chain seed key epoch-length journal
  voter scheme outcome dkg
  log backend server                      ; block log, JSON-RPC backend (with the pool) and server
  (fee-recipient 0)                       ; beneficiary when ValidatorConfigV2 names none
  (timeouts nil)                          ; voter timeouts plist (NIL: Tempo's)
  (finalized-headers (make-hash-table :test #'eql)) ; height -> dkg-block, current and previous epoch
  (logged-dealers nil)
  head head-digest                        ; last executed finalized header and digest
  (finalizations nil)
  (candidate-blocks (make-hash-table :test #'equalp))
  (candidate-bytes 0)
  (certificates (make-hash-table :test #'eql)) ; current epoch notarizations by view
  pending-proposal pending-verifications pending-certifications
  (fetch-id 0) (requests (make-hash-table :test #'eql))
  (last-fetch 0) (prune-below 0)
  (peers nil)                             ; the last tracked peer set ((key host port) ...)
  (backfill-target nil)                   ; finalized height known to exist above the head
  (backfill-frequency 8)                  ; marshal requests per second per peer (Tempo's quota)
  (backfill-buffer (make-hash-table))     ; height -> (block finalization . peer)
  (backfill-missing (make-hash-table))    ; height -> peers that answered with an error
  (backfill-next-ms (make-hash-table :test #'equalp)) ; peer -> earliest next request time
  (backfill-queued 0)                     ; a finalization above a gap, queued for its ancestors
  (proposed 0) (committed 0) (dealings-sent 0) (acks-sent 0) (epochs 0)
  (signing nil)                           ; whether it holds a share in the current epoch
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

(defun epoch-last-height (app)
  (1- (* (1+ (epoch app)) (validator-app-epoch-length app))))

(defun digest-height (app digest)
  "The height of the block DIGEST names when it is a candidate or the finalized head, else NIL."
  (let ((block (known-block app digest)))
    (cond (block (number-of block))
          ((octets= digest (head-hash app)) (head-number app)))))

(defun reproposal-p (app digest)
  "commonware marshal (standard Inline/Deferred): once the current epoch's last block is a
proposal's parent, later views of the epoch re-propose that block as is rather than build
blocks the epoch transition would cut off."
  (eql (digest-height app digest) (epoch-last-height app)))

(defmacro with-proposal-pool ((txpool app) &body body)
  "Run BODY with TXPOOL bound to the transaction pool proposals draw from (or NIL), holding
the pool's lock so that RPC submissions cannot change it while a payload is built."
  (let ((pool (gensym "POOL")) (fn (gensym "BODY")))
    `(let ((,pool (backend-pool-service (validator-app-backend ,app))))
       (flet ((,fn (,txpool) ,@body))
         (if ,pool
             (with-validator-pool (,pool) (,fn (validator-pool-pool ,pool)))
             (,fn nil))))))

(defun t15-p (app timestamp)
  (clempo.chainspec:fork-active-p :t15 timestamp (validator-app-chain app)))

(defmacro with-chain-write ((app) &body body)
  "Run BODY, which runs blocks on the state DB, with JSON-RPC requests held off."
  (let ((backend (gensym "BACKEND")) (thunk (gensym "THUNK")))
    `(let ((,backend (validator-app-backend ,app)))
       (flet ((,thunk () ,@body))
         (if ,backend
             (clempo.rpc.chain:with-backend-write (,backend) (,thunk))
             (,thunk))))))

(defun candidate-size (block)
  ;; Peer blocks retain their bytes. Locally built proposals have no raw block allocation;
  ;; their encoding bounds their size.
  (let ((raw (clempo.blocks:block-raw block)))
    (if raw (length raw) (length (clempo.blocks:encode-block block)))))

(defun evict-highest-candidates (app above)
  "Drop every candidate at the highest height when that height exceeds ABOVE; true if any
was dropped. A validator far behind fills its candidates with blocks broadcast near the
tip; the ones next to its head are the ones it must keep room for."
  (let* ((table (validator-app-candidate-blocks app))
         (highest (loop for block being the hash-values of table maximize (number-of block))))
    (when (and highest (> highest above))
      (maphash (lambda (hash block)
                 (when (= (number-of block) highest)
                   (decf (validator-app-candidate-bytes app) (candidate-size block))
                   (remhash hash table)))
               table)
      t)))

(defun stash-block (app block)
  (let* ((hash (digest-of block)) (table (validator-app-candidate-blocks app))
         (number (number-of block)))
    (when (and (< (head-number app) number (+ (head-number app) +candidate-limit+))
               (not (gethash hash table))
               (not (clempo.blocks:block-body-mismatches block)))
      (let ((size (candidate-size block)))
        (flet ((room-p ()
                 (and (< (hash-table-count table) +candidate-limit+)
                      (<= (+ (validator-app-candidate-bytes app) size) +candidate-bytes-limit+))))
          (loop repeat +candidate-limit+
                until (room-p)
                while (evict-highest-candidates app number))
          (when (room-p)
            (incf (validator-app-candidate-bytes app) size)
            (setf (gethash hash table) block)))))
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

;;; Execution and the DKG data in extraData.

(defun execute-checked (app block parent)
  "Execute BLOCK on PARENT's open state and commit it in the DB; (values diff result).
Never vote for a distinct chain with an execution-compatible state root."
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
      (values (clempo.store:db-commit-block db (number-of block) (digest-of block)) result))))

(defun dkg-block-from-header (header digest)
  (let ((context (clempo.headers:header-consensus-context header)))
    (clempo.dkg:make-dkg-block
     :height (clempo.headers:header-number header) :digest digest
     :parent (clempo.headers:header-parent-hash header) :timestamp (clempo.headers:header-timestamp header)
     :extra-data (clempo.headers:header-extra-data header)
     :parent-view (when context (clempo.headers:consensus-context-parent-view context)))))

(defun next-players (app)
  "The players of the next ceremony: the active ValidatorConfigV2 validators at the open
state (parent_state.rs next_players), sorted."
  (sorted-keys (mapcar #'registered-public-key (active-validators (validator-app-db app)))))

(defun boundary-extra-data (app ancestors)
  "The DKG outcome a boundary block on top of ANCESTORS (the unfinalized branch from the
head) carries: the ceremony output over that branch, and from ValidatorConfigV2 at the
parent's state, which the caller has open on the DB, the next players and whether the next
epoch runs a full DKG (parent_state.rs boundary_outcome)."
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
         (next-players app) (next-full-dkg-epoch (validator-app-db app)))
      (invariant (not missing) "complete proposal branch lacks a DKG ancestor")
      extra)))

(defun check-dkg-header (app block ancestors)
  "verify_header and verify_boundary_outcome: a boundary block must carry the outcome this
node computes at its parent's state (open on the DB); another block's extraData is empty or
the signed dealer log of its proposer. Returns the dealer, or NIL."
  (let* ((header (header-of block)) (extra (clempo.headers:header-extra-data header)))
    (cond ((boundary-p app (number-of block))
           (when (t15-p app (clempo.headers:header-timestamp header))
             ;; From T15 the outcome must take up the whole extraData.
             (clempo.dkg:decode-onchain-dkg-outcome extra :strict t))
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

(defun execute-branch (app blocks &key check)
  "Execute BLOCKS, the unfinalized branch from the head in order, on the open speculative
state. With CHECK, first check each block's DKG extraData at its parent's state. Returns the
dealers whose logs the blocks carry (NIL entries for blocks without one)."
  (loop with parent = (validator-app-head app)
        for block in blocks
        for done from 0
        collect (when check (check-dkg-header app block (subseq blocks 0 done)))
        do (execute-checked app block parent)
           (setf parent (header-of block))))

(defun app-send (app channel bytes &optional recipients)
  (network-send (validator-app-network app) channel bytes :recipients recipients))

(defun block-peer (app peer reason)
  "Disconnect PEER and refuse it for the block period, as commonware's Blocker does."
  (report app "blocked" "peer" (octets->hex peer) "reason" reason)
  (network-block (validator-app-network app) peer))

;;; The DKG actor's actions.

(defun apply-dkg-actions (app actions &optional outcome)
  (dolist (action actions)
    (destructuring-bind (kind &rest data) action
      (ecase kind
        (:send
         (destructuring-bind (peer epoch payload) data
           (let ((writer (clempo.cwcodec:make-cw-writer)))
             (clempo.cwcodec:write-varint writer epoch)
             (clempo.cwcodec:write-raw writer payload)
             (if (eql 0 (aref payload 0))
                 (incf (validator-app-dealings-sent app))
                 (incf (validator-app-acks-sent app)))
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
           (unless (or (zerop epoch) (null (validator-app-outcome app)))
             (report app "dkg" "epoch" epoch
                     "reshared" (json:jbool (not (equalp output (clempo.dkg:outcome-output (validator-app-outcome app)))))
                     "full" (json:jbool (clempo.dkg:outcome-is-next-full-dkg (validator-app-outcome app)))
                     "share" (json:jbool share)
                     "players" (coerce (mapcar #'octets->hex (clempo.dkg:outcome-players outcome)) 'vector)
                     "dealers" (coerce (mapcar #'octets->hex (clempo.dkg:outcome-dealers outcome)) 'vector)))
           (enter-epoch app outcome share)))))))

(defun handle-dkg (app peer octets)
  (let* ((reader (clempo.cwcodec:make-cw-reader octets))
         (epoch (clempo.cwcodec:read-varint reader)))
    (if (= (epoch app) epoch)
        (apply-dkg-actions app (clempo.dkg:dkg-receive (validator-app-dkg app) peer epoch
                                                       (subseq octets (clempo.cwcodec:cw-reader-position reader))))
        (note-peer-epoch app peer epoch))))

;;; Proposing, verifying and certifying.

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
    (when (reproposal-p app (cdr parent))
      (report app "reproposed" "height" (epoch-last-height app) "hash" (octets->hex (cdr parent))
              "view" (clempo.simplex:context-view context))
      (return-from propose (cdr parent)))
    (multiple-value-bind (ancestors available missing) (branch app (cdr parent))
      (when missing (return-from propose :pending))
      (unless available (return-from propose nil))
      (let* ((header (get-parent app ancestors)) (height (1+ (clempo.headers:header-number header)))
             (db (validator-app-db app))
             (stamp (max (1+ (clempo.headers:header-timestamp-millis header)) (io:now-ms))))
        (unless (= (floor height (validator-app-epoch-length app)) (epoch app))
          (return-from propose nil))
        (multiple-value-bind (seconds millis) (floor stamp 1000)
          (let* ((payload
                   (with-chain-write (app)
                     (clempo.store:with-speculative-blocks (db)
                       (execute-branch app ancestors)
                       (with-proposal-pool (txpool app)
                         (clempo.builder:build-payload
                          db header
                          (clempo.builder:make-payload-attributes
                           :timestamp seconds :timestamp-millis-part millis
                           :suggested-fee-recipient (validator-app-fee-recipient app)
                           :proposer-public-key (validator-app-key app)
                           :consensus-context (clempo.headers:make-consensus-context
                                               :epoch (epoch app) :view (clempo.simplex:context-view context)
                                               :parent-view (car parent) :proposer (validator-app-key app))
                           :extra-data (proposal-extra-data app ancestors height))
                          :chain (validator-app-chain app)
                          :pool txpool)))))
                 (block (clempo.builder:built-payload-block payload))
                 (digest (stash-block app block)))
            (when digest
              (incf (validator-app-proposed app))
              (app-send app 3 (clempo.validator-wire:encode-broadcast-block block))
              (report app "proposed" "height" height "hash" (octets->hex digest)
                      "transactions" (length (clempo.blocks:block-transactions block))))
            digest))))))

(defmethod clempo.simplex:app-propose ((app validator-app) context)
  (let ((answer (propose app context)))
    (when (eq answer :pending) (setf (validator-app-pending-proposal app) context))
    answer))

(defun verify-proposal (app context digest)
  (when (octets= digest (cdr (clempo.simplex:context-parent context)))
    ;; A re-proposal skips the parent and height checks: consensus settled the block's
    ;; validity in the view that first carried it. It is valid only for the epoch's last block.
    (return-from verify-proposal
      (if (digest-height app digest) (reproposal-p app digest) :pending)))
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
              (with-chain-write (app)
                (clempo.store:with-speculative-blocks (db)
                  (execute-branch app ancestors)
                  (check-dkg-header app block ancestors)
                  (execute-checked app block (get-parent app ancestors))))
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
        (let* ((block (car (last ancestors)))
               (context (clempo.headers:header-consensus-context (header-of block))))
          (unless (and context (= (clempo.headers:consensus-context-epoch context) (clempo.consensus:round-epoch round)))
            (return-from certify nil))
          ;; is_inferred_reproposal_at_certify: the epoch's last block notarized in a view
          ;; after the one its header names was re-proposed; its own view settled its validity.
          (when (/= (clempo.headers:consensus-context-view context) (clempo.consensus:round-view round))
            (return-from certify
              (and (boundary-p app (number-of block))
                   (> (clempo.consensus:round-view round) (clempo.headers:consensus-context-view context)))))
          (handler-case
              (let ((db (validator-app-db app)))
                (with-chain-write (app)
                  (clempo.store:with-speculative-blocks (db)
                    (execute-branch app (butlast ancestors))
                    (check-dkg-header app block (butlast ancestors))
                    (execute-checked app block (get-parent app (butlast ancestors)))))
                t)
            (error (condition)
              (report app "certificate-rejected" "reason" (princ-to-string condition))
              nil))))))

(defmethod clempo.simplex:app-certify ((app validator-app) round digest)
  (let ((answer (certify app round digest)))
    (when (eq answer :pending) (push (cons round digest) (validator-app-pending-certifications app)))
    answer))

;;; Requests to peers (simplex resolver on channel 2, marshal on channel 4).

(defun broadcast-artifact (app message &optional recipients)
  (let* ((certificate-p (typep message '(or clempo.consensus:notarization clempo.consensus:nullification
                                            clempo.consensus:finalization)))
         (channel (if certificate-p 1 0))
         (type (if certificate-p 'clempo.consensus:certificate 'clempo.consensus:vote))
         (round (if certificate-p (clempo.consensus:certificate-round message) (clempo.consensus:vote-round message))))
    (app-send app channel (clempo.consensus:encode-epoch-mux (clempo.consensus:round-epoch round) type message)
              recipients)))

(defun expire-requests (app)
  (let ((now (io:now-ms)) (requests (validator-app-requests app)))
    (maphash (lambda (id request)
               (when (<= (fetch-deadline request) now) (remhash id requests)))
             requests)))

(defun send-request (app channel key kind peer timeout)
  "Send a resolver request for KEY to PEER (NIL: every peer) and track it; true if sent."
  (let* ((now (io:now-ms))
         (id (incf (validator-app-fetch-id app)))
         (bytes (clempo.validator-wire:encode-resolver-message
                 channel (clempo.validator-wire:make-resolver-message
                          :kind :request :key key :id id :epoch (epoch app)))))
    (when (plusp (app-send app channel bytes (when peer (list peer))))
      (setf (gethash id (validator-app-requests app)) (make-fetch channel key kind (+ now timeout) peer))
      t)))

(defun resolve-request (app channel key kind &key peer)
  (let ((now (io:now-ms)) (requests (validator-app-requests app)))
    (expire-requests app)
    (unless (or (> (validator-app-last-fetch app) (- now 140))
                (>= (hash-table-count requests) +fetch-limit+)
                (loop for request being the hash-values of requests
                      thereis (and (= (fetch-channel request) channel) (equalp (fetch-key request) key))))
      (when (send-request app channel key kind peer 1500)
        (setf (validator-app-last-fetch app) now)))))

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
         ;; A finalization implies its ancestors: when bodies are slow to arrive, the newest
         ;; queued ones suffice.
         (let ((queue (nconc (validator-app-finalizations app) (list (first args)))))
           (setf (validator-app-finalizations app)
                 (if (> (length queue) +finalization-queue-limit+)
                     (last queue (floor +finalization-queue-limit+ 2))
                     queue))))
        (:request (resolve-request app 2 (first args) :certificate))))))

;;; Epochs.

(defun boundary-dkg-block (app epoch)
  "The finalized block that installed EPOCH: genesis for epoch 0, else the previous epoch's
last block. It is the voter's genesis (Floor::Genesis) and its timestamp picks the elector."
  (gethash (if (zerop epoch) 0 (1- (* epoch (validator-app-epoch-length app))))
           (validator-app-finalized-headers app)))

(defun enter-epoch (app outcome share)
  (let* ((players (clempo.dkg:outcome-players outcome))
         (sharing (clempo.dkg:outcome-sharing outcome))
         (index (position (validator-app-key app) players :test #'octets=))
         (boundary (boundary-dkg-block app (clempo.dkg:outcome-epoch outcome)))
         (v1 (progn (invariant boundary "no finalized boundary block for epoch ~D" (clempo.dkg:outcome-epoch outcome))
                    (clempo.chainspec:fork-active-p :t12 (clempo.dkg:dkg-block-timestamp boundary)
                                                    (validator-app-chain app))))
         (scheme (if (and index share)
                     (progn
                       (invariant (= index (clempo.dkg:share-index share)) "share index differs from key index")
                       (clempo.consensus:make-signer-scheme (clempo.dkg:sharing-coefficients sharing)
                                                           (clempo.dkg:sharing-total sharing)
                                                           (cons index (clempo.dkg:share-private share))))
                     (clempo.consensus:make-verifier-scheme (clempo.dkg:sharing-coefficients sharing)
                                                           (clempo.dkg:sharing-total sharing)))))
    (incf (validator-app-epochs app))
    (setf (validator-app-outcome app) outcome
          (validator-app-scheme app) scheme
          (validator-app-logged-dealers app) nil
          (validator-app-pending-proposal app) nil
          (validator-app-pending-verifications app) nil
          (validator-app-pending-certifications app) nil
          (validator-app-finalizations app) nil
          (validator-app-prune-below app) 0
          (validator-app-voter app)
          (apply #'clempo.simplex:make-voter
                 :scheme scheme :epoch (epoch app) :participants players
                 :me (and share index) :application app
                 :elector (if v1 :random-v1 :random-v0)
                 :genesis (clempo.dkg:dkg-block-digest boundary)
                 :journal (validator-app-journal app)
                 (when (validator-app-timeouts app) (list :timeouts (validator-app-timeouts app)))))
    (clrhash (validator-app-certificates app))
    (clrhash (validator-app-requests app))
    (setf (validator-app-signing app) (and index share t))
    (report app "epoch" "epoch" (epoch app) "signing" (json:jbool (and index share))
            "player" (json:jbool index) "participants" (length players))
    (refresh-peers app)
    (apply-effects app (clempo.simplex:voter-start (validator-app-voter app)))))

;;; Peers: each tracked set is the union of the epoch's players, the next players and the
;;; active ValidatorConfigV2 validators (peer_manager/actor.rs, config.rs PEERSETS_TO_TRACK),
;;; refreshed as blocks finalize, with their registered ingress addresses.

(defun peer-set (app)
  (let* ((db (validator-app-db app))
         (outcome (validator-app-outcome app))
         (active (active-validators db))
         (me (validator-app-key app)))
    (loop for key in (sorted-keys (append (clempo.dkg:outcome-players outcome)
                                          (clempo.dkg:outcome-next-players outcome)
                                          (mapcar #'registered-public-key active)))
          for entry = (or (find key active :key #'registered-public-key :test #'octets=)
                          (registered-by-key db key))
          unless (octets= key me)
            collect (if entry
                        (list key (registered-host entry) (registered-port entry) (registered-egress entry))
                        (list key nil nil)))))

(defun refresh-peers (app)
  (let ((peers (peer-set app)))
    (unless (equalp peers (validator-app-peers app))
      (setf (validator-app-peers app) peers)
      (network-track (validator-app-network app) (head-number app)
                     (remove nil peers :key #'second))
      (report app "peers" "height" (head-number app) "count" (length peers)))))

;;; Committing finalized blocks.

(defun commit-block (app block dealer finalization)
  "Make BLOCK, the head's child, durable: the block log first (so a restart re-executes it
if the state store had not synced), then the state, then the DKG actor."
  (let* ((store (validator-app-store app))
         (height (number-of block))
         (boundary (boundary-p app height))
         (block-epoch (epoch app))
         (header (header-of block))
         (claim (clempo.headers:header-consensus-context header))
         (digest (digest-of block))
         (dkg-block (dkg-block-from-header header digest))
         (outcome (when boundary
                    (clempo.dkg:decode-onchain-dkg-outcome (clempo.headers:header-extra-data header) :strict t))))
    (with-chain-write (app)
      (multiple-value-bind (diff result) (execute-checked app block (validator-app-head app))
        (block-log-append (validator-app-log app) height
                          (or (clempo.blocks:block-raw block) (clempo.blocks:encode-block block))
                          (mapcar #'clempo.receipts:encode-receipt (clempo.executor:block-result-receipts result))
                          finalization)
        (clempo.store:store-commit-block store diff)
        (when boundary (clempo.store:store-sync store))
        (setf (validator-app-head app) header (validator-app-head-digest app) digest
              (gethash height (validator-app-finalized-headers app)) dkg-block)
        (revalidate-pool (validator-app-backend app))))
    (incf (validator-app-committed app))
    (when dealer (pushnew dealer (validator-app-logged-dealers app) :test #'octets=))
    (report app "finalized" "height" height "hash" (octets->hex digest)
            "epoch" block-epoch "view" (clempo.headers:consensus-context-view claim)
            "proposer" (octets->hex (clempo.headers:consensus-context-proposer claim))
            "boundary" (json:jbool boundary)
            "transactions" (length (clempo.blocks:block-transactions block)))
    (let ((server (validator-app-server app)))
      (when server (clempo.rpcserver:publish-head server height :certificate finalization)))
    (apply-dkg-actions app (clempo.dkg:dkg-finalized-block (validator-app-dkg app) dkg-block) outcome)
    (when boundary
      ;; Keep the previous epoch's blocks: a restarted DKG actor recovers revealed shares
      ;; from its dealer logs.
      (let ((keep (- height (* 2 (validator-app-epoch-length app)))))
        (maphash (lambda (n block) (declare (ignore block))
                   (when (< n keep) (remhash n (validator-app-finalized-headers app))))
                 (validator-app-finalized-headers app))))))

(defun commit-finalization (app finalization branch)
  (let* ((db (validator-app-db app))
         ;; Validate the whole branch before making any part of it durable.
         (dealers (with-chain-write (app)
                    (clempo.store:with-speculative-blocks (db) (execute-branch app branch :check t))))
         (last (car (last branch))))
    (loop for block in branch
          for dealer in dealers
          do (commit-block app block dealer (when (eq block last) finalization)))
    ;; Discard side branches that fell below the head.
    (let ((below (head-number app)))
      (maphash (lambda (hash block)
                 (when (<= (number-of block) below)
                   (decf (validator-app-candidate-bytes app) (candidate-size block))
                   (remhash hash (validator-app-candidate-blocks app))))
               (validator-app-candidate-blocks app)))
    (refresh-peers app)
    finalization))

(defun drive-finality (app)
  (loop while (validator-app-finalizations app)
        for artifact = (first (validator-app-finalizations app))
        for proposal = (clempo.consensus:finalization-proposal artifact)
        for digest = (clempo.consensus:proposal-payload proposal)
        do (multiple-value-bind (blocks valid missing)
               (if (block-log-height (validator-app-log app) digest)
                   ;; Already finalized here (e.g. replayed from the journal after a restart):
                   ;; its block is no candidate, so asking for it would never end.
                   (values nil t nil)
                   (branch app digest))
             (cond (missing (ask-block app missing) (return))
                   ((not valid) (error "verified finalization does not extend our finalized head"))
                   (t
                    (pop (validator-app-finalizations app))
                    (when blocks
                      (let* ((block (car (last blocks)))
                             (context (clempo.headers:header-consensus-context (header-of block)))
                             (view (clempo.consensus:proposal-view proposal)))
                        (invariant (and context
                                        (= (clempo.consensus:proposal-epoch proposal) (clempo.headers:consensus-context-epoch context))
                                        (or (and (= view (clempo.headers:consensus-context-view context))
                                                 (= (clempo.consensus:proposal-parent proposal)
                                                    (clempo.headers:consensus-context-parent-view context)))
                                            ;; the epoch's last block, re-proposed in a later view
                                            (and (boundary-p app (number-of block))
                                                 (> view (clempo.headers:consensus-context-view context))
                                                 (>= (clempo.consensus:proposal-parent proposal)
                                                     (clempo.headers:consensus-context-view context)))))
                                   "finalization proposal metadata mismatch"))
                      (commit-finalization app artifact blocks)))))))

;;; Catching up (marshal backfill). A message from a later epoch shows the network finalized
;;; this epoch's last block (epoch manager handle_msg_for_unregistered_epoch hints the
;;; marshal); a block fetched for a finalization far above the head shows the gap. The
;;; validator then requests Finalized{height} for the heights above its head, at most
;;; BACKFILL-FREQUENCY per second from each peer (Tempo's marshal channel quota), verifies
;;; each certificate under its epoch's identity, and commits in height order; every boundary
;;; enters the next epoch and with it the identity that verifies the following heights.

(defun note-backfill-target (app height)
  (when (> height (head-number app))
    (let ((target (validator-app-backfill-target app)))
      (unless (and target (>= target height))
        (setf (validator-app-backfill-target app) height)
        (report app "behind" "head" (head-number app) "target" height)))))

(defun note-peer-epoch (app peer epoch)
  "PEER sent a message for EPOCH. One from a later epoch proves the current epoch's last
block is finalized."
  (declare (ignore peer))
  (when (> epoch (epoch app))
    (note-backfill-target app (epoch-last-height app))))

(defun backfill-peer (app now refused)
  "A connected peer, not among REFUSED (those that answered this height with an error),
whose marshal quota admits a request now, or NIL."
  (let ((gap (ceiling 1000 (max 1 (validator-app-backfill-frequency app)))))
    (loop for peer in (network-peers (validator-app-network app))
          for next = (gethash peer (validator-app-backfill-next-ms app) 0)
          when (and (<= next now) (not (member peer refused :test #'octets=)))
            do (setf (gethash peer (validator-app-backfill-next-ms app)) (+ now gap))
               (return peer))))

(defun drive-backfill (app)
  (let ((target (validator-app-backfill-target app)))
    (when target
      (when (<= target (head-number app))
        (setf (validator-app-backfill-target app) nil)
        (clrhash (validator-app-backfill-buffer app))
        (clrhash (validator-app-backfill-missing app))
        (report app "caught-up" "head" (head-number app))
        (return-from drive-backfill))
      (expire-requests app)
      (let ((now (io:now-ms))
            (outstanding (loop for request being the hash-values of (validator-app-requests app)
                               when (eq (fetch-kind request) :finalized)
                                 collect (clempo.validator-wire:marshal-key-height (fetch-key request)))))
        (loop for height from (1+ (max (head-number app) (validator-app-backfill-queued app)))
                to (min target (+ (head-number app) +backfill-window+))
              unless (or (member height outstanding)
                         (gethash height (validator-app-backfill-buffer app)))
                do (let ((peer (backfill-peer app now (gethash height (validator-app-backfill-missing app)))))
                     ;; Every peer refused it or none is free yet; APPLY-BACKFILL skips a
                     ;; height no peer holds a certificate for.
                     (when peer
                       (send-request app 4 (clempo.validator-wire:make-marshal-key :kind :finalized :height height)
                                     :finalized peer +backfill-timeout-ms+)))))
      (apply-backfill app))))

(defun current-finalization-p (app finalization)
  "FINALIZATION is of the current epoch and verifies under its identity."
  (and (= (clempo.consensus:round-epoch (clempo.consensus:certificate-round finalization)) (epoch app))
       (clempo.consensus:verify-artifact (validator-app-scheme app) finalization)))

(defun apply-backfill (app)
  "Commit buffered finalized blocks in height order while their certificates verify under
the current epoch's identity. A height every connected peer answered with an error is
skipped for the next buffered certificate, whose ancestors then arrive by digest."
  (loop
    (let* ((height (1+ (head-number app)))
           (buffer (validator-app-backfill-buffer app))
           (entry (gethash height buffer)))
      (cond (entry
             (remhash height buffer)
             (destructuring-bind (block finalization . peer) entry
               (cond ((not (current-finalization-p app finalization))
                      (block-peer app peer "invalid marshal finalization")
                      (return))
                     ((not (octets= (parent-of block) (head-hash app)))
                      (error "verified finalization at ~D does not extend our finalized head" height))
                     (t (stash-block app block)
                        (commit-finalization app finalization (list block))))))
            ((and (<= (validator-app-backfill-queued app) (head-number app))
                  (let ((peers (network-peers (validator-app-network app)))
                        (refused (gethash height (validator-app-backfill-missing app))))
                    (and peers (every (lambda (p) (member p refused :test #'octets=)) peers))))
             ;; No peer holds this height's own certificate (it was finalized as an ancestor):
             ;; queue the next certified height; its ancestors arrive by digest.
             (let ((later (loop for h being the hash-keys of buffer minimize h)))
               (when (and later (> later height))
                 (destructuring-bind (block finalization . peer) (gethash later buffer)
                   (remhash later buffer)
                   (cond ((current-finalization-p app finalization)
                          (stash-block app block)
                          (setf (validator-app-backfill-queued app) later
                                (validator-app-finalizations app)
                                (nconc (validator-app-finalizations app) (list finalization))))
                         (t (block-peer app peer "invalid marshal finalization")))))
               (return)))
            (t (return))))))

;;; Pending application answers and pruning.

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

;;; Serving peers and handling their messages.

(defun finalized-response (app key)
  "A marshal Finalized{height} value from the block log: the height's finalization and block,
or NIL when the height had no certificate of its own here."
  (let ((record (block-log-record (validator-app-log app) (clempo.validator-wire:marshal-key-height key))))
    (when (and record (log-record-finalization record))
      (clempo.validator-wire:encode-marshal-value
       key (clempo.blocks:decode-block (log-record-block record)) (log-record-finalization record)))))

(defun block-response (app key)
  "A marshal Block{digest} value: a candidate, or a finalized block from the log."
  (let* ((digest (clempo.validator-wire:marshal-key-digest key))
         (block (or (known-block app digest)
                    (let ((height (block-log-height (validator-app-log app) digest)))
                      (when height
                        (clempo.blocks:decode-block
                         (log-record-block (block-log-record (validator-app-log app) height))))))))
    (when block (clempo.validator-wire:encode-marshal-value key block nil))))

(defun answer-request (app channel peer message)
  (let* ((key (clempo.validator-wire:resolver-message-key message))
         (value (if (= channel 2)
                    (let ((artifact (clempo.simplex:voter-certificate (validator-app-voter app) key)))
                      (when artifact (clempo.cwcodec:cw-encode 'clempo.consensus:certificate artifact)))
                    (case (clempo.validator-wire:marshal-key-kind key)
                      (:block (block-response app key))
                      (:finalized (finalized-response app key))
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

(defun handle-response (app peer request message)
  (let ((data (clempo.validator-wire:resolver-message-value message)))
    (ecase (fetch-kind request)
      (:block
       (let ((block (clempo.validator-wire:decode-marshal-value (fetch-key request) data)))
         (when (> (number-of block) (1+ (head-number app)))
           ;; the parent of a finalized or notarized block is known to exist
           (note-backfill-target app (1- (number-of block))))
         (stash-block app block)))
      (:finalized
       (multiple-value-bind (block finalization)
           (clempo.validator-wire:decode-marshal-value (fetch-key request) data)
         (let ((height (number-of block)))
           (when (> height (head-number app))
             (setf (gethash height (validator-app-backfill-buffer app))
                   (list* block finalization peer))))))
      (:certificate
       (let ((artifact (clempo.cwcodec:cw-decode 'clempo.consensus:certificate data)))
         (multiple-value-bind (effects disposition)
             (clempo.simplex:voter-deliver (validator-app-voter app) (fetch-key request) artifact)
           (apply-effects app effects)
           (when (eq :invalid disposition)
             (block-peer app peer "resolver certificate does not satisfy its request"))))))))

(defun handle-resolver (app channel peer octets)
  (let* ((message (clempo.validator-wire:decode-resolver-message channel octets))
         (kind (clempo.validator-wire:resolver-message-kind message))
         (id (clempo.validator-wire:resolver-message-id message))
         (requests (validator-app-requests app)))
    (cond ((and (= channel 2) (/= (clempo.validator-wire:resolver-message-epoch message) (epoch app)))
           (note-peer-epoch app peer (clempo.validator-wire:resolver-message-epoch message)))
          ((eq kind :request) (answer-request app channel peer message))
          (t
           (let ((request (gethash id requests)))
             (when (and request (= channel (fetch-channel request))
                        (or (null (fetch-peer request)) (octets= peer (fetch-peer request))))
               (remhash id requests)
               (if (eq kind :response)
                   (handle-response app peer request message)
                   (when (eq (fetch-kind request) :finalized)
                     (pushnew peer (gethash (clempo.validator-wire:marshal-key-height (fetch-key request))
                                            (validator-app-backfill-missing app))
                              :test #'octets=)))))))))

(defun handle-consensus (app channel peer octets)
  (multiple-value-bind (message e)
      (clempo.consensus:decode-epoch-mux
       (if (zerop channel) 'clempo.consensus:vote 'clempo.consensus:certificate) octets)
    (if (= e (epoch app))
        (multiple-value-bind (effects disposition)
            (clempo.simplex:voter-receive (validator-app-voter app) peer message)
          (apply-effects app effects)
          (when (eq disposition :block)
            (block-peer app peer (if (zerop channel) "invalid vote" "invalid certificate"))))
        (note-peer-epoch app peer e))))

(defun handle-network-event (app event)
  (let ((peer (network-event-peer event)) (kind (network-event-kind event)))
    (case kind
      (:connected (report app "connected" "peer" (octets->hex peer)))
      (:disconnected (report app "disconnected" "peer" (octets->hex peer)))
      (:data
       (let ((octets (subseq (network-event-frame event) (network-event-start event) (network-event-end event)))
             (channel (network-event-channel event)))
         ;; commonware blocks a peer whose message does not decode, an invalid DKG message
         ;; (dkg actor), or a marshal response that does not match its request.
         (handler-case
             (case channel
               ((0 1) (handle-consensus app channel peer octets))
               ((2 4) (handle-resolver app channel peer octets))
               (3 (stash-block app (clempo.validator-wire:decode-broadcast-block octets)))
               (5 (handle-dkg app peer octets)))
           ((or clempo.dkg:dkg-error clempo.cwcodec:cw-error clempo.rlp:rlp-error) (condition)
             (block-peer app peer (format nil "channel ~D: ~A" channel condition)))))))
    (dolist (key (clempo.simplex:voter-take-blocked (validator-app-voter app)))
      (block-peer app key "vote signature does not verify"))))

(defun step-validator (app &key (timeout 0.012))
  "One turn of the event loop: a network event, then finality, catch-up, pending answers and
timeouts."
  (let ((event (network-receive (validator-app-network app) :timeout timeout)))
    (when event (handle-network-event app event)))
  (drive-finality app)
  (drive-backfill app)
  (drive-pending app)
  (prune-views app)
  (apply-effects app (clempo.simplex:voter-tick (validator-app-voter app))))
