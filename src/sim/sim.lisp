(in-package #:clempo.sim)

;;; Validators as Tempo runs them, reduced to what consensus needs: each block carries its
;;; height, parent, proposer and extraData (a dealer's signed DKG log, or at an epoch's last
;;; height the DKG outcome that installs the next epoch). Each node runs one clempo.dkg
;;; manager, driven as Tempo's validator drives its DKG actor: every finalized block in
;;; height order, the DKG channel's messages, the dealer log or boundary outcome a proposal
;;; carries and the checks of other proposals. The manager's (:enter epoch output share)
;;; starts a fresh simplex instance whose genesis is the previous boundary block, signing
;;; with the share the ceremony gave this node; (:exit epoch) stops the old one.

;;; Blocks.

(clempo.cwcodec:defcodec sim-block
  (height :varint)
  (parent (:array 32))
  (proposer (:array 32))
  (extra :bytes))

(defun block-digest (b) (clempo.keccak:keccak256 (clempo.cwcodec:cw-encode 'sim-block b)))

(defun dkg-block (b digest)
  "B as the DKG manager sees it. Simulated blocks have no timestamps: T12 is active from
genesis and T15 never, so the manager does not read them."
  (clempo.dkg:make-dkg-block :height (sim-block-height b) :digest digest
                             :parent (sim-block-parent b) :extra-data (sim-block-extra b)))

;;; Per-epoch state of one node: the voter runs from :enter to :exit; the scheme stays to
;;; verify the epoch's certificates.

(defstruct (epoch-state (:conc-name es-))
  epoch output share scheme voter genesis)

(defstruct (node (:constructor %make-node))
  index seed key
  dkg                                          ; the clempo.dkg manager
  (blocks (make-hash-table :test 'equalp))     ; digest -> sim-block
  (digests (make-hash-table))                  ; finalized height -> digest
  (certs (make-hash-table))                    ; finalized height -> finalization
  (finalized-height 0)
  (epochs (make-hash-table))                   ; epoch -> epoch-state
  (epoch 0)
  (pending-verify '())                         ; (epoch context digest)
  (pending-final '())                          ; finalizations whose blocks are missing
  (last-sync 0)
  (ceremonies '())                             ; (epoch . concluded-p), newest first
  (sim nil))

(defstruct (sim (:constructor %make-sim))
  config network nodes keys genesis genesis-outcome
  (proposers (make-hash-table :test 'equalp)))

(defun sim-epoch-length (sim) (getf (sim-config sim) :epoch-length))

(defun node-config (node key) (getf (sim-config (node-sim node)) key))
(defun node-l (node) (node-config node :epoch-length))
(defun node-net (node) (sim-network (node-sim node)))
(defun height-epoch (node h) (floor h (node-l node)))
(defun last-height (node epoch) (1- (* (1+ epoch) (node-l node))))

(defun current (node) (gethash (node-epoch node) (node-epochs node)))

(defun live-voter (node epoch)
  "EPOCH's epoch-state while its voter runs, else NIL."
  (let ((es (gethash epoch (node-epochs node))))
    (and es (es-voter es) es)))

(defun finalized-dkg-block (node height)
  "The manager's view of this node's finalized block at HEIGHT (its HEADER-AT)."
  (let ((d (gethash height (node-digests node))))
    (and d (dkg-block (gethash d (node-blocks node)) d))))

;;; Sending.

(defun send (node to message &rest keys)
  (if (= to (node-index node))
      (receive node (node-index node) message)
      (apply #'network-send (node-net node) (node-index node) to message keys)))

(defun broadcast (node message)
  (network-broadcast (node-net node) (node-index node) message))

(defun key-node-index (node key)
  (position key (sim-keys (node-sim node)) :test #'octets=))

;;; The application the voter calls.

(defun boundary-extra (node parent)
  "The extraData of a boundary block on PARENT (a digest): the manager's outcome over the
notarized branch, supplying each ancestor it names. NIL while an ancestor is missing here
\(it is requested)."
  (let ((m (node-dkg node))
        (pb (gethash parent (node-blocks node))))
    (loop
      (multiple-value-bind (octets missing height digest)
          (clempo.dkg:dkg-boundary-extra-data m (dkg-block pb parent) (sim-keys (node-sim node)) nil)
        (declare (ignore missing))
        (when octets (return octets))
        (let ((b (gethash digest (node-blocks node))))
          (unless b
            (want-block node digest)
            (return nil))
          (invariant (= height (sim-block-height b)) "node ~D: ancestor ~D has height ~D"
                     (node-index node) height (sim-block-height b))
          (clempo.dkg:dkg-notarized-block m (dkg-block b digest)))))))

(defmethod clempo.simplex:app-propose ((node node) context)
  (let* ((parent (cdr (clempo.simplex:context-parent context)))
         (epoch (clempo.simplex:context-epoch context))
         (pb (gethash parent (node-blocks node))))
    (cond ((null pb) nil)
          ;; the epoch's last block is re-proposed until it is finalized
          ((= (sim-block-height pb) (last-height node epoch)) parent)
          (t (let* ((height (1+ (sim-block-height pb)))
                    (extra (if (= height (last-height node epoch))
                               (boundary-extra node parent)
                               (or (clempo.dkg:dkg-dealer-log (node-dkg node) epoch)
                                   (make-octets 0)))))
               (when extra
                 (let* ((b (make-sim-block :height height :parent parent :proposer (node-key node)
                                           :extra extra))
                        (d (block-digest b)))
                   (setf (gethash d (node-blocks node)) b
                         (gethash d (sim-proposers (node-sim node))) (node-index node))
                   (broadcast node (list :block b))
                   d)))))))

(defun check-proposal (node context digest)
  "T, NIL or :pending (block or an ancestor not here yet, or the DKG check unavailable)."
  (let* ((parent (cdr (clempo.simplex:context-parent context)))
         (epoch (clempo.simplex:context-epoch context))
         (pb (gethash parent (node-blocks node)))
         (b (gethash digest (node-blocks node))))
    (cond ((or (null pb) (null b)) :pending)
          ((and (octets= digest parent) (= (sim-block-height pb) (last-height node epoch))) t)
          ((not (octets= (sim-block-parent b) parent)) nil)
          ((/= (sim-block-height b) (1+ (sim-block-height pb))) nil)
          ((> (sim-block-height b) (last-height node epoch)) nil)
          ((= (sim-block-height b) (last-height node epoch))
           (let ((ours (boundary-extra node parent)))
             (if ours (octets= (sim-block-extra b) ours) :pending)))
          ((zerop (length (sim-block-extra b))) t)
          (t (multiple-value-bind (dealer unavailable)
                 (clempo.dkg:dkg-verify-dealer-log (node-dkg node) epoch 0 (sim-block-extra b))
               (cond (unavailable :pending)  ; Tempo abstains
                     (dealer (octets= dealer (sim-block-proposer b)))
                     (t nil)))))))

(defmethod clempo.simplex:app-verify ((node node) context digest)
  (let ((r (check-proposal node context digest)))
    (when (eq r :pending)
      (push (list (clempo.simplex:context-epoch context) context digest)
            (node-pending-verify node))
      (unless (gethash digest (node-blocks node))
        (want-block node digest)))
    r))

(defun want-block (node digest)
  (broadcast node (list :want-block digest)))

(defun retry-verifications (node)
  "Check the pending proposals again; those of stopped voters are dropped."
  (let ((pending (node-pending-verify node)))
    (setf (node-pending-verify node) '())
    (dolist (p pending)
      (destructuring-bind (epoch context digest) p
        (let ((es (live-voter node epoch)))
          (when es
            (let ((r (check-proposal node context digest)))
              (if (eq r :pending)
                  (push p (node-pending-verify node))
                  (run-effects node es (clempo.simplex:voter-verified
                                        (es-voter es) (clempo.simplex:context-round context)
                                        digest r))))))))))

;;; Voter effects.

(defun run-effects (node es effects)
  (dolist (e effects)
    (ecase (first e)
      (:broadcast (broadcast node (list :consensus (second e))))
      (:send (dolist (to (second e)) (send node to (list :consensus (third e)))))
      ((:notarized :nullified) nil)
      (:finalized (on-finalization node (second e)))
      (:request (broadcast node (list :want-cert (es-epoch es) (second e)))))))

;;; Finality.

(defun on-finalization (node f)
  (let* ((digest (c:proposal-payload (c:finalization-proposal f)))
         (b (gethash digest (node-blocks node))))
    (cond ((null b)
           (pushnew f (node-pending-final node))
           (want-block node digest))
          ((<= (sim-block-height b) (node-finalized-height node))
           (unless (gethash (sim-block-height b) (node-certs node))
             (when (octets= digest (gethash (sim-block-height b) (node-digests node)))
               (setf (gethash (sim-block-height b) (node-certs node)) f))))
          (t
           ;; collect the unfinalized ancestors
           (let ((chain '()) (d digest))
             (loop for blk = (gethash d (node-blocks node))
                   do (cond ((null blk)
                             (pushnew f (node-pending-final node))
                             (want-block node d)
                             (return-from on-finalization))
                            ((<= (sim-block-height blk) (node-finalized-height node))
                             (invariant (octets= d (gethash (sim-block-height blk) (node-digests node)))
                                        "node ~D: finalized fork at height ~D"
                                        (node-index node) (sim-block-height blk))
                             (return))
                            (t (push (cons d blk) chain)
                               (setf d (sim-block-parent blk)))))
             (loop for (d . blk) in chain
                   do (finalize-block node d blk (and (octets= d digest) f))))))))

(defun finalize-block (node digest b cert)
  "Record B as final at its height and hand it to the DKG manager (heights arrive in order)."
  (let ((h (sim-block-height b)))
    (setf (gethash h (node-digests node)) digest
          (node-finalized-height node) h)
    (when cert (setf (gethash h (node-certs node)) cert))
    (run-dkg-actions node (clempo.dkg:dkg-finalized-block (node-dkg node) (dkg-block b digest)))))

;;; The DKG manager's actions.

(defun run-dkg-actions (node actions)
  (dolist (a actions)
    (ecase (first a)
      (:send (destructuring-bind (to epoch octets) (rest a)
               (send node (key-node-index node to) (list :dkg epoch octets))))
      (:exit (let ((es (gethash (second a) (node-epochs node))))
               (when es (setf (es-voter es) nil))))
      (:enter (destructuring-bind (epoch output share) (rest a)
                (enter-epoch node epoch output share))))))

(defun epoch-scheme (node output share)
  (let* ((sharing (clempo.dkg:output-public output))
         (coefficients (clempo.dkg:sharing-coefficients sharing))
         (total (clempo.dkg:sharing-total sharing)))
    (if share
        (c:make-signer-scheme coefficients total
                              (cons (clempo.dkg:share-index share) (clempo.dkg:share-private share)))
        (c:make-verifier-scheme coefficients total
                                :me (position (node-key node) (clempo.dkg:output-players output)
                                              :test #'octets=)))))

(defun enter-epoch (node epoch output share)
  "Run EPOCH: a voter over OUTPUT's sharing whose genesis is the previous boundary block."
  (let* ((players (clempo.dkg:output-players output))
         (me (position (node-key node) players :test #'octets=))
         (genesis (gethash (if (zerop epoch) 0 (last-height node (1- epoch))) (node-digests node)))
         (scheme (epoch-scheme node output share))
         (previous (gethash (1- epoch) (node-epochs node)))
         (es (make-epoch-state :epoch epoch :output output :share share :scheme scheme
                               :genesis genesis)))
    (when previous
      ;; a reshare that concluded gives a new output and this node a share of it
      (push (cons (1- epoch) (and share (not (equalp output (es-output previous)))))
            (node-ceremonies node)))
    (setf (es-voter es)
          (clempo.simplex:make-voter :scheme scheme :epoch epoch :participants players
                                     :me (and share me) :application node :genesis genesis
                                     :elector :random-v1
                                     :timeouts (node-config node :timeouts)))
    (setf (gethash epoch (node-epochs node)) es
          (node-epoch node) epoch)
    (run-effects node es (clempo.simplex:voter-start (es-voter es)))
    es))
