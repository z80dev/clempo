(in-package #:clempo.sim)

;;; Validators as Tempo runs them, reduced to what consensus needs: each block carries its
;;; height, parent, proposer and extraData (a dealer's signed DKG log, or at an epoch's last
;;; height the DKG outcome that installs the next epoch). Every epoch is a fresh simplex
;;; instance whose genesis is the previous boundary block; its signers hold the shares the
;;; previous epoch's ceremony (a reshare that keeps the group key) gave them.

;;; Blocks.

(clempo.cwcodec:defcodec sim-block
  (height :varint)
  (parent (:array 32))
  (proposer (:array 32))
  (extra :bytes))

(defun block-digest (b) (clempo.keccak:keccak256 (clempo.cwcodec:cw-encode 'sim-block b)))

;;; Per-epoch state of one node.

(defstruct (epoch-state (:conc-name es-))
  epoch outcome info scheme voter share genesis
  dealer dealings player own-log (log-finalized nil) (acked '()) (acks-sent '()))

(defstruct (node (:constructor %make-node))
  index seed key
  (blocks (make-hash-table :test 'equalp))     ; digest -> sim-block
  (digests (make-hash-table))                  ; finalized height -> digest
  (certs (make-hash-table))                    ; finalized height -> finalization
  (finalized-height 0)
  (epochs (make-hash-table))                   ; epoch -> epoch-state
  (epoch 0)
  (pending-verify '())                         ; (voter round digest context)
  (pending-final '())                          ; finalizations whose blocks are missing
  (synced '())                                 ; backfilled (block . cert) awaiting a cert
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
(defun mid-height (node epoch) (+ (* epoch (node-l node)) (floor (node-l node) 2)))

(defun current (node) (gethash (node-epoch node) (node-epochs node)))

;;; Sending.

(defun send (node to message &rest keys)
  (if (= to (node-index node))
      (receive node (node-index node) message)
      (apply #'network-send (node-net node) (node-index node) to message keys)))

(defun broadcast (node message)
  (network-broadcast (node-net node) (node-index node) message))

;;; The application the voter calls.

(defun branch (node digest epoch)
  "Blocks from EPOCH's genesis (excluded) to DIGEST, oldest first, or :missing."
  (let ((genesis (es-genesis (gethash epoch (node-epochs node)))))
    (loop with out = '()
          for d = digest then (sim-block-parent b)
          for b = (gethash d (node-blocks node))
          until (octets= d genesis)
          do (if b (push b out) (return :missing))
          finally (return out))))

(defun ceremony-outcome (node es extras)
  "The boundary outcome for ES's epoch + 1 from the epoch's dealer logs in EXTRAS."
  (let* ((logs (clempo.dkg:collect-dealer-logs (es-info es) extras))
         (output (clempo.dkg:conclude-ceremony logs (clempo.dkg:outcome-output (es-outcome es)))))
    (clempo.dkg:boundary-outcome output (sim-keys (node-sim node))
                                 :epoch (1+ (es-epoch es)))))

(defun block-extra (node es parent-block height branch)
  "What a block at HEIGHT carries: the DKG outcome at the epoch's last height, else this
node's signed dealer log while it is not yet in the branch."
  (declare (ignore parent-block))
  (cond ((= height (last-height node (es-epoch es)))
         (clempo.dkg:encode-onchain-dkg-outcome
          (ceremony-outcome node es (mapcar #'sim-block-extra branch))))
        ((and (es-own-log es)
              (notany (lambda (b) (octets= (sim-block-proposer b) (node-key node))) 
                      (remove-if (lambda (b) (zerop (length (sim-block-extra b)))) branch)))
         (clempo.cwcodec:cw-encode 'clempo.dkg:signed-dealer-log (es-own-log es)))
        (t (make-octets 0))))

(defmethod clempo.simplex:app-propose ((node node) context)
  (let* ((parent (cdr (clempo.simplex:context-parent context)))
         (epoch (clempo.simplex:context-epoch context))
         (es (gethash epoch (node-epochs node)))
         (pb (gethash parent (node-blocks node))))
    (cond ((null pb) nil)
          ;; the epoch's last block is re-proposed until it is finalized
          ((= (sim-block-height pb) (last-height node epoch)) parent)
          (t (let ((branch (branch node parent epoch)))
               (if (eq branch :missing)
                   nil
                   (let* ((height (1+ (sim-block-height pb)))
                          (b (make-sim-block :height height :parent parent :proposer (node-key node)
                                             :extra (block-extra node es pb height branch)))
                          (d (block-digest b)))
                     (setf (gethash d (node-blocks node)) b
                           (gethash d (sim-proposers (node-sim node))) (node-index node))
                     (broadcast node (list :block b))
                     d)))))))

(defun check-proposal (node context digest)
  "T, NIL or :pending (block not here yet)."
  (let* ((parent (cdr (clempo.simplex:context-parent context)))
         (epoch (clempo.simplex:context-epoch context))
         (es (gethash epoch (node-epochs node)))
         (pb (gethash parent (node-blocks node)))
         (b (gethash digest (node-blocks node))))
    (cond ((or (null pb) (null b)) :pending)
          ((and (octets= digest parent) (= (sim-block-height pb) (last-height node epoch))) t)
          ((not (octets= (sim-block-parent b) parent)) nil)
          ((/= (sim-block-height b) (1+ (sim-block-height pb))) nil)
          ((> (sim-block-height b) (last-height node epoch)) nil)
          (t (let ((branch (branch node parent epoch)))
               (cond ((eq branch :missing) :pending)
                     ((= (sim-block-height b) (last-height node epoch))
                      (octets= (sim-block-extra b)
                               (clempo.dkg:encode-onchain-dkg-outcome
                                (ceremony-outcome node es (mapcar #'sim-block-extra branch)))))
                     ((zerop (length (sim-block-extra b))) t)
                     (t (handler-case
                            (multiple-value-bind (dealer)
                                (clempo.dkg:extra-data-dealer-log (sim-block-extra b) (es-info es))
                              (octets= dealer (sim-block-proposer b)))
                          (clempo.dkg:dkg-error () nil)))))))))

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
  (let ((pending (node-pending-verify node)))
    (setf (node-pending-verify node) '())
    (dolist (p pending)
      (destructuring-bind (epoch context digest) p
        (let ((es (gethash epoch (node-epochs node)))
              (r (check-proposal node context digest)))
          (if (eq r :pending)
              (push p (node-pending-verify node))
              (when es
                (run-effects node es (clempo.simplex:voter-verified
                                      (es-voter es) (clempo.simplex:context-round context)
                                      digest r)))))))))

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
  (let ((h (sim-block-height b)))
    (setf (gethash h (node-digests node)) digest
          (node-finalized-height node) h)
    (when cert (setf (gethash h (node-certs node)) cert))
    (let ((es (gethash (height-epoch node h) (node-epochs node))))
      (when es
        (when (and (es-own-log es) (octets= (sim-block-proposer b) (node-key node))
                   (plusp (length (sim-block-extra b))) (/= h (last-height node (es-epoch es))))
          (setf (es-log-finalized es) t))
        (when (and (es-dealer es) (null (es-own-log es)) (>= h (mid-height node (es-epoch es))))
          (setf (es-own-log es) (clempo.dkg:finalize-dealer (es-dealer es))))
        (when (and (es-dealer es) (< h (mid-height node (es-epoch es))))
          (distribute-dealings node es))
        (when (= h (last-height node (es-epoch es)))
          (enter-epoch node b digest))))))

;;; Epochs and the DKG.

(defun epoch-scheme (node outcome share)
  (let* ((sharing (clempo.dkg:outcome-sharing outcome))
         (coefficients (clempo.dkg:sharing-coefficients sharing))
         (total (clempo.dkg:sharing-total sharing)))
    (if share
        (c:make-signer-scheme coefficients total
                              (cons (clempo.dkg:share-index share) (clempo.dkg:share-private share)))
        (c:make-verifier-scheme coefficients total
                                :me (position (node-key node) (clempo.dkg:outcome-players outcome)
                                              :test #'octets=)))))

(defun start-epoch (node outcome share genesis)
  "Run OUTCOME's epoch: a voter over its sharing, and this node's part in its ceremony."
  (let* ((epoch (clempo.dkg:outcome-epoch outcome))
         (info (clempo.dkg:round-info-from-outcome outcome :v1-active t))
         (players (clempo.dkg:outcome-players outcome))
         (me (position (node-key node) players :test #'octets=))
         (scheme (epoch-scheme node outcome share))
         (es (make-epoch-state :epoch epoch :outcome outcome :info info :scheme scheme
                               :share share :genesis genesis)))
    (setf (es-voter es)
          (clempo.simplex:make-voter :scheme scheme :epoch epoch :participants players
                                     :me (and share me) :application node :genesis genesis
                                     :elector :random-v1
                                     :timeouts (node-config node :timeouts)))
    (when (member (node-key node) (clempo.dkg:info-dealers info) :test #'octets=)
      (multiple-value-bind (dealer pub privates)
          (clempo.dkg:start-dealer info (node-seed node) :share share)
        (setf (es-dealer es) dealer (es-dealings es) (cons pub privates))))
    (when (member (node-key node) (clempo.dkg:info-players info) :test #'octets=)
      (setf (es-player es) (clempo.dkg:make-player info (node-seed node))))
    (setf (gethash epoch (node-epochs node)) es
          (node-epoch node) epoch)
    (when (es-dealer es) (distribute-dealings node es))
    (run-effects node es (clempo.simplex:voter-start (es-voter es)))
    es))

(defun distribute-dealings (node es)
  "Send each player that has not acked its dealing (again, as Tempo does on every finalized
block of the epoch's first half)."
  (destructuring-bind (pub . privates) (es-dealings es)
    (loop for (player . scalar) in privates
          for to = (position player (sim-keys (node-sim node)) :test #'octets=)
          unless (member player (es-acked es) :test #'octets=)
            do (send node to (list :dkg (es-epoch es) (node-key node) pub scalar)))))

(defun enter-epoch (node boundary digest)
  "The boundary block of the current epoch is final: conclude the ceremony and start the next."
  (let* ((es (gethash (height-epoch node (sim-block-height boundary)) (node-epochs node)))
         (outcome (clempo.dkg:decode-onchain-dkg-outcome (sim-block-extra boundary)))
         (extras (loop for h from (* (es-epoch es) (node-l node)) below (sim-block-height boundary)
                       collect (sim-block-extra (gethash (gethash h (node-digests node))
                                                         (node-blocks node)))))
         (logs (clempo.dkg:collect-dealer-logs (es-info es) extras)))
    (multiple-value-bind (output share ok)
        (clempo.dkg:conclude-ceremony logs (clempo.dkg:outcome-output (es-outcome es))
                                      :player (es-player es))
      (invariant (octets= (clempo.cwcodec:cw-encode 'clempo.dkg:output output)
                          (clempo.cwcodec:cw-encode 'clempo.dkg:output (clempo.dkg:outcome-output outcome)))
                 "node ~D: local ceremony output differs from the boundary outcome" (node-index node))
      (push (cons (es-epoch es) (and ok t)) (node-ceremonies node))
      (start-epoch node outcome (if ok share (es-share es)) digest))))
