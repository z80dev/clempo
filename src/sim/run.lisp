(in-package #:clempo.sim)

;;; Message handling. Messages on the simulated wire:
;;;   (:consensus vote-or-certificate)   (:block block)        (:want-block digest)
;;;   (:want-cert epoch view)            (:dkg epoch dealer-key pub-msg scalar)
;;;   (:ack epoch player-key ack)        (:sync height)        (:synced ((block . cert) ...))

(defun artifact-epoch (m)
  (c:round-epoch (typecase m
                   ((or c:notarize c:nullify c:finalize) (c:vote-round m))
                   (t (c:certificate-round m)))))

(defun node-of (node index) (aref (sim-nodes (node-sim node)) index))

(defun request-sync (node from)
  "Ask FROM for the finalized blocks after ours (at most once per second)."
  (when (> (io:now-ms) (+ (node-last-sync node) 1000))
    (setf (node-last-sync node) (io:now-ms))
    (send node from (list :sync (1+ (node-finalized-height node))) :reliable t)))

(defun receive (node from message)
  (let ((sender-key (aref (sim-keys-vector (node-sim node)) from)))
    (ecase (first message)
      (:consensus
       (let* ((m (second message))
              (es (gethash (artifact-epoch m) (node-epochs node))))
         (cond (es (run-effects node es (clempo.simplex:voter-receive (es-voter es) sender-key m)))
               ((> (artifact-epoch m) (node-epoch node)) (request-sync node from)))))
      (:block
       (let* ((b (second message)) (d (block-digest b)))
         (unless (gethash d (node-blocks node))
           (setf (gethash d (node-blocks node)) b)
           (retry-verifications node)
           (let ((finals (node-pending-final node)))
             (setf (node-pending-final node) '())
             (mapc (lambda (f) (on-finalization node f)) (reverse finals))))))
      (:want-block
       (let ((b (gethash (second message) (node-blocks node))))
         (when b (send node from (list :block b) :reliable t))))
      (:want-cert
       (destructuring-bind (epoch view) (rest message)
         (let* ((es (gethash epoch (node-epochs node)))
                (cert (and es (clempo.simplex:voter-certificate (es-voter es) view))))
           (when cert (send node from (list :consensus cert))))))
      (:dkg
       (destructuring-bind (epoch dealer pub scalar) (rest message)
         (let ((es (gethash epoch (node-epochs node))))
           (when (and es (es-player es))
             (let ((ack (or (cdr (assoc dealer (es-acks-sent es) :test #'octets=))
                            (handler-case
                                (clempo.dkg:player-dealer-message (es-player es) dealer pub scalar)
                              (clempo.dkg:dkg-error () nil)))))
               (when ack
                 (pushnew (cons dealer ack) (es-acks-sent es) :key #'car :test #'octets=)
                 (send node from (list :ack epoch (node-key node) ack))))))))
      (:ack
       (destructuring-bind (epoch player ack) (rest message)
         (let ((es (gethash epoch (node-epochs node))))
           (when (and es (es-dealer es) (null (es-own-log es))
                      (not (member player (es-acked es) :test #'octets=)))
             (handler-case
                 (progn (clempo.dkg:dealer-receive-player-ack (es-dealer es) player ack)
                        (push player (es-acked es)))
               (clempo.dkg:dkg-error () nil))))))
      (:sync
       (let ((items (loop for h from (second message)
                            to (min (node-finalized-height node) (+ (second message) (* 3 (node-l node))))
                          for d = (gethash h (node-digests node))
                          collect (cons (gethash d (node-blocks node)) (gethash h (node-certs node))))))
         (when items (send node from (list :synced items) :reliable t))))
      (:synced (ingest-synced node (second message))))))

(defun ingest-synced (node items)
  "Backfilled finalized blocks, in height order: store them, and finalize each run that ends
in a certificate verified under its epoch's scheme."
  (loop for (b . cert) in items
        do (setf (gethash (block-digest b) (node-blocks node)) b)
           (when cert
             (let ((es (gethash (c:proposal-epoch (c:finalization-proposal cert)) (node-epochs node))))
               (when (and es (> (sim-block-height b) (node-finalized-height node))
                          (c:verify-artifact (es-scheme es) cert))
                 (on-finalization node cert)))))
  (retry-verifications node))

;;; Scenarios.

(defvar *scenarios* (make-hash-table :test 'equal))

(defmacro defscenario (name &body plist)
  "Declare a simulation scenario:
  (defscenario \"4-faults\" :validators 4 :seed 7 :epochs 3 :epoch-length 12
    :latency (5 40) :drop 1/50 :faults ((:outage 3 :from 3000 :to 9000)))
Faults: (:outage NODE :from MS :to MS), (:silent NODE), (:partition (GROUP...) :from :to)."
  `(setf (gethash ,name *scenarios*) (list* :name ,name ',plist)))

(defun find-scenario (name)
  (or (gethash name *scenarios*) (error "no scenario ~S" name)))

(defun scenario-names ()
  (sort (loop for k being the hash-keys of *scenarios* collect k) #'string<))

(defparameter *default-timeouts* '(:leader 600 :certification 1200 :retry 2000)
  "Simulation timeouts (ms), shorter than Tempo's so faults resolve quickly.")

(defstruct sim-result
  name chain finalized-height epochs messages dropped ms identity identities proposers
  genesis-extra genesis-digest epoch-length heights-by-node summary ceremonies)

(defun sim-keys-vector (sim) (coerce (sim-keys sim) 'simple-vector))

(defun validator-seed (seed i)
  (clempo.keccak:keccak256 (octets-concat (integer->octets seed 8) (integer->octets i 4))))

(defun make-simulation (config)
  (let* ((n (getf config :validators))
         (seeds (loop for i below n collect (validator-seed (getf config :seed) i)))
         (pairs (sort (loop for s in seeds collect (cons (clempo.ed25519:ed25519-public-key s) s))
                      #'clempo.cwcodec:cw-order< :key #'car))
         (keys (mapcar #'car pairs))
         (net (make-network n :latency (getf config :latency '(10 50))
                              :drop (getf config :drop 0) :faults (getf config :faults)))
         (sim (%make-sim :config config :network net :keys keys)))
    (multiple-value-bind (output shares) (clempo.dkg:deal keys)
      (let* ((outcome (clempo.dkg:boundary-outcome output keys :epoch 0))
             (genesis (make-sim-block :height 0 :parent (make-octets 32) :proposer (make-octets 32)
                                      :extra (clempo.dkg:encode-onchain-dkg-outcome outcome)))
             (digest (block-digest genesis)))
        (setf (sim-genesis sim) genesis (sim-genesis-outcome sim) outcome
              (sim-nodes sim)
              (coerce (loop for (key . seed) in pairs for i from 0
                            collect (%make-node :index i :seed seed :key key :sim sim))
                      'simple-vector))
        (loop for node across (sim-nodes sim)
              do (setf (gethash digest (node-blocks node)) genesis
                       (gethash 0 (node-digests node)) digest)
                 (start-epoch node outcome
                              (cdr (assoc (node-key node) shares :test #'octets=))
                              digest))
        sim))))

(defun node-deadline (node)
  (let ((es (current node)))
    (and es (clempo.simplex:voter-next-deadline (es-voter es)))))

(defun fault-times (net)
  (loop for f in (network-faults net)
        append (list (fault-from f) (fault-to f))))

(defun run-loop (sim clock target max-ms)
  (let ((net (sim-network sim)))
    (loop
      (let* ((now (io:now-ms))
             (up (remove-if-not (lambda (n) (node-up-p (network-faults net) (node-index n) now))
                                (coerce (sim-nodes sim) 'list)))
             (candidates (remove nil (append (list (network-next-time net))
                                             (mapcar #'node-deadline up)
                                             (remove-if-not (lambda (x) (> x now)) (fault-times net)))))
             (next (and candidates (max now (reduce #'min candidates)))))
        ;; done when every validator but the permanently crashed ones has the whole chain
        (when (every (lambda (n) (or (>= (node-finalized-height n) target)
                                     (crashed-p (network-faults net) (node-index n))))
                     (coerce (sim-nodes sim) 'list))
          (return :done))
        (when (or (null next) (> next max-ms))
          (return :timeout))
        (manual-clock-set clock next)
        (dolist (event (network-pop-due net next))
          (destructuring-bind (from to message) (rest event)
            (receive (aref (sim-nodes sim) to) from message)))
        (dolist (n up)
          (let ((d (node-deadline n)))
            (when (and d (<= d next) (node-up-p (network-faults net) (node-index n) next))
              (run-effects n (current n) (clempo.simplex:voter-tick (es-voter (current n)))))))))))

(defun manual-clock-set (handle ms)
  (io:manual-clock-advance handle (- ms (io:now-ms))))

(defun run-scenario (name-or-config &key (max-ms 600000))
  "Run a scenario deterministically; returns a SIM-RESULT."
  (let* ((config (if (stringp name-or-config) (find-scenario name-or-config) name-or-config))
         (config (append config (list :timeouts *default-timeouts*)))
         (io:*entropy* (io:make-seeded-entropy (getf config :seed))))
    (multiple-value-bind (clock handle) (io:make-manual-clock 0)
      (let* ((io:*clock* clock)
             (sim (make-simulation config))
             (l (getf config :epoch-length))
             (target (1- (* (getf config :epochs) l)))
             (status (run-loop sim handle target max-ms)))
        (sim-result sim status)))))

(defun sim-result (sim status)
  "The certified chain (trimmed to the last block with its own certificate) and statistics."
  (let* ((nodes (coerce (sim-nodes sim) 'list))
         (ref (reduce (lambda (a b) (if (>= (node-finalized-height a) (node-finalized-height b)) a b))
                      nodes))
         (l (sim-epoch-length sim))
         (chain (loop for h from 1 to (node-finalized-height ref)
                      for d = (gethash h (node-digests ref))
                      for b = (gethash d (node-blocks ref))
                      collect (list h d (sim-block-parent b) (sim-block-extra b)
                                    (let ((cert (some (lambda (n) (and (equalp d (gethash h (node-digests n)))
                                                                       (gethash h (node-certs n))))
                                                      nodes)))
                                      (and cert (clempo.cwcodec:cw-encode 'c:finalization cert))))))
         (chain (let ((last (position-if #'fifth chain :from-end t)))
                  (if last (subseq chain 0 (1+ last)) '())))
         (top (if chain (first (car (last chain))) 0))
         (proposers (make-array (length nodes) :initial-element 0)))
    ;; safety: every node's finalized digests agree with the reference
    (dolist (n nodes)
      (loop for h from 1 to (node-finalized-height n)
            do (invariant (equalp (gethash h (node-digests n)) (gethash h (node-digests ref)))
                          "nodes ~D and ~D finalized different blocks at height ~D"
                          (node-index n) (node-index ref) h)))
    (loop for (nil d) in chain
          for p = (gethash d (sim-proposers sim))
          when p do (incf (aref proposers p)))
    (make-sim-result
     :name (getf (sim-config sim) :name) :chain chain :finalized-height top
     :epochs (floor (1+ top) l)
     :messages (network-sent (sim-network sim)) :dropped (network-dropped (sim-network sim))
     :ms (io:now-ms)
     :identity (clempo.dkg:outcome-network-identity (sim-genesis-outcome sim))
     :identities (loop for e from 0 to (node-epoch ref)
                       for es = (gethash e (node-epochs ref))
                       when es collect (clempo.dkg:outcome-network-identity (es-outcome es)))
     :proposers (coerce proposers 'list)
     :genesis-extra (sim-block-extra (sim-genesis sim))
     :genesis-digest (block-digest (sim-genesis sim))
     :epoch-length l
     :heights-by-node (mapcar #'node-finalized-height nodes)
     :ceremonies (reverse (node-ceremonies ref))
     :summary status)))

(defun verify-sim-chain (result)
  "Certify the simulated chain with the mainnet certificate verifier: anchored at the
genesis DKG outcome, every block linked from genesis, every certificate verified under its
epoch's identity installed from the previous boundary block. Returns the CERTIFICATION."
  (let ((v (c:make-finalization-verifier :epoch-length (sim-result-epoch-length result))))
    (c:verifier-register-boundary-outcome v (sim-result-genesis-extra result))
    (c:certify-blocks v (sim-result-chain result)
                      :previous-hash (sim-result-genesis-digest result))))
