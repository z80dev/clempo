(in-package #:clempo.validator-process.tests)

;;; The devnet's four validators (tests/validator/data/devnet-r6-keys, genesis in
;;; tests/tempo/executor/data/devnet-r6) run in one thread over an in-memory network: real
;;; execution, Simplex, DKG, block logs and journals in temporary datadirs.

;;; Validators.

(defun devnet-key-file (slot name)
  (asdf:system-relative-pathname
   "clempo-validator" (format nil "../../tests/validator/data/devnet-r6-keys/~D/~A" slot name)))

(defun devnet-seed (slot) (clempo.io:read-file-octets (devnet-key-file slot "signing.seed")))

(defstruct node slot dir network app (events '()))

(defparameter *test-timeouts*
  (list* :leader 400 :certification 600 :retry 400 clempo.simplex:*tempo-timeouts*))

(defun start-node (node chain)
  "Open NODE's validator on its datadir (fresh or resumed) and join the hub."
  (let* ((hub (mem-hub (node-network node)))
         (key (mem-key (node-network node))))
    (setf (hub-offline hub) (remove key (hub-offline hub) :test #'octets=)
          (node-app node)
          (net::open-validator-app
           :chain chain :datadir (node-dir node) :seed (devnet-seed (node-slot node))
           :share (net:read-signing-share (devnet-key-file (node-slot node) "signing.share"))
           :network (node-network node) :timeouts *test-timeouts*
           :reporter (lambda (row) (push row (node-events node)))))))

(defun stop-node (node)
  "Close NODE's validator cleanly; its peers stop hearing from it."
  (let ((network (node-network node)))
    (push (mem-key network) (hub-offline (mem-hub network)))
    (setf (car (mem-inbox network)) nil (cdr (mem-inbox network)) nil)
    (net::close-validator-app (shiftf (node-app node) nil))))

(defun node-head (node) (if (node-app node) (net::head-number (node-app node)) -1))

(defun step-node (node)
  (let ((app (node-app node)))
    (when app
      (loop for event = (net:network-receive (node-network node)) while event
            do (net::handle-network-event app event))
      (net::step-validator app :timeout 0))))

(defun run-nodes (nodes until &key (seconds 180))
  "Step every running node until (funcall UNTIL) or SECONDS of wall time."
  (loop with end = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        until (funcall until)
        do (when (> (get-internal-real-time) end) (return nil))
           (mapc #'step-node nodes)
        finally (return t)))

(defun crash-node (node)
  "Kill NODE's validator as a crash would: its state store's unsynced tail is lost (the
block log and journals sync every record)."
  (let* ((network (node-network node)) (app (shiftf (node-app node) nil)))
    (push (mem-key network) (hub-offline (mem-hub network)))
    (setf (car (mem-inbox network)) nil (cdr (mem-inbox network)) nil)
    (net::close-block-log (net::validator-app-log app))
    (close (clempo.store::store-log-stream (net::validator-app-store app)) :abort t)))

(defun call-with-cluster (function)
  "FUNCTION receives the four nodes, started from genesis in fresh datadirs."
  (let* ((chain (clempo.chainspec:load-chainspec (devnet-file "genesis.json")))
         (hub (make-hub))
         (nodes (loop for slot below 4
                      for network = (make-instance 'memory-network :hub hub
                                                                   :key (clempo.ed25519:ed25519-public-key (devnet-seed slot)))
                      do (push network (hub-nodes hub))
                      collect (make-node :slot slot :dir (make-temp-dir) :network network))))
    (unwind-protect
         (progn
           (dolist (node nodes) (start-node node chain))
           (funcall function nodes chain hub))
      (dolist (node nodes)
        (when (node-app node) (ignore-errors (net::close-validator-app (node-app node))))
        (uiop:delete-directory-tree (node-dir node) :validate t)))))

(defun events-of (node kind)
  (reverse (remove kind (node-events node) :test-not #'string= :key (lambda (e) (json:jget e "event")))))

(defun logged-hash (node height)
  (let ((record (net::block-log-record (net::validator-app-log (node-app node)) height)))
    (when record (clempo.rpc.chain:block-hash-of-raw (net::log-record-block record)))))

(defun check-agreement (nodes to)
  "Every running node logged the same block at each height up to TO."
  (loop for height from 0 to to
        for hashes = (loop for node in nodes when (node-app node) collect (logged-hash node height))
        always (and (every #'identity hashes) (every (lambda (h) (octets= h (first hashes))) hashes))))

(defun signing-epochs (node)
  (loop for e in (events-of node "epoch")
        when (eq :true (json:jget e "signing")) collect (json:jget e "epoch")))

(deftest cluster-reshares-restarts-and-catches-up ()
  ;; Four clempo validators finalize the devnet chain through two reshares. Validator 3 is
  ;; stopped mid-epoch and restarted from its datadir (replaying its journal, so it cannot
  ;; vote against itself), and later stopped for more than an epoch and restarted far
  ;; behind: it backfills finalized blocks over marshal, verifying each certificate under its
  ;; epoch's identity, reshares epoch by epoch, and signs again. No honest validator is
  ;; ever blocked, and every validator logs the same chain.
  (call-with-cluster
   (lambda (nodes chain hub)
     (let ((n3 (fourth nodes)) (others (subseq nodes 0 3)))
       (is (run-nodes nodes (lambda () (every (lambda (n) (>= (node-head n) 20)) nodes))))
       ;; a crash and restart within the epoch
       (crash-node n3)
       (is (run-nodes others (lambda () (every (lambda (n) (>= (node-head n) 35)) others))))
       (start-node n3 chain)
       (is (find "replaying" (node-events n3) :test #'string= :key (lambda (e) (json:jget e "event")))
           "the restarted validator re-executed what its state store had not synced")
       (is (run-nodes nodes (lambda () (every (lambda (n) (>= (node-head n) 62)) nodes))))
       (is (every (lambda (n) (member 1 (signing-epochs n))) nodes) "all four sign in epoch 1")
       ;; far behind: offline for more than an epoch
       (stop-node n3)
       (let ((stopped-at (node-head n3)))
         (declare (ignorable stopped-at))
         (is (run-nodes others (lambda () (every (lambda (n) (>= (node-head n) 125)) others))))
         (start-node n3 chain)
         (is (run-nodes nodes (lambda () (>= (node-head n3) (node-head (first nodes)) 125))))
         (is (find "behind" (node-events n3) :test #'string= :key (lambda (e) (json:jget e "event")))
             "the validator saw it was behind")
         (is (find "caught-up" (node-events n3) :test #'string= :key (lambda (e) (json:jget e "event")))))
       ;; and it votes again: the cluster finalizes with validator 3 proposing
       (let ((proposed (length (events-of n3 "proposed"))))
         (is (run-nodes nodes (lambda () (> (length (events-of n3 "proposed")) proposed)))
             "validator 3 proposes again after catching up"))
       (is (member 2 (signing-epochs n3)) "validator 3 signs in epoch 2")
       (is (check-agreement nodes (reduce #'min (mapcar #'node-head nodes))))
       (is (null (hub-blocks hub)) "no honest validator was blocked: ~S" (hub-blocks hub))))))
