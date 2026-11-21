(defpackage #:clempo.sim
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:c #:clempo.consensus) (#:io #:clempo.io))
  (:documentation "A seeded simulation of Tempo validators: N nodes run the simplex voter and
a clempo.dkg DKG manager on a simulated network with latency, drops, partitions and outages. Time is
clempo.io:*clock* (a manual clock the scheduler advances event by event) and randomness is
clempo.io:*entropy* (seeded), so a scenario with a seed replays exactly.

Scenarios are declared with DEFSCENARIO; RUN-SCENARIO returns a SIM-RESULT whose chain is
then checked by the same certificate verifier that follows mainnet (VERIFY-SIM-CHAIN).")
  (:export
   ;; network
   #:network #:make-network #:network-send #:network-broadcast #:network-next-time
   #:network-pop-due #:fault #:node-up-p #:link-up-p
   ;; scenarios
   #:defscenario #:find-scenario #:scenario-names #:run-scenario #:verify-sim-chain
   #:sim-result #:sim-result-chain #:sim-result-finalized-height #:sim-result-epochs
   #:sim-result-messages #:sim-result-dropped #:sim-result-ms #:sim-result-identity
   #:sim-result-identities #:sim-result-summary #:sim-result-proposers
   #:sim-result-ceremonies #:sim-result-heights-by-node))

(in-package #:clempo.sim)

;;; An event queue: a binary min-heap of (time seq . event), seq breaking ties in insertion
;;; order so equal-time events stay deterministic.

(defstruct (heap (:constructor make-heap ()))
  (items (make-array 64 :adjustable t :fill-pointer 0))
  (seq 0 :type (integer 0)))

(defun event< (a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b)) (< (second a) (second b)))))

(defun heap-push (heap time event)
  (let ((items (heap-items heap))
        (entry (list* time (incf (heap-seq heap)) event)))
    (vector-push-extend entry items)
    (loop with i = (1- (length items))
          while (plusp i)
          do (let ((parent (floor (1- i) 2)))
               (if (event< (aref items i) (aref items parent))
                   (progn (rotatef (aref items i) (aref items parent)) (setf i parent))
                   (return))))
    entry))

(defun heap-peek (heap)
  (let ((items (heap-items heap)))
    (and (plusp (length items)) (aref items 0))))

(defun heap-pop (heap)
  (let* ((items (heap-items heap))
         (top (aref items 0))
         (last (vector-pop items)))
    (when (plusp (length items))
      (setf (aref items 0) last)
      (loop with i = 0 and n = (length items)
            do (let* ((l (1+ (* 2 i))) (r (1+ l)) (m i))
                 (when (and (< l n) (event< (aref items l) (aref items m))) (setf m l))
                 (when (and (< r n) (event< (aref items r) (aref items m))) (setf m r))
                 (if (= m i)
                     (return)
                     (progn (rotatef (aref items i) (aref items m)) (setf i m))))))
    top))

;;; Faults, declared as data:
;;;   (:outage NODE :from MS :to MS)              the node neither sends nor receives
;;;   (:partition (GROUP...) :from MS :to MS)     links between different groups are cut
;;;   (:silent NODE)                              a crashed validator for the whole run

(defstruct (fault (:constructor %make-fault (kind node groups from to)))
  kind node groups from to)

(defun parse-fault (form)
  (destructuring-bind (kind arg &key (from 0) (to most-positive-fixnum)) form
    (ecase kind
      (:outage (%make-fault :outage arg nil from to))
      (:silent (%make-fault :outage arg nil 0 most-positive-fixnum))
      (:partition (%make-fault :partition nil arg from to)))))

(defun active-p (fault now)
  (<= (fault-from fault) now (1- (fault-to fault))))

(defun node-up-p (faults node now)
  (notany (lambda (f) (and (eq (fault-kind f) :outage) (eql (fault-node f) node) (active-p f now)))
          faults))

(defun crashed-p (faults node)
  "NODE never comes back (an outage without end)."
  (some (lambda (f) (and (eq (fault-kind f) :outage) (eql (fault-node f) node)
                         (= (fault-to f) most-positive-fixnum)))
        faults))

(defun group-of (groups node)
  (position-if (lambda (g) (member node g)) groups))

(defun link-up-p (faults from to now)
  (and (node-up-p faults from now)
       (node-up-p faults to now)
       (notany (lambda (f)
                 (and (eq (fault-kind f) :partition) (active-p f now)
                      (not (eql (group-of (fault-groups f) from) (group-of (fault-groups f) to)))))
               faults)))

;;; The network: messages are delivered after a latency drawn uniformly from LATENCY
;;; (min max) milliseconds, dropped with probability DROP (a ratio), and lost when the link
;;; is down when sent or when delivered.

(defstruct (network (:constructor %make-network))
  (size 0 :type (integer 0))
  (latency '(10 50))
  (drop 0)
  (faults '())
  (queue (make-heap))
  (sent 0 :type (integer 0))
  (dropped 0 :type (integer 0)))

(defun make-network (size &key (latency '(10 50)) (drop 0) faults)
  (%make-network :size size :latency latency :drop drop
                 :faults (mapcar #'parse-fault faults)))

(defun dropped-p (ratio)
  (and (plusp ratio)
       (< (io:random-below (denominator ratio)) (numerator ratio))))

(defun network-send (net from to message &key (reliable nil))
  "Queue MESSAGE from FROM to TO. RELIABLE messages (block backfill) still wait for the
link and the latency but are never dropped at random."
  (let ((now (io:now-ms)))
    (incf (network-sent net))
    (if (or (not (link-up-p (network-faults net) from to now))
            (and (not reliable) (dropped-p (network-drop net))))
        (incf (network-dropped net))
        (destructuring-bind (lo hi) (network-latency net)
          (heap-push (network-queue net) (+ now lo (io:random-below (1+ (- hi lo))))
                     (list :deliver from to message))))))

(defun network-broadcast (net from message)
  (dotimes (to (network-size net))
    (unless (= to from) (network-send net from to message))))

(defun network-schedule (net time event)
  (heap-push (network-queue net) time event))

(defun network-next-time (net)
  (let ((top (heap-peek (network-queue net)))) (and top (first top))))

(defun network-pop-due (net now)
  "Events due at or before NOW, in order. A delivery whose link went down in flight is lost."
  (loop while (let ((top (heap-peek (network-queue net)))) (and top (<= (first top) now)))
        for (nil nil . event) = (heap-pop (network-queue net))
        when (or (not (eq (first event) :deliver))
                 (destructuring-bind (from to msg) (rest event)
                   (declare (ignore msg))
                   (or (link-up-p (network-faults net) from to now)
                       (progn (incf (network-dropped net)) nil))))
          collect event))
