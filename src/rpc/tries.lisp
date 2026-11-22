(defpackage #:clempo.rpc.tries
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:chain #:clempo.rpc.chain) (#:store #:clempo.store) (#:history #:clempo.history)
                    (#:render #:clempo.rpc.render) (#:h #:clempo.headers) (#:trie #:clempo.trie)
                    (#:io #:clempo.io))
  (:documentation "States with tries for an archive backend: the methods that need Merkle tries
of a block's state (proofs and storage roots at the head, debug_intermediateRoots,
debug_executionWitness, debug_stateRootWithUpdates) where the backend otherwise keeps only the
flat clempo.history.

A trie state is a clempo.store state database loaded from a checkpoint (a datadir's
checkpoint-N.state, or the job-N.state checkpoints a history build leaves beside its segments)
and moved to the wanted block by rewriting every account and slot the history says changed in
between (CLEMPO.HISTORY:HISTORY-MAP-CHANGES), in either direction. Every move ends with the
state root checked against the block's header, so a state is never served unverified.

Two kinds of state are kept per backend: the head's, loaded once and only read (concurrent
proof readers), and a bounded pool of cursors. CALL-WITH-TRIE-STATE lends a call the pooled
state cheapest to move to its block, or a fresh one loaded from a checkpoint while the pool has
room (CHOOSE-CURSOR), moves it there, and the call has it to itself until it returns. Calls
near a checkpoint (the head's, a datadir's) run in parallel on states of their own; calls far
from every checkpoint share the states there are, as a chain of moves; calls beyond
*TRIE-CURSORS* or the heap's room (*HEAP-SHARE*) wait for a state to come back.")
  (:export #:head-trie-state #:with-trie-state #:call-with-trie-state #:trie-bases
           #:*trie-state-dirs* #:*reload-distance* #:*trie-cursors* #:*grow-distance* #:*heap-share*
           #:trie-updates #:branch-compact #:nibbles-hex #:octets-nibbles))

(in-package #:clempo.rpc.tries)

(defparameter *trie-state-dirs* '()
  "Extra directories (or checkpoint files) searched for state checkpoints, besides the history
directory; $CLEMPO_TRIE_STATE adds a colon-separated list.")

(defparameter *reload-distance* 2000000
  "A checkpoint this many blocks closer to the target than the cursor is loaded instead of
moving the cursor (a load costs about as much as moving across this many blocks).")

(defparameter *trie-cursors* 4
  "The most cursor states a backend's pool holds; $CLEMPO_TRIE_CURSORS overrides it when the
pool is made. Memory bounds it too (*HEAP-SHARE*).")

(defparameter *grow-distance* 200000
  "The pool adds a state beyond its first only for a call within this many blocks of a
checkpoint (MOVE-COST); a call further away shares the states the pool has.")

(defparameter *heap-share* 1/4
  "The pool takes in another state only while the heap holds less than this share of its
dynamic space (its first state is always let in), read when the pool is made. A mainnet state
near block 50,000,000 holds about 3 GB once its large storage tries are built (1.3 GB as
loaded), and a full collection needs room to copy everything that lives.")

(defun cursor-limit ()
  (let* ((env (io:getenv "CLEMPO_TRIE_CURSORS"))
         (n (and env (plusp (length env)) (parse-integer env :junk-allowed t))))
    (max 1 (or n *trie-cursors*))))

(defstruct (tries (:constructor make-tries ()))
  (head nil)                                          ; statedb at the backend head, read-only
  (head-lock (sb-thread:make-mutex :name "head trie state"))
  ;; The cursor pool: idle states, most recently returned first, and a ticket (n) per state
  ;; lent to a call at block N (being loaded, moved or used); together at most LIMIT, and
  ;; more than one only while the heap is below HEAP-SHARE of its space.
  (idle '() :type list)
  (lent '() :type list)
  (limit (cursor-limit) :type (integer 1))
  (heap-share *heap-share* :type (real 0 1))
  (lock (sb-thread:make-mutex :name "trie state cursors"))
  (returned (sb-thread:make-waitqueue :name "trie state cursor returned")))

(defvar *tries* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *tries-lock* (sb-thread:make-mutex :name "trie states"))

(defun backend-tries (backend)
  (or (gethash backend *tries*)
      (clempo.lock:with-lock (*tries-lock*)
        (or (gethash backend *tries*) (setf (gethash backend *tries*) (make-tries))))))

(defun unavailable (n why)
  (clempo.rpc:internal-error (format nil "state tries at block ~D are not available: ~A" n why)))

;;; Memory. Loading and moving states turns old generations into garbage faster than SBCL's
;;; own schedule collects them: with several states in flight the heap filled up (40 GB,
;;; four states) before the oldest generations were collected. So a load or move first
;;; collects the whole heap when it is over half full.

(defvar *collect-lock* (sb-thread:make-mutex :name "trie state collection"))

(defun heap-below-p (share)
  (< (sb-kernel:dynamic-usage) (* share (sb-ext:dynamic-space-size))))

(defun collect-if-half-full ()
  "Collect the whole heap when it is over half full, one thread at a time (a thread that
waited for another's collection looks again first)."
  (unless (heap-below-p 1/2)
    (clempo.lock:with-lock (*collect-lock*)
      (unless (heap-below-p 1/2) (sb-ext:gc :full t)))))

;;; Checkpoints

(defun checkpoint-number (path)
  "The block number a clempo.store checkpoint file holds the state after, or NIL."
  (with-open-file (in path :element-type 'octet)
    (let ((head (make-octets 20)))
      (when (and (= (read-sequence head in) 20)
                 (octets= (subseq head 0 8) (string->octets "CLEMPOCK")))
        (octets->integer head :start 12 :end 20)))))

(defun checkpoint-files (dir)
  (let ((p (pathname dir)))
    (if (pathname-name p)
        (list p)
        (append (directory (merge-pathnames "checkpoint-*.state" p))
                (directory (merge-pathnames "job-*.state" p))))))

(defun kv-datadir-p (dir)
  "DIR holds a clempo.store KV datadir (its MANIFEST is a KV manifest)."
  (let ((manifest (probe-file (merge-pathnames "MANIFEST" dir))))
    (and manifest (eql 0 (search "clempo-kv " (io:read-file-string manifest))))))

(defvar *datadir-heights* (make-hash-table :test 'equal :synchronized t)
  "datadir -> (MANIFEST write date . flushed height), so a base list costs no snapshot open
until the datadir flushes again.")

(defun datadir-height (dir)
  (let* ((date (file-write-date (merge-pathnames "MANIFEST" dir)))
         (cell (gethash dir *datadir-heights*)))
    (if (and cell (eql (car cell) date))
        (cdr cell)
        (let ((db (store:open-datadir-snapshot dir)))
          (when db
            (cdr (setf (gethash dir *datadir-heights*) (cons date (store:db-number db)))))))))

(defun trie-bases (backend)
  "The states a trie state can start from: ((number . base) ...), BASE a checkpoint path or
(:datadir . dir) for a clempo.store datadir's flushed state."
  (let ((dirs (append (let ((env (io:getenv "CLEMPO_TRIE_STATE")))
                        (when (and env (plusp (length env)))
                          (loop with start = 0
                                for colon = (position #\: env :start start)
                                collect (subseq env start colon)
                                while colon do (setf start (1+ colon)))))
                      *trie-state-dirs*
                      (let ((h (chain:backend-history backend)))
                        (when h (list (namestring (history:history-dir h))))))))
    (loop for dir in dirs
          for slashed = (if (and (plusp (length dir))
                                 (char/= (char dir (1- (length dir))) #\/)
                                 (not (search ".state" dir)))
                            (concatenate 'string dir "/")
                            dir)
          append (if (and (not (search ".state" dir)) (kv-datadir-p slashed))
                     (let ((n (handler-case (datadir-height slashed) (error () nil))))
                       (when (and n (<= n (chain:backend-head backend)))
                         (list (cons n (cons :datadir slashed)))))
                     (loop for path in (checkpoint-files slashed)
                           for n = (handler-case (checkpoint-number path) (error () nil))
                           when (and n (<= n (chain:backend-head backend))) collect (cons n path))))))

;;; Moving a state database between blocks

(defun header-root (n)
  (let ((b (render:chain-block n)))
    (unless b (unavailable n "the block is not in the archive"))
    (values (h:ethereum-header-state-root (render:cblock-inner b)) (render:cblock-hash b))))

(defun ensure-code (db h code-hash)
  (unless (or (octets= code-hash trie:+empty-code-hash+)
              (handler-case (store:db-code db code-hash) (error () nil)))
    (let ((code (history:history-code-by-hash h code-hash)))
      (if code
          (store:db-put-code db code)
          (unavailable (store:db-number db) (format nil "no code for hash ~A" (octets->hex code-hash)))))))

(defun note-recent-hashes (db n)
  "BLOCKHASH answers of a state after block N: the hashes of the 256 blocks up to N."
  (loop for k from (max 0 (- n 255)) to n
        do (store:db-note-block-hash db k (render:cblock-hash (render:chain-block k)))))

(defparameter *gap-penalty* 20000000
  "MOVE-COST of crossing a history gap seed, where every key of the state must be re-read.")

(defun crosses-gap-p (h from to)
  (let ((lo (min from to)) (hi (max from to)))
    (some (lambda (s) (and (< lo s) (<= s hi))) (history:history-gap-seeds h))))

(defun move-cost (h from to)
  "A rough cost of moving a state from block FROM to block TO, in blocks."
  (+ (abs (- from to)) (if (crosses-gap-p h from to) *gap-penalty* 0)))

(defun move-state (db n h)
  "Rewrite DB (a committed state database with tries) into the state after block N from the
history H, and check its root against block N's header. Across a gap seed (a block whose
deletions the history does not list) every key DB holds is re-read as well. Every 65,536
writes it collects the heap if it is over half full (COLLECT-IF-HALF-FULL)."
  (let ((from (store:db-number db)))
    (unless (= from n)
      (collect-if-half-full)
      (let ((view (or (history:history-state h n) (unavailable n "the history does not cover it")))
            (accounts (make-hash-table))
            (slots (make-hash-table))
            (writes 0))
        (declare (type fixnum writes))
        (flet ((note (address slot)
                 (if slot
                     (setf (gethash slot (or (gethash address slots)
                                             (setf (gethash address slots) (make-hash-table))))
                           t)
                     (setf (gethash address accounts) t))))
          (when (history:history-map-changes h from n #'note)
            (unavailable n (format nil "storage wipes between blocks ~D and ~D" from n)))
          (when (crosses-gap-p h from n)
            (store:map-accounts (lambda (address a)
                                  (note address nil)
                                  (store:map-account-storage (lambda (slot v) (declare (ignore v)) (note address slot)) a))
                                db)))
        (multiple-value-bind (root hash) (header-root n)
          (store:db-begin-block db)
          (let ((done nil))
            (flet ((wrote ()
                     (when (zerop (logand (incf writes) #xffff)) (collect-if-half-full))))
              (unwind-protect
                   (progn
                     (maphash (lambda (address flag)
                                (declare (ignore flag))
                                (wrote)
                                (multiple-value-bind (found nonce balance code-hash) (history:history-account view address)
                                  (cond (found (ensure-code db h code-hash)
                                               (store:db-set-account db address nonce balance code-hash))
                                        (t (store:db-delete-account db address)))))
                              accounts)
                     (maphash (lambda (address table)
                                (when (store:db-account db address)
                                  (maphash (lambda (slot flag)
                                             (declare (ignore flag))
                                             (wrote)
                                             (store:db-set-storage db address slot (history:history-storage view address slot)))
                                           table)))
                              slots)
                     (let ((actual (store:db-state-root db)))
                       (unless (octets= actual root)
                         (unavailable n (format nil "moving the state from block ~D gives root ~A, the header says ~A"
                                                from (octets->hex actual) (octets->hex root)))))
                     (setf done t))
                (unless done (store:db-revert-block db)))))
          (store:db-commit-block db n hash))))
    (note-recent-hashes db n)
    db))

(defun load-state (backend n &optional (bases (trie-bases backend)))
  "A fresh state database after block N: the checkpoint cheapest to move to N, moved there."
  (let* ((h (chain:backend-history backend))
         (base (first (sort (copy-list bases) #'<
                            :key (lambda (b) (if h (move-cost h (car b) n) (abs (- (car b) n))))))))
    (unless base (unavailable n "no state checkpoint (set CLEMPO_TRIE_STATE to a datadir)"))
    (collect-if-half-full)
    (let ((db (if (consp (cdr base))
                  (or (store:open-datadir-snapshot (cddr base))
                      (unavailable n (format nil "datadir ~A holds no state" (cddr base))))
                  (store:read-checkpoint (cdr base) :check-root t))))
      ;; a datadir may have flushed past its recorded height since: move from where it is
      (setf base (cons (store:db-number db) (cdr base)))
      (when (/= (car base) n)
        (unless h (unavailable n "no history to move a checkpoint with"))
        (move-state db n h))
      (unless (octets= (store:db-state-root db) (header-root n))
        (unavailable n (format nil "checkpoint ~A does not match the header" (cdr base))))
      db)))

;;; The head state and the cursor pool

(defun head-trie-state (&optional (backend chain:*backend*))
  "The state database with tries after the backend's head block, loaded on first use and
then only read (proofs over clean tries are safe for concurrent readers)."
  (let ((tries (backend-tries backend))
        (head (chain:backend-head backend)))
    (clempo.lock:with-lock ((tries-head-lock tries))
      (let ((db (tries-head tries)))
        (if (and db (= (store:db-number db) head))
            db
            (setf (tries-head tries) (load-state backend head)))))))

(defun cursor-cost (h from n)
  "MOVE-COST of a state at block FROM moved to N; NIL when it cannot move (no history)."
  (cond ((= from n) 0)
        (h (move-cost h from n))))

(defun choose-cursor (tries n h bases)
  "Under the pool lock, how a call at block N gets its state: (values :move db) to move the
idle state DB, (values :load victim) to load a fresh one from a checkpoint (VICTIM, NIL when
the pool grows, is the least recently returned idle state, dropped for it), or NIL to wait for
a lent state.
The pool grows below its LIMIT while the heap is below *HEAP-SHARE*, and past its first state
only for a call within *GROW-DISTANCE* of a checkpoint, where a new state costs little more
than its load. Calls far from every checkpoint share the states there are, as they shared the
one cursor before: a state moved from one such call to the next is far cheaper than states
loaded for each (the chain of a client walking the blocks). Choices are costed in blocks
moved: an idle state from where it is; a load from its checkpoint plus *RELOAD-DISTANCE*;
waiting from the block a lent state was lent for plus twice that."
  (let* ((d *reload-distance*)
         (base-cost (when bases
                      (loop for b in bases minimize (if h (move-cost h (car b) n) (abs (- (car b) n))))))
         (load-cost (when base-cost (+ d base-cost)))
         (idle nil) (idle-cost nil) (wait-cost nil)
         (held (+ (length (tries-idle tries)) (length (tries-lent tries))))
         (grow (and (< held (tries-limit tries))
                    (or (zerop held)
                        (and base-cost (<= base-cost *grow-distance*)
                             (heap-below-p (tries-heap-share tries)))))))
    (dolist (db (tries-idle tries))
      (let ((c (cursor-cost h (store:db-number db) n)))
        (when (and c (or (null idle-cost) (< c idle-cost))) (setf idle db idle-cost c))))
    (dolist (ticket (tries-lent tries))
      (let ((c (cursor-cost h (car ticket) n)))
        (when (and c (or (null wait-cost) (< (+ c d d) wait-cost))) (setf wait-cost (+ c d d)))))
    (flet ((no-worse (cost than) (or (null than) (and cost (<= cost than)))))
      (cond ((and idle (no-worse idle-cost load-cost) (no-worse idle-cost wait-cost)) (values :move idle))
            ((and (or grow (tries-idle tries)) (no-worse load-cost wait-cost))
             (values :load (unless grow (car (last (tries-idle tries))))))
            (t nil)))))

(defun lend-cursor (tries ticket h bases)
  "Wait until CHOOSE-CURSOR has a state for TICKET's call ((n) for a call at block N), then
count TICKET lent and return the choice."
  (clempo.lock:with-lock ((tries-lock tries))
    (loop
      (multiple-value-bind (plan db) (choose-cursor tries (car ticket) h bases)
        (when plan
          (when db (setf (tries-idle tries) (delete db (tries-idle tries) :count 1)))
          (push ticket (tries-lent tries))
          (return (values plan db))))
      (clempo.lock:condition-wait (tries-returned tries) (tries-lock tries)))))

(defun return-cursor (tries ticket db)
  "End TICKET's loan; DB, when it is whole (no block left open), becomes the newest idle state."
  (clempo.lock:with-lock ((tries-lock tries))
    (setf (tries-lent tries) (delete ticket (tries-lent tries) :count 1 :test #'eq))
    (when (and db (not (store:db-block-open-p db)))
      (push db (tries-idle tries)))
    (sb-thread:condition-broadcast (tries-returned tries))))

(defun call-with-trie-state (n fn &optional (backend chain:*backend*))
  "Call FN with a state database after block N lent from the backend's cursor pool
\(CHOOSE-CURSOR), moved there and checked against the header; no other call uses it until FN
returns. FN may execute blocks on it (store:with-speculative-blocks) but must leave it as it
found it."
  (let* ((tries (backend-tries backend))
         (h (chain:backend-history backend))
         (bases (trie-bases backend))
         (ticket (list n))
         (db nil))
    (unwind-protect
         (multiple-value-bind (plan cursor) (lend-cursor tries ticket h bases)
           (setf db (if (eq plan :move)
                        (handler-case (move-state cursor n h)
                          ;; a failed move reverted its block: the state is whole where it
                          ;; was, and stays pooled unless a checkpoint reaches N instead
                          (clempo.rpc:rpc-error (e)
                            (setf db cursor)
                            (if bases (load-state backend n bases) (error e))))
                        (load-state backend n bases)))
           (funcall fn db))
      (return-cursor tries ticket db))))

(defmacro with-trie-state ((db n) &body body)
  "Run BODY with DB bound to a pooled state after block N (see CALL-WITH-TRIE-STATE)."
  `(call-with-trie-state ,n (lambda (,db) ,@body)))

;;; reth's TrieUpdates (debug_stateRootWithUpdates): the branch nodes reth's trie walker and
;;; hash builder rebuild when they compute a root over a changed key set, as reth's trie tables
;;; store them (alloy-trie BranchNodeCompact). Paths are nibble lists in hashed-key space.
;;;
;;; reth's tables never hold the root node, so the walker starts below it: every stored node
;;; nearest the root is consumed (rebuilt from its children's stored hashes); inside a consumed
;;; node a child is skipped when its hash is stored and no changed key lies under it, followed
;;; into when it is stored itself, and otherwise rebuilt from its leaves. A branch is stored
;;; when one of its children is a stored-by-hash branch or a stored node (alloy HashBuilder's
;;; store_in_db_trie); its hash mask marks children that are branches referenced by hash, its
;;; tree mask children stored themselves (through an extension, the branch below it).

(defun nibbles-hex (nibbles)
  "alloy Nibbles as reth's TrieUpdates serialize them: packed two to a byte, an odd one padded."
  (with-output-to-string (s)
    (loop for (a b) on nibbles by #'cddr do (format s "~(~X~X~)" a (or b 0)))))

(defun octets-nibbles (octets)
  (loop for b across octets collect (ash b -4) collect (logand b 15)))

(defun extension-target (trie path)
  "The path of the branch an extension starting at PATH leads to."
  (append path (nth-value 1 (trie:trie-node-at trie path))))

(defun branch-stored-p (trie path)
  (multiple-value-bind (kind children) (trie:trie-node-at trie path)
    (and (eq kind :branch)
         (or (some (lambda (c) (and c (eq (car c) :branch) (= (length (cdr c)) 32))) children)
             (loop for i below 16
                   for c = (svref children i)
                   thereis (and c (case (car c)
                                    (:branch (branch-stored-p trie (append path (list i))))
                                    (:extension (branch-stored-p trie (extension-target trie (append path (list i))))))))))))

(defun branch-compact (trie path)
  "(values state-mask tree-mask hash-mask hashes) of the branch at PATH, or NIL."
  (multiple-value-bind (kind children) (trie:trie-node-at trie path)
    (when (eq kind :branch)
      (let ((state 0) (tree 0) (hash 0) (hashes '()))
        (dotimes (i 16)
          (let ((c (svref children i)) (cp (append path (list i))))
            (when c
              (setf state (logior state (ash 1 i)))
              (case (car c)
                (:branch (when (= (length (cdr c)) 32)
                           (setf hash (logior hash (ash 1 i)))
                           (push (cdr c) hashes))
                         (when (branch-stored-p trie cp) (setf tree (logior tree (ash 1 i)))))
                (:extension (when (branch-stored-p trie (extension-target trie cp))
                              (setf tree (logior tree (ash 1 i)))))))))
        (values state tree hash (nreverse hashes))))))

(defun stored-node-under (trie path)
  "The stored node a walker consuming the child at PATH reaches: PATH's branch, or the branch
below an extension at PATH, when stored; else NIL."
  (case (trie:trie-node-at trie path)
    (:branch (when (branch-stored-p trie path) path))
    (:extension (let ((target (extension-target trie path)))
                  (when (branch-stored-p trie target) target)))))

(defun trie-updates (pre post changes)
  "Emulate reth's walker over the trie PRE (as stored) computing the root of POST (both
fully hashed) for the changed keys CHANGES (64-nibble lists). Returns (values nodes removed
leaves): NODES an alist (path state tree hash hashes) of the rebuilt stored branches of POST,
REMOVED the paths of PRE's stored branches the walker consumed, LEAVES the full keys of the
leaves the hash builder rebuilt."
  (let ((nodes '()) (removed '()) (leaves '()))
    (labels ((on-path-p (path)
               (some (lambda (key) (let ((m (mismatch path key))) (or (null m) (= m (length path))))) changes))
             (emit (path)
               (when (and path (branch-stored-p post path))
                 (push (cons path (multiple-value-list (branch-compact post path))) nodes)))
             (rebuild (path)
               (multiple-value-bind (kind more) (trie:trie-node-at post path)
                 (case kind
                   (:branch (emit path)
                    (loop for i below 16 for c across more
                          when c do (rebuild (append path (list i)))))
                   (:extension (rebuild (append path more)))
                   (:leaf (push (append path more) leaves)))))
             (consume (path)
               (push path removed)
               (emit path)
               (multiple-value-bind (state tree hash) (branch-compact pre path)
                 (declare (ignore state))
                 (multiple-value-bind (kind children) (trie:trie-node-at post path)
                   (when (eq kind :branch)
                     (loop for i below 16
                           for c across children
                           for cp = (append path (list i))
                           when c
                             do (cond ((and hash (logbitp i hash) (not (on-path-p cp))))
                                      ((and tree (logbitp i tree) (stored-node-under pre cp))
                                       (consume (stored-node-under pre cp)))
                                      (t (rebuild cp))))))))
             (top (path)
               ;; the unstored region around the root: stored nodes are consumed
               (multiple-value-bind (kind more) (trie:trie-node-at post path)
                 (case kind
                   (:branch (if (and path (branch-stored-p pre path))
                                (consume path)
                                (loop for i below 16 for c across more
                                      when c do (top (append path (list i))))))
                   (:extension (top (append path more)))
                   (:leaf (push (append path more) leaves))))))
      (top '())
      (values (nreverse nodes) (nreverse removed) (nreverse leaves)))))
