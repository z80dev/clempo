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

Two states are kept per backend: the head's, loaded once and only read (concurrent proof
readers), and a cursor that WITH-TRIE-STATE moves from block to block for one caller at a time.")
  (:export #:head-trie-state #:with-trie-state #:call-with-trie-state #:trie-bases
           #:*trie-state-dirs* #:*reload-distance*
           #:trie-updates #:branch-compact #:nibbles-hex #:octets-nibbles))

(in-package #:clempo.rpc.tries)

(defparameter *trie-state-dirs* '()
  "Extra directories (or checkpoint files) searched for state checkpoints, besides the history
directory; $CLEMPO_TRIE_STATE adds a colon-separated list.")

(defparameter *reload-distance* 2000000
  "A checkpoint this many blocks closer to the target than the cursor is loaded instead of
moving the cursor (a load costs about as much as moving across this many blocks).")

(defstruct (tries (:constructor make-tries ()))
  (head nil)                                          ; statedb at the backend head, read-only
  (head-lock (sb-thread:make-mutex :name "head trie state"))
  (cursor nil)                                        ; statedb moved by WITH-TRIE-STATE
  (cursor-lock (sb-thread:make-mutex :name "trie state cursor")))

(defvar *tries* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *tries-lock* (sb-thread:make-mutex :name "trie states"))

(defun backend-tries (backend)
  (or (gethash backend *tries*)
      (sb-thread:with-mutex (*tries-lock*)
        (or (gethash backend *tries*) (setf (gethash backend *tries*) (make-tries))))))

(defun unavailable (n why)
  (clempo.rpc:internal-error (format nil "state tries at block ~D are not available: ~A" n why)))

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

(defun trie-bases (backend)
  "The checkpoints a trie state can start from: ((number . path) ...)."
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
          append (loop for path in (checkpoint-files (if (and (plusp (length dir))
                                                             (char/= (char dir (1- (length dir))) #\/)
                                                             (not (search ".state" dir)))
                                                        (concatenate 'string dir "/")
                                                        dir))
                       for n = (handler-case (checkpoint-number path) (error () nil))
                       when (and n (<= n (chain:backend-head backend))) collect (cons n path)))))

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
deletions the history does not list) every key DB holds is re-read as well."
  (let ((from (store:db-number db)))
    (unless (= from n)
      (let ((view (or (history:history-state h n) (unavailable n "the history does not cover it")))
            (accounts (make-hash-table))
            (slots (make-hash-table)))
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
            (unwind-protect
                 (progn
                   (maphash (lambda (address flag)
                              (declare (ignore flag))
                              (multiple-value-bind (found nonce balance code-hash) (history:history-account view address)
                                (cond (found (ensure-code db h code-hash)
                                             (store:db-set-account db address nonce balance code-hash))
                                      (t (store:db-delete-account db address)))))
                            accounts)
                   (maphash (lambda (address table)
                              (when (store:db-account db address)
                                (maphash (lambda (slot flag)
                                           (declare (ignore flag))
                                           (store:db-set-storage db address slot (history:history-storage view address slot)))
                                         table)))
                            slots)
                   (let ((actual (store:db-state-root db)))
                     (unless (octets= actual root)
                       (unavailable n (format nil "moving the state from block ~D gives root ~A, the header says ~A"
                                              from (octets->hex actual) (octets->hex root)))))
                   (setf done t))
              (unless done (store:db-revert-block db))))
          (store:db-commit-block db n hash))))
    (note-recent-hashes db n)
    db))

(defun load-state (backend n &optional (bases (trie-bases backend)))
  "A fresh state database after block N: the checkpoint cheapest to move to N, moved there."
  (let* ((h (chain:backend-history backend))
         (base (first (sort (copy-list bases) #'<
                            :key (lambda (b) (if h (move-cost h (car b) n) (abs (- (car b) n))))))))
    (unless base (unavailable n "no state checkpoint (set CLEMPO_TRIE_STATE to a datadir)"))
    (let ((db (store:read-checkpoint (cdr base) :check-root t)))
      (when (/= (car base) n)
        (unless h (unavailable n "no history to move a checkpoint with"))
        (move-state db n h))
      (unless (octets= (store:db-state-root db) (header-root n))
        (unavailable n (format nil "checkpoint ~A does not match the header" (cdr base))))
      db)))

;;; The two states

(defun head-trie-state (&optional (backend chain:*backend*))
  "The state database with tries after the backend's head block, loaded on first use and
then only read (proofs over clean tries are safe for concurrent readers)."
  (let ((tries (backend-tries backend))
        (head (chain:backend-head backend)))
    (sb-thread:with-mutex ((tries-head-lock tries))
      (let ((db (tries-head tries)))
        (if (and db (= (store:db-number db) head))
            db
            (setf (tries-head tries) (load-state backend head)))))))

(defun call-with-trie-state (n fn &optional (backend chain:*backend*))
  "Call FN with the backend's cursor state database moved to the state after block N, holding
it exclusively. FN may execute blocks on it (store:with-speculative-blocks) but must leave it
as it found it."
  (let ((tries (backend-tries backend)))
    (sb-thread:with-mutex ((tries-cursor-lock tries))
      (let* ((db (tries-cursor tries))
             (h (chain:backend-history backend))
             (bases (trie-bases backend))
             (reload (if h (loop for b in bases minimize (move-cost h (car b) n)) 0)))
        (setf db (if (and db h (or (null bases)
                                   (<= (move-cost h (store:db-number db) n) (+ reload *reload-distance*))))
                     (handler-case (move-state db n h)
                       ;; a failed move reverted its block: the cursor is still at its old block
                       (clempo.rpc:rpc-error (e)
                         (if bases (progn (setf (tries-cursor tries) nil) (load-state backend n bases)) (error e))))
                     (progn (setf (tries-cursor tries) nil)
                            (load-state backend n bases))))
        (setf (tries-cursor tries) db)
        (funcall fn db)))))

(defmacro with-trie-state ((db n) &body body)
  "Run BODY with DB bound to the cursor state after block N (see CALL-WITH-TRIE-STATE)."
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
