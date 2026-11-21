(in-package #:clempo.trie)

;;; State transition witnesses: the trie nodes reth's TrieWitness (crates/trie/trie/src/
;;; witness.rs) records for debug_executionWitness, computed over the parent state's tries.
;;;
;;; reth's node set, exactly:
;;;  1. A multiproof of every target: each touched account (hashed address), and each touched
;;;     slot of an account whose touched-slot map is not empty. A proof (proof_v2) retains every
;;;     node whose path (where the node hangs below its parent) is a prefix of a target key, the
;;;     root always (the empty root as the one byte 0x80). An extension and its branch form one
;;;     proof node; recording it records both encodings. Inline nodes are recorded too.
;;;  2. The block's changes applied to a sparse trie revealed from those proofs (storage tries
;;;     first, then the account trie), in the mode's order: legacy applies every removal (slot
;;;     value zero; account empty with an empty storage root) before the upserts, canonical the
;;;     upserts first. A removal that would leave a branch with one blinded child asks for the
;;;     proof of that sibling: the key is the sibling's path padded with zero nibbles and the
;;;     proof keeps only nodes below the branch (so the sibling and every node hanging on its
;;;     all-zero path). The sparse trie keeps the nodes at path length 2 in subtries; removals
;;;     that number at least a subtrie's revealed leaves, when its parent branch has exactly one
;;;     other child and that child is blinded, ask for that sibling before touching the
;;;     subtrie (reth's check_subtrie_collapse_needs_proof), whether or not the keys exist. An
;;;     update reaching a blinded node asks for the proof of its own key below the deepest
;;;     revealed branch. Every proof's nodes are recorded and revealed, and the pass repeats.
;;;  3. Legacy mode also records the storage root node of every account without a touched
;;;     slot. Canonical mode drops the empty node 0x80 and sorts the nodes.
;;; The nodes all come from the parent tries: a proof is computed on the database, never on
;;; the sparse trie.

(defun key-nibbles (key)
  (let ((v (make-array (* 2 (length key)) :element-type '(unsigned-byte 4))))
    (dotimes (i (length key) v)
      (setf (aref v (* 2 i)) (ash (aref key i) -4)
            (aref v (1+ (* 2 i))) (logand (aref key i) 15)))))

(defun path-nibbles (path)
  "The nibbles of a hex-prefix PATH, as a vector."
  (let* ((start (path-start path)) (len (path-length path))
         (v (make-array len :element-type '(unsigned-byte 4))))
    (dotimes (i len v) (setf (aref v i) (nibble path (+ start i))))))

(defun witness-node-bytes (node store)
  (let ((ref (node-ref node)))
    (if (and ref (< (length ref) 32))
        ref
        (multiple-value-bind (bytes end) (encode-node node store)
          (if (= end (length bytes)) bytes (subseq bytes 0 end))))))

;;; Recording proofs from the parent trie

(defstruct (witness-set (:constructor make-witness-set ()))
  (seen (make-hash-table :test 'equalp))
  (nodes '()))

(defun witness-add (w bytes)
  (unless (gethash bytes (witness-set-seen w))
    (setf (gethash bytes (witness-set-seen w)) t)
    (push bytes (witness-set-nodes w))))

(defun record-node (w node store)
  "Record an opened node; an extension records its branch as well (reth's BranchNodeV2)."
  (witness-add w (witness-node-bytes node store))
  (when (and (short-node-p node) (not (leaf-path-p (short-node-path node))))
    (witness-add w (witness-node-bytes (open-node (short-node-item node) store) store))))

(defun record-proof (w trie key minlen)
  "Record the proof_v2 nodes of KEY (nibbles) in TRIE (NIL: empty) hanging deeper than MINLEN
\(-1: from the root, the empty root included)."
  (let* ((store (and trie (trie-store trie)))
         (node (and trie (trie-root-node trie)))
         (h 0))
    (unless node
      (when (minusp minlen) (witness-add w (octets-of #x80)))
      (return-from record-proof))
    (loop
      (let ((n (open-node node store)))
        (when (> h minlen) (record-node w n store))
        (etypecase n
          (short-node
           (let ((path (short-node-path n)))
             (when (leaf-path-p path) (return))
             (let ((len (path-length path)) (start (path-start path)))
               (unless (and (<= (+ h len) (length key))
                            (loop for i below len always (= (nibble path (+ start i)) (aref key (+ h i)))))
                 (return))
               (incf h len)
               (let ((child (svref (branch-node-children (open-node (short-node-item n) store))
                                   (aref key h))))
                 (unless child (return))
                 (setf node child h (1+ h))))))
          (branch-node
           (let ((child (svref (branch-node-children n) (aref key h))))
             (unless child (return))
             (setf node child h (1+ h)))))))))

;;; The sparse trie: :blind (a parent-trie node not yet revealed), :leaf, :ext (its CHILD is
;;; the :branch), :branch (CHILDREN a 16-vector), :empty.

(defstruct (sn (:constructor make-sn (kind &key pre key children child)))
  kind pre key children child)

(defun reveal-sn (pre store)
  (let ((n (open-node pre store)))
    (flet ((branch (b)
             (make-sn :branch :children (map 'simple-vector
                                             (lambda (c) (and c (make-sn :blind :pre c)))
                                             (branch-node-children b)))))
      (etypecase n
        (branch-node (branch n))
        (short-node
         (let ((path (short-node-path n)))
           (if (leaf-path-p path)
               (make-sn :leaf :key (path-nibbles path))
               (make-sn :ext :key (path-nibbles path)
                             :child (branch (open-node (short-node-item n) store))))))))))

(defstruct (strie (:constructor %make-strie (root store trie)))
  root store trie)

(defun make-strie (trie)
  (%make-strie (if (and trie (trie-root-node trie)) (make-sn :blind :pre (trie-root-node trie)) (make-sn :empty))
               (and trie (trie-store trie)) trie))

(defun sn-branch (sn)
  (case (sn-kind sn) (:branch sn) (:ext (sn-child sn))))

(defun sn-logical (sn h)
  (if (eq (sn-kind sn) :ext) (+ h (length (sn-key sn))) h))

(defun child-count (branch)
  (count-if-not #'null (sn-children branch)))

(defun reveal-proof (st key minlen)
  "Reveal the blinded nodes of ST a proof of KEY below MINLEN retains."
  (let ((sn (strie-root st)) (set (lambda (new) (setf (strie-root st) new))) (h 0))
    (loop
      (case (sn-kind sn)
        (:blind
         (unless (> h minlen) (return))
         (let ((new (reveal-sn (sn-pre sn) (strie-store st))))
           (funcall set new)
           (setf sn new)))
        ((:empty :leaf) (return))
        (t
         (when (eq (sn-kind sn) :ext)
           (let ((k (sn-key sn)))
             (unless (and (<= (+ h (length k)) (length key))
                          (every #'= k (subseq key h (+ h (length k)))))
               (return))))
         (let* ((l (sn-logical sn h)) (b (sn-branch sn)) (nib (aref key l))
                (child (svref (sn-children b) nib)))
           (unless child (return))
           (setf set (lambda (new) (setf (svref (sn-children b) nib) new))
                 sn child h (1+ l))))))))

;;; Seeking: a frame is (sn h setter); the head frame is first.

(defun seek (st key &key from upper)
  "Walk ST along KEY from FROM (frames to start below; the root when NIL). Returns the result
\(:empty :blinded :leaf :diverged :nochild :subtrie) and the frames, head first. UPPER stops at
a revealed node hanging at path length 2 (a subtrie)."
  (let ((frames from))
    (multiple-value-bind (sn h set)
        (if from
            (values-list (first from))
            (values (strie-root st) 0 (lambda (new) (setf (strie-root st) new))))
      (unless from (push (list sn h set) frames))
      (loop
        (when (and upper (= h 2) (not (eq (sn-kind sn) :blind)) (cdr frames))
          (return (values :subtrie frames)))
        (ecase (sn-kind sn)
          (:empty (return (values :empty frames)))
          (:blind (return (values :blinded frames)))
          (:leaf (return (values (if (every #'= (sn-key sn) (subseq key h)) :leaf :diverged) frames)))
          ((:ext :branch)
           (when (eq (sn-kind sn) :ext)
             (let ((k (sn-key sn)))
               (unless (every #'= k (subseq key h (+ h (length k))))
                 (return (values :diverged frames)))))
           (let* ((l (sn-logical sn h)) (b (sn-branch sn)) (nib (aref key l))
                  (child (svref (sn-children b) nib)))
             (unless child (return (values :nochild frames)))
             (setf sn child h (1+ l)
                   set (let ((b b) (nib nib)) (lambda (new) (setf (svref (sn-children b) nib) new))))
             (push (list sn h set) frames))))))))

(defun frames-head-branch-logical (frames)
  "The logical path length of the deepest branch in FRAMES (the head's parent for a blind head)."
  (loop for (sn h) in frames
        when (member (sn-kind sn) '(:ext :branch)) return (sn-logical sn h)))

(defun concat-nibbles (&rest parts)
  (let ((v (apply #'concatenate 'vector parts)))
    (coerce v '(simple-array (unsigned-byte 4) (*)))))

(defun collapse (frame)
  "Replace the branch-like node of FRAME (one child left, revealed) by that child."
  (destructuring-bind (sn h set) frame
    (declare (ignore h))
    (let* ((b (sn-branch sn))
           (nib (position-if-not #'null (sn-children b)))
           (child (svref (sn-children b) nib))
           (prefix (concat-nibbles (if (eq (sn-kind sn) :ext) (sn-key sn) #()) (vector nib))))
      (funcall set
               (ecase (sn-kind child)
                 (:leaf (make-sn :leaf :key (concat-nibbles prefix (sn-key child))))
                 (:ext (make-sn :ext :key (concat-nibbles prefix (sn-key child)) :child (sn-child child)))
                 (:branch (make-sn :ext :key prefix :child child)))))))

(defun padded-key (prefix)
  (let ((k (make-array 64 :element-type '(unsigned-byte 4) :initial-element 0)))
    (replace k prefix)))

(defun sibling-request (key frame)
  "The proof request for the other child of FRAME's branch: (padded-key . parent-length)."
  (destructuring-bind (sn h set) frame
    (declare (ignore set))
    (let* ((l (sn-logical sn h)) (b (sn-branch sn)) (mine (aref key l))
           (sib (loop for i below 16 when (and (/= i mine) (svref (sn-children b) i)) return i)))
      (cons (padded-key (concat-nibbles (subseq key 0 l) (vector sib))) l))))

(defun lone-blind-sibling-p (key frame)
  (destructuring-bind (sn h set) frame
    (declare (ignore set))
    (let* ((b (sn-branch sn)) (mine (aref key (sn-logical sn h))))
      (and (= (child-count b) 2)
           (loop for i below 16 for c = (svref (sn-children b) i)
                 thereis (and c (/= i mine) (eq (sn-kind c) :blind)))))))

(defun remove-leaf (key frames)
  "Remove the leaf at the head of FRAMES (a subtrie's frames start at its root). Returns NIL,
or a proof request when the collapse needs a blinded sibling."
  (let ((parent (second frames)))
    (cond ((null parent)
           ;; The trie's root leaf leaves an empty root; a subtrie's, an empty slot.
           (destructuring-bind (sn h set) (first frames)
             (declare (ignore sn))
             (funcall set (if (zerop h) (make-sn :empty) nil)))
           nil)
          ((lone-blind-sibling-p key parent) (sibling-request key parent))
          (t (destructuring-bind (sn h set) parent
               (declare (ignore set))
               (let ((b (sn-branch sn)))
                 (setf (svref (sn-children b) (aref key (sn-logical sn h))) nil)
                 (when (= (child-count b) 1) (collapse parent))))
             nil))))

(defun remove-cascade (frames)
  "maybe_collapse_or_remove_branch from the branch at the head of FRAMES after it lost a child."
  (loop
    (let* ((frame (first frames)) (b (sn-branch (first frame))) (count (child-count b)))
      (cond ((>= count 2) (return))
            ((= count 0)
             (if (cdr frames)
                 (progn (funcall (third frame) nil) (pop frames))
                 (progn (funcall (third frame) (make-sn :empty)) (return))))
            (t (let ((c (find-if-not #'null (sn-children b))))
                 (unless (eq (sn-kind c) :blind) (collapse frame))
                 (return)))))))

(defun insert-leaf (key result frames)
  (destructuring-bind (sn h set) (first frames)
    (ecase result
      (:leaf)
      (:empty (funcall set (make-sn :leaf :key (subseq key h))))
      (:nochild
       (let ((l (sn-logical sn h)))
         (setf (svref (sn-children (sn-branch sn)) (aref key l))
               (make-sn :leaf :key (subseq key (1+ l))))))
      (:diverged
       (let* ((k (sn-key sn))
              (c (or (mismatch k key :start2 h) (length k)))
              (old (let ((rest (subseq k (1+ c))))
                     (ecase (sn-kind sn)
                       (:leaf (make-sn :leaf :key rest))
                       (:ext (if (zerop (length rest)) (sn-child sn)
                                 (make-sn :ext :key rest :child (sn-child sn)))))))
              (children (make-array 16 :initial-element nil))
              (branch (make-sn :branch :children children)))
         (setf (svref children (aref k c)) old
               (svref children (aref key (+ h c))) (make-sn :leaf :key (subseq key (+ h c 1))))
         (funcall set (if (zerop c) branch (make-sn :ext :key (subseq k 0 c) :child branch))))))))

(defun subtrie-leaves (sn)
  (ecase (sn-kind sn)
    ((:blind :empty) 0)
    (:leaf 1)
    ((:ext :branch) (loop for c across (sn-children (sn-branch sn)) sum (if c (subtrie-leaves c) 0)))))

(defun apply-one (key removal result frames)
  "Apply one update at a seek RESULT and FRAMES; return a proof request or NIL."
  (case result
    (:blinded (cons key (or (frames-head-branch-logical (cdr frames)) -1)))
    (t (if removal
           (when (eq result :leaf) (remove-leaf key frames))
           (progn (insert-leaf key result frames) nil)))))

(defun update-pass (st keys removal)
  "One pass of reth's update_leaves over the sorted KEYS (all removals or all upserts).
Returns the keys left and the proof requests ((key . parent-length) ...)."
  (let ((left '()) (requests '()))
    (flet ((one (key result frames)
             (let ((req (apply-one key removal result frames)))
               (when req (push req requests) (push key left)))))
      (loop while keys
            do (let ((key (pop keys)))
                 (multiple-value-bind (result frames) (seek st key :upper t)
                   (if (not (eq result :subtrie))
                       (one key result frames)
                       (let* ((group (cons key (loop while (and keys (= (aref (first keys) 0) (aref key 0))
                                                                (= (aref (first keys) 1) (aref key 1)))
                                                     collect (pop keys))))
                              (parent (second frames))
                              (slot (aref key (sn-logical (first parent) (second parent))))
                              (children (sn-children (sn-branch (first parent))))
                              (set (third (first frames))))
                         (if (and removal (>= (length group) (subtrie-leaves (first (first frames))))
                                  (lone-blind-sibling-p key parent))
                             (progn (push (sibling-request key parent) requests)
                                    (setf left (append (reverse group) left)))
                             (progn
                               (dolist (k group)
                                 ;; Earlier updates may have replaced or emptied the subtrie root.
                                 (let ((root (svref children slot)))
                                   (when root
                                     (multiple-value-bind (r fr) (seek st k :from (list (list root 2 set)))
                                       (one k r fr)))))
                               (when (null (svref children slot))
                                 (remove-cascade (cdr frames)))))))))))
    (values (sort left #'key<) requests)))

(defun key< (a b)
  (let ((i (mismatch a b))) (and i (< (aref a i) (aref b i)))))

(defun apply-updates (w keys-by-trie removal)
  "Apply the KEYS-BY-TRIE updates ((strie . keys) ...), fetching proofs until none is needed."
  (let ((pending (loop for (st . keys) in keys-by-trie
                       when keys collect (cons st (sort (copy-list keys) #'key<)))))
    (loop
      (let ((all '()))
        (setf pending
              (loop for (st . keys) in pending
                    nconc (multiple-value-bind (left requests) (update-pass st keys removal)
                            (when requests (push (cons st requests) all))
                            (when left (list (cons st left))))))
        (unless all (return))
        (loop for (st . requests) in all
              do (loop for (key . minlen) in requests
                       do (record-proof w (strie-trie st) key minlen)
                          (reveal-proof st key minlen)))))))

(defun trie-witness (account-trie accounts &key (mode :legacy))
  "reth's TrieWitness over the parent ACCOUNT-TRIE: the recorded node octets (see above), in
recording order for :legacy and sorted for :canonical. ACCOUNTS lists one entry per touched
account, (hashed-address empty-p storage-trie slots): EMPTY-P after the block (absent, or no
nonce, balance or code), STORAGE-TRIE the parent storage trie or NIL, SLOTS ((hashed-slot .
zero-p) ...) the touched slots with ZERO-P after the block. Hashed keys are 32 octets."
  (let ((w (make-witness-set))
        (canonical (eq mode :canonical))
        (sparse (make-hash-table :test 'eq)))
    (if (null accounts)
        (record-proof w account-trie (make-array 64 :element-type '(unsigned-byte 4) :initial-element 0) -1)
        (let ((account-st (make-strie account-trie)))
          (dolist (a accounts)
            (let ((key (key-nibbles (first a))))
              (record-proof w account-trie key -1)
              (reveal-proof account-st key -1)))
          (dolist (a accounts)
            (destructuring-bind (hashed empty-p storage slots) a
              (declare (ignore hashed empty-p))
              (when slots
                (let ((st (make-strie storage)))
                  (dolist (s slots)
                    (let ((key (key-nibbles (car s))))
                      (record-proof w storage key -1)
                      (reveal-proof st key -1)))
                  (setf (gethash a sparse) st)))))
          (flet ((storage-pass (removal)
                   (apply-updates w
                                  (loop for a in accounts for st = (gethash a sparse)
                                        when st collect (cons st (loop for (k . zero-p) in (fourth a)
                                                                       when (eq (and zero-p t) removal)
                                                                         collect (key-nibbles k))))
                                  removal)))
            (if canonical
                (progn (storage-pass nil) (storage-pass t))
                (progn (storage-pass t) (storage-pass nil))))
          (let ((removals '()) (upserts '()))
            (dolist (a accounts)
              (destructuring-bind (hashed empty-p storage slots) a
                (declare (ignore slots))
                (let* ((st (gethash a sparse))
                       (storage-empty (if st
                                          (eq (sn-kind (strie-root st)) :empty)
                                          (progn
                                            (unless canonical
                                              (let ((root (and storage (trie-root-node storage))))
                                                (if root
                                                    (record-node w (open-node root (trie-store storage)) (trie-store storage))
                                                    (witness-add w (octets-of #x80)))))
                                            (or (null storage) (null (trie-root-node storage)))))))
                  (if (and empty-p storage-empty)
                      (push (key-nibbles hashed) removals)
                      (push (key-nibbles hashed) upserts)))))
            (flet ((account-pass (keys removal)
                     (apply-updates w (list (cons account-st keys)) removal)))
              (if canonical
                  (progn (account-pass upserts nil) (account-pass removals t))
                  (progn (account-pass removals t) (account-pass upserts nil)))))))
    (let ((nodes (reverse (witness-set-nodes w))))
      (if canonical
          (sort (remove-if (lambda (n) (and (= (length n) 1) (= (aref n 0) #x80))) nodes)
                (lambda (a b)
                  (let ((i (mismatch a b)))
                    (cond ((null i) nil) ((>= i (length a)) t) ((>= i (length b)) nil)
                          (t (< (aref a i) (aref b i)))))))
          nodes))))
