(defpackage #:clempo.rpc.bal
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:h #:clempo.headers)
                    (#:executor #:clempo.executor) (#:state #:clempo.state) (#:rlp #:clempo.rlp)
                    (#:calls #:clempo.rpc.calls))
  (:documentation "EIP-7928 block access lists (eth_getBlockAccessList and its ByBlockHash,
ByBlockNumber and Raw forms) and reth_getBalanceChangesInBlock.

reth builds the list by re-executing the block on its parent's state with evm2's BAL builder
on (rpc-eth-api helpers/bal.rs, evm2 evm/bal): each committed transaction's loaded accounts
and slots are folded in at its block access index (0 for the pre-execution system calls,
i + 1 for transaction i, n + 1 for the post-execution ones); a value that differs from the
one before that index is a change, a loaded slot never changed is a read, a loaded account
with nothing changed still has an entry. Here the block is replayed on the executor's overlay
with a world observer that sees every account and slot access, and each index is closed by
comparing what it loaded with the values the index started from.")
  (:export #:bal-account #:make-bal-account #:bal-account-address #:bal-account-slots
           #:bal-account-balance #:bal-account-nonce #:bal-account-code
           #:block-access-list #:bal-json #:bal-rlp))

(in-package #:clempo.rpc.bal)

;;; The list: one entry per account, its changes as (index . value) lists, newest first.

(defstruct (bal-account (:constructor make-bal-account (address)))
  (address 0 :type (unsigned-byte 160))
  (slots (make-hash-table) :type hash-table) ; slot -> changes; NIL for a slot only read
  (balance '() :type list)
  (nonce '() :type list)
  (code '() :type list))                     ; (index . code octets)

(defun oldest-first (changes) (reverse changes))

(defun slot-changes (a)
  "alloy AccountChanges::sort: changed slots ascending, each with its changes by index."
  (sort (loop for slot being the hash-keys of (bal-account-slots a) using (hash-value changes)
              when changes collect (cons slot (oldest-first changes)))
        #'< :key #'car))

(defun slot-reads (a)
  (sort (loop for slot being the hash-keys of (bal-account-slots a) using (hash-value changes)
              unless changes collect slot)
        #'<))

(defun sorted-accounts (accounts)
  (sort (copy-list accounts) #'< :key #'bal-account-address))

(defun bal-json (accounts)
  "ACCOUNTS (a list of BAL-ACCOUNT) as alloy's BlockAccessList JSON, sorted by address."
  (flet ((changes (list render)
           (map 'simple-vector (lambda (c) (json:jobject "index" (q (car c)) "value" (funcall render (cdr c))))
                list)))
    (map 'simple-vector
         (lambda (a)
           (json:jobject
            "address" (addr (bal-account-address a))
            "storageChanges" (map 'simple-vector
                                  (lambda (s) (json:jobject "key" (q (car s)) "changes" (changes (cdr s) #'q)))
                                  (slot-changes a))
            "storageReads" (map 'simple-vector #'q (slot-reads a))
            "balanceChanges" (changes (oldest-first (bal-account-balance a)) #'q)
            "nonceChanges" (changes (oldest-first (bal-account-nonce a)) #'q)
            "codeChanges" (map 'simple-vector
                               (lambda (c) (json:jobject "index" (q (car c)) "code" (hexbytes (cdr c))))
                               (oldest-first (bal-account-code a)))))
         (sorted-accounts accounts))))

(defun bal-rlp (accounts)
  "ACCOUNTS RLP-encoded as alloy's BlockAccessList: a list of
[address, [[slot, [[index, value] ...]] ...], [slot ...], [[index, balance] ...],
 [[index, nonce] ...], [[index, code] ...]]."
  (flet ((pairs (changes) (mapcar (lambda (c) (list (car c) (cdr c))) changes)))
    (rlp:rlp-encode
     (mapcar (lambda (a)
               (list (integer->octets (bal-account-address a) 20)
                     (mapcar (lambda (s) (list (car s) (pairs (cdr s)))) (slot-changes a))
                     (slot-reads a)
                     (pairs (oldest-first (bal-account-balance a)))
                     (pairs (oldest-first (bal-account-nonce a)))
                     (pairs (oldest-first (bal-account-code a)))))
             (sorted-accounts accounts)))))

;;; Recording a replay

(defstruct (recorder (:conc-name rec-))
  (index 0 :type (unsigned-byte 64))
  (accounts (make-hash-table) :type hash-table)  ; address -> bal-account
  (before (make-hash-table) :type hash-table)    ; address -> (nonce balance code-hash) before the block
  (last (make-hash-table) :type hash-table)      ; address -> (nonce balance code-hash) before this index
  (last-slots (make-hash-table) :type hash-table) ; address -> slot -> value before this index
  (seen (make-hash-table) :type hash-table)      ; address -> account object, loaded in this index
  (seen-slots (make-hash-table) :type hash-table)) ; address -> slot -> T, loaded in this index

(defun account-info (account)
  (list (state:account-nonce account) (state:account-balance account) (state:account-code-hash account)))

(defun note-account (rec account)
  (let ((address (state:account-address account)))
    (setf (gethash address (rec-seen rec)) account)
    (unless (gethash address (rec-last rec))
      (let ((info (account-info account)))
        (setf (gethash address (rec-last rec)) info
              (gethash address (rec-before rec)) info)))))

(defun table-at (table key)
  (or (gethash key table) (setf (gethash key table) (make-hash-table))))

(defun note-slot (rec account slot original)
  "ORIGINAL is the slot's value when the running transaction started; on the first access in
the block that is its value before the index (an earlier change would have accessed it)."
  (note-account rec account)
  (let ((address (state:account-address account)))
    (setf (gethash slot (table-at (rec-seen-slots rec) address)) t)
    (let ((last (table-at (rec-last-slots rec) address)))
      (unless (nth-value 1 (gethash slot last))
        (setf (gethash slot last) original)))))

(defun observer (rec)
  "A clempo.state world observer recording into REC."
  (lambda (account slot original)
    (if slot (note-slot rec account slot original) (note-account rec account))))

(defun entry (rec address)
  (or (gethash address (rec-accounts rec))
      (setf (gethash address (rec-accounts rec)) (make-bal-account address))))

(defun close-index (rec world)
  "Fold what the current index loaded into the list and move to the next index."
  (let ((index (rec-index rec))
        (observer (state:world-observer world)))
    (setf (state:world-observer world) nil)
    (unwind-protect
         (progn
           (loop for address being the hash-keys of (rec-seen rec) using (hash-value account)
                 do (let ((a (entry rec address)))
                      (destructuring-bind (nonce balance code-hash) (gethash address (rec-last rec))
                        (unless (= nonce (state:account-nonce account))
                          (push (cons index (state:account-nonce account)) (bal-account-nonce a)))
                        (unless (= balance (state:account-balance account))
                          (push (cons index (state:account-balance account)) (bal-account-balance a)))
                        (unless (octets= code-hash (state:account-code-hash account))
                          (push (cons index (state:account-code world account)) (bal-account-code a))))
                      (setf (gethash address (rec-last rec)) (account-info account))))
           (loop for address being the hash-keys of (rec-seen-slots rec) using (hash-value slots)
                 do (let ((a (entry rec address))
                          (account (state:get-account world address))
                          (last (gethash address (rec-last-slots rec))))
                      (loop for slot being the hash-keys of slots
                            do (let ((value (state:account-sload world account slot)))
                                 (unless (nth-value 1 (gethash slot (bal-account-slots a)))
                                   (setf (gethash slot (bal-account-slots a)) '()))
                                 (unless (= value (gethash slot last))
                                   (push (cons index value) (gethash slot (bal-account-slots a)))
                                   (setf (gethash slot last) value)))))))
      (setf (state:world-observer world) observer))
    (clrhash (rec-seen rec))
    (clrhash (rec-seen-slots rec))
    (incf (rec-index rec))))

(defun parent-id (b)
  (list :hash (h:ethereum-header-parent-hash (h:header-inner (cblock-header b)))))

(defun replay-recorded (b)
  "Re-execute block B on its parent's state, recording every index. Returns the RECORDER."
  (when (zerop (cblock-number b)) (header-not-found (parent-id b)))
  (let* ((backend (state-at (1- (cblock-number b))))
         (rec (make-recorder))
         (world (state:make-world backend)))
    (setf (state:world-observer world) (observer rec))
    (flet ((system (what thunk)
             (handler-case (funcall thunk)
               (executor:execution-failure (e)
                 (internal-error (format nil "replaying the ~A system calls of block ~D: ~A" what
                                         (cblock-number b) (executor:execution-failure-reason e)))))))
      (let ((ctx (system "pre-execution"
                         (lambda ()
                           (executor:begin-call-block backend (executor:header-block-env (cblock-header b))
                                                      :block-hash-fn (calls:block-hash-fn) :world world)))))
        (close-index rec world)
        (dotimes (i (length (cblock-txs b)))
          (calls:replay-tx ctx b i)
          (close-index rec world))
        (system "post-execution" (lambda () (executor:finish-call-block ctx)))
        (close-index rec world)))
    rec))

(defun block-access-list (b)
  "Block B's access list as a list of BAL-ACCOUNT."
  (loop for a being the hash-values of (rec-accounts (replay-recorded b)) collect a))

(defun balance-changes (b)
  "reth_getBalanceChangesInBlock: every account whose balance after block B differs from its
balance before it, as a JSON object address -> balance."
  (if (zerop (cblock-number b))
      (json:jobject)
      (let ((rec (replay-recorded b)))
        (apply #'json:jobject
               (loop for a being the hash-values of (rec-accounts rec)
                     for changes = (bal-account-balance a)
                     when (and changes
                               (/= (cdr (first changes))
                                   (second (gethash (bal-account-address a) (rec-before rec)))))
                       append (list (addr (bal-account-address a)) (q (cdr (first changes)))))))))

;;; Methods

(defun id-block (id)
  "reth recovered_block for ID: the block, or NIL (pending has none)."
  (unless (eq id :pending)
    (let ((n (block-id-number id)))
      (and n (<= n chain:*head*) (chain-block n)))))

(defun access-list-json (id)
  (let ((b (id-block id)))
    (if b (bal-json (block-access-list b)) :null)))

(defrpc "eth_getBlockAccessList" ((id block-id)) "the EIP-7928 block access list of a block"
  (access-list-json id))

(defrpc "eth_getBlockAccessListByBlockHash" ((hash hash)) "eth_getBlockAccessList by block hash"
  (access-list-json (list :hash hash)))

(defrpc "eth_getBlockAccessListByBlockNumber" ((id block-number-or-tag))
  "eth_getBlockAccessList by block number or tag"
  (access-list-json id))

(defrpc "eth_getBlockAccessListRaw" ((id block-id)) "the RLP-encoded block access list of a block"
  (octets->hex (bal-rlp (block-access-list (or (id-block id) (header-not-found id))))))

(defrpc "reth_getBalanceChangesInBlock" ((id block-id))
  "accounts whose balance the block changed, with their new balances"
  (balance-changes (or (id-block id) (header-not-found id))))
