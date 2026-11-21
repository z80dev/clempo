(in-package #:clempo.state)

;;; Accounts as the current block sees them. Storage has three layers per account:
;;;   present  - slots written by the running transaction (journaled)
;;;   original - slot values at the start of the running transaction, a cache of reads
;;;              and of the writes of finished transactions in this block
;;;   pending  - slots written by finished transactions, not yet drained
;;; ORIGINAL is EIP-2200's "original value"; a slot not cached there comes from the
;;; backend unless the account was wiped in this block.

(defstruct (account (:constructor %make-account (address)))
  (address 0 :type u160)
  (nonce 0 :type (unsigned-byte 64))
  (balance 0 :type u256)
  (%code nil :type (or null octets))
  (code-hash +empty-code-hash+ :type octets)
  (commitment 0 :type u256)             ; T14 configuration commitment (TIP-1108), 0 if none
  (present nil :type (or null hash-table))
  (original nil :type (or null hash-table))
  (pending nil :type (or null hash-table))
  (exists nil :type boolean)
  (touched nil :type boolean)
  (created nil :type boolean)
  (destructed nil :type boolean)
  (wiped nil :type boolean)
  (code-changed nil :type boolean)
  (in-tx nil :type boolean)
  (in-backend nil :type boolean)
  (dirty nil :type boolean))

(defstruct (log-entry (:constructor make-log-entry (address topics data)))
  (address 0 :type u160)
  (topics '() :type list)
  (data +empty-code+ :type octets))

(defstruct (world (:constructor %make-world (backend)))
  backend
  (accounts (make-hash-table :size 256) :type hash-table)
  (journal (make-array 256 :adjustable t :fill-pointer 0) :type (and vector (not simple-array)))
  (warm-addresses (make-hash-table) :type hash-table)
  (warm-slots (make-hash-table) :type hash-table)
  (transient (make-hash-table) :type hash-table)
  (refund 0 :type integer)
  (logs '() :type list)
  (tx-accounts '() :type list)
  (block-accounts '() :type list)
  (observer nil :type (or null function))
  ;; One-entry memos of the last account and warm-slot table looked up: consecutive accesses
  ;; mostly hit one contract, and its address is a bignum, costly to hash.
  (last-account nil :type (or null account))
  (last-slots-address nil)
  (last-slots nil :type (or null hash-table))
  (last-warm-address -1))               ; -1: no address

(defun make-world (backend)
  "A world state over BACKEND, empty of changes."
  (%make-world backend))

(defmethod print-object ((a account) s)
  (print-unreadable-object (a s :type t)
    (format s "~(~40,'0x~) n=~D b=~D~:[~; exists~]" (account-address a) (account-nonce a)
            (account-balance a) (account-exists a))))

;;; Loading

(declaim (ftype (function (world integer) (values account &optional)) get-account))
(defun get-account (world address)
  "The account object for ADDRESS, loaded from the backend on first use. The world's
OBSERVER, when set, is called as (observer account nil nil) on every access."
  (declare #.*hot* (type world world))
  (let* ((accounts (world-accounts world))
         (last (world-last-account world))
         (account
           (if (and last (eql address (account-address last)))
               last
               (setf (world-last-account world)
                     (or (gethash address accounts)
                         (setf (gethash address accounts)
                               (let ((a (%make-account address)))
                                 (multiple-value-bind (found nonce balance code-hash commitment)
                                     (backend-account (world-backend world) address)
                                   (when found
                                     (setf (account-exists a) t
                                           (account-in-backend a) t
                                           (account-nonce a) nonce
                                           (account-balance a) balance
                                           (account-commitment a) (or commitment 0)
                                           (account-code-hash a)
                                           (if (octets= code-hash +empty-code-hash+)
                                               +empty-code-hash+
                                               code-hash))))
                                 a))))))
         (observer (world-observer world)))
    (when observer (funcall observer account nil nil))
    account))

(defun account-code (world account)
  (declare (type account account))
  (or (account-%code account)
      (setf (account-%code account)
            (if (octets= (account-code-hash account) +empty-code-hash+)
                +empty-code+
                (backend-code (world-backend world) (account-code-hash account))))))

(declaim (inline account-empty-p))
(defun account-empty-p (account)
  "EIP-161 emptiness: no nonce, no balance, no code."
  (declare (type account account))
  (and (zerop (account-nonce account))
       (zerop (account-balance account))
       (eq (account-code-hash account) +empty-code-hash+)))

(defun account-dead-p (account)
  "Absent from the state or empty (EIP-161)."
  (or (not (account-exists account)) (account-empty-p account)))

(defun account-alive-p (account) (not (account-dead-p account)))

;;; The journal: a stack of undo closures. A checkpoint is its length.

(declaim (inline journal note-tx-account))
(defun journal (world undo)
  (vector-push-extend undo (world-journal world))
  nil)

(defun note-tx-account (world account)
  (unless (account-in-tx account)
    (setf (account-in-tx account) t)
    (push account (world-tx-accounts world))))

(defun checkpoint (world)
  "A mark to ROLLBACK to."
  (fill-pointer (world-journal world)))

(defun rollback (world checkpoint)
  "Undo every change made since CHECKPOINT."
  (let ((j (world-journal world)))
    (loop while (> (fill-pointer j) checkpoint)
          do (funcall (the function (vector-pop j))))))

(define-constant +ripemd160-address+ 3)

(defun touch-account (world account)
  "EIP-161 touch. Reverting a touch of 0x03 keeps it touched (mainnet block 2,675,119)."
  (declare (type account account))
  (note-tx-account world account)
  (unless (account-touched account)
    (setf (account-touched account) t)
    (unless (= (account-address account) +ripemd160-address+)
      (journal world (lambda () (setf (account-touched account) nil))))))

(defmacro define-account-setter (name field (&rest extra) doc)
  "Define (NAME world address-or-account value): journal the old FIELD and existence, set
the field, mark the account existing and touched."
  `(defun ,name (world account value)
     ,doc
     (let* ((account (if (account-p account) account (get-account world account)))
            (old (,field account))
            (old-exists (account-exists account)))
       (journal world (lambda () (setf (,field account) old (account-exists account) old-exists)))
       (setf (,field account) value (account-exists account) t)
       ,@extra
       (touch-account world account)
       value)))

(define-account-setter set-balance account-balance () "Set the balance.")
(define-account-setter set-nonce account-nonce () "Set the nonce.")
(define-account-setter set-commitment account-commitment ()
  "Set the T14 configuration commitment (TIP-1108).")

(defun get-balance (world address) (account-balance (get-account world address)))
(defun get-nonce (world address) (account-nonce (get-account world address)))

(defun add-balance (world account amount)
  (let ((account (if (account-p account) account (get-account world account))))
    (if (zerop amount)
        (touch-account world account)
        (set-balance world account (+ (account-balance account) amount)))))

(defun sub-balance (world account amount)
  (let ((account (if (account-p account) account (get-account world account))))
    (invariant (<= amount (account-balance account)) "balance underflow")
    (if (zerop amount)
        (touch-account world account)
        (set-balance world account (- (account-balance account) amount)))))

(defun increment-nonce (world account)
  (let ((account (if (account-p account) account (get-account world account))))
    (set-nonce world account (1+ (account-nonce account)))))

(defun get-code (world address) (account-code world (get-account world address)))
(defun get-code-hash (world address) (account-code-hash (get-account world address)))
(defun code-size (world address) (length (get-code world address)))

(defun set-code (world account code)
  (declare (type octets code))
  (let* ((account (if (account-p account) account (get-account world account)))
         (old (account-code world account))
         (old-hash (account-code-hash account))
         (old-exists (account-exists account))
         (old-changed (account-code-changed account)))
    (journal world (lambda ()
                     (setf (account-%code account) old (account-code-hash account) old-hash
                           (account-exists account) old-exists
                           (account-code-changed account) old-changed)))
    (setf (account-%code account) code
          (account-code-hash account) (code-hash code)
          (account-exists account) t
          (account-code-changed account) t)
    (touch-account world account)
    code))

(defun account-exists-p (world address) (account-exists (get-account world address)))

(defun mark-created (world account)
  "EIP-6780: ACCOUNT was created by the running transaction."
  (unless (account-created account)
    (setf (account-created account) t)
    (note-tx-account world account)
    (journal world (lambda () (setf (account-created account) nil)))))

(defun created-p (account) (account-created account))

(defun mark-destructed (world account)
  "Schedule ACCOUNT for deletion at the end of the transaction. Returns T when it was not
already scheduled."
  (unless (account-destructed account)
    (setf (account-destructed account) t)
    (note-tx-account world account)
    (journal world (lambda () (setf (account-destructed account) nil)))
    t))

(defun destructed-p (account) (account-destructed account))

;;; Storage

(declaim (inline ensure-table))
(defun ensure-table (table-or-nil)
  (or table-or-nil (make-hash-table)))

(defun account-storage-original (world account slot)
  "The value of SLOT at the start of the running transaction. Calls the world's OBSERVER
as (observer account slot original): every storage read and write goes through here."
  (declare #.*hot* (type account account))
  (let ((v (%storage-original world account slot))
        (observer (world-observer world)))
    (when observer (funcall observer account slot v))
    v))

(defun %storage-original (world account slot)
  (declare #.*hot* (type account account))
  (let ((orig (or (account-original account)
                  (setf (account-original account) (make-hash-table)))))
    (multiple-value-bind (v found) (gethash slot orig)
      (if found
          v
          (setf (gethash slot orig)
                (if (account-wiped account)
                    0
                    (backend-storage (world-backend world) (account-address account) slot)))))))

(defun account-sload (world account slot)
  "The present value of SLOT."
  (declare #.*hot* (type account account))
  (let ((present (account-present account)))
    (if present
        (multiple-value-bind (v found) (gethash slot present)
          (if found v (account-storage-original world account slot)))
        (account-storage-original world account slot))))

(defun account-sstore (world account slot value)
  "Write SLOT; returns (values original present) as they were before the write."
  (declare #.*hot* (type account account))
  (let* ((original (account-storage-original world account slot))
         (present-table (or (account-present account)
                            (setf (account-present account) (make-hash-table)))))
    (multiple-value-bind (old found) (gethash slot present-table)
      (if found
          (journal world (lambda () (setf (gethash slot present-table) old)))
          (journal world (lambda () (remhash slot present-table))))
      (setf (gethash slot present-table) value)
      (note-tx-account world account)
      (values original (if found old original)))))

(defun sload (world address slot) (account-sload world (get-account world address) slot))
(defun sstore (world address slot value)
  (account-sstore world (get-account world address) slot value))
(defun storage-original (world address slot)
  (account-storage-original world (get-account world address) slot))

(defun storage-nonempty-p (world account)
  "True when ACCOUNT holds any nonzero storage (EIP-7610)."
  (or (and (account-present account)
           (loop for v being the hash-values of (account-present account) thereis (plusp v)))
      (and (account-pending account)
           (loop for v being the hash-values of (account-pending account) thereis (plusp v)))
      (and (not (account-wiped account))
           (not (backend-storage-empty-p (world-backend world) (account-address account))))))

(defun wipe-storage (account)
  "Forget all storage of ACCOUNT, committed included (deletion)."
  (setf (account-present account) nil
        (account-original account) nil
        (account-pending account) nil
        (account-wiped account) t))

;;; Transient storage (EIP-1153)

(defun tload (world address slot)
  (let ((table (gethash address (world-transient world))))
    (if table (values (gethash slot table 0)) 0)))

(defun tstore (world address slot value)
  (let ((table (or (gethash address (world-transient world))
                   (setf (gethash address (world-transient world)) (make-hash-table)))))
    (multiple-value-bind (old found) (gethash slot table)
      (if found
          (journal world (lambda () (setf (gethash slot table) old)))
          (journal world (lambda () (remhash slot table))))
      (setf (gethash slot table) value))))

(defun take-transient-storage (world address)
  "The transient storage of ADDRESS as an alist, removed from the world (not journaled)."
  (let ((table (gethash address (world-transient world))))
    (remhash address (world-transient world))
    (when table
      (loop for k being the hash-keys of table using (hash-value v) collect (cons k v)))))

;;; Access lists (EIP-2929)

(defun address-warm-p (world address)
  (or (eql address (world-last-warm-address world))
      (values (gethash address (world-warm-addresses world)))))

(defun warm-address (world address)
  "Mark ADDRESS warm. Returns T when it already was."
  (declare #.*hot*)
  (if (eql address (world-last-warm-address world))
      t
      (let ((table (world-warm-addresses world)))
        (prog1 (or (gethash address table)
                   (progn (setf (gethash address table) t)
                          (journal world (lambda ()
                                           (remhash address table)
                                           (when (eql address (world-last-warm-address world))
                                             (setf (world-last-warm-address world) -1))))
                          nil))
          (setf (world-last-warm-address world) address)))))

(defun warm-slots-of (world address create)
  "ADDRESS's table of warm slots (made when CREATE), or NIL."
  (declare #.*hot* (type world world))
  (if (and (world-last-slots world) (eql address (world-last-slots-address world)))
      (world-last-slots world)
      (let* ((table (world-warm-slots world))
             (slots (or (gethash address table)
                        (and create (setf (gethash address table) (make-hash-table))))))
        (when slots
          (setf (world-last-slots-address world) address (world-last-slots world) slots))
        slots)))

(defun slot-warm-p (world address slot)
  (let ((slots (warm-slots-of world address nil)))
    (and slots (values (gethash slot slots)))))

(defun warm-slot (world address slot)
  "Mark (ADDRESS, SLOT) warm. Returns T when it already was."
  (declare #.*hot*)
  (let ((slots (warm-slots-of world address t)))
    (or (gethash slot slots)
        (progn (setf (gethash slot slots) t)
               (journal world (lambda () (remhash slot slots)))
               nil))))

(defun access-slot (world address slot skip-cold)
  "EIP-2929 access to (ADDRESS, SLOT): (values cold-p skipped-p). A cold slot is warmed,
unless SKIP-COLD, when it stays cold and SKIPPED-P is true."
  (declare #.*hot*)
  (let ((slots (warm-slots-of world address (not skip-cold))))
    (cond ((and slots (gethash slot slots)) (values nil nil))
          (skip-cold (values t t))
          (t (setf (gethash slot slots) t)
             (journal world (lambda () (remhash slot slots)))
             (values t nil)))))

;;; Refunds and logs

(defun add-refund (world delta)
  (let ((old (world-refund world)))
    (journal world (lambda () (setf (world-refund world) old)))
    (setf (world-refund world) (+ old delta))))

(defun set-refund (world value)
  (let ((old (world-refund world)))
    (journal world (lambda () (setf (world-refund world) old)))
    (setf (world-refund world) value)))

(defun add-log (world entry)
  (let ((old (world-logs world)))
    (journal world (lambda () (setf (world-logs world) old)))
    (push entry (world-logs world))
    entry))

(defun logs-since (world)
  "The running transaction's logs, oldest first."
  (reverse (world-logs world)))

;;; End of a transaction

(defun delete-account (account)
  (setf (account-exists account) nil
        (account-nonce account) 0
        (account-balance account) 0
        (account-commitment account) 0
        (account-%code account) +empty-code+
        (account-code-hash account) +empty-code-hash+
        (account-code-changed account) nil)
  (wipe-storage account))

(defun reset-tx-state (world)
  (setf (fill-pointer (world-journal world)) 0
        (world-refund world) 0
        (world-logs world) '()
        (world-tx-accounts world) '()
        (world-last-slots-address world) nil
        (world-last-slots world) nil
        (world-last-warm-address world) -1)
  (clrhash (world-warm-addresses world))
  (clrhash (world-warm-slots world))
  (clrhash (world-transient world)))

(defun finalize-tx (world)
  "Close the running transaction: delete self-destructed accounts and touched empty ones
\(EIP-161), fold written slots into the transaction-start values, and clear the journal,
access lists, transient storage, refund counter and logs."
  (dolist (account (world-tx-accounts world))
    (when (or (account-destructed account)
              (and (account-touched account) (account-exists account) (account-empty-p account)))
      (delete-account account))
    (let ((present (account-present account)))
      (when (and present (plusp (hash-table-count present)))
        (let ((orig (ensure-table (account-original account)))
              (pending (ensure-table (account-pending account))))
          (setf (account-original account) orig (account-pending account) pending)
          (maphash (lambda (k v) (setf (gethash k orig) v (gethash k pending) v)) present)
          (clrhash present))))
    (unless (or (account-dirty account)
                (not (or (account-exists account) (account-in-backend account))))
      (setf (account-dirty account) t)
      (push account (world-block-accounts world)))
    (setf (account-in-tx account) nil
          (account-touched account) nil
          (account-created account) nil
          (account-destructed account) nil))
  (reset-tx-state world)
  world)

(defun discard-tx (world)
  "Abandon the running transaction: undo everything it did."
  (rollback world 0)
  (dolist (account (world-tx-accounts world))
    (setf (account-in-tx account) nil))
  (reset-tx-state world)
  world)

;;; Block-level changes

(defstruct (account-change (:constructor make-account-change
                               (address deleted-p storage-wiped-p nonce balance code-hash code
                                storage &optional (commitment 0))))
  (address 0 :type u160)
  (deleted-p nil)
  (storage-wiped-p nil)
  (nonce 0)
  (balance 0)
  (code-hash +empty-code-hash+)
  (code nil)
  (storage '() :type list)
  (commitment 0))

(defun drain-changes (world)
  "The net changes of the finished transactions since the last drain, one ACCOUNT-CHANGE
per account, and forget them. Storage is an alist of (slot . value), 0 meaning deleted;
when STORAGE-WIPED-P, apply the wipe before the slots."
  (prog1
      (loop for account in (world-block-accounts world)
            collect (prog1
                        (make-account-change
                         (account-address account)
                         (not (account-exists account))
                         (account-wiped account)
                         (account-nonce account)
                         (account-balance account)
                         (account-code-hash account)
                         (and (account-code-changed account) (account-%code account))
                         (let ((pending (account-pending account)))
                           (when pending
                             (loop for k being the hash-keys of pending using (hash-value v)
                                   collect (cons k v))))
                         (account-commitment account))
                      (setf (account-dirty account) nil
                            (account-in-backend account) (account-exists account)
                            (account-wiped account) nil
                            (account-code-changed account) nil
                            (account-pending account) nil)))
    (setf (world-block-accounts world) '())))

(defgeneric apply-changes (backend changes)
  (:documentation "Write CHANGES (a list of ACCOUNT-CHANGE) into BACKEND."))

(defmethod apply-changes ((b memory-backend) changes)
  (dolist (c changes b)
    (let ((address (account-change-address c)))
      (cond ((account-change-deleted-p c)
             (remhash address (memory-backend-accounts b)))
            (t
             (when (account-change-storage-wiped-p c)
               (remhash address (memory-backend-accounts b)))
             (let ((a (or (gethash address (memory-backend-accounts b))
                          (setf (gethash address (memory-backend-accounts b)) (make-mem-account)))))
               (setf (mem-account-nonce a) (account-change-nonce c)
                     (mem-account-balance a) (account-change-balance c)
                     (mem-account-code-hash a) (account-change-code-hash c)
                     (mem-account-commitment a) (account-change-commitment c))
               (let ((code (account-change-code c)))
                 (when (and code (plusp (length code)))
                   (setf (gethash (octets->integer (account-change-code-hash c))
                                  (memory-backend-codes b))
                         code)))
               (loop for (slot . value) in (account-change-storage c)
                     do (if (zerop value)
                            (remhash slot (mem-account-storage a))
                            (setf (gethash slot (mem-account-storage a)) value)))))))))

;;; EIP-7702 delegation designators: 0xef0100 || address

(defun delegation-target (code)
  "The address CODE delegates to, or NIL."
  (declare (type octets code))
  (when (and (= (length code) 23) (= (aref code 0) #xef) (= (aref code 1) 1) (= (aref code 2) 0))
    (octets->integer code :start 3 :end 23)))

(defun delegation-designator (address)
  (octets-concat (octets-of #xef 1 0) (integer->octets address 20)))
