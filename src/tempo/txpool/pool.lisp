(in-package #:clempo.txpool)

(defun adjust-authorities (pool entry delta)
  (dolist (authority (entry-authorities entry))
    (let ((count (+ (gethash authority (pool-authority-counts pool) 0) delta)))
      (if (zerop count) (remhash authority (pool-authority-counts pool))
          (setf (gethash authority (pool-authority-counts pool)) count)))))

(defun insert-entry (pool entry)
  (setf (gethash (entry-hash entry) (pool-entries pool)) entry)
  (setf (gethash (entry-tx entry) (pool-transactions pool)) entry)
  (if (entry-replay-hash entry)
      (setf (gethash (entry-replay-hash entry) (pool-replays pool)) entry)
      (let ((lane (entry-lane entry)))
        (setf (gethash lane (pool-lanes pool))
              (merge 'list (list entry) (gethash lane (pool-lanes pool)) #'< :key #'entry-nonce))))
  (adjust-authorities pool entry 1)
  entry)

(defun remove-entry (pool entry)
  (remhash (entry-hash entry) (pool-entries pool))
  (remhash (entry-tx entry) (pool-transactions pool))
  (if (entry-replay-hash entry)
      (remhash (entry-replay-hash entry) (pool-replays pool))
      (let* ((lane (entry-lane entry))
             (remaining (delete entry (gethash lane (pool-lanes pool)) :test #'eq)))
        (if remaining (setf (gethash lane (pool-lanes pool)) remaining)
            (remhash lane (pool-lanes pool)))))
  (adjust-authorities pool entry -1)
  (entry-tx entry))

(defun pool-remove-transaction (pool hash)
  "Remove one transaction by its 32-octet or integer hash, returning TX or NIL.
A removed lane head/gap demotes its descendants until the missing nonce is restored."
  (let ((entry (gethash (hash-key hash) (pool-entries pool))))
    (when entry (remove-entry pool entry))))

(defun authority-at-capacity (pool entry released)
  (find-if
   (lambda (authority)
     (>= (- (gethash authority (pool-authority-counts pool) 0)
            (if (and released (member authority (entry-authorities released) :test #'=)) 1 0))
         (pool-max-transactions-per-sender pool)))
   (entry-authorities entry)))

(defun replacement-priced-p (pool old new)
  ;; Both fee dimensions must rise by the configured percentage; integer ratios avoid rounding.
  (let ((factor (+ 100 (pool-price-bump pool))))
    (and (>= (* 100 (entry-fee-cap new)) (* factor (entry-fee-cap old)))
         (>= (* 100 (entry-priority-fee new)) (* factor (entry-priority-fee old))))))

(defun nonce-pending-p (pool entry)
  (if (entry-replay-hash entry) t
      (loop with expected = (entry-state-nonce entry)
            for existing in (gethash (entry-lane entry) (pool-lanes pool))
            do (when (/= (entry-nonce existing) expected) (return nil))
               (when (eq existing entry) (return t))
               (incf expected))))

(defun submit-transaction (pool tx reader &key timestamp fork)
  "Validate TX against READER, then insert atomically. Return (values TX STATUS REPLACED),
where STATUS is :PENDING for a gapless nonce lane or :QUEUED for a nonce gap. Time/fee
readiness is checked again by selection; :PENDING does not imply valid-after is reached.
Signals POOL-ERROR for rejection; backend failures propagate without changing the pool.
Call POOL-REVALIDATE before admitting against a newly committed canonical snapshot."
  (let ((hash (hash-key (clempo.transactions:transaction-hash tx))))
    (when (gethash hash (pool-entries pool)) (reject :already-imported)))
  (multiple-value-bind (tip spec base-fee) (snapshot-context reader timestamp fork)
    (let* ((entry (validate-entry pool tx reader tip spec))
           (lane (unless (entry-replay-hash entry)
                   (gethash (entry-lane entry) (pool-lanes pool))))
           (old (and lane (find (entry-nonce entry) lane :key #'entry-nonce)))
           (evicted nil))
      (when (and (entry-replay-hash entry)
                 (gethash (entry-replay-hash entry) (pool-replays pool)))
        (reject :already-imported "expiring nonce signing payload already pooled"))
      (when (and lane (>= (length lane) (pool-max-transactions-per-lane pool))
                 (> (entry-nonce entry) (entry-state-nonce entry)))
        (reject :lane-capacity))
      (when (and old (not (replacement-priced-p pool old entry)))
        (reject :replacement-underpriced))
      ;; Permit a current-nonce gap filler at capacity only when evicting the lane's
      ;; highest queued nonce frees a slot for EVERY saturated authority.
      (when (and (not old) lane (= (entry-nonce entry) (entry-state-nonce entry))
                 (> (entry-nonce (first lane)) (entry-nonce entry))
                 (authority-at-capacity pool entry nil))
        (let ((highest (car (last lane))))
          (unless (authority-at-capacity pool entry highest) (setf evicted highest))))
      (let ((authority (authority-at-capacity pool entry (or old evicted))))
        (when authority (reject :sender-capacity authority)))
      ;; All fallible checks precede mutation, including replacement-authority accounting.
      (when old (remove-entry pool old))
      (when evicted (remove-entry pool evicted))
      (setf (entry-id entry) (pool-next-id pool)
            (pool-timestamp pool) tip (pool-base-fee pool) base-fee)
      (incf (pool-next-id pool))
      (insert-entry pool entry)
      (values tx (if (nonce-pending-p pool entry) :pending :queued)
              (when old (entry-tx old))))))

(defun expired-p (entry timestamp)
  (or (let ((before (entry-valid-before entry)))
        (and before (<= before timestamp)))
      (let ((expiry (entry-key-expiry entry))) (and expiry (<= expiry timestamp)))))

(defun pool-sweep-expired (pool timestamp)
  "Evict transactions whose exclusive valid-before/key expiry is at or below TIMESTAMP.
Future valid-after never causes eviction. Return removed transactions in submission order."
  (let ((expired (sort (loop for entry being the hash-values of (pool-entries pool)
                            when (expired-p entry timestamp) collect entry)
                       #'< :key #'entry-id)))
    (mapcar (lambda (entry) (remove-entry pool entry)) expired)))

(defun includable-p (entry timestamp base-fee)
  (and (not (expired-p entry timestamp))
       (let ((after (entry-valid-after entry)))
         (or (null after) (<= after timestamp)))
       (>= (entry-fee-cap entry) base-fee)))

(defun entry-priority (entry base-fee)
  ;; ordering.rs saturates the effective tip to u64, including legacy gas prices.
  (min +u64-max+ (entry-priority-fee entry) (- (entry-fee-cap entry) base-fee)))

(defun better-p (a b base-fee)
  (let ((pa (entry-priority a base-fee)) (pb (entry-priority b base-fee)))
    (if (/= pa pb) (> pa pb) (< (entry-id a) (entry-id b)))))

(defun heap-push (heap lane base-fee)
  (let ((index (fill-pointer heap)))
    (vector-push-extend lane heap)
    (loop while (plusp index)
          for parent = (floor (1- index) 2)
          while (better-p (car lane) (car (aref heap parent)) base-fee)
          do (setf (aref heap index) (aref heap parent) index parent))
    (setf (aref heap index) lane)))

(defun heap-pop (heap base-fee)
  (let* ((best (aref heap 0)) (last (vector-pop heap)) (length (fill-pointer heap)))
    (when (plusp length)
      (let ((index 0))
        (loop for left = (1+ (* index 2))
              while (< left length)
              for right = (1+ left)
              for child = (if (and (< right length)
                                  (better-p (car (aref heap right)) (car (aref heap left)) base-fee))
                             right left)
              while (better-p (car (aref heap child)) (car last) base-fee)
              do (setf (aref heap index) (aref heap child) index child))
        (setf (aref heap index) last)))
    best))

(defun pool-best-transactions (pool &key (timestamp (pool-timestamp pool))
                                        (base-fee (pool-base-fee pool)))
  "Return a snapshot ordered by descending effective tip, FIFO ties, preserving each
(sender,nonce-key) dependency. A future/expired/under-base-fee head blocks its descendants.
Expiring nonces are independent. Does not remove entries or mutate canonical state.
Builders must skip remaining transactions of a lane when one cannot execute or fit."
  (let ((heap (make-array (max 1 (+ (hash-table-count (pool-lanes pool))
                                    (hash-table-count (pool-replays pool))))
                          :adjustable t :fill-pointer 0))
        (result nil))
    (maphash (lambda (key lane)
               (declare (ignore key))
               (when (and (= (entry-nonce (car lane)) (entry-state-nonce (car lane)))
                          (includable-p (car lane) timestamp base-fee))
                 (heap-push heap lane base-fee)))
             (pool-lanes pool))
    (maphash (lambda (hash entry)
               (declare (ignore hash))
               (when (includable-p entry timestamp base-fee)
                 (heap-push heap (list entry) base-fee)))
             (pool-replays pool))
    (loop while (plusp (fill-pointer heap))
          for lane = (heap-pop heap base-fee)
          for entry = (car lane)
          for next = (cdr lane)
          do (push (entry-tx entry) result)
             (when (and next (= (entry-nonce (car next)) (1+ (entry-nonce entry)))
                        (includable-p (car next) timestamp base-fee))
               (heap-push heap next base-fee)))
    (nreverse result)))

(defun pool-revalidate (pool reader &key timestamp fork)
  "Recheck every entry against a canonical snapshot, refreshing resolved fees, keys and
lane nonces; removes mined/stale/replayed/revoked/insolvent/expired entries. Return a
submission-ordered list of (32-OCTET-HASH . REASON) removals. Provider failures abort
before ANY mutation, so an unavailable backend never silently discards or approves TXs."
  (multiple-value-bind (tip spec base-fee) (snapshot-context reader timestamp fork)
    (let ((valid nil) (invalid nil))
      (dolist (old (sort (loop for entry being the hash-values of (pool-entries pool) collect entry)
                        #'< :key #'entry-id))
        (handler-case
            (let ((new (validate-entry pool (entry-tx old) reader tip spec :admission-p nil)))
              (setf (entry-id new) (entry-id old))
              (push new valid))
          (pool-error (condition)
            (push (cons (clempo.transactions:transaction-hash (entry-tx old))
                        (pool-error-code condition)) invalid))))
      (clrhash (pool-entries pool)) (clrhash (pool-lanes pool))
      (clrhash (pool-transactions pool))
      (clrhash (pool-replays pool)) (clrhash (pool-authority-counts pool))
      (dolist (entry (nreverse valid)) (insert-entry pool entry))
      (setf (pool-timestamp pool) tip (pool-base-fee pool) base-fee)
      (nreverse invalid))))
