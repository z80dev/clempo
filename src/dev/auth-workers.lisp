(in-package #:clempo.dev)

;;; Signature recovery does not read execution state. A producer thread reads blocks ahead
;;; of the replay and recovers their senders on a bounded worker pool while the replay
;;; thread executes the block before; transaction execution stays serial and ordered.

(defconstant +default-sender-threads+ 8)
(defconstant +pipeline-depth+ 1024
  "Weight (PREFETCHED-WEIGHT: blocks plus transactions) the producer may read and recover ahead.")
(defconstant +pipeline-batch+ 128
  "Most blocks the producer reads, decodes and recovers in one parallel batch: waking the
recovery workers costs as much as decoding dozens of empty blocks.")
(defconstant +pipeline-poll+ 0.0002
  "Seconds a full producer sleeps before it looks at the queue again.")

(defun recover-sender (tx)
  "TX's sender. A Tempo transaction's unique identifier (its expiring-nonce and channel key)
is derived here too and memoized on TX, off the replay thread."
  (let ((sender (clempo.transactions:transaction-sender tx)))
    (when (= (clempo.transactions:transaction-type tx) #x76)
      (clempo.transactions:transaction-unique-identifier tx sender))
    sender))

(defconstant +parallel-recovery-txs+ 4
  "Blocks with at least this many transactions have their senders recovered in parallel.")

(defun recover-senders (block mapper batches threads)
  (let* ((txs (clempo.blocks:block-transactions block))
         (n (length txs)))
    (if (< n +parallel-recovery-txs+)
        (mapcar #'recover-sender txs)
        (let ((transactions (coerce txs 'simple-vector))
              (workers (min n threads))
              (out (make-array n)))
          (multiple-value-bind (size extra) (floor n workers)
            (funcall mapper
                     (lambda (worker)
                       (let ((start (+ (* worker size) (min worker extra))))
                         (loop for i from start below (+ start size (if (< worker extra) 1 0))
                               do (setf (svref out i)
                                        (recover-sender (svref transactions i))))))
                     (svref batches workers)))
          (coerce out 'list)))))

(defun call-with-recovery-scratch (function)
  "Call FUNCTION with this thread's own signature arenas; they are never shared."
  (let ((clempo.secp256k1:*recovery-scratch* (clempo.secp256k1:make-recovery-scratch))
        (clempo.p256:*p256-scratch* (clempo.p256:make-p256-scratch))
        (clempo.p384:*p384-scratch* (clempo.p384:make-p384-scratch :memoize t)))
    (funcall function)))

(defstruct (prefetched (:constructor make-prefetched (number block senders failure)))
  number block senders failure)

(defun prefetched-weight (entry)
  "An entry's share of the pipeline's depth: one for the block and one per transaction, so
the queue holds thousands of empty blocks but only a few hundred busy ones."
  (let ((block (prefetched-block entry)))
    (if block (1+ (length (clempo.blocks:block-transactions block))) 1)))

;;; The replay thread never wakes the producer: on Darwin waking a sleeping thread costs the
;;; waker tens of microseconds, as much as executing several empty blocks, and early mainnet
;;; is millions of them. A full producer instead polls every +PIPELINE-POLL+ seconds; only
;;; the first request and the end of the run signal it. The producer wakes the replay
;;; thread only when it is actually waiting, once per wait.
(defstruct (block-pipeline (:conc-name pipeline-))
  (lock (sb-thread:make-mutex :name "block pipeline"))
  (ready (sb-thread:make-waitqueue))    ; an entry was queued or the producer stopped
  (room (sb-thread:make-waitqueue))     ; the first request was made or the pipeline is closing
  (queue '() :type list)                ; PREFETCHED entries, oldest first
  (tail '() :type list)                 ; the queue's last cons
  (count 0 :type fixnum)                ; the queued entries' PREFETCHED-WEIGHTs
  next                                  ; next number to produce; NIL until the first request
  (consumer-waiting nil)
  (done nil)                            ; the producer has exited
  (stopped nil)                         ; the consumer is finished
  (taken '() :type list)                ; entries the consumer moved out of QUEUE, oldest first
  (last nil))                           ; the entry handed out most recently

(defun pipeline-claim (pipeline weight)
  "Block numbers for the producer's next batch, oldest first: as many as fit the free depth
at WEIGHT per block, at most +PIPELINE-BATCH+ and at least one; NIL once the consumer has
stopped. Sleeps until at least half the depth is free."
  (loop
    (let ((claim (sb-thread:with-mutex ((pipeline-lock pipeline))
                   (loop until (or (pipeline-stopped pipeline) (pipeline-next pipeline))
                         do (sb-thread:condition-wait (pipeline-room pipeline) (pipeline-lock pipeline)))
                   (let ((free (- +pipeline-depth+ (pipeline-count pipeline))))
                     (cond ((pipeline-stopped pipeline) nil)
                           ;; at least half free: a full producer refills in large batches
                           ((>= (* 2 free) +pipeline-depth+)
                            (let ((first (pipeline-next pipeline))
                                  (k (max 1 (min +pipeline-batch+ (floor free weight)))))
                              (incf (pipeline-next pipeline) k)
                              (loop for n from first below (+ first k) collect n)))
                           (t :full))))))
      (if (eq claim :full) (sleep +pipeline-poll+) (return claim)))))

(defun pipeline-publish (pipeline entries)
  "Append ENTRIES (oldest first) to PIPELINE's queue; returns their total weight."
  (let ((cells (copy-list entries))
        (total (reduce #'+ entries :key #'prefetched-weight)))
    (sb-thread:with-mutex ((pipeline-lock pipeline))
      (if (pipeline-queue pipeline)
          (setf (cdr (pipeline-tail pipeline)) cells)
          (setf (pipeline-queue pipeline) cells))
      (setf (pipeline-tail pipeline) (last cells))
      (incf (pipeline-count pipeline) total)
      (when (pipeline-consumer-waiting pipeline)
        ;; cleared here, not by the waiter: until it runs, later entries must not wake it again
        (setf (pipeline-consumer-waiting pipeline) nil)
        (sb-thread:condition-broadcast (pipeline-ready pipeline))))
    total))

(defun pipeline-produce (pipeline prepare)
  "Fill PIPELINE in batches: PREPARE is called with a list of consecutive block numbers and
a function to publish their PREFETCHED entries with, oldest first, in as many calls as it
likes; it returns NIL once a failure was published (the end of what the producer can serve)."
  (let ((weight 1))
    (loop
      (let ((numbers (pipeline-claim pipeline weight))
            (blocks 0) (total 0))
        (unless numbers (return))
        (unless (funcall prepare numbers
                         (lambda (entries)
                           (when entries
                             (incf blocks (length entries))
                             (incf total (pipeline-publish pipeline entries)))))
          (return))
        (setf weight (max 1 (ceiling total (max blocks 1))))))))

(defun prepare-blocks (numbers chunk-fn mapper threads batches publish)
  "Read, decode, hash and recover NUMBERS (consecutive), publishing their PREFETCHED entries
in order; NIL after publishing a failure. CHUNK-FN maps a number to the archive chunk holding
it (on this thread: the chunk cache is not shared). The THREADS participants first decode
the batch a block at a time, recovering the senders of blocks with few transactions with
it; then each block with more is recovered on all participants and published at once, so
the replay starts on it while the next is recovered."
  (let* ((count (length numbers))
         (first (first numbers))
         (chunks (make-array count :initial-element nil))
         (out (make-array count :initial-element nil))
         (readable count)
         (pending '()))
    (loop for n in numbers for i from 0
          do (handler-case (setf (svref chunks i) (funcall chunk-fn n))
               (error (c)
                 (setf (svref out i) (make-prefetched n nil nil c) readable i)
                 (loop-finish))))
    (flet ((decode (i)
             (let ((n (+ first i)))
               (setf (svref out i)
                     (handler-case
                         (let* ((block (clempo.blocks:decode-block
                                        (clempo.archive:chunk-block (svref chunks i) n)))
                                (txs (clempo.blocks:block-transactions block)))
                           ;; memoized on BLOCK: the replay thread commits under it
                           (clempo.blocks:block-hash block)
                           (make-prefetched n block
                                            (if (< (length txs) +parallel-recovery-txs+)
                                                (mapcar #'recover-sender txs)
                                                :later)
                                            nil))
                       (error (c) (make-prefetched n nil nil c))))))
           (flush ()
             (funcall publish (nreverse pending))
             (setf pending '())))
      (if (<= readable 1)
          (when (= readable 1) (decode 0))
          (let ((workers (min readable threads)))
            (funcall mapper
                     (lambda (worker)
                       (loop for i from worker below readable by workers do (decode i)))
                     (svref batches workers))))
      (loop for i below (min count (1+ readable))
            for entry = (svref out i)
            do (if (eq (prefetched-senders entry) :later)
                   (progn
                     (flush)
                     (setf (prefetched-senders entry)
                           (handler-case (recover-senders (prefetched-block entry) mapper batches threads)
                             (error (c) (setf (prefetched-failure entry) c) nil)))
                     (push entry pending)
                     (flush))
                   (push entry pending))
               (when (prefetched-failure entry)
                 (flush)
                 (return-from prepare-blocks nil)))
      (flush)
      t)))

(defun pipeline-take (pipeline n)
  "The prefetched entry for block N, or NIL when the producer cannot serve N in order. The
consumer moves the whole queue to its own list at once, so most blocks take no lock."
  (loop
    (let ((head (first (pipeline-taken pipeline))))
      (cond ((null head)
             (sb-thread:with-mutex ((pipeline-lock pipeline))
               (unless (pipeline-next pipeline)
                 (setf (pipeline-next pipeline) n)
                 (sb-thread:condition-broadcast (pipeline-room pipeline)))
               (loop until (or (pipeline-queue pipeline) (pipeline-done pipeline))
                     do (setf (pipeline-consumer-waiting pipeline) t)
                        (sb-thread:condition-wait (pipeline-ready pipeline) (pipeline-lock pipeline))
                        (setf (pipeline-consumer-waiting pipeline) nil))
               (unless (pipeline-queue pipeline) (return nil))
               (setf (pipeline-taken pipeline) (shiftf (pipeline-queue pipeline) '())
                     (pipeline-tail pipeline) '()
                     (pipeline-count pipeline) 0)))
            ((= (prefetched-number head) n)
             (return (pop (pipeline-taken pipeline))))
            ((< (prefetched-number head) n)
             ;; the consumer skipped ahead: drop what it will never ask for
             (pop (pipeline-taken pipeline)))
            (t (return nil))))))

(defun call-with-replay-nursery (function)
  "Call FUNCTION with the allocation between collections raised to an eighth of the heap,
at most 2 GiB: a replay conses ~1 MB per block, mostly garbage by its end, and fewer, larger
nursery collections cost less in total (Busy: 1.2 s of GC instead of 1.8 s, peak RSS 3.1 GB)."
  (let ((old (sb-ext:bytes-consed-between-gcs)))
    (setf (sb-ext:bytes-consed-between-gcs)
          (max old (min (ash 2 30) (floor (sb-ext:dynamic-space-size) 8))))
    (unwind-protect (funcall function)
      ;; SBCL applies a raised nursery only from the next collection, but lowering it always
      ;; pulls the pending trigger down, which underflows (a TYPE-ERROR) when no collection
      ;; ran since the raise; the trigger is then still the old size's, so restore only the size.
      (handler-case (setf (sb-ext:bytes-consed-between-gcs) old)
        (type-error ()
          (setf (sb-alien:extern-alien "bytes_consed_between_gcs" sb-alien:unsigned-long) old))))))

(defconstant +state-root-threads+ 4
  "Participants (the replay thread included) building a large storage trie in bulk.")

(defun call-with-block-pipeline (function source threads)
  "As %CALL-WITH-BLOCK-PIPELINE, with the replay's nursery size and a trie-hashing pool:
a storage trie a checkpoint load left unbuilt is built on its first write, in parallel
when large (CLEMPO.TRIE:TRIE-FROM-ITEMS)."
  (call-with-replay-nursery
   (lambda ()
     (clempo.node.workers:call-with-workers
      (lambda (mapper)
        (clempo.trie:call-with-trie-hashing
         (lambda () (%call-with-block-pipeline function source threads))
         :parallel-map mapper))
      :threads +state-root-threads+ :name "state root"))))

(defun %call-with-block-pipeline (function source threads)
  "Call FUNCTION with (block-fn senders-fn) for CLEMPO.EXECUTOR:REPLAY-BLOCKS. SOURCE maps a
block number to its decoded block. With THREADS above 1, a producer thread reads the blocks
following the first one requested and recovers their senders on THREADS participants (itself
and THREADS - 1 workers), up to +PIPELINE-DEPTH+ blocks ahead, so recovery overlaps execution.
A block requested out of that order (a retried or earlier block) is read and recovered on the
calling thread. With 1 thread, blocks are read and senders recovered serially on demand."
  (if (= threads 1)
      (call-with-recovery-scratch (lambda () (funcall function source nil)))
      (let* ((pipeline (make-block-pipeline))
             (archive-dir clempo.archive:*archive-dir*)
             ;; the producer and an out-of-order request share SOURCE's chunk cache
             (source (let ((lock (sb-thread:make-mutex :name "block source")))
                       (lambda (n &optional mode) (sb-thread:with-mutex (lock) (funcall source n mode)))))
             (producer
               (sb-thread:make-thread
                (lambda ()
                  (let ((clempo.archive:*archive-dir* archive-dir))
                    (unwind-protect
                         (call-with-recovery-scratch
                          (lambda ()
                            (clempo.node.workers:call-with-workers
                             (lambda (mapper)
                               (let ((batches (make-array (1+ threads))))
                                 (dotimes (n (1+ threads))
                                   (setf (svref batches n) (loop for i below n collect i)))
                                 (pipeline-produce
                                  pipeline
                                  (lambda (numbers publish)
                                    (prepare-blocks numbers
                                                    (lambda (n) (funcall source n :chunk))
                                                    mapper threads batches publish)))))
                             :threads threads :name "transaction sender"
                             :worker-context #'call-with-recovery-scratch)))
                      (sb-thread:with-mutex ((pipeline-lock pipeline))
                        (setf (pipeline-done pipeline) t)
                        (sb-thread:condition-broadcast (pipeline-ready pipeline))))))
                :name "block prefetch")))
        (flet ((block-fn (n)
                 (let ((last (pipeline-last pipeline)))
                   (if (and last (= (prefetched-number last) n))
                       (prefetched-block last)
                       (let ((entry (or (pipeline-take pipeline n)
                                        (make-prefetched n (funcall source n) nil nil))))
                         (when (prefetched-failure entry) (error (prefetched-failure entry)))
                         (setf (pipeline-last pipeline) entry)
                         (prefetched-block entry)))))
               (senders-fn (block)
                 (let ((last (pipeline-last pipeline)))
                   (or (and last (eq block (prefetched-block last)) (prefetched-senders last))
                       (mapcar #'clempo.transactions:transaction-sender
                               (clempo.blocks:block-transactions block))))))
          (unwind-protect (call-with-recovery-scratch
                           (lambda () (funcall function #'block-fn #'senders-fn)))
            (sb-thread:with-mutex ((pipeline-lock pipeline))
              (setf (pipeline-stopped pipeline) t)
              (sb-thread:condition-broadcast (pipeline-room pipeline)))
            (sb-thread:join-thread producer :default nil))))))
