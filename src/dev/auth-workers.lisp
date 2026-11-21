(in-package #:clempo.dev)

;;; Signature recovery does not read execution state. A producer thread reads blocks ahead
;;; of the replay and recovers their senders on a bounded worker pool while the replay
;;; thread executes the block before; transaction execution stays serial and ordered.

(defconstant +default-sender-threads+ 8)
(defconstant +pipeline-depth+ 4 "Blocks the producer may read and recover ahead.")

(defun recover-senders (block mapper batches threads)
  (let* ((txs (clempo.blocks:block-transactions block))
         (n (length txs)))
    (if (< n 4)
        (mapcar #'clempo.transactions:transaction-sender txs)
        (let ((transactions (coerce txs 'simple-vector))
              (workers (min n threads))
              (out (make-array n)))
          (multiple-value-bind (size extra) (floor n workers)
            (funcall mapper
                     (lambda (worker)
                       (let ((start (+ (* worker size) (min worker extra))))
                         (loop for i from start below (+ start size (if (< worker extra) 1 0))
                               do (setf (svref out i)
                                        (clempo.transactions:transaction-sender (svref transactions i))))))
                     (svref batches workers)))
          (coerce out 'list)))))

(defun call-with-recovery-scratch (function)
  "Call FUNCTION with this thread's own signature arenas; they are never shared."
  (let ((clempo.secp256k1:*recovery-scratch* (clempo.secp256k1:make-recovery-scratch))
        (clempo.p256:*p256-scratch* (clempo.p256:make-p256-scratch)))
    (funcall function)))

(defstruct (prefetched (:constructor make-prefetched (number block senders failure)))
  number block senders failure)

(defstruct (block-pipeline (:conc-name pipeline-))
  (lock (sb-thread:make-mutex :name "block pipeline"))
  (ready (sb-thread:make-waitqueue))    ; an entry was queued or the producer stopped
  (room (sb-thread:make-waitqueue))     ; an entry was taken or the pipeline is closing
  (queue '() :type list)                ; PREFETCHED entries, oldest first
  (count 0 :type fixnum)
  next                                  ; next number to produce; NIL until the first request
  (done nil)                            ; the producer has exited
  (stopped nil)                         ; the consumer is finished
  (last nil))                           ; the entry handed out most recently

(defun pipeline-produce (pipeline source recover)
  (loop
    (let ((n (sb-thread:with-mutex ((pipeline-lock pipeline))
               (loop until (or (pipeline-stopped pipeline)
                               (and (pipeline-next pipeline)
                                    (< (pipeline-count pipeline) +pipeline-depth+)))
                     do (sb-thread:condition-wait (pipeline-room pipeline) (pipeline-lock pipeline)))
               (unless (pipeline-stopped pipeline)
                 (prog1 (pipeline-next pipeline) (incf (pipeline-next pipeline)))))))
      (unless n (return))
      (let ((entry (handler-case (let ((block (funcall source n)))
                                   (make-prefetched n block (funcall recover block) nil))
                     (error (c) (make-prefetched n nil nil c)))))
        (sb-thread:with-mutex ((pipeline-lock pipeline))
          (setf (pipeline-queue pipeline) (nconc (pipeline-queue pipeline) (list entry)))
          (incf (pipeline-count pipeline))
          (sb-thread:condition-broadcast (pipeline-ready pipeline)))
        ;; A failed read ends the run of numbers the producer can serve (the archive's end).
        (when (prefetched-failure entry) (return))))))

(defun pipeline-take (pipeline n)
  "The prefetched entry for block N, or NIL when the producer cannot serve N in order."
  (sb-thread:with-mutex ((pipeline-lock pipeline))
    (unless (pipeline-next pipeline)
      (setf (pipeline-next pipeline) n)
      (sb-thread:condition-broadcast (pipeline-room pipeline)))
    (loop
      (let ((head (first (pipeline-queue pipeline))))
        (cond ((and head (= (prefetched-number head) n))
               (pop (pipeline-queue pipeline))
               (decf (pipeline-count pipeline))
               (sb-thread:condition-broadcast (pipeline-room pipeline))
               (return head))
              ((and head (< (prefetched-number head) n))
               ;; the consumer skipped ahead: drop what it will never ask for
               (pop (pipeline-queue pipeline))
               (decf (pipeline-count pipeline))
               (sb-thread:condition-broadcast (pipeline-room pipeline)))
              ((or head (pipeline-done pipeline)) (return nil))
              (t (sb-thread:condition-wait (pipeline-ready pipeline) (pipeline-lock pipeline))))))))

(defun call-with-block-pipeline (function source threads)
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
                       (lambda (n) (sb-thread:with-mutex (lock) (funcall source n)))))
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
                                 (pipeline-produce pipeline source
                                                   (lambda (block) (recover-senders block mapper batches threads)))))
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
