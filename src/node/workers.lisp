(defpackage #:clempo.node.workers
  (:use #:cl #:clempo.base)
  (:export #:call-with-workers))

(in-package #:clempo.node.workers)

(defstruct worker-pool
  (lock (sb-thread:make-mutex :name "parallel work"))
  (work (sb-thread:make-waitqueue))
  (done (sb-thread:make-waitqueue))
  (remaining 0 :type fixnum)
  jobs function failure stopped)

(defun claim-job (pool wait)
  (sb-thread:with-mutex ((worker-pool-lock pool))
    (when wait
      (loop until (or (worker-pool-stopped pool) (worker-pool-jobs pool))
            do (sb-thread:condition-wait (worker-pool-work pool) (worker-pool-lock pool))))
    (when (and (not (worker-pool-stopped pool)) (worker-pool-jobs pool))
      (values (pop (worker-pool-jobs pool)) (worker-pool-function pool)))))

(defun run-job (pool job fn)
  (unwind-protect
       (handler-case (funcall fn job)
         (error (c)
           (sb-thread:with-mutex ((worker-pool-lock pool))
             (unless (worker-pool-failure pool) (setf (worker-pool-failure pool) c)))))
    (sb-thread:with-mutex ((worker-pool-lock pool))
      (when (zerop (decf (worker-pool-remaining pool)))
        (sb-thread:condition-notify (worker-pool-done pool))))))

(defun worker-loop (pool)
  (loop (multiple-value-bind (job fn) (claim-job pool t)
          (unless fn (return))
          (run-job pool job fn))))

(defun run-batch (pool function jobs)
  (let ((count (length jobs)))
    (invariant (<= count 16) "parallel batch exceeds 16 jobs")
    (sb-thread:with-mutex ((worker-pool-lock pool))
      (invariant (zerop (worker-pool-remaining pool)) "parallel worker pool already in use")
      (setf (worker-pool-jobs pool) jobs
            (worker-pool-function pool) function
            (worker-pool-failure pool) nil
            (worker-pool-remaining pool) count)
      (sb-thread:condition-broadcast (worker-pool-work pool))))
  ;; The caller works too, so THREADS is the total participant count.
  (loop (multiple-value-bind (job fn) (claim-job pool nil)
          (unless fn (return))
          (run-job pool job fn)))
  (let ((failure
          (sb-thread:with-mutex ((worker-pool-lock pool))
            (loop until (zerop (worker-pool-remaining pool))
                  do (sb-thread:condition-wait (worker-pool-done pool) (worker-pool-lock pool)))
            (prog1 (worker-pool-failure pool)
              (setf (worker-pool-failure pool) nil (worker-pool-function pool) nil)))))
    (when failure (error failure)))
  nil)

(defun call-with-workers (function &key (threads 4) (name "parallel work"))
  "Call FUNCTION with a synchronous mapper (function, jobs) for at most 16
jobs per batch. Only the calling thread may submit batches. THREADS includes
the caller; 1 passes NIL to select the caller's own serial path."
  (check-type threads (integer 1 16))
  (if (= threads 1)
      (funcall function nil)
      (let ((pool (make-worker-pool)) (workers nil))
        (unwind-protect
             (progn
               (dotimes (i (1- threads))
                 (push (sb-thread:make-thread (lambda () (worker-loop pool)) :name name) workers))
               (funcall function (lambda (fn jobs) (run-batch pool fn jobs))))
          (sb-thread:with-mutex ((worker-pool-lock pool))
            (setf (worker-pool-stopped pool) t)
            (sb-thread:condition-broadcast (worker-pool-work pool)))
          (mapc #'sb-thread:join-thread workers)))))
