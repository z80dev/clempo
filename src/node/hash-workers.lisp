(defpackage #:clempo.node.hash-workers
  (:use #:cl #:clempo.base)
  (:export #:call-with-root-workers))

(in-package #:clempo.node.hash-workers)

(defstruct hash-workers
  (lock (sb-thread:make-mutex :name "trie hash work"))
  (work (sb-thread:make-waitqueue))
  (done (sb-thread:make-waitqueue))
  (remaining 0 :type fixnum)
  jobs function failure stopped)

(defun claim-job (workers wait)
  (sb-thread:with-mutex ((hash-workers-lock workers))
    (when wait
      (loop until (or (hash-workers-stopped workers) (hash-workers-jobs workers))
            do (sb-thread:condition-wait (hash-workers-work workers) (hash-workers-lock workers))))
    (when (and (not (hash-workers-stopped workers)) (hash-workers-jobs workers))
      (values (pop (hash-workers-jobs workers)) (hash-workers-function workers)))))

(defun run-job (workers job fn)
  (unwind-protect
       (handler-case (funcall fn job)
         (error (c)
           (sb-thread:with-mutex ((hash-workers-lock workers))
             (unless (hash-workers-failure workers) (setf (hash-workers-failure workers) c)))))
    (sb-thread:with-mutex ((hash-workers-lock workers))
      (when (zerop (decf (hash-workers-remaining workers)))
        (sb-thread:condition-notify (hash-workers-done workers))))))

(defun hash-worker (workers)
  (loop (multiple-value-bind (job fn) (claim-job workers t)
          (unless fn (return))
          (run-job workers job fn))))

(defun map-root-jobs (workers function jobs)
  (sb-thread:with-mutex ((hash-workers-lock workers))
    (invariant (zerop (hash-workers-remaining workers)) "hash worker pool already in use")
    (setf (hash-workers-jobs workers) jobs
          (hash-workers-function workers) function
          (hash-workers-failure workers) nil
          (hash-workers-remaining workers) (length jobs))
    (sb-thread:condition-broadcast (hash-workers-work workers)))
  ;; The caller hashes as well, so THREADS is the total participant count.
  (loop (multiple-value-bind (job fn) (claim-job workers nil)
          (unless fn (return))
          (run-job workers job fn)))
  (let ((failure
          (sb-thread:with-mutex ((hash-workers-lock workers))
            (loop until (zerop (hash-workers-remaining workers))
                  do (sb-thread:condition-wait (hash-workers-done workers) (hash-workers-lock workers)))
            (prog1 (hash-workers-failure workers)
              (setf (hash-workers-failure workers) nil (hash-workers-function workers) nil)))))
    (when failure (error failure)))
  nil)

(defun call-with-root-workers (function &key (threads 4))
  "Call FUNCTION with a synchronous parallel mapper (function, jobs), reusable
for the dynamic extent of this call. THREADS includes the caller; 1 passes NIL
for the serial trie path. Only the calling thread may submit batches."
  (check-type threads (integer 1 16))
  (if (= threads 1)
      (funcall function nil)
      (let ((workers (make-hash-workers)) (pool nil))
        (unwind-protect
             (progn
               (dotimes (i (1- threads))
                 (push (sb-thread:make-thread (lambda () (hash-worker workers)) :name "trie subtree hash")
                       pool))
               (funcall function (lambda (fn jobs) (map-root-jobs workers fn jobs))))
          (sb-thread:with-mutex ((hash-workers-lock workers))
            (setf (hash-workers-stopped workers) t)
            (sb-thread:condition-broadcast (hash-workers-work workers)))
          (mapc #'sb-thread:join-thread pool)))))
