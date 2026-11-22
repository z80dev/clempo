(defpackage #:clempo.lock
  (:use #:cl #:clempo.base)
  (:documentation "Every mutex acquisition in clempo goes through WITH-LOCK, WITH-RECURSIVE-LOCK
and CONDITION-WAIT. Release builds expand them into SB-THREAD's forms. Check builds also
run a lock-order checker, as Linux's lockdep does: they record, per thread, which locks
are held when another is taken, keep the order seen between lock classes in one graph,
and signal LOCK-ORDER-VIOLATION when an acquisition would close a cycle, which is a
deadlock waiting for the wrong interleaving whether or not this run hit it.")
  (:export #:with-lock #:with-recursive-lock #:condition-wait
           #:lock-order-violation #:lock-order-violation-cycle
           #:take-lock-order-violations #:lock-order-summary))

(in-package #:clempo.lock)

;;; A lock's class is its mutex name, so every connection's "validator conn" mutex is
;;; one node of the graph: an order seen between two locks stands for every pair of
;;; locks of those classes. A mutex without a name is a class of its own.

(defun lock-class (mutex)
  (or (sb-thread:mutex-name mutex) mutex))

(defvar *held* '()
  "Mutexes this thread holds through WITH-LOCK, innermost first. Only ever rebound, so
each thread starts with none.")

(defvar *graph-lock* (sb-thread:make-mutex :name "lock order graph")
  "Guards the graph below; the checker's own lock is outside the graph.")

(defvar *order* (make-hash-table :test 'equal)
  "Class -> table of the classes taken while holding it -> the first thread that did.")

(defvar *violations* '()
  "Descriptions of violations not yet taken by TAKE-LOCK-ORDER-VIOLATIONS.")

(defvar *reported* (make-hash-table :test 'equal)
  "(holding . acquiring) pairs already signalled, so a hot path reports a cycle once.")

(define-condition lock-order-violation (error)
  ((thread :initarg :thread :reader lock-order-violation-thread)
   (cycle :initarg :cycle :reader lock-order-violation-cycle
          :documentation "Classes from the held lock through the acquired one back to it."))
  (:report (lambda (c s)
             (let ((cycle (lock-order-violation-cycle c)))
               (format s "lock order violation in thread ~S: acquiring ~S while holding ~S~:[, ~
                          but the order ~{~S~^ -> ~} was already seen~;, a second lock of the same class~]"
                       (lock-order-violation-thread c) (second cycle) (first cycle)
                       (equal (first cycle) (second cycle)) (rest cycle))))))

(defun order-path (from to)
  "Classes from FROM to TO along recorded edges, both ends included, or NIL."
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((walk (class)
               (cond ((equal class to) (list class))
                     ((gethash class seen) nil)
                     (t (setf (gethash class seen) t)
                        (let ((next (gethash class *order*)))
                          (when next
                            (loop for successor being the hash-keys of next
                                  for path = (walk successor)
                                  when path return (cons class path))))))))
      (walk from))))

(defun note-acquire (mutex)
  "Check taking MUTEX against the locks this thread holds and record the new orders.
Signal LOCK-ORDER-VIOLATION, with a CONTINUE restart, for each order that closes a cycle."
  (let ((class (lock-class mutex))
        (thread (sb-thread:thread-name sb-thread:*current-thread*))
        (cycles '()))
    (sb-thread:with-mutex (*graph-lock*)
      (dolist (held *held*)
        (let* ((before (lock-class held))
               (edges (or (gethash before *order*)
                          (setf (gethash before *order*) (make-hash-table :test 'equal)))))
          (unless (nth-value 1 (gethash class edges))
            (let ((back (if (equal before class) (list class) (order-path class before))))
              (cond ((null back) (setf (gethash class edges) thread))
                    ((not (gethash (cons before class) *reported*))
                     (setf (gethash (cons before class) *reported*) t)
                     (let ((c (make-condition 'lock-order-violation
                                              :thread thread :cycle (cons before back))))
                       (push (princ-to-string c) *violations*)
                       (push c cycles)))))))))
    (dolist (c (nreverse cycles))
      (restart-case (error c)
        (continue () :report "Acquire the lock anyway.")))))

(defun take-lock-order-violations ()
  "The violations seen since the last call, oldest first."
  (sb-thread:with-mutex (*graph-lock*)
    (reverse (shiftf *violations* '()))))

(defun lock-order-summary ()
  "(values classes orders): the size of the order graph seen so far."
  (sb-thread:with-mutex (*graph-lock*)
    (let ((classes (make-hash-table :test 'equal)) (orders 0))
      (loop for before being the hash-keys of *order* using (hash-value edges)
            do (setf (gethash before classes) t)
               (loop for after being the hash-keys of edges
                     do (setf (gethash after classes) t) (incf orders)))
      (values (hash-table-count classes) orders))))

(pushnew 'take-lock-order-violations *test-audits*)

(defun checked-p () (not (eq *build-mode* :release)))

(defmacro with-lock ((mutex) &body body)
  "Run BODY holding MUTEX (SB-THREAD:WITH-MUTEX), checked against the lock order in
check builds."
  (if (checked-p)
      (let ((m (gensym "MUTEX")))
        `(let ((,m ,mutex))
           (note-acquire ,m)
           (sb-thread:with-mutex (,m)
             (let ((*held* (cons ,m *held*)))
               ,@body))))
      `(sb-thread:with-mutex (,mutex) ,@body)))

(defmacro with-recursive-lock ((mutex) &body body)
  "Run BODY holding MUTEX, which this thread may already hold (SB-THREAD:WITH-RECURSIVE-LOCK)."
  (if (checked-p)
      (let ((m (gensym "MUTEX")) (f (gensym "BODY")))
        `(let ((,m ,mutex))
           (flet ((,f () ,@body))
             (if (sb-thread:holding-mutex-p ,m)
                 (,f)
                 (progn
                   (note-acquire ,m)
                   (sb-thread:with-mutex (,m)
                     (let ((*held* (cons ,m *held*)))
                       (,f))))))))
      `(sb-thread:with-recursive-lock (,mutex) ,@body)))

(defun condition-wait (queue mutex &key timeout)
  "Release MUTEX, wait on QUEUE for a notification or TIMEOUT seconds, and return
holding MUTEX again: true when notified, NIL when the wait timed out. SBCL's own
CONDITION-WAIT may return from a timeout without the mutex; this takes it back."
  (or (sb-thread:condition-wait queue mutex :timeout timeout)
      (progn
        (unless (sb-thread:holding-mutex-p mutex) (sb-thread:grab-mutex mutex))
        nil)))
