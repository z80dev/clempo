(defpackage #:clempo.lock.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:lock #:clempo.lock)))

(in-package #:clempo.lock.tests)

;;; Each test names its mutexes after itself: a lock class is its name, and the order
;;; graph lives as long as the process.

(defun violation-of (thunk)
  "The LOCK-ORDER-VIOLATION THUNK signals (continued past), or NIL."
  (let ((seen nil))
    (handler-bind ((lock:lock-order-violation
                     (lambda (c) (setf seen c) (invoke-restart 'continue))))
      (funcall thunk))
    seen))

(deftest lock-order-cycle-is-signalled-before-it-deadlocks ()
  (if (eq *build-mode* :release)
      (skip-test "the lock-order checker is compiled into check builds only")
      (let ((a (sb-thread:make-mutex :name "lock-tests cycle a"))
            (b (sb-thread:make-mutex :name "lock-tests cycle b"))
            (c (sb-thread:make-mutex :name "lock-tests cycle c")))
        ;; a -> b and b -> c on one thread, then c -> a on another: no deadlock happened,
        ;; but the orders admit one.
        (is (null (violation-of (lambda () (lock:with-lock (a) (lock:with-lock (b) (lock:with-lock (c))))))))
        (let ((v (sb-thread:join-thread
                  (sb-thread:make-thread
                   (lambda () (violation-of (lambda () (lock:with-lock (c) (lock:with-lock (a)))))))
                  :default nil)))
          (is v)
          (is (equal '("lock-tests cycle c" "lock-tests cycle a" "lock-tests cycle b" "lock-tests cycle c")
                     (and v (lock:lock-order-violation-cycle v)))))
        ;; recorded for the test audit, once, and taken here so this test passes
        (let ((recorded (lock:take-lock-order-violations)))
          (is= 1 (length recorded))
          (is (search "lock-tests cycle a" (first recorded))))
        (is (null (violation-of (lambda () (lock:with-lock (c) (lock:with-lock (a)))))) "reported once")
        (is (null (lock:take-lock-order-violations))))))

(deftest lock-order-flags-two-locks-of-one-class ()
  (if (eq *build-mode* :release)
      (skip-test "the lock-order checker is compiled into check builds only")
      (let ((x (sb-thread:make-mutex :name "lock-tests same class"))
            (y (sb-thread:make-mutex :name "lock-tests same class")))
        (is (violation-of (lambda () (lock:with-lock (x) (lock:with-lock (y))))))
        (is= 1 (length (lock:take-lock-order-violations))))))

(deftest recursive-lock-reentry-adds-no-order ()
  (let ((m (sb-thread:make-mutex :name "lock-tests recursive")))
    (is (null (violation-of (lambda ()
                              (lock:with-recursive-lock (m)
                                (lock:with-recursive-lock (m)
                                  (lock:with-lock ((sb-thread:make-mutex :name "lock-tests inner")))))))))
    (is (null (lock:take-lock-order-violations)))))

(deftest condition-wait-returns-holding-the-mutex ()
  (let ((m (sb-thread:make-mutex :name "lock-tests wait"))
        (q (sb-thread:make-waitqueue :name "lock-tests wait")))
    (lock:with-lock (m)
      (is (null (lock:condition-wait q m :timeout 0.01)) "timed out")
      (is (sb-thread:holding-mutex-p m)))))
