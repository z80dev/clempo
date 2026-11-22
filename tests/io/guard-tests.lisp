(defpackage #:clempo.guard.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:guard #:clempo.guard) (#:lock #:clempo.lock)))

(in-package #:clempo.guard.tests)

(guard:define-shared-struct (guarded-box (:constructor make-guarded-box ()))
  (lock (sb-thread:make-mutex :name "guard-tests box") :guard :immutable)
  (count 0 :type fixnum :guard (:lock lock))
  (latest nil :guard (:lock-writes lock))
  (state 0 :guard :atomic)
  (owner nil :guard (:unchecked "set once by the test before any other thread sees the box"))
  (mine nil :guard (:thread (box) (guarded-box-owner box))))

(defclass guarded-counter ()
  ((lock :initform (sb-thread:make-mutex :name "guard-tests counter") :reader counter-lock)
   (value :initform 0 :accessor counter-value)))

(guard:define-slot-guards guarded-counter
  (lock :immutable)
  (value (:lock lock)))

(defun violations-of (thunk)
  "The places whose guards THUNK violated (each access continued past), taken from the audit."
  (let ((seen '()))
    (handler-bind ((guard:guard-violation
                     (lambda (c)
                       (push (list (guard:guard-violation-place c) (guard:guard-violation-access c)) seen)
                       (invoke-restart 'continue))))
      (funcall thunk))
    (guard:take-guard-violations)
    (reverse seen)))

(defmacro checked-only (&body body)
  `(if (eq *build-mode* :release)
       (skip-test "guards are checked in check builds only")
       (progn ,@body)))

(deftest guarded-slots-need-their-lock ()
  (checked-only
    (let ((box (make-guarded-box)))
      (is (null (violations-of (lambda ()
                                 (lock:with-lock ((guarded-box-lock box))
                                   (incf (guarded-box-count box))
                                   (setf (guarded-box-latest box) :a))
                                 (guarded-box-latest box)))))
      (is (equal '(((guarded-box count) :read) ((guarded-box latest) :write))
                 (violations-of (lambda ()
                                  (guarded-box-count box)
                                  (setf (guarded-box-latest box) :b)))))
      ;; reported once per place and access
      (is (null (violations-of (lambda () (guarded-box-count box))))))))

(deftest atomic-slots-change-by-compare-and-swap ()
  (let ((box (make-guarded-box)))
    (is (eql 0 (sb-ext:compare-and-swap (guarded-box-state box) 0 1)))
    (is (eql 1 (guarded-box-state box)))
    (is (null (fboundp '(setf guarded-box-state))))
    (is (null (fboundp '(setf guarded-box-lock))) "an immutable slot has no writer")))

(deftest guarded-accessors-are-inline ()
  ;; DEFSTRUCT's accessors compile to the bare slot access; the guarded ones must too, or a
  ;; release build pays a full call per access (an atomic slot's CAS was one per server event)
  (dolist (name '(guarded-box-state (sb-ext:cas guarded-box-state)
                  guarded-box-count (setf guarded-box-count) guarded-box-lock))
    (is (eq 'inline (sb-int:info :function :inlinep name)) "~S is not inline" name)))

(deftest thread-confined-slots-refuse-other-threads ()
  (checked-only
    (let ((box (make-guarded-box)))
      (setf (guarded-box-owner box) sb-thread:*current-thread*)
      (is (null (violations-of (lambda () (setf (guarded-box-mine box) 1)))))
      (is (equal '(((guarded-box mine) :read))
                 (sb-thread:join-thread
                  (sb-thread:make-thread (lambda () (violations-of (lambda () (guarded-box-mine box)))))))))))

(deftest class-slot-guards-wrap-accessors ()
  (checked-only
    (let ((counter (make-instance 'guarded-counter)))
      (is (null (violations-of (lambda ()
                                 (lock:with-lock ((counter-lock counter))
                                   (incf (counter-value counter)))))))
      (is (equal '(((guarded-counter value) :write))
                 (violations-of (lambda () (setf (counter-value counter) 5)))))
      (is (= 5 (lock:with-lock ((counter-lock counter)) (counter-value counter)))))))

(deftest with-held-checks-its-callers ()
  (checked-only
    (let ((m (sb-thread:make-mutex :name "guard-tests held")))
      (is (null (violations-of (lambda () (lock:with-lock (m) (guard:with-held (m) t))))))
      (is (= 1 (length (violations-of (lambda () (guard:with-held (m) t)))))))))
