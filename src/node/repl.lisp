(defpackage #:clempo.node.repl
  (:use #:cl)
  (:local-nicknames (#:socket #:sb-bsd-sockets))
  (:export #:start-repl #:stop-repl #:repl-port #:*auth-timeout* #:*idle-timeout*))

(in-package #:clempo.node.repl)

(defparameter *auth-timeout* 5 "Seconds a connection has to send its token.")
(defparameter *idle-timeout* 60 "Seconds an authenticated session may wait for its next form.")
(defparameter *max-clients* 8 "Concurrent sessions; further connections are refused.")

(clempo.guard:define-shared-struct (repl (:constructor %make-repl (listener port token configure)))
  (listener nil :guard :immutable)
  (port nil :guard :immutable)
  (token nil :guard :immutable)
  (configure nil :guard :immutable)
  (thread nil :guard (:lock-writes lock))      ; set once by START-REPL, joined by STOP-REPL
  (stopped nil :guard (:lock lock))
  (clients '() :guard (:lock lock))            ; (socket . thread) of live sessions
  (lock (sb-thread:make-mutex :name "node REPL") :guard :immutable))

(defun authenticated-p (stream token)
  ;; Read at most one fixed-length line, without invoking the Lisp reader.
  (let ((difference 0))
    (loop for expected across token
          for actual = (read-char stream nil)
          do (unless actual (return-from authenticated-p nil))
             (setf difference (logior difference (logxor (char-code expected) (char-code actual)))))
    (and (eql #\Newline (read-char stream nil)) (zerop difference))))

(defun eval-session (stream)
  (let* ((*package* (find-package :cl-user))
         (*terminal-io* (make-two-way-stream stream stream))
         (*standard-input* stream) (*standard-output* stream) (*error-output* stream)
         (*debug-io* *terminal-io*) (*query-io* *terminal-io*) (*trace-output* stream)
         (*read-eval* nil) (*print-circle* t))
    (loop
      (format stream "~&~A> " (package-name *package*))
      (finish-output stream)
      (let ((form (handler-case (read stream nil stream)
                    (reader-error (c)
                      (format stream "~&Reader error: ~A~%" c)
                      (return)))))
        (when (eq form stream) (return))
        (handler-case (dolist (value (multiple-value-list (eval form)))
                        (prin1 value stream) (terpri stream))
          (error (c) (format stream "~&~A~%" c)))))))

(defun serve-client (repl client)
  ;; The token must arrive within *AUTH-TIMEOUT*; after that each read waits at most
  ;; *IDLE-TIMEOUT*, so an idle or silent connection frees its slot.
  (let ((stream (socket:socket-make-stream client :input t :output t :external-format :utf-8
                                                  :buffering :line :timeout *auth-timeout*)))
    (with-open-stream (stream stream)
      (write-line "clempo Lisp REPL: send token" stream)
      (finish-output stream)
      (cond ((authenticated-p stream (repl-token repl))
             (setf (sb-impl::fd-stream-timeout stream) (coerce *idle-timeout* 'single-float))
             (funcall (repl-configure repl) (lambda () (eval-session stream))))
            (t (write-line "Access denied" stream))))))

(defun client-thread (repl client)
  (unwind-protect
       (handler-case (serve-client repl client)
         ((or stream-error socket:socket-error sb-ext:timeout sb-sys:interactive-interrupt) () nil))
    (clempo.lock:with-lock ((repl-lock repl))
      (setf (repl-clients repl) (remove client (repl-clients repl) :key #'car)))
    (ignore-errors (socket:socket-close client))))

(defun accept-client (repl client)
  (clempo.lock:with-lock ((repl-lock repl))
    (cond ((or (repl-stopped repl) (>= (length (repl-clients repl)) *max-clients*))
           (ignore-errors (socket:socket-close client)))
          (t (push (cons client (sb-thread:make-thread (lambda () (client-thread repl client))
                                                       :name "clempo Lisp REPL session"))
                   (repl-clients repl))))))

(defun stopped-p (repl)
  (clempo.lock:with-lock ((repl-lock repl)) (repl-stopped repl)))

(defun run-repl (repl)
  (let ((listener (repl-listener repl)))
    (unwind-protect
         (loop until (stopped-p repl)
               when (sb-sys:wait-until-fd-usable (socket:socket-file-descriptor listener) :input 0.2)
                 do (let ((client (ignore-errors (socket:socket-accept listener))))
                      (when client (accept-client repl client))))
      (socket:socket-close listener))))

(defun start-repl (port token &key (configure #'funcall))
  "Start a localhost-only debugger. TOKEN is a 32-byte hex secret, without 0x.
CONFIGURE is called on each session thread with the session thunk so the node can
bind developer context. The token grants arbitrary Lisp execution."
  (check-type port (unsigned-byte 16))
  (unless (and (stringp token) (= 64 (length token)) (every (lambda (c) (digit-char-p c 16)) token))
    (error "REPL token must be a 64-digit hex string"))
  (let ((listener (make-instance 'socket:inet-socket :type :stream :protocol :tcp)))
    (handler-case
        (progn
          (setf (socket:sockopt-reuse-address listener) t)
          (socket:socket-bind listener #(127 0 0 1) port)
          (socket:socket-listen listener 4)
          (let ((repl (%make-repl listener (nth-value 1 (socket:socket-name listener))
                                  token configure)))
            (clempo.lock:with-lock ((repl-lock repl))
              (setf (repl-thread repl) (sb-thread:make-thread (lambda () (run-repl repl))
                                                             :name "clempo Lisp REPL")))
            repl))
      (error (c) (socket:socket-close listener) (error c)))))

(defun stop-repl (repl)
  "Stop accepting, end every session and join their threads. A session evaluating a form is
interrupted, so the node's store is never closed under a running REPL form."
  (let ((clients (clempo.lock:with-lock ((repl-lock repl))
                   (setf (repl-stopped repl) t)
                   (copy-list (repl-clients repl)))))
    (sb-thread:join-thread (repl-thread repl) :default nil)
    (loop for (socket . thread) in clients
          do (handler-case (socket:socket-shutdown socket :direction :io)
               (socket:socket-error () nil))
             (ignore-errors
              (sb-thread:interrupt-thread thread (lambda () (signal 'sb-sys:interactive-interrupt))))
             (sb-thread:join-thread thread :default nil))))
