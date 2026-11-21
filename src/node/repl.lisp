(defpackage #:clempo.node.repl
  (:use #:cl)
  (:local-nicknames (#:socket #:sb-bsd-sockets))
  (:export #:start-repl #:stop-repl #:repl-port))

(in-package #:clempo.node.repl)

(defstruct (repl (:constructor %make-repl (listener port token configure)))
  listener port token configure thread client stopped
  (lock (sb-thread:make-mutex :name "node REPL")))

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
  (with-open-stream (stream (socket:socket-make-stream client :input t :output t
                                                     :external-format :utf-8 :buffering :line
                                                     :timeout 300))
    (write-line "clempo Lisp REPL: send token" stream)
    (finish-output stream)
    (if (authenticated-p stream (repl-token repl))
        (funcall (repl-configure repl) (lambda () (eval-session stream)))
        (write-line "Access denied" stream))))

(defun stopped-p (repl)
  (sb-thread:with-mutex ((repl-lock repl)) (repl-stopped repl)))

(defun run-repl (repl)
  (let ((listener (repl-listener repl)))
    (unwind-protect
         (loop until (stopped-p repl)
               when (sb-sys:wait-until-fd-usable (socket:socket-file-descriptor listener) :input 0.2)
                 do (let ((client (socket:socket-accept listener)))
                      (unwind-protect
                           (when (sb-thread:with-mutex ((repl-lock repl))
                                   (unless (repl-stopped repl) (setf (repl-client repl) client)))
                             (handler-case (serve-client repl client)
                               ((or stream-error socket:socket-error sb-ext:timeout) () nil)))
                        (sb-thread:with-mutex ((repl-lock repl))
                          (setf (repl-client repl) nil)
                          (socket:socket-close client)))))
      (socket:socket-close listener))))

(defun start-repl (port token &key (configure #'funcall))
  "Start a localhost-only debugger. TOKEN is a 32-byte hex secret, without 0x.
CONFIGURE is called on the REPL thread with the session thunk so the node can
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
            (setf (repl-thread repl) (sb-thread:make-thread (lambda () (run-repl repl))
                                                           :name "clempo Lisp REPL"))
            repl))
      (error (c) (socket:socket-close listener) (error c)))))

(defun stop-repl (repl)
  (sb-thread:with-mutex ((repl-lock repl))
    (setf (repl-stopped repl) t)
    (let ((client (repl-client repl)))
      (when client
        ;; The stream may already have closed the descriptor on session exit.
        (handler-case (socket:socket-shutdown client :direction :io)
          (socket:socket-error () nil)))))
  (sb-thread:join-thread (repl-thread repl) :timeout 2 :default nil))
