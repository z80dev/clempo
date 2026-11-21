(defpackage #:clempo.node.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:socket #:sb-bsd-sockets) (#:repl #:clempo.node.repl)))

(in-package #:clempo.node.tests)

(defun call-repl (repl token fn)
  (let ((client (make-instance 'socket:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (socket:socket-connect client #(127 0 0 1) (repl:repl-port repl))
           (with-open-stream (stream (socket:socket-make-stream client :input t :output t
                                                               :timeout 2 :buffering :line))
             (is (string= "clempo Lisp REPL: send token" (read-line stream)))
             (write-line token stream)
             (finish-output stream)
             (funcall fn stream)))
      (socket:socket-close client))))

(deftest authenticated-loopback-repl ()
  (let* ((token (make-string 64 :initial-element #\a))
         (repl (repl:start-repl 0 token)))
    (unwind-protect
         (progn
           (call-repl repl (make-string 64 :initial-element #\b)
                      (lambda (s) (is (string= "Access denied" (read-line s)))))
           (call-repl repl token
                      (lambda (s)
                        (write-line "(values (+ 20 22) :ok)" s)
                        (finish-output s)
                        (is (string= "COMMON-LISP-USER> 42" (read-line s)))
                        (is (string= ":OK" (read-line s))))))
      (repl:stop-repl repl))))
