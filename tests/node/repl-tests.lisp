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

(deftest root-hashing-worker-pool ()
  (let ((serial (clempo.trie:make-trie))
        (parallel (clempo.trie:make-trie))
        (value (fill (make-octets 32) 7)))
    (dotimes (i 4096)
      (let ((key (integer->octets i 2)))
        (clempo.trie:trie-put serial key value)
        (clempo.trie:trie-put parallel key value)))
    (clempo.node.workers:call-with-workers
     (lambda (mapper)
       (is-equalp (clempo.trie:trie-root serial)
                  (clempo.trie:call-with-trie-hashing
                   (lambda () (clempo.trie:trie-root parallel)) :parallel-map mapper))
       ;; An error must return to the submitter without killing its worker.
       (signals error (funcall mapper (lambda (i) (when (= i 2) (error "test error")))
                                       '(1 2 3)))
       (signals error (funcall mapper #'identity (make-list 17)))
       (let ((result (make-array 3)))
         (funcall mapper (lambda (i) (setf (aref result i) (* i i))) '(0 1 2))
         (is-equalp #(0 1 4) result)))
     :threads 4)
    (is (null (clempo.node.workers:call-with-workers #'identity :threads 1)))))
