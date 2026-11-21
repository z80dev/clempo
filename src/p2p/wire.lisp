(defpackage #:clempo.p2p-wire
  (:use #:cl #:clempo.base)
  (:export #:p2p-error #:protocol-error #:read-octets #:write-octets
           #:secure-equal-p #:xor-octets #:connect-tcp))

(in-package #:clempo.p2p-wire)

(define-condition p2p-error (error)
  ((detail :initarg :detail :reader error-detail))
  (:report (lambda (condition stream)
             (format stream "p2p: ~A" (error-detail condition)))))

(defun protocol-error (control &rest args)
  (error 'p2p-error :detail (apply #'format nil control args)))

(defun read-octets (stream size)
  (let ((bytes (make-octets size)))
    (unless (= (read-sequence bytes stream) size)
      (protocol-error "unexpected end of stream"))
    bytes))

(defun write-octets (stream octets)
  (write-sequence octets stream))

(defun secure-equal-p (a b &key (start-a 0) (start-b 0) (count (- (length a) start-a)))
  "Compare COUNT octets without short circuiting on differences."
  (declare (type octets a b) (type fixnum start-a start-b count) #.*hot*)
  (and (<= 0 start-a (+ start-a count) (length a))
       (<= 0 start-b (+ start-b count) (length b))
       (let ((difference 0))
         (declare (type octet difference))
         (dotimes (i count (zerop difference))
           (setf difference (logior difference
                                    (logxor (aref a (+ start-a i))
                                            (aref b (+ start-b i)))))))))

(defun xor-octets (a b)
  (declare (type octets a b) #.*hot*)
  (invariant (= (length a) (length b)))
  (let ((out (make-octets (length a))))
    (dotimes (i (length a) out)
      (setf (aref out i) (logxor (aref a i) (aref b i))))))

(defun connect-tcp (host port &key (timeout 10))
  "Open a buffered binary TCP stream; the caller owns it."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (stream nil))
    (unwind-protect
         (let ((address (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))
           (sb-ext:with-timeout timeout
             (sb-bsd-sockets:socket-connect socket address port))
           (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t
                 stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                           :element-type 'octet :buffering :full
                                                           :timeout timeout :auto-close t)))
      (unless stream (sb-bsd-sockets:socket-close socket)))))
