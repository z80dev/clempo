(in-package #:clempo.validator-process.tests)

;;; An in-memory network for validators run in one thread: the engine's network generic
;;; functions over per-node FIFO inboxes, with nodes taken offline and every block recorded.

(defstruct hub
  (nodes '())                           ; memory-networks
  (offline '())                         ; keys whose messages are dropped both ways
  (blocks '()))                         ; (blocker . blocked) keys, every block call

(defclass memory-network ()
  ((hub :initarg :hub :reader mem-hub)
   (key :initarg :key :reader mem-key)
   (inbox :initform (cons nil nil) :reader mem-inbox) ; (head . tail) queue
   (tracked :initform '() :accessor mem-tracked)))

(defun mem-offline-p (hub key) (member key (hub-offline hub) :test #'octets=))

(defun mem-enqueue (network item)
  (let ((cell (list item)) (queue (mem-inbox network)))
    (if (car queue) (setf (cddr queue) cell (cdr queue) cell) (setf (car queue) cell (cdr queue) cell))))

(defmethod net:network-send ((n memory-network) channel bytes &key recipients priority)
  (declare (ignore priority))
  (let ((hub (mem-hub n)) (count 0))
    (unless (mem-offline-p hub (mem-key n))
      (dolist (other (hub-nodes hub))
        (when (and (not (eq other n)) (not (mem-offline-p hub (mem-key other)))
                   (or (null recipients) (member (mem-key other) recipients :test #'octets=)))
          (mem-enqueue other (net::event :data (mem-key n) channel bytes 0 (length bytes)))
          (incf count))))
    count))

(defmethod net:network-receive ((n memory-network) &key timeout)
  (declare (ignore timeout))
  (let ((queue (mem-inbox n)))
    (when (car queue)
      (let ((item (pop (car queue))))
        (unless (car queue) (setf (cdr queue) nil))
        item))))

(defmethod net:network-track ((n memory-network) id peers)
  (push (cons id peers) (mem-tracked n)))

(defmethod net:network-block ((n memory-network) key)
  (push (cons (mem-key n) key) (hub-blocks (mem-hub n))))

(defmethod net:network-peers ((n memory-network))
  (let ((hub (mem-hub n)))
    (unless (mem-offline-p hub (mem-key n))
      (loop for other in (hub-nodes hub)
            unless (or (eq other n) (mem-offline-p hub (mem-key other))) collect (mem-key other)))))

(defmethod net:network-listen-port ((n memory-network)) 0)

