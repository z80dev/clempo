(in-package #:clempo.dev)

;;; Signature recovery does not read execution state. Share one bounded worker
;;; pool across the replay and preserve transaction order in its results.

(defconstant +default-sender-threads+ 8)

(defun recover-senders (block mapper batches threads)
  (let* ((txs (clempo.blocks:block-transactions block))
         (n (length txs)))
    (if (< n 4)
        (mapcar #'clempo.transactions:transaction-sender txs)
        (let ((transactions (coerce txs 'simple-vector))
              (workers (min n threads))
              (out (make-array n)))
          (multiple-value-bind (size extra) (floor n workers)
            (funcall mapper
                     (lambda (worker)
                       (let ((start (+ (* worker size) (min worker extra))))
                         (loop for i from start below (+ start size (if (< worker extra) 1 0))
                               do (setf (svref out i)
                                        (clempo.transactions:transaction-sender (svref transactions i))))))
                     (svref batches workers)))
          (coerce out 'list)))))

(defun call-with-recovered-senders (function threads)
  (clempo.node.workers:call-with-workers
   (lambda (mapper)
     (if mapper
         (let ((batches (make-array (1+ threads))))
           (dotimes (n (1+ threads))
             (setf (svref batches n) (loop for i below n collect i)))
           (funcall function (lambda (block) (recover-senders block mapper batches threads))))
         (funcall function nil)))
   :threads threads :name "transaction sender"))
