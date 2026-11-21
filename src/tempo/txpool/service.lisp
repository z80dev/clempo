(in-package #:clempo.txpool)

;;; Listing and clearing, for the RPC's txpool_* methods and admin_clearTxpool.

(defun pool-snapshot (pool)
  "Every pooled transaction as (tx sender pending-p), in submission order. PENDING-P when
its nonce lane is gapless up to it (txpool_content's pending); the rest are queued."
  (mapcar (lambda (entry) (list (entry-tx entry) (entry-sender entry) (nonce-pending-p pool entry)))
          (sort (loop for entry being the hash-values of (pool-entries pool) collect entry)
                #'< :key #'entry-id)))

(defun pool-clear (pool)
  "Remove every pooled transaction; returns how many."
  (let ((hashes (loop for entry being the hash-values of (pool-entries pool)
                      collect (clempo.transactions:transaction-hash (entry-tx entry)))))
    (dolist (hash hashes) (pool-remove-transaction pool hash))
    (length hashes)))
