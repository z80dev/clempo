(defpackage #:clempo.blocks
  (:use #:cl #:clempo.base)
  (:shadow #:block)
  (:export #:block #:make-block #:block-header #:block-transactions #:block-ommers
           #:block-withdrawals #:block-withdrawals-present-p #:block-raw
           #:decode-block #:encode-block #:block-hash #:transactions-root))
(in-package #:clempo.blocks)

(defstruct block
  header (transactions nil :type list) (ommers nil :type list)
  (withdrawals nil :type list) (withdrawals-present-p nil :type boolean)
  (raw nil :type (or null octets)))

(defun decode-block (raw)
  "Decode the outer block list without losing the original header or transaction bytes."
  (multiple-value-bind (start end listp) (clempo.rlp:rlp-item-bounds raw 0 (length raw))
    (invariant (and listp (= end (length raw))) "block must be one RLP list")
    (let ((cursor start))
      (flet ((next-item ()
               (multiple-value-bind (payload stop list-p) (clempo.rlp:rlp-item-bounds raw cursor end)
                 (declare (ignore payload list-p))
                 (prog1 (subseq raw cursor stop) (setf cursor stop)))))
        (let* ((header (clempo.headers:decode-header (next-item)))
               (tx-items (clempo.rlp:rlp-decode (next-item)))
               (ommer-items (clempo.rlp:rlp-decode (next-item)))
               (present (< cursor end))
               (withdrawals (when present (clempo.rlp:rlp-decode (next-item)))))
          (invariant (and (= cursor end) (listp tx-items) (listp ommer-items) (listp withdrawals))
                     "invalid block body lists")
          (dolist (withdrawal withdrawals)
            (invariant (and (listp withdrawal) (= (length withdrawal) 4)) "invalid withdrawal")
            (clempo.rlp:rlp-decode-integer (first withdrawal) :max-bytes 8)
            (clempo.rlp:rlp-decode-integer (second withdrawal) :max-bytes 8)
            (clempo.defrlp:decode-field :address (third withdrawal))
            (clempo.rlp:rlp-decode-integer (fourth withdrawal) :max-bytes 8))
          (make-block :header header :raw raw
                      :transactions (mapcar (lambda (item)
                                              (clempo.transactions:decode-transaction
                                               (if (listp item) (clempo.rlp:rlp-encode item) item))) tx-items)
                      :ommers (mapcar #'clempo.headers:header-from-items ommer-items)
                      :withdrawals withdrawals :withdrawals-present-p present))))))

(defun encode-block (block)
  (clempo.rlp:rlp-encode
   (append (list (clempo.headers:header-to-items (block-header block))
                 (mapcar (lambda (tx)
                           (let ((raw (clempo.transactions:encode-transaction tx)))
                             (if (zerop (clempo.transactions:transaction-type tx))
                                 (clempo.rlp:make-rlp-raw raw) raw)))
                         (block-transactions block))
                 (mapcar #'clempo.headers:header-to-items (block-ommers block)))
           (when (block-withdrawals-present-p block) (list (block-withdrawals block))))))

(defun block-hash (block) (clempo.headers:header-hash (block-header block)))

(defun transactions-root (transactions)
  (clempo.trie:ordered-trie-root (mapcar #'clempo.transactions:encode-transaction transactions)))
