(defpackage #:clempo.blocks
  (:use #:cl #:clempo.base)
  (:shadow #:block)
  (:export #:block #:make-block #:block-header #:block-transactions #:block-ommers
           #:block-withdrawals #:block-withdrawals-present-p #:block-raw
           #:decode-block #:encode-block #:block-hash #:transactions-root #:block-body-mismatches))
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

(defun block-body-mismatches (block)
  "Return (FIELD EXPECTED ACTUAL) triples for body commitments, NIL on success.
FIELD is :TRANSACTIONS-ROOT, :OMMERS-HASH, :WITHDRAWALS-PRESENT-P or
:WITHDRAWALS-ROOT. Expected commitments come from the header. Withdrawal
presence is checked independently: an empty present list is not an absent list."
  (let* ((header (clempo.headers:header-inner (block-header block)))
         (root (clempo.headers:ethereum-header-withdrawals-root header))
         (present (block-withdrawals-present-p block))
         (mismatches nil))
    (flet ((check (field expected actual)
             (unless (equalp expected actual) (push (list field expected actual) mismatches))))
      (check :transactions-root (clempo.headers:ethereum-header-transactions-root header)
             (transactions-root (block-transactions block)))
      (check :ommers-hash (clempo.headers:ethereum-header-ommers-hash header)
             (clempo.keccak:keccak256
              (clempo.rlp:rlp-encode (mapcar #'clempo.headers:header-to-items (block-ommers block)))))
      (check :withdrawals-present-p (not (null root)) present)
      (when (and root present)
        (check :withdrawals-root root
               (clempo.trie:ordered-trie-root
                (mapcar #'clempo.rlp:rlp-encode (block-withdrawals block))))))
    (nreverse mismatches)))
