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
  "Decode body items directly from RAW, retaining only owned envelope byte strings."
  (multiple-value-bind (start end listp) (clempo.rlp:rlp-item-bounds raw 0 (length raw))
    (invariant (and listp (= end (length raw))) "block must be one RLP list")
    (let ((cursor start))
      (flet ((header ()
               (multiple-value-bind (header next)
                   (clempo.headers:decode-header raw :start cursor :end end :allow-trailing t)
                 (setf cursor next) header))
             (transactions ()
               (multiple-value-bind (start stop list-p) (clempo.rlp:rlp-item-bounds raw cursor end)
                 (invariant list-p "transactions must be a list")
                 (setf cursor stop)
                 (loop with items = nil with position = start
                       while (< position stop)
                       do (multiple-value-bind (payload next legacy-p)
                              (clempo.rlp:rlp-item-bounds raw position stop)
                            (push (clempo.transactions:decode-transaction
                                   (subseq raw (if legacy-p position payload) next) :copy-raw nil) items)
                            (setf position next))
                       finally (return (nreverse items)))))
             (ommers ()
               (multiple-value-bind (start stop list-p) (clempo.rlp:rlp-item-bounds raw cursor end)
                 (invariant list-p "ommers must be a list")
                 (setf cursor stop)
                 (loop with items = nil with position = start
                       while (< position stop)
                       do (multiple-value-bind (header next)
                              (clempo.headers:decode-header raw :start position :end stop
                                                               :allow-trailing t :retain-raw nil)
                            (push header items) (setf position next))
                       finally (return (nreverse items))))))
        (let* ((header (header)) (transactions (transactions)) (ommers (ommers))
               (present (< cursor end))
               (withdrawals (when present (clempo.rlp:rlp-decode raw :start cursor :end end))))
          (invariant (listp withdrawals) "withdrawals must be a list")
          (dolist (withdrawal withdrawals)
            (invariant (and (listp withdrawal) (= (length withdrawal) 4)) "invalid withdrawal")
            (clempo.rlp:rlp-decode-integer (first withdrawal) :max-bytes 8)
            (clempo.rlp:rlp-decode-integer (second withdrawal) :max-bytes 8)
            (clempo.defrlp:decode-field :address (third withdrawal))
            (clempo.rlp:rlp-decode-integer (fourth withdrawal) :max-bytes 8))
          (make-block :header header :raw raw :transactions transactions :ommers ommers
                      :withdrawals withdrawals :withdrawals-present-p present))))))

(defun encode-block (block)
  "Write directly into one result vector; no encoded envelope or header temporaries."
  (let* ((transactions (block-transactions block)) (ommers (block-ommers block))
         (tx-size (loop for tx in transactions for size = (clempo.transactions:transaction-encoded-length tx)
                        sum (+ size (if (zerop (clempo.transactions:transaction-type tx)) 0
                                        (clempo.rlp:header-length size)))))
         (ommer-size (loop for ommer in ommers sum (clempo.headers:header-encoded-length ommer)))
         (payload (+ (clempo.headers:header-encoded-length (block-header block))
                     tx-size (clempo.rlp:header-length tx-size)
                     ommer-size (clempo.rlp:header-length ommer-size)
                     (if (block-withdrawals-present-p block)
                         (clempo.rlp:rlp-encoded-length (block-withdrawals block)) 0)))
         (out (make-octets (+ payload (clempo.rlp:header-length payload))))
         (position (clempo.rlp:write-header out 0 payload #xc0)))
    (setf position (clempo.headers:header-encode-into (block-header block) out position)
          position (clempo.rlp:write-header out position tx-size #xc0))
    (dolist (tx transactions)
      (unless (zerop (clempo.transactions:transaction-type tx))
        (setf position (clempo.rlp:write-header out position
                                               (clempo.transactions:transaction-encoded-length tx) #x80)))
      (setf position (clempo.transactions:encode-transaction-into tx out position)))
    (setf position (clempo.rlp:write-header out position ommer-size #xc0))
    (dolist (ommer ommers) (setf position (clempo.headers:header-encode-into ommer out position)))
    (when (block-withdrawals-present-p block)
      (setf position (clempo.rlp:rlp-encode-into (block-withdrawals block) out position)))
    (invariant (= position (length out)) "block encoding size mismatch")
    out))

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
