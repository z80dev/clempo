(defpackage #:clempo.types.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.types.tests)

(deftest recorded-blocks-and-receipts ()
  (let ((text (clempo.io:read-file-string
               (asdf:system-relative-pathname "clempo-types" "../../tests/types/data/blocks.jsonl"))))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil) while line
            for row = (clempo.json:parse-json line)
            for raw = (hex->octets (clempo.json:jget row "raw"))
            for block = (clempo.blocks:decode-block raw)
            for receipts = (map 'list #'clempo.receipts:receipt-from-rpc (clempo.json:jget row "receipts"))
            do (is-equalp raw (clempo.blocks:encode-block block))
               (is (null (clempo.blocks:block-body-mismatches block)))
               (is-equalp (hex->octets (clempo.json:jget row "hash")) (clempo.blocks:block-hash block))
               (is-equalp (hex->octets (clempo.json:jget row "transactionsRoot"))
                          (clempo.blocks:transactions-root (clempo.blocks:block-transactions block)))
               (is-equalp (hex->octets (clempo.json:jget row "receiptsRoot"))
                          (clempo.receipts:receipts-root receipts))
               (is-equalp (clempo.headers:ethereum-header-logs-bloom
                           (clempo.headers:header-inner (clempo.blocks:block-header block)))
                          (clempo.receipts:receipts-bloom receipts))
               (dolist (receipt receipts)
                 (let ((encoded (clempo.receipts:encode-receipt receipt)))
                   (is-equalp encoded (clempo.receipts:encode-receipt
                                        (clempo.receipts:decode-receipt encoded)))))))))

(deftest declarative-trailing-fields ()
  (let* ((inner (clempo.headers:ethereum-header-from-items
                 (list (make-octets 32) (make-octets 32) (make-octets 20)
                       (make-octets 32) (make-octets 32) (make-octets 32)
                       (make-octets 256) (make-octets 0) (make-octets 0)
                       (make-octets 0) (make-octets 0) (make-octets 0)
                       (make-octets 0) (make-octets 32) (make-octets 8))))
         (header (clempo.headers:make-header :general-gas-limit 1 :shared-gas-limit 2
                                            :timestamp-millis-part 3 :inner inner)))
    (is= 15 (length (clempo.headers:ethereum-header-to-items inner)))
    (setf (clempo.headers:ethereum-header-base-fee-per-gas inner) 0)
    (is= 16 (length (clempo.headers:ethereum-header-to-items inner)))
    (let* ((encoded (clempo.headers:encode-header header))
           (decoded (clempo.headers:decode-header encoded)))
      (is-equalp encoded (clempo.headers:encode-header decoded))
      (let ((out (make-octets (+ 10 (clempo.headers:header-encoded-length decoded)))))
        (is= (+ 5 (length encoded)) (clempo.headers:header-encode-into decoded out 5))
        (is-equalp encoded (subseq out 5 (+ 5 (length encoded))))
        (multiple-value-bind (bounded next)
            (clempo.headers:decode-header out :start 5 :end (+ 5 (length encoded)))
          (is= (+ 5 (length encoded)) next)
          (is-equalp encoded (clempo.headers:header-raw bounded))
          (is-equalp encoded (clempo.headers:encode-header bounded))))
      (is= 0 (clempo.headers:ethereum-header-base-fee-per-gas (clempo.headers:header-inner decoded)))
      (is= 3 (clempo.headers:header-timestamp-millis decoded)))
    (setf (clempo.headers:ethereum-header-base-fee-per-gas inner) nil
          (clempo.headers:ethereum-header-withdrawals-root inner) (make-octets 32))
    (signals error (clempo.headers:encode-ethereum-header inner))
    (is= 5 (length (clempo.defrlp:rlp-field-documentation 'clempo.headers:header)))))

(deftest malformed-block-receipt-input ()
  (signals error (clempo.blocks:decode-block (hex->octets "c0")))
  (signals error (clempo.headers:decode-header (hex->octets "c4808080c0")))
  (signals error (clempo.receipts:decode-receipt (hex->octets "00c0")))
  (signals error (clempo.receipts:decode-receipt (hex->octets "76c0"))))

(deftest block-body-commitment-mismatches ()
  (let* ((empty clempo.trie:+empty-trie-root+)
         (inner (clempo.headers:make-ethereum-header
                 :transactions-root empty
                 :ommers-hash (clempo.keccak:keccak256 (clempo.rlp:rlp-encode nil))))
         (block (clempo.blocks:make-block :header (clempo.headers:make-header :inner inner))))
    (is (null (clempo.blocks:block-body-mismatches block)))
    (setf (clempo.headers:ethereum-header-withdrawals-root inner) empty)
    (is= '(:withdrawals-present-p)
         (mapcar #'first (clempo.blocks:block-body-mismatches block)))
    (setf (clempo.blocks:block-withdrawals-present-p block) t)
    (is (null (clempo.blocks:block-body-mismatches block)))
    (setf (clempo.headers:ethereum-header-transactions-root inner) (make-octets 32)
          (clempo.headers:ethereum-header-ommers-hash inner) (make-octets 32)
          (clempo.headers:ethereum-header-withdrawals-root inner) (make-octets 32))
    (is= '(:transactions-root :ommers-hash :withdrawals-root)
         (mapcar #'first (clempo.blocks:block-body-mismatches block)))
    (setf (clempo.headers:ethereum-header-transactions-root inner) empty
          (clempo.headers:ethereum-header-ommers-hash inner)
          (clempo.keccak:keccak256 (clempo.rlp:rlp-encode nil))
          (clempo.headers:ethereum-header-withdrawals-root inner) nil)
    (is= '((:withdrawals-present-p nil t)) (clempo.blocks:block-body-mismatches block))))

(deftest receipts-root-reuse-matches-the-trie ()
  ;; the last root is reused for equal receipts; every answer equals the trie's
  (flet ((plain (&rest gas) (loop for g in gas for i from 0
                                  collect (clempo.receipts:make-receipt :type (if (evenp i) 0 2) :status 1
                                                                        :cumulative-gas-used g)))
         (direct (receipts) (clempo.trie:ordered-trie-root
                             (mapcar #'clempo.receipts:encode-receipt receipts))))
    (let* ((log (clempo.receipts:make-event-log :address 7 :topics (list (make-octets 32))))
           (logged (list (clempo.receipts:make-receipt :type 2 :status 1 :cumulative-gas-used 5
                                                       :logs (list log))))
           (zero-bloom (list (clempo.receipts:make-receipt :type 0 :status 1 :cumulative-gas-used 0
                                                           :bloom (make-octets 256))))
           (cases (list (plain 0) (plain 0) (plain 21000) (plain 0 21000) (plain 0 21000) logged logged
                        (plain 0) zero-bloom (plain 0) (plain 5) logged '())))
      (dolist (receipts cases)
        (is-equalp (direct receipts) (clempo.receipts:receipts-root receipts)))
      (is-equalp (clempo.receipts:receipts-bloom logged) (clempo.receipts:logs-bloom (list log)))
      (is (every #'zerop (clempo.receipts:receipts-bloom (plain 0 1)))))))
