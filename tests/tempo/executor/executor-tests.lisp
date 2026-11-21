(defpackage #:clempo.executor.tests
  (:use #:cl #:clempo.base #:clempo.executor)
  (:local-nicknames (#:store #:clempo.store) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers)))

(in-package #:clempo.executor.tests)

(defparameter *data*
  (asdf:system-relative-pathname "clempo-tempo-executor" "../../../tests/tempo/executor/data/"))

(defun fixture-block (n)
  "Mainnet block N as debug_getRawBlock returned it (data/README.md)."
  (blocks:decode-block
   (hex->octets (string-trim '(#\Newline #\Space)
                             (clempo.io:read-file-string (merge-pathnames (format nil "block-~D.hex" n) *data*))))))

(defun header-state-root (block)
  (headers:ethereum-header-state-root (headers:header-inner (blocks:block-header block))))

(define-constant +mainnet-genesis-root+
  (hex->octets "83408974323f63ab969b23f0fe1dba30d7ee5dc5c524a975bae38187eaa2c7f6"))

(deftest genesis-matches-mainnet-block-0 ()
  (let ((db (genesis-db :chain :mainnet)))
    (is-equalp +mainnet-genesis-root+ (store:db-root-hash db))
    (is= 0 (store:db-number db))
    ;; block 1's parent is the genesis block
    (is-equalp (headers:ethereum-header-parent-hash (headers:header-inner (blocks:block-header (fixture-block 1))))
               (store:db-hash db))))

(defun blocks-1-3 ()
  (let ((table (make-hash-table)))
    (loop for n from 1 to 3 do (setf (gethash n table) (fixture-block n)))
    table))

(deftest system-blocks-replay-and-match ()
  (let* ((db (genesis-db :chain :mainnet))
         (table (blocks-1-3))
         (result (replay-blocks db 1 3 :block-fn (lambda (n) (gethash n table)))))
    (is= 3 (replay-result-matched-through result))
    (is= 3 (replay-result-last result))
    (is (null (replay-result-stopped result)))
    (is-equalp (blocks:block-hash (gethash 3 table)) (store:db-hash db))
    (is-equalp +mainnet-genesis-root+ (store:db-root-hash db))))

(defun tamper-state-root (block)
  "Corrupt the header's recorded state root (the block hash is kept from the raw bytes)."
  (let ((inner (headers:header-inner (blocks:block-header block))))
    (prog1 (copy-octets (headers:ethereum-header-state-root inner))
      (setf (headers:ethereum-header-state-root inner) (make-octets 32)))))

(deftest a-mismatch-stops-in-a-condition-and-retry-runs-the-block-again ()
  (let* ((db (genesis-db :chain :mainnet))
         (table (blocks-1-3))
         (good (tamper-state-root (gethash 2 table)))
         (signals '())
         (result
           (handler-bind ((block-mismatch
                            (lambda (c)
                              (push (list (block-mismatch-number c) (block-mismatch-field c)) signals)
                              ;; "fix the code" in the live image, then run the block again
                              (setf (headers:ethereum-header-state-root
                                     (headers:header-inner (blocks:block-header (gethash 2 table))))
                                    good)
                              (invoke-restart 'retry-block))))
             (replay-blocks db 1 3 :block-fn (lambda (n) (gethash n table))))))
    (is (equal '((2 :state-root)) signals))
    (is= 3 (replay-result-matched-through result))
    (is (null (replay-result-heals result)))))

(deftest skip-keeps-going-but-ends-the-contiguous-run ()
  (let* ((db (genesis-db :chain :mainnet))
         (table (blocks-1-3)))
    (tamper-state-root (gethash 2 table))
    (let ((result (handler-bind ((block-mismatch (lambda (c) (declare (ignore c)) (invoke-restart 'skip-block))))
                    (replay-blocks db 1 3 :block-fn (lambda (n) (gethash n table))))))
      (is (equal '(2) (replay-result-skips result)))
      (is= 1 (replay-result-matched-through result))
      (is= 3 (replay-result-last result)))))

(deftest abort-undoes-the-block-and-returns ()
  (let* ((db (genesis-db :chain :mainnet))
         (table (blocks-1-3)))
    (tamper-state-root (gethash 2 table))
    (let ((result (handler-bind ((block-mismatch (lambda (c) (invoke-restart 'abort-replay c))))
                    (replay-blocks db 1 3 :block-fn (lambda (n) (gethash n table))))))
      (is= 1 (replay-result-last result))
      (is= 1 (store:db-number db))
      (is (not (store:db-block-open-p db)))
      (is (typep (replay-result-stopped result) 'block-mismatch))
      (is (eq :state-root (block-mismatch-field (replay-result-stopped result)))))))

(defun db-at-3210 ()
  "Mainnet state after block 3210: blocks 1..3210 hold only the subblocks system transaction,
so the state is genesis (every header through 3210 carries the genesis state root)."
  (let ((db (genesis-db :chain :mainnet))
        (b3210 (fixture-block 3210)))
    (is-equalp +mainnet-genesis-root+ (header-state-root b3210))
    (store:db-begin-block db)
    (store:db-commit-block db 3210 (blocks:block-hash b3210))
    db))

(deftest heal-from-reference-applies-the-prestate-diff ()
  (let* ((db (db-at-3210))
         (b3211 (fixture-block 3211))
         (traces (clempo.json:parse-json
                  (clempo.io:read-file-string (merge-pathnames "prestate-diff-3211.json" *data*))))
         (seen nil)
         ;; Any error in a block (here one injected before execution) offers the restarts.
         (result (handler-bind ((error
                                  (lambda (c)
                                    (setf seen c)
                                    (invoke-restart 'heal-from-reference))))
                   (replay-blocks db 3211 3211
                                  :block-fn (lambda (n) (declare (ignore n)) b3211)
                                  :senders-fn (lambda (block) (declare (ignore block))
                                                (error "injected failure"))
                                  :heal-fn (lambda (db n block)
                                             (declare (ignore n))
                                             (clempo.executor::heal-block db block traces :chain :mainnet))))))
    (is seen)
    (is (equal '(3211) (replay-result-heals result)))
    (is= 3210 (replay-result-matched-through result))
    (is= 3211 (store:db-number db))
    (is-equalp (header-state-root b3211) (store:db-root-hash db))))
