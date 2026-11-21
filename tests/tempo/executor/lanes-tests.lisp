(in-package #:clempo.executor.tests)

(defun lanes-fixture (name)
  (merge-pathnames name *data*))

(defun lanes-block (n)
  "Mainnet block N (debug_getRawBlock, data/README.md)."
  (blocks:decode-block
   (hex->octets (string-trim '(#\Newline #\Space)
                             (clempo.io:read-file-string (lanes-fixture (format nil "block-~D.hex" n)))))))

(defun lanes-env (n)
  (header-block-env (blocks:block-header (lanes-block n)) :chain :mainnet))

(defun lane-count (block fork)
  (count-if (lambda (tx) (clempo.executor::payment-transaction-p tx fork))
            (blocks:block-transactions block)))

;;; Payment lane: the fork picks the classifier (V1 before T5, V2 from T5, V3 from T15).

(deftest payment-lane-follows-fork ()
  ;; (block its-fork txs lane-under-its-fork v1 v2 v3)
  (loop for (n fork txs lane v1 v2 v3) in '((2000175 :t0 2 1 1 1 1)
                                            (10000020 :t1c 2 1 1 0 1)
                                            (22000056 :t4 2 2 2 1 1)
                                            (26000002 :t5 2 2 2 2 2)
                                            (50000005 :t13 2 1 1 1 1))
        for block = (lanes-block n)
        do (is (eq fork (block-env-spec (lanes-env n))) "fork of ~D" n)
           (is= txs (length (blocks:block-transactions block)))
           (is= lane (lane-count block fork) "lane count of ~D" n)
           (is= v1 (lane-count block :t4) "V1 count of ~D" n)
           (is= v2 (lane-count block :t5) "V2 count of ~D" n)
           (is= v2 (lane-count block :t14) "V2 count of ~D under T14" n)
           (is= v3 (lane-count block :t15) "V3 count of ~D" n)))

;;; Proposer fee recipient over ValidatorConfigV2 slots recorded with eth_getStorageAt at the
;;; parent block (data/vc2-<parent>.txt: "slot value" in hex, one per line).

(defun vc2-db (parent)
  (let ((db (store:make-statedb)))
    (store:db-begin-block db)
    (with-open-file (s (lanes-fixture (format nil "vc2-~D.txt" parent)))
      (loop for line = (read-line s nil) while line
            do (let ((sp (position #\Space line)))
                 (store:db-set-storage db #xcccccccc00000000000000000000000000000001
                                       (parse-integer line :end sp :radix 16)
                                       (parse-integer line :start (1+ sp) :radix 16)))))
    db))

(deftest proposer-fee-recipient-matches-beneficiary ()
  ;; Before T14 nothing enforces it, but every mainnet block checked pays the proposer's
  ;; recorded fee recipient.
  (dolist (n '(30000000 50049999))
    (let ((env (lanes-env n)))
      (is (clempo.executor::block-env-proposer env))
      (is= (block-env-beneficiary env)
           (clempo.executor::proposer-fee-recipient (vc2-db (1- n)) env
                                                    (clempo.executor::block-env-proposer env))
           "block ~D" n))))

(deftest proposer-fee-recipient-absent ()
  (let* ((env (lanes-env 50049999))
         (key (clempo.executor::block-env-proposer env))
         (db (vc2-db 50049998)))
    ;; before T2
    (setf (clempo.executor::block-env-spec env) :t1c)
    (is (null (clempo.executor::proposer-fee-recipient db env key)))
    (setf (clempo.executor::block-env-spec env) :t14)
    ;; unknown key
    (is (null (clempo.executor::proposer-fee-recipient db env (make-octets 32))))
    ;; V2 initialized at 13486706: not yet as of a parent below it
    (setf (block-env-number env) 13486706)
    (is (null (clempo.executor::proposer-fee-recipient db env key)))
    (setf (block-env-number env) 13486707)
    (is (clempo.executor::proposer-fee-recipient db env key)))
  ;; during the V1 migration (is_init still false) a record already exists
  (let* ((env (lanes-env 30000000))
         (key (clempo.executor::block-env-proposer env)))
    (setf (block-env-number env) 13486706)
    (is (null (clempo.executor::proposer-fee-recipient (vc2-db 13486705) env key)))))

;;; Storage changes of the running transaction

(deftest collect-storage-changes-of-running-tx ()
  (let* ((token #x20c0000000000000000000000000000000000001)
         (other #x1111111111111111111111111111111111111111)
         (backend (clempo.state:make-memory-backend)))
    (clempo.state:memory-backend-put-account backend token)
    (clempo.state:memory-backend-put-account backend other)
    (clempo.state:memory-backend-put-storage backend token 1 5)
    (clempo.state:memory-backend-put-storage backend token 3 9)
    (let ((world (clempo.state:make-world backend))
          (sorted (lambda (l) (sort (copy-list l) #'< :key #'second))))
      ;; an earlier transaction of the block
      (clempo.state:sstore world token 3 10)
      (clempo.state:finalize-tx world)
      (is (null (clempo.executor::collect-storage-changes world)))
      ;; the running one
      (clempo.state:sstore world token 1 7)
      (clempo.state:sstore world token 1 8)
      (clempo.state:sstore world token 2 4)
      (clempo.state:sstore world token 3 10)    ; written, unchanged
      (clempo.state:sload world token 4)        ; read only
      (clempo.state:sstore world other 1 1)
      (let ((cp (clempo.state:checkpoint world)))
        (clempo.state:sstore world token 5 6)
        (clempo.state:rollback world cp))
      (is-equalp `((,token 1 5 8) (,token 2 0 4) (,token 3 10 10))
                 (funcall sorted (clempo.executor::collect-storage-changes world)))
      (is-equalp `((,other 1 0 1))
                 (clempo.executor::collect-storage-changes
                  world :address-filter (lambda (a) (= a other))))
      (is= 4 (length (clempo.executor::collect-storage-changes world :address-filter nil)))
      (clempo.state:finalize-tx world)
      (is (null (clempo.executor::collect-storage-changes world))))))
