(defpackage #:clempo.store.tests
  (:use #:cl #:clempo.base #:clempo.store)
  (:import-from #:clempo.trie #:make-secure-trie #:trie-put #:trie-root #:encode-account
                #:+empty-trie-root+ #:+empty-code-hash+)
  (:import-from #:clempo.rlp #:rlp-encode))

(in-package #:clempo.store.tests)

(defun reference-root (db)
  "The state root of DB built from scratch, without its cached tries."
  (let ((trie (make-secure-trie)))
    (map-accounts (lambda (address a)
                    (let ((st (make-secure-trie)))
                      (map-account-storage (lambda (slot v) (trie-put st (slot-octets slot) (rlp-encode v))) a)
                      (trie-put trie (address-octets address)
                                (encode-account (account-nonce a) (account-balance a) (trie-root st)
                                                (account-code-hash a)))))
                  db)
    (trie-root trie)))

(defun snapshot (db)
  "Every account and slot of DB as a sorted list."
  (let ((rows '()))
    (map-accounts (lambda (address a)
                    (push (list address (account-nonce a) (account-balance a)
                                (octets->hex (account-code-hash a))
                                (let ((slots '()))
                                  (map-account-storage (lambda (k v) (push (cons k v) slots)) a)
                                  (sort slots #'< :key #'car)))
                          rows))
                  db)
    (sort rows #'< :key #'first)))

(defun random-block (db rng number &key (accounts 12) (writes 40))
  "Open block NUMBER, make random writes over a small address space, and return nothing."
  (db-begin-block db)
  (dotimes (i writes)
    (let ((address (1+ (random accounts rng))))
      (case (random 10 rng)
        ((0 1 2) (db-set-account db address (random 5 rng) (random 1000 rng)
                                 (if (zerop (random 3 rng))
                                     (db-put-code db (octets-of (random 256 rng) (random 256 rng)))
                                     +empty-code-hash+)))
        (3 (db-delete-account db address))
        (4 (db-wipe-storage db address))
        (t (db-set-storage db address (random 8 rng)
                           (if (zerop (random 4 rng)) 0 (random (expt 2 256) rng)))))))
  number)

(defun block-hash (n) (integer->octets (+ #xabc000 n) 32))

(deftest statedb-root-matches-a-fresh-build ()
  (let ((db (make-statedb)) (rng (sb-ext:seed-random-state 7)))
    (is-equalp +empty-trie-root+ (db-state-root db))
    (loop for n from 1 to 30
          do (random-block db rng n)
             (db-commit-block db n (block-hash n))
             (is-equalp (reference-root db) (db-root-hash db) "block ~D" n))
    (is-equalp (block-hash 30) (db-block-hash db 30))))

(deftest statedb-revert-restores-the-block-before ()
  (let ((db (make-statedb)) (rng (sb-ext:seed-random-state 11)))
    (loop for n from 1 to 10 do (random-block db rng n) (db-commit-block db n (block-hash n)))
    (loop repeat 20
          do (let ((before (snapshot db)) (root (db-state-root db)))
               (random-block db rng 11 :writes 60)
               (db-state-root db)
               (db-revert-block db)
               (is-equal-snapshot before (snapshot db))
               (is-equalp root (db-state-root db))
               (is-equalp (reference-root db) (db-state-root db))))))

(defun is-equal-snapshot (a b) (is (equal a b) "snapshots differ"))

(deftest speculative-blocks-unwind-to-the-head ()
  (let ((db (make-statedb)) (rng (sb-ext:seed-random-state 13)))
    (loop for n from 1 to 5 do (random-block db rng n) (db-commit-block db n (block-hash n)))
    (let ((before (snapshot db)) (root (db-root-hash db)))
      (with-speculative-blocks (db)
        (loop for n from 6 to 9
              do (random-block db rng n :writes 50)
                 (db-commit-block db n (block-hash n))
                 (is-equalp (reference-root db) (db-root-hash db)))
        (with-speculative-blocks (db)
          (random-block db rng 10)
          (db-commit-block db 10 (block-hash 10))
          (is= 10 (db-number db)))
        (is= 9 (db-number db))
        (random-block db rng 10))       ; left open: unwound too
      (is= 5 (db-number db))
      (is (null (db-block-hash db 6)))
      (is-equalp (block-hash 5) (db-hash db))
      (is (equal before (snapshot db)))
      (is-equalp root (db-state-root db))
      (is-equalp (reference-root db) (db-state-root db))
      ;; and normal blocks continue from there
      (random-block db rng 6)
      (db-commit-block db 6 (block-hash 6))
      (is-equalp (reference-root db) (db-root-hash db)))))

(deftest block-diffs-replay-to-the-same-state ()
  (let ((db (make-statedb)) (copy (make-statedb)) (rng (sb-ext:seed-random-state 5)))
    (loop for n from 1 to 25
          do (random-block db rng n)
             (let* ((diff (db-commit-block db n (block-hash n)))
                    (again (decode-block-diff (encode-block-diff diff))))
               (is-equalp (encode-block-diff diff) (encode-block-diff again))
               (is-equalp (db-root-hash db) (db-apply-diff copy again))))
    (is (equal (snapshot db) (snapshot copy)))))

(defun temp-dir ()
  (format nil "/tmp/clempo-store-test-~D/" (random 100000000 (make-random-state t))))

(defun genesis (db)
  (db-begin-block db)
  (db-set-account db 1 0 100 +empty-code-hash+)
  (db-set-storage db 1 5 6)
  (db-commit-block db 0 (block-hash 0)))

(deftest store-survives-restarts-with-a-root-check ()
  (let ((dir (temp-dir)) (rng (sb-ext:seed-random-state 3)))
    (unwind-protect
         (let ((store (open-store dir :genesis #'genesis :interval 8)))
           (loop for n from 1 to 20
                 do (random-block (store-db store) rng n)
                    (store-commit-block store (db-commit-block (store-db store) n (block-hash n))))
           (let ((expected (snapshot (store-db store)))
                 (root (db-root-hash (store-db store))))
             (close-store store)
             ;; checkpoint at 16, blocks 17..20 from the log
             (is (probe-file (format nil "~Acheckpoint-~12,'0D.state" dir 16)))
             (let ((again (open-store dir)))
               (is= 20 (db-number (store-db again)))
               (is-equalp root (db-root-hash (store-db again)))
               (is (equal expected (snapshot (store-db again))))
               (is-equalp (block-hash 19) (db-block-hash (store-db again) 19))
               ;; keep going, checkpoint on close, reopen
               (random-block (store-db again) rng 21)
               (store-commit-block again (db-commit-block (store-db again) 21 (block-hash 21)))
               (let ((root21 (db-root-hash (store-db again))))
                 (close-store again :checkpoint t)
                 (let ((third (open-store dir)))
                   (is= 21 (db-number (store-db third)))
                   (is-equalp root21 (db-root-hash (store-db third)))
                   (close-store third))))))
      (sb-ext:delete-directory dir :recursive t))))

(deftest store-cuts-a-torn-log-tail ()
  (let ((dir (temp-dir)) (rng (sb-ext:seed-random-state 9)))
    (unwind-protect
         (let ((store (open-store dir :genesis #'genesis)))
           (loop for n from 1 to 5
                 do (random-block (store-db store) rng n)
                    (store-commit-block store (db-commit-block (store-db store) n (block-hash n))))
           (close-store store)
           ;; chop the last record in half
           (let* ((log (format nil "~Alog-~12,'0D.wal" dir 0))
                  (bytes (clempo.io:read-file-octets log)))
             (clempo.io:write-file-octets log (subseq bytes 0 (- (length bytes) 7))))
           (let ((again (open-store dir)))
             (is= 4 (db-number (store-db again)))
             (random-block (store-db again) rng 5)
             (store-commit-block again (db-commit-block (store-db again) 5 (block-hash 5)))
             (close-store again))
           (is= 5 (db-number (store-db (open-store dir)))))
      (sb-ext:delete-directory dir :recursive t))))

(deftest store-rejects-a-checkpoint-whose-root-is-wrong ()
  (let ((dir (temp-dir)))
    (unwind-protect
         (progn
           (close-store (open-store dir :genesis #'genesis))
           ;; flip the recorded root (after magic 8, version 4, number 8, hash 32)
           (let* ((path (format nil "~Acheckpoint-~12,'0D.state" dir 0))
                  (bytes (clempo.io:read-file-octets path)))
             (setf (aref bytes 52) (logxor 1 (aref bytes 52)))
             (clempo.io:write-file-octets path bytes))
           (signals store-root-mismatch (open-store dir)))
      (sb-ext:delete-directory dir :recursive t))))
