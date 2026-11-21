(defpackage #:clempo.history.tests
  (:use #:cl #:clempo.base #:clempo.history)
  (:local-nicknames (#:store #:clempo.store))
  (:import-from #:clempo.trie #:+empty-code-hash+))

(in-package #:clempo.history.tests)

;;; Random blocks over a small address and slot space, with deletes, wipes and code; after
;;; each block every account and slot is remembered, and every view of the history must
;;; answer exactly that.

(defparameter +addresses+ 10)
(defparameter +slots+ 6)

(defun random-block (db rng)
  (store:db-begin-block db)
  (dotimes (i 25)
    (let ((address (1+ (random +addresses+ rng))))
      (case (random 12 rng)
        ((0 1 2) (store:db-set-account db address (random 5 rng) (random 1000 rng)
                                       (if (zerop (random 3 rng))
                                           (store:db-put-code db (octets-of (random 256 rng) (random 256 rng)))
                                           +empty-code-hash+)))
        (3 (store:db-delete-account db address))
        (4 (store:db-wipe-storage db address))
        (t (store:db-set-storage db address (random +slots+ rng)
                                 (if (zerop (random 4 rng)) 0 (random (expt 2 256) rng))))))))

(defun block-hash (n) (integer->octets (+ #xabc000 n) 32))

(defun snapshot (db)
  "Every account and slot of the address space: ((found nonce balance code-hash storage-p slots...) ...)"
  (loop for address from 1 to +addresses+
        collect (let ((a (store:db-account db address)))
                  (list (and a t)
                        (if a (store:account-nonce a) 0)
                        (if a (store:account-balance a) 0)
                        (octets->hex (if a (store:account-code-hash a) +empty-code-hash+))
                        (and a (plusp (store:account-slot-count a)))
                        (loop for slot below +slots+ collect (store:db-storage db address slot))))))

(defun view-snapshot (view)
  (loop for address from 1 to +addresses+
        collect (multiple-value-bind (found nonce balance code-hash) (history-account view address)
                  (list found nonce balance (octets->hex code-hash)
                        (and found (not (history-storage-empty-p view address)))
                        (loop for slot below +slots+ collect (history-storage view address slot))))))

(defun temp-dir ()
  (format nil "/tmp/clempo-history-test-~D/" (random 100000000 (make-random-state t))))

(defun genesis-db ()
  (let ((db (store:make-statedb)))
    (store:db-begin-block db)
    (store:db-set-account db 1 0 100 (store:db-put-code db (octets-of 1 2 3)))
    (store:db-set-storage db 1 5 6)
    (store:db-set-storage db 2 1 7)
    (store:db-commit-block db 0 (block-hash 0))
    db))

(defun check-views (history snapshots from to)
  (loop for n from from to to
        do (let ((view (history-state history n)))
             (is view "no view at ~D" n)
             (when view
               (is (equal (aref snapshots n) (view-snapshot view)) "state after block ~D" n)))))

(defun build (dir db rng snapshots to &key (job 0) (flush-every 7))
  "Run blocks (db-number DB)+1..TO, recording each in a fresh writer of job JOB on DIR."
  (let ((history (open-history dir)))
    (let ((fresh (not (history-job-resume history job))))
      (history-start-writer history job db)
      (when fresh (history-record-state history db)))
    (setf (aref snapshots (store:db-number db)) (snapshot db))
    (loop for n from (1+ (store:db-number db)) to to
          do (random-block db rng)
             (history-record-block history db (store:db-commit-block db n (block-hash n)))
             (setf (aref snapshots n) (snapshot db))
             (when (zerop (mod n flush-every)) (history-flush history :db db)))
    history))

(deftest history-answers-every-block ()
  (let ((dir (temp-dir)) (rng (sb-ext:seed-random-state 21))
        (snapshots (make-array 200)))
    (unwind-protect
         (let* ((db (genesis-db))
                (history (build dir db rng snapshots 150)))
           ;; the tail (unflushed blocks) answers too
           (is (plusp (history-tail-blocks history)))
           (is= 150 (history-height history))
           (check-views history snapshots 0 150)
           (is (null (history-state history 151)))
           (is-equalp (octets-of 1 2 3) (history-code-by-hash history (clempo.keccak:keccak256 (octets-of 1 2 3))))
           ;; 21 flushes of 7 blocks: tiers of 8 merged
           (is (< (history-segment-count history) 12) "~D segments" (history-segment-count history))
           (history-flush history :db db)
           (close-history history)
           ;; reopened from disk, then compacted
           (let ((again (open-history dir)))
             (check-views again snapshots 0 150)
             (history-compact again)
             (is= 2 (history-segment-count again))
             (check-views again snapshots 0 150)
             (close-history again)))
      (sb-ext:delete-directory dir :recursive t))))

(deftest history-jobs-from-a-seed-and-resume ()
  (let ((dir (temp-dir)) (rng (sb-ext:seed-random-state 22))
        (snapshots (make-array 200)))
    (unwind-protect
         (let* ((db (genesis-db)))
           ;; job 0 covers 0..60; a second job starts from the state at 60 (a seed) and
           ;; covers 61..120; job 0's blocks up to 60 come first
           (let ((h0 (build dir db rng snapshots 60 :flush-every 9)))
             (history-flush h0 :db db)
             (close-history h0))
           (let ((h1 (build dir db rng snapshots 100 :job 60 :flush-every 6)))
             ;; the full segment at 60 overlaps job 0's last block; both agree
             (close-history h1))
           ;; resume job 60 from its checkpoint (at 96): blocks 97..100 were in the lost tail
           (let* ((h (open-history dir))
                  (resumed (history-job-resume h 60)))
             (is= 96 (store:db-number resumed))
             (is (equal (aref snapshots 96) (snapshot resumed)))
             (history-start-writer h 60 resumed)
             ;; re-run 97..120 with the blocks the first run made: replay them on the resumed state
             (let ((rng2 (sb-ext:seed-random-state 99)))
               (loop for n from 97 to 120
                     do (random-block resumed rng2)
                        (history-record-block h resumed (store:db-commit-block resumed n (block-hash n)))
                        (setf (aref snapshots n) (snapshot resumed))))
             (history-flush h :db resumed)
             (is= 120 (history-height h))
             (is (equal '((0 . 120)) (history-ranges h)) "~S" (history-ranges h))
             (check-views h snapshots 0 120)
             (close-history h)))
      (sb-ext:delete-directory dir :recursive t))))
