;;; From the checkout root on macOS (time -l reports peak resident bytes):
;;; CLEMPO_BUILD=release /usr/bin/time -l sbcl --dynamic-space-size 16384 --noinform --non-interactive \
;;;   --load scripts/build.lisp --eval '(asdf:load-system "clempo-store")' \
;;;   --load src/node/workers.lisp --load tests/trie/state-benchmark.lisp \
;;;   --eval '(clempo.trie.state-benchmark:run)'
(defpackage #:clempo.trie.state-benchmark
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:store #:clempo.store) (#:trie #:clempo.trie) (#:json #:clempo.json)
                    (#:workers #:clempo.node.workers))
  (:export #:run))

(in-package #:clempo.trie.state-benchmark)

(defconstant +hot-storage-owners+ 32)
(defconstant +warm-storage-owners+ 4096)
(defconstant +bulk-block-size+ 32768)

(defun slot-layout (slots)
  "Synthetic skew: hot owners hold 3/4 of all slots; warm owners share the rest."
  (let ((layout (make-array (+ 1 +hot-storage-owners+ +warm-storage-owners+))))
    (flet ((allocate (start owners count)
             (multiple-value-bind (per-owner extras) (floor count owners)
               (dotimes (i owners)
                 (setf (svref layout (+ start i)) (+ per-owner (if (< i extras) 1 0)))))))
      (let ((hot (floor (* 3 slots) 4)))
        (allocate 1 +hot-storage-owners+ hot)
        (allocate (1+ +hot-storage-owners+) +warm-storage-owners+ (- slots hot))))
    layout))

(defun commit-state (db)
  (let ((number (1+ (store:db-number db))))
    (store:db-commit-block db number (integer->octets number 32))))

(defun load-state (db accounts layout)
  "Populate the real flat tables and their secure tries in bounded batches."
  (loop for first from 1 to accounts by +bulk-block-size+
        do (store:db-begin-block db)
           (loop for address from first below (min (1+ accounts) (+ first +bulk-block-size+))
                 do (store:db-set-account db address 1 address trie:+empty-code-hash+))
           (commit-state db))
  (let ((pending 0))
    (loop for address from 1 below (length layout)
          do (loop for slot from 1 to (svref layout address)
                   do (when (zerop pending) (store:db-begin-block db))
                      (store:db-set-storage db address slot (1+ (logxor address slot)))
                      (when (= (incf pending) +bulk-block-size+)
                        (commit-state db)
                        (setf pending 0))))
    (when (plusp pending) (commit-state db)))
  (invariant (= accounts (store:db-account-count db)))
  (store:db-root-hash db))

(defun apply-dirty-set (db accounts layout sample repeat)
  ;; The fixture counts distinct addresses and slots over the whole block. Map
  ;; ranks to distinct simulated owners, and rotate them and their slot keys
  ;; between blocks. Values change but no key is deleted, so counts stay fixed.
  (let* ((height (svref sample 0)) (dirty-accounts (svref sample 1))
         (owners (svref sample 2)) (seed (+ height (* repeat 97559)))
         (value (+ accounts seed)) (plain-owners (- accounts (1- (length layout)))))
    (loop for dirty across owners for rank from 0
          for address = (if (< rank +hot-storage-owners+)
                            (1+ (mod (+ seed rank) +hot-storage-owners+))
                            (+ 1 +hot-storage-owners+
                               (mod (+ seed (- rank +hot-storage-owners+)) +warm-storage-owners+)))
          for slots = (svref layout address)
          for start = (mod (* 4093 seed) slots)
          do (invariant (<= dirty slots) "fixture wants ~D dirty slots at ~D, owner only has ~D"
                        dirty address slots)
             (dotimes (i dirty)
               (store:db-set-storage db address (1+ (mod (+ start i) slots)) value)))
    (dotimes (i (- dirty-accounts (length owners)))
      (let ((address (+ (length layout) (mod (+ seed i) plain-owners))))
        (store:db-set-account db address value address trie:+empty-code-hash+)))))

(defun percentile (sorted percent)
  (if (zerop (length sorted)) 0
      (aref sorted (max 0 (1- (ceiling (* (length sorted) percent) 100))))))

(defun us (ticks)
  (float (/ (* ticks 1000000) internal-time-units-per-second) 1d0))

(defun measure-root-updates (db accounts layout workload repeat)
  (let* ((n (* repeat (length workload)))
         (latencies (make-array n :element-type 'fixnum))
         (dirty-latencies (make-array n :element-type 'fixnum :fill-pointer 0))
         (max-slots 0) (elapsed 0) (allocated 0) (index 0)
         (base (store:db-state-root db))
         (roots (make-array n)))
    ;; Exactly the same committed starting state for every worker count.
    (store:with-speculative-blocks (db)
      (dotimes (i repeat)
        (loop for sample across workload
              do (store:db-begin-block db)
                 (apply-dirty-set db accounts layout sample i)
                 (let ((bytes-before (sb-ext:get-bytes-consed))
                       (start (get-internal-real-time)))
                   (setf (svref roots index) (store:db-state-root db))
                   (let ((ticks (- (get-internal-real-time) start)))
                     (incf elapsed ticks)
                     (incf allocated (- (sb-ext:get-bytes-consed) bytes-before))
                     (setf (aref latencies index) ticks)
                     (when (plusp (svref sample 1)) (vector-push ticks dirty-latencies))))
                 (setf max-slots (max max-slots (reduce #'+ (svref sample 2))))
                 (invariant (octets= (svref roots index) (store:block-diff-root (commit-state db))))
                 (incf index))))
    (invariant (octets= base (store:db-state-root db)) "speculation did not restore the starting trie")
    (sort latencies #'<)
    (sort dirty-latencies #'<)
    (values (json:jobject "blocks" n "dirtyBlocks" (length dirty-latencies) "maxDirtySlots" max-slots
                          "meanUs" (us (/ elapsed n)) "p50Us" (us (percentile latencies 50))
                          "p95Us" (us (percentile latencies 95)) "maxUs" (us (percentile latencies 100))
                          "dirtyP50Us" (us (percentile dirty-latencies 50))
                          "dirtyP95Us" (us (percentile dirty-latencies 95))
                          "meanRootBytesConsed" (round allocated n)
                          "lastRoot" (octets->hex (svref roots (1- n)) :prefix t))
            roots)))

(defun run (&key (repetitions 4) (fixture "tests/trie/data/mainnet-dirty-sets.json"))
  (check-type repetitions (integer 1 *))
  (let* ((data (json:parse-json (clempo.io:read-file-string fixture)))
         (accounts (json:jget data "accounts")) (slots (json:jget data "slots"))
         (layout (slot-layout slots)) (db (store:make-statedb))
         (names '("tip" "busyUniform" "busyHeaviest")) (serial nil))
    (sb-ext:gc :full t)
    (let* ((before (sb-kernel:dynamic-usage))
           (start (get-internal-real-time))
           (root (workers:call-with-workers
                  (lambda (mapper)
                    (trie:call-with-trie-hashing (lambda () (load-state db accounts layout))
                                                 :parallel-map mapper)) :threads 4))
           (setup (/ (us (- (get-internal-real-time) start)) 1d6)))
      (sb-ext:gc :full t)
      (write-line (json:to-json-string
                   (json:jobject "accounts" accounts "slots" slots "setupSeconds" setup
                                 "retainedLispBytes" (- (sb-kernel:dynamic-usage) before)
                                 "baseRoot" (octets->hex root :prefix t))))
      (dolist (threads '(1 4))
        (workers:call-with-workers
         (lambda (mapper)
           (trie:call-with-trie-hashing
            (lambda ()
              (dolist (name names)
                (multiple-value-bind (result roots)
                    (measure-root-updates db accounts layout (json:jget data name) repetitions)
                  (if (= threads 1) (push (cons name roots) serial)
                      (invariant (equalp roots (cdr (assoc name serial :test #'string=)))
                                 "serial and parallel roots differ for ~A" name))
                  (write-line (json:to-json-string
                               (append (json:jobject "workload" name "threads" threads) (cdr result))))
                  (force-output))))
            :parallel-map mapper)) :threads threads)))))
