(in-package #:clempo.dev)

(defstruct r1-archive-counts
  (blocks 0 :type integer) (failed 0 :type integer) (transactions 0 :type integer)
  (recoveries 0 :type integer) (bytes 0 :type integer))

(defun r1-archive-check-block (raw number reference counts)
  "Full byte/hash/root/sender parity against an independent Rust RPC block.
Receipts and execution are outside this archive codec/recovery check."
  (when (typep reference 'error) (error reference))
  (invariant (and (listp reference) (eq (first reference) :object))
             "Reference block ~D unavailable" number)
  (let* ((block (clempo.blocks:decode-block raw))
         (header (clempo.blocks:block-header block))
         (txs (clempo.blocks:block-transactions block))
         (rpc-txs (clempo.json:jget reference "transactions")))
    (invariant (= number (clempo.headers:header-number header)
                  (hex->integer (clempo.json:jget reference "number"))) "Block number mismatch")
    (invariant (octets= raw (clempo.blocks:encode-block block)) "Block byte roundtrip mismatch")
    (invariant (octets= (clempo.blocks:block-hash block)
                       (hex->octets (clempo.json:jget reference "hash"))) "Block hash mismatch")
    (let ((mismatches (clempo.blocks:block-body-mismatches block)))
      (invariant (null mismatches) "Block body commitments: ~S" mismatches))
    (invariant (and (vectorp rpc-txs) (= (length txs) (length rpc-txs)))
               "Transaction count mismatch")
    (incf (r1-archive-counts-transactions counts) (length txs))
    (loop for tx in txs for ref across rpc-txs for index from 0 do
      (invariant (octets= (clempo.transactions:transaction-hash tx)
                         (hex->octets (clempo.json:jget ref "hash")))
                 "Transaction ~D hash mismatch" index)
      (multiple-value-bind (sender key-id) (clempo.transactions:transaction-sender tx)
        ;; The legacy all-zero system signature has a defined zero sender, not a
        ;; cryptographic recovery. It is still compared to the reference below.
        (unless (and (zerop (clempo.transactions:transaction-type tx))
                     (zerop (clempo.transactions:transaction-r tx))
                     (zerop (clempo.transactions:transaction-s tx)))
          (incf (r1-archive-counts-recoveries counts)))
        (invariant (= sender (hex->integer (clempo.json:jget ref "from")))
                   "Transaction ~D sender mismatch" index)
        (when (= (clempo.transactions:transaction-type tx) #x76)
          (let ((expected (clempo.json:jget* ref "signature" "keyId")))
            (invariant (eql key-id (when (stringp expected) (hex->integer expected)))
                       "Transaction ~D key ID mismatch" index)))))
    t))

(defun r1-archive-run (from to threads batch directory url result-path &key checkpoint)
  "Check FROM..TO in parallel, committing completed chunks atomically for resumption.
Every failure is persisted. Interrupted chunks are rechecked, never counted twice;
checkpoint counters and elapsed processing time exclude uncommitted trailing work."
  (invariant (and (<= 0 from to) (plusp threads) (plusp batch)) "Invalid archive scan arguments")
  (let* ((size clempo.archive:+chunk-blocks+)
         (queue (loop for first from (* size (floor from size)) to to by size collect first))
         (chunk-total (length queue)) (chunks 0) (chunk-errors 0) (stop nil)
         (completed (make-hash-table :test #'eql))
         (total (make-r1-archive-counts))
         (prior-seconds 0d0) (prior-cpu 0d0) (prior-bytes 0)
         (lock (sb-thread:make-mutex :name "R1 archive results"))
         (started (clempo.io:monotonic-ticks)) (cpu-start (clempo.io:process-cpu-ticks))
         (bytes-start (sb-ext:get-bytes-consed))
         (failures-path (concatenate 'string result-path ".failures.jsonl"))
         (chunks-path (concatenate 'string result-path ".chunks.jsonl"))
         (progress *error-output*))
    (when checkpoint
      (invariant (and (eql 1 (clempo.json:jget checkpoint "checkpointVersion"))
                      (eql from (clempo.json:jget checkpoint "from"))
                      (eql to (clempo.json:jget checkpoint "to"))
                      (equal directory (clempo.json:jget checkpoint "archiveDirectory"))
                      (equal url (clempo.json:jget checkpoint "reference")))
                 "Checkpoint version, range, archive or reference differs")
      (let ((coverage 0) (done (clempo.json:jget checkpoint "completedChunks")))
        (invariant (vectorp done) "Checkpoint has no completed chunk list")
        (loop for first across done do
          (invariant (and (integerp first) (<= (car queue) first to) (zerop (mod first size))
                          (not (gethash first completed))) "Invalid checkpoint chunk ~S" first)
          (setf (gethash first completed) t)
          (incf coverage (1+ (- (min to (+ first size -1)) (max from first)))))
        (invariant (eql coverage (clempo.json:jget checkpoint "checkedBlocks"))
                   "Checkpoint counters disagree with completed chunk coverage"))
      (setf chunks (hash-table-count completed)
            queue (remove-if (lambda (first) (gethash first completed)) queue)
            (r1-archive-counts-blocks total) (clempo.json:jget checkpoint "checkedBlocks")
            (r1-archive-counts-failed total) (clempo.json:jget checkpoint "failedBlocks")
            (r1-archive-counts-transactions total) (clempo.json:jget checkpoint "transactions")
            (r1-archive-counts-recoveries total) (clempo.json:jget checkpoint "cryptographicRecoveries")
            (r1-archive-counts-bytes total) (clempo.json:jget checkpoint "rawBytes")
            prior-seconds (clempo.json:jget checkpoint "seconds")
            prior-cpu (clempo.json:jget checkpoint "cpuSeconds")
            prior-bytes (clempo.json:jget checkpoint "bytesAllocated")))
    (unless checkpoint
      (invariant (not (probe-file result-path)) "Result already exists; use -resume or another path"))
    (ensure-directories-exist result-path)
    (with-open-file (failures failures-path :direction :output :if-exists (if checkpoint :append :error)
                                          :if-does-not-exist (if checkpoint :error :create))
      (with-open-file (chunk-log chunks-path :direction :output :if-exists (if checkpoint :append :error)
                                            :if-does-not-exist (if checkpoint :error :create))
        (labels
            ((summary (finished)
               (let* ((seconds (+ prior-seconds
                                  (float (/ (- (clempo.io:monotonic-ticks) started)
                                            internal-time-units-per-second) 1d0)))
                      (n (r1-archive-counts-blocks total))
                      (bad (r1-archive-counts-failed total))
                      (recovered (r1-archive-counts-recoveries total)))
                 (clempo.json:jobject
                  "checkpointVersion" 1 "archiveDirectory" directory
                  "completedChunks" (coerce (sort (loop for first being the hash-keys of completed
                                                        collect first) #'<) 'vector)
                  "status" (if finished (if (and (= n (1+ (- to from))) (zerop chunk-errors))
                                            "complete" "incomplete") "running")
                  "from" from "to" to "expectedBlocks" (1+ (- to from))
                  "checkedBlocks" n "passedBlocks" (- n bad) "failedBlocks" bad
                  "transactions" (r1-archive-counts-transactions total)
                  "cryptographicRecoveries" recovered "rawBytes" (r1-archive-counts-bytes total)
                  "chunksCompleted" chunks "chunksTotal" chunk-total "chunkErrors" chunk-errors
                  "threads" threads "batch" batch "seconds" seconds
                  "cpuSeconds" (+ prior-cpu (float (/ (- (clempo.io:process-cpu-ticks) cpu-start)
                                                        internal-time-units-per-second) 1d0))
                  "bytesAllocated" (+ prior-bytes (- (sb-ext:get-bytes-consed) bytes-start))
                  "blocksPerSecond" (/ n (max seconds 0.000001d0))
                  "sendersPerSecond" (/ recovered (max seconds 0.000001d0))
                  "reference" url "referenceMethod" "eth_getBlockByNumber(full), every block"
                  "timing" "committed checkpoints: archive I/O, RPC, JSON, codecs, roots and recovery; interrupted trailing work excluded"
                  "failuresFile" failures-path "chunksFile" chunks-path)))
             (save-summary (finished)
               (let ((report (summary finished)))
                 (clempo.io:write-file-atomically
                  result-path (string->octets (clempo.json:to-json-string report)))
                 report))
             (record-failure (row)
               ;; Caller holds LOCK. Flushing makes a failure durable before continuing.
               (let ((json (clempo.json:to-json-string row)))
                 (write-line json failures) (finish-output failures)
                 (format progress "~&R1-ARCHIVE-FAIL ~A~%" json) (finish-output progress)))
             (work ()
               (loop
                 (let ((first (sb-thread:with-mutex (lock) (unless stop (pop queue)))))
                   (unless first (return))
                   (let* ((lo (max from first)) (hi (min to (+ first size -1)))
                          (counts (make-r1-archive-counts)) (complete nil))
                     (handler-case
                         (let ((chunk (clempo.archive:read-chunk first directory)))
                           (invariant (= size (clempo.archive:chunk-count chunk)) "Incomplete chunk ~D" first)
                           (loop for start from lo to hi by batch do
                             (let* ((end (min hi (+ start batch -1)))
                                    (refs (clempo.jsonrpc:rpc-batch
                                           (loop for n from start to end
                                                 collect (list "eth_getBlockByNumber"
                                                               (list (quantity->hex n) :true)))
                                           :url url)))
                               (invariant (= (length refs) (1+ (- end start))) "RPC batch response count")
                               (loop for n from start to end for ref in refs do
                                 (incf (r1-archive-counts-blocks counts))
                                 (handler-case
                                     (let ((raw (clempo.archive:chunk-block chunk n)))
                                       (incf (r1-archive-counts-bytes counts) (length raw))
                                       (r1-archive-check-block raw n ref counts))
                                   (error (condition)
                                     (incf (r1-archive-counts-failed counts))
                                     (sb-thread:with-mutex (lock)
                                       (record-failure
                                        (clempo.json:jobject "event" "block-failure" "block" n
                                                             "error" (princ-to-string condition)))))))))
                           (setf complete t))
                       (error (condition)
                         (sb-thread:with-mutex (lock)
                           (setf stop t) (incf chunk-errors)
                           (record-failure
                            (clempo.json:jobject "event" "chunk-error" "from" lo "to" hi
                                                 "error" (princ-to-string condition))))))
                     (sb-thread:with-mutex (lock)
                       (when complete
                         (incf (r1-archive-counts-blocks total) (r1-archive-counts-blocks counts))
                         (incf (r1-archive-counts-failed total) (r1-archive-counts-failed counts))
                         (incf (r1-archive-counts-transactions total) (r1-archive-counts-transactions counts))
                         (incf (r1-archive-counts-recoveries total) (r1-archive-counts-recoveries counts))
                         (incf (r1-archive-counts-bytes total) (r1-archive-counts-bytes counts))
                         (setf (gethash first completed) t)
                         (incf chunks))
                       (write-line
                        (clempo.json:to-json-string
                         (clempo.json:jobject "first" lo "last" hi "complete" (if complete :true :false)
                                              "checkedBlocks" (r1-archive-counts-blocks counts)
                                              "failedBlocks" (r1-archive-counts-failed counts)
                                              "transactions" (r1-archive-counts-transactions counts)
                                              "cryptographicRecoveries" (r1-archive-counts-recoveries counts)
                                              "rawBytes" (r1-archive-counts-bytes counts)))
                        chunk-log)
                       (finish-output chunk-log)
                       (let ((report (save-summary nil)))
                         (when (or stop (zerop (mod chunks 10)) (= chunks chunk-total))
                           (format progress "~&R1 archive: ~D/~D chunks, ~D blocks, ~D failures, ~,1F blocks/s, ~,1F senders/s~%"
                                   chunks chunk-total (r1-archive-counts-blocks total)
                                   (r1-archive-counts-failed total)
                                   (clempo.json:jget report "blocksPerSecond")
                                   (clempo.json:jget report "sendersPerSecond"))
                           (finish-output progress)))))))))
          (save-summary nil)
          (let ((workers nil))
            (unwind-protect
                 (dotimes (i threads)
                   (push (sb-thread:make-thread #'work :name (format nil "R1 archive ~D" i)) workers))
              ;; Do not leave workers writing to streams after this invocation returns.
              (dolist (worker workers) (sb-thread:join-thread worker))))
          (save-summary t))))))

(defcommand "r1-archive" (args)
  "check raw archive against Rust RPC [-from 0] [-to archive-tip] [-threads 6] [-batch 500] [-dir D] [-url U] [-result FILE] [-resume]"
  (let* ((directory (or (arg args :dir) (clempo.archive:archive-dir)))
         (directory (concatenate 'string (string-right-trim "/" directory) "/"))
         (tip (clempo.archive:archive-contiguous-height directory))
         (result (or (arg args :result)
                     (format nil "~A/scratch/clempo/r1-archive/~D.json"
                             (clempo.io:getenv "HOME") (clempo.io:now-seconds))))
         (checkpoint (when (arg-flag args :resume)
                       (invariant (arg args :result) "-resume requires an explicit -result path")
                       (clempo.json:parse-json (clempo.io:read-file-string result))))
         (from (arg-int args :from (if checkpoint (clempo.json:jget checkpoint "from") 0)))
         (to (arg-int args :to (if checkpoint (clempo.json:jget checkpoint "to") tip))))
    (invariant (<= 0 from to tip) "Requested range is outside archive 0..~D" tip)
    (let ((report (r1-archive-run from to (arg-int args :threads 6) (arg-int args :batch 500)
                                  directory (or (arg args :url) (clempo.jsonrpc:local-rpc-url)) result
                                  :checkpoint checkpoint)))
      (write-line (clempo.json:to-json-string report))
      (if (and (string= "complete" (clempo.json:jget report "status"))
               (zerop (clempo.json:jget report "failedBlocks"))) 0 1))))
