(defpackage #:clempo.dev.statetest
  (:use #:cl #:clempo.base #:clempo.dev)
  (:import-from #:clempo.io #:read-file-string)
  (:documentation "clempo-dev statetest and the R3e rung.")
  (:export #:run-state-test-file #:run-statetests #:*statetest-dir*))

(in-package #:clempo.dev.statetest)

;;; execution-spec-tests state tests (fixtures_develop v5.4.0) against clempo.evm.
;;; Each post entry runs one transaction on the pre state and must reproduce the post
;;; state (the fixture's "state" alloc, and its root once clempo.trie computes state
;;; roots), the logs hash, and validity (expectException). Forks before Istanbul are
;;; skipped: Tempo never ran them.

(defparameter *statetest-dir*
  (merge-pathnames "cache/clempo/fixtures/fixtures/state_tests/" (user-homedir-pathname)))

(defun j (obj key) (clempo.json:jget obj key))

(defun jhex (obj key &optional (default 0))
  (let ((v (j obj key))) (if (stringp v) (hex->integer v) default)))

(defun build-backend (pre)
  (let ((b (clempo.state:make-memory-backend)))
    (loop for (address . acct) in (cdr pre)
          for addr = (hex->integer address)
          do (clempo.state:memory-backend-put-account
              b addr :nonce (jhex acct "nonce") :balance (jhex acct "balance")
                     :code (hex->octets (j acct "code")))
             (loop for (slot . value) in (cdr (j acct "storage"))
                   do (clempo.state:memory-backend-put-storage b addr (hex->integer slot)
                                                               (hex->integer value))))
    b))

(defun fixture-tx (tx indexes)
  "The CLEMPO.EVM:EVM-TX of fixture transaction TX at post INDEXES."
  (let* ((di (j indexes "data")) (gi (j indexes "gas")) (vi (j indexes "value"))
         (access-lists (j tx "accessLists"))
         (access-list (and (vectorp access-lists) (< di (length access-lists))
                           (svref access-lists di)))
         (auths (j tx "authorizationList"))
         (blobs (j tx "blobVersionedHashes"))
         (to (j tx "to")))
    (clempo.evm:make-evm-tx
     :type (cond (auths 4) (blobs 3) ((j tx "maxFeePerGas") 2)
                 ((and access-list (not (eq access-list :null))) 1) (t 0))
     :nonce (jhex tx "nonce")
     :gas-limit (hex->integer (svref (j tx "gasLimit") gi))
     :gas-price (jhex tx "gasPrice")
     :max-fee (and (j tx "maxFeePerGas") (jhex tx "maxFeePerGas"))
     :max-priority-fee (and (j tx "maxPriorityFeePerGas") (jhex tx "maxPriorityFeePerGas"))
     :to (if (and (stringp to) (plusp (length to))) (hex->integer to) nil)
     :value (hex->integer (svref (j tx "value") vi))
     :data (hex->octets (svref (j tx "data") di))
     :access-list (when (vectorp access-list)
                    (loop for e across access-list
                          collect (cons (jhex e "address")
                                        (map 'list #'hex->integer (j e "storageKeys")))))
     :blob-hashes (when blobs (map 'list #'hex->integer blobs))
     :max-fee-per-blob-gas (jhex tx "maxFeePerBlobGas")
     :authorizations (when auths
                       (loop for a across auths
                             collect (clempo.evm:make-authorization
                                      :chain-id (jhex a "chainId") :address (jhex a "address")
                                      :nonce (jhex a "nonce") :y-parity (jhex a "yParity")
                                      ;; the fixture's "signer" is ignored: recovery is tested
                                      :r (jhex a "r") :s (jhex a "s"))))
     :sender (jhex tx "sender"))))

(defun fixture-spec (fork-name fork config)
  (let ((schedule (j (j config "blobSchedule") fork-name)))
    (if schedule
        (clempo.evm:make-spec fork :blob-target (jhex schedule "target") :blob-max (jhex schedule "max")
                                   :blob-update-fraction (jhex schedule "baseFeeUpdateFraction"))
        (clempo.evm:make-spec fork))))

(defun fixture-block (env spec config)
  (let ((excess (jhex env "currentExcessBlobGas")))
    (clempo.evm:make-block-env
     :number (jhex env "currentNumber") :timestamp (jhex env "currentTimestamp")
     :coinbase (jhex env "currentCoinbase") :gas-limit (jhex env "currentGasLimit")
     :base-fee (jhex env "currentBaseFee") :prevrandao (jhex env "currentRandom")
     :difficulty (jhex env "currentDifficulty")
     :chain-id (jhex config "chainid" 1)
     :excess-blob-gas excess
     :blob-base-fee (clempo.evm:blob-base-fee excess (max 1 (clempo.evm:spec-blob-update-fraction spec)))
     :block-hash-fn (lambda (n) (clempo.keccak:keccak256-integer (string->octets (princ-to-string n)))))))

(defun logs-hash (logs)
  (clempo.keccak:keccak256
   (clempo.rlp:rlp-encode
    (loop for l in logs
          collect (list (integer->octets (clempo.state:log-entry-address l) 20)
                        (loop for topic in (clempo.state:log-entry-topics l)
                              collect (integer->octets topic 32))
                        (clempo.state:log-entry-data l))))))

(defun state-diff (backend expected)
  "NIL when BACKEND holds exactly the accounts of the fixture alloc EXPECTED, else a
description of the first difference."
  (let ((accounts (clempo.state:memory-backend-accounts backend))
        (seen (make-hash-table)))
    (loop for (address . acct) in (cdr expected)
          for addr = (hex->integer address)
          for a = (gethash addr accounts)
          do (setf (gethash addr seen) t)
             (unless a (return-from state-diff (format nil "missing account ~A" address)))
             (unless (= (clempo.state:mem-account-nonce a) (jhex acct "nonce"))
               (return-from state-diff (format nil "~A nonce ~D want ~D" address
                                               (clempo.state:mem-account-nonce a) (jhex acct "nonce"))))
             (unless (= (clempo.state:mem-account-balance a) (jhex acct "balance"))
               (return-from state-diff (format nil "~A balance ~D want ~D" address
                                               (clempo.state:mem-account-balance a) (jhex acct "balance"))))
             (unless (octets= (clempo.state:mem-account-code-hash a)
                              (clempo.state:code-hash (hex->octets (j acct "code"))))
               (return-from state-diff (format nil "~A code differs" address)))
             (let ((want (cdr (j acct "storage")))
                   (have (clempo.state:mem-account-storage a)))
               (unless (= (length want) (hash-table-count have))
                 (return-from state-diff (format nil "~A has ~D slots want ~D" address
                                                 (hash-table-count have) (length want))))
               (loop for (slot . value) in want
                     unless (eql (gethash (hex->integer slot) have) (hex->integer value))
                       do (return-from state-diff
                            (format nil "~A slot ~A = ~X want ~A" address slot
                                    (gethash (hex->integer slot) have) value)))))
    (loop for addr being the hash-keys of accounts
          unless (gethash addr seen)
            do (return-from state-diff (format nil "extra account ~(~40,'0x~)" addr)))
    nil))

(defstruct st-result name fork index ok reason)

;;; With -trace, every case runs under a tracer that checks the hook protocol: frames
;;; nest, every TRACE-STEP is closed by its TRACE-STEP-END, and nothing is left open. This
;;; exercises the traced interpreter variant against the same expectations.

(defvar *trace-check* nil)

(defclass checking-tracer ()
  ((frames :initform '() :accessor ct-frames)
   (steps :initform '() :accessor ct-steps)
   (count :initform 0 :accessor ct-count)
   (problem :initform nil :accessor ct-problem)))

(defun ct-fail (tr fmt &rest args)
  (unless (ct-problem tr) (setf (ct-problem tr) (apply #'format nil fmt args))))

(defmethod clempo.evm:trace-call-start ((tr checking-tracer) frame)
  (unless (= (clempo.evm:frame-depth frame) (length (ct-frames tr)))
    (ct-fail tr "call-start at depth ~D with ~D open frames" (clempo.evm:frame-depth frame)
             (length (ct-frames tr))))
  (push frame (ct-frames tr)))

(defmethod clempo.evm:trace-call-end ((tr checking-tracer) frame status gas-left output)
  (declare (ignore status gas-left output))
  (unless (eq frame (pop (ct-frames tr))) (ct-fail tr "call-end of a frame that is not open")))

(defmethod clempo.evm:trace-step ((tr checking-tracer) frame pc op gas sp stack)
  (declare (ignore op gas sp stack))
  (unless (eq frame (first (ct-frames tr))) (ct-fail tr "step outside its frame"))
  (incf (ct-count tr))
  (push (cons frame pc) (ct-steps tr)))

(defmethod clempo.evm:trace-step-end ((tr checking-tracer) frame pc op gas sp stack status)
  (declare (ignore op gas sp stack status))
  (let ((open (pop (ct-steps tr))))
    (unless (and open (eq (car open) frame) (= (cdr open) pc))
      (ct-fail tr "step-end at pc ~D does not close the open step ~S" pc (cdr open)))))

(defun run-post-entry (pre tx env config fork-name fork post)
  "Run one post entry; returns NIL on success or a reason string."
  (let* ((backend (build-backend pre))
         (world (clempo.state:make-world backend))
         (clempo.evm:*spec* (fixture-spec fork-name fork config))
         (clempo.evm:*block* (fixture-block env clempo.evm:*spec* config))
         (clempo.evm:*tracer* (and *trace-check* (make-instance 'checking-tracer)))
         (etx (fixture-tx tx (j post "indexes")))
         (expect-invalid (j post "expectException"))
         (result nil)
         (invalid nil))
    (handler-case
        (setf result (clempo.evm:apply-transaction (make-instance 'clempo.evm:ethereum-handler) world etx))
      (clempo.evm:invalid-transaction (c) (setf invalid (clempo.evm:invalid-transaction-reason c))))
    (let ((tr clempo.evm:*tracer*))
      (when tr
        (when (or (ct-frames tr) (ct-steps tr))
          (ct-fail tr "~D frames and ~D steps left open" (length (ct-frames tr)) (length (ct-steps tr))))
        (when (ct-problem tr) (return-from run-post-entry (format nil "tracer: ~A" (ct-problem tr))))))
    (cond ((and expect-invalid (not invalid))
           (return-from run-post-entry (format nil "expected ~A, tx was valid" expect-invalid)))
          ((and invalid (not expect-invalid))
           (return-from run-post-entry (format nil "tx rejected ~A" invalid))))
    (clempo.state:apply-changes backend (clempo.state:drain-changes world))
    (let ((logs-hash (octets->hex (logs-hash (if result (clempo.evm:tx-result-logs result) '())))))
      (unless (string-equal logs-hash (j post "logs"))
        (return-from run-post-entry (format nil "logs hash ~A want ~A" logs-hash (j post "logs")))))
    (let ((diff (state-diff backend (j post "state"))))
      (when diff (return-from run-post-entry diff)))
    (let ((root (octets->hex (clempo.state:backend-state-root backend))))
      (unless (string-equal root (j post "hash"))
        (return-from run-post-entry (format nil "state root ~A want ~A" root (j post "hash")))))
    nil))

(defvar *debug* nil "When true, print a backtrace for a case that signals an error.")

(defun run-state-test-file (path &key fork-filter)
  "Run every Istanbul..Osaka case of the fixture file PATH; a list of ST-RESULT."
  (let ((doc (clempo.json:parse-json (read-file-string path)))
        (results '()))
    (loop for (name . test) in (cdr doc)
          do (let ((pre (j test "pre")) (tx (j test "transaction"))
                   (env (j test "env")) (config (j test "config")))
               (loop for (fork-name . posts) in (cdr (j test "post"))
                     for fork = (clempo.evm:parse-fork fork-name)
                     when (and fork (or (null fork-filter) (= fork fork-filter)))
                       do (loop for post across posts
                                for index from 0
                                do (let ((reason (handler-case
                                                     (handler-bind ((error (lambda (e)
                                                                             (when (and *debug* (not (typep e 'clempo.evm:invalid-transaction)))
                                                                               (format t "~&~A: ~A~%" name e)
                                                                               (sb-debug:print-backtrace :count 25)))))
                                                       (run-post-entry pre tx env config fork-name fork post))
                                                   (error (e) (format nil "error: ~A" e)))))
                                     (push (make-st-result :name name :fork fork-name :index index
                                                           :ok (null reason) :reason reason)
                                           results))))))
    (nreverse results)))


(defun statetest-files (target)
  (let ((p (probe-file target)))
    (cond ((null p) (error "no such file or directory: ~A" target))
          ((pathname-name p) (list p))
          (t (sort (directory (merge-pathnames "**/*.json" p)) #'string< :key #'namestring)))))

(defun run-statetests (files &key fork-filter (threads 12) (on-result nil) trace-check)
  "Run FILES on THREADS workers; returns all ST-RESULTs."
  (let ((queue (copy-list files))
        (lock (sb-thread:make-mutex :name "statetest"))
        (all '()))
    (flet ((worker ()
             (clempo.evm:with-evm-thread
              (let ((*trace-check* trace-check))
               (loop
                 (let ((file (sb-thread:with-mutex (lock) (pop queue))))
                   (unless file (return))
                   (let ((rs (handler-case (run-state-test-file file :fork-filter fork-filter)
                               (error (e)
                                 (list (make-st-result :name (namestring file) :fork "?" :index 0
                                                       :ok nil :reason (format nil "file error: ~A" e)))))))
                     (sb-thread:with-mutex (lock)
                       (when on-result (funcall on-result file rs))
                       (setf all (nconc rs all))))))))))
      (mapc #'sb-thread:join-thread
            (loop repeat (max 1 threads) collect (sb-thread:make-thread #'worker :name "statetest"))))
    all))

(defun summarize-statetests (results)
  (let ((by-fork (make-hash-table :test 'equal)))
    (dolist (r results)
      (let ((cell (or (gethash (st-result-fork r) by-fork)
                      (setf (gethash (st-result-fork r) by-fork) (cons 0 0)))))
        (incf (cdr cell))
        (when (st-result-ok r) (incf (car cell)))))
    by-fork))

(defcommand "statetest" (args)
  "run EEST state tests: statetest [dir-or-file] [-fork Osaka] [-threads N] [-trace] [-v]"
  (let* ((target (or (first (arg args :rest)) (namestring *statetest-dir*)))
         (fork-filter (let ((f (arg args :fork))) (and f (or (clempo.evm:parse-fork f)
                                                             (error "unknown fork ~A" f)))))
         (verbose (arg-flag args :v))
         (files (statetest-files target))
         (start (get-internal-real-time))
         (results (run-statetests files :fork-filter fork-filter
                                        :threads (arg-int args :threads 12)
                                        :trace-check (arg-flag args :trace)
                                        :on-result (lambda (file rs)
                                                     (declare (ignore file))
                                                     (dolist (r rs)
                                                       (unless (st-result-ok r)
                                                         (format t "FAIL ~A [~A #~D]: ~A~%"
                                                                 (st-result-name r) (st-result-fork r)
                                                                 (st-result-index r) (st-result-reason r))))
                                                     (when verbose (finish-output)))))
         (passed (count-if #'st-result-ok results))
         (by-fork (summarize-statetests results)))
    (dolist (f clempo.evm:*forks*)
      (let ((cell (gethash (string-capitalize f) by-fork)))
        (when cell (format t "~10A ~D/~D~%" (string-capitalize f) (car cell) (cdr cell)))))
    (format t "statetest: ~D/~D passed in ~,1F s~%" passed (length results)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (if (= passed (length results)) 0 1)))

(clempo.dev::defrung "R3e" (args)
  "execution-spec-tests v5.4.0 state tests, Istanbul..Osaka"
  (let* ((results (run-statetests (statetest-files (namestring *statetest-dir*))
                                  :threads (arg-int args :threads 12)))
         (passed (count-if #'st-result-ok results)))
    (clempo.json:jobject "score" (format nil "~D/~D" passed (length results))
                         "passed" passed "total" (length results)
                         "fixtures" "execution-spec-tests v5.4.0 fixtures_develop state_tests")))
