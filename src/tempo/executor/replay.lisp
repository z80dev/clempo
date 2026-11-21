(in-package #:clempo.executor)

;;; Replay: execute a range of blocks on a state database, check each against its header,
;;; and stop at the first difference in a BLOCK-MISMATCH condition. The restarts:
;;;
;;;   RETRY-BLOCK          undo the block and execute it again (after redefining code in
;;;                        the live image)
;;;   HEAL-FROM-REFERENCE  undo the block and apply the reference's post-state for it
;;;                        instead (HEAL-FN); the replay is then no longer contiguous
;;;   SKIP-BLOCK           keep the state as executed (unchanged if execution failed) and go
;;;                        on; the replay is no longer contiguous
;;;   ABORT-REPLAY         undo the block and return
;;;
;;; An error that is not a mismatch (a bug in the executor) reaches the debugger with the
;;; same restarts established. Non-interactive callers choose with HANDLER-BIND.

(defstruct replay-result
  (from 0) (to 0)
  (last 0)                  ; last block committed to the state database
  (matched-through 0)       ; last block of the contiguous matching run from FROM - 1
  (heals '()) (skips '())   ; block numbers, oldest first
  (blocks 0) (txs 0) (gas 0)
  (stopped nil)             ; the condition that stopped the run, if any
  (stop-reason nil))

(defun replay-blocks (db from to &key block-fn senders-fn commit-fn heal-fn progress-fn
                                     (chain *chain*))
  "Execute blocks FROM..TO on DB, whose head must be block FROM - 1.
BLOCK-FN maps a number to a decoded block. SENDERS-FN, when given, maps a block to its
recovered senders. COMMIT-FN receives each committed block's diff (for the store).
HEAL-FN is called with (db number block) inside the open block to write the reference's
post-state. PROGRESS-FN receives (number block-result) after each matching block.
Returns a REPLAY-RESULT."
  (invariant (= (store:db-number db) (1- from)) "replay from ~D needs state at ~D, have ~D"
             from (1- from) (store:db-number db))
  (let ((result (make-replay-result :from from :to to :last (1- from) :matched-through (1- from)))
        (contiguous t)
        (executed nil)                  ; the current block ran to its end
        (n from))
    (labels ((undo () (when (store:db-block-open-p db) (store:db-revert-block db)))
             (commit (block)
               (let ((diff (store:db-commit-block db n (blocks:block-hash block))))
                 (when commit-fn (funcall commit-fn diff))
                 (setf (replay-result-last result) n)))
             (signal-mismatch (number fields &key tx-index detail outcome)
               (destructuring-bind (field expected actual) (first fields)
                 (error 'block-mismatch :number number :tx-index tx-index :field field
                                        :expected expected :actual actual :detail detail
                                        :fields fields :result outcome)))
             (run (block)
               (let* ((header (blocks:block-header block))
                      (outcome
                        (handler-case
                            (execute-block db block :chain chain
                                                    :senders (when senders-fn (funcall senders-fn block)))
                          (execution-failure (c)
                            (signal-mismatch n (list (list :execution nil (type-of c)))
                                      :tx-index (execution-failure-tx-index c)
                                      :detail (execution-failure-reason c)))))
                      (fields (progn (setf executed t) (block-mismatches outcome header))))
                 (when fields (signal-mismatch n fields :outcome outcome))
                 (commit block)
                 (when contiguous (setf (replay-result-matched-through result) n))
                 (incf (replay-result-blocks result))
                 (incf (replay-result-txs result) (block-result-tx-count outcome))
                 (incf (replay-result-gas result) (block-result-gas-used outcome))
                 (when progress-fn (funcall progress-fn n outcome)))))
      (loop while (<= n to)
            do (let ((block (funcall block-fn n)))
                 (setf executed nil)
                 (invariant (octets= (headers:ethereum-header-parent-hash
                                      (headers:header-inner (blocks:block-header block)))
                                     (store:db-hash db))
                            "block ~D does not extend the state's head ~D" n (store:db-number db))
                 (restart-case (progn (run block) (incf n))
                   (retry-block ()
                     :report (lambda (s) (format s "Undo block ~D and execute it again." n))
                     (undo))
                   (heal-from-reference ()
                     :report (lambda (s) (format s "Apply the reference's post-state for block ~D and continue." n))
                     :test (lambda (c) (declare (ignore c)) heal-fn)
                     (undo)
                     (store:db-begin-block db)
                     (funcall heal-fn db n block)
                     (let ((root (store:db-state-root db))
                           (expected (headers:ethereum-header-state-root
                                      (headers:header-inner (blocks:block-header block)))))
                       (unless (octets= root expected)
                         (undo)
                         (error 'block-mismatch :number n :field :state-root :expected expected
                                                :actual root :detail "after healing from the reference")))
                     (commit block)
                     (setf contiguous nil)
                     (push n (replay-result-heals result))
                     (incf n))
                   (skip-block ()
                     :report (lambda (s) (format s "Keep block ~D's state as executed and continue." n))
                     ;; A block whose execution failed part-way keeps the parent state.
                     (unless executed (undo))
                     (unless (store:db-block-open-p db) (store:db-begin-block db))
                     (commit block)
                     (setf contiguous nil)
                     (push n (replay-result-skips result))
                     (incf n))
                   (abort-replay (&optional condition)
                     :report (lambda (s) (format s "Undo block ~D and stop the replay." n))
                     (undo)
                     (setf (replay-result-stopped result) condition
                           (replay-result-stop-reason result)
                           (when condition (princ-to-string condition)))
                     (loop-finish)))))
      (setf (replay-result-heals result) (nreverse (replay-result-heals result))
            (replay-result-skips result) (nreverse (replay-result-skips result)))
      result)))

;;; Healing from the reference: debug_traceBlockByNumber with the prestateTracer in diff mode
;;; (evm2 inspectors geth_prestate_diff_traces). Per transaction, POST holds the changed fields
;;; of each changed account, PRE its fields before; an account only in PRE was destroyed; a
;;; slot only in PRE became zero. Accounts empty before the transaction are absent from PRE.

(defun json-quantity (x)
  (etypecase x (integer x) (string (hex->integer x))))

(defun apply-prestate-diff (db pre post)
  "Write one transaction's PRE/POST (JSON objects keyed by address) into the open block."
  (flet ((entries (obj) (when (clempo.json:jobject-p obj) (cdr obj))))
    (loop for (address-hex . state) in (entries pre)
          for address = (hex->integer address-hex)
          unless (clempo.json:jget post address-hex)
            do (store:db-delete-account db address))
    (loop for (address-hex . state) in (entries post)
          for address = (hex->integer address-hex)
          for old = (store:db-account db address)
          for code = (clempo.json:jget state "code")
          do (store:db-set-account
              db address
              (let ((v (clempo.json:jget state "nonce"))) (if v (json-quantity v) (if old (store:account-nonce old) 0)))
              (let ((v (clempo.json:jget state "balance"))) (if v (json-quantity v) (if old (store:account-balance old) 0)))
              (cond (code (let ((bytes (hex->octets code)))
                            (if (zerop (length bytes)) clempo.trie:+empty-code-hash+ (store:db-put-code db bytes))))
                    (old (store:account-code-hash old))
                    (t clempo.trie:+empty-code-hash+)))
             (let ((before (clempo.json:jget (clempo.json:jget pre address-hex) "storage"))
                   (after (clempo.json:jget state "storage")))
               (loop for (slot . nil) in (entries before)
                     unless (clempo.json:jget after slot)
                       do (store:db-set-storage db address (hex->integer slot) 0))
               (loop for (slot . value) in (entries after)
                     do (store:db-set-storage db address (hex->integer slot) (hex->integer value)))))))

(defun heal-block (db block traces &key (chain *chain*))
  "Inside the open block: run BLOCK's pre-execution system changes (fork markers, the
EIP-4788/2935 calls, zone installs) as we execute them, then the reference's per-transaction
prestate diffs TRACES (the parsed debug_traceBlockByNumber result, a vector of {result: {pre,
post}}), then the post-execution committee call. The system calls are not in the traces."
  (let* ((*chain* chain)
         (env (header-block-env (blocks:block-header block) :chain chain))
         (ctx (begin-block db env :block block :db-block nil)))
    (with-block-context (ctx)
      (write-changes db (state:drain-changes (ctx-world ctx))))
    (loop for trace across traces
          for result = (clempo.json:jget trace "result")
          do (apply-prestate-diff db (clempo.json:jget result "pre") (clempo.json:jget result "post")))
    ;; A fresh world reads the healed state for the post-execution call.
    (setf (ctx-world ctx) (state:make-world db))
    (with-block-context (ctx)
      (apply-committee-call ctx)
      (write-changes db (state:drain-changes (ctx-world ctx))))))
