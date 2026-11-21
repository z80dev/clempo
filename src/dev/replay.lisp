(in-package #:clempo.dev)

;;; R3: replay mainnet from genesis through clempo.executor, state in a clempo.store datadir.
;;;
;;;   clempo-dev replay [-from N] [-to M] [-datadir D] [-checkpoint K] [-heal K] [-skip K]
;;;                     [-result file.json] [-diagnose] [-interactive] [-chain mainnet]
;;;
;;; A mismatching block stops in clempo.executor:block-mismatch. Non-interactive runs pick a
;;; restart by flag: heal from the reference (CLEMPO_RPC prestate diff) while -heal allows,
;;; skip while -skip allows, otherwise abort. With -interactive (or REPLAY called from a
;;; REPL) the condition reaches the debugger with RETRY-BLOCK, HEAL-FROM-REFERENCE,
;;; SKIP-BLOCK and ABORT-REPLAY. The datadir remembers whether its state was ever healed or
;;; skipped (replay.sexp), so R3 counts only the contiguous run from genesis.

(defparameter *r3-result-path* "~/scratch/clempo/r3/replay.json"
  "Where `replay` writes its result unless -result says otherwise; R3 reads it.")

(defun expand-home (path)
  (if (and (plusp (length path)) (char= (char path 0) #\~))
      (concatenate 'string (clempo.io:getenv "HOME") (subseq path 1))
      path))

;;; Blocks from the archive, one chunk cached.

(defun make-archive-source ()
  (let ((chunk nil) (first -1))
    (lambda (n)
      (let ((want (* clempo.archive:+chunk-blocks+ (floor n clempo.archive:+chunk-blocks+))))
        (unless (= want first)
          (setf chunk (clempo.archive:read-chunk want) first want))
        (clempo.blocks:decode-block (clempo.archive:chunk-block chunk n))))))

;;; Sender recovery dominates busy blocks (ecrecover per transaction), so a block's senders are
;;; recovered on several threads. Recovery reads only immutable tables; each worker still runs
;;; inside clempo.evm:with-evm-thread for its own Keccak scratch.

(defparameter *sender-threads* 8)

(defun recover-senders (block)
  (let* ((txs (coerce (clempo.blocks:block-transactions block) 'simple-vector))
         (n (length txs))
         (out (make-array n)))
    (if (< n 4)
        (dotimes (i n) (setf (svref out i) (clempo.transactions:transaction-sender (svref txs i))))
        (let* ((k (min *sender-threads* n))
               (threads
                 (loop for w below k
                       collect (let ((w w))
                                 (sb-thread:make-thread
                                  (lambda ()
                                    (clempo.evm:with-evm-thread
                                      (loop for i from w below n by k
                                            do (setf (svref out i)
                                                     (clempo.transactions:transaction-sender (svref txs i))))))
                                  :name "senders")))))
          (mapc #'sb-thread:join-thread threads)))
    (coerce out 'list)))

;;; The datadir's history: was every committed block matched?

(defun history-path (datadir)
  (merge-pathnames "replay.sexp" (concatenate 'string (string-right-trim "/" (expand-home datadir)) "/")))

(defun read-history (datadir)
  (let ((path (and datadir (history-path datadir))))
    (if (and path (probe-file path))
        (with-open-file (in path) (let ((*read-eval* nil)) (read in)))
        (list :heals '() :skips '()))))

(defun write-history (datadir history)
  (when datadir
    (clempo.io:write-file-atomically
     (history-path datadir)
     (string->octets (with-standard-io-syntax (prin1-to-string history))))))

;;; Reference data

(defun reference-prestate-diff (n)
  (clempo.jsonrpc:rpc-call "debug_traceBlockByNumber"
                           (list (quantity->hex n)
                                 (clempo.json:jobject "tracer" "prestateTracer"
                                                      "tracerConfig" (clempo.json:jobject "diffMode" :true)))))

;;; Opcode-level diagnosis: re-run the failing block on the parent state with a step recorder
;;; and compare each step's pc and gas with debug_traceTransaction's struct logs.

(defclass step-recorder ()
  ((steps :initform (make-array 4096 :adjustable t :fill-pointer 0) :reader recorder-steps)))

(defmethod clempo.evm:trace-step ((r step-recorder) frame pc op gas sp stack)
  (declare (ignore sp stack))
  (vector-push-extend (list pc op gas (clempo.evm:frame-depth frame)) (recorder-steps r)))

(defun record-tx-steps (db block index chain)
  "Steps of BLOCK's transaction INDEX executed on DB (the parent state); undoes the block."
  (let* ((clempo.executor:*chain* chain)
         (env (clempo.executor:header-block-env (clempo.blocks:block-header block) :chain chain))
         (txs (clempo.blocks:block-transactions block))
         (ctx (clempo.executor:begin-block db env :block block))
         (recorder (make-instance 'step-recorder)))
    (unwind-protect
         (progn
           (loop for tx in txs for i below index
                 do (clempo.executor:apply-transaction ctx tx (clempo.transactions:transaction-sender tx)))
           (let ((tx (nth index txs)) (clempo.evm:*tracer* recorder))
             (ignore-errors (clempo.executor:apply-transaction ctx tx (clempo.transactions:transaction-sender tx)))))
      (clempo.store:db-revert-block db))
    (recorder-steps recorder)))

(defun diagnose-opcodes (db block index &key (chain :mainnet))
  "The first step where our run of BLOCK's tx INDEX leaves the reference's (pc or gas)."
  (let* ((ours (record-tx-steps db block index chain))
         (hash (octets->hex (clempo.transactions:transaction-hash (nth index (clempo.blocks:block-transactions block)))))
         (trace (clempo.jsonrpc:rpc-call "debug_traceTransaction"
                                         (list hash (clempo.json:jobject "disableStorage" :true "disableStack" :true
                                                                         "disableMemory" :true))))
         (theirs (clempo.json:jget trace "structLogs")))
    (loop for i from 0
          for mine across ours
          for ref across theirs
          do (destructuring-bind (pc op gas depth) mine
               (unless (and (eql pc (clempo.json:jget ref "pc")) (eql gas (clempo.json:jget ref "gas")))
                 (let ((prev (when (plusp i) (aref theirs (1- i)))))
                   (return-from diagnose-opcodes
                     (format nil "step ~D depth ~D: ours pc ~D op 0x~2,'0X gas ~D; reference pc ~A op ~A gas ~A; previous reference step ~A cost ~A, ours ~S"
                             i depth pc op gas (clempo.json:jget ref "pc") (clempo.json:jget ref "op")
                             (clempo.json:jget ref "gas") (and prev (clempo.json:jget prev "op"))
                             (and prev (clempo.json:jget prev "gasCost"))
                             (when (plusp i) (aref ours (1- i)))))))))
    (format nil "all ~D of our steps agree with the reference's first ~D" (length ours) (length theirs))))

(defun diagnose-receipts (c)
  "Name the first transaction whose receipt differs from the reference's."
  (let ((result (clempo.executor:block-mismatch-result c)))
    (when result
      (let* ((n (clempo.executor:block-mismatch-number c))
             (reference (map 'list #'clempo.receipts:receipt-from-rpc
                             (clempo.jsonrpc:rpc-call "eth_getBlockReceipts" (list (quantity->hex n))))))
        (loop for ours in (clempo.executor:block-result-receipts result)
              for theirs in reference
              for i from 0
              do (flet ((differs (field a b)
                          (log-line "diagnose: block ~D tx ~D: ~(~A~) is ~A, reference ~A" n i field a b)
                          (return-from diagnose-receipts (list i field a b))))
                   (unless (eql (clempo.receipts:receipt-status ours) (clempo.receipts:receipt-status theirs))
                     (differs :status (clempo.receipts:receipt-status ours) (clempo.receipts:receipt-status theirs)))
                   (unless (eql (clempo.receipts:receipt-cumulative-gas-used ours)
                                (clempo.receipts:receipt-cumulative-gas-used theirs))
                     (differs :cumulative-gas-used (clempo.receipts:receipt-cumulative-gas-used ours)
                              (clempo.receipts:receipt-cumulative-gas-used theirs)))
                   (unless (equalp (clempo.receipts:encode-receipt ours) (clempo.receipts:encode-receipt theirs))
                     (differs :logs (length (clempo.receipts:receipt-logs ours))
                              (length (clempo.receipts:receipt-logs theirs))))))
        (log-line "diagnose: block ~D: every receipt matches the reference; the state differs" n)
        nil))))

;;; The command

(defun json-height (x) (or x :null))

(defun replay (&key from to datadir (checkpoint 0) (heal 0) (skip 0) result-path diagnose
                    interactive (chain :mainnet))
  "Run a replay; returns the result JSON object. Callable from a REPL (pass :interactive t to
get the restarts in the debugger)."
  (let* ((clempo.executor:*chain* chain)
         (source (make-archive-source))
         (start (get-internal-real-time))
         (store (when datadir
                  (clempo.store:open-store (expand-home datadir)
                                           :genesis (lambda (db) (clempo.executor:load-genesis db :chain chain))
                                           :interval checkpoint
                                           :log (lambda (fmt &rest args) (apply #'log-line fmt args)))))
         (db (if store (clempo.store:store-db store) (clempo.executor:genesis-db :chain chain)))
         (history (read-history datadir))
         (from (or from (1+ (clempo.store:db-number db))))
         (to (or to (clempo.archive:archive-contiguous-height)))
         (heals 0) (skips 0) (diagnosis nil)
         (last-log (get-internal-real-time)) (last-n from)
         (result nil))
    (log-line "replay: ~D..~D from state at ~D (~A), datadir ~A" from to (clempo.store:db-number db)
              (octets->hex (clempo.store:db-root-hash db)) (or datadir "none"))
    (flet ((progress (n outcome)
             (declare (ignore outcome))
             (let ((now (get-internal-real-time)))
               (when (or (zerop (mod n 100000))
                         (> (- now last-log) (* 30 internal-time-units-per-second)))
                 (let ((secs (/ (- now last-log) internal-time-units-per-second)))
                   (log-line "replay: block ~D, ~,0F blocks/s" n (/ (- n last-n) (max secs 1/1000))))
                 (setf last-log now last-n n))))
           (choose (c)
             (log-line "replay: ~A" c)
             (when (and diagnose (typep c 'clempo.executor:block-mismatch))
               (setf diagnosis (ignore-errors (diagnose-receipts c))))
             ;; Any failure inside a block (a mismatch or an executor error) can be healed: a
             ;; survey lists every failure class in one run.
             (cond ((and (< heals heal) (find-restart 'clempo.executor:heal-from-reference c))
                    (incf heals) (invoke-restart 'clempo.executor:heal-from-reference))
                   ((and (typep c 'clempo.executor:block-mismatch) (< skips skip))
                    (incf skips) (invoke-restart 'clempo.executor:skip-block))
                   (t (invoke-restart 'clempo.executor:abort-replay c)))))
      (unwind-protect
           (flet ((run ()
                    (clempo.executor:replay-blocks
                     db from to
                     :chain chain
                     :block-fn source
                     :senders-fn #'recover-senders
                     :commit-fn (when store (lambda (diff) (clempo.store:store-commit-block store diff)))
                     :heal-fn (lambda (db n block)
                                (clempo.executor:heal-block db block (reference-prestate-diff n) :chain chain))
                     :progress-fn #'progress)))
             (setf result (if interactive
                              (run)
                              (handler-bind ((error #'choose)) (run))))
             ;; Opcode diagnosis of the stopping block, on the parent state the abort restored.
             (let ((c (clempo.executor:replay-result-stopped result)))
               (when (and diagnose diagnosis (typep c 'clempo.executor:block-mismatch))
                 (log-line "diagnose: ~A"
                           (handler-case (diagnose-opcodes db (funcall source (clempo.executor:block-mismatch-number c))
                                                           (first diagnosis) :chain chain)
                             (error (e) (format nil "opcode diagnosis failed: ~A" e)))))))
        (when store (clempo.store:close-store store))))
    (let* ((secs (max 1/1000 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
           (r result)
           (prior-heals (getf history :heals))
           (prior-skips (getf history :skips))
           (all-heals (append prior-heals (clempo.executor:replay-result-heals r)))
           (all-skips (append prior-skips (clempo.executor:replay-result-skips r)))
           (first-bad (let ((bad (append all-heals all-skips))) (when bad (reduce #'min bad))))
           ;; contiguous from genesis: no earlier heal or skip, and this run's matched run
           (height (if first-bad
                       (1- first-bad)
                       (clempo.executor:replay-result-matched-through r)))
           (gas (clempo.executor:replay-result-gas r))
           (blocks (clempo.executor:replay-result-blocks r))
           (json (clempo.json:jobject
                  "from" from "to" to
                  "last" (clempo.executor:replay-result-last r)
                  "matched_through" (clempo.executor:replay-result-matched-through r)
                  "r3_height" height
                  "heals" (coerce (clempo.executor:replay-result-heals r) 'simple-vector)
                  "skips" (coerce (clempo.executor:replay-result-skips r) 'simple-vector)
                  "datadir" (or datadir :null)
                  "blocks" blocks
                  "txs" (clempo.executor:replay-result-txs r)
                  "gas" gas
                  "seconds" (float secs 1d0)
                  "blocks_per_s" (float (/ blocks secs) 1d0)
                  "mgas_per_s" (float (/ gas secs 1000000) 1d0)
                  "stop" (or (clempo.executor:replay-result-stop-reason r) :null)
                  "diagnosis" (if diagnosis (format nil "~{~A~^ ~}" diagnosis) :null))))
      (when datadir
        (write-history datadir (list :heals all-heals :skips all-skips)))
      (let ((path (expand-home (or result-path *r3-result-path*))))
        (ensure-directories-exist path)
        (clempo.io:write-file-atomically path (clempo.json:to-json-octets json)))
      (log-line "replay: ~D blocks, ~D txs in ~,1F s (~,0F blocks/s, ~,2F Mgas/s); last ~D, R3 height ~D~@[; stopped: ~A~]"
                blocks (clempo.executor:replay-result-txs r) secs (/ blocks secs) (/ gas secs 1000000)
                (clempo.executor:replay-result-last r) height (clempo.executor:replay-result-stop-reason r))
      json)))

(defcommand "replay" (args)
  "R3: replay [-from N] [-to M] [-datadir D] [-checkpoint K] [-heal K] [-skip K] [-result F] [-diagnose] [-interactive]"
  (let ((json (replay :from (arg-int args :from) :to (arg-int args :to)
                      :datadir (arg args :datadir)
                      :checkpoint (arg-int args :checkpoint 0)
                      :heal (arg-int args :heal 0)
                      :skip (arg-int args :skip 0)
                      :result-path (arg args :result)
                      :diagnose (arg-flag args :diagnose)
                      :interactive (arg-flag args :interactive)
                      :chain (intern (string-upcase (arg args :chain "mainnet")) :keyword))))
    (if (member (clempo.json:jget json "stop") '(nil :null)) 0 1)))

(defrung "R3" (args)
  "highest mainnet block N such that 1..N executed from genesis match (from the last replay result)"
  (let ((path (expand-home (arg args :result *r3-result-path*))))
    (if (probe-file path)
        (let ((json (clempo.json:parse-json (clempo.io:read-file-string path))))
          (clempo.json:jobject "score" (clempo.json:jget json "r3_height")
                               "source" path
                               "last" (clempo.json:jget json "last")
                               "stop" (clempo.json:jget json "stop")))
        (clempo.json:jobject "score" 0 "source" path "stop" "no replay result"))))
