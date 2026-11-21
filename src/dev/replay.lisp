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
             (cond ((and (typep c 'clempo.executor:block-mismatch) (< heals heal))
                    (incf heals) (invoke-restart 'clempo.executor:heal-from-reference))
                   ((and (typep c 'clempo.executor:block-mismatch) (< skips skip))
                    (incf skips) (invoke-restart 'clempo.executor:skip-block))
                   (t (invoke-restart 'clempo.executor:abort-replay c)))))
      (unwind-protect
           (flet ((run ()
                    (clempo.executor:replay-blocks
                     db from to
                     :chain chain
                     :block-fn (make-archive-source)
                     :commit-fn (when store (lambda (diff) (clempo.store:store-commit-block store diff)))
                     :heal-fn (lambda (db n block)
                                (clempo.executor:heal-block db block (reference-prestate-diff n) :chain chain))
                     :progress-fn #'progress)))
             (setf result (if interactive
                              (run)
                              (handler-bind ((error #'choose)) (run)))))
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
