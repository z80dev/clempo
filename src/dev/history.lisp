(in-package #:clempo.dev)

;;; Historical state (clempo.history): build it by replaying the archive, inspect it, time it.
;;;
;;;   clempo-dev history-build [-dir D] [-seed checkpoint.state] [-to M] [-interval K]
;;;   clempo-dev history-info [-dir D]
;;;   clempo-dev history-bench [-dir D] [-n N]
;;;   clempo-dev history-compact [-dir D] [-job J]
;;;
;;; A build job replays blocks from genesis (job 0) or from a seed checkpoint (a clempo.store
;;; checkpoint such as R3's, job = its block) and records every block's changes; it writes the
;;; job's whole starting state as a full segment first, flushes a segment and a resume
;;; checkpoint every K blocks, and resumes from that checkpoint when run again. Jobs from
;;; different seeds run in parallel on one directory; a job stops before the next job's seed.
;;; Only blocks that match their headers (gas, receipts, bloom, state root) enter the history.

(defparameter *history-dir* "~/cache/clempo/history/mainnet")

(defun history-dir-arg (args) (expand-home (arg args :dir *history-dir*)))

(defun next-seed-after (history job)
  "The first block of another job's full segment after JOB, or NIL."
  (find-if (lambda (from) (> from job)) (clempo.history:history-seeds history)))

(defun history-build (&key dir seed to (interval 500000) (chain :mainnet)
                           (sender-threads +default-sender-threads+))
  "Run a history build job; returns the replay result."
  (let* ((clempo.executor:*chain* chain)
         (history (clempo.history:open-history dir))
         (seed-db (when seed (clempo.store:read-checkpoint (expand-home seed))))
         (job (if seed-db (clempo.store:db-number seed-db) 0))
         (resumed (clempo.history:history-job-resume history job))
         (db (or resumed seed-db (clempo.executor:genesis-db :chain chain)))
         (source (make-archive-source))
         (stop (let ((next (next-seed-after history job)))
                 (min (or to most-positive-fixnum)
                      (if next (1- next) most-positive-fixnum)
                      (clempo.archive:archive-contiguous-height))))
         (start (get-internal-real-time))
         (last-log start) (last-n (clempo.store:db-number db)))
    (clempo.history:history-start-writer history job db)
    (unless resumed
      (log-line "history: job ~D: full state at ~D (~D accounts)" job (clempo.store:db-number db)
                (clempo.store:db-account-count db))
      (clempo.history:history-record-state history db))
    (log-line "history: job ~D: blocks ~D..~D into ~A" job (1+ (clempo.store:db-number db)) stop dir)
    (labels ((commit (diff)
               (clempo.history:history-record-block history db diff)
               (when (>= (clempo.history:history-tail-blocks history) interval)
                 (clempo.history:history-flush history :db db)
                 ;; a job seeded since this one started ends this one there; its seed segment
                 ;; is on disk, not in this job's view until refreshed
                 (clempo.history:history-refresh history)
                 (let ((next (next-seed-after history job)))
                   (when next (setf stop (min stop (1- next)))))))
             (block-at (n)
               (when (> n stop) (throw 'history-stop nil))
               (funcall source n))
             (progress (n outcome)
               (declare (ignore outcome))
               (let ((now (get-internal-real-time)))
                 (when (or (zerop (mod n 100000)) (> (- now last-log) (* 60 internal-time-units-per-second)))
                   (log-line "history: block ~D, ~,0F blocks/s, ~D segments, tail ~D versions" n
                             (/ (- n last-n) (max 1/1000 (/ (- now last-log) internal-time-units-per-second)))
                             (clempo.history:history-segment-count history)
                             (clempo.history:history-tail-versions history))
                   (setf last-log now last-n n)))))
      (let ((result
              (catch 'history-stop
               (call-with-recovered-senders
               (lambda (recover)
                 (handler-bind ((error (lambda (c)
                                         (log-line "history: stopping: ~A" c)
                                         (let ((r (find-restart 'clempo.executor:abort-replay c)))
                                           (when r (invoke-restart r c))))))
                   (clempo.executor:replay-blocks db (1+ (clempo.store:db-number db)) stop
                                                  :chain chain :block-fn #'block-at :senders-fn recover
                                                  :commit-fn #'commit :progress-fn #'progress)))
               sender-threads))))
        (clempo.history:history-flush history :db db)
        (log-line "history: job ~D at ~D (~A) in ~,0F s; covers ~S" job (clempo.store:db-number db)
                  (or (and result (clempo.executor:replay-result-stop-reason result))
                      "reached the end")
                  (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                  (clempo.history:history-ranges history))
        (clempo.history:close-history history)
        result))))

(defcommand "history-build" (args)
  "replay blocks recording historical state: history-build [-dir D] [-seed checkpoint] [-to M] [-interval K]"
  (history-build :dir (history-dir-arg args) :seed (arg args :seed) :to (arg-int args :to)
                 :interval (arg-int args :interval 500000)
                 :chain (intern (string-upcase (arg args :chain "mainnet")) :keyword))
  0)

(defun directory-bytes (dir)
  (loop for p in (directory (merge-pathnames "*.*" dir))
        sum (with-open-file (in p :element-type '(unsigned-byte 8)) (file-length in))))

(defcommand "history-info" (args)
  "describe a history directory: history-info [-dir D]"
  (let* ((dir (history-dir-arg args))
         (history (clempo.history:open-history dir)))
    (format t "~A~%ranges ~S~%segments ~D~%bytes ~:D~%" dir (clempo.history:history-ranges history)
            (clempo.history:history-segment-count history)
            (directory-bytes (clempo.history:history-dir history)))
    (dolist (s (clempo.history::history-segments history))
      (format t "  ~A ~:D versions ~:D bytes~%" (file-namestring (clempo.history::segment-path s))
              (clempo.history::segment-versions s) (clempo.history::segment-length s)))
    0))

(defcommand "history-compact" (args)
  "merge contiguous segments: history-compact [-dir D] [-job J] (not on a job that is running)"
  (let ((history (clempo.history:open-history (history-dir-arg args))))
    (if (arg args :job)
        (clempo.history:history-compact history :job (arg-int args :job))
        (clempo.history:history-compact history))
    (format t "segments ~D~%" (clempo.history:history-segment-count history))
    0))

(defcommand "history-bench" (args)
  "time historical lookups at random blocks: history-bench [-dir D] [-n N]"
  (let* ((history (clempo.history:open-history (history-dir-arg args)))
         (n (arg-int args :n 100000))
         (rng (sb-ext:seed-random-state 42))
         (keys (clempo.history::sample-keys history 4096 rng))
         (ranges (clempo.history:history-ranges history))
         (hits 0))
    (when (or (null keys) (null ranges)) (format t "empty history~%") (return-from command 1))
    (flet ((random-block ()
             (let ((r (nth (random (length ranges) rng) ranges)))
               (+ (car r) (random (1+ (- (cdr r) (car r))) rng)))))
      (let ((views (loop repeat 64 collect (clempo.history:history-state history (random-block))))
            (start (get-internal-real-time)))
        (dotimes (i n)
          (let ((view (nth (random 64 rng) views))
                (key (svref keys (random (length keys) rng))))
            (if (< key (ash 1 416))
                (when (clempo.history:history-account view key) (incf hits))
                (unless (zerop (clempo.history:history-storage view (ldb (byte 160 256) key) (ldb (byte 256 0) key)))
                  (incf hits)))))
        (let ((secs (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
          (format t "~:D lookups (~:D found) in ~,2F s: ~,2F us each; ~D segments~%" n hits secs
                  (/ (* secs 1d6) n) (clempo.history:history-segment-count history)))))
    0))
