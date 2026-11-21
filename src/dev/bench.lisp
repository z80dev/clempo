(in-package #:clempo.dev)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defparameter *bench-ranges*
  '(("early" 1 1000000) ("busy" 9890000 9909999) ("recent" 49410000 49449999)))

(defun bench-peak-rss ()
  ;; Darwin reports bytes, Linux reports KiB. This is a process lifetime peak.
  #+(or darwin linux)
  (multiple-value-bind (ok user system peak) (sb-unix:unix-getrusage sb-unix:rusage_self)
    (declare (ignore user system))
    (if ok (* peak #+darwin 1 #+linux 1024) :null))
  #-(or darwin linux) :null)

(defun bench-seconds (ticks)
  (/ ticks (float internal-time-units-per-second 1d0)))

(defun bench-run (db store from to profile sender-threads &key (profile-mode :cpu) (profile-threads :all))
  (let* ((wall (get-internal-real-time)) (cpu (get-internal-run-time))
         (gc sb-ext:*gc-run-time*) (bytes (sb-ext:get-bytes-consed))
         (result nil))
    (when profile
      (sb-sprof:reset)
      (sb-sprof:start-profiling :mode profile-mode :sample-interval 0.005 :max-samples 400000
                               :threads (if (eq profile-threads :main) (list sb-thread:*current-thread*) :all)))
    (unwind-protect
         (handler-bind ((error (lambda (condition)
                                 (let ((restart (find-restart 'clempo.executor:abort-replay condition)))
                                   (when restart (invoke-restart restart condition))))))
           (setf result (call-with-block-pipeline
                         (lambda (block-fn senders-fn)
                           (clempo.executor:replay-blocks
                            db from to :chain :mainnet :block-fn block-fn
                            :senders-fn senders-fn
                            :commit-fn (when store (lambda (diff) (clempo.store:store-commit-block store diff)))))
                         (make-archive-source)
                         sender-threads))
           (when store (clempo.store:store-sync store)))
      (when profile (sb-sprof:stop-profiling)))
    (let ((secs (max (bench-seconds (- (get-internal-real-time) wall)) 1d-6))
          (cpu-secs (bench-seconds (- (get-internal-run-time) cpu)))
          (gc-secs (bench-seconds (- sb-ext:*gc-run-time* gc)))
          (consed (- (sb-ext:get-bytes-consed) bytes))
          (rss (bench-peak-rss)))
      (when profile
        (ensure-directories-exist profile)
        (with-open-file (out profile :direction :output :if-exists :supersede)
          (sb-sprof:report :type :graph :max 150 :stream out)))
      (values result secs cpu-secs gc-secs consed rss))))

(defcommand "bench" (args)
  "R7: bench -range busy|early|recent [-parent SAVED_DIR -datadir FRESH_DIR] [-sender-threads N] [-result FILE] [-profile FILE [-profile-mode cpu|alloc] [-profile-threads all|main]]"
  (let* ((range (arg args :range)) (spec (assoc range *bench-ranges* :test #'equal))
         (parent (arg args :parent)) (datadir (arg args :datadir)) (profile (arg args :profile))
         (profile-mode (if (equal (arg args :profile-mode "cpu") "alloc") :alloc :cpu))
         (profile-threads (if (equal (arg args :profile-threads "all") "main") :main :all))
         (sender-threads (arg-int args :sender-threads +default-sender-threads+)))
    (unless spec (error "Expected -range busy, early or recent, got ~S" range))
    (check-type sender-threads (integer 1 16))
    (destructuring-bind (name from to) spec
      (let* ((path (expand-home (arg args :result (format nil "~~/scratch/clempo/bench/clempo-~A.json" name))))
             (parent-height (1- from))
             (parent-block (clempo.blocks:decode-block (clempo.archive:raw-block parent-height)))
             (root (clempo.headers:header-state-root (clempo.blocks:block-header parent-block)))
             (store nil))
        (when (and (plusp parent-height) (not (and parent datadir)))
          (error "~A requires -parent and -datadir at block ~D" name parent-height))
        (when (and parent (not datadir)) (error "-parent requires a fresh -datadir"))
        (when datadir
          (setf datadir (uiop:ensure-directory-pathname (expand-home datadir)))
          (when (probe-file datadir) (error "Benchmark datadir must not exist: ~A" datadir))
          (when parent
            (setf parent (uiop:ensure-directory-pathname (expand-home parent)))
            (unless (probe-file (merge-pathnames "MANIFEST" parent))
              (error "Not a saved clempo.store datadir: ~A" parent))
            (ensure-directories-exist (uiop:pathname-parent-directory-pathname datadir))
            (uiop:run-program (list "cp" "-R" (namestring parent) (namestring datadir))
                              :error-output *error-output*))
          (setf store (clempo.store:open-store datadir :genesis #'clempo.executor:load-genesis)))
        (unwind-protect
             (let ((db (if store (clempo.store:store-db store) (clempo.executor:genesis-db))))
               (unless (and (= parent-height (clempo.store:db-number db))
                            (octets= root (clempo.store:db-root-hash db))
                            (octets= (clempo.blocks:block-hash parent-block) (clempo.store:db-hash db)))
                 (error "Saved state does not match canonical parent ~D" parent-height))
               (multiple-value-bind (result secs cpu gc consed peak)
                   (bench-run db store from to (and profile (expand-home profile)) sender-threads
                              :profile-mode profile-mode :profile-threads profile-threads)
                 (let* ((blocks (clempo.executor:replay-result-blocks result))
                        (gas (clempo.executor:replay-result-gas result))
                        (stop (clempo.executor:replay-result-stop-reason result))
                        (ok (and (null stop) (= blocks (1+ (- to from)))
                                 (= to (clempo.executor:replay-result-matched-through result))))
                        (json (clempo.json:jobject
                               "range" name "from" from "to" to
                               "parent_root" (octets->hex root)
                               "last" (clempo.executor:replay-result-last result)
                               "complete" (if ok :true :false) "stop" (or stop :null)
                               "profiled" (if profile :true :false)
                               "sender_threads" sender-threads
                               "persistent" (if store :true :false)
                               "blocks" blocks "txs" (clempo.executor:replay-result-txs result) "gas" gas
                               "seconds" secs "blocks_per_s" (/ blocks secs) "mgas_per_s" (/ gas secs 1e6)
                               "cpu_seconds" cpu "gc_seconds" gc "peak_rss_bytes" peak "bytes_consed" consed)))
                   (ensure-directories-exist path)
                   (clempo.io:write-file-atomically path (clempo.json:to-json-octets json))
                   (log-line "bench ~A: ~D blocks in ~,3F s (~,0F blocks/s, ~,2F Mgas/s), ~A"
                             name blocks secs (/ blocks secs) (/ gas secs 1e6) path)
                   (unless ok (log-line "bench stopped at ~D: ~A" (clempo.executor:replay-result-last result) stop))
                   (if ok 0 1))))
          (when store (clempo.store:close-store store)))))))
