(in-package #:clempo.dev)

(defun probe-devnet (manifest slot seconds)
  (let ((seen (make-hash-table :test #'equalp))
        (counts (make-array 6 :initial-element 0))
        (by-peer (make-hash-table :test #'equalp)))
    (apply
     #'clempo.validator:call-with-validator-network
     (lambda (network)
       (clempo.validator:network-track
        network 0 (loop for other across (clempo.validator:manifest-slots manifest) unless (eq other slot)
                        collect (list (clempo.validator:slot-public-key other) "127.0.0.1"
                                      (clempo.validator:slot-port other))))
       (let ((deadline (+ (clempo.io:monotonic-ticks) (* seconds internal-time-units-per-second))))
         (loop for remaining = (/ (- deadline (clempo.io:monotonic-ticks)) internal-time-units-per-second)
               while (plusp remaining)
               for event = (clempo.validator:network-receive network :timeout (min remaining 1))
               when event do
                 (let ((key (clempo.validator:network-event-peer event)))
                   (setf (gethash key seen) t)
                   (when (eq (clempo.validator:network-event-kind event) :data)
                     (let ((channel (clempo.validator:network-event-channel event)))
                       (incf (aref counts channel))
                       (incf (aref (or (gethash key by-peer)
                                       (setf (gethash key by-peer) (make-array 6 :initial-element 0)))
                                   channel))))))))
     :seed (clempo.validator:read-slot-seed slot) :listen-host "127.0.0.1"
     :listen-port (clempo.validator:slot-port slot) (clempo.validator:local-network-options))
    (let ((all-peers (loop for peer across (clempo.validator:manifest-slots manifest)
                           unless (eq peer slot) collect peer)))
      (values
       (clempo.json:jobject "slot" (clempo.validator:slot-number slot) "seconds" seconds
                            "authenticated_peers" (hash-table-count seen)
                            "votes" (aref counts 0) "certificates" (aref counts 1)
                            "broadcast_blocks" (aref counts 3) "dkg" (aref counts 5)
                            "peers"
                            (coerce (loop for peer in all-peers
                                          for key = (clempo.validator:slot-public-key peer)
                                          collect (clempo.json:jobject "slot" (clempo.validator:slot-number peer)
                                                                       "authenticated" (clempo.json:jbool (gethash key seen))
                                                                       "channels" (gethash key by-peer #(0 0 0 0 0 0))))
                                    'vector))
       (and (= (length all-peers) (hash-table-count seen)) (plusp (aref counts 0)) (plusp (aref counts 3)))))))

(defcommand "devnet-probe" (args)
  "observe authenticated local consensus traffic: -manifest PATH -slot N [-seconds 20]"
  (let ((path (arg args :manifest)) (number (arg-int args :slot)) (seconds (arg-int args :seconds 20)))
    (unless (and (stringp path) (typep number '(integer 0 *)) (typep seconds '(integer 1 3600)))
      (log-line "devnet-probe: specify -manifest PATH -slot N and a positive duration")
      (return-from command 2))
    (let ((manifest (clempo.validator:read-devnet-manifest path)))
      (multiple-value-bind (report ok)
          (probe-devnet manifest (clempo.validator:manifest-slot manifest number) seconds)
        (write-line (clempo.json:to-json-string report))
        (if ok 0 1)))))

(defun mixed-devnet-proof (path expected)
  (when path
    (let ((summary nil))
      (with-open-file (in path)
        (loop for line = (read-line in nil) while line
              for record = (clempo.json:parse-json line)
              when (string= "summary" (clempo.json:jget record "kind" ""))
                do (when summary (error "Multiple devnet summaries in ~A" path))
                   (setf summary record)))
      (let ((slots (clempo.json:jget summary "clempo_slots")))
        (unless (and (simple-vector-p slots) (= (length slots) expected)
                     (= (length (remove-duplicates slots)) expected)
                     (every (lambda (slot) (typep slot '(integer 0 3))) slots)
                     (eq :true (clempo.json:jget summary "quorum"))
                     (typep (clempo.json:jget summary "compared") '(integer 1 *))
                     (typep (clempo.json:jget summary "boundary_outcomes") '(integer 1 *))
                     (every (lambda (slot)
                              (let ((key (write-to-string slot)))
                                (and (typep (clempo.json:jget* summary "clempo_proposals" key) '(integer 1 *))
                                     (typep (clempo.json:jget* summary "clempo_reshare_epochs" key) '(integer 1 *)))))
                            slots))
          (error "Report ~A does not demonstrate ~D clempo signers needed for quorum through a reshare" path expected))
        summary))))

(defrung "R6-devnet" (args)
  "validate mixed-validator evidence (not a live run): -one REPORT.jsonl and/or -two REPORT.jsonl from scripts/devnet; see docs/DEVNET.md"
  (unless (or (arg args :one) (arg args :two))
    (score-input-error "Produce mixed-validator reports with scripts/devnet (docs/DEVNET.md), then supply -one REPORT.jsonl and/or -two REPORT.jsonl."))
  (let* ((usage "Use -one REPORT.jsonl for one clempo signer or -two REPORT.jsonl for two.")
         (one (when (arg args :one) (mixed-devnet-proof (required-score-option args :one usage) 1)))
         (two (when (arg args :two) (mixed-devnet-proof (required-score-option args :two usage) 2))))
    (clempo.json:jobject "score" (+ (if one 1 0) (if two 1 0))
                         "one" (or one :null) "two" (or two :null))))
