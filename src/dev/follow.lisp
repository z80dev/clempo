(in-package #:clempo.dev)

(defcommand "follow" (args)
  "follow [-last N] [-upstream URL] [-certify [-cert-rpc URL]] [-once | -seconds N] [-archive DIR | -no-archive] [-json]"
  (let* ((url (arg args :upstream (clempo.jsonrpc:local-rpc-url)))
         (tip (clempo.follow:rpc-head :latest :url url))
         (distance (arg-int args :last 1000))
         (once (arg-flag args :once))
         (seconds (arg-int args :seconds))
         (archive (arg args :archive (clempo.archive:archive-dir)))
         (start (get-internal-real-time)))
    (check-type distance (integer 0 *))
    (when (and once seconds) (error "Choose either -once or -seconds"))
    (let* ((number (max 0 (- (clempo.follow:head-number tip) distance)))
           (anchor (if (= number (clempo.follow:head-number tip)) tip
                       (clempo.follow:rpc-head number :url url)))
           (certifier (when (arg-flag args :certify)
                        (clempo.follow.certs:make-mainnet-certifier
                         anchor :upstream url
                         :url (arg args :cert-rpc clempo.jsonrpc:+mainnet-rpc+))))
           (follower (clempo.follow:make-follower
                      anchor :rpc-url url :archive-dir archive :certifier certifier
                      :write-archive (not (arg-flag args :no-archive))
                      :batch-size (arg-int args :batch 32)))
           (next-report start)
           (max-lag 0)
           (caught-up nil))
      (log-line "follow: anchor ~D, initial tip ~D (headers and bodies~:[~; and certificates~], no execution)"
                number (clempo.follow:head-number tip) certifier)
      (let* ((head (clempo.follow:follow-live
                    follower :tip tip :once once :seconds seconds
                    :on-poll (lambda (f remote)
                               (let* ((local (clempo.follow:follower-head f))
                                      (lag (- (clempo.follow:head-number remote)
                                              (clempo.follow:head-number local)))
                                      (clock (get-internal-real-time)))
                                 (when (zerop lag) (setf caught-up t))
                                 (when caught-up (setf max-lag (max max-lag lag)))
                                 (when (>= clock next-report)
                                   (log-line "follow: head ~D, lag ~D, verified ~D blocks"
                                             (clempo.follow:head-number local) lag
                                             (clempo.follow:follower-blocks f))
                                   (setf next-report (+ clock (* 30 internal-time-units-per-second))))))))
             (elapsed (round (* 1000 (- (get-internal-real-time) start))
                             internal-time-units-per-second)))
        (if (arg-flag args :json)
            (write-line
             (clempo.json:to-json-string
              (clempo.json:jobject "anchor" number "head" (clempo.follow:head-number head)
                                  "hash" (octets->hex (clempo.follow:head-hash head) :prefix t)
                                  "blocks" (clempo.follow:follower-blocks follower)
                                  "transactions" (clempo.follow:follower-transactions follower)
                                  "certified" (clempo.follow:follower-certified follower)
                                  "chunks" (clempo.follow:follower-chunks follower)
                                  "maxLagAfterCatchup" max-lag "elapsedMs" elapsed)))
            (format t "Verified ~D blocks, head ~D (~A)~%"
                    (clempo.follow:follower-blocks follower) (clempo.follow:head-number head)
                    (octets->hex (clempo.follow:head-hash head) :prefix t))))
      0)))
