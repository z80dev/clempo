(in-package #:clempo.dev)

(defparameter *p2p-reference-key*
  (hex->octets "eb67e74b3f7331f5c7b79341aa7e2b7ca0ea1904f3e895178abfa950df1b70e416bbf86bb5df22337904a5cec6a17c519545caad2f2cb7edfed3285503f42fd3"))

(defcommand "p2p-sync" (args)
  "download and verify inclusive blocks: -from A -to B [-host 127.0.0.1 -port 30311 -node-key HEX -version 69 -archive DIR]"
  (let* ((from (arg-int args :from)) (to (arg-int args :to))
         (host (arg args :host "127.0.0.1")) (port (arg-int args :port 30311))
         (key (if (arg args :node-key) (hex->octets (arg args :node-key)) *p2p-reference-key*))
         (version (arg-int args :version))
         (dir (if (arg args :archive) (namestring (uiop:ensure-directory-pathname (arg args :archive)))
                  (clempo.archive:archive-dir))))
    (unless (and (typep from 'u64) (typep to 'u64) (<= from to))
      (log-line "p2p-sync: specify an inclusive range with -from A -to B")
      (return-from command 2))
    (let ((peer (clempo.eth:eth-dial host port key :versions (if version (list version) '(69 68))))
          (chunk nil))
      (unwind-protect
           (let ((start (get-internal-real-time)))
             (log-line "p2p-sync: negotiated eth/~D with ~A:~D" (clempo.eth:eth-version peer) host port)
             (let* ((count (clempo.eth:download-blocks
                            peer from to
                            (lambda (n header body)
                              (let ((first (* clempo.archive:+chunk-blocks+
                                              (floor n clempo.archive:+chunk-blocks+))))
                                (unless (and chunk (= first (clempo.archive:chunk-first chunk)))
                                  (setf chunk (clempo.archive:read-chunk first dir)))
                                (unless (clempo.eth:block-matches-p (clempo.archive:chunk-block chunk n) header body)
                                  (error "Block ~D differs from archived RLP" n))))))
                    (seconds (/ (- (get-internal-real-time) start) (float internal-time-units-per-second 1d0))))
               (format t "eth/~D: ~:D blocks (~:D..~:D) matched archive byte for byte in ~,3F s (~,0F blocks/s)~%"
                       (clempo.eth:eth-version peer) count from to seconds (/ count (max seconds 1d-6)))
               0))
        (clempo.eth:eth-close peer)))))
