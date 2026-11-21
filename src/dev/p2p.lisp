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
           (let ((start (clempo.io:monotonic-ticks)))
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
                    (seconds (/ (- (clempo.io:monotonic-ticks) start) (float internal-time-units-per-second 1d0))))
               (format t "eth/~D: ~:D blocks (~:D..~:D) matched archive byte for byte in ~,3F s (~,0F blocks/s)~%"
                       (clempo.eth:eth-version peer) count from to seconds (/ count (max seconds 1d-6)))
               0))
        (clempo.eth:eth-close peer)))))

(defun public-follow-rpc-head (url)
  (let ((head (clempo.jsonrpc:rpc-call "eth_getBlockByNumber" '("finalized" :false) :url url)))
    (unless (clempo.json:jobject-p head) (error "RPC has no finalized head"))
    (hex->integer (clempo.json:jget head "number"))))

(defcommand "p2p-discover" (args)
  "discover public Tempo peers: [-chain mainnet|moderato] [-protocol auto|v4|v5] [-seconds 20]"
  (let* ((chain (cond ((string-equal (arg args :chain "mainnet") "mainnet") :mainnet)
                      ((string-equal (arg args :chain) "moderato") :moderato)
                      (t (error "Use -chain mainnet or moderato"))))
         (protocols (or (cdr (assoc (arg args :protocol "auto")
                                    '(("auto" :v4 :v5) ("v4" :v4) ("v5" :v5))
                                    :test #'string-equal))
                        (error "Use -protocol auto, v4 or v5")))
         (peers (clempo.discovery:discover-peers (clempo.eth-source:tempo-bootnodes chain)
                                                 :timeout (arg-int args :seconds 20) :protocols protocols)))
    (write-line
     (clempo.json:to-json-string
      (clempo.json:jobject
       "chain" (string-downcase chain) "verifiedEndpoints" (length peers)
       "peers" (map 'vector
                    (lambda (p)
                      (clempo.json:jobject
                       "host" (clempo.discovery:peer-host p)
                       "udpPort" (clempo.discovery:peer-udp-port p)
                       "tcpPort" (clempo.discovery:peer-tcp-port p)
                       "publicKey" (octets->hex (clempo.discovery:peer-public-key p))))
                    peers))))
    (if peers 0 1)))

(defcommand "p2p-follow-proof" (args)
  "follow public devp2p and compare every finalized raw block: [-upstream URL] [-seconds 1800] [-batch 32] [-peers ENODES_FILE]"
  (let* ((url (arg args :upstream (clempo.jsonrpc:local-rpc-url)))
         (seconds (arg-int args :seconds 1800))
         (batch (arg-int args :batch 32))
         (peers (arg args :peers))
         (bootnodes (if peers (clempo.eth-source:read-enodes peers) (clempo.eth-source:tempo-bootnodes)))
         (units internal-time-units-per-second)
         (next (1+ (public-follow-rpc-head url)))
         (from next)
         (elapsed 0) (reported 0) (connections 0))
    (check-type seconds (integer 1 *))
    (check-type batch (integer 1 128))
    (let ((start (clempo.io:monotonic-ticks))
          (progress (clempo.io:monotonic-ticks)))
      (clempo.eth-source:call-with-public-block-source
       (lambda (fetch)
         (loop
           for tip = (public-follow-rpc-head url)
           for count = (min batch (max 0 (- (1+ tip) next)))
           do (when (plusp count)
                (let ((raw (handler-case (funcall fetch next count)
                             (clempo.p2p-wire:p2p-error (c)
                               (log-line "p2p-follow-proof: ~A" c)
                               nil))))
                  (when raw
                    (let ((expected (clempo.archive:fetch-blocks next count :url url)))
                      (dotimes (i count)
                        (unless (octets= (svref raw i) (svref expected i))
                          (error "Public peer block ~D differs from RPC data" (+ next i)))))
                    (incf next count)
                    (setf progress (clempo.io:monotonic-ticks)))))
              (setf elapsed (floor (- (clempo.io:monotonic-ticks) start) units))
              (when (>= elapsed (+ reported 30))
                (log-line "p2p-follow-proof: ~Ds, ~:D blocks matched, own ~:D RPC ~:D"
                          elapsed (- next from) (1- next) tip)
                (setf reported elapsed))
              (when (> (- (clempo.io:monotonic-ticks) progress) (* 120 units))
                (error "Public devp2p did not advance for 120 seconds"))
              (when (and (>= elapsed seconds) (> next tip))
                (return))
              (when (or (zerop count) (<= next tip)) (sleep 0.5))))
       :bootnodes bootnodes
       :on-peer (lambda (peer version)
                  (incf connections)
                  (log-line "p2p-follow-proof: eth/~D ~A:~D (~A)"
                            version (clempo.discovery:peer-host peer) (clempo.discovery:peer-tcp-port peer)
                            (if (clempo.discovery:peer-verified-p peer) "UDP discovery" "configured enode")))))
    (unless (> next from) (error "Public devp2p did not download any blocks"))
    (write-line (clempo.json:to-json-string
                 (clempo.json:jobject "source" "public-devp2p" "seconds" elapsed
                                      "from" from "to" (1- next) "matched" (- next from)
                                      "connections" connections "rpc" url)))
    0))
