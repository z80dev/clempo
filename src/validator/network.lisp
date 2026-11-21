(in-package #:clempo.validator)

;;; The validator's authenticated peer network: Tempo's commonware-p2p 2026.10.1
;;; authenticated::lookup network (network.rs, config.rs, actors/{listener,dialer,
;;; tracker,peer}, authenticated/{connection,channels,relay,throttle}.rs and
;;; utils/limited.rs) as configured by Tempo crates/consensus/src/{network,config,
;;; args}.rs. Peers come only from NETWORK-TRACK (the chain's validator sets).
;;;
;;; Rate limits follow commonware exactly:
;;; - inbound: each connection has one GCRA per registered channel (burst = rate)
;;;   and one for pings (one per half ping interval). A message over quota is not
;;;   dropped; the connection's reader waits until the limiter admits it (Throttle),
;;;   which back-pressures that peer only. An admitted message whose channel
;;;   backlog is full is dropped (Inbox::deliver).
;;; - outbound: each channel has one GCRA per recipient, shared across sends. A
;;;   recipient over quota is skipped for that message (LimitedSender::check); a
;;;   full per-peer priority queue drops it for that peer (Relay::send).
;;; Threads: one listener, one dialer, one per handshake or dial attempt, and a
;;; reader plus a writer per connection. Nothing here hardcodes an address.

(defconstant +network-inbox-limit+ (* 64 1024 1024))
(defconstant +network-outbox-limit+ (* 32 1024 1024))
(defconstant +network-outbox-entries+ 128)
(defconstant +send-batch-size+ 8)
(defconstant +tempo-max-message-size+ 8388780
  "crates/consensus network::max_message_size for Tempo's default participants.")

(defun tempo-channel-quotas (&key (backfill-frequency 8))
  "Tempo's registered channels and their per-second quotas (burst = rate):
votes, certificates, resolver, broadcaster, marshal (--consensus.backfill-frequency)
and DKG."
  `((0 . 128) (1 . 128) (2 . 128) (3 . 8) (4 . ,backfill-frequency) (5 . 128)))

(defun local-network-options ()
  "Keyword options matching Tempo's --consensus.use-local-defaults, for loopback or
private devnets; CALL-WITH-VALIDATOR-NETWORK defaults are Tempo's production values."
  (list :allow-private-ips t :handshake-ip-period 0.062 :handshake-subnet-period 0.007
        :peer-connection-cooldown 1 :dial-frequency 0.5 :ping-interval 5
        :block-seconds 3600 :max-concurrent-handshakes 1024))

(defun default-priority (channel)
  "Tempo sends broadcast blocks (3) and marshal backfill (4) at low priority."
  (if (member channel '(3 4)) :low :high))

;;; Bounded queues. A QUEUES holds several rings sharing one byte budget; ring 0 is
;;; always drained first, the rest in order (outbox: high, low) or round-robin
;;; (inbox: status events, then one ring per channel).

(defstruct (ring (:constructor make-ring (capacity &aux (cells (make-array capacity :initial-element nil)))))
  (cells #() :type simple-vector :read-only t)
  (next 0 :type fixnum)
  (count 0 :type fixnum))

(defun ring-push (ring value)
  (let* ((cells (ring-cells ring)) (capacity (length cells)) (count (ring-count ring)))
    (when (< count capacity)
      (setf (aref cells (mod (+ (ring-next ring) count) capacity)) value)
      (incf (ring-count ring))
      t)))

(defun ring-pop (ring)
  (let* ((cells (ring-cells ring)) (next (ring-next ring)) (value (aref cells next)))
    (setf (aref cells next) nil
          (ring-next ring) (mod (1+ next) (length cells)))
    (decf (ring-count ring))
    value))

(defstruct (queues (:constructor %make-queues (rings byte-limit rotate)))
  (rings #() :type simple-vector :read-only t)
  (byte-limit 0 :type fixnum :read-only t)
  (rotate nil :read-only t)
  (bytes 0 :type fixnum)
  (cursor 1 :type fixnum)
  (mutex (thr:make-mutex :name "validator queues") :read-only t)
  (ready (thr:make-waitqueue :name "validator queues") :read-only t))

(defun make-queues (capacities byte-limit &key rotate)
  (%make-queues (map 'simple-vector #'make-ring capacities) byte-limit rotate))

(defun queues-put (queues index value size)
  "Append VALUE (SIZE bytes) to ring INDEX; NIL when the ring or byte budget is full."
  (thr:with-mutex ((queues-mutex queues))
    (when (and (<= (+ (queues-bytes queues) size) (queues-byte-limit queues))
               (ring-push (aref (queues-rings queues) index) (cons size value)))
      (incf (queues-bytes queues) size)
      (thr:condition-broadcast (queues-ready queues))
      t)))

(defun queues-pop-locked (queues)
  (let* ((rings (queues-rings queues)) (n (length rings)))
    (flet ((take (ring)
             (let ((cell (ring-pop ring)))
               (decf (queues-bytes queues) (car cell))
               (return-from queues-pop-locked (values (cdr cell) t)))))
      (when (plusp (ring-count (aref rings 0))) (take (aref rings 0)))
      (if (queues-rotate queues)
          (loop repeat (1- n)
                for index = (queues-cursor queues)
                do (setf (queues-cursor queues) (if (= index (1- n)) 1 (1+ index)))
                   (when (plusp (ring-count (aref rings index))) (take (aref rings index))))
          (loop for index from 1 below n
                when (plusp (ring-count (aref rings index))) do (take (aref rings index))))
      (values nil nil))))

(defun queues-take (queues limit timeout)
  "Remove up to LIMIT values, waiting up to TIMEOUT seconds while all rings are empty."
  (let ((mutex (queues-mutex queues))
        (deadline (+ (get-internal-real-time) (ceiling (* timeout internal-time-units-per-second)))))
    (thr:with-mutex (mutex)
      (loop for remaining = (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)
            while (and (zerop (queues-bytes-or-count queues)) (plusp remaining))
            do (thr:condition-wait (queues-ready queues) mutex :timeout remaining))
      (loop repeat limit
            for (value present) = (multiple-value-list (queues-pop-locked queues))
            while present collect value))))

(defun queues-bytes-or-count (queues)
  (loop for ring across (queues-rings queues) sum (ring-count ring)))

(defun queues-wake (queues)
  (thr:with-mutex ((queues-mutex queues)) (thr:condition-broadcast (queues-ready queues))))

;;; Events, configuration and state.

(defstruct (network-event (:constructor event (kind peer &optional channel frame start end)))
  kind peer channel frame start end)

(defstruct (net-config (:conc-name cfg-))
  sake-version cups-version max-message-size channels quotas tracked-sets max-peers-per-set
  block-ns handshake-timeout dial-timeout max-concurrent-handshakes cooldown-ns dial-frequency
  ping-ns allow-private-ips bypass-ip-check)

(defstruct (conn (:constructor make-conn (socket)))
  "A TCP socket and its stream. SHUTDOWN wakes blocked reads/writes from any thread;
the CLOSED flag keeps a late shutdown from touching a reused descriptor."
  socket stream (closed nil) (mutex (thr:make-mutex :name "validator conn") :read-only t))

(defun conn-shutdown (conn)
  (thr:with-mutex ((conn-mutex conn))
    (unless (conn-closed conn) (ignore-errors (sock:socket-shutdown (conn-socket conn) :direction :io)))))

(defun conn-close (conn)
  (thr:with-mutex ((conn-mutex conn))
    (unless (conn-closed conn)
      (setf (conn-closed conn) t)
      (if (conn-stream conn)
          (ignore-errors (close (conn-stream conn) :abort t))
          (ignore-errors (sock:socket-close (conn-socket conn)))))))

(defun conn-open-stream (conn)
  (let ((socket (conn-socket conn)))
    (setf (sock:sockopt-tcp-nodelay socket) t
          (conn-stream conn) (sock:socket-make-stream socket :input t :output t :element-type 'octet
                                                             :buffering :full :timeout nil :auto-close t))))

(defstruct (link (:constructor make-link (key session conn)))
  key session conn (killed nil) writer
  (outbox (make-queues (list +network-outbox-entries+ +network-outbox-entries+) +network-outbox-limit+)
          :read-only t))

(defstruct (peer-record (:conc-name record-) (:constructor make-record (key)))
  key host port egress (sets 0) (status :inert) link stale (next-reservable 0) (next-dial 0))

(defstruct (validator-network (:constructor %make-network))
  seed key config listener (listen-port 0)
  (lock (thr:make-mutex :name "validator network") :read-only t)
  (records (make-hash-table :test #'equalp) :read-only t)
  (sets nil)                            ; ((id . keys) ...), newest first
  (blocked (make-hash-table :test #'equalp) :read-only t)
  (outbound nil)                        ; channel -> keyed GCRA
  ip-limiter subnet-limiter
  (handshakes 0) (handshake-conns nil)
  inbox channel-rings
  (threads nil) (stopping nil))

(defgeneric network-receive (network &key timeout)
  (:documentation "Dequeue the next :CONNECTED, :DISCONNECTED or :DATA event, or NIL
after TIMEOUT seconds (default 0)."))
(defgeneric network-send (network channel bytes &key recipients priority)
  (:documentation "Queue immutable BYTES on CHANNEL for RECIPIENTS (default every
connected peer) at PRIORITY (:HIGH or :LOW; default :LOW for broadcast/marshal 3,4,
else :HIGH). Returns how many peers it was queued for: recipients over their
outbound channel quota or with full queues are skipped."))
(defgeneric network-track (network id peers)
  (:documentation "Register peer set ID (strictly increasing; otherwise ignored and
NIL is returned). PEERS lists (public-key host port [egress-ip]); the egress IP
defaults to HOST when it is an IPv4 literal. The latest TRACKED-SETS sets are
retained: their peers are dialed and admitted, everyone else is refused and
disconnected."))
(defgeneric network-block (network public-key)
  (:documentation "Disconnect PUBLIC-KEY and refuse or stop dialing it for the block
period. Returns T when newly blocked."))
(defgeneric network-peers (network)
  (:documentation "Snapshot the identities of authenticated, connected peers."))
(defgeneric network-listen-port (network)
  (:documentation "The bound listener port (useful with :LISTEN-PORT 0)."))

(defmacro with-network-lock ((network) &body body)
  `(thr:with-mutex ((validator-network-lock ,network)) ,@body))

(defun network-stopping-p (network)
  (with-network-lock (network) (validator-network-stopping network)))

(defun spawn-locked (network name function)
  "Start a worker unless stopping; reap finished workers. Call with the lock held."
  (unless (validator-network-stopping network)
    (setf (validator-network-threads network)
          (delete-if-not #'thr:thread-alive-p (validator-network-threads network)))
    (let ((thread (thr:make-thread function :name name)))
      (push thread (validator-network-threads network))
      thread)))

(defun note-network (control &rest arguments)
  (format *error-output* "~&validator network: ~?~%" control arguments))

;;; Addresses.

(defun parse-ipv4 (string)
  "Return the 4-octet address of a dotted-quad STRING, or NIL."
  (let ((parts (uiop:split-string string :separator ".")))
    (when (and (= 4 (length parts))
               (every (lambda (part) (and (<= 1 (length part) 3) (every #'digit-char-p part)
                                          (<= (parse-integer part) 255)))
                      parts))
      (map 'octets #'parse-integer parts))))

(defun global-ipv4-p (ip)
  "commonware IpAddrExt::is_global for IPv4: not private, loopback, link-local,
shared, documentation, benchmarking, reserved, multicast or broadcast."
  (let ((a (aref ip 0)) (b (aref ip 1)) (c (aref ip 2)))
    (not (or (= a 0) (= a 10) (= a 127) (>= a 224)
             (and (= a 100) (<= 64 b 127))
             (and (= a 169) (= b 254))
             (and (= a 172) (<= 16 b 31))
             (and (= a 192) (= b 168))
             (and (= a 192) (= b 0) (member c '(0 2)))
             (and (= a 198) (<= 18 b 19))
             (and (= a 198) (= b 51) (= c 100))
             (and (= a 203) (= b 0) (= c 113))))))

(defun resolve-ipv4 (host)
  (or (parse-ipv4 host)
      (coerce (sock:host-ent-address (sock:get-host-by-name host)) 'octets)))

(defun dial-preferred-p (network key)
  "For two clempo peers only one direction dials: the lexicographically smaller key.
Inbound connections from tracked peers are accepted regardless (Rust always dials)."
  (loop for local across (validator-network-key network)
        for peer across key
        when (/= local peer) return (< local peer)))

;;; The tracker (directory.rs). All of these run with the network lock held.

(defun blocked-p (network key now)
  (let ((until (gethash key (validator-network-blocked network))))
    (and until (> until now))))

(defun eligible-p (network record now)
  (and (plusp (record-sets record)) (not (blocked-p network (record-key record) now))))

(defun acceptable-p (network key ip now)
  (let ((record (gethash key (validator-network-records network)))
        (config (validator-network-config network)))
    (and record (eligible-p network record now) (eq :inert (record-status record))
         (or (cfg-bypass-ip-check config) (equalp ip (record-egress record))))))

(defun registered-ip-p (network ip now)
  (loop for record being the hash-values of (validator-network-records network)
        thereis (and (eligible-p network record now) (equalp ip (record-egress record)))))

(defun reserve (network record now)
  "Reserve RECORD for one dial or listen attempt, honouring the per-peer cooldown."
  (let ((cooldown (cfg-cooldown-ns (validator-network-config network))))
    (when (and (eligible-p network record now) (eq :inert (record-status record))
               (>= now (record-next-reservable record)))
      (setf (record-status record) :reserved
            (record-stale record) nil
            (record-next-reservable record) (+ now cooldown)
            (record-next-dial record) (+ now cooldown (io:random-below (1+ (floor cooldown 2)))))
      t)))

(defun kill-link (link)
  "Ask LINK's threads to stop; its reader releases the peer. Safe from any thread."
  (setf (link-killed link) t)
  (conn-shutdown (link-conn link))
  (queues-wake (link-outbox link)))

(defun teardown-if-needed (network record)
  (let ((records (validator-network-records network)))
    (when (or (zerop (record-sets record)) (record-stale record))
      (case (record-status record)
        (:active (kill-link (record-link record)))
        (:reserved (setf (record-stale record) t))))
    (when (and (zerop (record-sets record)) (eq :inert (record-status record))
               (eq record (gethash (record-key record) records)))
      (remhash (record-key record) records)
      (loop for limiter in (validator-network-outbound network)
            do (clempo.cw-limit:keyed-gcra-forget (cdr limiter) (record-key record))))))

(defun release (network record)
  (setf (record-status record) :inert (record-link record) nil (record-stale record) nil)
  (teardown-if-needed network record))

;;; Generic function methods.

(defmethod network-listen-port ((network validator-network))
  (validator-network-listen-port network))

(defmethod network-peers ((network validator-network))
  (with-network-lock (network)
    (loop for record being the hash-values of (validator-network-records network)
          when (eq :active (record-status record)) collect (record-key record))))

(defmethod network-receive ((network validator-network) &key (timeout 0))
  (first (queues-take (validator-network-inbox network) 1 timeout)))

(defmethod network-send ((network validator-network) channel bytes
                         &key recipients (priority (default-priority channel)))
  (check-type bytes octets)
  (check-type priority (member :high :low))
  (let* ((config (validator-network-config network))
         (limiter (or (cdr (assoc channel (validator-network-outbound network)))
                      (error "Channel ~S is not registered" channel)))
         (frame (cw:encode-data channel bytes :max-message-size (cfg-max-message-size config)))
         (index (if (eq priority :high) 0 1))
         (now (clempo.cw-limit:monotonic-ns))
         (count 0))
    (with-network-lock (network)
      (loop for record being the hash-values of (validator-network-records network)
            for key = (record-key record) for link = (record-link record)
            when (and (eq :active (record-status record))
                      (or (null recipients) (member key recipients :test #'octets=))
                      (clempo.cw-limit:keyed-gcra-check limiter key now)
                      (queues-put (link-outbox link) index frame (length frame)))
              do (incf count)))
    count))

(defun parse-tracked-peer (entry)
  (destructuring-bind (key host port &optional egress) entry
    (check-type key (octets 32))
    (check-type host string)
    (check-type port (integer 1 65535))
    (let ((egress (if egress (or (parse-ipv4 egress) (error "Egress ~S is not an IPv4 address" egress))
                      (parse-ipv4 host))))
      (list (copy-seq key) host port egress))))

(defmethod network-track ((network validator-network) id peers)
  (check-type id (integer 0 #xffffffffffffffff))
  (let* ((config (validator-network-config network))
         (self (validator-network-key network))
         (entries (mapcar #'parse-tracked-peer peers))
         (keys (remove-duplicates (mapcar #'first entries) :test #'octets=)))
    (unless (= (length keys) (length entries)) (error "Peer set ~D repeats an identity" id))
    (when (> (length (adjoin self keys :test #'octets=)) (cfg-max-peers-per-set config))
      (error "Peer set ~D exceeds ~D identities" id (cfg-max-peers-per-set config)))
    (with-network-lock (network)
      (let ((sets (validator-network-sets network))
            (records (validator-network-records network)))
        (when (and sets (<= id (car (first sets))))
          (return-from network-track nil))
        (loop for (key host port egress) in entries
              unless (octets= key self)
                do (let ((record (or (gethash key records)
                                     (setf (gethash key records) (make-record key)))))
                     (unless (and (equal host (record-host record)) (eql port (record-port record))
                                  (equalp egress (record-egress record)))
                       (when (record-host record) (setf (record-stale record) t))
                       (setf (record-host record) host (record-port record) port
                             (record-egress record) egress))
                     (incf (record-sets record))
                     (teardown-if-needed network record)))
        (push (cons id (remove self keys :test #'octets=)) (validator-network-sets network))
        (loop while (> (length (validator-network-sets network)) (cfg-tracked-sets config))
              do (dolist (key (cdr (car (last (validator-network-sets network)))))
                   (let ((record (gethash key records)))
                     (decf (record-sets record))
                     (teardown-if-needed network record)))
                 (setf (validator-network-sets network) (butlast (validator-network-sets network))))
        t))))

(defmethod network-block ((network validator-network) public-key)
  (check-type public-key (octets 32))
  (let ((now (clempo.cw-limit:monotonic-ns)))
    (with-network-lock (network)
      (unless (or (octets= public-key (validator-network-key network))
                  (blocked-p network public-key now))
        (let ((blocked (validator-network-blocked network)))
          (maphash (lambda (key until) (when (<= until now) (remhash key blocked))) blocked)
          (setf (gethash (copy-seq public-key) blocked) (+ now (cfg-block-ns (validator-network-config network)))))
        (let ((record (gethash public-key (validator-network-records network))))
          (when record
            (case (record-status record)
              (:active (kill-link (record-link record)))
              (:reserved (setf (record-stale record) t)))))
        t))))

;;; Connections (actors/peer, connection.rs).

(defun throttle (network link limiter)
  "Wait until LIMITER admits one message (commonware Throttle::receive)."
  (loop (multiple-value-bind (ok earliest) (clempo.cw-limit:gcra-check limiter)
          (when ok (return t))
          (when (or (link-killed link) (network-stopping-p network)) (return nil))
          (sleep (min 0.1 (max 0.001 (/ (- earliest (clempo.cw-limit:monotonic-ns)) 1d9)))))))

(defun ready-for-input (stream timeout)
  (or (listen stream) (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input timeout)))

(defun run-writer (network link)
  (let ((session (link-session link)) (outbox (link-outbox link))
        (ping-ns (cfg-ping-ns (validator-network-config network))))
    (unwind-protect
         (loop with next-ping = (+ (clempo.cw-limit:monotonic-ns) ping-ns)
               until (link-killed link)
               do (let* ((wait (/ (max 0 (- next-ping (clempo.cw-limit:monotonic-ns))) 1d9))
                         (frames (queues-take outbox +send-batch-size+ (min 0.1 wait))))
                    (unless (link-killed link)
                      (when (>= (clempo.cw-limit:monotonic-ns) next-ping)
                        (setf frames (append frames (list (cw:encode-ping)))
                              next-ping (+ (clempo.cw-limit:monotonic-ns) ping-ns)))
                      (when frames (cw:send-frames session frames)))))
      (kill-link link))))

(defun run-reader (network link)
  (let* ((config (validator-network-config network))
         (session (link-session link)) (stream (conn-stream (link-conn link)))
         (inbox (validator-network-inbox network))
         (rings (validator-network-channel-rings network))
         (limiters (loop for (channel . rate) in (cfg-quotas config)
                         collect (cons channel (multiple-value-call #'clempo.cw-limit:make-gcra
                                                 (clempo.cw-limit:quota-per-second rate)))))
         (ping-limiter (multiple-value-call #'clempo.cw-limit:make-gcra
                         (clempo.cw-limit:quota-with-period (max 0.001d0 (/ (cfg-ping-ns config) 2d9))))))
    (loop until (or (link-killed link) (network-stopping-p network))
          when (ready-for-input stream 0.1)
            do (multiple-value-bind (kind channel frame start end) (cw:receive-message session)
                 (if (eq kind :ping)
                     (throttle network link ping-limiter)
                     (when (throttle network link (cdr (assoc channel limiters)))
                       ;; A full channel backlog drops the message (Inbox::deliver).
                       (queues-put inbox (cdr (assoc channel rings))
                                   (event :data (link-key link) channel frame start end)
                                   (length frame))))))))

(defun run-link (network link record)
  (unwind-protect
       (handler-case
           (progn
             (let ((writer (with-network-lock (network)
                             (spawn-locked network "validator peer writer"
                                           (lambda ()
                                             (handler-case (run-writer network link)
                                               (serious-condition () nil)))))))
               (setf (link-writer link) writer)
               (when writer (run-reader network link))))
         (serious-condition (condition)
           (unless (or (link-killed link) (network-stopping-p network))
             (note-network "peer ~A disconnected: ~A" (octets->hex (link-key link)) condition))))
    (kill-link link)
    (when (link-writer link) (thr:join-thread (link-writer link) :default nil))
    (ignore-errors (cw:close-session (link-session link)))
    (conn-close (link-conn link))
    (with-network-lock (network)
      (when (eq link (record-link record)) (release network record)))
    (queues-put (validator-network-inbox network) 0 (event :disconnected (link-key link)) 0)))

(defun connect-link (network record session conn)
  "Promote a reserved RECORD to an active connection, or release it. Lock held."
  (let ((now (clempo.cw-limit:monotonic-ns)) (key (record-key record)))
    (cond ((and (not (validator-network-stopping network)) (eq :reserved (record-status record))
                (not (record-stale record)) (eligible-p network record now)
                (eq record (gethash key (validator-network-records network))))
           (let ((link (make-link key session conn)))
             (cond ((spawn-locked network "validator peer reader" (lambda () (run-link network link record)))
                    ;; Still under the lock, so this precedes the reader's :DISCONNECTED.
                    (setf (record-status record) :active (record-link record) link)
                    (queues-put (validator-network-inbox network) 0 (event :connected key) 0)
                    t)
                   (t (release network record) nil))))
          (t (release network record) nil))))

(defun session-options (network)
  (let ((config (validator-network-config network)))
    (list :max-message-size (cfg-max-message-size config) :channels (cfg-channels config)
          :sake-version (cfg-sake-version config) :cups-version (cfg-cups-version config))))

;;; Listener (actors/listener.rs).

(defun run-handshake (network conn ip)
  (let ((record nil) (connected nil))
    (unwind-protect
         (handler-case
             (let ((session (sb-ext:with-timeout (cfg-handshake-timeout (validator-network-config network))
                              (conn-open-stream conn)
                              (apply #'cw:accept (conn-stream conn) (validator-network-seed network)
                                     (lambda (key)
                                       (with-network-lock (network)
                                         (acceptable-p network key ip (clempo.cw-limit:monotonic-ns))))
                                     (session-options network)))))
               (with-network-lock (network)
                 (let ((candidate (gethash (cw:session-peer session) (validator-network-records network))))
                   (when (and candidate (acceptable-p network (record-key candidate) ip (clempo.cw-limit:monotonic-ns))
                              (reserve network candidate (clempo.cw-limit:monotonic-ns)))
                     (setf record candidate
                           connected (connect-link network candidate session conn)
                           record nil)))))
           (serious-condition () nil))
      (with-network-lock (network)
        (when record (release network record))
        (decf (validator-network-handshakes network))
        (setf (validator-network-handshake-conns network)
              (delete conn (validator-network-handshake-conns network))))
      (unless connected (conn-close conn)))))

(defun admit-connection (network socket address)
  "The listener's pre-handshake policy. Returns true when a handshake thread owns SOCKET."
  (let* ((config (validator-network-config network))
         (ip (coerce address 'octets))
         (now (clempo.cw-limit:monotonic-ns))
         (conn (make-conn socket)))
    (with-network-lock (network)
      (when (and (or (cfg-allow-private-ips config) (global-ipv4-p ip))
                 (or (cfg-bypass-ip-check config) (registered-ip-p network ip now))
                 ;; Both limiters are charged before either refusal, as in Rust.
                 (let ((ip-ok (clempo.cw-limit:keyed-gcra-check (validator-network-ip-limiter network) ip now))
                       (subnet-ok (clempo.cw-limit:keyed-gcra-check (validator-network-subnet-limiter network)
                                                                    (subseq ip 0 3) now)))
                   (and ip-ok subnet-ok))
                 (< (validator-network-handshakes network) (cfg-max-concurrent-handshakes config)))
        (incf (validator-network-handshakes network))
        (push conn (validator-network-handshake-conns network))
        (or (spawn-locked network "validator handshake" (lambda () (run-handshake network conn ip)))
            (progn (decf (validator-network-handshakes network))
                   (pop (validator-network-handshake-conns network))
                   nil))))))

(defun run-listener (network)
  (let* ((listener (validator-network-listener network))
         (fd (sock:socket-file-descriptor listener))
         (accepted 0))
    (loop until (network-stopping-p network) do
      (when (sb-sys:wait-until-fd-usable fd :input 0.1)
        (multiple-value-bind (socket address) (ignore-errors (sock:socket-accept listener))
          (when socket
            (when (> (incf accepted) 16384)
              (setf accepted 0)
              (with-network-lock (network)
                (clempo.cw-limit:keyed-gcra-retain-recent (validator-network-ip-limiter network))
                (clempo.cw-limit:keyed-gcra-retain-recent (validator-network-subnet-limiter network))))
            (unless (ignore-errors (admit-connection network socket address))
              (ignore-errors (sock:socket-close socket)))))))))

;;; Dialer (actors/dialer.rs): one reservation per tick from a shuffled queue.

(defun dialable-keys (network now)
  (let ((config (validator-network-config network)))
    (loop for record being the hash-values of (validator-network-records network)
          when (and (eligible-p network record now) (eq :inert (record-status record))
                    (>= now (record-next-dial record))
                    (dial-preferred-p network (record-key record))
                    (let ((ip (parse-ipv4 (record-host record))))
                      (or (null ip) (cfg-allow-private-ips config) (global-ipv4-p ip))))
            collect (record-key record))))

(defun run-dial (network record)
  (let ((config (validator-network-config network)) (conn nil) (connected nil) (released nil))
    (unwind-protect
         (handler-case
             (let ((session
                     (sb-ext:with-timeout (cfg-dial-timeout config)
                       (let ((ip (resolve-ipv4 (record-host record))))
                         (unless (or (cfg-allow-private-ips config) (global-ipv4-p ip))
                           (error "Refusing to dial private address ~A" (record-host record)))
                         (setf conn (make-conn (make-instance 'sock:inet-socket :type :stream :protocol :tcp)))
                         (with-network-lock (network)
                           (when (validator-network-stopping network) (error "Network stopping"))
                           (push conn (validator-network-handshake-conns network)))
                         (sock:socket-connect (conn-socket conn) ip (record-port record))
                         (conn-open-stream conn)
                         (apply #'cw:dial (conn-stream conn) (validator-network-seed network) (record-key record)
                                (session-options network))))))
               (with-network-lock (network)
                 (setf released t connected (connect-link network record session conn))))
           (serious-condition () nil))
      (with-network-lock (network)
        (unless released (release network record))
        (when conn
          (setf (validator-network-handshake-conns network)
                (delete conn (validator-network-handshake-conns network)))))
      (when (and conn (not connected)) (conn-close conn)))))

(defun run-dialer (network)
  (let ((queue nil) (frequency (cfg-dial-frequency (validator-network-config network))))
    (loop until (network-stopping-p network) do
      (with-network-lock (network)
        (let ((now (clempo.cw-limit:monotonic-ns)))
          (unless queue
            (setf queue (dialable-keys network now))
            ;; Fisher-Yates over the snapshot, as the Rust dialer shuffles.
            (let ((vector (coerce queue 'simple-vector)))
              (loop for i from (1- (length vector)) downto 1
                    do (rotatef (aref vector i) (aref vector (io:random-below (1+ i)))))
              (setf queue (coerce vector 'list))))
          (loop for key = (pop queue) while key
                for record = (gethash key (validator-network-records network))
                when (and record (>= now (record-next-dial record)) (reserve network record now))
                  do (unless (spawn-locked network "validator dial" (lambda () (run-dial network record)))
                       (release network record))
                     (return))))
      (loop with end = (+ (get-internal-real-time) (ceiling (* frequency internal-time-units-per-second)))
            while (and (< (get-internal-real-time) end) (not (network-stopping-p network)))
            do (sleep (min 0.05 frequency))))))

;;; Entry point.

(defun call-with-validator-network
    (function &key seed (listen-host "0.0.0.0") (listen-port 0) (sake-version 0) (cups-version 0)
                (max-message-size +tempo-max-message-size+) (channel-quotas (tempo-channel-quotas))
                (tracked-sets 1) (max-peers-per-set 128) (block-seconds (* 4 3600))
                (handshake-timeout 5) (dial-timeout 15) (max-concurrent-handshakes 512)
                (handshake-ip-period 5) (handshake-subnet-period 0.015) (peer-connection-cooldown 60)
                (dial-frequency 1) (ping-interval 50) allow-private-ips bypass-ip-check)
  "Listen on LISTEN-HOST:LISTEN-PORT (IPv4; port 0 picks one, see
NETWORK-LISTEN-PORT), run the listener and dialer, call FUNCTION with the network,
then stop and join every thread and zero this network's copy of SEED.
Defaults are Tempo's production settings (crates/consensus args.rs); pass
(LOCAL-NETWORK-OPTIONS) for a loopback devnet. SAKE-VERSION and CUPS-VERSION are
not negotiated (commonware-stream sake/mod.rs) and Tempo pins 0/0: peers must be
configured alike, and a mismatch fails the handshake closed. Periods are seconds."
  (check-type seed (octets 32))
  (check-type listen-host string)
  (check-type listen-port (integer 0 65535))
  (check-type tracked-sets (integer 1 *))
  (check-type max-peers-per-set (integer 1 *))
  (check-type max-concurrent-handshakes (integer 1 *))
  (dolist (quota channel-quotas)
    (check-type (car quota) (integer 0 #xffffffffffffffff))
    (check-type (cdr quota) (integer 1 *)))
  (let* ((seed (copy-seq seed))
         (max-peers (* max-peers-per-set tracked-sets))
         (channels (mapcar #'car channel-quotas))
         (config (make-net-config
                  :sake-version sake-version :cups-version cups-version
                  :max-message-size max-message-size :channels channels :quotas channel-quotas
                  :tracked-sets tracked-sets :max-peers-per-set max-peers-per-set
                  :block-ns (ceiling (* block-seconds 1000000000))
                  :handshake-timeout handshake-timeout :dial-timeout dial-timeout
                  :max-concurrent-handshakes max-concurrent-handshakes
                  :cooldown-ns (ceiling (* peer-connection-cooldown 1000000000))
                  :dial-frequency dial-frequency :ping-ns (ceiling (* ping-interval 1000000000))
                  :allow-private-ips allow-private-ips :bypass-ip-check bypass-ip-check))
         (listener (make-instance 'sock:inet-socket :type :stream :protocol :tcp))
         (network
           (%make-network
            :seed seed :key (clempo.ed25519:ed25519-public-key seed) :config config :listener listener
            :outbound (loop for (channel . rate) in channel-quotas
                            collect (cons channel (multiple-value-call #'clempo.cw-limit:make-keyed-gcra
                                                    (clempo.cw-limit:quota-per-second rate))))
            :ip-limiter (multiple-value-call #'clempo.cw-limit:make-keyed-gcra
                          (clempo.cw-limit:quota-with-period handshake-ip-period))
            :subnet-limiter (multiple-value-call #'clempo.cw-limit:make-keyed-gcra
                              (clempo.cw-limit:quota-with-period handshake-subnet-period))
            ;; Each channel backlog holds one quota burst per allowed peer (channels.rs).
            :inbox (make-queues (cons (* 4 max-peers) (loop for (nil . rate) in channel-quotas
                                                            collect (* max-peers rate)))
                                +network-inbox-limit+ :rotate t)
            :channel-rings (loop for channel in channels for index from 1 collect (cons channel index)))))
    (unwind-protect
         (progn
           (setf (sock:sockopt-reuse-address listener) t)
           (sock:socket-bind listener (resolve-ipv4 listen-host) listen-port)
           (sock:socket-listen listener 128)
           (setf (validator-network-listen-port network) (nth-value 1 (sock:socket-name listener)))
           (with-network-lock (network)
             (spawn-locked network "validator listener" (lambda () (run-listener network)))
             (spawn-locked network "validator dialer" (lambda () (run-dialer network))))
           (funcall function network))
      (with-network-lock (network)
        (setf (validator-network-stopping network) t)
        (dolist (conn (validator-network-handshake-conns network)) (conn-shutdown conn))
        (loop for record being the hash-values of (validator-network-records network)
              when (record-link record) do (kill-link (record-link record))))
      ;; Workers may start their last children until they observe STOPPING.
      (loop for threads = (with-network-lock (network)
                            (shiftf (validator-network-threads network) nil))
            while threads
            do (dolist (thread threads) (thr:join-thread thread :default nil)))
      (sock:socket-close listener)
      (fill seed 0))))
