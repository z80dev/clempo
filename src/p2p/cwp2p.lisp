(defpackage #:clempo.cwp2p
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.p2p-wire #:protocol-error)
  (:documentation "Tempo's commonware authenticated::lookup network over SAKE/CUPS.
The protocol is Data(0, channel:u64-varint, length:u32-varint, bytes) or Ping(1).
Channels are registered locally, never negotiated. Tempo uses channels 0..5 and
pins SAKE V0/CUPS V0; simplex/DKG payloads additionally carry a mux subchannel varint.
SAKE and CUPS versions are not negotiated either (commonware-stream sake/mod.rs):
DIAL, ACCEPT and CONNECT take :SAKE-VERSION and :CUPS-VERSION (0 or 1, default 0),
and both ends must be configured alike; a mismatch fails the handshake closed.
Unlike authenticated::discovery, lookup has no Greeting, signed advertised
addresses, BitVec, or Peers messages. Peer identities/addresses come from the
chain's validator registry, not from network gossip. Sending discovery messages
on this protocol is an error (its Greeting discriminant conflicts with Ping).
This library does not discover peers, join consensus, or sign consensus data.
Public validators admit registered identities and, normally, registered source
IPs. A fresh key cannot pass that policy; interoperability requires an explicitly
authorized identity/address or a controlled local reference, not chain joining
or generating validator consensus signatures. No public peer is contacted here.
The caller owns admission/IP policy and scheduling: call SEND-PING periodically
(including when application traffic is flowing), and drive RECEIVE-MESSAGE or
DISPATCH-MESSAGE. No background scanning or threads are started.")
  (:export #:+tempo-network-namespace+ #:stream-namespace
           #:+votes-channel+ #:+certificates-channel+ #:+resolver-channel+
           #:+broadcast-channel+ #:+marshal-channel+ #:+dkg-channel+
           #:+max-payload-overhead+
           #:encode-data #:encode-ping #:decode-message #:encode-mux #:decode-mux
           #:session #:wrap-connection #:dial #:accept #:connect
           #:close-session #:session-peer #:session-closed-p #:session-max-message-size
           #:register-channel #:channel-handler
           #:send-data #:send-frames #:send-ping #:receive-message #:receive-data
           #:dispatch-message))

(in-package #:clempo.cwp2p)

;;; Specification: commonware-p2p 2026.10.1 authenticated/{data,lookup/types}.rs,
;;; lookup/network.rs and utils/mux.rs; Tempo consensus/{config,network,lib}.rs.
;;; The recorded devnet fixture contains channel payloads, NOT encrypted records
;;; or complete lookup frames. Preserve every payload byte, including mux prefixes.

(define-constant +tempo-network-namespace+ (hex->octets "0554454d504f5f503250")
  "union_unique(TEMPO, _P2P); lookup appends _STREAM for SAKE authentication.")
(defconstant +votes-channel+ 0)
(defconstant +certificates-channel+ 1)
(defconstant +resolver-channel+ 2)
(defconstant +broadcast-channel+ 3)
(defconstant +marshal-channel+ 4)
(defconstant +dkg-channel+ 5)
(defconstant +max-payload-overhead+ 16)

(defun stream-namespace (&optional (network-namespace +tempo-network-namespace+))
  "Derive the raw SAKE namespace from a lookup network namespace (not vice versa)."
  (octets-concat network-namespace (string->octets "_STREAM")))

(defun check-uint (value bits)
  (unless (and (integerp value) (<= 0 value) (<= (integer-length value) bits))
    (protocol-error "commonware: ~S is not a u~D" value bits))
  value)

(defun check-range (bytes start end)
  (check-type bytes octets)
  (unless (and (integerp start) (integerp end) (<= 0 start end (length bytes)))
    (protocol-error "commonware: invalid octet range ~S..~S" start end)))

(defun varint-size (value)
  (max 1 (ceiling (integer-length value) 7)))

(defun put-varint (bytes position value)
  (loop
    (setf (aref bytes position)
          (logior (logand value 127) (if (> value 127) 128 0)))
    (incf position)
    (setf value (ash value -7))
    (when (zerop value) (return position))))

(defun get-varint (bytes position end bits)
  "Read canonical unsigned LEB128; return the integer and next position."
  (let ((value 0))
    (loop for shift from 0 below bits by 7
          do (when (= position end)
               (protocol-error "commonware: truncated u~D varint" bits))
             (let* ((byte (aref bytes position)) (part (logand byte 127)))
               (incf position)
               (when (and (> (+ shift 7) bits) (>= part (ash 1 (- bits shift))))
                 (protocol-error "commonware: overflowing u~D varint" bits))
               (setf value (logior value (ash part shift)))
               (unless (logbitp 7 byte)
                 (when (and (plusp shift) (zerop part))
                   (protocol-error "commonware: noncanonical varint"))
                 (return-from get-varint (values value position)))))
    (protocol-error "commonware: overlong u~D varint" bits)))

(defun encode-data (channel payload &key (start 0) (end (length payload))
                                     (max-message-size #xffffffff))
  "Encode one lookup Data frame. START/END allow forwarding without slicing."
  (check-uint channel 64)
  (check-uint max-message-size 32)
  (check-range payload start end)
  (let ((size (- end start)))
    (when (> size max-message-size)
      (protocol-error "commonware: payload exceeds ~D bytes" max-message-size))
    (let* ((header-size (+ 1 (varint-size channel) (varint-size size)))
           (out (make-octets (+ header-size size)))
           (position (put-varint out 1 channel)))
      (setf (aref out 0) 0)
      (put-varint out position size)
      (replace out payload :start1 header-size :start2 start :end2 end)
      out)))

(defun encode-ping ()
  (octets-of 1))

(defun decode-message (frame &key (max-message-size #xffffffff))
  "Return KIND, CHANNEL, START, END. The payload is FRAME[START,END), without a
copy or displaced-array allocation. KIND is :DATA or :PING (CHANNEL is NIL).
Reject trailing bytes, noncanonical varints, and lengths over the configured cap."
  (check-type frame octets)
  (check-uint max-message-size 32)
  (let ((end (length frame)))
    (when (zerop end) (protocol-error "commonware: empty lookup frame"))
    (case (aref frame 0)
      (0
       (multiple-value-bind (channel position) (get-varint frame 1 end 64)
         (multiple-value-bind (size position) (get-varint frame position end 32)
           (when (> size max-message-size)
             (protocol-error "commonware: payload exceeds ~D bytes" max-message-size))
           (unless (= size (- end position))
             (protocol-error "commonware: payload length ~D does not match frame" size))
           (values :data channel position end))))
      (1
       (unless (= end 1) (protocol-error "commonware: trailing bytes after Ping"))
       (values :ping nil 1 1))
      (otherwise (protocol-error "commonware: unknown lookup variant ~D" (aref frame 0))))))

(defun encode-mux (subchannel payload &key (start 0) (end (length payload)))
  "Prefix a subchannel u64 varint; there is no nested length field."
  (check-uint subchannel 64)
  (check-range payload start end)
  (let* ((prefix-size (varint-size subchannel))
         (out (make-octets (+ prefix-size (- end start)))))
    (put-varint out 0 subchannel)
    (replace out payload :start1 prefix-size :start2 start :end2 end)
    out))

(defun decode-mux (payload &key (start 0) (end (length payload)))
  "Return SUBCHANNEL, START, END within PAYLOAD, without copying the body.
Only apply this to muxed channels (Tempo 0,1,2,5), not broadcast/marshal (3,4)."
  (check-range payload start end)
  (multiple-value-bind (subchannel position) (get-varint payload start end 64)
    (values subchannel position end)))

(defstruct (session (:constructor %make-session)
                    (:conc-name session-))
  connection
  (max-message-size (* 16 1024 1024) :type (unsigned-byte 32) :read-only t)
  (channels (make-hash-table :test #'eql) :read-only t)
  (closed-p nil :type boolean)
  (ping-interval-ms 30000 :type (integer 1 *) :read-only t)
  (next-ping-ms 0 :type integer)
  (send-lock (sb-thread:make-mutex :name "commonware send") :read-only t)
  (receive-lock (sb-thread:make-mutex :name "commonware receive") :read-only t))

(defun session-peer (session)
  "The authenticated Ed25519 identity, not a claimed message field. Do not mutate."
  (clempo.cw-stream:connection-peer (session-connection session)))

(defun ensure-open (session)
  (when (session-closed-p session) (protocol-error "commonware: closed session")))

(defun register-channel (session channel &optional handler)
  "Register locally; this sends no negotiation message. Configure before I/O.
HANDLER, if non-NIL, is called as (session channel frame start end)."
  (ensure-open session)
  (check-uint channel 64)
  (when handler (check-type handler function))
  (when (nth-value 1 (gethash channel (session-channels session)))
    (protocol-error "commonware: channel ~D is already registered" channel))
  (setf (gethash channel (session-channels session)) handler)
  channel)

(defun channel-handler (session channel)
  "Return a registered channel's handler (possibly NIL); reject unknown channels."
  (multiple-value-bind (handler present) (gethash channel (session-channels session))
    (unless present (protocol-error "commonware: unregistered channel ~S" channel))
    handler))

(defun (setf channel-handler) (handler session channel)
  "Replace a registered channel's handler before concurrent I/O starts."
  (ensure-open session)
  (channel-handler session channel)
  (when handler (check-type handler function))
  (setf (gethash channel (session-channels session)) handler))

(defun new-session (max-message-size channels ping-interval-ms now-ms)
  (check-uint max-message-size 32)
  ;; CUPS V0's u32 record length also includes its 16-byte authentication tag.
  (when (> max-message-size (- #xffffffff +max-payload-overhead+ 16))
    (protocol-error "commonware: payload limit leaves no room for stream framing"))
  (check-type ping-interval-ms (integer 1 *))
  (check-type now-ms integer)
  (let ((session (%make-session :max-message-size max-message-size
                               :ping-interval-ms ping-interval-ms
                               :next-ping-ms (+ now-ms ping-interval-ms))))
    (dolist (channel channels) (register-channel session channel))
    session))

(defun wrap-connection (connection &key (max-message-size (* 16 1024 1024))
                                       (channels '(0 1 2 3 4 5))
                                       (ping-interval-ms 30000)
                                       (now-ms (clempo.io:now-ms)))
  "Adopt an already authenticated CUPS connection. Its plaintext limit must admit
MAX-MESSAGE-SIZE+16. CLOSE-SESSION owns it from successful return onward."
  (let ((session (new-session max-message-size channels ping-interval-ms now-ms)))
    (setf (session-connection session) connection)
    session))

(defun close-session (session)
  "Close the owned stream, including after malformed or unauthenticated input."
  (unless (session-closed-p session)
    (setf (session-closed-p session) t)
    (when (session-connection session)
      (clempo.cw-stream:close-connection (session-connection session))))
  t)

(defun dial (stream seed peer &key (namespace +tempo-network-namespace+)
                                  (max-message-size (* 16 1024 1024))
                                  (channels '(0 1 2 3 4 5)) (ping-interval-ms 30000)
                                  (now-ms (clempo.io:now-ms)) (max-age-ms 10000)
                                  (synchrony-ms 5000) ephemeral-secret
                                  (sake-version 0) (cups-version 0))
  "Authenticate an outbound binary stream to the expected PEER and adopt it.
NAMESPACE is the lookup network namespace, not its derived SAKE namespace.
SAKE-VERSION/CUPS-VERSION are configured, not negotiated (Tempo pins 0/0, the
default). The stream is closed if upgrade fails."
  (let ((session (new-session max-message-size channels ping-interval-ms now-ms)))
    (handler-case
        (setf (session-connection session)
              (clempo.cw-stream:dial
               stream seed peer (stream-namespace namespace)
               :version sake-version :cups-version cups-version
               :now-ms now-ms :max-age-ms max-age-ms :synchrony-ms synchrony-ms
               :max-message-size (+ max-message-size +max-payload-overhead+)
               :ephemeral-secret ephemeral-secret))
      (error (condition) (ignore-errors (close stream)) (error condition)))
    session))

(defun accept (stream seed bouncer &key (namespace +tempo-network-namespace+)
                                       (max-message-size (* 16 1024 1024))
                                       (channels '(0 1 2 3 4 5)) (ping-interval-ms 30000)
                                       (now-ms (clempo.io:now-ms)) (max-age-ms 10000)
                                       (synchrony-ms 5000) ephemeral-secret
                                       (sake-version 0) (cups-version 0))
  "Authenticate and adopt a caller-accepted binary stream. BOUNCER is mandatory:
it receives the remote public key and must return true only for permitted peers.
The caller must apply source-IP admission and handshake deadlines to STREAM.
No address advertisement or discovery handshake follows the SAKE exchange.
SAKE-VERSION/CUPS-VERSION must equal the dialer's; nothing negotiates them."
  (check-type bouncer function)
  (let ((session (new-session max-message-size channels ping-interval-ms now-ms)))
    (handler-case
        (setf (session-connection session)
              (clempo.cw-stream:accept
               stream seed (stream-namespace namespace)
               :version sake-version :cups-version cups-version
               :bouncer bouncer :now-ms now-ms :max-age-ms max-age-ms
               :synchrony-ms synchrony-ms
               :max-message-size (+ max-message-size +max-payload-overhead+)
               :ephemeral-secret ephemeral-secret))
      (error (condition) (ignore-errors (close stream)) (error condition)))
    session))

(defun connect (host port seed peer &rest options &key (timeout 10) &allow-other-keys)
  "Connect only to the explicitly supplied peer; remaining OPTIONS are DIAL's
(including :SAKE-VERSION/:CUPS-VERSION). TIMEOUT bounds TCP connect/I/O via the
owned transport. Never scans addresses."
  (let ((dial-options (copy-list options)))
    (remf dial-options :timeout)
    (let ((stream (clempo.p2p-wire:connect-tcp host port :timeout timeout)))
      (handler-case (apply #'dial stream seed peer dial-options)
        (error (condition) (ignore-errors (close stream)) (error condition))))))

(defmacro with-session-failure ((session) &body body)
  `(handler-case (progn ,@body)
     (error (condition)
       (ignore-errors (close-session ,session))
       (error condition))))

(defun send-data (session channel payload &key (start 0) (end (length payload)))
  "Send on a registered channel. No consensus message is constructed or signed."
  (ensure-open session)
  (channel-handler session channel)
  (let ((frame (encode-data channel payload :start start :end end
                            :max-message-size (session-max-message-size session))))
    (sb-thread:with-mutex ((session-send-lock session))
      (ensure-open session)
      (with-session-failure (session)
        (clempo.cw-stream:send-message (session-connection session) frame))))
  t)

(defun send-frames (session frames)
  "Send pre-encoded lookup frames (from ENCODE-DATA, or ENCODE-PING) as one batch
with a single flush, like commonware's send_many. Each frame is checked against
the session's channels and payload limit before anything is written."
  (ensure-open session)
  (dolist (frame frames)
    (multiple-value-bind (kind channel)
        (decode-message frame :max-message-size (session-max-message-size session))
      (when (eq kind :data) (channel-handler session channel))))
  (sb-thread:with-mutex ((session-send-lock session))
    (ensure-open session)
    (with-session-failure (session)
      (loop for (frame . more) on frames
            do (clempo.cw-stream:send-message (session-connection session) frame
                                              :flush (null more)))))
  t)

(defun send-ping (session &key if-due (now-ms (clempo.io:now-ms)))
  "Send Ping (no Pong exists). With IF-DUE, send only after the periodic deadline.
Returns true when sent. The caller schedules this independently of data traffic."
  (sb-thread:with-mutex ((session-send-lock session))
    (ensure-open session)
    (when (or (not if-due) (>= now-ms (session-next-ping-ms session)))
      (with-session-failure (session)
        (clempo.cw-stream:send-message (session-connection session) (encode-ping)))
      (setf (session-next-ping-ms session) (+ now-ms (session-ping-interval-ms session)))
      t)))

(defun receive-message (session)
  "Receive one authenticated lookup frame; return KIND, CHANNEL, FRAME, START,
END. Ping is only a keepalive and causes no response. Invalid input, unknown
channels, EOF, or transport errors close the connection. FRAME owns the payload."
  (sb-thread:with-mutex ((session-receive-lock session))
    (ensure-open session)
    (with-session-failure (session)
      (let ((frame (clempo.cw-stream:receive-message (session-connection session))))
        (multiple-value-bind (kind channel start end)
            (decode-message frame :max-message-size (session-max-message-size session))
          (when (eq kind :data) (channel-handler session channel))
          (values kind channel frame start end))))))

(defun receive-data (session)
  "Consume keepalives and return CHANNEL, FRAME, START, END for the next Data."
  (loop
    (multiple-value-bind (kind channel frame start end) (receive-message session)
      (when (eq kind :data) (return (values channel frame start end))))))

(defun dispatch-message (session)
  "Receive one message and invoke its channel handler, if installed. Return the
same five values as RECEIVE-MESSAGE. Handlers may forward FRAME[START,END) with
SEND-DATA. Handler errors propagate without classifying the peer as malformed."
  (multiple-value-bind (kind channel frame start end) (receive-message session)
    (when (eq kind :data)
      (let ((handler (channel-handler session channel)))
        (when handler (funcall handler session channel frame start end))))
    (values kind channel frame start end)))
