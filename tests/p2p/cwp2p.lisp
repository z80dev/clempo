(defpackage #:clempo.cwp2p.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:cw #:clempo.cwp2p)))

(in-package #:clempo.cwp2p.tests)

(defun fixture-frames ()
  (clempo.json:parse-json
   (clempo.io:read-file-string
    (asdf:system-relative-pathname "clempo-p2p"
                                  "../../tests/p2p/data/tempo_devnet_frames.json"))))

(defun repeated-byte (byte)
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element byte))

(defun call-with-session-pair (function &key (dialer-channels '(0 1 2 3 4 5))
                                           (listener-channels '(0 1 2 3 4 5))
                                           (max-message-size 1024)
                                           (admit t))
  "Real SAKE/CUPS over loopback TCP, never a public address or validator identity."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (worker nil) (dialer nil) (listener nil) (accepted-stream nil)
        (client-stream nil))
    (unwind-protect
         (sb-ext:with-timeout 15
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen socket 1)
           (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
             (declare (ignore address))
             (setf worker
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (let ((accepted (sb-bsd-sockets:socket-accept socket)))
                            (setf accepted-stream
                                  (sb-bsd-sockets:socket-make-stream
                                   accepted :input t :output t :element-type '(unsigned-byte 8)
                                   :buffering :none :timeout 5))
                            (setf listener
                                  (cw:accept
                                   accepted-stream (repeated-byte 2)
                                   (lambda (peer)
                                     (and admit
                                          (octets= peer (hex->octets
                                                        "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c"))))
                                   :channels listener-channels :max-message-size max-message-size
                                   :now-ms 1700000000000 :ephemeral-secret (repeated-byte 4))))
                        (error (condition) condition)))
                    :name "commonware loopback accept"))
             (setf client-stream (clempo.p2p-wire:connect-tcp "127.0.0.1" port :timeout 5))
             (setf dialer
                   (cw:dial client-stream (repeated-byte 1)
                            (hex->octets "8139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b394")
                            :channels dialer-channels :max-message-size max-message-size
                            :now-ms 1700000000000 :ephemeral-secret (repeated-byte 3)))
             (let ((result (sb-thread:join-thread worker)))
               (when (typep result 'error) (error result)))
             (funcall function dialer listener)))
      (when dialer (ignore-errors (cw:close-session dialer)))
      (when listener (ignore-errors (cw:close-session listener)))
      (when client-stream (ignore-errors (close client-stream)))
      (when accepted-stream (ignore-errors (close accepted-stream)))
      (ignore-errors (sb-bsd-sockets:socket-close socket))
      (when worker
        (when (sb-thread:thread-alive-p worker)
          (ignore-errors (sb-thread:terminate-thread worker)))
        (sb-thread:join-thread worker :default nil)))))

(deftest lookup-wire-vectors ()
  (is-equalp (hex->octets "0554454d504f5f5032505f53545245414d") (cw:stream-namespace))
  (is-equalp (hex->octets "01") (cw:encode-ping))
  (is-equalp (hex->octets "000000") (cw:encode-data 0 (make-octets 0)))
  (is-equalp (hex->octets "00800103616263")
             (cw:encode-data 128 (string->octets "abc")))
  (is-equalp (hex->octets "00ffffffffffffffffff0100")
             (cw:encode-data #xffffffffffffffff (make-octets 0)))
  (multiple-value-bind (kind channel start end) (cw:decode-message (octets-of 1))
    (is (eq :ping kind)) (is (null channel)) (is= 1 start) (is= 1 end))
  (let ((frame (hex->octets "00800103616263")))
    (multiple-value-bind (kind channel start end) (cw:decode-message frame)
      (is (eq :data kind)) (is= 128 channel) (is= 4 start) (is= 7 end)
      (is-equalp (string->octets "abc") (subseq frame start end))))
  (is-equalp (hex->octets "000102bbcc")
             (cw:encode-data 1 (hex->octets "aabbccdd") :start 1 :end 3)))

(deftest lookup-varint-boundaries ()
  (dolist (channel '(0 1 127 128 16383 16384 4294967295 9223372036854775808
                    18446744073709551615))
    (let ((frame (cw:encode-data channel (octets-of 0 255))))
      (multiple-value-bind (kind decoded start end) (cw:decode-message frame)
        (is (eq :data kind)) (is= channel decoded)
        (is= 2 (- end start)) (is-equalp (octets-of 0 255) (subseq frame start end))))))

(deftest lookup-malformed-frames ()
  (dolist (hex '("" "02" "03" "ff" "0100" "00" "0080" "00800000"
                 "00ffffffffffffffffff0200" "008080808080808080808000"
                 "00008000" "00008080808010" "0000808080808000"
                 "000001" "00000000" "00000200"))
    (signals clempo.p2p-wire:p2p-error (cw:decode-message (hex->octets hex))))
  (signals clempo.p2p-wire:p2p-error
    (cw:decode-message (hex->octets "00000100") :max-message-size 0))
  (is (eq :data (cw:decode-message (hex->octets "000000") :max-message-size 0)))
  (signals clempo.p2p-wire:p2p-error (cw:encode-data -1 (make-octets 0)))
  (signals clempo.p2p-wire:p2p-error (cw:encode-data (ash 1 64) (make-octets 0)))
  (signals clempo.p2p-wire:p2p-error
    (cw:encode-data 0 (octets-of 1) :max-message-size 0))
  (signals clempo.p2p-wire:p2p-error (cw:encode-data 0 (octets-of 1) :start 2))
  (signals clempo.p2p-wire:p2p-error (cw:encode-data 0 (octets-of 1) :end -1)))

(deftest mux-wire-vectors ()
  (is-equalp (hex->octets "25010203") (cw:encode-mux 37 (octets-of 1 2 3)))
  (is-equalp (hex->octets "800100") (cw:encode-mux 128 (octets-of 0)))
  (is-equalp (hex->octets "ffffffffffffffffff01aa")
             (cw:encode-mux #xffffffffffffffff (octets-of #xaa)))
  (multiple-value-bind (subchannel start end)
      (cw:decode-mux (hex->octets "ff8001aabbff") :start 1 :end 5)
    (is= 128 subchannel) (is= 3 start) (is= 5 end))
  (multiple-value-bind (subchannel start end) (cw:decode-mux (octets-of 0))
    (is= 0 subchannel) (is= 1 start) (is= 1 end))
  (dolist (hex '("" "80" "8000" "ffffffffffffffffff02" "80808080808080808080"))
    (signals clempo.p2p-wire:p2p-error (cw:decode-mux (hex->octets hex))))
  (signals clempo.p2p-wire:p2p-error (cw:encode-mux (ash 1 64) (make-octets 0))))

(deftest recorded-tempo-devnet-payloads ()
  ;; These are exact application payloads from Rust Tempo c8b89f9, not a capture
  ;; of the outer lookup envelope. Outer headers below follow commonware's codec.
  (let ((records (fixture-frames)) (peers (make-hash-table :test #'equal))
        (counts (make-array 6 :initial-element 0)))
    (is= 36 (length records))
    (loop for record across records
          for channel = (clempo.json:jget record "channel")
          for payload = (hex->octets (clempo.json:jget record "data"))
          for length-hex = (cdr (assoc (length payload)
                                      '((134 . "8601") (133 . "8501") (323 . "c302")
                                        (667 . "9b05") (101 . "65") (100 . "64"))))
          do (is length-hex)
             (incf (aref counts channel))
             (setf (gethash (clempo.json:jget record "from") peers) t)
             (let ((expected (octets-concat (octets-of 0 channel)
                                            (hex->octets length-hex) payload)))
               (is-equalp expected (cw:encode-data channel payload))
               (multiple-value-bind (kind decoded start end) (cw:decode-message expected)
                 (is (eq :data kind)) (is= channel decoded)
                 (is-equalp payload (subseq expected start end))
                 (when (member channel '(0 1 5))
                   (multiple-value-bind (epoch body-start body-end)
                       (cw:decode-mux expected :start start :end end)
                     (is= 37 epoch) (is= (1+ start) body-start) (is= end body-end)
                     (is-equalp payload
                                (cw:encode-mux epoch expected :start body-start :end body-end)))))))
    (is-equalp #(9 9 0 9 0 9) counts)
    (is= 3 (hash-table-count peers))))

(deftest authenticated-session-recorded-payloads ()
  (call-with-session-pair
   (lambda (dialer listener)
     (is-equalp (hex->octets "8139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b394")
                (cw:session-peer dialer))
     (is-equalp (hex->octets "8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c")
                (cw:session-peer listener))
     ;; Forward received slices unchanged. Both directions use real encrypted
     ;; records; no peer fixture's validator key is used to impersonate it.
     (dolist (channel '(0 1 2 3 4 5))
       (setf (cw:channel-handler listener channel)
             (lambda (session channel frame start end)
               (cw:send-data session channel frame :start start :end end))))
     (loop for record across (fixture-frames)
           for channel = (clempo.json:jget record "channel")
           for payload = (hex->octets (clempo.json:jget record "data"))
           do (cw:send-data dialer channel payload)
              (cw:dispatch-message listener)
              (multiple-value-bind (received frame start end) (cw:receive-data dialer)
                (is= channel received)
                (is-equalp payload (subseq frame start end)))))))

(deftest authenticated-session-keepalive-and-channels ()
  (call-with-session-pair
   (lambda (dialer listener)
     (is (not (cw:send-ping dialer :if-due t :now-ms 1700000029999)))
     (is (cw:send-ping dialer :if-due t :now-ms 1700000030000))
     (is (not (cw:send-ping dialer :if-due t :now-ms 1700000030000)))
     (is (eq :ping (cw:receive-message listener)))
     (cw:send-ping listener :now-ms 1700000030000)
     (cw:send-data listener 0 (octets-of 9))
     (multiple-value-bind (channel frame start end) (cw:receive-data dialer)
       (is= 0 channel) (is-equalp (octets-of 9) (subseq frame start end)))
     (signals clempo.p2p-wire:p2p-error (cw:register-channel dialer 0))
     (signals clempo.p2p-wire:p2p-error (cw:send-data dialer 99 (make-octets 0)))
     (signals clempo.p2p-wire:p2p-error
       (cw:send-data dialer 0 (make-octets 1025)))
     (is (not (cw:session-closed-p dialer)))
     (cw:register-channel dialer 99)
     (cw:register-channel listener 99)
     (cw:send-data dialer 99 (make-octets 0))
     (multiple-value-bind (channel frame start end) (cw:receive-data listener)
       (declare (ignore frame)) (is= 99 channel) (is= start end))
     (cw:close-session dialer)
     (is (cw:session-closed-p dialer))
     (is (cw:close-session dialer))
     (signals clempo.p2p-wire:p2p-error (cw:send-ping dialer)))))

(deftest authenticated-session-unknown-channel-closes ()
  (call-with-session-pair
   (lambda (dialer listener)
     (cw:send-data dialer 1 (octets-of 7))
     (signals clempo.p2p-wire:p2p-error (cw:receive-message listener))
     (is (cw:session-closed-p listener)))
   :dialer-channels '(0 1) :listener-channels '(0)))

(deftest authenticated-session-malformed-frame-closes ()
  (call-with-session-pair
   (lambda (dialer listener)
     ;; Valid authenticated CUPS ciphertext containing an invalid lookup frame.
     (clempo.cw-stream:send-message (cw::session-connection dialer) (octets-of 1 0))
     (signals clempo.p2p-wire:p2p-error (cw:receive-message listener))
     (is (cw:session-closed-p listener)))))

(deftest authenticated-session-admission-required ()
  ;; The rejected dial can observe either EOF or a TCP reset, depending on the OS.
  (signals error
    (call-with-session-pair (lambda (dialer listener) (declare (ignore dialer listener)))
                            :admit nil)))
