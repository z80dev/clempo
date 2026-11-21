(in-package #:clempo.validator)

(defconstant +network-message-limit+ (* 16 1024 1024))
(defconstant +network-inbox-limit+ (* 64 1024 1024))
(defconstant +network-outbox-limit+ (* 32 1024 1024))

(defstruct (mailbox (:constructor make-mailbox (cells byte-limit)))
  (cells #() :type simple-vector :read-only t)
  (byte-limit 0 :type fixnum :read-only t)
  (next 0 :type fixnum)
  (count 0 :type fixnum)
  (bytes 0 :type fixnum)
  (mutex (thr:make-mutex :name "validator mailbox") :read-only t)
  (ready (thr:make-waitqueue :name "validator mailbox") :read-only t))

(defun enqueue (box value size)
  (thr:with-mutex ((mailbox-mutex box))
    (let* ((cells (mailbox-cells box)) (capacity (length cells)) (count (mailbox-count box)))
      (when (and (< count capacity) (<= (+ (mailbox-bytes box) size) (mailbox-byte-limit box)))
        (setf (aref cells (mod (+ (mailbox-next box) count) capacity)) (cons size value))
        (incf (mailbox-count box))
        (incf (mailbox-bytes box) size)
        (thr:condition-notify (mailbox-ready box))
        t))))

(defun dequeue (box timeout)
  (thr:with-mutex ((mailbox-mutex box))
    (when (and (zerop (mailbox-count box)) (plusp timeout))
      (unless (thr:condition-wait (mailbox-ready box) (mailbox-mutex box) :timeout timeout)
        (return-from dequeue nil)))
    (when (plusp (mailbox-count box))
      (let* ((cells (mailbox-cells box)) (next (mailbox-next box)) (cell (aref cells next)))
        (setf (aref cells next) nil (mailbox-next box) (mod (1+ next) (length cells)))
        (decf (mailbox-count box))
        (decf (mailbox-bytes box) (car cell))
        (cdr cell)))))

(defstruct (network-event (:constructor event (kind peer &optional channel frame start end)))
  kind peer channel frame start end)

(defstruct (link (:constructor make-link (session stream)))
  session stream
  (outbox (make-mailbox (make-array 128 :initial-element nil) +network-outbox-limit+)
          :read-only t))

(defstruct (remote (:constructor make-remote (slot)))
  slot link)

(defstruct (validator-network (:constructor %make-network))
  seed key listener remotes
  (lock (thr:make-mutex :name "validator peers") :read-only t)
  (inbox (make-mailbox (make-array 512 :initial-element nil) +network-inbox-limit+)
         :read-only t)
  (stopping nil)
  workers)

(defun network-stopping-p (network)
  (thr:with-mutex ((validator-network-lock network)) (validator-network-stopping network)))

(defun remote-for-key (network key)
  (find key (validator-network-remotes network) :test #'octets=
                                               :key (lambda (remote) (slot-public-key (remote-slot remote)))))

(defun dial-preferred-p (network remote)
  ;; For two Lisp peers only one direction may open. Otherwise simultaneous
  ;; handshakes can each keep the connection the other side just closed.
  (loop for local across (validator-network-key network)
        for peer across (slot-public-key (remote-slot remote))
        when (/= local peer) return (< local peer)))

(defun install-link (network remote session stream)
  (thr:with-mutex ((validator-network-lock network))
    (cond ((and remote (not (validator-network-stopping network)) (not (remote-link remote)))
           (setf (remote-link remote) (make-link session stream)))
          (t nil))))

(defun current-link (network remote)
  (thr:with-mutex ((validator-network-lock network)) (remote-link remote)))

(defun close-link (network remote link)
  (thr:with-mutex ((validator-network-lock network))
    (when (eq link (remote-link remote)) (setf (remote-link remote) nil)))
  (ignore-errors (cw:close-session (link-session link))))

(defun network-peers (network)
  "Snapshot the authenticated, active peer identities."
  (thr:with-mutex ((validator-network-lock network))
    (loop for remote in (validator-network-remotes network)
          when (remote-link remote) collect (slot-public-key (remote-slot remote)))))

(defun network-receive (network &key (timeout 0))
  "Dequeue the next :CONNECTED, :DISCONNECTED or :DATA event, or NIL at TIMEOUT seconds."
  (dequeue (validator-network-inbox network) timeout))

(defun network-send (network channel bytes &key recipients)
  "Enqueue immutable BYTES on CHANNEL. RECIPIENTS defaults to all peers; returns
how many queues accepted the message. Each link has both entry and byte limits."
  (let ((count 0) (item (cons channel bytes)))
    (check-type bytes octets)
    (thr:with-mutex ((validator-network-lock network))
      (dolist (remote (validator-network-remotes network))
        (let ((link (remote-link remote)) (key (slot-public-key (remote-slot remote))))
          (when (and link (or (null recipients) (member key recipients :test #'octets=))
                     (enqueue (link-outbox link) item (length bytes)))
            (incf count)))))
    count))

(defun ready-for-input (stream timeout)
  (or (listen stream) (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream) :input timeout)))

(defun run-link (network link)
  (let* ((session (link-session link)) (stream (link-stream link))
         (peer (cw:session-peer session)) (inbox (validator-network-inbox network)))
    (enqueue inbox (event :connected peer) 0)
    (unwind-protect
         (loop until (network-stopping-p network) do
           (loop repeat 8 for item = (dequeue (link-outbox link) 0)
                 while item do (cw:send-data session (car item) (cdr item)))
           (cw:send-ping session :if-due t)
           (when (ready-for-input stream 0.015)
             (multiple-value-bind (kind channel frame start end) (cw:receive-message session)
               (when (eq kind :data)
                 (unless (enqueue inbox (event :data peer channel frame start end) (length frame))
                   (clempo.p2p-wire:protocol-error "validator inbound mailbox full"))))))
      (enqueue inbox (event :disconnected peer) 0))))

(defun note-link-failure (slot condition)
  (format *error-output* "~&validator: peer slot ~D disconnected: ~A~%" (slot-number slot) condition))

(defun dial-link (network remote)
  (let* ((slot (remote-slot remote))
         (stream (clempo.p2p-wire:connect-tcp "127.0.0.1" (slot-port slot) :timeout 1))
         (installed nil))
    (unwind-protect
         (let ((session (sb-ext:with-timeout 3
                          (cw:dial stream (validator-network-seed network) (slot-public-key slot)
                                   :max-message-size +network-message-limit+ :ping-interval-ms 5000))))
           (setf installed (install-link network remote session stream)))
      (unless installed (ignore-errors (close stream :abort t))))))

(defun run-peer (network remote)
  (let ((slot (remote-slot remote)) (dial-p (dial-preferred-p network remote)))
    (unwind-protect
         (loop until (network-stopping-p network) do
           (let ((link (or (current-link network remote)
                           (and dial-p (ignore-errors (dial-link network remote))))))
             (if link
                 (unwind-protect
                      (handler-case (run-link network link)
                        (error (condition)
                          (unless (network-stopping-p network) (note-link-failure slot condition))))
                   (close-link network remote link))
                 ;; Rust attempts both directions; stagger retries around its
                 ;; authenticated lookup reservation window.
                 (sleep (if dial-p (/ (+ 100 (io:random-below 350)) 1000.0) 0.05)))))
      (let ((link (current-link network remote)))
        (when link (close-link network remote link))))))

(defun accept-link (network socket address)
  (let ((stream nil) (installed nil))
    (unwind-protect
         (when (equalp address #(127 0 0 1))
           (let ((remote nil))
             (setf (sock:sockopt-tcp-nodelay socket) t
                   stream (sock:socket-make-stream socket :input t :output t :element-type 'octet
                                                         :buffering :full :timeout 1 :auto-close t))
             (let ((session (sb-ext:with-timeout 3
                              (cw:accept stream (validator-network-seed network)
                                         (lambda (identity)
                                           (setf remote (remote-for-key network identity))
                                           (and remote (not (dial-preferred-p network remote))
                                                (not (current-link network remote))))
                                         :max-message-size +network-message-limit+ :ping-interval-ms 5000))))
               (setf installed (install-link network remote session stream)))))
      (unless installed
        (if stream (ignore-errors (close stream :abort t)) (ignore-errors (sock:socket-close socket)))))))

(defun run-listener (network)
  (let* ((listener (validator-network-listener network)) (fd (sock:socket-file-descriptor listener)))
    (loop until (network-stopping-p network) do
      (when (sb-sys:wait-until-fd-usable fd :input 0.1)
        (multiple-value-bind (socket address) (sock:socket-accept listener)
          (ignore-errors (accept-link network socket address)))))))

(defun call-with-validator-network (manifest slot function)
  "Open the chosen devnet slot's loopback listener, authenticate to known peers,
call FUNCTION with the network, and join every worker before returning."
  (let* ((seed (read-slot-seed slot))
         (listener (make-instance 'sock:inet-socket :type :stream :protocol :tcp))
         (network (%make-network :seed seed :key (slot-public-key slot) :listener listener
                                 :remotes (loop for other across (manifest-slots manifest)
                                                unless (eq slot other) collect (make-remote other)))))
    (unwind-protect
         (progn
           (setf (sock:sockopt-reuse-address listener) t)
           (sock:socket-bind listener #(127 0 0 1) (slot-port slot))
           (sock:socket-listen listener (length (manifest-slots manifest)))
           (push (thr:make-thread (lambda () (run-listener network)) :name "validator listener")
                 (validator-network-workers network))
           (dolist (remote (validator-network-remotes network))
             (let ((remote remote))
               (push (thr:make-thread (lambda () (run-peer network remote)) :name "validator peer")
                     (validator-network-workers network))))
           (funcall function network))
      (thr:with-mutex ((validator-network-lock network)) (setf (validator-network-stopping network) t))
      (dolist (worker (validator-network-workers network)) (thr:join-thread worker :default nil))
      (sock:socket-close listener)
      (fill seed 0))))
