(defpackage #:clempo.validator-process.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:net #:clempo.validator) (#:sock #:sb-bsd-sockets) (#:json #:clempo.json)
                    (#:cw #:clempo.cwp2p)))
(in-package #:clempo.validator-process.tests)

;;; Every network here listens on loopback with an ephemeral port.

(deftest validator-priority-queues ()
  (let ((queues (net::make-queues '(2 3) 10)))
    (is (net::queues-put queues 1 :low-1 3))
    (is (net::queues-put queues 1 :low-2 3))
    (is (net::queues-put queues 0 :high-1 1))
    (is (not (net::queues-put queues 0 :oversize 4)))     ; byte budget is shared
    (is (net::queues-put queues 0 :high-2 1))
    (is (not (net::queues-put queues 0 :overcount 0)))    ; ring 0 holds two
    ;; High priority drains first, batches stop at the limit, then low in order.
    (is-equalp '(:high-1 :high-2 :low-1) (net::queues-take queues 3 0))
    (is-equalp '(:low-2) (net::queues-take queues 8 0))
    (is-equalp '() (net::queues-take queues 8 0.001)))
  ;; The inbox serves status first, then rotates over channel rings.
  (let ((queues (net::make-queues '(4 4 4) 100 :rotate t)))
    (net::queues-put queues 1 :a1 1) (net::queues-put queues 1 :a2 1)
    (net::queues-put queues 2 :b1 1) (net::queues-put queues 0 :status 0)
    (is-equalp '(:status :a1 :b1 :a2) (net::queues-take queues 8 0))))

(deftest validator-network-address-policy ()
  (is-equalp (octets-of 10 1 2 3) (net::parse-ipv4 "10.1.2.3"))
  (dolist (bad '("10.1.2" "10.1.2.256" "a.b.c.d" "1.2.3.4.5" "" "1..2.3" "01234.1.1.1"))
    (is (null (net::parse-ipv4 bad))))
  (is (net::global-ipv4-p (octets-of 8 8 8 8)))
  (dolist (private '((127 0 0 1) (10 0 0 1) (192 168 1 1) (172 16 0 1) (172 31 255 255)
                     (169 254 1 1) (100 64 0 1) (0 1 2 3) (224 0 0 1) (255 255 255 255)
                     (192 0 2 1) (198 51 100 1) (203 0 113 1) (198 18 0 1)))
    (is (not (net::global-ipv4-p (apply #'octets-of private)))))
  (is (net::global-ipv4-p (octets-of 172 32 0 1)))
  (is-equalp '((0 . 128) (1 . 128) (2 . 128) (3 . 8) (4 . 8) (5 . 128)) (net:tempo-channel-quotas))
  (is= 2 (cdr (assoc 4 (net:tempo-channel-quotas :backfill-frequency 2))))
  (is (eq :low (net::default-priority 3)))
  (is (eq :high (net::default-priority 5))))

(defun ordered-seeds (n)
  "N seeds sorted by their public keys, so tests know which side dials."
  (sort (loop for i from 1 to n collect (make-array 32 :element-type 'octet :initial-element (* 17 i)))
        (lambda (a b)
          (loop for x across (clempo.ed25519:ed25519-public-key a)
                for y across (clempo.ed25519:ed25519-public-key b)
                when (/= x y) return (< x y)))))

(defun key-of (seed) (clempo.ed25519:ed25519-public-key seed))

(defparameter *test-options*
  (list :listen-host "127.0.0.1" :allow-private-ips t :handshake-ip-period 0.0001
        :handshake-subnet-period 0.0001 :peer-connection-cooldown 0.05 :dial-frequency 0.05
        :ping-interval 2 :block-seconds 1.5 :handshake-timeout 2 :dial-timeout 3))

(defun call-with-test-network (seed function &rest options)
  (apply #'net:call-with-validator-network function :seed seed (append options *test-options*)))

(defmacro with-test-network ((var seed &rest options) &body body)
  `(call-with-test-network ,seed (lambda (,var) ,@body) ,@options))

(defun entry (seed network)
  (list (key-of seed) "127.0.0.1" (net:network-listen-port network)))

(defun await (predicate &key (seconds 8))
  (loop with end = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        for value = (funcall predicate)
        when value return value
        while (< (get-internal-real-time) end)
        do (sleep 0.02)))

(defun connected-p (network seed)
  (member (key-of seed) (net:network-peers network) :test #'octets=))

(defun drain-data (network &key (seconds 0.2))
  "Collect :DATA events for SECONDS, as (channel . payload)."
  (loop with end = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        while (< (get-internal-real-time) end)
        for event = (net:network-receive network :timeout 0.02)
        when (and event (eq :data (net:network-event-kind event)))
          collect (cons (net:network-event-channel event)
                        (subseq (net:network-event-frame event)
                                (net:network-event-start event) (net:network-event-end event)))))

(defun pair-exchange (&rest options)
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (apply #'call-with-test-network s1
           (lambda (a)
             (apply #'call-with-test-network s2
                    (lambda (b)
                      (is (net:network-track a 1 (list (entry s2 b))))
                      (is (net:network-track b 7 (list (entry s1 a))))
                      (is (await (lambda () (and (connected-p a s2) (connected-p b s1)))))
                      (dotimes (channel 6)
                        (is= 1 (net:network-send a channel (octets-of channel 1)))
                        (is= 1 (net:network-send b channel (octets-of channel 2)
                                                 :recipients (list (key-of s1)))))
                      (let ((at-a (drain-data a :seconds 1)) (at-b (drain-data b :seconds 0.3)))
                        (is-equalp '(0 1 2 3 4 5) (sort (mapcar #'car at-a) #'<))
                        (is-equalp '(0 1 2 3 4 5) (sort (mapcar #'car at-b) #'<))
                        (is (every (lambda (item) (equalp (cdr item) (octets-of (car item) 2))) at-a))
                        (is (every (lambda (item) (equalp (cdr item) (octets-of (car item) 1))) at-b))))
                    options))
           options)))

(deftest validator-network-all-channels-v0 ()
  (pair-exchange))

(deftest validator-network-all-channels-v1 ()
  (pair-exchange :sake-version 1 :cups-version 1))

(deftest validator-network-version-mismatch-fails-closed ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (a s1)
      (with-test-network (b s2 :sake-version 1 :cups-version 1)
        (net:network-track a 1 (list (entry s2 b)))
        (net:network-track b 1 (list (entry s1 a)))
        (sleep 1.5)
        (is (null (net:network-peers a)))
        (is (null (net:network-peers b)))))))

(defun raw-connect (seed network &key (sake-version 0) (cups-version 0))
  (cw:connect "127.0.0.1" (net:network-listen-port network) seed (net::validator-network-key network)
              :timeout 3 :max-message-size net::+tempo-max-message-size+
              :sake-version sake-version :cups-version cups-version))

(deftest validator-network-refuses-untracked ()
  (destructuring-bind (stranger s1 s2) (ordered-seeds 3)
    (with-test-network (b s2)
      ;; Nothing tracked: even loopback is an unregistered source address.
      (signals error (raw-connect stranger b))
      (with-test-network (a s1)
        (net:network-track b 1 (list (entry s1 a)))
        ;; The address is registered now, but the identity is not.
        (signals error (raw-connect stranger b))
        (net:network-track a 1 (list (entry s2 b)))
        (is (await (lambda () (connected-p b s1))))
        ;; Untracked identities are never listed as peers.
        (is-equalp (list (key-of s1)) (net:network-peers b))
        ;; Stale or repeated set ids are ignored.
        (is (null (net:network-track b 1 nil)))
        (is (null (net:network-track b 0 nil)))
        (is (connected-p b s1))
        ;; Malformed peer lists are rejected before any state changes.
        (signals error (net:network-track b 5 (list (list (octets-of 1 2) "127.0.0.1" 1))))
        (signals error (net:network-track b 5 (list (entry s1 a) (entry s1 a))))
        (signals error (net:network-track b 5 (list (list (key-of s1) "127.0.0.1" 0))))
        (signals error (net:network-track b 5 (list (list (key-of s1) "127.0.0.1" 1 "host"))))
        (is (connected-p b s1))))))

(deftest validator-network-retrack-disconnects ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (a s1)
      (with-test-network (b s2)
        (net:network-track a 1 (list (entry s2 b)))
        (net:network-track b 1 (list (entry s1 a)))
        (is (await (lambda () (connected-p a s2))))
        ;; The listener's new set omits the dialer: it is disconnected and refused.
        (is (net:network-track b 2 nil))
        (is (await (lambda () (and (null (net:network-peers a)) (null (net:network-peers b))))))
        (sleep 0.5)
        (is (null (net:network-peers b)))
        (is (net:network-track b 3 (list (entry s1 a))))
        (is (await (lambda () (and (connected-p a s2) (connected-p b s1)))))
        ;; The dialer's new set omits the listener: it disconnects and stops dialing.
        (is (net:network-track a 2 nil))
        (is (await (lambda () (null (net:network-peers b)))))
        (sleep 0.5)
        (is (null (net:network-peers a)))
        (is (net:network-track a 3 (list (entry s2 b))))
        (is (await (lambda () (connected-p b s1))))))))

(deftest validator-network-retains-tracked-sets ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (a s1)
      (with-test-network (b s2 :tracked-sets 2)
        (net:network-track a 1 (list (entry s2 b)))
        (net:network-track b 1 (list (entry s1 a)))
        (is (await (lambda () (connected-p b s1))))
        ;; With two retained sets, the peer survives one set that omits it.
        (net:network-track b 2 nil)
        (sleep 0.3)
        (is (connected-p b s1))
        (net:network-track b 3 nil)
        (is (await (lambda () (null (net:network-peers b)))))))))

(deftest validator-network-block ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (dolist (blocker '(:dialer :listener))
      (with-test-network (a s1)
        (with-test-network (b s2)
          (net:network-track a 1 (list (entry s2 b)))
          (net:network-track b 1 (list (entry s1 a)))
          (is (await (lambda () (connected-p a s2))))
          (if (eq blocker :dialer)
              (is (net:network-block a (key-of s2)))
              (is (net:network-block b (key-of s1))))
          (is (not (net:network-block a (key-of s1))))   ; never ourselves
          (is (await (lambda () (and (null (net:network-peers a)) (null (net:network-peers b))))))
          ;; Refused and not redialed for the block period (1.5 s)...
          (sleep 0.8)
          (is (null (net:network-peers a)))
          (is (null (net:network-peers b)))
          ;; ...then allowed again.
          (is (await (lambda () (and (connected-p a s2) (connected-p b s1))))))))))

(deftest validator-network-inbound-quota ()
  (destructuring-bind (flooder s1 s2) (ordered-seeds 3)
    (with-test-network (b s2)
      (with-test-network (a s1)
        ;; The flooder's key is lowest, so B never dials it; it dials B itself.
        (net:network-track b 1 (list (entry s1 a) (list (key-of flooder) "127.0.0.1" 1)))
        (net:network-track a 1 (list (entry s2 b)))
        (is (await (lambda () (connected-p b s1))))
        (let ((session (raw-connect flooder b)) (start (get-internal-real-time)))
          (unwind-protect
               (progn
                 (is (await (lambda () (connected-p b flooder))))
                 ;; 24 broadcast messages bypass the sender's quota (8/s, burst 8).
                 (dotimes (i 24) (cw:send-data session 3 (octets-of i)))
                 (cw:send-data session 0 (octets-of 99))
                 (is= 1 (net:network-send a 0 (octets-of 42)))
                 (let* ((early (drain-data b :seconds 0.5))
                        (elapsed (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
                   ;; Another peer's votes flow during the flood.
                   (is (member (cons 0 (octets-of 42)) early :test #'equalp))
                   ;; The flooder is held to burst + rate x elapsed (commonware waits)...
                   (is (<= (count 3 early :key #'car) (+ 8 (ceiling (* 8 elapsed)) 1)))
                   (is (not (member (cons 0 (octets-of 99)) early :test #'equalp)))
                   ;; ...and nothing admitted is lost: the rest arrives in order at 8/s.
                   (let ((all (append early (drain-data b :seconds 3))))
                     (is-equalp (loop for i below 24 collect (cons 3 (octets-of i)))
                               (remove 0 all :key #'car))
                     (is (member (cons 0 (octets-of 99)) all :test #'equalp)))))
            (cw:close-session session)))))))

(deftest validator-network-outbound-quota ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (a s1)
      (with-test-network (b s2)
        (net:network-track a 1 (list (entry s2 b)))
        (net:network-track b 1 (list (entry s1 a)))
        (is (await (lambda () (connected-p a s2))))
        ;; Broadcast is 8/s with burst 8 per recipient; votes are 128/s.
        (is= 8 (loop repeat 20 sum (net:network-send a 3 (octets-of 1))))
        (is= 1 (net:network-send a 0 (octets-of 2)))
        (sleep 0.3)
        (is (<= 2 (loop repeat 20 sum (net:network-send a 3 (octets-of 3))) 3))
        (let ((data (drain-data b :seconds 0.5)))
          (is (<= 10 (count 3 data :key #'car) 11))
          (is= 1 (count 0 data :key #'car)))
        ;; Oversized and unregistered sends are caller errors.
        (signals error (net:network-send a 9 (octets-of 1)))
        (signals error (net:network-send a 0 (make-octets (1+ net::+tempo-max-message-size+))))
        (signals error (net:network-send a 0 (octets-of 1) :priority :urgent))))))

(deftest validator-network-high-priority-first ()
  (destructuring-bind (reader s1) (ordered-seeds 2)
    (with-test-network (a s1 :channel-quotas '((0 . 1000) (3 . 1000)))
      (net:network-track a 1 (list (list (key-of reader) "127.0.0.1" 1)))
      (let ((session (cw:connect "127.0.0.1" (net:network-listen-port a) reader (key-of s1)
                                 :timeout 5 :channels '(0 3)
                                 :max-message-size net::+tempo-max-message-size+)))
        (unwind-protect
             (progn
               (is (await (lambda () (connected-p a reader))))
               ;; The reader is not reading yet, so the low-priority backlog builds up.
               (let ((payload (make-octets 65536)))
                 (is= 100 (loop repeat 100 sum (net:network-send a 3 payload))))
               (is= 1 (net:network-send a 0 (octets-of 7)))
               (let ((order (loop repeat 101 collect (cw:receive-data session))))
                 (is= 1 (count 0 order))
                 (is (< (position 0 order) 50))))
          (cw:close-session session))))))

(defun open-stalled-client (port)
  (let ((socket (make-instance 'sock:inet-socket :type :stream :protocol :tcp)))
    (sock:socket-connect socket #(127 0 0 1) port)
    socket))

(defun closed-by-peer-p (socket seconds)
  "True when the peer closes SOCKET within SECONDS without sending anything."
  (let ((stream (sock:socket-make-stream socket :input t :element-type 'octet :timeout seconds)))
    (handler-case (eq :eof (read-byte stream nil :eof))
      (sb-sys:io-timeout () nil)
      (error () t))))

(deftest validator-network-handshakes-off-accept-thread ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (b s2 :handshake-timeout 4)
      (with-test-network (a s1)
        (net:network-track b 1 (list (entry s1 a)))
        ;; A TCP client that never speaks holds a handshake slot, not the listener.
        (let ((stalled (open-stalled-client (net:network-listen-port b))))
          (unwind-protect
               (progn
                 (sleep 0.2)
                 (net:network-track a 1 (list (entry s2 b)))
                 (is (await (lambda () (connected-p b s1)) :seconds 2)))
            (sock:socket-close stalled)))))))

(deftest validator-network-handshake-cap ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    (with-test-network (b s2 :max-concurrent-handshakes 1 :handshake-timeout 1)
      (net:network-track b 1 (list (list (key-of s1) "127.0.0.1" 1)))
      (let ((first (open-stalled-client (net:network-listen-port b))) (second nil) (third nil))
        (unwind-protect
             (progn
               (sleep 0.2)
               ;; The slot is taken: a further connection is closed at once.
               (setf second (open-stalled-client (net:network-listen-port b)))
               (is (closed-by-peer-p second 0.5))
               ;; The handshake deadline frees the slot and closes the staller.
               (is (closed-by-peer-p first 2))
               (setf third (open-stalled-client (net:network-listen-port b)))
               (is (not (closed-by-peer-p third 0.3))))
          (dolist (socket (list first second third))
            (when socket (ignore-errors (sock:socket-close socket)))))))))

(deftest validator-network-source-policy ()
  (destructuring-bind (s1 s2) (ordered-seeds 2)
    ;; Private sources are refused before any handshake unless allowed.
    (with-test-network (b s2 :allow-private-ips nil)
      (net:network-track b 1 (list (list (key-of s1) "127.0.0.1" 1)))
      (let ((socket (open-stalled-client (net:network-listen-port b))))
        (unwind-protect (is (closed-by-peer-p socket 1))
          (sock:socket-close socket))))
    ;; The per-IP handshake quota closes a second attempt inside the period.
    (with-test-network (b s2 :handshake-ip-period 5)
      (net:network-track b 1 (list (list (key-of s1) "127.0.0.1" 1)))
      (let ((first (open-stalled-client (net:network-listen-port b)))
            (second (open-stalled-client (net:network-listen-port b))))
        (unwind-protect
             (progn (is (closed-by-peer-p second 1))
                    (is (not (closed-by-peer-p first 0.3))))
          (sock:socket-close first)
          (sock:socket-close second))))))
