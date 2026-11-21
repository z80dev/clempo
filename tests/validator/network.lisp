(defpackage #:clempo.validator-process.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:net #:clempo.validator) (#:sock #:sb-bsd-sockets) (#:json #:clempo.json)))
(in-package #:clempo.validator-process.tests)

(deftest bounded-validator-mailbox ()
  (let ((box (net::make-mailbox (make-array 2 :initial-element nil) 5)))
    (is (net::enqueue box :first 3))
    (is (not (net::enqueue box :oversize 3)))
    (is (net::enqueue box :second 2))
    (is (not (net::enqueue box :overcount 0)))
    (is= :first (net::dequeue box 0))
    (is (net::enqueue box :wrapped 3))
    (is= :second (net::dequeue box 0))
    (is= :wrapped (net::dequeue box 0))
    (is= nil (net::dequeue box 0.001))))

(defun open-test-socket ()
  (let ((socket (make-instance 'sock:inet-socket :type :stream :protocol :tcp)))
    (sock:socket-bind socket #(127 0 0 1) 0)
    socket))

(defun test-ports ()
  (let ((a (open-test-socket)) (b nil))
    (unwind-protect
         (progn
           (setf b (open-test-socket))
           (values (nth-value 1 (sock:socket-name a)) (nth-value 1 (sock:socket-name b))))
      (when b (sock:socket-close b))
      (sock:socket-close a))))

(defun test-slot (seed path slot port rpc-port)
  (json:jobject "slot" slot "public_key" (octets->hex (clempo.ed25519:ed25519-public-key seed))
               "consensus_address" (format nil "127.0.0.1:~D" port)
               "rpc_url" (format nil "http://127.0.0.1:~D" rpc-port)
               "signing_seed" (namestring path) "signing_share" (namestring path)))

(defun until-event (network kind)
  (loop with end = (+ (get-internal-real-time) (* 5 internal-time-units-per-second))
        while (< (get-internal-real-time) end)
        for event = (net:network-receive network :timeout 0.1)
        when (and event (eq (net:network-event-kind event) kind)) return event
        finally (error "No validator event of kind ~S" kind)))

(deftest validator-manifest-admission ()
  (signals error (net::loopback-port "localhost:4444" "127.0.0.1:"))
  (signals error (net::loopback-port "127.0.0.1:4444/ignored" "127.0.0.1:"))
  (signals error (net::loopback-port "127.0.0.1:0" "127.0.0.1:"))
  (is= 4444 (net::loopback-port "127.0.0.1:4444" "127.0.0.1:")))

(deftest validator-authenticated-links ()
  (multiple-value-bind (port-a port-b) (test-ports)
    (uiop:with-temporary-file (:pathname a-path :stream a-stream :direction :output :element-type 'octet)
      (uiop:with-temporary-file (:pathname b-path :stream b-stream :direction :output :element-type 'octet)
        (uiop:with-temporary-file (:pathname path :stream out :direction :output)
          (let* ((a (make-array 32 :element-type 'octet :initial-element 7))
                 (b (make-array 32 :element-type 'octet :initial-element 8))
                 (a-key (clempo.ed25519:ed25519-public-key a))
                 (b-key (clempo.ed25519:ed25519-public-key b)))
            (write-sequence a a-stream) (force-output a-stream)
            (write-sequence b b-stream) (force-output b-stream)
            (clempo.json:write-json
             (json:jobject "version" 1 "genesis" (namestring path) "epoch_length" 60
                           "validators" (vector (test-slot a a-path 0 port-a port-a)
                                                (test-slot b b-path 1 port-b port-b))) out)
            (force-output out)
            (let* ((manifest (net:read-devnet-manifest path))
                   (a-slot (net:manifest-slot manifest 0)) (b-slot (net:manifest-slot manifest 1)))
              (net:call-with-validator-network
               manifest a-slot
               (lambda (network-a)
                 (net:call-with-validator-network
                  manifest b-slot
                  (lambda (network-b)
                    (is (octets= b-key (net:network-event-peer (until-event network-a :connected))))
                    (is (octets= a-key (net:network-event-peer (until-event network-b :connected))))
                    (is= 1 (net:network-send network-a 3 (octets-of 10 12 17) :recipients (list b-key)))
                    (let ((event (until-event network-b :data)))
                      (is= 3 (net:network-event-channel event))
                      (is-equalp (octets-of 10 12 17)
                                 (subseq (net:network-event-frame event)
                                         (net:network-event-start event) (net:network-event-end event)))))))))))))))
