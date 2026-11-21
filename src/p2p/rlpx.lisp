(defpackage #:clempo.rlpx
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:aes #:clempo.aes) (#:sha #:clempo.sha2)
                    (#:ec #:clempo.secp256k1) (#:k #:clempo.keccak) (#:rlp #:clempo.rlp))
  (:import-from #:clempo.p2p-wire #:protocol-error #:read-octets #:write-octets
                #:secure-equal-p #:xor-octets)
  (:export #:rlpx-dial #:rlpx-accept #:rlpx-send #:rlpx-receive #:rlpx-close
           #:rlpx-peer #:rlpx-public-key))

(in-package #:clempo.rlpx)

(define-constant +ecies-overhead+ 113) ; public key (65), IV (16), MAC (32)
(define-constant +frame-limit+ #xffffff)

(defun random-key () (ec:random-private-key))

(defun shared-x (key public)
  (or (ec:ecdh key public) (protocol-error "invalid secp256k1 public key")))

(defun ecies-keys (secret)
  ;; Concat KDF (SEC 1): one SHA-256 block suffices for two 128-bit keys.
  (let ((ctx (sha:make-sha256)))
    (sha:sha256-update ctx (octets-of 0 0 0 1))
    (sha:sha256-update ctx secret)
    (let ((material (sha:sha256-final ctx)))
      (values (aes:make-aes (subseq material 0 16)) (sha:sha256 material :start 16)))))

(defun ecies-encrypt (public clear &key (key (random-key)) (iv (clempo.io:random-octets 16)))
  "Return the full EIP-8 packet, including the authenticated size prefix."
  (let* ((size (+ (length clear) +ecies-overhead+))
         (wire (make-octets (+ 2 size))))
    (invariant (<= size #xffff) "oversize ECIES packet")
    (setf (aref wire 0) (ash size -8) (aref wire 1) (logand size 255) (aref wire 2) 4)
    (replace wire (ec:private->public key) :start1 3)
    (replace wire iv :start1 67)
    (multiple-value-bind (aes mac-key) (ecies-keys (shared-x key public))
      (let ((cipher (aes:aes-ctr-xor (aes:make-aes-ctr aes iv) clear)))
        (replace wire cipher :start1 83)
        (replace wire (sha:hmac-sha256 mac-key iv cipher (subseq wire 0 2)) :start1 (- (+ 2 size) 32))))
    wire))

(defun ecies-decrypt (key wire)
  (unless (and (<= (+ 2 +ecies-overhead+) (length wire) #x10001)
               (= (octets->integer wire :end 2) (- (length wire) 2))
               (= (aref wire 2) 4))
    (protocol-error "invalid EIP-8 ECIES packet"))
  (let* ((tag-start (- (length wire) 32))
         (iv (subseq wire 67 83))
         (cipher (subseq wire 83 tag-start)))
    (multiple-value-bind (aes mac-key) (ecies-keys (shared-x key (subseq wire 3 67)))
      (unless (secure-equal-p (sha:hmac-sha256 mac-key iv cipher (subseq wire 0 2))
                              wire :start-b tag-start)
        (protocol-error "EIP-8 ECIES authentication failed"))
      (aes:aes-ctr-xor (aes:make-aes-ctr aes iv) cipher :out cipher))))

(defun read-handshake (stream key)
  (let* ((prefix (read-octets stream 2))
         (n (octets->integer prefix))
         (packet (octets-concat prefix (read-octets stream n))))
    (values (ecies-decrypt key packet) packet)))

(defun handshake-items (clear count)
  ;; Decode only requested top-level fields, skipping EIP-8 extensions without
  ;; recursing into peer-controlled trailing data. Padding is outside the list.
  (multiple-value-bind (pos end list-p) (rlp:rlp-item-bounds clear 0 (length clear))
    (unless list-p (protocol-error "expected EIP-8 RLP list"))
    (loop repeat count collect
          (multiple-value-bind (start stop nested) (rlp:rlp-item-bounds clear pos end)
            (when nested (protocol-error "invalid EIP-8 field"))
            (setf pos stop)
            (subseq clear start stop)))))

(defun auth-packet (key remote-key ephemeral nonce)
  (multiple-value-bind (r s recovery) (ec:sign (xor-octets (shared-x key remote-key) nonce) ephemeral)
    (ecies-encrypt remote-key
                   (octets-concat (rlp:rlp-encode
                                   (list (octets-concat (integer->octets r 32) (integer->octets s 32)
                                                        (octets-of recovery))
                                         (ec:private->public key) nonce 4))
                                  (clempo.io:random-octets (+ 100 (clempo.io:random-below 200)))))))

(defun parse-auth (key clear)
  (destructuring-bind (signature remote nonce version) (handshake-items clear 4)
    (rlp:rlp-decode-integer version :max-bytes 8)
    (unless (and (= (length signature) 65) (= (length remote) 64) (= (length nonce) 32))
      (protocol-error "invalid EIP-8 auth fields"))
    (let ((ephemeral (ec:ecrecover (xor-octets (shared-x key remote) nonce)
                                   (octets->integer signature :end 32)
                                   (octets->integer signature :start 32 :end 64)
                                   (aref signature 64))))
      (unless ephemeral (protocol-error "invalid EIP-8 ephemeral signature"))
      (values remote ephemeral nonce))))

(defun parse-ack (clear)
  (destructuring-bind (ephemeral nonce version) (handshake-items clear 3)
    (rlp:rlp-decode-integer version :max-bytes 8)
    (unless (and (= (length ephemeral) 64) (= (length nonce) 32))
      (protocol-error "invalid EIP-8 ack fields"))
    (values ephemeral nonce)))

(defun derive-secrets (ephemeral remote-ephemeral initiator-nonce recipient-nonce)
  (let* ((ecdh (shared-x ephemeral remote-ephemeral))
         (nonce-hash (k:keccak256-concat recipient-nonce initiator-nonce))
         (shared (k:keccak256-concat ecdh nonce-hash))
         (aes (k:keccak256-concat ecdh shared)))
    (values aes (k:keccak256-concat ecdh aes))))

(defstruct (frame-state (:constructor %make-frame-state (ctr mac-aes digest)))
  ctr mac-aes digest
  (scratch (k:make-keccak) :read-only t)
  (mix-block (make-octets 16) :type (octets 16) :read-only t))

(defun make-frame-state (aes-key mac-key seed &optional packet)
  (let ((digest (k:make-keccak)))
    (k:keccak-update digest seed)
    (when packet (k:keccak-update digest packet))
    (%make-frame-state (aes:make-aes-ctr (aes:make-aes aes-key) (make-octets 16))
                       (aes:make-aes mac-key) digest)))

(defun frame-digest (state)
  (k:keccak-digest (frame-state-digest state) (frame-state-scratch state)))

(defun mix-mac (state data offset digest)
  (declare (type octets data digest) (type fixnum offset) #.*hot*)
  (let ((tmp (frame-state-mix-block state)))
    (replace tmp digest :end2 16)
    (aes:aes-encrypt-block (frame-state-mac-aes state) tmp tmp)
    (dotimes (i 16)
      (setf (aref tmp i) (logxor (aref tmp i) (aref data (+ offset i)))))
    (k:keccak-update (frame-state-digest state) tmp)
    (frame-digest state)))

(defun header-mac (state header)
  (mix-mac state header 0 (frame-digest state)))

(defun body-mac (state body start end)
  (k:keccak-update (frame-state-digest state) body :start start :end end)
  (let ((digest (frame-digest state))) (mix-mac state digest 0 digest)))

(defun padded-size (n) (logandc2 (+ n 15) 15))

(defun encode-frame (state payload)
  (declare (type octets payload))
  (let* ((n (length payload))
         (body-end (+ 32 (padded-size n))))
    (unless (<= n +frame-limit+) (protocol-error "oversize RLPx frame"))
    (let ((wire (make-octets (+ body-end 16))))
      (setf (aref wire 0) (ldb (byte 8 16) n)
            (aref wire 1) (ldb (byte 8 8) n)
            (aref wire 2) (ldb (byte 8 0) n)
            (aref wire 3) #xc2 (aref wire 4) #x80 (aref wire 5) #x80)
      (replace wire payload :start1 32)
      (aes:aes-ctr-xor (frame-state-ctr state) wire :end 16 :out wire)
      (replace wire (header-mac state wire) :start1 16 :end2 16)
      (aes:aes-ctr-xor (frame-state-ctr state) wire :start 32 :end body-end :out wire :out-start 32)
      (replace wire (body-mac state wire 32 body-end) :start1 body-end :end2 16)
      wire)))

(defun decode-header (state wire)
  (unless (and (= (length wire) 32) (secure-equal-p (header-mac state wire) wire :start-b 16 :count 16))
    (protocol-error "RLPx header authentication failed"))
  (aes:aes-ctr-xor (frame-state-ctr state) wire :end 16 :out wire)
  (unless (and (= (aref wire 3) #xc2) (= (aref wire 4) #x80) (= (aref wire 5) #x80))
    (protocol-error "unsupported RLPx frame header"))
  (octets->integer wire :end 3))

(defun decode-body (state n wire &optional with-padding)
  (let ((padded (padded-size n)))
    (unless (and (= (length wire) (+ padded 16))
                 (secure-equal-p (body-mac state wire 0 padded) wire :start-b padded :count 16))
      (protocol-error "RLPx body authentication failed"))
    ;; CTR consumes the padding too, but it is not part of the delivered message.
    (aes:aes-ctr-xor (frame-state-ctr state) wire :end padded :out wire)
    (values (if with-padding wire (subseq wire 0 n)) n)))

(defstruct (rlpx (:constructor %make-rlpx (stream peer public-key ingress egress)))
  stream peer public-key ingress egress broken-p)

(defun establish (stream key remote ephemeral remote-ephemeral nonce-a nonce-b auth ack initiator-p)
  (multiple-value-bind (aes mac) (derive-secrets ephemeral remote-ephemeral nonce-a nonce-b)
    (let ((a->b (make-frame-state aes mac (xor-octets mac nonce-b) auth))
          (b->a (make-frame-state aes mac (xor-octets mac nonce-a) ack)))
      (%make-rlpx stream remote (ec:private->public key)
                  (if initiator-p b->a a->b) (if initiator-p a->b b->a)))))

(defun rlpx-dial (stream remote &key (key (random-key)))
  "Initiate an EIP-8 session on STREAM, authenticating REMOTE's 64-octet public key."
  (let* ((ephemeral (random-key)) (nonce (clempo.io:random-octets 32))
         (auth (auth-packet key remote ephemeral nonce)))
    (write-octets stream auth)
    (force-output stream)
    (multiple-value-bind (clear ack) (read-handshake stream key)
      (multiple-value-bind (remote-ephemeral nonce-b) (parse-ack clear)
        (establish stream key remote ephemeral remote-ephemeral nonce nonce-b auth ack t)))))

(defun rlpx-accept (stream key)
  "Accept an EIP-8 session on STREAM using our static private scalar KEY."
  (multiple-value-bind (clear auth) (read-handshake stream key)
    (multiple-value-bind (remote remote-ephemeral nonce-a) (parse-auth key clear)
      (let* ((ephemeral (random-key)) (nonce-b (clempo.io:random-octets 32))
             (ack (ecies-encrypt remote
                                 (octets-concat (rlp:rlp-encode (list (ec:private->public ephemeral) nonce-b 4))
                                                (clempo.io:random-octets (+ 100 (clempo.io:random-below 200)))))))
        (write-octets stream ack)
        (force-output stream)
        (establish stream key remote ephemeral remote-ephemeral nonce-a nonce-b auth ack nil)))))

(defun rlpx-send (connection payload)
  (when (rlpx-broken-p connection) (protocol-error "RLPx session failed"))
  (let ((stream (rlpx-stream connection)))
    (write-octets stream (encode-frame (rlpx-egress connection) payload))
    (force-output stream)))

(defun rlpx-close (connection)
  (close (rlpx-stream connection) :abort t))

(defun rlpx-receive (connection &key with-padding)
  "Read the next authenticated payload and return it and its length. WITH-PADDING
returns the backing octets without copying; only the indicated prefix is payload.
A failed read invalidates the session; its owner still closes the stream."
  (when (rlpx-broken-p connection) (protocol-error "RLPx session failed"))
  (let ((complete nil))
    (unwind-protect
         (let* ((stream (rlpx-stream connection))
                (state (rlpx-ingress connection))
                (size (decode-header state (read-octets stream 32))))
           (multiple-value-prog1
               (decode-body state size (read-octets stream (+ (padded-size size) 16)) with-padding)
             (setf complete t)))
      (unless complete (setf (rlpx-broken-p connection) t)))))
