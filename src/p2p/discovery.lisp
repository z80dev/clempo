(defpackage #:clempo.discovery
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:ec #:clempo.secp256k1) (#:k #:clempo.keccak)
                    (#:rlp #:clempo.rlp) (#:aes #:clempo.aes) (#:sha #:clempo.sha2))
  (:import-from #:clempo.p2p-wire #:p2p-error #:protocol-error #:secure-equal-p)
  (:export #:peer #:make-peer #:parse-enode #:peer-host #:peer-public-key
           #:peer-udp-port #:peer-tcp-port #:peer-verified-p #:public-ip-p #:discover-peers)
  (:documentation "Bounded, client-side Ethereum discovery v4 and v5.1. Returned
identities have answered an endpoint-bound authenticated request, not merely appeared
in another node's routing table. Discovery does not establish chain membership.
Wire specifications: ethereum/devp2p discv4.md, discv5/{discv5-wire,discv5-theory}.md
and enr.md (EIP-778); forward-compatible v4 decoding follows EIP-8."))
(in-package #:clempo.discovery)

(defconstant +packet-limit+ 1280)
(defconstant +reply-limit+ 16)

(defstruct (peer (:constructor %make-peer (host address udp-port tcp-port public-key
                                              &optional verified-p)))
  "A public numeric endpoint and its secp256k1 identity. Accessor results must not
be mutated. VERIFIED-P means discovery endpoint proof, not Ethereum/Tempo status."
  (host "" :type string :read-only t)
  (address (make-octets 0) :type octets :read-only t)
  (udp-port 0 :type (unsigned-byte 16) :read-only t)
  (tcp-port 0 :type (unsigned-byte 16) :read-only t)
  (public-key (make-octets 0) :type octets :read-only t)
  (verified-p nil :type boolean :read-only t))

(defun numeric-address (host)
  (unless (and (stringp host) (<= 1 (length host) 45)
               (every (lambda (c) (or (digit-char-p c 16) (find c ".:"))) host))
    (protocol-error "discovery requires a numeric IP address"))
  (handler-case
      (map 'octets #'identity
           (if (find #\: host) (sb-bsd-sockets:make-inet6-address host)
               (sb-bsd-sockets:make-inet-address host)))
    (error () (protocol-error "invalid numeric discovery IP address"))))

(defun address-host (address)
  (if (= (length address) 4)
      (format nil "~D.~D.~D.~D" (aref address 0) (aref address 1)
              (aref address 2) (aref address 3))
      (format nil "~(~{~X~^:~}~)"
              (loop for i from 0 below 16 by 2
                    collect (+ (ash (aref address i) 8) (aref address (1+ i)))))))

(defun public-ip-p (address)
  "True for a usable public unicast numeric IP string or 4/16-octet address.
Reject private, loopback, link-local, multicast, documentation, benchmarking,
transition and reserved ranges. No DNS lookup or network operation is performed."
  (when (stringp address)
    (setf address (handler-case (numeric-address address) (p2p-error () nil))))
  (when (typep address 'octets)
    (case (length address)
      (4 (let ((a (aref address 0)) (b (aref address 1)) (c (aref address 2)))
           (and (< 0 a 224) (not (member a '(10 127)))
                (not (and (= a 100) (<= 64 b 127)))
                (not (and (= a 169) (= b 254)))
                (not (and (= a 172) (<= 16 b 31)))
                (not (and (= a 192)
                          (or (and (= b 0) (member c '(0 2)))
                              (and (= b 88) (= c 99)) (= b 168))))
                (not (and (= a 198)
                          (or (member b '(18 19)) (and (= b 51) (= c 100)))))
                (not (and (= a 203) (= b 0) (= c 113))))))
      (16 (let ((prefix (octets->integer address :end 4)))
            (and (= (ash prefix -29) 1) ; only 2000::/3 global unicast
                 (not (= (ash prefix -9) (ash #x20010000 -9))) ; IANA special /23
                 (not (= prefix #x20010db8))
                 (not (= (ash prefix -16) #x2002)) ; 6to4 can embed private IPv4
                 (not (= (ash prefix -12) #x3fff0))))))))

(defun valid-public-key-p (public)
  (and (typep public 'octets) (= (length public) 64)
       (let ((x (octets->integer public :end 32)) (y (octets->integer public :start 32)))
         (and (< x ec:+secp256k1-p+) (< y ec:+secp256k1-p+)
              (= (mod (* y y) ec:+secp256k1-p+)
                 (mod (+ (* x x x) 7) ec:+secp256k1-p+))))))

(defun make-peer (&key host udp-port tcp-port public-key)
  "Construct an unverified public endpoint. HOST must be a numeric IPv4/IPv6
string, UDP-PORT is 1..65535, TCP-PORT is 0..65535 (zero means unavailable),
and PUBLIC-KEY is the 64-octet uncompressed secp256k1 x||y. Invalid data signals
P2P-ERROR. The endpoint and key are copied; no DNS or network access occurs."
  (let ((address (numeric-address host)))
    (unless (and (public-ip-p address) (typep udp-port '(integer 1 65535))
                 (typep tcp-port '(integer 0 65535)) (valid-public-key-p public-key))
      (protocol-error "invalid or non-public discovery peer"))
    (%make-peer (address-host address) address udp-port tcp-port (copy-seq public-key))))

(defun decimal-port (text &key (allow-zero nil))
  (unless (and (<= 1 (length text) 5) (every (lambda (c) (digit-char-p c 10)) text))
    (protocol-error "invalid enode port"))
  (let ((port (parse-integer text)))
    (unless (<= (if allow-zero 0 1) port 65535) (protocol-error "invalid enode port"))
    port))

(defun parse-enode (text)
  "Parse enode://<128 hex digits>@<numeric IP>:<tcp>[?discport=<udp>].
Bracket IPv6 addresses. UDP defaults to TCP. Reject DNS, credentials, fragments,
unknown query parameters, invalid curve keys and non-public endpoints."
  (unless (and (stringp text) (<= 140 (length text) 256)
               (string= text "enode://" :end1 8) (char= (char text 136) #\@)
               (every (lambda (c) (digit-char-p c 16)) (subseq text 8 136)))
    (protocol-error "invalid enode URI"))
  (let* ((query (position #\? text :start 137)) (end (or query (length text)))
         (bracket-p (char= (char text 137) #\[))
         (host-end (if bracket-p (position #\] text :start 138 :end end)
                       (position #\: text :start 137 :end end))))
    (unless (and host-end
                 (if bracket-p (and (< (1+ host-end) end)
                                    (char= (char text (1+ host-end)) #\:)) t))
      (protocol-error "invalid enode endpoint"))
    (let* ((tcp (decimal-port (subseq text (+ host-end (if bracket-p 2 1)) end)
                              :allow-zero t))
           (udp (if query
                    (progn
                      (unless (and (<= (+ query 10) (length text))
                                   (string= text "?discport=" :start1 query :end1 (+ query 10)))
                        (protocol-error "invalid enode query"))
                      (decimal-port (subseq text (+ query 10))))
                    tcp)))
      (make-peer :host (subseq text (if bracket-p 138 137) host-end)
                 :udp-port udp :tcp-port tcp :public-key (hex->octets (subseq text 8 136))))))

;;; RLP views keep unknown EIP-8 extensions opaque, without recursive allocation.
(defstruct (item (:constructor %item (data encoded-start start end list-p)))
  data encoded-start start end list-p)

(defun item-at (data pos end)
  (multiple-value-bind (start stop list-p) (rlp:rlp-item-bounds data pos end)
    (%item data pos start stop list-p)))

(defun item-fields (item count &key exact)
  (unless (item-list-p item) (protocol-error "expected discovery RLP list"))
  (let ((pos (item-start item)) (fields nil))
    (loop repeat count do
      (let ((field (item-at (item-data item) pos (item-end item))))
        (push field fields)
        (setf pos (item-end field))))
    (when (and exact (/= pos (item-end item))) (protocol-error "extra discovery fields"))
    (nreverse fields)))

(defun item-children (item limit)
  (unless (item-list-p item) (protocol-error "expected discovery RLP list"))
  (loop with pos = (item-start item) with out = nil
        while (< pos (item-end item)) for n from 0
        do (when (>= n limit) (protocol-error "too many discovery list items"))
           (let ((child (item-at (item-data item) pos (item-end item))))
             (push child out) (setf pos (item-end child)))
        finally (return (nreverse out))))

(defun item-octets (item &optional size)
  (unless (and (not (item-list-p item))
               (or (null size) (= (- (item-end item) (item-start item)) size)))
    (protocol-error "invalid discovery byte string"))
  (subseq (item-data item) (item-start item) (item-end item)))

(defun item-integer (item max-bytes)
  (rlp:rlp-decode-integer (item-octets item) :max-bytes max-bytes))

(defun item-endpoint (item)
  (destructuring-bind (ip udp tcp) (item-fields item 3)
    (let ((address (item-octets ip)))
      (unless (member (length address) '(4 16)) (protocol-error "invalid discovery IP width"))
      (list address (item-integer udp 2) (item-integer tcp 2)))))

(defun signature-bytes (hash key &optional recovery-p)
  (multiple-value-bind (r s v) (ec:sign hash key)
    (octets-concat (integer->octets r 32) (integer->octets s 32)
                   (if recovery-p (octets-of v) (make-octets 0)))))

(defun signature-matches-p (hash signature public)
  (let ((r (octets->integer signature :end 32))
        (s (octets->integer signature :start 32 :end 64)))
    (loop for v below 4 thereis
          (let ((recovered (ec:ecrecover hash r s v)))
            (and recovered (octets= recovered public))))))

(defun encode-v4 (type fields key &optional (padding (make-octets 0)))
  (let* ((payload (octets-concat (octets-of type) (rlp:rlp-encode fields) padding))
         (signed (octets-concat (signature-bytes (k:keccak256 payload) key t) payload)))
    (unless (<= (+ 32 (length signed)) +packet-limit+) (protocol-error "oversize discovery packet"))
    (octets-concat (k:keccak256 signed) signed)))

(defstruct v4-packet type sender hash fields)

(defun decode-v4 (wire &key (now (clempo.io:now-seconds)))
  (handler-case
      (progn
        (unless (and (typep wire 'octets) (<= 99 (length wire) +packet-limit+)
                     (secure-equal-p (k:keccak256 wire :start 32) wire :count 32))
          (protocol-error "invalid discovery v4 packet hash or length"))
        (let* ((type (aref wire 97))
               (sender (and (<= (aref wire 96) 3)
                            (ec:ecrecover (k:keccak256 wire :start 97)
                                          (octets->integer wire :start 32 :end 64)
                                          (octets->integer wire :start 64 :end 96)
                                          (aref wire 96)))))
          (unless sender (protocol-error "invalid discovery v4 signature"))
          (let* ((count (case type (1 4) (2 3) (3 2) (4 2)
                         (otherwise (protocol-error "unsupported discovery v4 packet type"))))
                 (fields (item-fields (item-at wire 98 (length wire)) count))
                 (expiration (item-integer (nth (case type (1 3) (2 2) (otherwise 1)) fields) 8))
                 (decoded
                   (case type
                     (1 (list (item-integer (first fields) 8)
                              (item-endpoint (second fields)) (item-endpoint (third fields))))
                     (2 (list (item-endpoint (first fields)) (item-octets (second fields) 32)))
                     (3 (let ((target (item-octets (first fields) 64)))
                          (unless (valid-public-key-p target) (protocol-error "invalid findnode target"))
                          (list target)))
                     (4 (list
                         (loop for node in (item-children (first fields) +reply-limit+)
                               collect
                               (destructuring-bind (ip udp tcp id) (item-fields node 4 :exact t)
                                 (let ((address (item-octets ip)) (public (item-octets id 64))
                                       (udp (item-integer udp 2)) (tcp (item-integer tcp 2)))
                                   (unless (member (length address) '(4 16))
                                     (protocol-error "invalid neighbor IP width"))
                                   (when (and (public-ip-p address) (plusp udp)
                                              (valid-public-key-p public))
                                     (%make-peer (address-host address) address udp tcp public))))))))))
            (when (< expiration now) (protocol-error "expired discovery packet"))
            (make-v4-packet :type type :sender sender :hash (subseq wire 0 32) :fields decoded))))
    (rlp:rlp-error () (protocol-error "malformed discovery v4 RLP"))))

(defun monotonic-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun expiration () (+ (clempo.io:now-seconds) 20))

(defun query-v4 (peer key local-address local-port send receive deadline target)
  ;; SEND accepts octets; RECEIVE takes an absolute monotonic deadline and returns
  ;; one datagram or NIL. The transport is connected to PEER's exact UDP endpoint.
  (let* ((ping (encode-v4 1 (list 4 (list local-address local-port 0)
                                 (list (peer-address peer) (peer-udp-port peer) 0) (expiration)) key))
         (ping-hash (subseq ping 0 32)) (proved nil) (ping-seen nil) (pong-count 0)
         (bond-end (min deadline (+ (monotonic-seconds) 0.6)))
         (received 0) (neighbors nil))
    (labels ((read-packet (until)
               (loop while (and (< received 64) (< (monotonic-seconds) until))
                     for wire = (funcall receive until)
                     do (unless wire (return nil)) (incf received)
                     thereis
                     (handler-case
                         (let ((packet (decode-v4 wire)))
                           (when (octets= (v4-packet-sender packet) (peer-public-key peer)) packet))
                       (p2p-error () nil))))
             (answer-ping (packet)
               (when (< pong-count 2)
                 (incf pong-count)
                 (funcall send (encode-v4 2 (list (list (peer-address peer) (peer-udp-port peer) 0)
                                                   (v4-packet-hash packet) (expiration)) key)))
               (setf ping-seen t)))
      (funcall send ping)
      (loop while (and (< received 64) (< (monotonic-seconds) bond-end))
            for packet = (read-packet bond-end)
            unless packet do (return)
            do (when packet
                 (case (v4-packet-type packet)
                   (1 (answer-ping packet))
                   (2 (when (octets= ping-hash (second (v4-packet-fields packet)))
                        (setf proved t bond-end (min bond-end (+ (monotonic-seconds) 0.15)))))))
            when (and proved ping-seen) do (return))
      (unless proved (return-from query-v4 (values nil nil)))
      (when (< (monotonic-seconds) deadline)
        (funcall send (encode-v4 3 (list target (expiration)) key))
        (let ((until (min deadline (+ (monotonic-seconds) 0.5))))
          (loop while (and (< received 64) (< (length neighbors) +reply-limit+)
                           (< (monotonic-seconds) until))
                for packet = (read-packet until)
                unless packet do (return)
                do (when packet
                     (case (v4-packet-type packet)
                       (1 (answer-ping packet))
                       (4 (dolist (node (first (v4-packet-fields packet)))
                            (when (and node (< (length neighbors) +reply-limit+)
                                       (not (find (peer-public-key node) neighbors
                                                  :key #'peer-public-key :test #'octets=)))
                              (push node neighbors)))))))))
      (values t (nreverse neighbors)))))

;;; Discovery v5 uses compressed ECDH, unlike RLPx's raw shared x coordinate.
(defun compress-public (public)
  (octets-concat (octets-of (+ 2 (logand (aref public 63) 1))) (subseq public 0 32)))

(defun decompress-public (compressed)
  (unless (and (= (length compressed) 33) (member (aref compressed 0) '(2 3)))
    (protocol-error "invalid compressed secp256k1 key"))
  (let* ((p ec:+secp256k1-p+) (x (octets->integer compressed :start 1))
         (yy (mod (+ (* x x x) 7) p))
         (y (clempo.modexp:modular-expt yy (ash (1+ p) -2) p)))
    (unless (and (< x p) (= (mod (* y y) p) yy))
      (protocol-error "invalid compressed secp256k1 point"))
    (unless (= (logand y 1) (logand (aref compressed 0) 1)) (setf y (- p y)))
    (octets-concat (integer->octets x 32) (integer->octets y 32))))

(defun compressed-ecdh (key public)
  (unless (and (typep key `(integer 1 ,(1- ec:+secp256k1-n+)))
               (valid-public-key-p public))
    (protocol-error "invalid discovery ECDH key"))
  ;; Reuse the existing curve engine; the public ECDH API deliberately drops y.
  (multiple-value-bind (x y)
      (ec::secp-fast-mul key (ec::secp-table (octets->integer public :end 32)
                                           (octets->integer public :start 32)) 5)
    (unless x (protocol-error "invalid discovery shared point"))
    (octets-concat (octets-of (+ 2 (logand y 1))) (integer->octets x 32))))

(defun v5-keys (key public challenge initiator-id recipient-id)
  (let ((keys (sha:hkdf-sha256 challenge (compressed-ecdh key public)
                              (octets-concat (string->octets "discovery v5 key agreement")
                                             initiator-id recipient-id) 32)))
    (values (subseq keys 0 16) (subseq keys 16 32))))

(defun v5-id-signature (key challenge ephemeral destination-id)
  (signature-bytes (sha:sha256 (octets-concat (string->octets "discovery v5 identity proof")
                                             challenge ephemeral destination-id)) key))

(defun local-enr (key)
  ;; No external address is claimed: a short-lived client cannot infer a stable
  ;; public endpoint from its locally bound address or a single peer's assertion.
  (let* ((content (list 1 "id" "v4" "secp256k1" (compress-public (ec:private->public key))))
         (signature (signature-bytes (k:keccak256 (rlp:rlp-encode content)) key)))
    (rlp:rlp-encode (cons signature content))))

(defun octets-less-p (a b)
  (loop for x across a for y across b
        when (/= x y) do (return-from octets-less-p (< x y)))
  (< (length a) (length b)))

(defun decode-enr (record)
  "Return (values public-key sequence usable-public-peer-or-NIL) after EIP-778
signature, size, identity-scheme and sorted/unique key validation."
  (unless (<= (- (item-end record) (item-encoded-start record)) 300)
    (protocol-error "oversize ENR"))
  (let* ((fields (item-children record 64))
         (signature (and fields (item-octets (first fields) 64)))
         (sequence (and (rest fields) (item-integer (second fields) 8)))
         (pairs (cddr fields)) (previous nil) (table (make-hash-table :test 'equal)))
    (unless (and sequence (evenp (length pairs))) (protocol-error "invalid ENR fields"))
    (loop for (key value) on pairs by #'cddr
          for bytes = (item-octets key)
          do (when (and previous (not (octets-less-p previous bytes)))
               (protocol-error "ENR keys are not sorted and unique"))
             (setf previous bytes (gethash (map 'string #'code-char bytes) table) value))
    (let ((id (gethash "id" table)) (compressed (gethash "secp256k1" table)))
      (unless (and id compressed (octets= (item-octets id) (string->octets "v4")))
        (protocol-error "unsupported ENR identity scheme"))
      (let* ((public (decompress-public (item-octets compressed 33)))
             (content (rlp:rlp-encode
                       (loop for field in (rest fields)
                             collect (rlp:make-rlp-raw
                                      (subseq (item-data field) (item-encoded-start field) (item-end field)))))))
        (unless (signature-matches-p (k:keccak256 content) signature public)
          (protocol-error "invalid ENR signature"))
        (let ((ip nil) (ip6 nil) (udp 0) (tcp 0) (udp6 nil) (tcp6 nil))
          (flet ((port (name)
                   (let ((field (gethash name table))) (when field (item-integer field 2)))))
            (when (gethash "ip" table) (setf ip (item-octets (gethash "ip" table) 4)))
            (when (gethash "ip6" table) (setf ip6 (item-octets (gethash "ip6" table) 16)))
            (setf udp (or (port "udp") 0) tcp (or (port "tcp") 0)
                  udp6 (port "udp6") tcp6 (port "tcp6")))
          (values public sequence
                  (cond ((and (public-ip-p ip) (plusp udp))
                         (%make-peer (address-host ip) ip udp tcp public))
                        ((and (public-ip-p ip6) (plusp (or udp6 udp)))
                         (%make-peer (address-host ip6) ip6 (or udp6 udp) (or tcp6 tcp) public)))))))))

;;; SP 800-38D, restricted to discovery's AES-128, 96-bit nonce and 128-bit tag.
(defun gcm-multiply (x y)
  (loop with z = 0 with v = y for bit downfrom 127 to 0
        when (logbitp bit x) do (setf z (logxor z v))
        do (setf v (logxor (ash v -1) (if (oddp v) #xe1000000000000000000000000000000 0)))
        finally (return z)))

(defun gcm-tag (cipher nonce ad ciphertext)
  (let ((h (octets->integer (aes:aes-encrypt-block cipher (make-octets 16))))
        (state 0))
    (dolist (part (list ad ciphertext))
      (loop for start from 0 below (length part) by 16
            for end = (min (+ start 16) (length part))
            for block = (ash (octets->integer part :start start :end end) (* 8 (- 16 (- end start))))
            do (setf state (gcm-multiply (logxor state block) h))))
    (setf state (gcm-multiply (logxor state (ash (* 8 (length ad)) 64)
                                     (* 8 (length ciphertext))) h))
    (integer->octets
     (logxor state (octets->integer
                    (aes:aes-encrypt-block cipher (octets-concat nonce (octets-of 0 0 0 1))))) 16)))

(defun gcm-encrypt (key nonce plain ad)
  (let* ((cipher (aes:make-aes key))
         (ciphertext (aes:aes-ctr-xor (aes:make-aes-ctr cipher (octets-concat nonce (octets-of 0 0 0 2))) plain)))
    (octets-concat ciphertext (gcm-tag cipher nonce ad ciphertext))))

(defun gcm-decrypt (key nonce wire ad)
  (unless (>= (length wire) 16) (protocol-error "truncated discovery GCM tag"))
  (let* ((cipher (aes:make-aes key)) (ciphertext (subseq wire 0 (- (length wire) 16))))
    (unless (secure-equal-p (gcm-tag cipher nonce ad ciphertext) wire :start-b (length ciphertext))
      (protocol-error "discovery GCM authentication failed"))
    (aes:aes-ctr-xor (aes:make-aes-ctr cipher (octets-concat nonce (octets-of 0 0 0 2))) ciphertext)))

(defstruct v5-packet flag nonce authdata associated-data message)

(defun encode-v5 (destination flag nonce authdata &key key plain random-message
                                                        (iv (clempo.io:random-octets 16)))
  (let* ((header (octets-concat (string->octets "discv5") (octets-of 0 1 flag) nonce
                                (integer->octets (length authdata) 2) authdata))
         (ad (octets-concat iv header))
         (message (cond (random-message random-message) (key (gcm-encrypt key nonce plain ad))
                        (t (make-octets 0))))
         (wire (octets-concat iv (aes:aes-ctr-xor (aes:make-aes-ctr
                                                  (aes:make-aes (subseq destination 0 16)) iv) header)
                              message)))
    (unless (<= 63 (length wire) +packet-limit+) (protocol-error "invalid discovery v5 packet size"))
    wire))

(defun decode-v5 (wire destination)
  (unless (and (typep wire 'octets) (<= 63 (length wire) +packet-limit+))
    (protocol-error "invalid discovery v5 packet size"))
  (let* ((iv (subseq wire 0 16))
         (ctr (aes:make-aes-ctr (aes:make-aes (subseq destination 0 16)) iv))
         (static (aes:aes-ctr-xor ctr wire :start 16 :end 39))
         (flag (aref static 8)) (size (octets->integer static :start 21 :end 23))
         (end (+ 39 size)))
    (unless (and (octets= (subseq static 0 6) (string->octets "discv5"))
                 (= (aref static 6) 0) (= (aref static 7) 1) (<= end (length wire))
                 (case flag (0 (and (= size 32) (<= (+ end 16) (length wire))))
                       (1 (and (= size 24) (= end (length wire))))
                       (2 (and (>= size 34) (<= (+ end 16) (length wire))))))
      (protocol-error "invalid discovery v5 header"))
    (let ((auth (aes:aes-ctr-xor ctr wire :start 39 :end end)))
      (when (= flag 2)
        (unless (and (= (aref auth 32) 64) (= (aref auth 33) 33) (>= size 131))
          (protocol-error "invalid discovery v5 handshake sizes")))
      (make-v5-packet :flag flag :nonce (subseq static 9 21) :authdata auth
                      :associated-data (octets-concat iv static auth) :message (subseq wire end)))))

(defun v5-message (type fields)
  (octets-concat (octets-of type) (rlp:rlp-encode fields)))

(defun v5-message-fields (plain count)
  (unless (>= (length plain) 2) (protocol-error "truncated discovery v5 message"))
  (let* ((root (item-at plain 1 (length plain))) (fields (item-fields root count :exact t)))
    (unless (= (item-end root) (length plain)) (protocol-error "trailing discovery v5 message data"))
    (unless (<= (length (item-octets (first fields))) 8) (protocol-error "oversize discovery request ID"))
    fields))

(defun log-distance (a b)
  (integer-length (logxor (octets->integer a) (octets->integer b))))

(defun query-v5 (peer key send receive deadline target)
  (let* ((self (k:keccak256 (ec:private->public key)))
         (remote (k:keccak256 (peer-public-key peer)))
         (enr (local-enr key)) (counter 0) (received 0) (pong-count 0)
         (request (clempo.io:random-octets 8)) (write-key nil) (read-key nil)
         (seen (make-hash-table :test 'equalp)) (proved nil) (neighbors nil))
    (labels ((nonce ()
               (octets-concat (integer->octets (incf counter) 4) (clempo.io:random-octets 8)))
             (read-packet (until)
               (loop while (and (< received 64) (< (monotonic-seconds) until))
                     for wire = (funcall receive until)
                     do (unless wire (return nil)) (incf received)
                     thereis (handler-case (decode-v5 wire self) (p2p-error () nil))))
             (send-message (type fields)
               (funcall send (encode-v5 remote 0 (nonce) self :key write-key
                                           :plain (v5-message type fields))))
             (read-message (until)
               (loop for packet = (read-packet until) while packet
                     when (and (= (v5-packet-flag packet) 0)
                               (octets= (v5-packet-authdata packet) remote)
                               (not (gethash (v5-packet-nonce packet) seen)))
                       do (let ((plain (handler-case
                                           (gcm-decrypt read-key (v5-packet-nonce packet)
                                                        (v5-packet-message packet)
                                                        (v5-packet-associated-data packet))
                                         (p2p-error () nil))))
                            (when plain
                              (setf (gethash (v5-packet-nonce packet) seen) t)
                              (when (and (plusp (length plain)) (= (aref plain 0) 1))
                                (handler-case
                                    (let ((fields (v5-message-fields plain 2)))
                                      (item-integer (second fields) 8)
                                      (when (< pong-count 2)
                                        (incf pong-count)
                                        (send-message 2 (list (item-octets (first fields)) 1
                                                              (peer-address peer) (peer-udp-port peer)))))
                                  (p2p-error () nil) (rlp:rlp-error () nil)))
                              (return plain))))))
      (let* ((initial-nonce (nonce)) (until (min deadline (+ (monotonic-seconds) 1)))
             (challenge nil))
        (funcall send (encode-v5 remote 0 initial-nonce self
                                    :random-message (clempo.io:random-octets 20)))
        (loop for packet = (read-packet until) while packet
              when (and (= (v5-packet-flag packet) 1)
                        (octets= initial-nonce (v5-packet-nonce packet)))
                do (setf challenge packet) (return))
        (unless challenge (return-from query-v5 (values nil nil)))
        (let* ((ephemeral (1+ (clempo.io:random-below (1- ec:+secp256k1-n+))))
               (public (compress-public (ec:private->public ephemeral)))
               (data (v5-packet-associated-data challenge)))
          (multiple-value-setq (write-key read-key)
            (v5-keys ephemeral (peer-public-key peer) data self remote))
          (funcall send
                   (encode-v5 remote 2 (nonce)
                              (octets-concat self (octets-of 64 33)
                                             (v5-id-signature key data public remote) public enr)
                              :key write-key :plain (v5-message 1 (list request 1)))))
        (setf until (min deadline (+ (monotonic-seconds) 0.5)))
        (loop for plain = (read-message until) while plain
              do (handler-case
                     (when (and (plusp (length plain)) (= (aref plain 0) 2))
                       (destructuring-bind (id seq ip port) (v5-message-fields plain 4)
                         (item-integer seq 8)
                         (unless (member (length (item-octets ip)) '(4 16))
                           (protocol-error "invalid v5 pong IP"))
                         (item-integer port 2)
                         (when (octets= request (item-octets id)) (setf proved t))))
                   (p2p-error () nil) (rlp:rlp-error () nil))
              when proved do (return)))
      (unless proved (return-from query-v5 (values nil nil)))
      (let* ((distance (log-distance remote (k:keccak256 target)))
             (distances (remove-duplicates (remove-if-not (lambda (x) (<= 1 x 256))
                                                          (list distance (1- distance) (1+ distance)))))
             (id (clempo.io:random-octets 8))
             (until (min deadline (+ (monotonic-seconds) 0.5)))
             (total nil) (responses 0) (records 0))
        (when (< (monotonic-seconds) deadline)
          (send-message 3 (list id distances))
          (loop for plain = (read-message until) while plain
                do (handler-case
                       (when (and (plusp (length plain)) (= (aref plain 0) 4))
                         (destructuring-bind (response-id count entries) (v5-message-fields plain 3)
                           (when (octets= id (item-octets response-id))
                             (let ((n (item-integer count 1)))
                               (unless (and (<= 1 n +reply-limit+) (or (null total) (= total n)))
                                 (protocol-error "invalid discovery NODES response count"))
                               (setf total n)
                               (incf responses)
                               (dolist (record (item-children entries +reply-limit+))
                                 (incf records)
                                 (when (> records +reply-limit+) (protocol-error "too many discovery ENRs"))
                                 (multiple-value-bind (public seq node) (decode-enr record)
                                   (declare (ignore seq))
                                   (unless (member (log-distance remote (k:keccak256 public)) distances)
                                     (protocol-error "ENR is outside requested discovery distance"))
                                   (when (and node (not (find public neighbors :key #'peer-public-key :test #'octets=)))
                                     (push node neighbors))))))))
                     (p2p-error () nil) (rlp:rlp-error () nil))
                when (or (and total (>= responses total)) (>= records +reply-limit+)) do (return))))
      (values t (nreverse neighbors)))))

(defun open-datagram (peer)
  (let* ((address (peer-address peer))
         (socket (make-instance (if (= (length address) 4)
                                    'sb-bsd-sockets:inet-socket 'sb-bsd-sockets:inet6-socket)
                                :type :datagram :protocol :udp))
         (ready nil))
    (unwind-protect
         (progn
           ;; A connected UDP socket accepts only this exact source IP/port. It
           ;; also selects the real local route for the v4 ping's from endpoint.
           (sb-bsd-sockets:socket-connect socket address (peer-udp-port peer))
           (setf (sb-bsd-sockets:non-blocking-mode socket) t ready t)
           socket)
      (unless ready (sb-bsd-sockets:socket-close socket)))))

(defun datagram-receiver (socket)
  (let ((buffer (make-octets (1+ +packet-limit+))))
    (lambda (deadline)
      (let ((remaining (- deadline (monotonic-seconds))))
        (when (and (plusp remaining)
                   (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor socket)
                                               :input remaining nil))
          (multiple-value-bind (wire size) (sb-bsd-sockets:socket-receive socket buffer nil :dontwait t)
            (when (and wire (<= size +packet-limit+)) (subseq wire 0 size))))))))

(defun discover-peers (bootnodes &key (timeout 10) (max-peers 32) (max-queries 64)
                                   (protocols '(:v4 :v5)) key)
  "Discover public Ethereum endpoints from a list of enode strings or PEERs.
TIMEOUT is one total wall-duration budget in seconds (0..300); MAX-QUERIES bounds
queried endpoints (1..256), MAX-PEERS bounds returned identities (1..256). Initial
seeds are visited in input order before distance-sorted referrals. PROTOCOLS is a
nonempty, duplicate-free list of :V4 and/or :V5, tried in order at each endpoint;
use '(:V5) for independent v5 discovery. There is no port scanning or retransmission.
Only peers answering a signed matching v4 PONG or an identity-bound encrypted v5
PONG are returned; referrals alone are never marked verified. TCP port zero peers
can route a lookup but are not returned. No TCP connection or transaction is made.
KEY optionally supplies a secp256k1 scalar; otherwise a fresh ephemeral identity is
created. Invalid arguments/bootnodes signal P2P-ERROR; unreachable peers, malformed
network packets and deadline exhaustion yield the verified subset (possibly NIL).
This proves discovery identity/reachability only, not Tempo chain or validator status."
  (unless (and (realp timeout) (<= 0 timeout 300) (typep max-peers '(integer 1 256))
               (typep max-queries '(integer 1 256)) (listp bootnodes)
               (listp protocols) (<= 1 (length protocols) 2)
               (every (lambda (p) (member p '(:v4 :v5))) protocols)
               (= (length protocols) (length (remove-duplicates protocols)))
               (or (null key) (typep key `(integer 1 ,(1- ec:+secp256k1-n+)))))
    (protocol-error "invalid discovery bounds or key"))
  (when (zerop timeout) (return-from discover-peers nil))
  (let* ((deadline (+ (monotonic-seconds) timeout))
         (key (or key (1+ (clempo.io:random-below (1- ec:+secp256k1-n+)))))
         (target (ec:private->public key)) (target-hash (octets->integer (k:keccak256 target)))
         (queue nil) (seeds nil) (seen (make-hash-table :test 'equalp))
         (distances (make-hash-table :test 'equalp)) (results nil)
         (candidate-limit (min 1024 (* +reply-limit+ max-queries))) (candidates 0))
    (labels ((add-candidate (node &optional seed-p)
               (when (and (< candidates candidate-limit) (not (octets= (peer-public-key node) target))
                          (not (gethash (peer-public-key node) seen)))
                 (setf (gethash (peer-public-key node) seen) t
                       (gethash (peer-public-key node) distances)
                       (logxor target-hash (octets->integer (k:keccak256 (peer-public-key node)))))
                 (incf candidates)
                 (if seed-p (push node seeds) (push node queue))))
             (distance (node) (gethash (peer-public-key node) distances)))
      (loop for seed in bootnodes for n from 0
            do (when (>= n 1024) (protocol-error "too many discovery bootnodes"))
               (add-candidate (typecase seed
                                (string (parse-enode seed))
                                (peer (make-peer :host (peer-host seed) :udp-port (peer-udp-port seed)
                                                 :tcp-port (peer-tcp-port seed) :public-key (peer-public-key seed)))
                                (otherwise (protocol-error "invalid discovery bootnode"))) t))
      (setf seeds (nreverse seeds))
      (handler-case
          (sb-ext:with-timeout (max 0.001 (- deadline (monotonic-seconds)))
            (loop repeat max-queries
                  while (and (or seeds queue) (< (length results) max-peers) (< (monotonic-seconds) deadline))
                  do (unless seeds (setf queue (stable-sort queue #'< :key #'distance)))
                     (let* ((node (if seeds (pop seeds) (pop queue))) (socket nil))
                       (handler-case
                           (unwind-protect
                                (progn
                                  (setf socket (open-datagram node))
                                  (let ((receive (datagram-receiver socket)))
                                    (multiple-value-bind (local-address local-port) (sb-bsd-sockets:socket-name socket)
                                      (dolist (protocol protocols)
                                        (when (< (monotonic-seconds) deadline)
                                          (multiple-value-bind (verified referrals)
                                              (flet ((send (wire)
                                                       (sb-bsd-sockets:socket-send socket wire nil :dontwait t)))
                                                (if (eq protocol :v4)
                                                    (query-v4 node key (map 'octets #'identity local-address) local-port
                                                              #'send receive deadline target)
                                                    (query-v5 node key #'send receive deadline target)))
                                            (when verified
                                              (when (plusp (peer-tcp-port node))
                                                (push (%make-peer (peer-host node) (peer-address node)
                                                                  (peer-udp-port node) (peer-tcp-port node)
                                                                  (peer-public-key node) t) results))
                                              (dolist (referral referrals) (add-candidate referral))
                                              (return))))))))
                             (when socket (sb-bsd-sockets:socket-close socket)))
                         (sb-bsd-sockets:socket-error () nil)))))
        (sb-ext:timeout () nil)))
    (nreverse results)))
