(defpackage #:clempo.discovery.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:d #:clempo.discovery) (#:ec #:clempo.secp256k1)
                    (#:k #:clempo.keccak) (#:rlp #:clempo.rlp))
  (:import-from #:clempo.p2p-wire #:p2p-error))
(in-package #:clempo.discovery.tests)

;;; Fixture provenance: literal vectors below are from Ethereum EIP-8's RLPx
;;; Discovery section, ethereum/devp2p/discv5/discv5-wire-test-vectors.md and
;;; ethereum/devp2p/enr.md (EIP-778). Other packets are deterministic fixtures
;;; built from private scalars 1,2,3 and explicitly specified RLP fields. Tests
;;; perform no socket operations and do not use public endpoints as transports.
(defun bytes (&rest chunks) (hex->octets (apply #'concatenate 'string chunks)))

(defun sk (n) "The private key of scalar N." (integer->octets n 32))

(defun test-peer (&optional (key 2) (host "8.8.8.8"))
  (d:make-peer :host host :udp-port 30303 :tcp-port 30303 :public-key (ec:private->public (sk key))))

(defun test-enr (key &optional (ip (octets-of 9 9 9 9)))
  (let* ((content (list 1 "id" "v4" "ip" ip "secp256k1"
                        (d::compress-public (ec:private->public (sk key))) "tcp" 30303 "udp" 30303))
         (signature (d::signature-bytes (k:keccak256 (rlp:rlp-encode content)) (sk key))))
    (rlp:rlp-encode (cons signature content))))

(deftest discovery-public-address-policy ()
  (dolist (host '("8.8.8.8" "1.1.1.1" "99.33.22.55" "172.32.1.1" "2001:4860:4860::8888"
                  "2606:4700:4700::1111"))
    (is (d:public-ip-p host) "~A" host))
  (dolist (host '("0.1.2.3" "10.0.0.1" "100.64.0.1" "100.127.255.254" "127.0.0.1"
                  "169.254.1.1" "172.16.0.1" "172.31.255.254" "192.168.1.1" "192.0.0.1"
                  "192.0.2.2" "192.88.99.1" "198.18.0.1" "198.19.0.1" "198.51.100.1"
                  "203.0.113.1" "224.0.0.1" "240.0.0.1" "255.255.255.255" "localhost"
                  "::" "::1" "::ffff:127.0.0.1" "fc00::1" "fe80::1" "ff02::1"
                  "2001::1" "2001:db8::1" "2002:7f00:1::1" "3fff::1" "8.8.8.8%en0"))
    (is (not (d:public-ip-p host)) "~A" host))
  (is (d:public-ip-p (octets-of 8 8 8 8)))
  (is (not (d:public-ip-p (octets-of 8 8 8))))
  (is (not (d:public-ip-p nil))))

(deftest discovery-enode-endpoints ()
  (let* ((public (ec:private->public (sk 1)))
         (uri (format nil "enode://~A@8.8.8.8:30303?discport=30301" (octets->hex public :prefix nil)))
         (peer (d:parse-enode uri)))
    (is-equalp "8.8.8.8" (d:peer-host peer))
    (is= 30303 (d:peer-tcp-port peer)) (is= 30301 (d:peer-udp-port peer))
    (is (octets= public (d:peer-public-key peer)))
    (is (not (d:peer-verified-p peer)))
    (is= 30303 (d:peer-udp-port (d:parse-enode (format nil "enode://~A@8.8.8.8:30303" (octets->hex public :prefix nil)))))
    (let ((peer6 (d:parse-enode (format nil "enode://~A@[2001:4860::8888]:1" (octets->hex public :prefix nil)))))
      (is (d:public-ip-p (d:peer-host peer6))) (is= 1 (d:peer-udp-port peer6)))
    (dolist (suffix '("127.0.0.1:30303" "10.1.2.3:30303" "localhost:30303"
                      "8.8.8.8:0" "8.8.8.8:65536" "8.8.8.8:-1" "8.8.8.8:30303#x"
                      "8.8.8.8:30303?discport=0" "8.8.8.8:30303?foo=1"
                      "8.8.8.8:30303?discport=1&foo=2" "[::1]:30303"))
      (signals p2p-error (d:parse-enode (format nil "enode://~A@~A" (octets->hex public :prefix nil) suffix))))
    (signals p2p-error (d:make-peer :host "8.8.8.8" :udp-port 1 :tcp-port 1 :public-key (make-octets 64)))
    (signals p2p-error (d:parse-enode "enode://bad"))
    (signals p2p-error (d:parse-enode (format nil "enode://~A@8.8.8.8:1" (make-string 128 :initial-element #\z))))))

(deftest discovery-v4-eip8-pings ()
  (let ((public (ec:private->public (bytes "b71c71a67e1177ad4e901695e1b4b9ee17ae16c6668d313eac2f96dbcda3f291"))))
    (dolist (vector
             (list
              (bytes "e9614ccfd9fc3e74360018522d30e1419a143407ffcce748de3e22116b7e8dc92ff74788c0b6663a"
                     "aa3d67d641936511c8f8d6ad8698b820a7cf9e1be7155e9a241f556658c55428ec0563514365799a"
                     "4be2be5a685a80971ddcfa80cb422cdd0101ec04cb847f000001820cfa8215a8d790000000000000"
                     "000000000000000000018208ae820d058443b9a3550102")
              (bytes "577be4349c4dd26768081f58de4c6f375a7a22f3f7adda654d1428637412c3d7fe917cadc56d4e5e"
                     "7ffae1dbe3efffb9849feb71b262de37977e7c7a44e677295680e9e38ab26bee2fcbae207fba3ff3"
                     "d74069a50b902a82c9903ed37cc993c50001f83e82022bd79020010db83c4d001500000000abcdef"
                     "12820cfa8215a8d79020010db885a308d313198a2e037073488208ae82823a8443b9a355c5010203"
                     "040531b9019afde696e582a78fa8d95ea13ce3297d4afb8ba6433e4154caa5ac6431af1b80ba7602"
                     "3fa4090c408f6b4bc3701562c031041d4702971d102c9ab7fa5eed4cd6bab8f7af956f7d565ee191"
                     "7084a95398b6a21eac920fe3dd1345ec0a7ef39367ee69ddf092cbfe5b93e5e568ebc491983c09c7"
                     "6d922dc3")))
      (let ((packet (d::decode-v4 vector :now 0)))
        (is= 1 (d::v4-packet-type packet))
        (is (octets= public (d::v4-packet-sender packet)))
        (is= 3322 (second (second (d::v4-packet-fields packet)))))
      (signals p2p-error (d::decode-v4 vector :now #xffffffff)))))

(deftest discovery-v4-typed-and-authenticated-codec ()
  (let* ((key (sk 1)) (endpoint (list (octets-of 8 8 8 8) 30303 30303))
         (target (ec:private->public (sk 2))) (token (make-octets 32))
         (messages (list (cons 1 (list 555 endpoint endpoint 2000 '(1 (2 (3)))))
                         (cons 2 (list endpoint token 2000 '(1 (2 (3)))))
                         (cons 3 (list target 2000 '(1 (2 (3)))))
                         (cons 4 (list (list (append endpoint (list target))) 2000 '(1 (2 (3))))))))
    (dolist (message messages)
      (let* ((wire (d::encode-v4 (car message) (cdr message) key (octets-of 99 98 97)))
             (packet (d::decode-v4 wire :now 1000)))
        (is= (car message) (d::v4-packet-type packet))
        (is (octets= (ec:private->public key) (d::v4-packet-sender packet)))
        (let ((bad (copy-seq wire)))
          (setf (aref bad (1- (length bad))) (logxor 1 (aref bad (1- (length bad)))))
          (signals p2p-error (d::decode-v4 bad :now 1000)))
        (let ((bad (copy-seq wire)))
          (fill bad 0 :start 32 :end 64)
          (replace bad (k:keccak256 bad :start 32))
          (signals p2p-error (d::decode-v4 bad :now 1000)))))
    (dolist (wire (list (make-octets 98) (make-octets 1281)
                       (d::encode-v4 1 (list 4 endpoint endpoint 999) key)
                       (d::encode-v4 2 (list endpoint (make-octets 31) 2000) key)
                       (d::encode-v4 3 (list (make-octets 64) 2000) key)
                       (d::encode-v4 1 (list 4 (list (make-octets 5) 1 1) endpoint 2000) key)
                       (d::encode-v4 1 (list 4 (list (octets-of 8 8 8 8) 65536 1) endpoint 2000) key)
                       (d::encode-v4 5 (list 2000) key)))
      (signals p2p-error (d::decode-v4 wire :now 1000)))
    (let* ((wire (d::encode-v4 4 (list (list (list (octets-of 127 0 0 1) 1 1 target)
                                               (list (octets-of 8 8 8 8) 1 1 target)) 2000) key))
           (nodes (remove nil (first (d::v4-packet-fields (d::decode-v4 wire :now 1000))))))
      (is= 1 (length nodes)) (is-equalp "8.8.8.8" (d:peer-host (first nodes))))))

(deftest discovery-v4-bond-before-findnode ()
  (let* ((clempo.io:*clock* (lambda () 1000000))
         (remote (test-peer)) (queue nil) (sent nil) (incoming nil)
         (local (list (octets-of 1 1 1 1) 20000 0)) (referral (test-peer 3 "9.9.9.9")))
    (flet ((send (wire)
             (let ((packet (d::decode-v4 wire)))
               (push (d::v4-packet-type packet) sent)
               (case (d::v4-packet-type packet)
                 (1 (setf queue
                          (list (d::encode-v4 1 (list 4 (list (octets-of 8 8 8 8) 30303 30303) local 1020) (sk 2))
                                (d::encode-v4 2 (list local (subseq wire 0 32) 1020) (sk 2)))))
                 (2 (setf incoming t))
                 (3 (is incoming)
                    (setf queue (list (d::encode-v4 4
                                                  (list (list (list (octets-of 9 9 9 9) 30303 30303
                                                                    (d:peer-public-key referral))) 1020) (sk 2))))))))
           (receive (until) (declare (ignore until)) (pop queue)))
      (multiple-value-bind (proved peers)
          (d::query-v4 remote (sk 1) (first local) (second local) #'send #'receive
                       (+ (d::monotonic-seconds) 10) (ec:private->public (sk 1)))
        (is proved) (is-equalp '(1 2 3) (reverse sent)) (is= 1 (length peers))
        (is (octets= (d:peer-public-key referral) (d:peer-public-key (first peers))))))))

(deftest discovery-v4-rejects-unsolicited-and-wrong-identity ()
  (let* ((clempo.io:*clock* (lambda () 1000000)) (queue nil) (sent 0)
         (local (list (octets-of 1 1 1 1) 20000 0)))
    (flet ((send (wire)
             (incf sent)
             (setf queue (list (d::encode-v4 2 (list local (subseq wire 0 32) 1020) (sk 3))
                               (d::encode-v4 2 (list local (make-octets 32) 1020) (sk 2))
                               (d::encode-v4 4 (list nil 1020) (sk 2)))))
           (receive (until) (declare (ignore until)) (pop queue)))
      (multiple-value-bind (proved peers)
          (d::query-v4 (test-peer) (sk 1) (first local) (second local) #'send #'receive
                       (+ (d::monotonic-seconds) 10) (ec:private->public (sk 1)))
        (is (null proved)) (is (null peers)) (is= 1 sent)))))

(deftest discovery-v4-bounded-malformed-input ()
  (let ((received 0) (sent 0))
    (flet ((send (wire) (declare (ignore wire)) (incf sent))
           (receive (until) (declare (ignore until)) (incf received) (octets-of 0)))
      (is (null (d::query-v4 (test-peer) (sk 1) (octets-of 1 1 1 1) 20000 #'send #'receive
                            (+ (d::monotonic-seconds) 10) (ec:private->public (sk 1)))))
      (is= 64 received) (is= 1 sent))))

(deftest discovery-v5-cryptographic-standard-vectors ()
  (let* ((secret (bytes "fb757dc581730490a1d7a00deea65e9b1936924caaea8f44d476014856b68736"))
         (public (bytes "039961e4c2356d61bedb83052c115d311acb3a96f5777296dcf297351130266231"))
         (a (bytes "aaaa8419e9f49d0083561b48287df592939a8d19947d8c0ef88f2a4856a69fbb"))
         (b (bytes "bbbb9d047f0488c0b5a93c1c3f2d8bafc7c8ff337024a55434a0d0555de64db9"))
         (challenge (bytes "000000000000000000000000000000006469736376350001010102030405060708090a0b0c0018"
                           "0102030405060708090a0b0c0d0e0f100000000000000000")))
    (is (octets= (bytes "033b11a2a1f214567e1537ce5e509ffd9b21373247f2a3ff6841f4976f53165e7e")
                 (d::compressed-ecdh secret (d::decompress-public public))))
    (multiple-value-bind (write read)
        (d::v5-keys secret (d::decompress-public
                            (bytes "0317931e6e0840220642f230037d285d122bc59063221ef3226b1f403ddc69ca91"))
                    challenge a b)
      (is (octets= (bytes "dccc82d81bd610f4f76d3ebe97a40571") write))
      (is (octets= (bytes "ac74bb8773749920b0d3a8881c173ec5") read)))
    (is (octets= (bytes "94852a1e2318c4e5e9d422c98eaf19d1d90d876b29cd06ca7cb7546d0fff7b484"
                        "fe86c09a064fe72bdbef73ba8e9c34df0cd2b53e9d65528c2c7f336d5dfc6e6")
                 (d::v5-id-signature secret challenge public b))))
  (let* ((key (bytes "9f2d77db7004bf8a1a85107ac686990b"))
         (nonce (bytes "27b5af763c446acd2749fe8e")) (plain (bytes "01c20101"))
         (ad (bytes "93a7400fa0d6a694ebc24d5cf570f65d04215b6ac00757875e3f3a5f42107903"))
         (wire (bytes "a5d12a2d94b8ccb3ba55558229867dc13bfa3648")))
    (is (octets= wire (d::gcm-encrypt key nonce plain ad)))
    (is (octets= plain (d::gcm-decrypt key nonce wire ad)))
    (setf (aref wire 0) (logxor 1 (aref wire 0)))
    (signals p2p-error (d::gcm-decrypt key nonce wire ad))))

(deftest discovery-v5-standard-packet-vectors ()
  (let* ((destination (bytes "bbbb9d047f0488c0b5a93c1c3f2d8bafc7c8ff337024a55434a0d0555de64db9"))
         (source (bytes "aaaa8419e9f49d0083561b48287df592939a8d19947d8c0ef88f2a4856a69fbb"))
         (wire (bytes "00000000000000000000000000000000088b3d4342774649325f313964a39e55"
                      "ea96c005ad52be8c7560413a7008f16c9e6d2f43bbea8814a546b7409ce783d3"
                      "4c4f53245d08dab84102ed931f66d1492acb308fa1c6715b9d139b81acbdcc"))
         (packet (d::decode-v5 wire destination))
         (plain (d::gcm-decrypt (make-octets 16) (d::v5-packet-nonce packet)
                                (d::v5-packet-message packet) (d::v5-packet-associated-data packet))))
    (is= 0 (d::v5-packet-flag packet)) (is (octets= source (d::v5-packet-authdata packet)))
    (is (octets= (bytes "01c6840000000102") plain))
    (is (octets= wire (d::encode-v5 destination 0 (fill (make-octets 12) 255) source
                                   :key (make-octets 16) :plain plain :iv (make-octets 16))))
    (let* ((challenge-wire
             (bytes "00000000000000000000000000000000088b3d434277464933a1ccc59f5967ad"
                    "1d6035f15e528627dde75cd68292f9e6c27d6b66c8100a873fcbaed4e16b8d"))
           (challenge (d::decode-v5 challenge-wire destination)))
      (is= 1 (d::v5-packet-flag challenge))
      (is (octets= (bytes "0102030405060708090a0b0c") (d::v5-packet-nonce challenge)))
      (is (octets= (bytes "000000000000000000000000000000006469736376350001010102030405060708090a0b0c0018"
                          "0102030405060708090a0b0c0d0e0f100000000000000000")
                   (d::v5-packet-associated-data challenge))))
    (signals p2p-error (d::decode-v5 wire (make-octets 32)))
    (signals p2p-error (d::decode-v5 (make-octets 62) destination))
    (signals p2p-error (d::decode-v5 (make-octets 1281) destination))))

(deftest discovery-enr-standard-and-malformed-records ()
  (let* ((record (rlp:rlp-encode
                  (list (bytes "7098ad865b00a582051940cb9cf36836572411a47278783077011599ed5cd16b"
                               "76f2635f4e234738f30813a89eb9137e3e3df5266e3a1f11df72ecf1145ccb9c")
                        1 "id" "v4" "ip" (bytes "7f000001") "secp256k1"
                        (bytes "03ca634cae0d49acb401d8a4c6b6fe8c55b70d115bf400769cc1400f3258cd3138")
                        "udp" 30303))))
    (multiple-value-bind (public sequence peer) (d::decode-enr (d::item-at record 0 (length record)))
      (is= 1 sequence) (is (null peer)) ; authenticated localhost is not public
      (is (octets= (bytes "a448f24c6d18e575453db13171562b71999873db5b286df957af199ec94617f7")
                   (k:keccak256 public))))
    (setf (aref record 10) (logxor 1 (aref record 10)))
    (signals p2p-error (d::decode-enr (d::item-at record 0 (length record)))))
  (let* ((record (test-enr 3)))
    (multiple-value-bind (public sequence peer) (d::decode-enr (d::item-at record 0 (length record)))
      (is (octets= public (ec:private->public (sk 3)))) (is= 1 sequence)
      (is-equalp "9.9.9.9" (d:peer-host peer)) (is (not (d:peer-verified-p peer))))
    (dolist (fields (list (list (make-octets 64) 1 "id" "v4" "id" "v4")
                          (list (make-octets 64) 1 "secp256k1" (make-octets 33) "id" "v4")
                          (list (make-octets 64) 1 "id" "v5")
                          (list (make-octets 64) 1 "id")
                          (list (make-octets 64) 1 "id" (make-octets 301))))
      (let ((wire (rlp:rlp-encode fields)))
        (signals p2p-error (d::decode-enr (d::item-at wire 0 (length wire))))))))

(deftest discovery-v5-client-handshake-and-findnode ()
  (let* ((clempo.io:*entropy* (clempo.io:make-seeded-entropy 4217))
         (self (k:keccak256 (ec:private->public (sk 1)))) (remote (k:keccak256 (ec:private->public (sk 2))))
         (queue nil) (challenge-data nil) (write-key nil) (read-key nil)
         (response-counter 0) (sent 0) (pong-wire nil) (candidate nil))
    (labels ((reply (type fields)
               (d::encode-v5 self 0 (integer->octets (incf response-counter) 12) remote
                             :key read-key :plain (d::v5-message type fields)))
             (send (wire)
               (incf sent)
               (let ((packet (d::decode-v5 wire remote)))
                 (case (d::v5-packet-flag packet)
                   (0 (if (null write-key)
                          (let* ((auth (octets-concat (make-octets 16) (make-octets 8)))
                                 (challenge (d::encode-v5 self 1 (d::v5-packet-nonce packet) auth)))
                            (setf challenge-data (d::v5-packet-associated-data (d::decode-v5 challenge self))
                                  queue (list (d::encode-v5 self 1 (make-octets 12) auth) challenge)))
                          (let* ((plain (d::gcm-decrypt write-key (d::v5-packet-nonce packet)
                                                       (d::v5-packet-message packet) (d::v5-packet-associated-data packet)))
                                 (fields (d::v5-message-fields plain 2))
                                 (distances (mapcar (lambda (x) (d::item-integer x 2))
                                                   (d::item-children (second fields) 3))))
                            (is= 3 (aref plain 0))
                            (setf candidate
                                  (loop for key from 3 to 100
                                        when (member (d::log-distance remote (k:keccak256 (ec:private->public (sk key)))) distances)
                                          return key))
                            (is candidate)
                            (setf queue (list pong-wire
                                              (reply 4 (list (d::item-octets (first fields)) 1
                                                             (list (rlp:make-rlp-raw (test-enr candidate))))))))))
                   (2 (let* ((auth (d::v5-packet-authdata packet))
                             (signature (subseq auth 34 98)) (ephemeral (subseq auth 98 131)))
                        (is (d::signature-matches-p
                             (clempo.sha2:sha256
                              (octets-concat (string->octets "discovery v5 identity proof")
                                             challenge-data ephemeral remote)) signature (ec:private->public (sk 1))))
                        (multiple-value-bind (public seq peer)
                            (d::decode-enr (d::item-at auth 131 (length auth)))
                          (declare (ignore seq peer)) (is (octets= public (ec:private->public (sk 1)))))
                        (multiple-value-setq (write-key read-key)
                          (d::v5-keys (sk 2) (d::decompress-public ephemeral) challenge-data self remote))
                        (let* ((plain (d::gcm-decrypt write-key (d::v5-packet-nonce packet)
                                                     (d::v5-packet-message packet) (d::v5-packet-associated-data packet)))
                               (fields (d::v5-message-fields plain 2)))
                          (is= 1 (aref plain 0))
                          (setf pong-wire (reply 2 (list (d::item-octets (first fields)) 1 (octets-of 1 1 1 1) 20000)))
                          (let ((bad (copy-seq pong-wire)))
                            (setf (aref bad (1- (length bad))) (logxor 1 (aref bad (1- (length bad)))))
                            (setf queue (list bad pong-wire)))))))))
             (receive (until) (declare (ignore until)) (pop queue)))
      (multiple-value-bind (proved peers)
          (d::query-v5 (test-peer) (sk 1) #'send #'receive (+ (d::monotonic-seconds) 10) (ec:private->public (sk 1)))
        (is proved) (is= 3 sent) (is= 1 (length peers))
        (is (octets= (ec:private->public (sk candidate)) (d:peer-public-key (first peers))))))))

(deftest discovery-no-network-zero-budget-and-option-bounds ()
  (is (null (d:discover-peers (list (test-peer)) :timeout 0)))
  (is (null (d:discover-peers nil :timeout 1 :key (sk 1))))
  (is (null (d:discover-peers nil :timeout 1 :key (sk 1) :protocols '(:v5))))
  (is (null (d:discover-peers nil :timeout 1 :key (sk 1) :protocols '(:v5 :v4))))
  (signals p2p-error (d:discover-peers nil :timeout -1))
  (signals p2p-error (d:discover-peers nil :timeout 301))
  (signals p2p-error (d:discover-peers nil :max-queries 0))
  (signals p2p-error (d:discover-peers nil :max-peers 257))
  (signals p2p-error (d:discover-peers nil :protocols nil))
  (signals p2p-error (d:discover-peers nil :protocols '(:v4 :v4)))
  (signals p2p-error (d:discover-peers nil :protocols '(:v6)))
  (signals p2p-error (d:discover-peers nil :key 1))
  (signals p2p-error (d:discover-peers nil :key (make-octets 32))))
