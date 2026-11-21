(defpackage #:clempo.validator
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:cw #:clempo.cwp2p) (#:sock #:sb-bsd-sockets) (#:thr #:sb-thread)
                    (#:json #:clempo.json) (#:io #:clempo.io))
  (:export #:read-devnet-manifest #:devnet-manifest #:manifest-genesis #:manifest-epoch-length
           #:manifest-slots #:validator-slot #:slot-number #:slot-public-key #:slot-seed-path
           #:slot-share-path #:slot-port #:slot-rpc-url #:manifest-slot #:read-slot-seed
           #:call-with-validator-network #:network-receive #:network-send #:network-peers
           #:network-event #:network-event-kind #:network-event-peer #:network-event-channel
           #:run-devnet-validator
           #:network-event-frame #:network-event-start #:network-event-end))

(in-package #:clempo.validator)

(defconstant +max-devnet-validators+ 32)

(defstruct (validator-slot (:conc-name slot-) (:constructor %make-slot))
  (number 0 :type fixnum :read-only t)
  (public-key (make-octets 32) :type (octets 32) :read-only t)
  (port 0 :type (unsigned-byte 16) :read-only t)
  (seed-path nil :read-only t)
  (share-path nil :read-only t)
  (rpc-url nil :read-only t))

(defstruct (devnet-manifest (:conc-name manifest-) (:constructor %make-manifest))
  (genesis nil :read-only t)
  (epoch-length 0 :type u64 :read-only t)
  (slots #() :type simple-vector :read-only t))

(defun require-field (object key type)
  (let ((value (json:jget object key)))
    (unless (typep value type) (error "Invalid manifest field ~S: expected ~S" key type))
    value))

(defun named-file (object key)
  (let ((path (pathname (require-field object key 'string))))
    (unless (uiop:absolute-pathname-p path) (error "Manifest path for ~A must be absolute" key))
    path))

(defun loopback-port (address prefix)
  (unless (and (< (length prefix) (length address))
               (string= prefix address :end2 (length prefix))
               (every (lambda (c) (find c "0123456789")) (subseq address (length prefix))))
    (error "Expected private loopback endpoint in manifest: ~S" address))
  (let ((port (parse-integer address :start (length prefix))))
    (unless (<= 1024 port 65535) (error "Invalid devnet port ~D" port))
    port))

(defun decode-slot (record count)
  (let* ((key (require-field record "public_key" 'string))
         (public (hex->octets key))
         (rpc (require-field record "rpc_url" 'string)))
    (unless (and (member (length key) '(64 66)) (= (length public) 32))
      (error "Invalid Ed25519 public key in devnet manifest"))
    (loopback-port rpc "http://127.0.0.1:")
    (%make-slot :number (require-field record "slot" `(integer 0 ,(1- count)))
                :public-key public
                :port (loopback-port (require-field record "consensus_address" 'string) "127.0.0.1:")
                :seed-path (named-file record "signing_seed")
                :share-path (named-file record "signing_share")
                :rpc-url rpc)))

(defun read-devnet-manifest (path)
  "Read the harness manifest. Do not connect to any non-loopback address."
  (let* ((document (json:parse-json (io:read-file-string path)))
         (records (require-field document "validators" 'simple-vector))
         (n (length records)) (slots (make-array n :initial-element nil)))
    (unless (and (eql (json:jget document "version") 1) (<= 2 n +max-devnet-validators+))
      (error "Invalid devnet manifest version or validator count"))
    (dotimes (i n)
      (let ((slot (decode-slot (aref records i) n)))
        (when (or (aref slots (slot-number slot))
                  (some (lambda (other)
                          (and other (or (= (slot-port slot) (slot-port other))
                                         (octets= (slot-public-key slot) (slot-public-key other))
                                         (string= (slot-rpc-url slot) (slot-rpc-url other))))) slots))
          (error "Duplicate devnet slot, address or public key"))
        (setf (aref slots (slot-number slot)) slot)))
    (%make-manifest :genesis (named-file document "genesis")
                    :epoch-length (require-field document "epoch_length" '(integer 1 #xffffffffffffffff))
                    :slots slots)))

(defun manifest-slot (manifest n)
  (unless (typep n `(integer 0 ,(1- (length (manifest-slots manifest)))))
    (error "Unknown devnet slot ~S" n))
  (aref (manifest-slots manifest) n))

(defun read-slot-seed (slot)
  (let ((seed (io:read-file-octets (slot-seed-path slot))))
    (unless (and (= (length seed) 32)
                 (octets= (clempo.ed25519:ed25519-public-key seed) (slot-public-key slot)))
      (fill seed 0)
      (error "Signing seed does not match public key of slot ~D" (slot-number slot)))
    seed))
