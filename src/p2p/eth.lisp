(defpackage #:clempo.eth
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:rlp #:clempo.rlp) (#:rlpx #:clempo.rlpx) (#:spec #:clempo.chainspec)
                    (#:sock #:sb-bsd-sockets) (#:thr #:sb-thread))
  (:import-from #:clempo.p2p-wire #:protocol-error #:connect-tcp)
  (:export #:make-eth-chain #:tempo-eth-chain #:fork-id
           #:eth-dial #:eth-handshake #:eth-close #:eth-version #:eth-peer-status
           #:call-with-eth-listener
           #:get-block-headers #:get-block-bodies #:get-receipts #:download-blocks
           #:encode-message #:decode-message
           #:rlp-view #:rlp-view-data #:rlp-view-start #:rlp-view-end #:rlp-view-octets
           #:tempo-header-number #:tempo-header-timestamp #:block-header-view
           #:block-matches-p))

(in-package #:clempo.eth)

(defconstant +serve-items-max+ 1024)
(defconstant +serve-bytes-soft-max+ (* 2 1024 1024))
;; Leave room for framing and Snappy overhead below RLPx's 24-bit frame limit.
(defconstant +serve-bytes-hard-max+ (* 13 1024 1024))

(defstruct (rlp-view (:constructor make-view (data start end)))
  "Read-only reference to an encoded item in a retained packet."
  (data nil :type octets :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t))

(defun rlp-view-octets (view)
  (subseq (rlp-view-data view) (rlp-view-start view) (rlp-view-end view)))

(defun encode-field (schema value)
  (flet ((uint (bytes) (unless (typep value `(unsigned-byte ,(* 8 bytes)))
                        (protocol-error "integer exceeds ~D byte field" bytes)) value))
    (cond ((eq schema :u64) (uint 8))
          ((eq schema :u32) (uint 4))
          ((eq schema :u16) (uint 2))
          ((eq schema :u8) (uint 1))
          ((eq schema :u256) (uint 32))
          ((eq schema :bool) (if value 1 0))
          ((eq schema :string) (etypecase value (string value) (octets value)))
          ((eq schema :origin) (if (typep value 'octets) (encode-field '(:bytes 32) value) (uint 8)))
          ((member schema '(:raw :raw-list))
           (rlp:make-rlp-raw (etypecase value (rlp-view (rlp-view-octets value)) (octets value))))
          ((eq (first schema) :bytes)
           (unless (typep value `(octets ,(second schema)))
             (protocol-error "expected ~D byte field" (second schema))) value)
          ((eq (first schema) :vector)
           (map 'list (lambda (x) (encode-field (second schema) x)) value))
          (t (invariant (eq (first schema) :tuple))
             (unless (= (length value) (length (rest schema)))
               (protocol-error "wrong field count"))
             (mapcar #'encode-field (rest schema) value)))))

(defun rlp-uint (data start stop max-bytes)
  (unless (and (<= (- stop start) max-bytes)
               (or (= start stop) (not (zerop (aref data start)))))
    (protocol-error "invalid RLP integer"))
  (octets->integer data :start start :end stop))

(defun decode-field (schema data position end)
  (multiple-value-bind (start stop list-p) (rlp:rlp-item-bounds data position end)
    (labels ((bytes (&optional n)
               (when (or list-p (and n (/= n (- stop start))))
                 (protocol-error "invalid RLP byte field"))
               (subseq data start stop))
             (uint (n)
               (when list-p (protocol-error "expected RLP integer"))
               (rlp-uint data start stop n))
             (item (type)
               (multiple-value-bind (value next) (decode-field type data start stop)
                 (setf start next)
                 value)))
      (values
       (cond ((eq schema :u64) (uint 8))
             ((eq schema :u32) (uint 4))
             ((eq schema :u16) (uint 2))
             ((eq schema :u8) (uint 1))
             ((eq schema :u256) (uint 32))
             ((eq schema :bool) (let ((x (uint 1)))
                                  (unless (<= x 1) (protocol-error "invalid RLP boolean")) (= x 1)))
             ((eq schema :string) (bytes))
             ((eq schema :origin) (if (= (- stop start) 32) (bytes 32) (uint 8)))
             ((member schema '(:raw :raw-list))
              (when (and (eq schema :raw-list) (not list-p)) (protocol-error "expected RLP list"))
              (make-view data position stop))
             ((eq (first schema) :bytes) (bytes (second schema)))
             ((eq (first schema) :vector)
              (unless list-p (protocol-error "expected RLP vector"))
              (let ((limit (third schema)))
                (loop for i from 0 while (< start stop)
                      when (and limit (>= i limit)) do (protocol-error "too many RLP vector items")
                      collect (item (second schema)))))
             (t (invariant (eq (first schema) :tuple))
                (unless list-p (protocol-error "expected RLP message list"))
                (prog1 (mapcar #'item (rest schema))
                  (unless (= start stop) (protocol-error "trailing RLP fields")))))
       stop))))

(defvar *messages* (make-hash-table))

(defmacro defmessage (name code &rest fields)
  `(setf (gethash ,name *messages*) (cons ,code '(:tuple ,@fields))))

;; Absolute codes with only the eth capability negotiated (base protocol occupies 0x00..0x0f).
(defmessage :hello #x00 :u64 :string (:vector (:tuple :string :u8)) :u16 (:bytes 64))
(defmessage :disconnect #x01 :u8)
(defmessage :ping #x02)
(defmessage :pong #x03)
(defmessage :status68 #x10 :u8 :u64 :u256 (:bytes 32) (:bytes 32) (:tuple (:bytes 4) :u64))
(defmessage :status69 #x10 :u8 :u64 (:bytes 32) (:tuple (:bytes 4) :u64) :u64 :u64 (:bytes 32))
(defmessage :get-block-headers #x13 :u64 (:tuple :origin :u64 :u32 :bool))
(defmessage :block-headers #x14 :u64 (:vector :raw-list))
(defmessage :get-block-bodies #x15 :u64 (:vector (:bytes 32) #.+serve-items-max+))
(defmessage :block-bodies #x16 :u64 (:vector :raw-list))
(defmessage :get-receipts #x1f :u64 (:vector (:bytes 32) #.+serve-items-max+))
;; Receipt envelopes are opaque, retaining the wire encoding (69 omits the bloom).
(defmessage :receipts #x20 :u64 (:vector (:vector :raw)))
(defmessage :block-range-update #x21 :u64 :u64 (:bytes 32))

(defun message-spec (name)
  (or (gethash name *messages*) (error "Unknown wire message ~S" name)))

(defun message-code (name) (car (message-spec name)))

(defun encode-message (name &rest fields)
  "Encode the RLP payload of NAME, not including its code."
  (rlp:rlp-encode (encode-field (cdr (message-spec name)) fields)))

(defun decode-message (name data &key (start 0))
  "Decode NAME from a canonical RLP payload. Opaque RLP items are read-only views."
  (unless (<= 0 start (length data)) (protocol-error "invalid RLP message range"))
  (multiple-value-bind (fields end) (decode-field (cdr (message-spec name)) data start (length data))
    (unless (= end (length data)) (protocol-error "trailing bytes in ~S" name))
    fields))

(defun crc32-update (crc data)
  (loop for b across data
        do (setf crc (logxor crc b))
           (dotimes (bit 8)
             (setf crc (logxor (ash crc -1) (if (oddp crc) #xedb88320 0)))))
  crc)

(defstruct (eth-chain (:constructor %make-eth-chain (network genesis schedule)))
  network genesis schedule)

(defun make-eth-chain (network genesis fork-times &key (genesis-time 0))
  "An all-timestamp Tempo chain, omitting duplicate and genesis-time forks."
  (let* ((times (remove-duplicates (sort (remove-if (lambda (x) (<= x genesis-time)) fork-times) #'<)))
         (crc (crc32-update #xffffffff genesis))
         (schedule (loop for prev = 0 then next
                         for next in (append times '(0))
                         collect (list prev (integer->octets (logxor #xffffffff crc) 4) next)
                         unless (zerop next) do (setf crc (crc32-update crc (integer->octets next 8))))))
    (%make-eth-chain network genesis schedule)))

(defvar *tempo-mainnet* nil)

(defun from-chainspec (chain)
  (make-eth-chain (spec:chain-id chain) (spec:genesis-block-hash chain)
                  (loop for fork in clempo.forks:+forks+
                        for timestamp = (spec:fork-activation fork chain)
                        when timestamp collect timestamp)
                  :genesis-time (spec:genesis-timestamp chain)))

(defun tempo-eth-chain (&optional (chain :mainnet))
  "Derive the devp2p profile from the Tempo chainspec. Cache the default mainnet profile."
  (if (eq chain :mainnet)
      (or *tempo-mainnet* (setf *tempo-mainnet* (from-chainspec (spec:load-chainspec chain))))
      (from-chainspec (spec:load-chainspec chain))))

(defun current-fork (chain timestamp)
  (loop for row in (eth-chain-schedule chain)
        for next = (third row)
        when (or (zerop next) (< timestamp next)) return row))

(defun fork-id (chain timestamp)
  "Return the EIP-2124/EIP-6122 (4-byte digest next-timestamp) list."
  (rest (current-fork chain timestamp)))

(defun compatible-fork-p (chain timestamp number peer-fork)
  (destructuring-bind (digest next) peer-fork
    (destructuring-bind (local-time local-digest local-next) (current-fork chain timestamp)
      (declare (ignore local-next))
      (if (octets= local-digest digest)
          (or (zerop next)
              (< (if (if (< number 1300000000) (> next 1300000000) (plusp local-time)) timestamp number)
                 next))
          (loop for (time known next-known) in (eth-chain-schedule chain)
                when (octets= known digest) return (if (< time local-time) (= next next-known) t))))))

(defstruct (eth (:constructor %make-eth (rlpx version source)))
  rlpx version source peer-status
  (counter 0 :type u64))

(defun send (link name fields &key (compress-p t))
  (let ((payload (apply #'encode-message name fields)))
    (rlpx:rlpx-send link (octets-concat (rlp:rlp-encode (message-code name))
                                       (if compress-p (clempo.snappy:compress payload) payload)))))

(defun frame-code (frame end)
  (unless (plusp end) (protocol-error "empty devp2p frame"))
  ;; Hello's code zero is the RLP empty string (0x80), not the byte 0.
  (let ((code (aref frame 0)))
    (cond ((= code #x80) 0)
          ((<= 1 code #x7f) code)
          (t (protocol-error "invalid devp2p message code #x~X" code)))))

(defun disconnect-reason (data start &optional (end (length data)))
  ;; The spec encodes a one-element list; early clients also sent the bare integer.
  (multiple-value-bind (a b list-p) (rlp:rlp-item-bounds data start end)
    (unless (= b end) (protocol-error "trailing devp2p disconnect bytes"))
    (if list-p
        (first (decode-field '(:tuple :u8) data start end))
        (rlp-uint data a b 1))))

(defun receive (link &key (compressed-p t))
  (multiple-value-bind (frame end) (rlpx:rlpx-receive link :with-padding compressed-p)
    (let ((code (frame-code frame end)))
      (when (= code 1)
        (protocol-error "peer disconnected (reason ~D)"
                        (if compressed-p
                            (or (and (<= end 3) (ignore-errors (disconnect-reason frame 1 end)))
                                (disconnect-reason (clempo.snappy:decompress frame :start 1 :end end :max-size 2) 0))
                            (disconnect-reason frame 1 end))))
      (if compressed-p
          (values code (clempo.snappy:decompress frame :start 1 :end end :max-size (* 16 1024 1024)) 0)
          (values code frame 1)))))

(defun request-kind (code)
  (case code
    (#x13 (values :get-block-headers :block-headers))
    (#x15 (values :get-block-bodies :block-bodies))
    (#x1f (values :get-receipts :receipts))))

(defun block-body-bytes (raw header)
  (let* ((start (rlp-view-end header))
         (size (- (length raw) start))
         (out (make-octets (+ size (rlp:header-length size)))))
    (replace out raw :start1 (rlp:write-header out 0 size #xc0) :start2 start)))

(defun receipt-on-wire (raw version)
  ;; EIP-2718 typed envelopes are strings in eth/68. EIP-7642 uses a flat
  ;; RLP list for every receipt in eth/69, regardless of transaction type.
  (when (zerop (length raw)) (protocol-error "empty stored receipt"))
  (let* ((typed (< (aref raw 0) #x80)) (start (if typed 1 0)))
    (if (= version 68)
        (if typed (rlp:rlp-encode raw) raw)
        (multiple-value-bind (first end list-p) (rlp:rlp-item-bounds raw start (length raw))
          (unless (and list-p (= end (length raw))) (protocol-error "invalid stored receipt"))
          (let* ((status-end (nth-value 1 (rlp:rlp-item-bounds raw first end)))
                 (gas-end (nth-value 1 (rlp:rlp-item-bounds raw status-end end)))
                 (logs-start (nth-value 1 (rlp:rlp-item-bounds raw gas-end end)))
                 (size (+ 1 (- gas-end first) (- end logs-start)))
                 (out (make-octets (+ size (rlp:header-length size))))
                 (pos (rlp:write-header out 0 size #xc0)))
            (setf (aref out pos) (if typed (aref raw 0) #x80))
            (replace out raw :start1 (1+ pos) :start2 first :end2 gas-end)
            (replace out raw :start1 (+ 1 pos (- gas-end first)) :start2 logs-start))))))

(defun block-receipts-on-wire (source height version)
  (multiple-value-bind (raw known) (funcall source :receipts height)
    (when known
      (loop with size = 0 with items = nil
            for receipt in raw
            for wire = (receipt-on-wire receipt version)
            do (incf size (length wire))
               (when (> size +serve-bytes-hard-max+)
                 (return-from block-receipts-on-wire (values nil nil)))
               (push wire items)
            finally (return (values (nreverse items) (+ size (rlp:header-length size))))))))

(defun serve-query (source version request query)
  (let ((items nil) (size 0))
    (labels ((append-item (item bytes)
               (when (> (+ size bytes) +serve-bytes-hard-max+)
                 (return-from serve-query (nreverse items)))
               (push item items)
               (incf size bytes))
             (hash-header (raw hash)
               (let ((header (block-header-view raw)))
                 (when (octets= hash (clempo.keccak:keccak256 raw :start (rlp-view-start header)
                                                                  :end (rlp-view-end header)))
                   header))))
      (ecase request
        (:get-block-headers
         (destructuring-bind (origin count skip reverse) query
           (loop repeat (min count +serve-items-max+)
                 with hash = (when (typep origin 'octets) origin)
                 for height = (if hash (funcall source :number hash) origin)
                   then (+ height (if reverse (- (1+ skip)) (1+ skip)))
                 while (and (typep height 'u64) (<= size +serve-bytes-soft-max+))
                 do (let* ((raw (funcall source :block height))
                           (header (when raw (if hash (hash-header raw hash) (block-header-view raw)))))
                      (unless header (return))
                      (append-item (rlp-view-octets header) (- (rlp-view-end header) (rlp-view-start header)))
                      (setf hash nil)))))
        ((:get-block-bodies :get-receipts)
         (loop for hash in query repeat +serve-items-max+
               while (<= size +serve-bytes-soft-max+)
               do (let* ((height (funcall source :number hash))
                         (raw (when height (funcall source :block height)))
                         (header (when raw (hash-header raw hash))))
                    (unless header (return))
                    (if (eq request :get-block-bodies)
                        (let* ((payload (- (length raw) (rlp-view-end header)))
                               (bytes (+ payload (rlp:header-length payload))))
                          (when (> (+ size bytes) +serve-bytes-hard-max+) (return))
                          (append-item (block-body-bytes raw header) bytes))
                        (multiple-value-bind (receipts bytes) (block-receipts-on-wire source height version)
                          (unless bytes (return))
                          (append-item receipts bytes))))))))
    (nreverse items)))

(defun service-message (peer code payload)
  (let ((link (eth-rlpx peer)))
    (case code
      (#x02 (decode-message :ping payload) (send link :pong nil) t)
      (#x03 (decode-message :pong payload) t)
      ((#x13 #x15 #x1f)
       (multiple-value-bind (request response) (request-kind code)
         (destructuring-bind (id query) (decode-message request payload)
           (let ((source (eth-source peer)))
             (send link response (list id (when source (serve-query source (eth-version peer) request query)))))))
       t))))

(defun next-message (peer)
  (loop repeat 1024 do
    (multiple-value-bind (code payload) (receive (eth-rlpx peer))
      (unless (service-message peer code payload)
        (return-from next-message (values code payload)))))
  (protocol-error "too many devp2p control messages before response"))

(defun exchange-hello (link versions listen-port)
  (send link :hello (list 5 "clempo/0.1" (mapcar (lambda (v) (list "eth" v)) versions)
                          listen-port (rlpx:rlpx-public-key link)) :compress-p nil)
  (multiple-value-bind (code data start) (receive link :compressed-p nil)
    (unless (= code 0) (protocol-error "expected devp2p Hello, got #x~X" code))
    (destructuring-bind (p2p-version name caps port peer-id) (decode-message :hello data :start start)
      (declare (ignore name port))
      (unless (= p2p-version 5) (protocol-error "unsupported devp2p version ~D" p2p-version))
      (unless (octets= peer-id (rlpx:rlpx-peer link)) (protocol-error "Hello node ID does not match RLPx peer"))
      (or (loop for v in versions
                when (find-if (lambda (c) (and (= v (second c))
                                                (octets= (first c) (load-time-value (string->octets "eth"))))) caps)
                  maximize v into largest
                finally (return (unless (zerop largest) largest)))
          (protocol-error "no common eth/68 or eth/69 capability")))))

(defun eth-handshake (link &key (chain (tempo-eth-chain)) (versions '(69 68))
                                (timestamp 0) (earliest 0) (latest 0)
                                (head (eth-chain-genesis chain)) (listen-port 0) source)
  "Exchange Hello and Status on authenticated RLPx LINK."
  (check-type versions cons)
  (unless (and (every (lambda (v) (member v '(68 69))) versions) (<= 0 earliest latest))
    (error "Invalid ETH capabilities or advertised range"))
  (let* ((version (exchange-hello link versions listen-port))
         (status (if (= version 68) :status68 :status69))
         (genesis (eth-chain-genesis chain)) (network (eth-chain-network chain))
         (fork (fork-id chain timestamp)) (peer (%make-eth link version source)))
    (send link status (if (= version 68) (list version network 0 head genesis fork)
                          (list version network genesis fork earliest latest head)))
    (loop repeat 8 do
      (multiple-value-bind (code payload) (receive link)
        ;; Base protocol control messages may race the initial ETH Status.
        (case code
          ((#x02 #x03) (service-message peer code payload))
          (#x10
           (let* ((fields (decode-message status payload))
                  (peer-genesis (nth (if (= version 68) 4 2) fields))
                  (peer-fork (nth (if (= version 68) 5 3) fields)))
             (unless (and (= (first fields) version) (= (second fields) network) (octets= genesis peer-genesis))
               (protocol-error "wrong eth version, network or genesis"))
             (unless (compatible-fork-p chain timestamp latest peer-fork)
               (protocol-error "incompatible Tempo fork ID ~A/~D" (octets->hex (first peer-fork)) (second peer-fork)))
             (when (and (= version 69) (> (fifth fields) (sixth fields)))
               (protocol-error "peer advertised invalid block range"))
             (setf (eth-peer-status peer) fields)
             (return-from eth-handshake peer)))
          (otherwise (protocol-error "expected eth status, got #x~X" code)))))
    (protocol-error "too many base messages before eth status")))

(defun eth-dial (host port public-key &rest options &key (timeout 10) key &allow-other-keys)
  "Open an owned TCP/RLPx/ETH connection. TIMEOUT bounds TCP socket waits; KEY
is the optional local secp256k1 private key. Other options go to ETH-HANDSHAKE."
  (let ((stream (connect-tcp host port :timeout timeout)) (peer nil))
    (unwind-protect
         (let ((link (if key (rlpx:rlpx-dial stream public-key :key key) (rlpx:rlpx-dial stream public-key))))
           (setf peer (apply #'eth-handshake link
                             (loop for (name value) on options by #'cddr
                                   unless (member name '(:timeout :key)) append (list name value)))))
      (unless peer (close stream :abort t)))))

(defun eth-close (peer)
  (unwind-protect
       (handler-case (send (eth-rlpx peer) :disconnect '(8))
         ((or error sb-ext:timeout) () nil)) ; client quitting
    (rlpx:rlpx-close (eth-rlpx peer))))

(defun query-peer (peer request response argument)
  (let ((id (incf (eth-counter peer))) (expected (message-code response)))
    (send (eth-rlpx peer) request (list id argument))
    (loop repeat 1024 do
      (multiple-value-bind (code payload) (next-message peer)
        (cond ((= code expected)
               (destructuring-bind (received data) (decode-message response payload)
                 (unless (= received id) (protocol-error "unexpected eth request ID ~D" received))
                 (return-from query-peer data)))
              ((and (= code #x21) (= (eth-version peer) 69))
               (decode-message :block-range-update payload))
              ;; Unsolicited block/transaction announcements on a downloading connection.
              ((member code '(#x11 #x12 #x17 #x18)) nil)
              (t (protocol-error "unexpected eth message #x~X while waiting for #x~X" code expected)))))
    (protocol-error "too many unsolicited eth messages before response")))

(defun get-block-headers (peer origin limit &key (skip 0) reverse)
  "Request header views, starting from a block number or hash."
  (let ((headers (query-peer peer :get-block-headers :block-headers (list origin limit skip reverse))))
    (when (> (length headers) limit) (protocol-error "peer returned too many headers"))
    headers))

(defun get-block-bodies (peer hashes)
  "Request block-body views for HASHES, which may be a list or vector."
  (let ((bodies (query-peer peer :get-block-bodies :block-bodies hashes)))
    (when (> (length bodies) (length hashes)) (protocol-error "peer returned too many block bodies"))
    bodies))

(defun get-receipts (peer hashes)
  "Request per-block lists of RLP receipt envelopes (eth/69 omits the bloom)."
  (let ((receipts (query-peer peer :get-receipts :receipts hashes)))
    (when (> (length receipts) (length hashes)) (protocol-error "peer returned too many receipt lists"))
    receipts))

(defun list-item-range (data position end skip)
  (multiple-value-bind (start stop list-p) (rlp:rlp-item-bounds data position end)
    (unless list-p (protocol-error "expected RLP list"))
    (dotimes (i skip)
      (setf start (nth-value 1 (rlp:rlp-item-bounds data start stop))))
    (values start stop)))

(defun tempo-header-field (header index)
  (let ((data (rlp-view-data header)))
    (multiple-value-bind (inner outer-end)
        (list-item-range data (rlp-view-start header) (rlp-view-end header) 3)
      (multiple-value-bind (pos end) (list-item-range data inner outer-end index)
        (multiple-value-bind (start stop list-p) (rlp:rlp-item-bounds data pos end)
          (when list-p (protocol-error "expected integer in Tempo header"))
          (rlp-uint data start stop 8))))))

(defun tempo-header-number (header) (tempo-header-field header 8))
(defun tempo-header-timestamp (header) (tempo-header-field header 11))

(defun block-header-view (block)
  (multiple-value-bind (start end) (list-item-range block 0 (length block) 0)
    (unless (= end (length block)) (protocol-error "trailing raw block bytes"))
    (multiple-value-bind (payload stop list-p) (rlp:rlp-item-bounds block start end)
      (declare (ignore payload))
      (unless list-p (protocol-error "expected header in raw block"))
      (make-view block start stop))))

(defun equal-range-p (a start-a end-a b start-b end-b)
  (declare (type octets a b) (type fixnum start-a end-a start-b end-b) #.*hot*)
  (and (= (- end-a start-a) (- end-b start-b))
       (loop for i from start-a below end-a for j from start-b
             always (= (aref a i) (aref b j)))))

(defun block-matches-p (block header body)
  "Check the exact RLP header and body payload against an archive block.
The body is a list on the wire; its members follow the header directly in the archived block."
  (let* ((expected (block-header-view block)) (data (rlp-view-data body)))
    (multiple-value-bind (start end) (list-item-range data (rlp-view-start body) (rlp-view-end body) 0)
      (and (equal-range-p block (rlp-view-start expected) (rlp-view-end expected)
                          (rlp-view-data header) (rlp-view-start header) (rlp-view-end header))
           (equal-range-p block (rlp-view-end expected) (length block) data start end)))))

(defun download-blocks (peer from to consume &key (batch 128))
  "Download the inclusive range FROM..TO. Call CONSUME with (number header-view body-view).
Only a bounded batch is retained; capped responses are continued from their last block."
  (check-type batch (integer 1 1024))
  (check-type from u64)
  (check-type to u64)
  (loop with number = from while (<= number to) do
    (let ((headers (get-block-headers peer number (min batch (1+ (- to number))))))
      (unless headers (protocol-error "peer has no header for block ~D" number))
      (loop for header in headers for n from number
            unless (= (tempo-header-number header) n) do (protocol-error "peer returned wrong header at ~D" n))
      (let ((hashes (loop for header in headers
                          collect (clempo.keccak:keccak256 (rlp-view-data header)
                                                         :start (rlp-view-start header) :end (rlp-view-end header)))))
        (loop while hashes do
          (let ((bodies (get-block-bodies peer hashes)))
            (unless bodies (protocol-error "peer has no body for block ~D" number))
            (dolist (body bodies)
              (funcall consume number (pop headers) body)
              (pop hashes)
              (incf number)))))))
  (max 0 (1+ (- to from))))

;;; The acceptor owns the bounded set of worker threads. It shuts down their
;;; sockets, then joins them before its caller can release the backing store.
(defstruct inbound socket thread)

(defun run-inbound-eth (socket stream peer status earliest latest head)
  (let* ((link (eth-rlpx peer))
         (fd (sock:socket-file-descriptor socket))
         (advertise (= (eth-version peer) 69))
         (last-input (clempo.io:monotonic-ticks))
         (ping-at (+ last-input (* 10 internal-time-units-per-second)))
         (update-at ping-at))
    (loop
      (when (or (listen stream) (sb-sys:wait-until-fd-usable fd :input 0.25))
        (multiple-value-bind (code payload) (receive link)
          (setf last-input (clempo.io:monotonic-ticks))
          (unless (service-message peer code payload)
            (cond
              ((and advertise (= code #x21)) (decode-message :block-range-update payload))
              ;; This peer does not advertise headers, bodies, or transactions unsolicited.
              ((member code '(#x11 #x12 #x17 #x18)) nil)
              (t (protocol-error "unexpected inbound eth message #x~X" code))))))
      (let ((now (clempo.io:monotonic-ticks)))
        (when (> now (+ last-input (* 45 internal-time-units-per-second))) (return))
        (when (>= now ping-at)
          (send link :ping nil)
          (setf ping-at (+ now (* 10 internal-time-units-per-second))))
        (when (and advertise (>= now update-at))
          (setf update-at (+ now (* 10 internal-time-units-per-second)))
          (multiple-value-bind (first last hash) (funcall status)
            (when (or (/= first earliest) (>= last (+ latest 32))
                      (and (= last latest) (not (octets= hash head))))
              (send link :block-range-update (list first last hash))
              (setf earliest first latest last head hash))))))))

(defun call-with-eth-listener (host port key source status thunk
                               &key (chain (tempo-eth-chain)) (versions '(69 68))
                                 (max-peers 16) on-peer on-error)
  "Listen on numeric HOST and PORT (zero selects a free port), and call THUNK
with the bound port. KEY is a secp256k1 private key (32 octets). STATUS returns
(values earliest latest head-hash timestamp) for the current local chain.
SOURCE (:number hash) returns a canonical height or NIL; (:block height)
returns raw block octets or NIL; (:receipts height) returns (values list known-p)
of canonical EIP-2718 envelopes, including NIL/T for a known empty block.
Callbacks run concurrently across connections. ON-PEER receives the validated
peer key and negotiated version. ON-ERROR receives a connection failure.
Every connection and worker is closed and joined before this function returns."
  (check-type port (unsigned-byte 16))
  (check-type max-peers (integer 1 128))
  (unless (clempo.secp256k1:private->public key) (error "Invalid ETH listener key"))
  (let* ((v6 (find #\: host))
         (address (if v6 (sock:make-inet6-address host) (sock:make-inet-address host)))
         (listener (make-instance (if v6 'sock:inet6-socket 'sock:inet-socket) :type :stream :protocol :tcp))
         (lock (thr:make-mutex :name "eth inbound sockets"))
         (stopping nil) (acceptor nil) (workers nil))
    (labels ((stopping-p () (clempo.lock:with-lock (lock) stopping))
             (worker (entry bound-port)
               (let ((socket (inbound-socket entry)) (stream nil))
                 (unwind-protect
                      (handler-case
                          (progn
                            (setf (sock:non-blocking-mode socket) nil
                                  (sock:sockopt-tcp-nodelay socket) t
                                  stream (sock:socket-make-stream socket :input t :output t :element-type 'octet
                                                                         :buffering :full :timeout 5 :auto-close t))
                            (let ((link (sb-ext:with-timeout 5 (rlpx:rlpx-accept stream key))))
                              (multiple-value-bind (first last hash time) (funcall status)
                                (let ((peer (sb-ext:with-timeout 5
                                              (eth-handshake link :chain chain :versions versions
                                                             :listen-port bound-port :source source
                                                             :earliest first :latest last :head hash :timestamp time))))
                                  (when on-peer (funcall on-peer (rlpx:rlpx-peer link) (eth-version peer)))
                                  (run-inbound-eth socket stream peer status first last hash)))))
                        ((or error sb-ext:timeout) (c)
                          (when (and on-error (not (stopping-p))) (funcall on-error c))))
                   (clempo.lock:with-lock (lock)
                     (setf (inbound-socket entry) nil)
                     (if stream (ignore-errors (close stream :abort t))
                         (ignore-errors (sock:socket-close socket)))))))
             (accept-loop (bound-port)
               (let ((fd (sock:socket-file-descriptor listener)))
                 (loop until (stopping-p) do
                   (setf workers
                         (delete-if (lambda (entry)
                                      (let ((thread (inbound-thread entry)))
                                        (unless (thr:thread-alive-p thread)
                                          (thr:join-thread thread :default nil)
                                          t)))
                                    workers))
                   (when (sb-sys:wait-until-fd-usable fd :input 0.1)
                     (let ((socket (handler-case (sock:socket-accept listener)
                                     (sock:socket-error (c) (when on-error (funcall on-error c)) nil))))
                       (when socket
                         (if (>= (length workers) max-peers)
                             (sock:socket-close socket)
                             (let ((entry (make-inbound :socket socket)))
                               (handler-case
                                   (progn
                                     (setf (inbound-thread entry)
                                           (thr:make-thread (lambda () (worker entry bound-port))
                                                            :name "eth inbound peer"))
                                     (push entry workers))
                                 (error (c)
                                   (sock:socket-close socket)
                                   (when on-error (funcall on-error c)))))))))))))
      (unwind-protect
           (progn
             (setf (sock:sockopt-reuse-address listener) t
                   (sock:non-blocking-mode listener) t)
             (sock:socket-bind listener address port)
             (sock:socket-listen listener max-peers)
             (let ((bound-port (nth-value 1 (sock:socket-name listener))))
               (setf acceptor (thr:make-thread (lambda () (accept-loop bound-port)) :name "eth acceptor"))
               (funcall thunk bound-port)))
        (clempo.lock:with-lock (lock) (setf stopping t))
        (when acceptor (thr:join-thread acceptor :default nil))
        (sock:socket-close listener)
        (clempo.lock:with-lock (lock)
          (dolist (entry workers)
            (when (inbound-socket entry)
              (ignore-errors (sock:socket-shutdown (inbound-socket entry) :direction :io)))))
        (dolist (entry workers) (thr:join-thread (inbound-thread entry) :default nil))))))
