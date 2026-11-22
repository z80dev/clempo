(defpackage #:clempo.receipts
  (:use #:cl #:clempo.base)
  (:export #:receipt #:make-receipt #:receipt-type #:receipt-status #:receipt-cumulative-gas-used
           #:receipt-logs #:receipt-bloom #:receipt-fee-token #:receipt-fee-payer
           #:receipt-from-rpc #:decode-receipt #:encode-receipt #:receipts-root
           #:event-log #:make-event-log #:event-log-address #:event-log-topics #:event-log-data
           #:logs-bloom #:receipts-bloom))
(in-package #:clempo.receipts)

(defstruct event-log (address 0 :type (unsigned-byte 160)) (topics nil :type list)
  (data (make-octets 0) :type octets))
(defstruct receipt (type 0 :type (unsigned-byte 8)) (status 0 :type bit)
  (cumulative-gas-used 0 :type (unsigned-byte 64)) (logs nil :type list)
  (bloom nil :type (or null octets)) fee-token fee-payer)

(defbound +bloom-bytes+ 256 "Bytes of a logs bloom."
  ;; A probe is an 11-bit index (#x7FF) into the bloom's bits.
  (= (* 8 +bloom-bytes+) (1+ #x7ff)))

(defbound +bloom-probes+ 3
  "Bits each address or topic sets, from successive byte pairs of its Keccak-256 hash."
  (<= (* 2 +bloom-probes+) 32))

(defbound +bloom-words+ (floor +bloom-bytes+ sb-vm:n-word-bytes)
  "Machine words of a bloom, which RECEIPTS-BLOOM merges a word at a time."
  (= (* +bloom-words+ sb-vm:n-word-bytes) +bloom-bytes+))

(defun bloom-accrue (bloom bytes &optional k)
  "Set BYTES' three bloom bits in BLOOM. K, a reusable keccak context, saves allocating one
for inputs that are not 32 or 64 bytes."
  (declare (type octets bloom bytes) #.clempo.base:*hot*)
  (let ((hash (make-array 32 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent hash))
    (clempo.keccak:keccak256-into bytes 0 (length bytes) hash 0 k)
    (dotimes (i +bloom-probes+)
      (let* ((bit (logand #x7ff (logior (ash (aref hash (* i 2)) 8) (aref hash (1+ (* i 2))))))
             (byte (- (1- +bloom-bytes+) (ash bit -3))))
        (setf (aref bloom byte) (logior (aref bloom byte) (ash 1 (logand bit 7))))))))

(defun logs-bloom (logs)
  (let ((bloom (make-octets +bloom-bytes+))
        (address (make-array 20 :element-type '(unsigned-byte 8)))
        (k (when logs (clempo.keccak:make-keccak))))
    (declare (dynamic-extent address))
    (dolist (log logs bloom)
      (bloom-accrue bloom (integer-into-octets (event-log-address log) address 0 20) k)
      (dolist (topic (event-log-topics log)) (bloom-accrue bloom topic k)))))

(defvar +zero-bloom+ (make-octets +bloom-bytes+)
  "The bloom of no logs; read, never written (a receipt without logs has a NIL bloom).")

(declaim (inline receipt-bloom*))
(defun receipt-bloom* (receipt)
  "RECEIPT's bloom, computed from its logs when not stored; shared and read-only when empty."
  (or (receipt-bloom receipt)
      (let ((logs (receipt-logs receipt))) (if logs (logs-bloom logs) +zero-bloom+))))

(defun receipts-bloom (receipts)
  (declare #.clempo.base:*hot*)
  (let ((bloom (make-octets +bloom-bytes+)))
    (declare (type (octets #.+bloom-bytes+) bloom))
    (dolist (receipt receipts bloom)
      (unless (and (null (receipt-bloom receipt)) (null (receipt-logs receipt))) ; adds nothing
        (let ((other (receipt-bloom* receipt)))
          (declare (type octets other))
          (invariant (= (length other) +bloom-bytes+) "a bloom is ~D bytes, not ~D" +bloom-bytes+ (length other))
          (dotimes (i +bloom-words+)    ; a machine word at a time
            (setf (sb-kernel:%vector-raw-bits bloom i)
                  (logior (sb-kernel:%vector-raw-bits bloom i) (sb-kernel:%vector-raw-bits other i)))))))))

(defun log-items (log)
  (list (integer->octets (event-log-address log) 20) (event-log-topics log) (event-log-data log)))

(defun log-from-items (items)
  (invariant (and (listp items) (= 3 (length items)) (listp (second items))) "invalid log fields")
  (make-event-log :address (clempo.defrlp:decode-field :address (first items))
                  :topics (mapcar (lambda (x) (clempo.defrlp:decode-field '(:bytes 32) x)) (second items))
                  :data (clempo.defrlp:decode-field :bytes (third items))))

(defun encode-receipt (receipt)
  "Consensus encoding excludes RPC-only feeToken and feePayer metadata."
  (let ((body (clempo.rlp:rlp-encode
               (list (receipt-status receipt) (receipt-cumulative-gas-used receipt)
                     (receipt-bloom* receipt)
                     (mapcar #'log-items (receipt-logs receipt))))))
    (if (zerop (receipt-type receipt)) body
        (octets-concat (octets-of (receipt-type receipt)) body))))

(defun decode-receipt (raw)
  (invariant (plusp (length raw)) "empty receipt")
  (let* ((typed (< (aref raw 0) #x80)) (type (if typed (aref raw 0) 0))
         (items (clempo.rlp:rlp-decode raw :start (if typed 1 0))))
    (invariant (and (member type '(0 1 2 4 #x76)) (or (not typed) (plusp type))) "unsupported receipt type")
    (invariant (and (listp items) (= 4 (length items)) (listp (fourth items))) "invalid receipt fields")
    (let ((status (clempo.rlp:rlp-decode-integer (first items) :max-bytes 1)))
      (invariant (<= status 1) "invalid receipt status")
      (make-receipt :type type :status status
                    :cumulative-gas-used (clempo.rlp:rlp-decode-integer (second items) :max-bytes 8)
                    :bloom (clempo.defrlp:decode-field '(:bytes 256) (third items))
                    :logs (mapcar #'log-from-items (fourth items))))))

(defun receipt-from-rpc (object)
  "Rebuild bloom from logs, never trusting the RPC's cached bloom."
  (flet ((quantity (key) (hex->integer (clempo.json:jget object key)))
         (optional-address (key)
           (let ((value (clempo.json:jget object key)))
             (when (stringp value) (hex->integer value)))))
    (make-receipt
     :type (quantity "type") :status (quantity "status")
     :cumulative-gas-used (quantity "cumulativeGasUsed")
     :fee-token (optional-address "feeToken") :fee-payer (optional-address "feePayer")
     :logs (loop for log across (clempo.json:jget object "logs")
                 collect (make-event-log :address (hex->integer (clempo.json:jget log "address"))
                                         :topics (map 'list #'hex->octets (clempo.json:jget log "topics"))
                                         :data (hex->octets (clempo.json:jget log "data")))))))

(defvar *last-receipts-root* (cons nil nil)
  "(key . root) of the last RECEIPTS-ROOT, replaced whole, never mutated: consecutive blocks
often have identical receipts (early mainnet's are one system transaction's each), and the
trie costs more than comparing them. The key is a vector of (type status cumulative-gas)
triples when no receipt has logs or a stored bloom (they then determine the encodings), else
the list of encodings.")

(defun plain-receipts-key (receipts)
  "RECEIPTS' (type status cumulative-gas ...) vector when none has logs or a stored bloom."
  (when (every (lambda (r) (and (null (receipt-logs r)) (null (receipt-bloom r)))) receipts)
    (let ((key (make-array (* 3 (length receipts)))) (i 0))
      (dolist (r receipts key)
        (setf (svref key i) (receipt-type r)
              (svref key (+ i 1)) (receipt-status r)
              (svref key (+ i 2)) (receipt-cumulative-gas-used r))
        (incf i 3)))))

(defun receipts-root (receipts)
  (let ((last *last-receipts-root*)
        (plain (and receipts (plain-receipts-key receipts))))
    (if (and plain (equalp plain (car last)))
        (copy-seq (cdr last))
        (let ((encodings (mapcar #'encode-receipt receipts)))
          (if (and (not plain) encodings (listp (car last))
                   (= (length encodings) (length (car last)))
                   (every #'octets= encodings (car last)))
              (copy-seq (cdr last))
              (let ((root (clempo.trie:ordered-trie-root encodings)))
                (setf *last-receipts-root* (cons (or plain encodings) (copy-seq root)))
                root))))))
