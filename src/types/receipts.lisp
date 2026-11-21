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

(defun bloom-accrue (bloom bytes)
  (declare (type octets bloom bytes) #.clempo.base:*hot*)
  (let ((hash (clempo.keccak:keccak256 bytes)))
    (declare (type octets hash))
    (dotimes (i 3)
      (let* ((bit (logand #x7ff (logior (ash (aref hash (* i 2)) 8) (aref hash (1+ (* i 2))))))
             (byte (- 255 (ash bit -3))))
        (setf (aref bloom byte) (logior (aref bloom byte) (ash 1 (logand bit 7))))))))

(defun logs-bloom (logs)
  (let ((bloom (make-octets 256)))
    (dolist (log logs bloom)
      (bloom-accrue bloom (integer->octets (event-log-address log) 20))
      (dolist (topic (event-log-topics log)) (bloom-accrue bloom topic)))))

(defun receipts-bloom (receipts)
  (let ((bloom (make-octets 256)))
    (dolist (receipt receipts bloom)
      (let ((other (logs-bloom (receipt-logs receipt))))
        (dotimes (i 256) (setf (aref bloom i) (logior (aref bloom i) (aref other i))))))))

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
                     (or (receipt-bloom receipt) (logs-bloom (receipt-logs receipt)))
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

(defun receipts-root (receipts)
  (clempo.trie:ordered-trie-root (mapcar #'encode-receipt receipts)))
