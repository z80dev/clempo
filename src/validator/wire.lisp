(defpackage #:clempo.validator-wire
  (:use #:cl #:clempo.base #:clempo.cwcodec)
  (:export #:marshal-key #:make-marshal-key #:marshal-key-kind #:marshal-key-digest
           #:marshal-key-height #:marshal-key-round
           #:resolver-message #:make-resolver-message #:resolver-message-id
           #:resolver-message-kind #:resolver-message-key #:resolver-message-value
           #:resolver-message-epoch #:encode-resolver-message #:decode-resolver-message
           #:encode-broadcast-block #:decode-broadcast-block
           #:encode-marshal-value #:decode-marshal-value))
(in-package #:clempo.validator-wire)

;;; commonware-resolver 2026.10.1 p2p/wire.rs. Channel 2 alone has Tempo's
;;; epoch mux; its U64 key is FIXED width, unlike consensus View's varint.
(defstruct marshal-key kind digest height round)
(defstruct resolver-message (id 0) kind key value epoch)

(defun finished (reader)
  (unless (zerop (cw-reader-remaining reader))
    (cw-fail "trailing wire bytes: ~D" (cw-reader-remaining reader))))

(defun write-key (w key)
  (ecase (marshal-key-kind key)
    (:block
     (unless (= 32 (length (marshal-key-digest key))) (cw-fail "digest must be 32 bytes"))
     (write-u8 w 0) (write-raw w (marshal-key-digest key)))
    (:finalized (write-u8 w 1) (write-varint w (marshal-key-height key)))
    (:notarized (write-u8 w 2)
     (cw-write 'clempo.consensus:view-round w (marshal-key-round key)))))

(defun read-key (r)
  (case (read-u8 r)
    (0 (make-marshal-key :kind :block :digest (read-raw r 32)))
    (1 (make-marshal-key :kind :finalized :height (read-varint r)))
    (2 (make-marshal-key :kind :notarized
                         :round (cw-read 'clempo.consensus:view-round r)))
    (t (cw-fail "invalid marshal key tag"))))

(defun encode-resolver-message (channel message)
  "Encode a channel 2 or 4 transport payload, including the epoch mux on 2.
VALUE is raw response octets; channel 2 certificates use CW-ENCODE CERTIFICATE."
  (unless (member channel '(2 4)) (cw-fail "not a resolver channel: ~D" channel))
  (let ((w (make-cw-writer)))
    (when (= channel 2) (write-varint w (resolver-message-epoch message)))
    (write-uint w (resolver-message-id message) 8)
    (ecase (resolver-message-kind message)
      (:request
       (write-u8 w 0)
       (if (= channel 2) (write-uint w (resolver-message-key message) 8)
           (write-key w (resolver-message-key message))))
      (:response
       (write-u8 w 1)
       (let ((value (resolver-message-value message)))
         (write-length w (length value)) (write-raw w value)))
      (:error (write-u8 w 2)))
    (cw-writer-octets w)))

(defun decode-resolver-message (channel octets)
  "Decode a complete channel 2/4 payload; response VALUE remains raw octets.
The caller must correlate ID with its outstanding key before decoding a response."
  (unless (member channel '(2 4)) (cw-fail "not a resolver channel: ~D" channel))
  (let* ((r (make-cw-reader octets))
         (epoch (when (= channel 2) (read-varint r)))
         (message (make-resolver-message :epoch epoch :id (read-uint r 8))))
    (case (read-u8 r)
      (0 (setf (resolver-message-kind message) :request
               (resolver-message-key message) (if (= channel 2) (read-uint r 8) (read-key r))))
      (1 (setf (resolver-message-kind message) :response
               (resolver-message-value message) (read-raw r (read-length r))))
      (2 (setf (resolver-message-kind message) :error))
      (t (cw-fail "invalid resolver payload tag")))
    (finished r)
    message))

(defun decode-broadcast-block (octets)
  "Channel 3 is exactly one execution-block RLP, without a length or mux prefix.
This decodes structure; execution validity and certificate verification are caller duties."
  (clempo.blocks:decode-block octets))

(defun encode-broadcast-block (block)
  (clempo.blocks:encode-block block))

(defun certificate-type (key)
  (ecase (marshal-key-kind key)
    (:block nil)
    (:finalized 'clempo.consensus:finalization)
    (:notarized 'clempo.consensus:notarization)))

(defun decode-marshal-value (key octets)
  "Return (values block certificate). Certified responses are (certificate, block)
with NO certificate enum tag. Checks key height/round and payload commitment;
cryptographic certificate and execution validation remain the caller's duties."
  (let* ((r (make-cw-reader octets))
         (type (certificate-type key))
         (certificate (when type (cw-read type r)))
         (block (decode-broadcast-block (read-raw r (cw-reader-remaining r))))
         (hash (clempo.blocks:block-hash block)))
    (ecase (marshal-key-kind key)
      (:block (unless (equalp hash (marshal-key-digest key)) (cw-fail "block commitment mismatch")))
      (:finalized
       (unless (= (marshal-key-height key)
                  (clempo.headers:header-number (clempo.blocks:block-header block)))
         (cw-fail "finalized height mismatch")))
      (:notarized
       (unless (clempo.consensus:round= (marshal-key-round key)
                                       (clempo.consensus:certificate-round certificate))
         (cw-fail "notarized round mismatch"))))
    (when certificate
      (let ((proposal (etypecase certificate
                        (clempo.consensus:finalization
                         (clempo.consensus:finalization-proposal certificate))
                        (clempo.consensus:notarization
                         (clempo.consensus:notarization-proposal certificate)))))
        (unless (equalp hash (clempo.consensus:proposal-payload proposal))
          (cw-fail "certificate payload mismatch"))))
    (values block certificate)))

(defun encode-marshal-value (key block &optional certificate)
  "Encode the response value (not the resolver envelope)."
  (let ((w (make-cw-writer)) (type (certificate-type key)))
    (if type (cw-write type w certificate)
        (when certificate (cw-fail "block response cannot carry a certificate")))
    (write-raw w (encode-broadcast-block block))
    (let ((bytes (cw-writer-octets w)))
      (decode-marshal-value key bytes)
      bytes)))
