(defpackage #:clempo.validator-wire.tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:wire #:clempo.validator-wire) (#:cw #:clempo.cwcodec)))
(in-package #:clempo.validator-wire.tests)

(defun golden-vectors ()
  (clempo.json:jget
   (clempo.json:parse-json
    (clempo.io:read-file-string
     (asdf:system-relative-pathname "clempo-validator" "../../tests/consensus/data/rust_golden.json")))
   "vectors"))

(defun golden-vector (name)
  (find name (golden-vectors) :test #'string= :key (lambda (v) (clempo.json:jget v "name"))))

(defun golden-bytes (name)
  (hex->octets (clempo.json:jget (golden-vector name) "bytes")))

(deftest rust-resolver-envelopes ()
  (loop for row across (golden-vectors)
        for channel = (clempo.json:jget row "channel")
        when (member channel '(2 4))
          do (let* ((bytes (hex->octets (clempo.json:jget row "bytes")))
                    (message (wire:decode-resolver-message channel bytes)))
               (is= (clempo.json:jget row "id") (wire:resolver-message-id message))
               (is-equalp bytes (wire:encode-resolver-message channel message))
               (when (= channel 2)
                 (is= (clempo.json:jget row "epoch") (wire:resolver-message-epoch message))
                 (when (eq :request (wire:resolver-message-kind message))
                   (is= (clempo.json:jget row "view") (wire:resolver-message-key message)))
                 (when (eq :response (wire:resolver-message-kind message))
                   (let* ((value (wire:resolver-message-value message))
                          (cert (cw:cw-decode 'clempo.consensus:certificate value)))
                     (is= (wire:resolver-message-epoch message)
                          (clempo.consensus:round-epoch (clempo.consensus:certificate-round cert)))
                     (is-equalp value (cw:cw-encode 'clempo.consensus:certificate cert))))))))

(deftest rust-broadcast-and-backfill-values ()
  (let* ((raw (golden-bytes "broadcast/block"))
         (block (wire:decode-broadcast-block raw)))
    (is-equalp raw (wire:encode-broadcast-block block))
    (is-equalp (hex->octets (clempo.json:jget (golden-vector "broadcast/block") "digest"))
               (clempo.blocks:block-hash block))
    (dolist (kind '("block" "finalized" "notarized"))
      (let* ((request (wire:decode-resolver-message
                       4 (golden-bytes (concatenate 'string "marshal/request/" kind))))
             (response (wire:decode-resolver-message
                        4 (golden-bytes (concatenate 'string "marshal/response/" kind))))
             (key (wire:resolver-message-key request))
             (value (wire:resolver-message-value response)))
        (multiple-value-bind (decoded certificate) (wire:decode-marshal-value key value)
          (is-equalp raw (wire:encode-broadcast-block decoded))
          (is-equalp value (wire:encode-marshal-value key decoded certificate)))))))

(deftest recorded-devnet-broadcasts ()
  (loop for row across
        (clempo.json:parse-json
         (clempo.io:read-file-string
          (asdf:system-relative-pathname "clempo-p2p" "../../tests/p2p/data/tempo_devnet_frames.json")))
        when (= 3 (clempo.json:jget row "channel"))
          do (let ((raw (hex->octets (clempo.json:jget row "data"))))
               (is-equalp raw (wire:encode-broadcast-block (wire:decode-broadcast-block raw))))))

(deftest rejects-malformed-wire ()
  (dolist (channel '(2 4))
    (let ((raw (golden-bytes (if (= channel 2) "resolver/request/e3/v17" "marshal/request/notarized"))))
      (dotimes (end (length raw))
        (signals cw:cw-error (wire:decode-resolver-message channel (subseq raw 0 end))))
      (signals cw:cw-error
        (wire:decode-resolver-message channel (octets-concat raw (octets-of 0))))))
  (signals cw:cw-error (wire:decode-resolver-message 4 (hex->octets "000000000000000003")))
  (signals cw:cw-error (wire:decode-resolver-message 4 (hex->octets "00000000000000000003")))
  (signals cw:cw-error (wire:decode-resolver-message 4 (hex->octets "0000000000000000018000")))
  (signals cw:cw-error (wire:decode-resolver-message 2 (hex->octets "8000000000000000000002")))
  (signals error (wire:decode-broadcast-block (octets-of #xc0)))
  (let* ((request (wire:decode-resolver-message 4 (golden-bytes "marshal/request/notarized")))
         (response (wire:decode-resolver-message 4 (golden-bytes "marshal/response/notarized")))
         (key (wire:resolver-message-key request)))
    (setf (wire:marshal-key-round key) (clempo.consensus:make-round :epoch 0 :view 2))
    (signals cw:cw-error (wire:decode-marshal-value key (wire:resolver-message-value response)))))
