(defpackage #:clempo.dkg.tests
  (:use #:cl #:clempo.base #:clempo.cwcodec #:clempo.dkg))

(in-package #:clempo.dkg.tests)

(defun data-path (name)
  (asdf:system-relative-pathname "clempo-consensus-dkg"
                                 (concatenate 'string "../../../tests/consensus/dkg/data/" name)))

(defun data-octets (name)
  "A fixture: hex text for .hex files, raw bytes otherwise."
  (let ((path (data-path name)))
    (if (string= (pathname-type path) "hex")
        (hex->octets (string-trim '(#\Space #\Newline #\Return)
                                  (clempo.io:read-file-string path)))
        (clempo.io:read-file-octets path))))

(defun data-json (name)
  (clempo.json:parse-json (clempo.io:read-file-string (data-path name))))

(defun hex (o) (octets->hex o :prefix nil))

(defparameter *outcome-fixtures*
  ;; file, epoch it installs, chain
  '(("mainnet_block_42098399_extradata.hex" 1949)
    ("mainnet_block_44085599_extradata.hex" 2041)
    ("mainnet_block_44107199_extradata.hex" 2042)
    ("moderato_block_37735199_extradata.hex" 1747)
    ("mainnet_block_32443199_extradata.bin" 1502)))

(deftest dkg-outcome-fixtures-roundtrip ()
  (loop for (file epoch) in *outcome-fixtures*
        for octets = (data-octets file)
        for outcome = (decode-onchain-dkg-outcome octets :strict t)
        do (is= epoch (outcome-epoch outcome))
           (is (octets= octets (encode-onchain-dkg-outcome outcome)))
           (let ((sharing (outcome-sharing outcome))
                 (players (outcome-players outcome)))
             (is (eq :nonzero-counter (sharing-mode sharing)))
             (is= (length players) (sharing-total sharing))
             (is= (output-quorum (outcome-output outcome)) (sharing-threshold sharing))
             (is= 96 (length (outcome-network-identity outcome)))
             (is (every (lambda (p) (= 32 (length p))) (outcome-next-players outcome))))))

(deftest dkg-outcome-network-identities ()
  (flet ((identity-of (file)
           (outcome-network-identity (decode-onchain-dkg-outcome (data-octets file)))))
    (is= (clempo.json:jget (data-json "mainnet-network-identity.json") "identity")
         (octets->hex (identity-of "mainnet_block_44107199_extradata.hex")))
    (is= 2042 (clempo.json:jget (data-json "mainnet-network-identity.json") "from_epoch"))
    (is= (clempo.json:jget (data-json "testnet-network-identity.json") "identity")
         (octets->hex (identity-of "moderato_block_37735199_extradata.hex")))
    ;; Epoch 2041's outcome scheduled a full DKG, so 2042 has a new identity.
    (is (outcome-is-next-full-dkg
         (decode-onchain-dkg-outcome (data-octets "mainnet_block_44085599_extradata.hex"))))
    (is (not (octets= (identity-of "mainnet_block_44085599_extradata.hex")
                      (identity-of "mainnet_block_44107199_extradata.hex"))))))

(deftest dkg-outcome-strictness ()
  (let* ((octets (data-octets "mainnet_block_44107199_extradata.hex"))
         (longer (octets-concat octets (hex->octets "00"))))
    ;; Tempo reads extraData with Read: trailing bytes are ignored unless strict.
    (is= 2042 (outcome-epoch (decode-onchain-dkg-outcome longer)))
    (signals cw-error (decode-onchain-dkg-outcome longer :strict t))
    (signals cw-error (decode-onchain-dkg-outcome (subseq octets 0 (1- (length octets)))))
    ;; the trailing bool must be 0 or 1
    (let ((bad (copy-seq octets)))
      (setf (aref bad (1- (length bad))) 2)
      (signals cw-error (decode-onchain-dkg-outcome bad)))))

(defun outcome-offsets (octets)
  "Byte offsets of the sharing's mode byte and of the first player in an outcome."
  (let* ((r (make-cw-reader octets)))
    (read-varint r)                     ; epoch
    (read-raw r 32)                     ; summary
    (values (cw-reader-position r))))

(deftest dkg-outcome-rejects-malformed ()
  (let* ((octets (data-octets "mainnet_block_44107199_extradata.hex"))
         (mode-at (outcome-offsets octets)))
    ;; RootsOfUnity is not accepted by ModeVersion v0; unknown tags never.
    (let ((bad (copy-seq octets))) (setf (aref bad mode-at) 1)
      (signals cw-error (decode-onchain-dkg-outcome bad))
      (let ((*max-mode-version* 1))
        ;; decode-onchain-dkg-outcome pins v0 as Tempo does
        (signals cw-error (decode-onchain-dkg-outcome bad))))
    (let ((bad (copy-seq octets))) (setf (aref bad mode-at) 2)
      (signals cw-error (decode-onchain-dkg-outcome bad)))
    ;; total 0 is out of range
    (let ((bad (copy-seq octets))) (fill bad 0 :start (+ mode-at 1) :end (+ mode-at 5))
      (signals cw-error (decode-onchain-dkg-outcome bad)))
    ;; an identity coefficient is rejected (G2::read)
    (let ((bad (copy-seq octets)) (at (+ mode-at 5 1)))
      (setf (aref bad at) #xc0) (fill bad 0 :start (1+ at) :end (+ at 96))
      (signals cw-error (decode-onchain-dkg-outcome bad)))))

(deftest dkg-outcome-sets-must-be-sorted ()
  (let* ((outcome (decode-onchain-dkg-outcome
                   (data-octets "mainnet_block_44107199_extradata.hex")))
         (players (outcome-next-players outcome)))
    (when (>= (length players) 2)
      (let ((swapped (clempo.dkg::copy-onchain-dkg-outcome outcome)))
        (setf (outcome-next-players swapped)
              (list* (second players) (first players) (cddr players)))
        (signals cw-error (decode-onchain-dkg-outcome
                           (encode-onchain-dkg-outcome swapped)))))))

(deftest dkg-share-and-message-codecs ()
  ;; Share = varint index || nonzero scalar (rust_messages.json shares)
  (loop for h across (clempo.json:jget (data-json "rust_messages.json") "shares")
        for i from 0
        for share = (cw-decode 'share (hex->octets h))
        do (is= i (share-index share))
           (is= h (hex (cw-encode 'share share))))
  (signals cw-error (cw-decode 'share (hex->octets (format nil "00~64,'0X" 0))))
  (signals cw-error (cw-decode 'share (hex->octets (format nil "00~64,'0X" +group-order+)))))
