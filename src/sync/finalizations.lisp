(defpackage #:clempo.follow.certs
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:follow #:clempo.follow) (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:consensus #:clempo.consensus) (#:headers #:clempo.headers)
                    (#:blocks #:clempo.blocks))
  (:export #:make-certifier #:certifier-anchor #:certifier-uncertified #:certify-from
           #:certifier-from #:certifier-from-source #:discover-certify-from #:descend-header-chain
           #:network-identity))

(in-package #:clempo.follow.certs)

;;; Trust. A finalization certificate for a block of epoch E verifies under E's group
;;; identity: the chain's published network identity from its from-epoch on (tempo's
;;; mainnet-network-identity.json and testnet-network-identity.json), or an identity
;;; read from the DKG outcome in the certified boundary block that closes E-1.
;;;
;;; The follower's first epoch needs its identity before any block is certified. It is
;;; taken from the anchor boundary (the last block of the previous epoch), and that block
;;; is believed only when (a) it is a block the caller has itself executed (TRUSTED-HASH
;;; returns its hash), or (b) its own finalization certificate verifies under the network
;;; identity. The upstream alone is never believed for an identity: a forged boundary
;;; block or a forged certificate fails closed.
;;;
;;; Certificate endpoints only serve finalizations from some height on (CERTIFY-FROM).
;;; Blocks below it are admitted on their parent links and execution alone, as goempo's
;;; -certify-from does; boundaries crossed there register their outcomes, so the first
;;; certified epoch verifies under the identity the hash-linked chain carries.

(defun network-identity (chain)
  "The published network identity and its from-epoch for CHAIN."
  (ecase chain
    (:mainnet (values consensus:+mainnet-network-identity+ consensus:+mainnet-network-from-epoch+))
    (:moderato
     (values (load-time-value
              (hex->octets "967ae1a6d3ddbe5cb0fe5e6fc58e74249787b4a277619b549fed6609d04660540dd2449cd7174def35e40f544e5ad72d15d064191a205ed6e0c49619c68f975108b614d0d51def9a8416a10a07c4193bea0cbbc4252ec1b33f21095a5d7aa590"))
             1747))))

(defun certify-from (chain)
  "The first height the public certificate endpoint served when probed on 2026-11-21."
  (ecase chain (:mainnet 41166893) (:moderato 36627280)))

(defstruct (certifier (:constructor %make-certifier (verifier url epoch-length from network-epoch)))
  verifier url epoch-length from network-epoch
  (anchor nil)          ; how the first epoch's identity was established
  (from-source nil)     ; :given, :recorded, :discovered or :recorded-unprobed
  (uncertified 0 :type integer))

(defun fetch-finalization (url height)
  (let ((result (handler-case (rpc:rpc-call "consensus_getFinalization"
                                            (list (json:jobject "height" height)) :url url
                                            :retries 1)
                  (rpc:rpc-error () :null))))
    (and (json:jobject-p result) result)))

(defun discover-certify-from (url known)
  "The first height URL serves a finalization for. KNOWN (the recorded value) is kept when
it is still served; when the endpoint has pruned past it, the earliest served height is
found by bisection up to the latest finalization. Returns (values height how)."
  (cond ((fetch-finalization url known) (values known :recorded))
        (t (let* ((latest (rpc:rpc-call "consensus_getFinalization" '("latest") :url url))
                  (high (hex->integer (json:jget* latest "block" "header" "number")))
                  (low known))
             ;; Invariant: LOW is not served, HIGH is.
             (loop while (> (- high low) 1)
                   do (let ((mid (floor (+ low high) 2)))
                        (if (fetch-finalization url mid) (setf high mid) (setf low mid))))
             (values high :discovered)))))

(defun register-outcome (certifier height extra-data)
  "Install the identity in boundary HEIGHT's EXTRA-DATA; check it names the next epoch and,
at the network identity's from-epoch, that identity."
  (let* ((verifier (certifier-verifier certifier))
         (next (1+ (floor height (certifier-epoch-length certifier))))
         (outcome (handler-case (clempo.dkg:decode-onchain-dkg-outcome extra-data)
                    (error (c) (error "DKG outcome at boundary ~D does not decode: ~A" height c)))))
    (unless (= next (clempo.dkg:outcome-epoch outcome))
      (error "DKG outcome at boundary ~D names epoch ~D, expected ~D" height
             (clempo.dkg:outcome-epoch outcome) next))
    (when (= next (certifier-network-epoch certifier))
      (unless (octets= (clempo.dkg:outcome-network-identity outcome)
                       (nth-value 0 (consensus:verifier-network-identity verifier)))
        (error "DKG outcome at boundary ~D is not the published network identity" height)))
    (consensus:verifier-register-boundary-outcome verifier extra-data)))

(defun make-certifier (anchor &key (chain :mainnet) (upstream (rpc:local-rpc-url))
                                   (url (ecase chain (:mainnet rpc:+mainnet-rpc+)
                                          (:moderato rpc:+moderato-rpc+)))
                                   from trusted-hash)
  "A certifier for following from ANCHOR (a FOLLOW:HEAD). URL serves consensus_getFinalization;
UPSTREAM serves raw blocks. TRUSTED-HASH, if given, maps a block number to the hash of a
block the caller executed itself (or NIL). FROM (default: DISCOVER-CERTIFY-FROM from the
recorded CERTIFY-FROM, or that recorded value when URL cannot be probed) is the first height
whose certificate is required."
  (let ((from-source :given))
    (unless from
      (multiple-value-setq (from from-source)
        (handler-case (discover-certify-from url (certify-from chain))
          (error () (values (certify-from chain) :recorded-unprobed)))))
    (let ((certifier (build-certifier anchor chain upstream url from trusted-hash)))
      (setf (certifier-from-source certifier) from-source)
      certifier)))

(defun build-certifier (anchor chain upstream url from trusted-hash)
  (let* ((spec (clempo.chainspec:load-chainspec chain))
         (length (clempo.chainspec:chain-epoch-length spec))
         (epoch (floor (1+ (follow:head-number anchor)) length))
         (boundary (1- (* epoch length))))
    (multiple-value-bind (identity network-epoch) (network-identity chain)
      (let ((certifier (%make-certifier
                        (consensus:make-finalization-verifier
                         :epoch-length length :network-identity identity
                         :network-from-epoch network-epoch)
                        url length from network-epoch)))
        (if (zerop epoch)
            (setf (certifier-anchor certifier) :genesis-epoch)
            (anchor-boundary certifier boundary anchor upstream trusted-hash))
        certifier))))

(defun anchor-boundary (certifier boundary anchor upstream trusted-hash)
  (let* ((block (blocks:decode-block
                 (svref (clempo.archive:fetch-blocks boundary 1 :url upstream :batch 1) 0)))
         (header (blocks:block-header block))
         (hash (blocks:block-hash block))
         (local (and trusted-hash (funcall trusted-hash boundary))))
    (unless (= boundary (headers:header-number header))
      (error "Upstream returned block ~D for the DKG anchor ~D" (headers:header-number header) boundary))
    (when (blocks:block-body-mismatches block)
      (error "Upstream DKG anchor block ~D has a body that does not match its header" boundary))
    (cond
      (local
       (unless (octets= local hash)
         (error "Upstream DKG anchor at block ~D differs from the executed block" boundary))
       (register-outcome certifier boundary (headers:header-extra-data header))
       (setf (certifier-anchor certifier) :executed))
      ((< boundary (certifier-from certifier))
       ;; No certificate exists this low; the anchor is as trusted as the hash-linked
       ;; blocks below CERTIFY-FROM, and must be the upstream's canonical block.
       (let ((expected (if (= boundary (follow:head-number anchor)) anchor
                           (follow:rpc-head boundary :url upstream))))
         (unless (octets= (follow:head-hash expected) hash)
           (error "Upstream DKG anchor at block ~D does not match the canonical hash" boundary)))
       (register-outcome certifier boundary (headers:header-extra-data header))
       (setf (certifier-anchor certifier) :hash-linked))
      ((= (1+ (floor boundary (certifier-epoch-length certifier))) (certifier-network-epoch certifier))
       ;; The boundary before the network identity's from-epoch: its certificate needs the
       ;; previous committee's key, but the outcome is accepted only if it names the
       ;; published network identity itself (REGISTER-OUTCOME), so nothing is believed.
       (register-outcome certifier boundary (headers:header-extra-data header))
       (setf (certifier-anchor certifier) :network-identity))
      ((< (floor boundary (certifier-epoch-length certifier)) (certifier-network-epoch certifier))
       ;; No key for its epoch is published, but every block is final with its descendants:
       ;; verify the from-epoch's boundary certificate under the network identity, then
       ;; follow parent hashes down to this boundary.
       (link-to-certified-boundary certifier boundary hash upstream)
       (register-outcome certifier boundary (headers:header-extra-data header))
       (setf (certifier-anchor certifier) :certified-descendant))
      (t
       (let ((result (fetch-finalization (certifier-url certifier) boundary)))
         (unless result
           (error "No finalization certificate for the DKG anchor block ~D" boundary))
         (let ((certificate (json:jget result "certificate")))
           (unless (stringp certificate)
             (error "Malformed finalization for the DKG anchor block ~D" boundary))
           ;; Verified under the epoch it closes: the network identity, never the
           ;; upstream's. CERTIFY-BLOCKS then registers the boundary's outcome.
           (consensus:certify-blocks
            (certifier-verifier certifier)
            (list (list boundary hash (headers:header-parent-hash header)
                        (headers:header-extra-data header) certificate)))
           (setf (certifier-anchor certifier) :certificate)))))))

(defun raw-headers (from count upstream)
  "Raw header RLP of blocks FROM..FROM+COUNT-1 from UPSTREAM, as a list."
  (loop for result in (rpc:rpc-batch (loop for n from from below (+ from count)
                                           collect (list "debug_getRawHeader" (list (quantity->hex n))))
                                     :url upstream)
        for n from from
        collect (if (stringp result) (hex->octets result)
                    (error "Upstream has no raw header ~D: ~A" n result))))

(defun link-to-certified-boundary (certifier boundary hash upstream &key (batch 500))
  "Authenticate block BOUNDARY (HASH) as an ancestor of the last block of the network
identity's from-epoch, whose own certificate verifies under that published identity."
  (let* ((target (1- (* (1+ (certifier-network-epoch certifier)) (certifier-epoch-length certifier))))
         (raw (first (raw-headers target 1 upstream)))
         (header (headers:decode-header raw))
         (result (fetch-finalization (certifier-url certifier) target))
         (certificate (and result (json:jget result "certificate"))))
    (unless (stringp certificate)
      (error "No finalization certificate for block ~D to authenticate the DKG anchor ~D" target boundary))
    (consensus:certify-blocks
     (certifier-verifier certifier)
     (list (list target (clempo.keccak:keccak256 raw) (headers:header-parent-hash header)
                 (headers:header-extra-data header) certificate)))
    (unless (octets= hash (descend-header-chain (headers:header-parent-hash header) (1- target) boundary
                                                (lambda (from count) (raw-headers from count upstream))
                                                :batch batch))
      (error "DKG anchor block ~D is not an ancestor of the certified block ~D" boundary target))))

(defun descend-header-chain (expected top bottom fetch &key (batch 500))
  "Walk raw headers TOP down to BOTTOM+1 (FETCH: from count -> list of raw headers), each of
which must hash to EXPECTED, the parent hash named by the header above it. Returns the parent
hash the lowest one names: the hash of block BOTTOM, if the chain holds."
  (loop for high = top then (- high batch)
        while (> high bottom)
        do (let ((from (max (1+ bottom) (1+ (- high batch)))))
             (dolist (raw (reverse (funcall fetch from (1+ (- high from)))))
               (let ((header (headers:decode-header raw)))
                 (unless (octets= expected (clempo.keccak:keccak256 raw))
                   (error "Header chain breaks at block ~D" (headers:header-number header)))
                 (setf expected (headers:header-parent-hash header))))))
  expected)

(defun not-finalized-p (result)
  (or (eq :null result)
      (and (typep result 'rpc:rpc-error)
           (eql 503 (rpc:rpc-error-code result))
           (member (rpc:rpc-error-data result) '("certificate" "block") :test #'equal))))

(defun check-certificate-window (certifier missing)
  ;; Tempo uses the same error for a missing archive entry and a block that
  ;; has not finalized yet. A trailing miss at the tip can be polled, but
  ;; fail closed rather than sleeping forever on a historical gap.
  (let* ((result (rpc:rpc-call "consensus_getFinalization" '("latest")
                               :url (certifier-url certifier)))
         (height (hex->integer (json:jget* result "block" "header" "number"))))
    (when (> (- height missing) (certifier-epoch-length certifier))
      (error "Certificate endpoint is missing block ~D (latest finalized ~D)" missing height))))

(defun admit-uncertified (certifier blocks)
  "Admit the leading blocks below CERTIFY-FROM; returns how many."
  (loop for block across blocks
        for header = (blocks:block-header block)
        for height = (headers:header-number header)
        while (< height (certifier-from certifier))
        count t
        do (incf (certifier-uncertified certifier))
           (when (consensus:boundary-height-p (consensus:make-fixed-epocher
                                               (certifier-epoch-length certifier))
                                              height)
             (register-outcome certifier height (headers:header-extra-data header)))))

(defmethod follow:certify-follow-batch ((certifier certifier) blocks previous-hash)
  (let ((skipped (admit-uncertified certifier blocks)))
    (when (plusp skipped)
      ;; The rest certifies on the next call, from a batch starting at CERTIFY-FROM.
      (return-from follow:certify-follow-batch
        (values skipped (make-array skipped :initial-element nil))))
    (certify-batch certifier blocks previous-hash)))

(defun certify-batch (certifier blocks previous-hash)
  (let ((responses
          (rpc:rpc-batch
           (loop for block across blocks
                 for height = (headers:header-number (blocks:block-header block))
                 collect (list "consensus_getFinalization" (list (json:jobject "height" height))))
           :url (certifier-url certifier))))
    (let ((certified
            (loop for response in responses for block across blocks
                  for header = (blocks:block-header block)
                  for height = (headers:header-number header)
                  for hash = (blocks:block-hash block)
                  until (when (not-finalized-p response)
                          (check-certificate-window certifier height)
                          t)
                  when (typep response 'rpc:rpc-error) do (error response)
                  collect (let ((certificate (json:jget response "certificate"))
                                (digest (json:jget response "digest")))
                            (unless (and (stringp certificate) (stringp digest)
                                         (octets= hash (hex->octets digest)))
                              (error "Certificate RPC result at block ~D is malformed or for another block"
                                     height))
                            (list height hash (headers:header-parent-hash header)
                                  (headers:header-extra-data header) certificate)))))
      (if certified
          (let ((result (consensus:certify-blocks (certifier-verifier certifier) certified
                                                  :previous-hash previous-hash)))
            (invariant (= (length certified) (consensus:certification-verified result))
                       "incomplete certificate verification result")
            (values (length certified) (map 'simple-vector #'fifth certified)))
          0))))
