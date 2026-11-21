(defpackage #:clempo.follow.certs
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:follow #:clempo.follow) (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:consensus #:clempo.consensus) (#:headers #:clempo.headers)
                    (#:blocks #:clempo.blocks))
  (:export #:make-mainnet-certifier))

(in-package #:clempo.follow.certs)

(defstruct (mainnet-certifier (:constructor %make-mainnet-certifier (verifier url epoch-length)))
  verifier url epoch-length)

(defun make-mainnet-certifier (anchor &key (upstream (rpc:local-rpc-url)) (url rpc:+mainnet-rpc+))
  "Trust the upstream's canonical DKG boundary before the first followed block.
URL is a certificate-enabled RPC endpoint; it need not be the raw-block upstream."
  (let* ((chain (clempo.chainspec:load-chainspec :mainnet))
         (length (clempo.chainspec:chain-epoch-length chain))
         (epoch (floor (1+ (follow:head-number anchor)) length))
         (boundary (max 0 (1- (* epoch length))))
         (expected (if (= boundary (follow:head-number anchor)) anchor
                       (follow:rpc-head boundary :url upstream)))
         (block (blocks:decode-block
                 (svref (clempo.archive:fetch-blocks boundary 1 :url upstream :batch 1) 0)))
         (header (blocks:block-header block))
         (verifier (consensus:make-finalization-verifier
                    :epoch-length length :network-identity consensus:+mainnet-network-identity+
                    :network-from-epoch consensus:+mainnet-network-from-epoch+)))
    (unless (and (= boundary (headers:header-number header))
                 (octets= (follow:head-hash expected) (blocks:block-hash block)))
      (error "Upstream DKG anchor at block ~D does not match the canonical hash" boundary))
    (let ((outcome (consensus:verifier-register-boundary-outcome verifier
                                                               (headers:header-extra-data header))))
      (unless (= epoch (clempo.dkg:outcome-epoch outcome))
        (error "DKG anchor at ~D has epoch ~D, expected ~D" boundary
               (clempo.dkg:outcome-epoch outcome) epoch)))
    (%make-mainnet-certifier verifier url length)))

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
                               :url (mainnet-certifier-url certifier)))
         (height (hex->integer (json:jget* result "block" "header" "number"))))
    (when (> (- height missing) (mainnet-certifier-epoch-length certifier))
      (error "Certificate endpoint is missing block ~D (latest finalized ~D)" missing height))))

(defmethod follow:certify-follow-batch ((certifier mainnet-certifier) blocks previous-hash)
  (let ((responses
          (rpc:rpc-batch
           (loop for block across blocks
                 for height = (headers:header-number (blocks:block-header block))
                 collect (list "consensus_getFinalization" (list (json:jobject "height" height))))
           :url (mainnet-certifier-url certifier))))
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
          (let ((result (consensus:certify-blocks (mainnet-certifier-verifier certifier) certified
                                                  :previous-hash previous-hash)))
            (invariant (= (length certified) (consensus:certification-verified result))
                       "incomplete certificate verification result")
            (values (length certified) (map 'simple-vector #'fifth certified)))
          0))))
