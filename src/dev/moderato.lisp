(in-package #:clempo.dev)

;;; R6f is an observational dry run, not registration or validator admission.
;;; Specification: Tempo testnet-network-identity.json, contracts/src/precompiles/
;;; validator_config_v2.rs and validator-config/src/lib.rs; goempo docs/DEVNET.md.
;;; Example: bin/clempo-dev moderato-readiness -last 8 -timeout 10
;;; The disposable signing seed is never printed or persisted. Its registration
;;; payload uses documentation-only IPs by default and cannot be used to operate
;;; a validator after this process exits. No transaction RPC is implemented here.

(define-constant +readiness-moderato-identity+
  (hex->octets "967ae1a6d3ddbe5cb0fe5e6fc58e74249787b4a277619b549fed6609d04660540dd2449cd7174def35e40f544e5ad72d15d064191a205ed6e0c49619c68f975108b614d0d51def9a8416a10a07c4193bea0cbbc4252ec1b33f21095a5d7aa590"))
(defconstant +readiness-moderato-from-epoch+ 1747)
(defparameter *readiness-registry* "0xcccccccc00000000000000000000000000000001")
(defparameter *readiness-add-signature* "addValidator(address,bytes32,string,string,address,bytes)")

(defun readiness-calldata (signature values)
  (clempo.abi:abi-encode-call (clempo.abi:abi-selector signature)
                            (clempo.abi:signature-types signature) values))

(defun readiness-registration (seed validator ingress egress fee-recipient)
  "Return public key, proof and exact V2 calldata for disposable Moderato inputs.
The Ed25519 proof signs Commonware union_unique(namespace, keccak256(preimage));
chain ID is u64 big endian, addresses 20 bytes, endpoint lengths single bytes."
  (let* ((in (clempo.abi:utf8-encode ingress))
         (out (clempo.abi:utf8-encode egress))
         (namespace (string->octets "TEMPO_VALIDATOR_CONFIG_V2_ADD_VALIDATOR"))
         (public (clempo.ed25519:ed25519-public-key seed)))
    (invariant (and (<= (length in) 255) (<= (length out) 255)) "endpoint too long")
    (let* ((digest (clempo.keccak:keccak256
                    (octets-concat (integer->octets 42431 8)
                                   (hex->octets *readiness-registry*)
                                   (integer->octets validator 20)
                                   (octets-of (length in)) in
                                   (octets-of (length out)) out
                                   (integer->octets fee-recipient 20))))
           (proof (clempo.ed25519:ed25519-sign
                   seed (octets-concat (octets-of (length namespace)) namespace digest))))
      (values public proof
              (readiness-calldata *readiness-add-signature*
                                  (list validator (octets->integer public) ingress egress
                                        fee-recipient proof))))))
(defun (setf readiness-field) (value key object)
  (let ((entry (assoc key (cdr object) :test #'string=)))
    (if entry (setf (cdr entry) value)
        (setf (cdr object) (append (cdr object) (list (cons key value))))))
  value)


(defun readiness-call (data tag &optional from)
  "Only eth_call, pinned to TAG; FROM is used solely for the owner simulation."
  (let ((call (clempo.json:jobject "to" *readiness-registry* "data" (octets->hex data))))
    (when from (setf (readiness-field "from" call) from))
    (hex->octets (clempo.jsonrpc:rpc-call "eth_call" (list call tag)))))

(defun readiness-certificates (spec count)
  "Verify COUNT recent finalized blocks with the published Moderato trust anchor.
Never trust an uncertified RPC-supplied boundary as a replacement anchor."
  (let* ((head (clempo.jsonrpc:rpc-call "eth_getBlockByNumber" (list "finalized" :false)))
         (to (hex->integer (clempo.json:jget head "number")))
         (from (- to count -1))
         (verifier (clempo.consensus:make-finalization-verifier
                    :epoch-length (clempo.chainspec:chain-epoch-length spec)
                    :network-identity +readiness-moderato-identity+
                    :network-from-epoch +readiness-moderato-from-epoch+))
         (blocks '()))
    (invariant (>= from (* +readiness-moderato-from-epoch+
                           (clempo.chainspec:chain-epoch-length spec))) "range predates trust anchor")
    (loop for start from from to to by 16
          for end = (min to (+ start 15))
          for rows = (clempo.jsonrpc:rpc-batch
                      (loop for height from start to end
                            collect (list "debug_getRawBlock" (list (quantity->hex height)))
                            collect (list "consensus_getFinalization"
                                          (list (clempo.json:jobject "height" height)))))
          do (loop for (raw reply) on rows by #'cddr for height from start
                   do (when (typep raw 'clempo.jsonrpc:rpc-error) (error raw))
                      (when (typep reply 'clempo.jsonrpc:rpc-error) (error reply))
                      (let ((facts (block-facts (hex->octets raw)))
                            (certificate (clempo.json:jget reply "certificate")))
                        (invariant (= height (first facts)) "RPC returned wrong block height")
                        (invariant (stringp certificate) "missing finalization at ~D" height)
                        (push (append facts (list certificate)) blocks))))
    (let ((result (clempo.consensus:certify-blocks verifier (nreverse blocks))))
      (invariant (octets= (clempo.consensus:certification-last-hash result)
                          (hex->octets (clempo.json:jget head "hash"))) "finalized head hash differs")
      (clempo.json:jobject "status" "verified" "from" from "to" to
                          "certificates" (clempo.consensus:certification-certificates result)
                          "anchorFromEpoch" +readiness-moderato-from-epoch+
                          "anchor" (octets->hex +readiness-moderato-identity+)))))

(defun readiness-peer (seed tag timeout)
  "Try exactly one live registry IPv4 ingress with a fresh, unregistered identity.
No application data, votes or dealings are sent. Failure is an observation, not
proof of a membership rejection (TCP, IP policy and admission can look alike)."
  (let* ((validators (first (clempo.abi:abi-decode
                             '((:array (:tuple (:bytes-n 32) :address :string :string
                                        :address (:uint 64) (:uint 64) (:uint 64))))
                             (readiness-call (readiness-calldata "getActiveValidators()" nil) tag)
                             :strict t)))
         (public (octets->integer (clempo.ed25519:ed25519-public-key seed)))
         (peer (find-if (lambda (v)
                          (let* ((endpoint (third v)) (colon (position #\: endpoint)))
                            (and (zerop (eighth v)) colon
                                 (null (position #\: endpoint :start (1+ colon)))
                                 (clempo.discovery:public-ip-p (subseq endpoint 0 colon))
                                 (let ((port (parse-integer endpoint :start (1+ colon) :junk-allowed t)))
                                   (and (typep port '(integer 1 65535))
                                        (every (lambda (c) (digit-char-p c 10))
                                               (subseq endpoint (1+ colon))))))))
                        validators)))
    (invariant (not (find public validators :key #'first)) "ephemeral identity is already active")
    (invariant peer "no supported IPv4 consensus ingress in live registry")
    (let* ((endpoint (third peer)) (colon (position #\: endpoint))
           (host (subseq endpoint 0 colon)) (port (parse-integer endpoint :start (1+ colon)))
           (stage "tcp-connect") (session nil)
           (stream nil))
      (flet ((report (status &optional detail)
               (clempo.json:jobject "status" status "endpoint" endpoint "stage" stage
                                   "peerPublicKey" (octets->hex (integer->octets (first peer) 32))
                                   "detail" (or detail "") "joined" :false)))
        (unwind-protect
             (handler-case
                 (sb-ext:with-timeout timeout
                   (setf stream (clempo.p2p-wire:connect-tcp host port :timeout timeout)
                         stage "sake-authentication"
                         session (clempo.cwp2p:dial stream seed (integer->octets (first peer) 32)))
                   (setf stage "authenticated-transport")
                   (report "authenticated-not-joined"))
               (sb-ext:timeout () (report "timeout" "No admission conclusion follows from timeout."))
               (error (c) (report "connection-failed" (princ-to-string c))))
          (when session (ignore-errors (clempo.cwp2p:close-session session)))
          (when stream (ignore-errors (close stream))))))))

(defcommand "moderato-readiness" (args)
  "read-only R6f: [-rpc URL] [-last 8] [-timeout 10]; ephemeral keys, owner eth_call, trusted certificates, one consensus dial; never submits"
  (let* ((clempo.jsonrpc:*rpc-url* (arg args :rpc clempo.jsonrpc:+moderato-rpc+))
         (spec (clempo.chainspec:load-chainspec :moderato))
         (count (arg-int args :last 8)) (timeout (arg-int args :timeout 10))
         (seed (clempo.io:random-octets 32))
         (validator (octets->integer (clempo.io:random-octets 20)))
         (ingress "192.0.2.1:8000") (egress "192.0.2.1")
         (report (clempo.json:jobject "chain" "moderato" "chainId" 42431
                                    "transactionsSent" 0 "joined" :false "validatorReady" :false
                                    "scope" "Disposable registration dry run; not execution sync, DKG or membership proof."))
         (failed nil))
    (unwind-protect
         (handler-case
             (progn
               (invariant (and (<= 1 count 3000) (<= 1 timeout 60)) "invalid count or timeout")
               (invariant (= (clempo.chainspec:chain-id spec) 42431
                             (hex->integer (clempo.jsonrpc:rpc-call "eth_chainId" nil)))
                          "RPC is not Moderato")
               (let* ((tag (clempo.jsonrpc:rpc-call "eth_blockNumber" nil))
                      (owner (first (clempo.abi:abi-decode
                                     '(:address)
                                     (readiness-call (readiness-calldata "owner()" nil) tag) :strict t))))
                 (invariant (plusp owner) "registry owner is zero")
                 (multiple-value-bind (public proof data)
                     (readiness-registration seed validator ingress egress validator)
                   (setf (readiness-field "registration" report)
                         (clempo.json:jobject "ephemeral" :true "privateKeyRetained" :false
                                             "validator" (octets->hex (integer->octets validator 20))
                                             "feeRecipient" (octets->hex (integer->octets validator 20))
                                             "publicKey" (octets->hex public) "signature" (octets->hex proof)
                                             "ingress" ingress "egress" egress "calldata" (octets->hex data)
                                             "owner" (octets->hex (integer->octets owner 20)) "block" tag))
                   (handler-case
                       (setf (readiness-field "simulation" report)
                             (clempo.json:jobject "status" "eth_call-succeeded"
                                                 "validatorIndex" (first (clempo.abi:abi-decode
                                                   '((:uint 64))
                                                   (readiness-call data tag (octets->hex (integer->octets owner 20)))
                                                   :strict t))))
                     (error (c) (setf failed t (readiness-field "simulation" report) (princ-to-string c)))))
                 (handler-case (setf (readiness-field "certificates" report) (readiness-certificates spec count))
                   (error (c) (setf failed t (readiness-field "certificates" report) (princ-to-string c))))
                 (handler-case (setf (readiness-field "consensusPeer" report) (readiness-peer seed tag timeout))
                   (error (c) (setf failed t (readiness-field "consensusPeer" report) (princ-to-string c)))))
               (write-line (clempo.json:to-json-string report))
               (if failed 1 0))
           (error (c)
             (setf (readiness-field "error" report) (princ-to-string c))
             (write-line (clempo.json:to-json-string report))
             1))
      (fill seed 0))))
