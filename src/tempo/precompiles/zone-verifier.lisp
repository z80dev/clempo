(in-package #:clempo.zone-verifier)

;;; TIP-1098, crates/precompiles/src/zone_verifier. This stateless view precompile
;;; has no storage slots, events, or contract-specific ABI errors. Invalid proofs
;;; return false; malformed ABI uses the shared dispatcher, and OOG still halts.
(defcontract zone-verifier (:address +zone-verifier-address+))

(defconstant +nitro-batch-type-hash+
  #xb6f39555cba9bf38842c669ea0c90bca6aad793881d75a0034e33352fbecb25e)

;; The parser structs are defined by later, alphabetically loaded components.
(declaim (notinline nitro-attestation-pcrs nitro-attestation-timestamp
                   nitro-attestation-user-data))

(defun batch-commitment (parent-chain-id &rest args)
  "EIP-712 hashStruct of NitroBatchAttestation, as 32 octets (not an EIP-712 domain hash).
ARGS are the twelve verify ABI arguments, in ABI order; proof is not part of the statement."
  (destructuring-bind (zone-id tempo-block-number anchor-block-number anchor-block-hash
                      expected-withdrawal-batch-index next-zone-height block-transition
                      deposit-queue-transition token-enablement-transition withdrawal-queue-hash
                      verifier-config proof)
      args
    (declare (ignore proof))
    (let ((bytes (make-octets (* 19 32))))
      (loop for value in (list +nitro-batch-type-hash+ parent-chain-id +zone-verifier-address+
                              zone-id tempo-block-number anchor-block-number anchor-block-hash
                              expected-withdrawal-batch-index next-zone-height
                              (first block-transition) (second block-transition)
                              (first deposit-queue-transition) (second deposit-queue-transition)
                              (third deposit-queue-transition) (fourth deposit-queue-transition)
                              (first token-enablement-transition) (second token-enablement-transition)
                              withdrawal-queue-hash (clempo.keccak:keccak256-integer verifier-config))
            for offset from 0 by 32
            do (loop for i below 32
                     do (setf (aref bytes (+ offset i)) (ldb (byte 8 (* 8 (- 31 i))) value))))
      ;; Rust uses alloy keccak256 directly, not the metered StorageCtx operation.
      (clempo.keccak:keccak256 bytes))))

(defun verify-with-policy (portal args root-der pcrs)
  "Verify with an explicitly supplied root and PCR triple (the Rust policy test seam).
Production dispatch always supplies the AWS pinned root and fork-approved measurements."
  (let ((zone-id (first args)) (config (nth 10 args)) (proof (nth 11 args)))
    (unless (= portal (clempo.zone-factory:portal-address zone-id))
      (return-from verify-with-policy nil))
    (unless (and (= (length config) 1) (= (aref config 0) 1) (plusp (length proof)))
      (return-from verify-with-policy
        (and (= (length config) 1) (= (aref config 0) 2) (zerop (length proof)))))
    (let* ((timestamp (min (block-timestamp) u64-max))
           (attestation (verify-attestation-with-root proof timestamp root-der)))
      (unless (and attestation pcrs) (return-from verify-with-policy nil))
      (loop for expected in pcrs for index from 0
            for actual = (aref (nitro-attestation-pcrs attestation) index)
            unless (and actual (octets= actual expected))
              do (return-from verify-with-policy nil))
      (unless (and (<= (nitro-attestation-timestamp attestation)
                       (min u64-max (+ (min u64-max (* timestamp 1000)) 300000)))
                   (= (length (nitro-attestation-user-data attestation)) 32))
        (return-from verify-with-policy nil))
      (octets= (nitro-attestation-user-data attestation)
               (apply #'batch-commitment (chain-id) args)))))

(defun verify (portal &rest args)
  "ZoneVerifier::verify. PORTAL is msg.sender; ARGS follow the exact IZoneVerifier ABI."
  (verify-with-policy portal args (aws-nitro-root) (approved-pcrs)))

(defcall zone-verifier verify*
    (zone-id tempo-block-number anchor-block-number anchor-block-hash
     expected-withdrawal-batch-index next-zone-height block-transition
     deposit-queue-transition token-enablement-transition withdrawal-queue-hash verifier-config proof)
    (:sol "verify(uint32,uint64,uint64,bytes32,uint64,uint256,(bytes32,bytes32),(bytes32,bytes32,uint64,uint64),(uint64,uint64),bytes32,bytes,bytes)"
     :returns "bool" :since :t13)
  (verify sender zone-id tempo-block-number anchor-block-number anchor-block-hash
          expected-withdrawal-batch-index next-zone-height block-transition
          deposit-queue-transition token-enablement-transition withdrawal-queue-hash verifier-config proof))

(defdispatch zone-verifier (self input sender)
  (charge-input-cost input)
  ;; Before ABI decoding, the exact selector with oversized calldata returns false.
  ;; 4-byte selector, 17 head words, config length + padded byte, proof length + proof.
  (if (and (> (length input) (+ 4 (* 17 32) (* 2 32) 32 24576))
           (= (octets->integer input :end 4) #xebb2ddc9))
      (make-octets 32)
      (dispatch-calls)))

(register-precompile +zone-verifier-address+ 'zone-verifier :t13)
