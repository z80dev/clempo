(defpackage #:clempo.consensus
  (:use #:cl #:clempo.base #:clempo.cwcodec)
  (:local-nicknames (#:bls #:clempo.bls12-381))
  (:documentation "Tempo's simplex consensus artifacts (commonware-consensus 2026.10.1 simplex
with the bls12381_threshold::vrf scheme over MinSig, namespace \"TEMPO\"): the wire types,
the threshold signing scheme, and the finalization-certificate verifier with its epoch
identities.

Signatures are G1 points and public keys G2 points, both carried as compressed octets
\(48 and 96 bytes); a vote or certificate signature is a pair (vote, seed).")
  (:export
   ;; wire
   #:+namespace+ #:+signature-size+ #:+public-key-size+ #:+digest-size+
   #:view-round #:make-round #:round-epoch #:round-view #:round=
   #:proposal #:make-proposal #:proposal-round #:proposal-parent #:proposal-payload
   #:proposal-epoch #:proposal-view
   #:vrf-signature #:make-vrf-signature #:vrf-signature-vote #:vrf-signature-seed
   #:attestation #:make-attestation #:attestation-signer #:attestation-signature
   #:notarize #:make-notarize #:notarize-proposal #:notarize-attestation
   #:nullify #:make-nullify #:nullify-round #:nullify-attestation
   #:finalize #:make-finalize #:finalize-proposal #:finalize-attestation
   #:notarization #:make-notarization #:notarization-proposal #:notarization-certificate
   #:nullification #:make-nullification #:nullification-round #:nullification-certificate
   #:finalization #:make-finalization #:finalization-proposal #:finalization-certificate
   #:vote #:certificate
   #:vote-round #:vote-signer #:certificate-round
   #:encode-epoch-mux #:decode-epoch-mux
   ;; namespaces and subjects
   #:vote-namespace #:seed-namespace #:subject-namespace #:subject-message #:seed-message
   #:subject-of
   ;; scheme
   #:scheme #:make-signer-scheme #:make-verifier-scheme #:make-certificate-verifier
   #:scheme-identity #:scheme-namespace #:scheme-share #:scheme-total #:scheme-threshold
   #:scheme-me #:scheme-coefficients #:scheme-partial-public
   #:sign-subject #:verify-attestation #:assemble-certificate
   #:verify-certificate #:verify-certificates #:batch-scalar-source
   #:sign-vote #:verify-vote #:assemble-from-votes #:verify-artifact
   ;; certify
   #:fixed-epocher #:make-fixed-epocher #:epocher-length #:epoch-of-height
   #:epoch-first-height #:epoch-last-height #:boundary-height-p
   #:certificate-error #:certificate-error-reason
   #:finalization-verifier #:make-finalization-verifier
   #:verifier-register-epoch #:verifier-identity-for #:verifier-network-identity
   #:verifier-advance-network-identity #:verifier-register-boundary-outcome
   #:+mainnet-network-identity+ #:+mainnet-network-from-epoch+ #:+mainnet-epoch-length+
   #:decode-finalization #:check-finalization-block
   #:*batch-mapper* #:certify-blocks #:verify-batch
   #:certification #:certification-verified #:certification-certificates #:certification-first-height
   #:certification-last-height #:certification-last-hash #:certification-epochs
   #:certificate-error-height #:certificate-error-detail))

(in-package #:clempo.consensus)

(define-constant +namespace+ (string->octets "TEMPO") "Tempo's consensus namespace (config.rs).")
(defconstant +signature-size+ 48 "A MinSig signature: a compressed G1 point.")
(defconstant +public-key-size+ 96 "A MinSig public key: a compressed G2 point.")
(defconstant +digest-size+ 32 "Tempo's simplex Digest is the block hash.")

;;; Round = (epoch, view) as varints; Proposal = round, parent view, payload digest.

(defcodec (view-round (:conc-name round-) (:constructor make-round))
  (epoch :varint :type u64)
  (view :varint :type u64))

(defun round= (a b)
  (and (= (round-epoch a) (round-epoch b)) (= (round-view a) (round-view b))))

(defcodec proposal
  (round view-round)
  (parent :varint :doc "the view of the parent proposal")
  (payload (:array 32) :doc "the block hash"))

(defun proposal-epoch (p) (round-epoch (proposal-round p)))
(defun proposal-view (p) (round-view (proposal-round p)))

;;; bls12381_threshold::vrf::Signature: the vote signature then the seed signature, both
;;; G1. Kept as compressed octets (Rust reads them lazily too).

(defcodec vrf-signature
  (vote (:array 48))
  (seed (:array 48)))

(defcodec attestation
  (signer (:varint 32) :doc "the participant index")
  (signature vrf-signature))

;;; Votes.

(defcodec notarize (proposal proposal) (attestation attestation))
(defcodec nullify (round view-round) (attestation attestation))
(defcodec finalize (proposal proposal) (attestation attestation))

(defcwunion vote (0 notarize) (1 nullify) (2 finalize))

;;; Certificates: a recovered threshold signature pair.

(defcodec notarization (proposal proposal) (certificate vrf-signature))
(defcodec nullification (round view-round) (certificate vrf-signature))
(defcodec finalization (proposal proposal) (certificate vrf-signature))

(defcwunion certificate (0 notarization) (1 nullification) (2 finalization))

(defun vote-round (v)
  (etypecase v
    (notarize (proposal-round (notarize-proposal v)))
    (nullify (nullify-round v))
    (finalize (proposal-round (finalize-proposal v)))))

(defun vote-signer (v)
  (attestation-signer (etypecase v
                        (notarize (notarize-attestation v))
                        (nullify (nullify-attestation v))
                        (finalize (finalize-attestation v)))))

(defun certificate-round (c)
  (etypecase c
    (notarization (proposal-round (notarization-proposal c)))
    (nullification (nullification-round c))
    (finalization (proposal-round (finalization-proposal c)))))

;;; Tempo multiplexes the vote, certificate and resolver channels by epoch: a varint epoch
;;; then the message.

(defun encode-epoch-mux (epoch type value)
  (let ((w (make-cw-writer)))
    (write-varint w epoch)
    (cw-write type w value)
    (cw-writer-octets w)))

(defun decode-epoch-mux (type octets)
  "Returns (values value epoch)."
  (let* ((r (make-cw-reader octets))
         (epoch (read-varint r))
         (v (cw-read type r)))
    (when (plusp (cw-reader-remaining r))
      (cw-fail "~D extra bytes after ~S" (cw-reader-remaining r) type))
    (values v epoch)))

;;; Subjects (simplex/types.rs Subject, scheme/mod.rs Namespace). A subject is
;;; (:notarize proposal), (:nullify round) or (:finalize proposal). The vote signature signs
;;; the subject's message under its namespace; the seed signature signs the round under
;;; NAMESPACE_SEED, so notarize, nullify and finalize of one round share one seed.

(defun suffixed (namespace suffix)
  (namespace-union namespace (string->octets suffix)))

(defun vote-namespace (namespace kind)
  (suffixed namespace (ecase kind
                        (:notarize "_NOTARIZE")
                        (:nullify "_NULLIFY")
                        (:finalize "_FINALIZE"))))

(defun seed-namespace (namespace) (suffixed namespace "_SEED"))

(defun subject-namespace (namespace subject)
  (vote-namespace namespace (first subject)))

(defun subject-message (subject)
  (ecase (first subject)
    ((:notarize :finalize) (cw-encode 'proposal (second subject)))
    (:nullify (cw-encode 'view-round (second subject)))))

(defun subject-round (subject)
  (ecase (first subject)
    ((:notarize :finalize) (proposal-round (second subject)))
    (:nullify (second subject))))

(defun seed-message (subject)
  (cw-encode 'view-round (subject-round subject)))

(defun subject-of (artifact)
  "The subject a vote or certificate signs."
  (etypecase artifact
    (notarize (list :notarize (notarize-proposal artifact)))
    (nullify (list :nullify (nullify-round artifact)))
    (finalize (list :finalize (finalize-proposal artifact)))
    (notarization (list :notarize (notarization-proposal artifact)))
    (nullification (list :nullify (nullification-round artifact)))
    (finalization (list :finalize (finalization-proposal artifact)))))
