(defpackage #:clempo.dkg
  (:use #:cl #:clempo.base #:clempo.cwcodec)
  (:documentation "commonware's Feldman/Desmedt DKG and reshare for BLS12-381 MinSig
\(commonware-cryptography 2026.10.1, bls12381/dkg/feldman_desmedt.rs) and Tempo's on-chain DKG
artifacts (crates/dkg-onchain-artifacts, consensus/src/dkg), byte-compatible with both.

Values at the API boundary:
  G2 points (public polynomial coefficients, partial public keys, the network identity)
    are 96-byte compressed octet vectors;
  scalars (shares, dealings) are integers mod the BLS12-381 group order;
  ed25519 public keys are 32-byte octet vectors, signatures 64-byte octet vectors;
  sets of public keys (dealers, players, revealed) are lists sorted by octets;
  a transcript summary is a 32-byte octet vector.

Wire types (CW-ENCODE / CW-DECODE names, DEFCODEC structs):
  SHARING        mode, total (u32), coefficients (vec of G2)
  OUTPUT         summary, public SHARING, dealers, players, revealed
  ONCHAIN-DKG-OUTCOME  varint epoch, OUTPUT, next players, is-next-full-dkg
  DEALER-PUB-MSG, DEALER-PRIV-MSG (scalar), PLAYER-ACK (signature), ACK-OR-REVEAL,
  DEALER-RESULT, DEALER-LOG, SIGNED-DEALER-LOG, SHARE, DKG-MESSAGE (Tempo's DKG channel).
Reads take their Rust read configuration from special variables: *MAX-PARTICIPANTS*
\(the participant bound) and *MAX-MODE-VERSION* (0: NonZeroCounter only, 1: also RootsOfUnity).

Stable API for consensus:
  (decode-onchain-dkg-outcome octets &key strict) -> outcome   [strict: no trailing bytes]
  (encode-onchain-dkg-outcome outcome) -> octets
  outcome-epoch outcome-output outcome-next-players outcome-is-next-full-dkg
  outcome-network-identity outcome-sharing outcome-players outcome-dealers
  sharing-coefficients sharing-total sharing-mode sharing-threshold
  (sharing-public sharing) -> G2 octets, (sharing-partial-public sharing index) -> G2 octets

Malformed bytes signal CW-ERROR (a subclass of ERROR); protocol failures signal DKG-ERROR.")
  (:export
   ;; conditions and read configuration
   #:dkg-error #:dkg-error-reason
   #:*max-participants* #:*max-mode-version* #:+max-validators+
   ;; constants
   #:+group-order+ #:+dkg-namespace+
   ;; scalars and sharing modes
   #:mode-scalar
   ;; sharing
   #:sharing #:make-sharing #:sharing-p #:sharing-mode #:sharing-total #:sharing-coefficients
   #:sharing-threshold #:sharing-public #:sharing-partial-public
   ;; output
   #:output #:make-output #:output-p #:output-summary #:output-public #:output-dealers
   #:output-players #:output-revealed #:output-quorum
   ;; on-chain outcome
   #:onchain-dkg-outcome #:make-onchain-dkg-outcome #:onchain-dkg-outcome-p
   #:outcome-epoch #:outcome-output #:outcome-next-players #:outcome-is-next-full-dkg
   #:outcome-network-identity #:outcome-sharing #:outcome-players #:outcome-dealers
   #:decode-onchain-dkg-outcome #:encode-onchain-dkg-outcome
   ;; messages and logs
   #:dealer-pub-msg #:make-dealer-pub-msg #:dealer-pub-msg-commitment
   #:dealer-priv-msg #:player-ack #:ack-or-reveal #:dealer-result
   #:dealer-log #:make-dealer-log #:dealer-log-p #:dealer-log-pub-msg #:dealer-log-results
   #:signed-dealer-log #:make-signed-dealer-log #:signed-dealer-log-p
   #:signed-dealer-log-dealer #:signed-dealer-log-log #:signed-dealer-log-sig
   #:decode-signed-dealer-log
   #:share #:make-share #:share-p #:share-index #:share-private
   #:dkg-message #:summary #:ed25519-public-key #:ed25519-signature #:g2-point #:scalar
   ;; transcripts (commonware transcript.rs)
   #:transcript #:new-transcript #:resume-transcript #:transcript-commit #:transcript-append
   #:transcript-fork #:transcript-noise #:transcript-summarize #:transcript-sign
   #:transcript-verify #:sign-summary #:verify-summary
   ;; round info
   #:info #:make-round-info #:info-summary #:info-round #:info-previous #:info-mode
   #:info-reveal #:info-dealers #:info-players #:info=
   ;; Tempo
   #:+tempo-namespace+ #:round-info-from-outcome))
