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

Rounds and transcripts:
  (make-round-info namespace round &key previous mode reveal dealers players) -> info
    Info::new::<N3f1>; INFO-SUMMARY is the round's transcript summary (= the output's summary).
  new-transcript resume-transcript transcript-commit transcript-fork transcript-summarize
  transcript-noise transcript-sign transcript-verify: commonware's BLAKE3 Transcript.

Protocol (signing keys are 32-byte ed25519 seeds; randomness from clempo.io:*entropy*):
  (start-dealer info seed &key share) -> (values dealer pub-msg ((player . scalar) ...))
  (dealer-receive-player-ack dealer player ack)   (finalize-dealer dealer) -> signed-dealer-log
  (make-player info seed)   (player-dealer-message player dealer pub-msg scalar) -> ack | NIL
  (check-signed-dealer-log signed info) -> (values dealer log) | NIL
  (make-logs info) (logs-record logs dealer log) (logs-pre-verify logs)
  (observe logs) -> output     (finalize-player player logs) -> (values output share)
  (deal players &key mode) -> (values output ((player . share) ...))   the trusted dealer

Tempo:
  (round-info-from-outcome outcome &key namespace v1-active) -> info     Round::from_state
  (extra-data-dealer-log octets info &key strict) -> (values dealer log)  read_dealer_log
  (collect-dealer-logs info extra-datas) -> logs
  (conclude-ceremony logs previous-output &key player) -> (values output share succeeded)
  (boundary-outcome output next-players &key epoch next-full-dkg-epoch) -> outcome
  height-epoch epoch-first-height epoch-last-height boundary-height-p

Malformed bytes signal CW-ERROR (a subclass of ERROR); refused protocol input signals
DKG-ERROR (reason keyword in DKG-ERROR-REASON); a round that cannot produce an output signals
DKG-FAILURE (a DKG-ERROR).")
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
   #:+tempo-namespace+ #:round-info-from-outcome
   ;; protocol
   #:dkg-failure #:dkg-failure-required #:dkg-failure-found #:dkg-failure-faults
   #:dkg-failure-unavailable
   #:check-signed-dealer-log #:logs #:make-logs #:logs-info #:logs-record #:logs-pre-verify #:observe
   #:dealer #:start-dealer #:dealer-receive-player-ack #:finalize-dealer
   #:player #:make-player #:player-dealer-message #:finalize-player #:player-index
   #:deal
   ;; Tempo integration
   #:height-epoch #:epoch-first-height #:epoch-last-height #:boundary-height-p
   #:extra-data-dealer-log #:collect-dealer-logs #:conclude-ceremony #:boundary-outcome))
