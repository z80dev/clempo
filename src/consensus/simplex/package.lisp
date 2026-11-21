(defpackage #:clempo.simplex
  (:use #:cl #:clempo.base #:clempo.cwcodec #:clempo.consensus)
  (:documentation "commonware simplex consensus (commonware-consensus 2026.10.1) as Tempo runs
it: rotating leaders elected by Random V0/V1 (or round robin), the voter with its batcher's
lazy vote verification and certificate assembly, and its resolver's certificate requests, as a
pure, deterministic state machine. Time is read only through clempo.io:now-ms and randomness
\(batch-verification weights) through clempo.io:*entropy*, so a simulator drives it by
rebinding clempo.io:*clock* and clempo.io:*entropy*.

Voter:
  (make-voter &key scheme epoch participants me application elector timeouts namespace genesis)
     SCHEME: a clempo.consensus signer scheme for participant ME, or a verifier scheme for an
     observer (ME NIL). PARTICIPANTS: ed25519 public keys (32 octets) in index order.
     ELECTOR: :RANDOM-V1 (default), :RANDOM-V0, :ROUND-ROBIN or an elector object.
     TIMEOUTS: plist overriding *TEMPO-TIMEOUTS* (:leader :certification :retry :skip
     :skip-budget :view-retention; ms and views). GENESIS: payload of view 0 (32 octets).
  (voter-start voter)                    -> effects; enters view 1
  (voter-receive voter sender message)   -> (values effects block): MESSAGE a vote (notarize,
     nullify, finalize) or certificate (notarization, nullification, finalization); SENDER
     the participant index or ed25519 key (NIL allowed for certificates). BLOCK is :BLOCK
     when commonware's batcher blocks SENDER for this message: a vote from a non-participant,
     signed by another participant, for another epoch or conflicting with the signer's earlier
     vote; a certificate that fails verification or is for another epoch.
  (voter-take-blocked voter)             -> keys of participants whose votes failed
     verification since the last call (the batcher blocks them)
  (voter-tick voter)                     -> effects; fires timeouts due at now-ms
  (voter-next-deadline voter)            -> ms or NIL
  (voter-view voter), (voter-last-finalized voter), (voter-leader-of voter view)
  (voter-certificate voter view)         -> the certificate to answer a peer's resolver
     request for VIEW (Rust's produce), or NIL (answer with an error frame)
  (voter-deliver voter view certificate) -> (values effects outcome): a resolver response to
     our request for VIEW; outcome :valid, :ignored or :invalid (penalize the peer)

Application (generic functions the simulator specializes on its application object):
  (app-propose app context)            -> payload (32 octets), :PENDING or NIL (no proposal)
  (app-verify app context payload)     -> T, NIL or :PENDING
  (app-certify app round payload)      -> T, NIL or :PENDING (default method: T)
  CONTEXT: context-round, context-view, context-epoch, context-leader (index),
  context-leader-key, context-parent = (parent-view . parent-payload).
  Pending answers: (voter-proposed voter round payload-or-nil), (voter-verified voter round
  payload ok), (voter-certified voter round payload ok), each returning effects.

Effects, in order:
  (:broadcast message)   a vote or certificate for every other participant
  (:notarized notarization) (:nullified nullification)
  (:finalized finalization)   in increasing view order, at most once per view (a
                              finalization implies its ancestors)
  (:request view)        a certificate (notarization, nullification or finalization) for VIEW
                         is missing; deliver one with voter-receive. Repeated with each nullify
                         retry while still missing.
  (:send recipients message) is reserved: this voter broadcasts everything.

Journal (commonware's voter journal; consensus touches no files, so it is a protocol):
  make-voter :journal J. Every vote the voter signs, certificate it records and certification
  verdict is (journal-append J epoch view octets) and made durable by (journal-sync J epoch)
  before any effect leaves the voter; voter-start replays (journal-replay J epoch) (octets in
  ascending view, append order within a view), so a restarted voter never votes against what
  it sent; (journal-prune J epoch min-view) drops what is no longer retained. Octets are
  commonware's simplex Artifact encoding (encode-artifact / decode-artifact).
  MEMORY-JOURNAL is the in-memory reference backend.

Wire: votes go on channel 0, certificates on channel 1, resolver frames on channel 2, each
behind Tempo's varint epoch mux (clempo.consensus encode-epoch-mux); every vote and
certificate effect is a broadcast to all participants.

Electors: (make-random-elector :v0|:v1), (make-round-robin-elector &key seed),
  (elect elector epoch view n seed) -> leader index; SEED is the seed signature of the
  certificate that unlocked VIEW (NIL for view 1).")
  (:export
   ;; electors
   #:elector #:make-random-elector #:make-round-robin-elector #:elect
   ;; application
   #:app-propose #:app-verify #:app-certify
   #:context #:context-round #:context-view #:context-epoch #:context-leader
   #:context-leader-key #:context-parent
   ;; voter
   #:*tempo-timeouts*
   #:voter #:make-voter #:voter-start #:voter-receive #:voter-tick #:voter-next-deadline
   #:voter-proposed #:voter-verified #:voter-certified
   #:voter-view #:voter-last-finalized #:voter-epoch #:voter-me #:voter-leader-of
   #:voter-certificate #:voter-deliver #:voter-journal #:voter-take-blocked
   ;; journal
   #:journal-append #:journal-sync #:journal-replay #:journal-prune
   #:memory-journal #:make-memory-journal #:memory-journal-drop-unsynced
   #:artifact #:encode-artifact #:decode-artifact #:artifact-view
   #:certification-record #:make-certification-record #:certification-record-round
   #:certification-record-ok))
