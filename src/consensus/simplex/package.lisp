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
  (voter-receive voter sender message)   -> effects; MESSAGE a vote (notarize, nullify,
     finalize) or certificate (notarization, nullification, finalization); SENDER the
     participant index or ed25519 key (NIL allowed for certificates)
  (voter-tick voter)                     -> effects; fires timeouts due at now-ms
  (voter-next-deadline voter)            -> ms or NIL
  (voter-view voter), (voter-last-finalized voter), (voter-leader-of voter view)
  (voter-certificate voter view)         -> the certificate to answer a peer's request for VIEW

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
   #:voter-certificate))
