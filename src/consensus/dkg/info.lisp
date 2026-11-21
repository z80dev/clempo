;;; A DKG round's Info (feldman_desmedt.rs Info::new): who deals, who plays, the previous
;;; output being reshared, and the transcript summary binding all of it.

(in-package #:clempo.dkg)

(define-constant +dkg-namespace+ (string->octets "_COMMONWARE_CRYPTOGRAPHY_BLS12381_DKG")
  "The transcript namespace of every Feldman/Desmedt DKG round.")

(defstruct (info (:constructor %make-info))
  (summary nil :type octets)
  (round 0 :type unsigned-byte)
  (previous nil :type (or null output))
  (mode :nonzero-counter :type (member :nonzero-counter :roots-of-unity))
  (reveal :v1 :type (member :v0 :v1))
  (dealers '() :type list)
  (players '() :type list))

(defun key-set-octets (keys)
  "Set<PublicKey>::encode."
  (let ((w (make-cw-writer)))
    (write-length w (length keys))
    (dolist (k keys) (write-ed25519-public-key w k))
    (cw-writer-octets w)))

(defun option-output-octets (output)
  "Option<Output>::encode."
  (if output
      (octets-concat (octets-of 1) (cw-encode 'output output))
      (octets-of 0)))

(defun modes-packet (&rest values)
  "commonware_codec::Modes::new: seven-bit values, continuation bits on all but the last,
trailing zeros dropped; NIL when every value is zero."
  (let ((last (position-if #'plusp values :from-end t)))
    (when last
      (apply #'octets-of (loop for v in values for i from 0 to last
                               collect (if (< i last) (logior v #x80) v))))))

(defun check-key-set (keys what)
  (loop for (a b) on keys
        while b
        unless (cw-order< a b)
          do (dkg-fail what "~(~A~) must be sorted and unique" what)))

(defun key-index (key keys)
  (position key keys :test #'octets=))

(defun make-round-info (namespace round &key previous (mode :nonzero-counter) (reveal :v1)
                                           dealers players)
  "Info::new::<N3f1>. NAMESPACE isolates applications (Tempo: \"TEMPO\"); ROUND is a counter
\(Tempo: the epoch); PREVIOUS is the output being reshared, or NIL for a fresh DKG; DEALERS
and PLAYERS are sorted lists of ed25519 public keys; REVEAL is :v0 or :v1."
  (check-key-set dealers :num-dealers)
  (check-key-set players :num-players)
  (unless (< 0 (length dealers) #xffffffff) (dkg-fail :num-dealers "~D" (length dealers)))
  (unless (< 0 (length players) #xffffffff) (dkg-fail :num-players "~D" (length players)))
  (when previous
    (let ((prev-players (output-players previous))
          (public (output-public previous)))
      (unless (and (= (sharing-total public) (length prev-players))
                   (= (sharing-threshold public) (quorum (length prev-players))))
        (dkg-fail :invalid-previous-output))
      (let ((unknown (find-if-not (lambda (d) (key-index d prev-players)) dealers)))
        (when unknown (dkg-fail :dealer-not-in-round "~A" (octets->hex unknown))))
      (when (< (length dealers) (quorum (length prev-players)))
        (dkg-fail :num-dealers "~D below the previous quorum" (length dealers)))))
  (let ((tr (new-transcript +dkg-namespace+ :v0)))
    (transcript-commit tr namespace)
    (transcript-commit tr (integer->octets round 8))
    (transcript-commit tr (option-output-octets previous))
    (transcript-commit tr (key-set-octets dealers))
    (transcript-commit tr (key-set-octets players))
    (let ((modes (modes-packet (ecase mode (:nonzero-counter 0) (:roots-of-unity 1))
                               (ecase reveal (:v0 0) (:v1 1)))))
      (when modes (transcript-commit tr modes)))
    (%make-info :summary (transcript-summarize tr) :round round :previous previous
                :mode mode :reveal reveal :dealers dealers :players players)))

(defun info= (a b) (octets= (info-summary a) (info-summary b)))

;;; Quantities derived from a round (all under the N3f1 fault model, as Tempo uses).

(defun info-degree (info) (1- (quorum (length (info-players info)))))

(defun info-required-commitments (info)
  (max (quorum (length (info-dealers info)))
       (let ((previous (info-previous info)))
         (if previous (output-quorum previous) 0))))

(defun info-max-reveals (info) (max-faults (length (info-players info))))

(defun info-reveal-threshold (info)
  (ecase (info-reveal info)
    (:v0 (1+ (max-faults (length (info-players info)))))
    (:v1 (let ((faults (max-faults (length (let ((previous (info-previous info)))
                                             (if previous
                                                 (output-players previous)
                                                 (info-dealers info)))))))
           (invariant (> (info-required-commitments info) faults)
                      "a quorum must contain more participants than max_faults")
           (- (info-required-commitments info) faults)))))

(defun info-player-index (info player) (key-index player (info-players info)))
(defun info-dealer-index (info dealer) (key-index dealer (info-dealers info)))

(defun info-player-scalar (info player)
  (let ((index (info-player-index info player)))
    (and index (mode-scalar (info-mode info) (length (info-players info)) index))))
