(in-package #:clempo.dkg.tests)

(defun tempo-namespace () (string->octets "TEMPO"))

(deftest dkg-transcript-framing ()
  ;; transcript.rs docs: under V0, 127 zeros then [0x80] collide with 127 zeros || 0x7f
  ;; committed at once; V1's reversed length framing separates them.
  (flet ((split (version)
           (let ((tr (clempo.dkg::new-transcript (make-octets 0) version)))
             (clempo.dkg::transcript-commit tr (make-octets 127))
             (clempo.dkg::transcript-commit tr (octets-of #x80))
             (clempo.dkg::transcript-summarize tr)))
         (merged (version)
           (let ((tr (clempo.dkg::new-transcript (make-octets 0) version))
                 (data (make-octets 128)))
             (setf (aref data 127) #x7f)
             (clempo.dkg::transcript-commit tr data)
             (clempo.dkg::transcript-summarize tr))))
    (is (octets= (split :v0) (merged :v0)))
    (is (not (octets= (split :v1) (merged :v1)))))
  ;; append then commit is one packet; resume and fork change the summary
  (let* ((a (clempo.dkg::new-transcript (string->octets "test")))
         (b (clempo.dkg::new-transcript (string->octets "test"))))
    (clempo.dkg::transcript-append a (string->octets "A"))
    (clempo.dkg::transcript-commit a (string->octets "B"))
    (clempo.dkg::transcript-commit b (string->octets "AB"))
    (is (octets= (clempo.dkg::transcript-summarize a) (clempo.dkg::transcript-summarize b)))
    (let ((s (clempo.dkg::transcript-summarize a)))
      (is (not (octets= s (clempo.dkg::transcript-summarize
                           (clempo.dkg::resume-transcript s)))))
      (is (not (octets= (clempo.dkg::transcript-summarize
                         (clempo.dkg::transcript-fork a (string->octets "A")))
                        (clempo.dkg::transcript-summarize
                         (clempo.dkg::transcript-fork a (string->octets "B")))))))))

(deftest dkg-modes-packet ()
  ;; commonware-codec mode.rs: modes![1, 1] = 81 01; Modes::new([1, 0, 2]) = 81 80 02
  (is= "8101" (hex (clempo.dkg::modes-packet 1 1)))
  (is= "818002" (hex (clempo.dkg::modes-packet 1 0 2)))
  (is= "8001" (hex (clempo.dkg::modes-packet 0 1)))
  (is (null (clempo.dkg::modes-packet 0 0))))

(deftest dkg-mainnet-epoch-2042-summary ()
  ;; The ceremony run during epoch 2041 (Round::from_state over the outcome in boundary block
  ;; 44085599, namespace "TEMPO", Mode::NonZeroCounter, Reveal::V1 since T12 was active at that
  ;; boundary's timestamp 1791970990 >= t12Time 1791900000) produced the output installed by
  ;; block 44107199, whose summary is the round's Info summary.
  (let* ((outcome (decode-onchain-dkg-outcome (data-octets "mainnet_block_44085599_extradata.hex")))
         (next (decode-onchain-dkg-outcome (data-octets "mainnet_block_44107199_extradata.hex")))
         (info (clempo.dkg::round-info-from-outcome outcome :v1-active t)))
    (is= 2041 (clempo.dkg::info-round info))
    (is (null (clempo.dkg::info-previous info)))   ; is_next_full_dkg: a fresh DKG
    (is= (hex (output-summary (outcome-output next))) (hex (clempo.dkg::info-summary info)))
    (is (equalp (outcome-players next) (clempo.dkg::info-players info)))
    ;; the dealers of 2042's output are the selected subset of the round's dealers
    (is (subsetp (outcome-dealers next) (clempo.dkg::info-dealers info) :test #'octets=))
    ;; Reveal::V0 binds a different transcript
    (is (not (octets= (output-summary (outcome-output next))
                      (clempo.dkg::info-summary
                       (clempo.dkg::round-info-from-outcome outcome
                                                            :v1-active nil)))))))

(deftest dkg-info-validation ()
  (let* ((outcome (decode-onchain-dkg-outcome (data-octets "mainnet_block_44107199_extradata.hex")))
         (output (outcome-output outcome))
         (players (output-players output)))
    ;; a reshare: dealers must be previous players, at least a previous quorum of them
    (is (clempo.dkg::make-round-info (tempo-namespace) 2042 :previous output
                                     :dealers players :players players))
    (signals dkg-error (clempo.dkg::make-round-info (tempo-namespace) 2042 :previous output
                                                    :dealers (subseq players 0 3)
                                                    :players players))
    (signals dkg-error (clempo.dkg::make-round-info
                        (tempo-namespace) 2042 :previous output
                        :dealers (cons (make-octets 32) (rest players)) :players players))
    (signals dkg-error (clempo.dkg::make-round-info (tempo-namespace) 1 :dealers '() :players players))
    (signals dkg-error (clempo.dkg::make-round-info (tempo-namespace) 1 :dealers (reverse players)
                                                    :players players))))
