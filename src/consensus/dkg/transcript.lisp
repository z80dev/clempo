;;; commonware's Transcript (cryptography/src/transcript.rs): a BLAKE3 hash over framed
;;; packets, forked and resumed from 32-byte summaries, signed with ed25519.
;;;
;;; A transcript hashes [tag] || packet || frame(varint(length)) || ...; a resumed, forked or
;;; noise transcript is keyed with the summary it continues from. BLAKE3 here is one-shot, so a
;;; transcript keeps the bytes it has absorbed and hashes them when summarized.

(in-package #:clempo.dkg)

(defconstant +start-new+ 0)
(defconstant +start-resume+ 1)
(defconstant +start-fork+ 2)
(defconstant +start-noise+ 3)

(defstruct (transcript (:constructor %make-transcript (key version)))
  (key nil :type (or null octets))
  (version :v0 :type (member :v0 :v1))
  (bytes (make-array 64 :element-type 'octet :adjustable t :fill-pointer 0))
  (pending 0 :type unsigned-byte))

(defun absorb (transcript octets)
  (let ((buffer (transcript-bytes transcript)))
    (loop for b across octets do (vector-push-extend b buffer (max 64 (length buffer))))))

(defun frame-length (pending version)
  "V0 frames a packet with varint(length); V1 with the varint's bytes reversed."
  (let ((v (varint-octets pending)))
    (if (eq version :v1) (reverse v) v)))

(defun transcript-start (tag summary version)
  (let ((tr (%make-transcript summary version)))
    (absorb tr (octets-of tag))
    tr))

(defun transcript-append (transcript octets)
  "Transcript::append: more bytes of the current packet."
  (absorb transcript octets)
  (incf (transcript-pending transcript) (length octets))
  transcript)

(defun transcript-commit (transcript octets)
  "Transcript::commit: OCTETS end the current packet."
  (transcript-append transcript octets)
  (absorb transcript (frame-length (transcript-pending transcript)
                                   (transcript-version transcript)))
  (setf (transcript-pending transcript) 0)
  transcript)

(defun new-transcript (namespace &optional (version :v0))
  "Transcript::new: a fresh transcript whose first packet is NAMESPACE."
  (transcript-commit (transcript-start +start-new+ nil version) namespace))

(defun resume-transcript (summary &optional (version :v0))
  "Transcript::resume: continue from SUMMARY (a different history than never summarizing)."
  (transcript-start +start-resume+ summary version))

(defun transcript-hash (transcript &key (length 32))
  (let* ((tail (if (zerop (transcript-pending transcript))
                   (make-octets 0)
                   (frame-length (transcript-pending transcript)
                                 (transcript-version transcript))))
         (data (octets-concat (coerce (transcript-bytes transcript) 'octets) tail))
         (key (transcript-key transcript)))
    (if key
        (clempo.blake3:blake3-keyed key data :length length)
        (clempo.blake3:blake3 data :length length))))

(defun transcript-summarize (transcript)
  "Transcript::summarize: the 32-byte summary of everything committed."
  (transcript-hash transcript))

(defun transcript-fork (transcript label)
  "Transcript::fork: a transcript keyed with this one's summary, first packet LABEL."
  (transcript-commit (transcript-start +start-fork+ (transcript-summarize transcript)
                                       (transcript-version transcript))
                     label))

(defun transcript-noise (transcript label)
  "Transcript::noise: a function of N returning the next N bytes of the BLAKE3 XOF stream
keyed with this transcript's summary and labelled LABEL."
  (let ((source (transcript-commit (transcript-start +start-noise+
                                                     (transcript-summarize transcript)
                                                     (transcript-version transcript))
                                   label))
        (stream (make-octets 0))
        (position 0))
    (lambda (n)
      (when (> (+ position n) (length stream))
        (setf stream (transcript-hash source :length (max 64 (* 2 (+ position n))))))
      (prog1 (subseq stream position (+ position n))
        (incf position n)))))

;;; Signing: Summary::sign uses the empty namespace, i.e. ed25519 over union_unique("", s).

(defun summary-message (summary)
  (union-unique (make-octets 0) summary))

(defun sign-summary (seed summary)
  (clempo.ed25519:ed25519-sign seed (summary-message summary)))

(defun verify-summary (public-key summary signature)
  (clempo.ed25519:ed25519-verify public-key (summary-message summary) signature))

(defun transcript-sign (transcript seed)
  (sign-summary seed (transcript-summarize transcript)))

(defun transcript-verify (transcript public-key signature)
  (verify-summary public-key (transcript-summarize transcript) signature))
