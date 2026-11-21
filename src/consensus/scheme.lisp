(in-package #:clempo.consensus)

;;; The bls12381_threshold::vrf scheme (commonware-consensus simplex/scheme/bls12381_threshold
;;; /vrf.rs) over MinSig. Three roles, as in Rust:
;;;   signer               holds a share; signs, verifies partials, assembles certificates
;;;   verifier             holds the public polynomial; verifies partials and certificates
;;;   certificate verifier holds only the group public key (the epoch's identity)
;;; Every vote carries two partial signatures: the vote over the subject and the seed over
;;; the round. A certificate is the pair recovered by Lagrange interpolation, verified with
;;; one random linear combination against the identity.

(defstruct (scheme (:constructor %make-scheme))
  (role :certificate-verifier :type (member :signer :verifier :certificate-verifier))
  (namespace +namespace+ :type octets)
  (identity nil :type (or null octets))  ; the group public key, G2 compressed
  (coefficients '() :type list)  ; the public polynomial, constant first
  (total 0 :type (integer 0))
  (share nil)  ; (index . scalar) for a signer
  (me nil)  ; this participant's index, if any
  (partials nil :type (or null simple-vector)))  ; partial public keys by index

(defun scheme-threshold (scheme)
  "Signatures needed for a certificate: the polynomial's degree + 1 (the quorum n - f)."
  (length (scheme-coefficients scheme)))

(defun make-certificate-verifier (identity &key (namespace +namespace+))
  (invariant (= (length identity) +public-key-size+) "identity must be ~D bytes" +public-key-size+)
  (%make-scheme :role :certificate-verifier :namespace namespace :identity identity))

(defun quorum (n) (- n (floor (1- n) 3)))

(defun make-verifier-scheme (coefficients total &key (namespace +namespace+) me)
  "A verifier over the public polynomial COEFFICIENTS (compressed G2, constant first) for
TOTAL participants."
  (invariant (= (length coefficients) (quorum total))
             "polynomial threshold ~D must equal the quorum of ~D" (length coefficients) total)
  (%make-scheme :role :verifier :namespace namespace :identity (first coefficients)
                :coefficients coefficients :total total :me me
                :partials (make-array total :initial-element nil)))

(defun make-signer-scheme (coefficients total share &key (namespace +namespace+))
  "A signer holding SHARE = (index . scalar). Signals an invariant violation when the share
does not match the polynomial (Rust's Scheme::signer returns None)."
  (let ((s (make-verifier-scheme coefficients total :namespace namespace :me (car share))))
    (invariant (octets= (scheme-partial-public s (car share))
                        (bls:bls-public-key :min-sig (cdr share)))
               "share ~D does not match the public polynomial" (car share))
    (setf (scheme-role s) :signer (scheme-share s) share)
    s))

(defun scheme-partial-public (scheme index)
  "The partial public key of participant INDEX: the polynomial evaluated at index + 1."
  (let ((cache (scheme-partials scheme)))
    (invariant (and cache (< -1 index (length cache))) "no participant ~D" index)
    (or (svref cache index)
        (setf (svref cache index)
              (bls:bls-partial-public-key :min-sig (scheme-coefficients scheme) index
                                          (scheme-total scheme))))))

(defun batch-scalar-source ()
  "Independent 128-bit weights for random linear combinations, from clempo.io:*entropy*."
  (lambda () (octets->integer (clempo.io:random-octets 16))))

(defun subject-entries (scheme subject signature)
  "The two (namespace message signature) entries a VRF signature carries for SUBJECT."
  (let ((ns (scheme-namespace scheme)))
    (list (list (subject-namespace ns subject) (subject-message subject)
                (vrf-signature-vote signature))
          (list (seed-namespace ns) (seed-message subject) (vrf-signature-seed signature)))))

(defun sign-subject (scheme subject)
  "This signer's attestation over SUBJECT."
  (destructuring-bind (index . secret) (or (scheme-share scheme)
                                           (unreachable "scheme cannot sign"))
    (let ((ns (scheme-namespace scheme)))
      (make-attestation
       :signer index
       :signature (make-vrf-signature
                   :vote (bls:bls-sign :min-sig secret (subject-message subject)
                                       :namespace (subject-namespace ns subject))
                   :seed (bls:bls-sign :min-sig secret (seed-message subject)
                                       :namespace (seed-namespace ns)))))))

(defun verify-attestation (scheme subject attestation &key (scalar-source (batch-scalar-source)))
  (let ((signer (attestation-signer attestation)))
    (and (< signer (scheme-total scheme))
         (bls:bls-batch-verify-same-signer
          :min-sig (scheme-partial-public scheme signer)
          (subject-entries scheme subject (attestation-signature attestation))
          scalar-source))))

(defun assemble-certificate (scheme attestations)
  "Recover the certificate signature pair from at least threshold attestations with
distinct signers (unverified, as Rust's assemble)."
  (let ((threshold (scheme-threshold scheme)) (total (scheme-total scheme)))
    (invariant (>= (length attestations) threshold) "~D attestations, need ~D"
               (length attestations) threshold)
    (flet ((recover (key)
             (bls:bls-threshold-recover
              :min-sig threshold total
              (mapcar (lambda (a) (cons (attestation-signer a)
                                        (funcall key (attestation-signature a))))
                      attestations))))
      (make-vrf-signature :vote (recover #'vrf-signature-vote)
                          :seed (recover #'vrf-signature-seed)))))

(defun verify-certificate (scheme subject signature &key (scalar-source (batch-scalar-source)))
  "Verify a certificate's vote and seed signatures against the scheme's identity."
  (bls:bls-batch-verify-same-signer :min-sig (scheme-identity scheme)
                                    (subject-entries scheme subject signature)
                                    scalar-source))

(defun verify-certificates (scheme items &key (scalar-source (batch-scalar-source)))
  "Batch-verify ITEMS, a list of (subject . vrf-signature), in one random linear combination,
as Rust's verify_certificates: every vote signature is an entry, and each distinct seed
message is an entry once (two certificates of one round must carry the same seed)."
  (when (null items) (return-from verify-certificates t))
  (let ((ns (scheme-namespace scheme))
        (seeds (make-hash-table :test 'equalp))
        (entries '()))
    (loop for (subject . sig) in items
          do (push (list (subject-namespace ns subject) (subject-message subject)
                         (vrf-signature-vote sig))
                   entries)
             (let* ((msg (seed-message subject))
                    (previous (gethash msg seeds)))
               (cond ((null previous)
                      (setf (gethash msg seeds) (vrf-signature-seed sig))
                      (push (list (seed-namespace ns) msg (vrf-signature-seed sig)) entries))
                     ((not (octets= previous (vrf-signature-seed sig)))
                      (return-from verify-certificates nil)))))
    (bls:bls-batch-verify-same-signer :min-sig (scheme-identity scheme) (nreverse entries)
                                      scalar-source)))

;;; Votes and certificates as artifacts.

(defun sign-vote (scheme kind thing)
  "A signed vote: KIND :notarize or :finalize over proposal THING, or :nullify over round."
  (let ((a (sign-subject scheme (list kind thing))))
    (ecase kind
      (:notarize (make-notarize :proposal thing :attestation a))
      (:nullify (make-nullify :round thing :attestation a))
      (:finalize (make-finalize :proposal thing :attestation a)))))

(defun vote-attestation (vote)
  (etypecase vote
    (notarize (notarize-attestation vote))
    (nullify (nullify-attestation vote))
    (finalize (finalize-attestation vote))))

(defun verify-vote (scheme vote &rest keys)
  (apply #'verify-attestation scheme (subject-of vote) (vote-attestation vote) keys))

(defun assemble-from-votes (scheme votes)
  "The certificate recovered from VOTES, all of one kind over one subject."
  (let* ((first (first votes))
         (sig (assemble-certificate scheme (mapcar #'vote-attestation votes))))
    (etypecase first
      (notarize (make-notarization :proposal (notarize-proposal first) :certificate sig))
      (nullify (make-nullification :round (nullify-round first) :certificate sig))
      (finalize (make-finalization :proposal (finalize-proposal first) :certificate sig)))))

(defun certificate-signature (c)
  (etypecase c
    (notarization (notarization-certificate c))
    (nullification (nullification-certificate c))
    (finalization (finalization-certificate c))))

(defun verify-artifact (scheme artifact &rest keys)
  "Verify a vote (against its signer's partial key) or a certificate (against the identity)."
  (etypecase artifact
    ((or notarize nullify finalize) (apply #'verify-vote scheme artifact keys))
    ((or notarization nullification finalization)
     (apply #'verify-certificate scheme (subject-of artifact) (certificate-signature artifact)
            keys))))
