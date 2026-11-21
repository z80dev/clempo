(in-package #:clempo.consensus)

;;; Finalization certificates (tempo crates/consensus/src/finalization_verifier).
;;;
;;; A block at height H belongs to epoch H / L (commonware FixedEpocher, L = 21,600 on
;;; mainnet and moderato); the last block of an epoch is its boundary and carries in its
;;; extraData the DKG outcome that names the next epoch and its group public key. A
;;; certificate for a block of epoch E verifies against E's identity: one learned from a
;;; certified boundary block, or the network identity (a published key valid from its
;;; from_epoch until the next full DKG) as a fallback, as Rust's FinalizationVerifier does.

;;; Mainnet's published network identity (tempo mainnet-network-identity.json): the group
;;; key every epoch from 2042 verifies under until the next full DKG.

(define-constant +mainnet-network-identity+
  (hex->octets "8627d33753b0d90a8eae75363d6b393ab9204076545b3af6caf831903e4bedde4374f1106551b481fb5ea35e59d51c92165c963881c718216e3e4570a307c08885af55c274f19488f709afe021ca3d9e567138ab097860d9146e895407bea182"))
(defconstant +mainnet-network-from-epoch+ 2042)
(defconstant +mainnet-epoch-length+ 21600)

;;; Epochs.

(defstruct (fixed-epocher (:constructor make-fixed-epocher (length)))
  (length 21600 :type (integer 1)))

(defun epocher-length (e) (fixed-epocher-length e))
(defun epoch-of-height (e height) (floor height (fixed-epocher-length e)))
(defun epoch-first-height (e epoch) (* epoch (fixed-epocher-length e)))
(defun epoch-last-height (e epoch) (1- (* (1+ epoch) (fixed-epocher-length e))))
(defun boundary-height-p (e height)
  (= height (epoch-last-height e (epoch-of-height e height))))

;;; Failures carry a reason keyword and restarts where the caller can repair.

(define-condition certificate-error (error)
  ((reason :initarg :reason :reader certificate-error-reason)
   (height :initarg :height :initform nil :reader certificate-error-height)
   (detail :initarg :detail :initform nil :reader certificate-error-detail))
  (:report (lambda (c s)
             (format s "finalization~@[ for block ~D~]: ~(~A~)~@[ (~A)~]"
                     (certificate-error-height c) (certificate-error-reason c)
                     (certificate-error-detail c)))))

(defun certificate-fail (reason height &optional fmt &rest args)
  (error 'certificate-error :reason reason :height height
                            :detail (when fmt (apply #'format nil fmt args))))

;;; The verifier: identities by epoch plus the network identity fallback.

(defstruct (finalization-verifier (:constructor %make-finalization-verifier))
  (epocher (make-fixed-epocher 21600) :type fixed-epocher)
  (namespace +namespace+ :type octets)
  (network-from-epoch 0 :type (integer 0))
  (network-scheme nil)
  (schemes (make-hash-table) :type hash-table))  ; epoch -> scheme

(defun make-finalization-verifier (&key (epoch-length 21600) network-identity
                                        (network-from-epoch 0) (namespace +namespace+))
  "NETWORK-IDENTITY (96 octets, valid from NETWORK-FROM-EPOCH) is the fallback for epochs
with no registered boundary; NIL means only registered epochs verify."
  (%make-finalization-verifier
   :epocher (make-fixed-epocher epoch-length) :namespace namespace
   :network-from-epoch network-from-epoch
   :network-scheme (when network-identity
                     (make-certificate-verifier network-identity :namespace namespace))))

(defun verifier-network-identity (v)
  (let ((s (finalization-verifier-network-scheme v)))
    (and s (values (scheme-identity s) (finalization-verifier-network-from-epoch v)))))

(defun verifier-register-epoch (v epoch identity-or-scheme)
  "Install EPOCH's identity (96 octets or a scheme)."
  (setf (gethash epoch (finalization-verifier-schemes v))
        (if (scheme-p identity-or-scheme)
            identity-or-scheme
            (make-certificate-verifier identity-or-scheme
                                       :namespace (finalization-verifier-namespace v)))))

(defun verifier-register-boundary-outcome (v extra-data)
  "Decode the DKG outcome in a certified boundary block's EXTRA-DATA (Rust reads it without
requiring the whole buffer) and install the next epoch's identity. Returns the outcome."
  (let ((outcome (clempo.dkg:decode-onchain-dkg-outcome extra-data)))
    (verifier-register-epoch v (clempo.dkg:outcome-epoch outcome)
                             (clempo.dkg:outcome-network-identity outcome))
    outcome))

(defun verifier-advance-network-identity (v identity from-epoch)
  "Retire the fallback for a newer, different identity (Rust advance_network_identity)."
  (multiple-value-bind (current current-from) (verifier-network-identity v)
    (when (and (or (null current) (> from-epoch current-from))
               (not (and current (octets= current identity))))
      (setf (finalization-verifier-network-scheme v)
            (make-certificate-verifier identity :namespace (finalization-verifier-namespace v))
            (finalization-verifier-network-from-epoch v) from-epoch)
      t)))

(defun verifier-identity-for (v epoch &optional height)
  "The scheme that verifies EPOCH's certificates, or a CERTIFICATE-ERROR."
  (or (gethash epoch (finalization-verifier-schemes v))
      (let ((s (finalization-verifier-network-scheme v)))
        (and s (>= epoch (finalization-verifier-network-from-epoch v)) s))
      (certificate-fail :identity-unavailable height "epoch ~D has no identity" epoch)))

;;; Decoding what consensus_getFinalization returns.

(defun decode-finalization (certificate)
  "A finalization from its codec bytes (octets or a hex string, as the RPC's certificate)."
  (cw-decode 'finalization (if (stringp certificate) (hex->octets certificate) certificate)))

(defun check-finalization-block (v finalization height block-hash)
  "Rust decode_and_verify's structural checks: the certificate finalizes this block hash
and its epoch is the one containing HEIGHT."
  (let ((p (finalization-proposal finalization)))
    (unless (octets= (proposal-payload p) block-hash)
      (certificate-fail :block-digest-mismatch height "certifies ~A, block is ~A"
                        (octets->hex (proposal-payload p)) (octets->hex block-hash)))
    (let ((expected (epoch-of-height (finalization-verifier-epocher v) height)))
      (unless (= expected (proposal-epoch p))
        (certificate-fail :epoch-mismatch height "certificate epoch ~D, block epoch ~D"
                          (proposal-epoch p) expected))))
  finalization)

;;; Certifying a run of blocks. Blocks come in height order as
;;;   (height hash parent-hash extra-data certificate)
;;; Each must link to the one before, and its certificate must finalize it in its epoch. A
;;; block may come without a certificate when a later block of the same epoch has one: the
;;; parent links make that block final too (simplex finalizes a block's ancestors). Epoch
;;; boundaries and the last block of the run must carry their own.
;;; A boundary block's DKG outcome installs the next epoch's identity; that identity is
;;; trusted only because the boundary block's own certificate is in the same run, verified
;;; under the epoch it closes. Signature checks are grouped into batches (one random linear
;;; combination each) and mapped with *BATCH-MAPPER*, which the dev layer rebinds to run
;;; batches on threads; the run is accepted only if every batch verifies, so the order the
;;; batches finish in does not matter.

(defvar *batch-mapper* #'mapcar
  "How certificate batches are mapped: (funcall *batch-mapper* function batches).")

(defstruct (certification (:constructor %make-certification))
  (verified 0 :type (integer 0))      ; blocks certified, directly or through a descendant
  (certificates 0 :type (integer 0))  ; certificates verified
  (first-height nil)
  (last-height nil)
  (last-hash nil)
  (epochs '()))  ; (epoch identity-hex count), oldest first

(defun verify-batch (batch)
  "BATCH = (scheme epoch . items); verify, bisecting to the first bad height on failure.
Returns NIL or the CERTIFICATE-ERROR it found (so a parallel mapper never unwinds)."
  (destructuring-bind (scheme epoch . items) batch
    (labels ((ok (items)
               (verify-certificates scheme
                                    (mapcar (lambda (i) (cons (subject-of (cdr i))
                                                              (finalization-certificate (cdr i))))
                                            items)))
             (bisect (items)
               (cond ((ok items) nil)
                     ((null (rest items))
                      (make-condition 'certificate-error
                                      :reason :invalid :height (car (first items))
                                      :detail (format nil "signature does not verify under epoch ~D's identity ~A"
                                                      epoch (octets->hex (scheme-identity scheme) :end 8))))
                     (t (let ((half (floor (length items) 2)))
                          (or (bisect (subseq items 0 half))
                              (bisect (nthcdr half items))))))))
      (bisect items))))

(defun certify-blocks (v blocks &key (batch 1000) previous-hash)
  "Certify BLOCKS (see above) with verifier V, installing identities from boundary blocks.
PREVIOUS-HASH, when given, is the hash the first block's parent must have. Signals
CERTIFICATE-ERROR at the lowest failing height; returns a CERTIFICATION."
  (let* ((e (finalization-verifier-epocher v))
         (result (%make-certification :last-hash previous-hash))
         (batches '())
         (uncovered '())
         (current '()) (current-epoch nil) (current-scheme nil))
    (flet ((close-batch ()
             (when current
               (push (list* current-scheme current-epoch (nreverse current)) batches)
               (setf current '()))))
      (loop for (height hash parent extra certificate) in blocks
            do (let ((last (certification-last-height result)))
                 (when (and last (/= height (1+ last)))
                   (certificate-fail :gap height "after ~D" last))
                 (when (and (certification-last-hash result)
                            (not (octets= parent (certification-last-hash result))))
                   (certificate-fail :parent-mismatch height "parent ~A" (octets->hex parent))))
               (cond
                 ((null certificate)
                  ;; covered by the next certified block of its epoch through the parent links
                  (when (and uncovered (/= (epoch-of-height e (first uncovered))
                                           (epoch-of-height e height)))
                    (certificate-fail :uncertified (first uncovered) "no certificate in its epoch covers it"))
                  (push height uncovered))
                 (t
                  (let* ((f (check-finalization-block v (decode-finalization certificate) height hash))
                         (epoch (proposal-epoch (finalization-proposal f))))
                    (when (and uncovered (/= (epoch-of-height e (first uncovered)) epoch))
                      (certificate-fail :uncertified (first uncovered) "no certificate in its epoch covers it"))
                    (setf uncovered '())
                    (unless (eql epoch current-epoch)
                      (close-batch)
                      (setf current-epoch epoch
                            current-scheme (verifier-identity-for v epoch height))
                      (push (list epoch (octets->hex (scheme-identity current-scheme)) 0)
                            (certification-epochs result)))
                    (push (cons height f) current)
                    (incf (certification-certificates result))
                    (incf (third (first (certification-epochs result))))
                    (when (>= (length current) batch) (close-batch)))))
               (unless (certification-first-height result)
                 (setf (certification-first-height result) height))
               (setf (certification-last-height result) height
                     (certification-last-hash result) hash)
               (when (boundary-height-p e height)
                 (when uncovered
                   (certificate-fail :uncertified (first uncovered) "the epoch ends uncertified"))
                 (close-batch)
                 (handler-case (verifier-register-boundary-outcome v extra)
                   (cw-error (c)
                     (certificate-fail :bad-dkg-outcome height "~A" c)))))
      (when uncovered
        (certificate-fail :uncertified (first uncovered) "no later certificate covers it"))
      (close-batch))
    (let ((failures (remove nil (funcall *batch-mapper* #'verify-batch (reverse batches)))))
      (when failures
        (error (first (sort failures #'< :key #'certificate-error-height)))))
    (setf (certification-verified result) (length blocks)
          (certification-epochs result) (reverse (certification-epochs result)))
    result))
