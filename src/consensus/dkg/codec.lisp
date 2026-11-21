;;; Wire types of the DKG and of Tempo's on-chain artifacts, declared with clempo.cwcodec.
;;; Rust's read configurations (max participants, mode version) are special variables.

(in-package #:clempo.dkg)

(define-condition dkg-error (error)
  ((reason :initarg :reason :reader dkg-error-reason)
   (detail :initarg :detail :initform nil :reader dkg-error-detail))
  (:report (lambda (c s)
             (format s "dkg: ~(~A~)~@[: ~A~]" (dkg-error-reason c) (dkg-error-detail c))))
  (:documentation "A DKG operation refused its input. REASON is a keyword naming the Rust
error variant (:player-not-in-round, :invalid-dealer-share, :insufficient-logs, ...)."))

(defun dkg-fail (reason &optional fmt &rest args)
  (error 'dkg-error :reason reason :detail (and fmt (apply #'format nil fmt args))))

(define-constant +group-order+
  #x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  "r, the order of the BLS12-381 groups; scalars live mod r.")

(define-constant +max-validators+ 65535
  "Tempo's MAX_VALIDATORS (u16::MAX): the participant bound of OnchainDkgOutcome.")

(defvar *max-participants* +max-validators+
  "The participant bound of Rust's read configuration (NonZeroU32 max_participants /
max_players): sharing totals, polynomial lengths and participant sets are checked against it.")

(defvar *max-mode-version* 0
  "Rust's ModeVersion for reading a Sharing's mode: 0 accepts NonZeroCounter only, 1 also
RootsOfUnity. Tempo reads with v0.")

;;; Leaves.

(defun read-fixed (r n) (read-raw r n))

(defun write-summary (w v) (write-raw w v))
(defun read-summary (r) (read-fixed r 32))
(defcwtype summary :write write-summary :read read-summary)

(defun write-ed25519-signature (w v)
  (invariant (= (length v) 64) "ed25519 signature must be 64 bytes")
  (write-raw w v))
(defun read-ed25519-signature (r) (read-fixed r 64))
(defcwtype ed25519-signature :write write-ed25519-signature :read read-ed25519-signature)
(defcwtype player-ack :write write-ed25519-signature :read read-ed25519-signature)

(defun write-ed25519-public-key (w v)
  (invariant (= (length v) 32) "ed25519 public key must be 32 bytes")
  (write-raw w v))
(defun read-ed25519-public-key (r) (read-fixed r 32))
(defcwtype ed25519-public-key :write write-ed25519-public-key :read read-ed25519-public-key)

(defun check-g2-octets (octets)
  "G2::read's checks on a compressed point: the compression flag is set and the point is
not the identity."
  (unless (logbitp 7 (aref octets 0)) (cw-fail "G2: bad encoding (not compressed)"))
  (when (logbitp 6 (aref octets 0)) (cw-fail "G2: infinity")))

(defun write-g2-point (w v)
  (invariant (= (length v) 96) "compressed G2 point must be 96 bytes")
  (write-raw w v))
(defun read-g2-point (r)
  (let ((octets (read-fixed r 96)))
    (check-g2-octets octets)
    octets))
(defcwtype g2-point :write write-g2-point :read read-g2-point)

(defun write-scalar (w v)
  (invariant (and (integerp v) (<= 0 v) (< v +group-order+)) "scalar out of range")
  (write-uint w v 32))
(defun read-scalar (r)
  "Scalar with ScalarReadCfg::AllowZero: 32 big-endian bytes below r."
  (let ((v (read-uint r 32)))
    (unless (< v +group-order+) (cw-fail "Scalar: not below the group order"))
    v))
(defun read-nonzero-scalar (r)
  "Scalar with ScalarReadCfg::RejectZero (private keys, shares, dealings)."
  (let ((v (read-scalar r)))
    (when (zerop v) (cw-fail "Scalar: zero"))
    v))
(defcwtype scalar :write write-scalar :read read-scalar)
(defcwtype dealer-priv-msg :write write-scalar :read read-nonzero-scalar)

;;; Sharing modes: how participants map to evaluation points.

(defun write-sharing-mode (w mode)
  (write-u8 w (ecase mode (:nonzero-counter 0) (:roots-of-unity 1))))
(defun read-sharing-mode (r)
  (let* ((tag (read-u8 r))
         (mode (case tag (0 :nonzero-counter) (1 :roots-of-unity)
                 (t (cw-fail "invalid sharing mode tag ~D" tag)))))
    (when (and (eq mode :roots-of-unity) (< *max-mode-version* 1))
      (cw-fail "Mode: unsupported mode for version ~D" *max-mode-version*))
    mode))
(defcwtype sharing-mode :write write-sharing-mode :read read-sharing-mode)

(defun write-sharing-total (w n) (write-uint w n 4))
(defun read-sharing-total (r)
  (let ((n (read-uint r 4)))
    (unless (<= 1 n *max-participants*) (cw-fail "Sharing: total ~D not in range" n))
    n))
(defcwtype sharing-total :write write-sharing-total :read read-sharing-total)

;;; Declared shapes, fields in wire order.

(defcodec sharing
  (mode sharing-mode :default :nonzero-counter)
  (total sharing-total)
  ;; the public polynomial, constant first, as compressed G2 octets
  (coefficients (:vec g2-point :min 1 :max *max-participants*)))

(defcodec output
  (summary summary)
  (public sharing)
  (dealers (:set ed25519-public-key :min 1 :max *max-participants*))
  (players (:set ed25519-public-key :min 1 :max *max-participants*))
  (revealed (:set ed25519-public-key :min 0 :max *max-participants*)))

(defcodec (onchain-dkg-outcome (:conc-name outcome-))
  (epoch :varint)
  (output output)
  (next-players (:set ed25519-public-key :min 1 :max +max-validators+))
  (is-next-full-dkg :bool))

(defcodec dealer-pub-msg
  (commitment (:vec g2-point :min 1 :max *max-participants*)))

(defcwunion ack-or-reveal
  (0 :ack player-ack)
  (1 :reveal dealer-priv-msg))

(defcwunion dealer-result
  (0 :ok (:map ed25519-public-key ack-or-reveal :min 0 :max *max-participants*))
  (1 :too-many-reveals))

(defcodec dealer-log
  (pub-msg dealer-pub-msg)
  ;; (:ok . ((player . (:ack . sig)) (player . (:reveal . scalar)) ...)) or :too-many-reveals
  (results dealer-result))

(defcodec signed-dealer-log
  (dealer ed25519-public-key)
  (log dealer-log)
  (sig ed25519-signature))

(defcodec share
  (index (:varint 32))
  (private dealer-priv-msg))

(defcwunion dkg-message
  ;; Tempo's DKG channel message (consensus/src/dkg/manager/actor/mod.rs Message).
  (0 :dealer (:tuple dealer-pub-msg dealer-priv-msg))
  (1 :ack player-ack))

;;; Sharing helpers that need no curve arithmetic.

(defun compressed-identity-p (octets)
  (and (= (aref octets 0) #xc0) (loop for i from 1 below (length octets)
                                      always (zerop (aref octets i)))))

(defun sharing-threshold (sharing)
  "Sharing::required: the exact degree of the public polynomial plus one, i.e. the number of
shares needed to recover the secret."
  (let ((coefficients (sharing-coefficients sharing)))
    (max 1 (- (length coefficients)
              (loop for c in (reverse coefficients)
                    while (compressed-identity-p c) count t)))))

(defun sharing-public (sharing)
  "The group public key: the constant coefficient."
  (first (sharing-coefficients sharing)))

(defun output-quorum (output)
  (quorum (length (output-players output))))

;;; Fault model N3f1.

(defun max-faults (n)
  (invariant (plusp n) "n must not be zero")
  (floor (1- n) 3))

(defun quorum (n) (- n (max-faults n)))

;;; Tempo's on-chain outcome.

(defun outcome-sharing (outcome) (output-public (outcome-output outcome)))
(defun outcome-players (outcome) (output-players (outcome-output outcome)))
(defun outcome-dealers (outcome) (output-dealers (outcome-output outcome)))
(defun outcome-network-identity (outcome)
  "The network identity: the constant coefficient of the outcome's public polynomial."
  (sharing-public (outcome-sharing outcome)))

(defun decode-onchain-dkg-outcome (octets &key strict)
  "OnchainDkgOutcome::read with Tempo's configuration (MAX_VALIDATORS, ModeVersion v0).
Tempo reads boundary extraData with Read, ignoring trailing bytes; STRICT rejects them."
  (let ((*max-participants* +max-validators+)
        (*max-mode-version* 0))
    (values (cw-decode 'onchain-dkg-outcome octets :partial (not strict)))))

(defun encode-onchain-dkg-outcome (outcome)
  (cw-encode 'onchain-dkg-outcome outcome))

(defun decode-signed-dealer-log (octets max-players &key strict)
  "SignedDealerLog::read_cfg(max_players). Tempo's read_dealer_log ignores trailing bytes
before T15 and rejects them from T15 (STRICT)."
  (let ((*max-participants* max-players))
    (values (cw-decode 'signed-dealer-log octets :partial (not strict)))))
