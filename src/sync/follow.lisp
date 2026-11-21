(defpackage #:clempo.follow
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:archive #:clempo.archive) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers))
  (:export #:head #:make-head #:head-number #:head-hash #:head-state-root #:rpc-head
           #:follower #:make-follower #:follower-head #:follower-blocks #:follower-transactions
           #:follower-certified #:follower-chunks #:follower-rpc-url
           #:follow-error #:follow-error-height #:follow-error-reason
           #:apply-follow-block #:certify-follow-batch #:follow-to #:follow-live))

(in-package #:clempo.follow)

(defstruct head
  (number 0 :type (unsigned-byte 64) :read-only t)
  (hash nil :type (or null octets) :read-only t)
  (state-root nil :type (or null octets) :read-only t))

(define-condition follow-error (error)
  ((height :initarg :height :reader follow-error-height)
   (reason :initarg :reason :reader follow-error-reason)
   (details :initarg :details :reader follow-error-details))
  (:report (lambda (condition out)
             (format out "follow at block ~D: ~A (~A)" (follow-error-height condition)
                     (follow-error-reason condition) (follow-error-details condition)))))

(defun reject-block (height reason details)
  (error 'follow-error :height height :reason reason :details details))

(defun rpc-head (number &key (url (rpc:local-rpc-url)))
  "Fetch the current HEAD when NUMBER is :LATEST, otherwise the canonical header at NUMBER."
  (check-type number (or (eql :latest) (unsigned-byte 64)))
  (let* ((tag (if (eq number :latest) "latest" (quantity->hex number)))
         (result (rpc:rpc-call "eth_getBlockByNumber" (list tag :false) :url url)))
    (unless (json:jobject-p result) (error "Upstream has no canonical block at ~A" tag))
    (let ((height (hex->integer (json:jget result "number")))
          (hash (hex->octets (json:jget result "hash")))
          (root (hex->octets (json:jget result "stateRoot"))))
      (unless (and (typep height '(unsigned-byte 64)) (or (eq number :latest) (= number height))
                   (= 32 (length hash)) (= 32 (length root)))
        (reject-block height :upstream-header "invalid number or hash length"))
      (make-head :number height :hash hash :state-root root))))

(defgeneric apply-follow-block (executor block)
  (:documentation "Execute and commit a verified BLOCK or signal before advancing the follower."))

(defgeneric certify-follow-batch (certifier blocks previous-hash)
  (:documentation "Verify a consecutive prefix of BLOCKS (decoded block vector) and return its length.
PREVIOUS-HASH is the last accepted block hash. Only a not-yet-finalized suffix
may be omitted. Invalid certificates or transport failures must signal."))

(defstruct (follower (:constructor %make-follower (head rpc-url executor certifier archive-dir
                                                   use-archive write-archive batch-size)))
  (head nil :type (or null head))
  (rpc-url nil :type (or null string) :read-only t)
  (executor nil :read-only t)
  (certifier nil :read-only t)
  (archive-dir nil :read-only t)
  (use-archive nil :read-only t)
  (write-archive nil :read-only t)
  (batch-size 32 :type (integer 1 *) :read-only t)
  archive-cache
  archive-first
  archive-pending
  (blocks 0 :type integer)
  (transactions 0 :type integer)
  (certified 0 :type integer)
  (chunks 0 :type integer))

(defun make-follower (head &key (rpc-url (rpc:local-rpc-url)) executor certifier
                             (archive-dir (archive:archive-dir)) use-archive write-archive
                             (batch-size 32))
  "Anchor a new follower at HEAD. Without an EXECUTOR verify linkage and body roots only."
  (check-type batch-size (integer 1 *))
  (check-type head head)
  (let ((hash (head-hash head))) (check-type hash (simple-array (unsigned-byte 8) (32))))
  (%make-follower head rpc-url executor certifier
                  (namestring (uiop:ensure-directory-pathname archive-dir))
                  use-archive write-archive batch-size))

(defun chunk-first-for (height)
  (* (floor height archive:+chunk-blocks+) archive:+chunk-blocks+))

(defun archived-chunk (follower first)
  (when (follower-use-archive follower)
    (let ((cached (follower-archive-cache follower))
          (dir (follower-archive-dir follower)))
      (cond ((and cached (= first (archive:chunk-first cached))) cached)
            ((archive:chunk-present-p first dir)
             (let ((chunk (archive:read-chunk first dir)))
               (unless (= (archive:chunk-count chunk) archive:+chunk-blocks+)
                 (reject-block first :archive-chunk "expected a complete chunk"))
               (setf (follower-archive-cache follower) chunk)))
            (t (setf (follower-archive-cache follower) nil))))))

(defun fetch-follow-batch (follower from count)
  (let ((chunk (archived-chunk follower (chunk-first-for from))))
    (if chunk
        (let ((raw (make-array count)))
          (dotimes (i count raw) (setf (svref raw i) (archive:chunk-block chunk (+ from i)))))
        (archive:fetch-blocks from count :url (follower-rpc-url follower) :batch count))))

(defun verify-follow-batch (from previous-hash expected raw)
  ;; Authenticate the entire batch before calling either downstream protocol.
  (let* ((count (length raw)) (blocks (make-array count)) (heads (make-array count))
         (parent previous-hash))
    (dotimes (i count)
      (let* ((height (+ from i))
             (block (blocks:decode-block (svref raw i)))
             (header (blocks:block-header block)))
        (unless (= height (headers:header-number header))
          (reject-block height :block-number (headers:header-number header)))
        (unless (octets= parent (headers:header-parent-hash header))
          (reject-block height :parent-hash
                        (list (octets->hex parent :prefix t)
                              (octets->hex (headers:header-parent-hash header) :prefix t))))
        (let ((mismatches (blocks:block-body-mismatches block)))
          (when mismatches (reject-block height :body-root mismatches)))
        (setf parent (blocks:block-hash block)
              (svref blocks i) block
              (svref heads i) (make-head :number height :hash parent
                                         :state-root (headers:header-state-root header)))))
    (unless (octets= (head-hash expected) parent)
      (reject-block (head-number expected) :canonical-hash
                    (list (octets->hex (head-hash expected) :prefix t)
                          (octets->hex parent :prefix t))))
    (values blocks heads)))

(defun archive-follow-block (follower height raw)
  (when (follower-write-archive follower)
    (let* ((first (chunk-first-for height))
           (pending (follower-archive-pending follower))
           (dir (follower-archive-dir follower)))
      (when (and (= first height) (not (archive:chunk-present-p first dir)))
        (setf pending (make-array archive:+chunk-blocks+)
              (follower-archive-pending follower) pending
              (follower-archive-first follower) first))
      (when (and pending (eql first (follower-archive-first follower)))
        (let ((index (- height first)))
          (setf (svref pending index) raw)
          (when (= (1+ index) archive:+chunk-blocks+)
            (unless (archive:chunk-present-p first dir)
              (archive:write-chunk first pending dir)
              (incf (follower-chunks follower)))
            (setf (follower-archive-pending follower) nil
                  (follower-archive-first follower) nil)))))))

(defun follow-to (follower tip)
  "Follow up to canonical TIP and return the current HEAD and whether TIP was reached.
An unavailable finalization certificate stops at the last certified block. An
upstream reorg or mismatching block fails closed; this function never rolls back."
  (let* ((limit (head-number tip))
         (anchor (follower-head follower))
         (last (head-number anchor))
         (url (follower-rpc-url follower))
         (executor (follower-executor follower))
         (certifier (follower-certifier follower)))
    (when (< limit last) (reject-block last :upstream-behind limit))
    (loop while (< last limit)
          for from = (1+ last)
          for end = (min limit (+ last (follower-batch-size follower))
                         (+ (chunk-first-for from) archive:+chunk-blocks+ -1))
          for expected = (if (= end limit) tip (rpc-head end :url url))
          for raw = (fetch-follow-batch follower from (- end last))
          do (multiple-value-bind (blocks heads)
                 (verify-follow-batch from (head-hash (follower-head follower)) expected raw)
               (let ((accepted (if certifier
                                   (certify-follow-batch certifier blocks
                                                         (head-hash (follower-head follower)))
                                   (length blocks))))
                 (invariant (<= 0 accepted (length blocks)) "bad certified prefix length ~S" accepted)
                 (dotimes (i accepted)
                   (let ((block (svref blocks i)) (head (svref heads i)))
                     (when executor (apply-follow-block executor block))
                     (setf (follower-head follower) head last (head-number head))
                     (incf (follower-blocks follower))
                     (incf (follower-transactions follower) (length (blocks:block-transactions block)))
                     (when certifier (incf (follower-certified follower)))
                     (archive-follow-block follower last (svref raw i))))
                 (unless (= accepted (length blocks))
                   (return-from follow-to (values (follower-head follower) nil))))))
    (unless (octets= (head-hash tip) (head-hash (follower-head follower)))
      (reject-block limit :canonical-hash "upstream and local head at the same height differ"))
    (values (follower-head follower) t)))

(defun follow-live (follower &key tip (poll-seconds 0.25) once seconds on-poll)
  "Catch up to the supplied TIP (or the current one), then poll until interrupted.
ONCE exits after reaching the initial TIP. SECONDS counts from first reaching it.
ON-POLL receives FOLLOWER and remote TIP at the end of each poll."
  (check-type poll-seconds (real (0) *))
  (check-type seconds (or null (real 0 *)))
  (let ((url (follower-rpc-url follower))
        (initial-tip tip)
        (since nil))
    (loop for target = (or initial-tip (rpc-head :latest :url url))
          do (multiple-value-bind (head caught-up) (follow-to follower target)
               (declare (ignore head))
               (when caught-up
                 (setf initial-tip nil)
                 (unless since (setf since (get-internal-real-time)))))
             (when on-poll (funcall on-poll follower target))
             (when (and since
                        (or once (and seconds (>= (- (get-internal-real-time) since)
                                                  (* seconds internal-time-units-per-second)))))
               (return (follower-head follower)))
             (sleep poll-seconds))))
