(defpackage #:clempo.follow
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:rpc #:clempo.jsonrpc) (#:json #:clempo.json)
                    (#:archive #:clempo.archive) (#:blocks #:clempo.blocks)
                    (#:headers #:clempo.headers))
  (:export #:head #:make-head #:head-number #:head-hash #:head-state-root #:rpc-head
           #:follower #:make-follower #:follower-head #:follower-blocks #:follower-transactions
           #:follower-certified #:follower-chunks #:follower-rpc-url
           #:follow-error #:follow-error-height #:follow-error-reason
           #:execution-failed #:execution-failed-cause
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

(define-condition execution-failed (error)
  ((cause :initarg :cause :reader execution-failed-cause))
  (:report (lambda (condition out)
             (format out "~A" (execution-failed-cause condition))))
  (:documentation "The executor failed on a verified block: local state may be wrong, so
FOLLOW-LIVE never retries it. Every other failure (upstream, transport, certificates,
malformed blocks) is rejected data and is retried."))

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

(defgeneric apply-follow-block (executor block &key certificate)
  (:documentation "Execute and commit a verified BLOCK, with its verified certificate if available."))

(defgeneric certify-follow-batch (certifier blocks previous-hash)
  (:documentation "Verify a consecutive prefix of BLOCKS and return its length and optionally
a vector of its verified certificate hex strings. PREVIOUS-HASH is the last accepted block hash.
Only a not-yet-finalized suffix may be omitted; invalid certificates and transport failures signal."))

(defstruct (follower (:constructor %make-follower
                         (head rpc-url block-source executor certifier archive-dir
                          use-archive write-archive batch-size)))
  (head nil :type (or null head))
  (rpc-url nil :type (or null string) :read-only t)
  (block-source nil :type (or null function) :read-only t)
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

(defun make-follower (head &key (rpc-url (rpc:local-rpc-url)) block-source executor certifier
                             (archive-dir (archive:archive-dir)) use-archive write-archive
                             (batch-size 32))
  "Anchor at HEAD. Without an EXECUTOR verify linkage and body roots only.
BLOCK-SOURCE, if supplied, takes FROM and COUNT and returns a simple-vector of
COUNT raw block encodings. It replaces RPC body fetches, not RPC canonical heads."
  (check-type batch-size (integer 1 *))
  (check-type block-source (or null function))
  (check-type head head)
  (let ((hash (head-hash head))) (check-type hash (simple-array (unsigned-byte 8) (32))))
  (%make-follower head rpc-url block-source executor certifier
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
  (let ((chunk (archived-chunk follower (chunk-first-for from)))
        (source (follower-block-source follower)))
    (cond (chunk
           (let ((raw (make-array count)))
             (dotimes (i count raw) (setf (svref raw i) (archive:chunk-block chunk (+ from i))))))
          (source
           (let ((raw (funcall source from count)))
             (unless (and (simple-vector-p raw) (= count (length raw)))
               (reject-block from :block-source "wrong block batch shape"))
             raw))
          (t (archive:fetch-blocks from count :url (follower-rpc-url follower) :batch count)))))

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

(defun follow-to (follower tip &key stop)
  "Follow up to canonical TIP and return the current HEAD and whether TIP was reached.
An unavailable finalization certificate stops at the last certified block, and so does
STOP returning true between batches. An upstream reorg or mismatching block fails
closed; this function never rolls back."
  (let* ((limit (head-number tip))
         (anchor (follower-head follower))
         (last (head-number anchor))
         (url (follower-rpc-url follower))
         (executor (follower-executor follower))
         (certifier (follower-certifier follower)))
    (when (< limit last) (reject-block last :upstream-behind limit))
    (loop while (and (< last limit) (not (and stop (funcall stop))))
          for from = (1+ last)
          for end = (min limit (+ last (follower-batch-size follower))
                         (+ (chunk-first-for from) archive:+chunk-blocks+ -1))
          for expected = (if (= end limit) tip (rpc-head end :url url))
          for raw = (fetch-follow-batch follower from (- end last))
          do (multiple-value-bind (blocks heads)
                 (verify-follow-batch from (head-hash (follower-head follower)) expected raw)
               (multiple-value-bind (accepted certificates)
                   (if certifier
                       (certify-follow-batch certifier blocks (head-hash (follower-head follower)))
                       (length blocks))
                 (invariant (<= 0 accepted (length blocks)) "bad certified prefix length ~S" accepted)
                 (dotimes (i accepted)
                   (let ((block (svref blocks i)) (head (svref heads i)))
                     (when executor
                       (handler-bind ((error (lambda (c) (error 'execution-failed :cause c))))
                         (apply-follow-block executor block
                                             :certificate (and certificates (svref certificates i)))))
                     (setf (follower-head follower) head last (head-number head))
                     (incf (follower-blocks follower))
                     (incf (follower-transactions follower) (length (blocks:block-transactions block)))
                     (when (and certificates (svref certificates i)) (incf (follower-certified follower)))
                     (archive-follow-block follower last (svref raw i))))
                 (unless (= accepted (length blocks))
                   (return-from follow-to (values (follower-head follower) nil))))))
    (when (< (head-number (follower-head follower)) limit)
      (return-from follow-to (values (follower-head follower) nil)))
    (unless (octets= (head-hash tip) (head-hash (follower-head follower)))
      (reject-block limit :canonical-hash "upstream and local head at the same height differ"))
    (values (follower-head follower) t)))

(define-constant +retries-bounded+ 20)
(define-constant +backoff-max-ms+ 10000)

(defun pause (ms stop)
  "Sleep MS milliseconds in short slices; return true early once STOP says so."
  (let ((until (+ (clempo.io:now-ms) ms)))
    (loop (when (and stop (funcall stop)) (return t))
          (let ((left (- until (clempo.io:now-ms))))
            (when (<= left 0) (return nil))
            (sleep (/ (min left 100) 1000))))))

(defun follow-live (follower &key tip (poll-seconds 0.25) once seconds on-poll on-target stop on-retry)
  "Catch up to the supplied TIP (or the current one), then poll until STOP returns true.
ONCE exits after reaching the initial TIP. SECONDS counts from first reaching it.
ON-TARGET receives FOLLOWER and each remote TIP before following to it; ON-POLL the same
at the end of each poll.
A failed step (upstream, transport, certificate or rejected block) is retried with
exponential backoff up to 10 s, calling ON-RETRY with the attempt and condition; a live
follower retries forever, a bounded one (ONCE or SECONDS) gives up after 20 in a row.
EXECUTION-FAILED is never retried."
  (check-type poll-seconds (real (0) *))
  (check-type seconds (or null (real 0 *)))
  (let ((url (follower-rpc-url follower))
        (poll-ms (max 1 (round (* 1000 poll-seconds))))
        (initial-tip tip)
        (since nil)
        (retries 0))
    (loop
      (when (and stop (funcall stop)) (return (follower-head follower)))
      (let ((failure
              (handler-case
                  (let ((target (or initial-tip (rpc-head :latest :url url))))
                    (when on-target (funcall on-target follower target))
                    (when (nth-value 1 (follow-to follower target :stop stop))
                      (setf initial-tip nil)
                      (unless since (setf since (clempo.io:now-ms))))
                    (when on-poll (funcall on-poll follower target))
                    nil)
                (execution-failed (c) (error c))
                (error (c) c))))
        (cond (failure
               (incf retries)
               (when (and (or once seconds) (> retries +retries-bounded+))
                 (error failure))
               (when on-retry (funcall on-retry retries failure))
               (when (pause (min (* poll-ms (expt 2 (min retries 8))) +backoff-max-ms+) stop)
                 (return (follower-head follower))))
              (t
               (setf retries 0)
               (when (and since
                          (or once (and seconds (>= (- (clempo.io:now-ms) since) (* seconds 1000)))))
                 (return (follower-head follower)))
               (when (pause poll-ms stop)
                 (return (follower-head follower)))))))))
