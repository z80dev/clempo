(in-package #:clempo.dev)

;;; Consensus commands: certify finalization certificates over a mainnet range. Blocks come from the raw archive or the private node (CLEMPO_RPC);
;;; certificates from the public RPC's consensus_getFinalization (the private node runs
;;; without certification), cached under ~/cache/clempo/finalizations/<chain>/.

(defparameter *certificate-rpc* "https://rpc.tempo.xyz")

(defun finalization-cache-dir (&optional (chain "mainnet"))
  (format nil "~A/cache/clempo/finalizations/~A/" (clempo.io:getenv "HOME") chain))

(defconstant +certificate-file-span+ 1000 "heights per certificate cache file")

(defun certificate-file (first)
  (format nil "~Afin-~10,'0D.txt" (finalization-cache-dir) first))

(defun read-certificate-file (first)
  "Cached certificates for FIRST..FIRST+999 as a vector of hex strings, or NIL."
  (let ((path (certificate-file first)))
    (when (probe-file path)
      (let ((v (make-array +certificate-file-span+ :initial-element nil)))
        (with-open-file (in path)
          (loop for line = (read-line in nil)
                while line
                do (let ((space (position #\Space line)))
                     (setf (aref v (- (parse-integer line :end space) first))
                           (subseq line (1+ space))))))
        (and (every #'identity v) v)))))

(defun fetch-certificates (from count)
  "consensus_getFinalization for FROM..FROM+COUNT-1, batches of 60; a vector of hex strings."
  (let ((out (make-array count)))
    (loop for start from 0 below count by 60
          do (let ((results (clempo.jsonrpc:rpc-batch
                             (loop for h from (+ from start) below (+ from (min count (+ start 60)))
                                   collect (list "consensus_getFinalization"
                                                 (list (clempo.json:jobject "height" h))))
                             :url *certificate-rpc*)))
               (loop for r in results
                     for i from start
                     do (invariant (clempo.json:jobject-p r) "no finalization for block ~D: ~A"
                                   (+ from i) r)
                        (setf (aref out i) (clempo.json:jget r "certificate")))))
    out))

(defun certificate-span (first)
  "The certificates of one cache span, from the cache or fetched and cached."
  (or (read-certificate-file first)
      (let ((v (fetch-certificates first +certificate-file-span+)))
        (clempo.io:write-file-atomically
         (certificate-file first)
         (string->octets (with-output-to-string (s)
                           (loop for c across v for h from first
                                 do (format s "~D ~A~%" h c)))))
        v)))

(defun map-on-threads (function items &key (threads 8))
  "MAPCAR on THREADS worker threads, results in order."
  (let* ((items (coerce items 'simple-vector))
         (out (make-array (length items)))
         (next 0)
         (lock (sb-thread:make-mutex))
         (workers
           (loop repeat (max 1 (min threads (length items)))
                 collect (sb-thread:make-thread
                          (lambda ()
                            (loop for i = (sb-thread:with-mutex (lock)
                                            (when (< next (length items))
                                              (prog1 next (incf next))))
                                  while i
                                  do (setf (aref out i) (funcall function (aref items i)))))))))
    (mapc #'sb-thread:join-thread workers)
    (coerce out 'list)))

(defun fetch-raw-blocks (from count &key (batch 500))
  "debug_getRawBlock FROM..FROM+COUNT-1 from the private node (CLEMPO_RPC), as a vector."
  (let ((out (make-array count)))
    (loop for start from 0 below count by batch
          do (loop for r in (clempo.jsonrpc:rpc-batch
                             (loop for h from (+ from start) below (+ from (min count (+ start batch)))
                                   collect (list "debug_getRawBlock" (list (quantity->hex h)))))
                   for i from start
                   do (invariant (stringp r) "block ~D: ~A" (+ from i) r)
                      (setf (aref out i) (hex->octets r))))
    out))

(defun raw-blocks (from to &key (threads 8))
  "Raw blocks FROM..TO: archived chunks where present, else the private node."
  (let ((spans (loop for first from (* clempo.archive:+chunk-blocks+ (floor from clempo.archive:+chunk-blocks+))
                       to to by clempo.archive:+chunk-blocks+
                     collect first)))
    (apply #'concatenate 'simple-vector
           (map-on-threads
            (lambda (first)
              (let ((lo (max from first)) (hi (min to (+ first clempo.archive:+chunk-blocks+ -1))))
                (if (clempo.archive:chunk-present-p first)
                    (let ((chunk (clempo.archive:read-chunk first)))
                      (coerce (loop for n from lo to hi collect (clempo.archive:chunk-block chunk n))
                              'simple-vector))
                    (fetch-raw-blocks lo (1+ (- hi lo))))))
            spans :threads threads))))

(defun block-facts (raw)
  "(height hash parent-hash extra-data) of a raw block; the body is checked against the
header's transactions root."
  (let* ((b (clempo.blocks:decode-block raw))
         (h (clempo.blocks:block-header b))
         (inner (clempo.headers:header-inner h)))
    (invariant (octets= (clempo.headers:ethereum-header-transactions-root inner)
                        (clempo.blocks:transactions-root (clempo.blocks:block-transactions b)))
               "block ~D: body does not match its transactions root"
               (clempo.headers:ethereum-header-number inner))
    (list (clempo.headers:ethereum-header-number inner)
          (clempo.headers:header-hash h)
          (clempo.headers:ethereum-header-parent-hash inner)
          (clempo.headers:ethereum-header-extra-data inner))))

(defun certify-range (from to &key (threads 16) (anchor :boundary) (log t))
  "Verify the finalization certificate of every block FROM..TO. ANCHOR :boundary takes the
identity of FROM's epoch from the DKG outcome in the previous epoch's boundary block (on
trust, as goempo and Tempo's follow mode anchor); :network uses mainnet's published network
identity. Returns the CERTIFICATION and timings."
  (let* ((v (clempo.consensus:make-finalization-verifier
             :epoch-length clempo.consensus:+mainnet-epoch-length+
             :network-identity (when (eq anchor :network) clempo.consensus:+mainnet-network-identity+)
             :network-from-epoch clempo.consensus:+mainnet-network-from-epoch+))
         (e (clempo.consensus:make-fixed-epocher clempo.consensus:+mainnet-epoch-length+))
         (epoch (clempo.consensus:epoch-of-height e from))
         (t0 (clempo.io:monotonic-ticks)))
    (flet ((secs () (/ (- (clempo.io:monotonic-ticks) t0) (float internal-time-units-per-second)))
           (say (fmt &rest args) (when log (apply #'log-line fmt args))))
      (when (eq anchor :boundary)
        (let* ((boundary (1- (clempo.consensus:epoch-first-height e epoch)))
               (facts (block-facts (aref (raw-blocks boundary boundary) 0)))
               (outcome (clempo.consensus:verifier-register-boundary-outcome v (fourth facts))))
          (say "certify: anchor = DKG outcome in block ~D: epoch ~D identity ~A~A" boundary
               (clempo.dkg:outcome-epoch outcome)
               (octets->hex (clempo.dkg:outcome-network-identity outcome) :end 8)
               (if (octets= (clempo.dkg:outcome-network-identity outcome)
                            clempo.consensus:+mainnet-network-identity+)
                   " (= mainnet network identity)" ""))))
      (let* ((raws (raw-blocks from to :threads threads))
             (t-blocks (secs))
             (facts (map-on-threads #'block-facts raws :threads threads))
             (spans (loop for first from (* +certificate-file-span+ (floor from +certificate-file-span+))
                            to to by +certificate-file-span+ collect first))
             (certs (map-on-threads #'certificate-span spans :threads 4))
             (t-fetch (secs))
             (blocks (loop for f in facts
                           for h = (first f)
                           collect (append f (list (aref (nth (floor (- h (first spans)) +certificate-file-span+) certs)
                                                         (mod h +certificate-file-span+))))))
             (clempo.consensus:*batch-mapper*
               (lambda (fn batches) (map-on-threads fn batches :threads threads)))
             (result (clempo.consensus:certify-blocks v blocks :batch 250)))
        (say "certify: ~D blocks (~,1Fs), certificates (~,1Fs), verified in ~,1Fs"
             (length raws) t-blocks (- t-fetch t-blocks) (- (secs) t-fetch))
        (dolist (ep (clempo.consensus:certification-epochs result))
          (say "  epoch ~D: ~D certificates under identity ~A" (first ep) (third ep)
               (subseq (second ep) 0 18)))
        (values result (secs))))))

(defcommand "certify" (args)
  "verify every block's finalization certificate: certify -from A -to B [-threads 16] [-anchor boundary|network]"
  (let ((from (arg-int args :from)) (to (arg-int args :to)))
    (unless (and from to (<= from to))
      (log-line "certify: need -from A -to B")
      (return-from command 2))
    (handler-case
        (multiple-value-bind (result secs)
            (certify-range from to :threads (arg-int args :threads 16)
                                   :anchor (if (string-equal (arg args :anchor "boundary") "network")
                                               :network :boundary))
          (write-line
           (clempo.json:to-json-string
            (clempo.json:jobject "from" from "to" to
                                 "blocks" (clempo.consensus:certification-verified result)
                                 "certificates" (clempo.consensus:certification-certificates result)
                                 "epochs" (coerce (mapcar #'first (clempo.consensus:certification-epochs result))
                                                  'simple-vector)
                                 "seconds" (round secs))))
          0)
      (clempo.consensus:certificate-error (c)
        (log-line "certify: FAILED: ~A" c)
        1))))
