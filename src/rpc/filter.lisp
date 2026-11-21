(defpackage #:clempo.rpc.filter
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render #:clempo.rpc.state)
  (:local-nicknames (#:json #:clempo.json) (#:serde #:clempo.serde) (#:chain #:clempo.rpc.chain)
                    (#:calls #:clempo.rpc.calls))
  (:documentation "trace_filter (reth crates/rpc/rpc/src/trace.rs): the parity traces of a block
range that match alloy's TraceFilter, paged by after and count. Blocks replay in order through
clempo.rpc.calls' tracing; Tempo has no block rewards, so no reward traces are added.")
  (:export #:trace-matches-p #:paginate))

(in-package #:clempo.rpc.filter)

(defparameter *max-trace-filter-blocks* 100 "reth DEFAULT_MAX_TRACE_FILTER_BLOCKS")

(defparam trace-filter-mode (src) "alloy TraceFilterMode: :union or :intersection"
  (if (string= (serde:read-enum src '("union" "intersection")) "union") :union :intersection))

(calls:defvec addresses address)

(calls:defobject trace-filter (:expected "struct TraceFilter" :deny-unknown t)
  (:from-block "fromBlock" calls:q64 :optional t)
  (:to-block "toBlock" calls:q64 :optional t)
  (:from-address "fromAddress" addresses :optional t)
  (:to-address "toAddress" addresses :optional t)
  (:mode "mode" trace-filter-mode :optional t)
  (:after "after" calls:plain-u64 :optional t)
  (:count "count" calls:plain-u64 :optional t)
  (:block-hash "blockHash" hash :optional t))

;;; Matching: TraceFilterMatcher. An empty address list matches every address.

(defun address-of (hex) (and (stringp hex) (hex->integer hex)))

(defun trace-matches-p (trace from to mode)
  "Whether the localized parity TRACE matches the address lists FROM and TO under MODE."
  (let* ((action (json:jget trace "action"))
         (result (json:jget trace "result")))
    (flet ((in (addresses hex) (or (null addresses) (member (address-of hex) addresses))))
      (multiple-value-bind (from-ok to-ok)
          (let ((type (json:jget trace "type")))
            (cond ((string= type "call")
                   (values (in from (json:jget action "from")) (in to (json:jget action "to"))))
                  ((string= type "create")
                   (values (in from (json:jget action "from"))
                           (if (json:jobject-p result)
                               (in to (json:jget result "address"))
                               (null to))))
                  ((string= type "suicide")
                   (values (in from (json:jget action "address")) (in to (json:jget action "refundAddress"))))
                  (t (unreachable "trace type ~A" type))))
        (if (eq mode :union)
            (cond ((null from) to-ok)
                  ((null to) from-ok)
                  (t (or from-ok to-ok)))
            (and from-ok to-ok))))))

(defun paginate (traces after count)
  "TRACES without the first AFTER, at most COUNT of them: (values page complete-p).
COMPLETE-P is true when more traces cannot change the page (reth returns it at once)."
  (let ((n (length traces)))
    (cond ((and after (>= after n)) (values '() nil))
          (t (let ((rest (if after (nthcdr after traces) traces)))
               (if (and count (< count (length rest)))
                   (values (subseq rest 0 count) t)
                   (values rest nil)))))))

;;; The method

(defun filter-range (filter)
  "reth's block range checks; returns (values start end)."
  (let ((hash (getf filter :block-hash))
        (from (getf filter :from-block)) (to (getf filter :to-block))
        (latest chain:*head*))
    (when hash
      (when (or from to)
        (rpc-fail +invalid-params+ "cannot specify both blockHash and fromBlock/toBlock, choose one or the other"))
      (let ((n (block-id-number (list :hash hash))))
        (unless n (header-not-found (list :hash hash)))
        (setf from n to n)))
    (let ((start (or from latest)) (end (or to latest)))
      (when (> start latest) (header-not-found start))
      (when (> end latest) (header-not-found end))
      (when (> start end)
        (rpc-fail +invalid-params+ "invalid parameters: fromBlock cannot be greater than toBlock"))
      (when (> (- end start) *max-trace-filter-blocks*)
        (rpc-fail +invalid-params+ (format nil "Block range too large; currently limited to ~D blocks"
                                           *max-trace-filter-blocks*)))
      (values start end))))

(defrpc "trace_filter" ((filter trace-filter))
  "parity traces of a block range matching from/to addresses, paged by after and count"
  (multiple-value-bind (start end) (filter-range filter)
    (let ((from (getf filter :from-address)) (to (getf filter :to-address))
          (mode (getf filter :mode :intersection))
          (after (getf filter :after)) (count (getf filter :count))
          (matched '()))
      ;; reth pages after each block and answers at once when COUNT traces follow AFTER
      (coerce
       (loop for n from start to end
             for b = (or (chain-block n) (header-not-found n))
             do (setf matched
                      (append matched
                              (remove-if-not (lambda (tr) (trace-matches-p tr from to mode))
                                             (coerce (apply #'concatenate 'simple-vector
                                                            (calls:block-traces b (calls:parity-maker '(:trace))
                                                                                #'calls:parity-localized))
                                                     'list))))
                (multiple-value-bind (page complete) (paginate matched after count)
                  (when complete (return page)))
             finally (return (paginate matched after count)))
       'simple-vector))))
