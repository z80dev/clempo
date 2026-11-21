(defpackage #:clempo.rpc.logs
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:serde #:clempo.serde)
                    (#:r #:clempo.receipts) (#:eth #:clempo.rpc.eth) (#:rlp #:clempo.rlp))
  (:documentation "eth_getLogs and eth_feeHistory: header-bloom scans over block ranges with
reth's limits and messages (100,000 blocks per query, 20,000 logs per response).")
  (:export #:log-filter #:read-log-filter #:filter-logs-in-block #:log-matches-p #:logs-in-range
           #:filter-from #:filter-to #:filter-block-hash))

(in-package #:clempo.rpc.logs)

(defparameter *max-blocks-per-filter* 100000)
(defparameter *max-logs-per-response* 20000)

(defstruct (log-filter (:conc-name filter-))
  from to block-hash
  (addresses nil)          ; list of integers; NIL matches any
  (topics nil)             ; list per position: NIL (any) or a list of 32-byte octets
  (bloom nil))             ; FILTER-BLOOM-BITS, once computed

;;; Reading a Filter object

(defparameter +variadic-error+
  "Invalid variadic value or array type: data did not match any variant of untagged enum Variadic")

(defun hex-of-length-p (s n)
  (and (stringp s)
       (let ((h (if (and (>= (length s) 2) (string-equal "0x" s :end2 2)) (subseq s 2) s)))
         (and (= (length h) (* 2 n)) (every (lambda (c) (digit-char-p c 16)) h)))))

(defun variadic (value n convert)
  "alloy's ValueOrArray: null, one value or an array of them; NIL when malformed (:fail)."
  (cond ((eq value :null) nil)
        ((hex-of-length-p value n) (list (funcall convert value)))
        ((and (vectorp value) (not (stringp value)) (every (lambda (v) (hex-of-length-p v n)) value))
         (map 'list convert value))
        (t :fail)))

(defun read-log-filter (src)
  (unless (eq (serde:peek-kind src) :object) (serde:invalid-type src "Filter object"))
  (let ((f (make-log-filter)) (address :absent) (topics nil))
    (serde:do-object (key src)
      (cond ((string= key "fromBlock")
             (setf (filter-from f) (if (serde:read-null-p src) nil (read-param 'block-number-or-tag-positioned src))))
            ((string= key "toBlock")
             (setf (filter-to f) (if (serde:read-null-p src) nil (read-param 'block-number-or-tag-positioned src))))
            ((string= key "blockHash")
             (setf (filter-block-hash f) (if (serde:read-null-p src) nil (read-param 'hash src))))
            ((string= key "address") (setf address (serde:read-value src)))
            ((string= key "topics")
             (let ((v (serde:read-value src)))
               (setf topics
                     (cond ((eq v :null) nil)
                           ((and (vectorp v) (not (stringp v)))
                            (map 'list (lambda (entry)
                                         (let ((set (variadic entry 32 #'hex->octets)))
                                           (when (eq set :fail) (serde:serde-fail-here src +variadic-error+))
                                           set))
                                 v))
                           (t (serde:serde-fail-here src "invalid type: ~A, expected a sequence"
                                                     (if (stringp v) (format nil "string ~A" (serde:rust-debug-string v)) "map")))))))
            (t (serde:serde-fail-after-key src "unknown field `~A`, expected one of `fromBlock`, `toBlock`, `address`, `topics`, `blockHash`" key))))
    (unless (eq address :absent)
      (let ((set (variadic address 20 #'hex->integer)))
        (when (eq set :fail) (serde:serde-fail-here src +variadic-error+))
        (setf (filter-addresses f) set)))
    (when (> (length topics) 4) (serde:serde-fail-here src "exceeded maximum topics len"))
    (when (and (filter-block-hash f) (or (filter-from f) (filter-to f)))
      (serde:serde-fail-here src "cannot specify both blockHash and fromBlock/toBlock, choose one or the other"))
    (setf (filter-topics f) topics)
    f))

(defparam block-number-or-tag-positioned (src) "BlockNumberOrTag inside an object: errors carry positions"
  (if (eq (serde:peek-kind src) :string)
      (let ((s (serde:read-string src)))
        (handler-case (clempo.rpc::parse-block-number-or-tag s)
          (serde:serde-error (e) (serde:serde-fail-here src "~A" (serde:serde-error-message e)))))
      (serde:invalid-type src "a string")))

(defparam filter (src) "alloy Filter" (read-log-filter src))

;;; Matching

(defun bloom-bits (bytes)
  "Where keccak(BYTES) sets its 3 bits in a 256-octet bloom: (octet mask octet mask octet mask)."
  (let ((h (clempo.keccak:keccak256 bytes)) (v (make-array 6 :element-type 'fixnum)))
    (loop for i from 0 below 6 by 2
          do (let ((bit (logand (logior (ash (aref h i) 8) (aref h (1+ i))) 2047)))
               (setf (aref v i) (- 255 (floor bit 8)) (aref v (1+ i)) (ash 1 (mod bit 8)))))
    v))

(defun bloom-bits-p (bits bloom offset)
  "Whether the 256 octets of BLOOM from OFFSET have every bit of BITS (BLOOM-BITS)."
  (declare (type (simple-array fixnum (6)) bits) (type octets bloom) (type fixnum offset))
  (invariant (<= 0 offset (- (length bloom) 256)) "bloom outside its octets")
  (loop for i from 0 below 6 by 2
        always (logtest (aref bits (1+ i)) (aref bloom (+ offset (aref bits i))))))

(defun filter-bloom-bits (f)
  "F's addresses and topics as BLOOM-BITS, computed once: (addresses . topics), each NIL
\(any) or a list of alternatives, the topics one entry per position."
  (or (filter-bloom f)
      (setf (filter-bloom f)
            (cons (mapcar (lambda (a) (bloom-bits (integer->octets a 20))) (filter-addresses f))
                  (mapcar (lambda (set) (mapcar #'bloom-bits set)) (filter-topics f))))))

(defun filter-matches-bloom-p (f bloom &optional (offset 0))
  "Whether the bloom at OFFSET in BLOOM may hold a log F matches."
  (destructuring-bind (addresses . topics) (filter-bloom-bits f)
    (and (or (null addresses) (some (lambda (bits) (bloom-bits-p bits bloom offset)) addresses))
         (every (lambda (set) (or (null set) (some (lambda (bits) (bloom-bits-p bits bloom offset)) set)))
                topics))))

(defun log-matches-p (f log)
  (and (or (null (filter-addresses f)) (member (r:event-log-address log) (filter-addresses f)))
       (loop for set in (filter-topics f)
             for i from 0
             always (or (null set)
                        (let ((topic (nth i (r:event-log-topics log))))
                          (and topic (member topic set :test #'octets=)))))))

(defun filter-logs-in-block (f b)
  "The rendered logs of cached block B that match F (JSON-RAW texts, written once per log)."
  (let ((out '()) (log-index 0))
    (loop for rc across (cblock-receipts b)
          for i from 0
          do (dolist (log (r:receipt-logs rc))
               (when (log-matches-p f log)
                 (push (rendered-log b i log-index log) out))
               (incf log-index)))
    (nreverse out)))

(defun raw-header-bloom (raw)
  "The offset of the logs bloom in raw block RAW, found without decoding the block."
  (multiple-value-bind (bs be) (rlp:rlp-item-bounds raw 0 (length raw))
    (declare (ignore be))
    (multiple-value-bind (hs he) (rlp:rlp-item-bounds raw bs (length raw))
      (let ((pos hs))
        (dotimes (i 3) (setf pos (nth-value 1 (rlp:rlp-item-bounds raw pos he))))
        (multiple-value-bind (is ie) (rlp:rlp-item-bounds raw pos he)
          (let ((p is))
            (dotimes (i 6) (setf p (nth-value 1 (rlp:rlp-item-bounds raw p ie))))
            (multiple-value-bind (s e) (rlp:rlp-item-bounds raw p ie)
              (invariant (= (- e s) 256) "a header bloom is 256 octets")
              s)))))))

(defun block-may-match-p (f n)
  "Whether block N's header bloom admits F (NIL when the backend lacks N): read from the
decoded block when it is cached, else from the raw block."
  (let ((b (cached-cblock n)))
    (if b
        (filter-matches-bloom-p f (clempo.headers:ethereum-header-logs-bloom (cblock-inner b)))
        (let ((raw (chain:backend-block-raw chain:*backend* n)))
          (and raw (filter-matches-bloom-p f raw (raw-header-bloom raw)))))))

(defun logs-in-range (f from to)
  (when (< to from) (rpc-fail +invalid-params+ "invalid block range params"))
  (when (> (- to from) *max-blocks-per-filter*)
    (rpc-fail +invalid-params+ (format nil "query exceeds max block range ~D" *max-blocks-per-filter*)))
  (let ((logs '()) (count 0))
    (loop for n from from to to
          when (block-may-match-p f n)
            do (let ((found (filter-logs-in-block f (chain-block n))))
                 (incf count (length found))
                 (setf logs (nreconc found logs))
                 (when (and (/= from to) (> count *max-logs-per-response*))
                   (rpc-fail +invalid-params+
                             (format nil "query exceeds max results ~D, retry with the range ~D-~D"
                                     *max-logs-per-response* from (if (= n from) from (1- n)))))))
    (coerce (nreverse logs) 'simple-vector)))

(defun range-number (tag)
  "A filter bound: NIL and pending mean unset."
  (if (or (null tag) (eq tag :pending)) nil (eth:tag-number tag)))

(defun get-logs (f)
  (let ((head chain:*head*))
    (if (filter-block-hash f)
        (let ((b (eth:resolve-block (list :hash (filter-block-hash f)))))
          (unless b (eth:block-not-found (list :hash (filter-block-hash f))))
          (if (filter-matches-bloom-p f (clempo.headers:ethereum-header-logs-bloom (cblock-inner b)))
              (coerce (filter-logs-in-block f b) 'simple-vector)
              #()))
        (let ((from (range-number (filter-from f))) (to (range-number (filter-to f))))
          (when (and to (> to head))
            (rpc-fail +invalid-params+
                      (format nil "block range extends beyond current head block: requested ~D, head ~D" to head)))
          (let ((from (or from head)) (to (or to head)))
            (when (> from to) (rpc-fail +invalid-params+ "invalid block range params"))
            (logs-in-range f from to))))))

(defrpc "eth_getLogs" ((f filter)) "logs matching a filter" (get-logs f))

;;; eth_feeHistory

(defparam fee-percentiles (src) "Option<Vec<f64>>"
  (let ((v '()))
    (serde:do-array (i src)
      (if (eq (serde:peek-kind src) :number)
          (push (coerce (serde:read-number src) 'double-float) v)
          (serde:invalid-type src "f64")))
    (nreverse v)))

(defun effective-tip (tx base-fee)
  (let ((type (clempo.transactions:transaction-type tx)))
    (if (member type '(0 1))
        (max 0 (- (clempo.transactions:transaction-gas-price tx) base-fee))
        (let ((max-fee (clempo.transactions:transaction-max-fee-per-gas tx)))
          (if (< max-fee base-fee) 0
              (min (clempo.transactions:transaction-max-priority-fee-per-gas tx) (- max-fee base-fee)))))))

(defun block-rewards (b percentiles)
  (let* ((base (or (cblock-base-fee b) 0))
         (receipts (cblock-receipts b))
         (prev 0)
         (txs (sort (loop for rc across receipts for i from 0
                          collect (prog1 (cons (- (r:receipt-cumulative-gas-used rc) prev)
                                               (effective-tip (svref (cblock-txs b) i) base))
                                    (setf prev (r:receipt-cumulative-gas-used rc))))
                    #'< :key #'cdr))
         (total (if (plusp (length receipts)) (r:receipt-cumulative-gas-used (svref receipts (1- (length receipts)))) 0))
         (index 0) (cumulative (if txs (car (first txs)) 0)))
    (loop for p in percentiles
          collect (if (null txs) 0
                      (let ((threshold (truncate (/ (* (coerce total 'double-float) p) 100d0))))
                        (loop while (and (< cumulative threshold) (< index (1- (length txs))))
                              do (incf index) (incf cumulative (car (nth index txs))))
                        (cdr (nth index txs)))))))

(defconstant +fee-history-cache-blocks+ 1124
  "reth FeeHistoryCacheConfig max_blocks (MAX_HEADER_HISTORY + 100): a running node's cache
holds the fee history of this many blocks up to its head.")

(defun quarter-percentile (p)
  "reth approximate_percentile at the cache's resolution 4: P rounded (half away from zero)
to a multiple of 0.25."
  (let* ((x (* p 4d0)) (r (ffloor x)))
    (/ (if (>= (- x r) 0.5d0) (+ r 1d0) r) 4d0)))

(defun fee-history (count newest percentiles)
  "reth's fee_history over headers and receipts. PERCENTILES is :ABSENT, or a list (possibly
empty). A range inside the last +FEE-HISTORY-CACHE-BLOCKS+ blocks is answered as reth's
fee history cache answers it once a running node has filled it: a reward row per block (empty
percentiles give empty rows) at percentiles rounded to 0.25. An older range takes reth's
cache-free path: exact percentiles, and no rows at all for empty percentiles."
  (when (zerop count) (return-from fee-history (json:jobject "gasUsedRatio" #() "oldestBlock" "0x0")))
  (when (and (listp percentiles)
             (or (> (length percentiles) 100)
                 (some (lambda (p) (or (< p 0) (> p 100))) percentiles)
                 (loop for (a b) on percentiles while b thereis (> a b))))
    (rpc-fail +invalid-params+ "invalid reward percentiles"))
  (let ((count (min count 1024))
        (newest (if (eq newest :pending) :latest newest)))
    (when (and (integerp newest) (> newest chain:*head*))
      (rpc-fail +invalid-params+ (format nil "request beyond head block: requested ~D, head ~D" newest chain:*head*)))
    (let* ((end (eth:tag-number newest))
           (count (min count (1+ end)))
           (start (- (1+ end) count))
           (blocks (loop for n from start to end collect (chain-block n)))
           (last (car (last blocks))))
      (apply #'json:jobject
             "baseFeePerGas"
             (coerce (append (mapcar (lambda (b) (q (or (cblock-base-fee b) 0))) blocks)
                             ;; reth asks the chain spec at the newest header's own timestamp
                             (list (q (clempo.chainspec:next-block-base-fee
                                       (cblock-timestamp last) (chain:backend-chain chain:*backend*) (cblock-base-fee last)
                                       (clempo.headers:ethereum-header-gas-used (cblock-inner last))))))
                     'simple-vector)
             "gasUsedRatio" (map 'simple-vector
                                 (lambda (b) (/ (coerce (clempo.headers:ethereum-header-gas-used (cblock-inner b)) 'double-float)
                                                (coerce (clempo.headers:ethereum-header-gas-limit (cblock-inner b)) 'double-float)))
                                 blocks)
             "baseFeePerBlobGas" (coerce (loop repeat (1+ count) collect "0x1") 'simple-vector)
             "blobGasUsedRatio" (coerce (loop repeat count collect 0d0) 'simple-vector)
             "oldestBlock" (q start)
             (when (listp percentiles)
               (list "reward" (if (>= start (- chain:*head* (1- +fee-history-cache-blocks+)))
                                  (let ((approximated (mapcar #'quarter-percentile percentiles)))
                                    (map 'simple-vector
                                         (lambda (b) (map 'simple-vector #'q (block-rewards b approximated)))
                                         blocks))
                                  (if percentiles
                                      (map 'simple-vector
                                           (lambda (b) (map 'simple-vector #'q (block-rewards b percentiles)))
                                           blocks)
                                      #()))))))))

(defrpc "eth_feeHistory" ((count u64) (newest block-number-or-tag)
                          (percentiles fee-percentiles :default :absent))
  "base fees, gas used ratios and reward percentiles over a range"
  (fee-history count newest percentiles))

;;; Installed filters (eth_newFilter and friends), kept in the server process.

(defstruct installed kind filter last-block last-poll)

(defvar *filters* (make-hash-table :test 'equal) "filter id -> installed, under *FILTERS-LOCK*")
(defvar *filters-lock* (sb-thread:make-mutex :name "installed filters"))

(defparameter *stale-filter-seconds* 300
  "reth DEFAULT_STALE_FILTER_TTL: a filter not polled for this long is removed.")

(defun evict-stale-filters (now)
  "Remove every filter whose last poll (or install) is *STALE-FILTER-SECONDS* old, as reth's
clear_stale_filters does; called with *FILTERS-LOCK* held."
  (let ((ttl (* 1000 *stale-filter-seconds*)))
    (loop for id being the hash-keys of *filters* using (hash-value it)
          when (>= (- now (installed-last-poll it)) ttl)
            collect id into stale
          finally (mapc (lambda (id) (remhash id *filters*)) stale))))

(defparam filter-id (src) "reth FilterId: untagged Num(u64) or Str(String)"
  ;; an untagged enum buffers the value first (numbers converted), then tries each variant
  (let ((v (if (eq (serde:peek-kind src) :number) (serde:read-number src) (serde:read-value src))))
    (cond ((stringp v) v)
          ((and (integerp v) (<= 0 v +u64-max+)) (format nil "~D" v))
          (t (serde:serde-fail "data did not match any variant of untagged enum FilterId")))))

(defun install (kind &optional filter)
  (let ((id (format nil "0x~(~32,'0x~)" (octets->integer (clempo.io:random-octets 16))))
        (now (clempo.io:now-ms)))
    (sb-thread:with-mutex (*filters-lock*)
      (evict-stale-filters now)
      (setf (gethash id *filters*)
            (make-installed :kind kind :filter filter :last-block chain:*head* :last-poll now)))
    id))

(defun poll-filter (id &optional advance-to)
  "Filter ID as polled now: (values installed previous-last-block). ADVANCE-TO moves its last
block, atomically, so concurrent polls never answer the same blocks twice."
  (let ((now (clempo.io:now-ms)))
    (sb-thread:with-mutex (*filters-lock*)
      (evict-stale-filters now)
      (let ((it (or (gethash id *filters*) (rpc-fail -32000 "filter not found"))))
        (setf (installed-last-poll it) now)
        (values it (prog1 (installed-last-block it)
                     (when advance-to (setf (installed-last-block it) advance-to))))))))

(defrpc "eth_newFilter" ((f filter)) "install a log filter" (install :logs f))
(defrpc "eth_newBlockFilter" () "install a new-block filter" (install :blocks))
(defrpc "eth_newPendingTransactionFilter" () "install a pending-transaction filter" (install :pending))
(defrpc "eth_uninstallFilter" ((id filter-id)) "remove a filter"
  (if (sb-thread:with-mutex (*filters-lock*) (remhash id *filters*)) :true :false))

(defrpc "eth_getFilterChanges" ((id filter-id)) "what a filter saw since its last poll"
  (let ((head chain:*head*))
    (multiple-value-bind (it last) (poll-filter id head)
      (let ((from (1+ last)))
        (ecase (installed-kind it)
          (:blocks (coerce (loop for n from from to head collect (hex32 (cblock-hash (chain-block n)))) 'simple-vector))
          (:pending #())
          (:logs
           (let ((f (installed-filter it)))
             (if (filter-block-hash f)
                 (get-logs f)
                 ;; reth get_filter_block_range: a fromBlock earlier than the poll start
                 ;; wins (so a past range answers its logs at every poll), a toBlock caps
                 ;; the head
                 (let* ((filter-from (range-number (filter-from f)))
                        (filter-to (range-number (filter-to f)))
                        (from (if filter-from (min from filter-from) from))
                        (to (if filter-to (min head filter-to) head)))
                   (if (> from to) #() (logs-in-range f from to)))))))))))

(defrpc "eth_getFilterLogs" ((id filter-id)) "every log a log filter matches"
  (let ((it (poll-filter id)))
    (unless (eq (installed-kind it) :logs) (rpc-fail -32000 "filter not found"))
    (get-logs (installed-filter it))))
