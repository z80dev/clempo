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
  (topics nil))            ; list per position: NIL (any) or a list of 32-byte octets

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

(defun bloom-has-p (bloom bytes)
  "BLOOM (256 octets) contains the 3 bits of keccak(BYTES)."
  (let ((h (clempo.keccak:keccak256 bytes)))
    (loop for i from 0 below 6 by 2
          always (let* ((bit (logand (logior (ash (aref h i) 8) (aref h (1+ i))) 2047))
                        (byte (- 255 (floor bit 8))))
                   (logbitp (mod bit 8) (aref bloom byte))))))

(defun filter-matches-bloom-p (f bloom)
  (and (or (null (filter-addresses f))
           (some (lambda (a) (bloom-has-p bloom (integer->octets a 20))) (filter-addresses f)))
       (every (lambda (set) (or (null set) (some (lambda (topic) (bloom-has-p bloom topic)) set)))
              (filter-topics f))))

(defun log-matches-p (f log)
  (and (or (null (filter-addresses f)) (member (r:event-log-address log) (filter-addresses f)))
       (loop for set in (filter-topics f)
             for i from 0
             always (or (null set)
                        (let ((topic (nth i (r:event-log-topics log))))
                          (and topic (member topic set :test #'octets=)))))))

(defun filter-logs-in-block (f b)
  "The rendered logs of cached block B that match F."
  (let ((out '()) (log-index 0))
    (loop for rc across (cblock-receipts b)
          for i from 0
          do (dolist (log (r:receipt-logs rc))
               (when (log-matches-p f log)
                 (push (log-json b i log-index i log) out))
               (incf log-index)))
    (nreverse out)))

(defun raw-header-bloom (raw)
  "The logs bloom of raw block RAW, read without decoding the block."
  (multiple-value-bind (bs be) (rlp:rlp-item-bounds raw 0 (length raw))
    (declare (ignore be))
    (multiple-value-bind (hs he) (rlp:rlp-item-bounds raw bs (length raw))
      (let ((pos hs))
        (dotimes (i 3) (setf pos (nth-value 1 (rlp:rlp-item-bounds raw pos he))))
        (multiple-value-bind (is ie) (rlp:rlp-item-bounds raw pos he)
          (let ((p is))
            (dotimes (i 6) (setf p (nth-value 1 (rlp:rlp-item-bounds raw p ie))))
            (multiple-value-bind (s e) (rlp:rlp-item-bounds raw p ie)
              (subseq raw s e))))))))

(defun logs-in-range (f from to)
  (when (< to from) (rpc-fail +invalid-params+ "invalid block range params"))
  (when (> (- to from) *max-blocks-per-filter*)
    (rpc-fail +invalid-params+ (format nil "query exceeds max block range ~D" *max-blocks-per-filter*)))
  (let ((logs '()) (count 0))
    (loop for n from from to to
          for raw = (chain:backend-block-raw chain:*backend* n)
          when (and raw (filter-matches-bloom-p f (raw-header-bloom raw)))
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

(defun fee-history (count newest percentiles)
  "reth's fee_history over headers and receipts (the cache-free path). PERCENTILES is :ABSENT,
or a list (possibly empty)."
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
                                       (cblock-timestamp last) :mainnet (cblock-base-fee last)
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
               (list "reward" (if percentiles
                                  (map 'simple-vector
                                       (lambda (b) (map 'simple-vector #'q (block-rewards b percentiles)))
                                       blocks)
                                  #())))))))

(defrpc "eth_feeHistory" ((count u64) (newest block-number-or-tag)
                          (percentiles fee-percentiles :default :absent))
  "base fees, gas used ratios and reward percentiles over a range"
  (fee-history count newest percentiles))

;;; Installed filters (eth_newFilter and friends), kept in the server process.

(defstruct installed kind filter last-block)

(defvar *filters* (make-hash-table :test 'equal :synchronized t) "filter id -> installed")

(defparam filter-id (src) "reth FilterId: untagged Num(u64) or Str(String)"
  ;; an untagged enum buffers the value first (numbers converted), then tries each variant
  (let ((v (if (eq (serde:peek-kind src) :number) (serde:read-number src) (serde:read-value src))))
    (cond ((stringp v) v)
          ((and (integerp v) (<= 0 v +u64-max+)) (format nil "~D" v))
          (t (serde:serde-fail "data did not match any variant of untagged enum FilterId")))))

(defun install (kind &optional filter)
  (let ((id (format nil "0x~(~32,'0x~)" (octets->integer (clempo.io:random-octets 16)))))
    (setf (gethash id *filters*) (make-installed :kind kind :filter filter :last-block chain:*head*))
    id))

(defun installed-or-fail (id)
  (or (gethash id *filters*) (rpc-fail -32000 "filter not found")))

(defrpc "eth_newFilter" ((f filter)) "install a log filter" (install :logs f))
(defrpc "eth_newBlockFilter" () "install a new-block filter" (install :blocks))
(defrpc "eth_newPendingTransactionFilter" () "install a pending-transaction filter" (install :pending))
(defrpc "eth_uninstallFilter" ((id filter-id)) "remove a filter"
  (if (remhash id *filters*) :true :false))

(defrpc "eth_getFilterChanges" ((id filter-id)) "what a filter saw since its last poll"
  (let* ((it (installed-or-fail id)) (from (1+ (installed-last-block it))) (head chain:*head*))
    (setf (installed-last-block it) head)
    (ecase (installed-kind it)
      (:blocks (coerce (loop for n from from to head collect (hex32 (cblock-hash (chain-block n)))) 'simple-vector))
      (:pending #())
      (:logs (if (> from head) #() (logs-in-range (installed-filter it) from head))))))

(defrpc "eth_getFilterLogs" ((id filter-id)) "every log a log filter matches"
  (let ((it (installed-or-fail id)))
    (unless (eq (installed-kind it) :logs) (rpc-fail -32000 "filter not found"))
    (get-logs (installed-filter it))))
