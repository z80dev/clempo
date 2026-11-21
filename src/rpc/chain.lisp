(defpackage #:clempo.rpc.chain
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:archive #:clempo.archive) (#:rlp #:clempo.rlp) (#:io #:clempo.io)
                    (#:rpc #:clempo.jsonrpc))
  (:documentation "Where the JSON-RPC server reads the chain. A backend is a CLOS object; the
server calls the BACKEND-* generic functions inside WITH-BACKEND-READ, which holds the
backend's read lock for the whole request and snapshots the head in *HEAD*, so a request
never sees a block that is half applied. A follower applies and commits a block inside
WITH-BACKEND-WRITE.

ARCHIVE-BACKEND serves the raw block archive (clempo.archive) with the hash index built by
BUILD-INDEX and a receipts store filled from an upstream node's debug_getRawReceipts,
each block's receipts checked against its header's receipts root before they are stored.")
  (:export #:backend #:backend-head #:backend-block-raw #:backend-block-number
           #:no-transaction-pool #:backend-submit-raw-transaction #:backend-pool-transactions
           #:backend-clear-pool
           #:backend-tx-location #:backend-receipts-raw #:backend-chain-id #:backend-chain
           #:with-backend-read #:with-backend-write #:call-outside-backend-read #:*backend* #:*head*
           #:archive-backend #:make-archive-backend #:archive-backend-head #:backend-state
           #:history-dir #:backend-history #:archive-backend-finalization-dir #:backend-record-receipts
           #:build-index #:index-dir #:index-covered-height
           #:block-hash-of-raw #:scan-block-hashes #:block-parts
           #:receipts-store-get #:receipts-store-put #:missing-receipts
           #:archive-backend-receipts #:archive-backend-upstream))

(in-package #:clempo.rpc.chain)

(defvar *backend* nil "The backend serving the current request.")
(defvar *head* nil "The head block number snapshotted at the start of the current request.")

(defclass backend ()
  ((lock :initform (sb-thread:make-mutex :name "backend") :reader backend-lock)
   (readers :initform 0 :accessor backend-readers)
   (queue :initform (sb-thread:make-waitqueue) :reader backend-queue)
   ;; The chain spec served: genesis, fork schedule, chain id (anything LOAD-CHAINSPEC takes).
   (chain :initarg :chain :initform :mainnet)))

(defmethod initialize-instance :after ((b backend) &key)
  (setf (slot-value b 'chain) (clempo.chainspec:load-chainspec (slot-value b 'chain))))

(defun backend-chain (backend)
  "BACKEND's chain spec; requests execute under it (CLEMPO.EXECUTOR:*CHAIN*)."
  (slot-value backend 'chain))

(defun backend-chain-id (backend) (clempo.chainspec:chain-id (backend-chain backend)))

(defgeneric backend-head (backend) (:documentation "The highest committed block number."))
(defgeneric backend-block-raw (backend number) (:documentation "Block NUMBER's RLP octets, or NIL."))
(defgeneric backend-block-number (backend hash)
  (:documentation "The number of the block whose hash is the octets HASH, or NIL."))
(defgeneric backend-tx-location (backend hash)
  (:documentation "(values block-number index) of the transaction with hash HASH, or NIL."))
(defgeneric backend-receipts-raw (backend number)
  (:documentation "Block NUMBER's EIP-2718 receipt envelopes as a list of octets, or NIL."))
(defgeneric backend-state (backend number)
  (:documentation "A read-only view of the state after block NUMBER, or NIL when the backend
has none (historical state). State methods answer an error for NIL.")
  (:method (backend number) (declare (ignore backend number)) nil))

;;; The transaction pool. A backend without one (the archive and a follower) serves an
;;; empty pool and refuses submissions with NO-TRANSACTION-POOL.

(define-condition no-transaction-pool (error) ()
  (:report "transaction pool unavailable"))

(defgeneric backend-submit-raw-transaction (backend raw)
  (:documentation "Decode and admit the EIP-2718 octets RAW into the backend's pool and return
the transaction's 32-octet hash. Signals CLEMPO.TXPOOL:POOL-ERROR when the pool rejects it.")
  (:method (backend raw) (declare (ignore backend raw)) (error 'no-transaction-pool)))

(defgeneric backend-pool-transactions (backend)
  (:documentation "A snapshot of the pool: a list of (transaction sender pending-p), where
SENDER is the recovered sender as an integer and PENDING-P is false for a queued
\(nonce-gapped) transaction.")
  (:method (backend) (declare (ignore backend)) nil))

(defgeneric backend-clear-pool (backend)
  (:documentation "Remove every transaction from the pool; return how many were removed.")
  (:method (backend) (declare (ignore backend)) 0))

;;; Readers and writers: many concurrent requests, one writer at a time between them.

(defun call-with-backend-read (backend thunk)
  (let ((lock (backend-lock backend)))
    (sb-thread:with-mutex (lock) (incf (backend-readers backend)))
    (unwind-protect
         (let ((*backend* backend) (*head* (backend-head backend))
               (clempo.executor:*chain* (backend-chain backend)))
           (funcall thunk))
      (sb-thread:with-mutex (lock)
        (when (zerop (decf (backend-readers backend)))
          (sb-thread:condition-broadcast (backend-queue backend)))))))

(defmacro with-backend-read ((backend) &body body)
  `(call-with-backend-read ,backend (lambda () ,@body)))

(defun call-outside-backend-read (thunk)
  "Run THUNK with the current request's read hold on *BACKEND* released, so a writer may commit
blocks meanwhile (a request that waits for a block). The hold is taken again before returning
and *HEAD* is snapshotted anew."
  (let* ((backend *backend*) (lock (backend-lock backend)))
    (sb-thread:with-mutex (lock)
      (when (zerop (decf (backend-readers backend)))
        (sb-thread:condition-broadcast (backend-queue backend))))
    (unwind-protect (funcall thunk)
      (sb-thread:with-mutex (lock) (incf (backend-readers backend)))
      (setf *head* (backend-head backend)))))

(defun call-with-backend-write (backend thunk)
  "Wait until no request is running, then run THUNK with new requests held back."
  (let ((lock (backend-lock backend)))
    (sb-thread:with-mutex (lock)
      (loop until (zerop (backend-readers backend))
            do (sb-thread:condition-wait (backend-queue backend) lock))
      (funcall thunk))))

(defmacro with-backend-write ((backend) &body body)
  `(call-with-backend-write ,backend (lambda () ,@body)))

;;; Raw block structure, without decoding transactions.

(defun block-parts (raw)
  "Bounds of the header item and of each transaction's EIP-2718 bytes in block RAW.
Returns (values header-start header-end tx-bounds) where tx-bounds is a list of (start . end)."
  (declare (type octets raw))
  (multiple-value-bind (bstart bend) (rlp:rlp-item-bounds raw 0 (length raw))
    (multiple-value-bind (hstart hend) (rlp:rlp-item-bounds raw bstart bend)
      (declare (ignore hstart))
      (multiple-value-bind (tstart tend) (rlp:rlp-item-bounds raw hend bend)
        (values bstart hend
                (loop with pos = tstart
                      while (< pos tend)
                      collect (multiple-value-bind (ps pe listp) (rlp:rlp-item-bounds raw pos tend)
                                (prog1 (if listp (cons pos pe) (cons ps pe))
                                  (setf pos pe)))))))))

(defun block-hash-of-raw (raw)
  (multiple-value-bind (hs he) (block-parts raw)
    (clempo.keccak:keccak256 raw :start hs :end he)))

(defun scan-block-hashes (raw)
  "(values block-hash tx-hashes) of block RAW."
  (multiple-value-bind (hs he txs) (block-parts raw)
    (values (clempo.keccak:keccak256 raw :start hs :end he)
            (mapcar (lambda (b) (clempo.keccak:keccak256 raw :start (car b) :end (cdr b))) txs))))

;;; The hash index: two files of 16-byte entries (u64 hash prefix, u64 value) sorted by
;;; prefix, preceded by a 65537-entry table of bucket offsets on the prefix's top 16 bits.
;;; blocks.idx maps a block hash prefix to the block number; txs.idx maps a transaction
;;; hash prefix to (block << 20 | index). Prefix matches are confirmed by the caller.

(defun index-dir (&optional (chain "mainnet"))
  (format nil "~A/cache/clempo/index/~A/" (io:getenv "HOME") chain))

(defconstant +buckets+ 65536)
(defconstant +index-header+ 24) ; "CLIX" pad u64 covered-height, u64 count

(defun hash-prefix (hash) (octets->integer hash :start 0 :end 8))

(defun index-path (dir name) (format nil "~A~A.idx" dir name))

(defun sort-entries (keys values)
  "Sort the parallel u64 vectors by key with a bucket pass on the top 16 bits. Returns
\(values keys values offsets)."
  (let* ((n (length keys))
         (counts (make-array (1+ +buckets+) :element-type 'fixnum :initial-element 0))
         (ok (make-array n :element-type '(unsigned-byte 64)))
         (ov (make-array n :element-type '(unsigned-byte 64))))
    (loop for k across keys do (incf (aref counts (1+ (ash k -48)))))
    (loop for i from 1 to +buckets+ do (incf (aref counts i) (aref counts (1- i))))
    (let ((fill (copy-seq counts)))
      (loop for k across keys for v across values
            do (let ((p (aref fill (ash k -48))))
                 (setf (aref ok p) k (aref ov p) v)
                 (incf (aref fill (ash k -48))))))
    ;; insertion sort inside each bucket (buckets are small)
    (loop for b from 0 below +buckets+
          do (loop for i from (1+ (aref counts b)) below (aref counts (1+ b))
                   do (let ((k (aref ok i)) (v (aref ov i)) (j (1- i)))
                        (loop while (and (>= j (aref counts b)) (> (aref ok j) k))
                              do (setf (aref ok (1+ j)) (aref ok j) (aref ov (1+ j)) (aref ov j))
                                 (decf j))
                        (setf (aref ok (1+ j)) k (aref ov (1+ j)) v))))
    (values ok ov counts)))

(defun write-index (path covered keys values)
  (multiple-value-bind (ok ov offsets) (sort-entries keys values)
    (let ((tmp (format nil "~A.tmp" path)))
      (ensure-directories-exist path)
      (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
        (let ((buf (make-octets (max +index-header+ (* 8 (1+ +buckets+))))))
          (replace buf (string->octets "CLIX"))
          (replace buf (integer->octets covered 8) :start1 8)
          (replace buf (integer->octets (length ok) 8) :start1 16)
          (write-sequence buf out :end +index-header+)
          (loop for i from 0 to +buckets+
                do (replace buf (integer->octets (aref offsets i) 8) :start1 (* 8 i)))
          (write-sequence buf out :end (* 8 (1+ +buckets+))))
        (let ((chunk (make-octets (* 16 65536))))
          (loop for start from 0 below (length ok) by 65536
                do (let ((n (min 65536 (- (length ok) start))))
                     (dotimes (i n)
                       (let ((k (aref ok (+ start i))) (v (aref ov (+ start i))) (p (* 16 i)))
                         (dotimes (j 8)
                           (setf (aref chunk (+ p j)) (ldb (byte 8 (* 8 (- 7 j))) k)
                                 (aref chunk (+ p 8 j)) (ldb (byte 8 (* 8 (- 7 j))) v)))))
                     (write-sequence chunk out :end (* 16 n))))))
      (sb-unix:unix-rename tmp path))))

(defun index-covered-height (&optional (dir (index-dir)))
  "The last block the index covers, or -1 when there is no index."
  (let ((path (index-path dir "blocks")))
    (if (probe-file path)
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (let ((h (make-octets +index-header+)))
            (read-sequence h in)
            (octets->integer h :start 8 :end 16)))
        -1)))

(defun build-index (&key (dir (index-dir)) (archive-dir (archive:archive-dir)) (threads 16)
                      (to (archive:archive-contiguous-height archive-dir)) (log t))
  "Scan archive chunks 0..TO and write blocks.idx and txs.idx. Returns (values blocks txs)."
  (let* ((firsts (loop for c from 0 to to by archive:+chunk-blocks+ collect c))
         (results (make-array (length firsts)))
         (next 0) (lock (sb-thread:make-mutex :name "index")) (done 0))
    (flet ((worker ()
             (loop
               (let ((i (sb-thread:with-mutex (lock) (when (< next (length firsts)) (prog1 next (incf next))))))
                 (unless i (return))
                 (let* ((first (nth i firsts))
                        (chunk (archive:read-chunk first archive-dir))
                        (bk '()) (tk '()))
                   (loop for n from first below (min (1+ to) (+ first archive:+chunk-blocks+))
                         do (multiple-value-bind (bh txs) (scan-block-hashes (archive:chunk-block chunk n))
                              (push (cons (hash-prefix bh) n) bk)
                              (loop for th in txs for j from 0
                                    do (push (cons (hash-prefix th) (logior (ash n 20) j)) tk))))
                   (setf (aref results i) (cons bk tk))
                   (sb-thread:with-mutex (lock)
                     (incf done)
                     (when (and log (zerop (mod done 100)))
                       (format *error-output* "~&index: ~D/~D chunks~%" done (length firsts))
                       (force-output *error-output*))))))))
      (mapc #'sb-thread:join-thread
            (loop repeat threads collect (sb-thread:make-thread #'worker :name "index"))))
    (flet ((flatten (key)
             (let* ((n (loop for r across results sum (length (funcall key r))))
                    (ks (make-array n :element-type '(unsigned-byte 64)))
                    (vs (make-array n :element-type '(unsigned-byte 64)))
                    (p 0))
               (loop for r across results
                     do (dolist (e (funcall key r))
                          (setf (aref ks p) (car e) (aref vs p) (cdr e)) (incf p)))
               (values ks vs))))
      (multiple-value-bind (bk bv) (flatten #'car)
        (write-index (index-path dir "blocks") to bk bv)
        (multiple-value-bind (tk tv) (flatten #'cdr)
          (write-index (index-path dir "txs") to tk tv)
          (values (length bk) (length tk)))))))

(defstruct (hindex (:conc-name hindex-)) path stream offsets covered
  (lock (sb-thread:make-mutex :name "index file")))

(defun open-index (path)
  (when (probe-file path)
    (let* ((in (open path :element-type '(unsigned-byte 8)))
           (h (make-octets +index-header+))
           (tab (make-octets (* 8 (1+ +buckets+))))
           (offsets (make-array (1+ +buckets+) :element-type '(unsigned-byte 64))))
      (read-sequence h in)
      (read-sequence tab in)
      (dotimes (i (1+ +buckets+))
        (setf (aref offsets i) (octets->integer tab :start (* 8 i) :end (* 8 (1+ i)))))
      (make-hindex :path path :stream in :offsets offsets
                  :covered (octets->integer h :start 8 :end 16)))))

(defun index-lookup (index prefix)
  "Every value stored under PREFIX."
  (let* ((b (ash prefix -48))
         (from (aref (hindex-offsets index) b)) (to (aref (hindex-offsets index) (1+ b)))
         (buf (make-octets (* 16 (- to from)))))
    (sb-thread:with-mutex ((hindex-lock index))
      (file-position (hindex-stream index) (+ +index-header+ (* 8 (1+ +buckets+)) (* 16 from)))
      (read-sequence buf (hindex-stream index)))
    (loop for i from 0 below (- to from)
          when (= prefix (octets->integer buf :start (* 16 i) :end (+ 8 (* 16 i))))
            collect (octets->integer buf :start (+ 8 (* 16 i)) :end (+ 16 (* 16 i))))))

;;; Receipts store: per archive chunk, an append-only file of records
;;;   u64 block | u32 length | RLP list of receipt envelopes

(defun receipts-dir (&optional (chain "mainnet"))
  (format nil "~A/cache/clempo/receipts/~A/" (io:getenv "HOME") chain))

(defstruct rstore dir (lock (sb-thread:make-mutex :name "receipts store"))
  (offsets (make-hash-table :synchronized t)))   ; chunk-first -> hash number -> (offset . length); filled lazily by RPC threads

(defun rstore-path (store first) (format nil "~Areceipts-~10,'0D.dat" (rstore-dir store) first))

(defun rstore-chunk-offsets (store first)
  (or (gethash first (rstore-offsets store))
      (setf (gethash first (rstore-offsets store))
            (let ((table (make-hash-table)) (path (rstore-path store first)))
              (when (probe-file path)
                (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((h (make-octets 12)))
                    (loop for pos = (file-position in)
                          while (= 12 (read-sequence h in))
                          do (let ((n (octets->integer h :start 0 :end 8))
                                   (len (octets->integer h :start 8 :end 12)))
                               (setf (gethash n table) (cons (+ pos 12) len))
                               (file-position in (+ pos 12 len)))))))
              table))))

(defun receipts-store-get (store n)
  (sb-thread:with-mutex ((rstore-lock store))
    (let* ((first (* archive:+chunk-blocks+ (floor n archive:+chunk-blocks+)))
           (entry (gethash n (rstore-chunk-offsets store first))))
      (when entry
        (with-open-file (in (rstore-path store first) :element-type '(unsigned-byte 8))
          (file-position in (car entry))
          (let ((o (make-octets (cdr entry))))
            (read-sequence o in)
            (rlp:rlp-decode o)))))))

(defun receipts-store-put (store n receipts)
  (sb-thread:with-mutex ((rstore-lock store))
    (let* ((first (* archive:+chunk-blocks+ (floor n archive:+chunk-blocks+)))
           (table (rstore-chunk-offsets store first)))
      (unless (gethash n table)
        (let ((payload (rlp:rlp-encode receipts)) (path (rstore-path store first)))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :append :if-does-not-exist :create)
            (let ((pos (file-position out)))
              (write-sequence (integer->octets n 8) out)
              (write-sequence (integer->octets (length payload) 4) out)
              (write-sequence payload out)
              (setf (gethash n table) (cons (+ pos 12) (length payload))))))))))

;;; The archive backend

(define-condition missing-receipts (error)
  ((number :initarg :number :reader missing-receipts-number))
  (:report (lambda (c s) (format s "receipts of block ~D are not available" (missing-receipts-number c)))))

(defclass archive-backend (backend)
  ((archive-dir :initarg :archive-dir :reader archive-backend-dir)
   (head :initarg :head :accessor archive-backend-head)
   (chunks :initform '() :accessor archive-backend-chunks)        ; MRU list of chunks
   (chunk-lock :initform (sb-thread:make-mutex :name "chunks") :reader chunk-lock)
   (blocks-index :initarg :blocks-index :reader blocks-index)
   (txs-index :initarg :txs-index :reader txs-index)
   (tail :initform nil :accessor archive-backend-tail)            ; (blocks-table . txs-table) above the index
   (receipts :initarg :receipts :reader archive-backend-receipts)
   (upstream :initarg :upstream :initform nil :reader archive-backend-upstream)
   (history :initarg :history :initform :unopened)                ; clempo.history, NIL or :unopened
   (history-dir :initarg :history-dir :initform nil)              ; NIL: HISTORY-DIR of the chain
   (finalization-dir :initarg :finalization-dir :initform nil     ; NIL: the chain's certificate archive
                     :reader archive-backend-finalization-dir)
   (history-lock :initform (sb-thread:make-mutex :name "history") :reader history-lock)
   (history-checked :initform 0 :accessor history-checked)))     ; last refresh, clempo.io:now-ms

(defun make-archive-backend (&rest initargs &key (class 'archive-backend) (archive-dir (archive:archive-dir))
                                                 (index-dir (index-dir)) head (upstream nil)
                                                 (receipts-dir (receipts-dir))
                              &allow-other-keys)
  "A backend over the raw block archive. HEAD defaults to the archive's contiguous height.
UPSTREAM, a JSON-RPC URL, fills the receipts store on first use. CLASS (a subclass of
ARCHIVE-BACKEND) is instantiated with the remaining INITARGS as well."
  (let ((extra (loop for (k v) on initargs by #'cddr
                     unless (member k '(:class :archive-dir :index-dir :head :upstream :receipts-dir))
                       append (list k v))))
    (apply #'make-instance class
           :archive-dir archive-dir
           :head (or head (archive:archive-contiguous-height archive-dir))
           :blocks-index (open-index (index-path index-dir "blocks"))
           :txs-index (open-index (index-path index-dir "txs"))
           :receipts (make-rstore :dir receipts-dir)
           :upstream upstream
           extra)))

(defmethod backend-head ((b archive-backend)) (archive-backend-head b))

;;; Historical state: a clempo.history directory built by `clempo-dev history-build`.

(defun history-dir (&optional (chain "mainnet"))
  "$CLEMPO_HISTORY, or ~/cache/clempo/history/<chain>/."
  (or (io:getenv "CLEMPO_HISTORY")
      (format nil "~A/cache/clempo/history/~A/" (io:getenv "HOME") chain)))

(defun backend-history (b)
  "B's history, opened on first use (NIL when the directory does not exist)."
  (sb-thread:with-mutex ((history-lock b))
    (let ((h (slot-value b 'history)))
      (if (eq h :unopened)
          (setf (slot-value b 'history)
                (let ((dir (or (slot-value b 'history-dir)
                               (history-dir (if (eql (backend-chain-id b) 4217) "mainnet" (backend-chain-id b))))))
                  (when (probe-file dir) (clempo.history:open-history dir))))
          h))))

(defparameter *history-refresh-seconds* 10
  "How often a miss looks for segments a running history build has added.")

(defmethod backend-state ((b archive-backend) number)
  "The state after block NUMBER from the history, or NIL when it does not cover it."
  (let ((h (backend-history b)))
    (when h
      (or (clempo.history:history-state h number)
          (let ((now (clempo.io:now-ms)))
            (when (sb-thread:with-mutex ((history-lock b))
                    (when (> (- now (history-checked b)) (* *history-refresh-seconds* 1000))
                      (setf (history-checked b) now)
                      (clempo.history:history-refresh h)
                      t))
              (clempo.history:history-state h number)))))))

(defun archive-chunk (b first)
  (sb-thread:with-mutex ((chunk-lock b))
    (let ((hit (assoc first (archive-backend-chunks b))))
      (if hit
          (progn (setf (archive-backend-chunks b) (cons hit (remove hit (archive-backend-chunks b))))
                 (cdr hit))
          (let ((chunk (archive:read-chunk first (archive-backend-dir b))))
            (push (cons first chunk) (archive-backend-chunks b))
            (when (> (length (archive-backend-chunks b)) 24)
              (setf (archive-backend-chunks b) (subseq (archive-backend-chunks b) 0 24)))
            chunk)))))

(defmethod backend-block-raw ((b archive-backend) n)
  (when (and (integerp n) (<= 0 n (archive-backend-head b)))
    (let ((first (* archive:+chunk-blocks+ (floor n archive:+chunk-blocks+))))
      ;; a chunk already read is immutable: only an unread one costs a probe (a stat call)
      (when (or (sb-thread:with-mutex ((chunk-lock b)) (assoc first (archive-backend-chunks b)))
                (archive:chunk-present-p first (archive-backend-dir b)))
        (archive:chunk-block (archive-chunk b first) n)))))

(defun backend-index-covered (b)
  (let ((i (blocks-index b))) (if i (hindex-covered i) -1)))

(defun tail-tables (b)
  "Hash tables for blocks above the index, built on first use."
  (let ((head (archive-backend-head b)))
    (sb-thread:with-mutex ((chunk-lock b))
      (let ((tail (archive-backend-tail b)))
        (if (and tail (= (car tail) head))
            (cdr tail)
            (let ((blocks (make-hash-table :test 'equalp :synchronized t))
                  (txs (make-hash-table :test 'equalp :synchronized t)))
              (setf (archive-backend-tail b) (list* head blocks txs))
              (cons blocks txs)))))))

(defun fill-tail (b tables)
  (let ((covered (backend-index-covered b)))
    (when (zerop (hash-table-count (car tables)))
      (loop for n from (1+ covered) to (archive-backend-head b)
            for raw = (backend-block-raw b n)
            while raw
            do (multiple-value-bind (bh ths) (scan-block-hashes raw)
                 (setf (gethash bh (car tables)) n)
                 (loop for th in ths for j from 0 do (setf (gethash th (cdr tables)) (cons n j))))))))

(defmethod backend-block-number ((b archive-backend) hash)
  (or (and (blocks-index b)
           (loop for n in (index-lookup (blocks-index b) (hash-prefix hash))
                 when (and (<= n (archive-backend-head b))
                           (octets= hash (block-hash-of-raw (backend-block-raw b n))))
                   return n))
      (let ((tables (tail-tables b)))
        (fill-tail b tables)
        (gethash hash (car tables)))))

(defmethod backend-tx-location ((b archive-backend) hash)
  (let ((loc (or (and (txs-index b)
                      (loop for v in (index-lookup (txs-index b) (hash-prefix hash))
                            for n = (ash v -20) for j = (ldb (byte 20 0) v)
                            when (and (<= n (archive-backend-head b))
                                      (let* ((raw (backend-block-raw b n))
                                             (tx (nth j (nth-value 2 (block-parts raw)))))
                                        (and tx (octets= hash (clempo.keccak:keccak256 raw :start (car tx) :end (cdr tx))))))
                              return (cons n j)))
                 (let ((tables (tail-tables b)))
                   (fill-tail b tables)
                   (gethash hash (cdr tables))))))
    (when loc (values (car loc) (cdr loc)))))

(defun fetch-receipts (url n)
  (let ((r (rpc:rpc-call "debug_getRawReceipts" (list (quantity->hex n)) :url url)))
    (map 'list #'hex->octets r)))

(defgeneric backend-record-receipts (backend number receipts)
  (:documentation "Persist block NUMBER's EIP-2718 receipt envelopes (a list of octets) in
BACKEND's receipts store, for a backend that executes blocks itself."))

(defmethod backend-record-receipts ((b archive-backend) number receipts)
  (receipts-store-put (archive-backend-receipts b) number receipts))

(defmethod backend-receipts-raw ((b archive-backend) n)
  (let ((store (archive-backend-receipts b)))
    (or (receipts-store-get store n)
        (let ((url (archive-backend-upstream b)))
          (unless url (error 'missing-receipts :number n))
          (let* ((receipts (fetch-receipts url n))
                 (raw (backend-block-raw b n))
                 (header (multiple-value-bind (hs he) (block-parts raw)
                           (clempo.headers:decode-header (subseq raw hs he))))
                 (root (clempo.trie:ordered-trie-root receipts)))
            (invariant (octets= root (clempo.headers:ethereum-header-receipts-root (clempo.headers:header-inner header)))
                       "upstream receipts of block ~D do not match its receipts root" n)
            (receipts-store-put store n receipts)
            receipts)))))
