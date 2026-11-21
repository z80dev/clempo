(defpackage #:clempo.rpc.sender
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:chain #:clempo.rpc.chain) (#:tx #:clempo.transactions)
                    (#:archive #:clempo.archive) (#:sig #:clempo.signatures))
  (:documentation "eth_getTransactionBySenderAndNonce without historical state.

reth answers it (after the pool, which this node does not have) by binary-searching the
blocks for the first one after which the sender's account nonce exceeds NONCE, then
returning the first transaction of that block whose signer is the sender and whose
nonce() is NONCE, or null. Tempo's 0x76 transactions make nonce() and the account nonce
differ: a nonzero nonce key (2D or expiring nonce) leaves the account nonce alone, except
that a first CREATE call bumps it; EIP-7702 and Tempo authorizations bump their
authority's nonce when it matches.

The sender index records, per address, every event that can move its account nonce or
answer the query, in chain order; looking a query up replays them to find that block.
Account nonce changes the chain does not show (a delegated account's code running CREATE,
genesis nonces of accounts that never send a protocol-nonce transaction) are not modelled.")
  (:export #:build-sender-index #:sender-index-path #:sender-events #:sender-nonce-block))

(in-package #:clempo.rpc.sender)

;;; Events. Each is three u64 words: the address key (the first 8 bytes of keccak256 of the
;;; address), the location (block << 20 | transaction index), and
;;; kind << 60 | sequence << 48 | nonce, so that the events of one transaction sort as they
;;; apply: the transaction's own nonce use, its authorizations in list order, then the
;;; nonce bump of a deferred CREATE.

(defconstant +protocol+ 1 "a protocol-nonce transaction with nonce m: the account nonce becomes m+1")
(defconstant +protocol-create+ 2
  "a 0x76 protocol-nonce transaction whose first call is CREATE: the nonce is m until the CREATE")
(defconstant +keyed+ 3 "a 0x76 transaction that does not use the account nonce (2D or expiring)")
(defconstant +authorization+ 6 "an authorization with nonce k: applies when the account nonce is k")
(defconstant +create-bump+ 7 "the CREATE of a 0x76 transaction: the account nonce goes up by one")

(defconstant +nonce-mask+ (1- (ash 1 48)) "stored nonces are clamped here; equality is confirmed")

(defun address-key (address)
  (octets->integer (clempo.keccak:keccak256 (integer->octets address 20)) :start 0 :end 8))

(defun event-word (kind seq nonce)
  (logior (ash kind 60) (ash (min seq #xfff) 48) (min nonce +nonce-mask+)))

(defun event-kind (word) (ldb (byte 4 60) word))
(defun event-nonce (word) (ldb (byte 48 0) word))
(defun location-block (loc) (ash loc -20))

(defun valid-authorization-chain-p (auth chain-id)
  (let ((c (clempo.rlp:rlp-decode-integer (first auth))))
    (or (zerop c) (= c chain-id))))

(defun transaction-events (tx sender chain-id emit)
  "Call EMIT with (address kind seq nonce) for each event of TX, sent by SENDER."
  (let ((type (tx:transaction-type tx)))
    (flet ((authorizations (list)
             (loop for auth in list for j from 0
                   when (valid-authorization-chain-p auth chain-id)
                     do (let ((authority (ignore-errors (sig:authorization-signer auth))))
                          (when authority
                            (funcall emit authority +authorization+ j
                                     (clempo.rlp:rlp-decode-integer (third auth))))))))
      (if (= type #x76)
          (let ((create (zerop (length (first (first (tx:transaction-calls tx))))))
                (keyed (plusp (tx:transaction-nonce-key tx))))
            (funcall emit sender (cond (keyed +keyed+) (create +protocol-create+) (t +protocol+))
                     0 (tx:transaction-nonce tx))
            (authorizations (tx:transaction-tempo-authorization-list tx))
            (when create (funcall emit sender +create-bump+ 0 0)))
          ;; System transactions (sender 0x0) never move its nonce, so the reference's search
          ;; never selects a block for them: they need no events.
          (unless (zerop sender)
            (funcall emit sender +protocol+ 0 (tx:transaction-nonce tx))
            (when (= type 4) (authorizations (tx:transaction-authorization-list tx))))))))

(defun block-events (raw number chain-id emit)
  "Call EMIT with (key location word) for every event of block RAW."
  (loop for (start . end) in (nth-value 2 (chain:block-parts raw))
        for i from 0
        do (let* ((tx (tx:decode-transaction (subseq raw start end) :copy-raw nil))
                  (loc (logior (ash number 20) i)))
             (transaction-events tx (values (tx:transaction-sender tx)) chain-id
                                 (lambda (address kind seq nonce)
                                   (funcall emit (address-key address) loc (event-word kind seq nonce)))))))

;;; The index file: "CLSN", u64 covered height, u64 event count, a 65537-entry table of
;;; event offsets by the key's top 16 bits, then 24-byte events sorted by key, stably, so
;;; each address's events stay in chain order.

(defconstant +buckets+ 65536)
(defconstant +header+ 24)
(defconstant +table+ (* 8 (1+ +buckets+)))

(defun sender-index-path (&optional (dir (chain:index-dir))) (format nil "~Asenders.idx" dir))

(defun put-u64 (o pos v) (dotimes (j 8) (setf (aref o (+ pos j)) (ldb (byte 8 (* 8 (- 7 j))) v))))
(defun get-u64 (o pos) (octets->integer o :start pos :end (+ pos 8)))

(defun write-sender-index (path covered parts)
  "PARTS: per chunk, in chain order, a vector of (key loc word) triples."
  (let* ((n (loop for p across parts sum (floor (length p) 3)))
         (counts (make-array (1+ +buckets+) :element-type 'fixnum :initial-element 0))
         (ks (make-array n :element-type '(unsigned-byte 64)))
         (ls (make-array n :element-type '(unsigned-byte 64)))
         (ws (make-array n :element-type '(unsigned-byte 64))))
    (loop for p across parts
          do (loop for i from 0 below (length p) by 3 do (incf (aref counts (1+ (ash (aref p i) -48))))))
    (loop for i from 1 to +buckets+ do (incf (aref counts i) (aref counts (1- i))))
    (let ((fill (copy-seq counts)))
      (loop for p across parts
            do (loop for i from 0 below (length p) by 3
                     do (let ((d (aref fill (ash (aref p i) -48))))
                          (setf (aref ks d) (aref p i) (aref ls d) (aref p (+ i 1)) (aref ws d) (aref p (+ i 2)))
                          (incf (aref fill (ash (aref p i) -48)))))))
    (loop for b from 0 below +buckets+
          for lo = (aref counts b) for hi = (aref counts (1+ b))
          unless (loop for i from (1+ lo) below hi always (<= (aref ks (1- i)) (aref ks i)))
            do (let ((perm (stable-sort (coerce (loop for i from lo below hi collect i) 'simple-vector)
                                        #'< :key (lambda (i) (aref ks i)))))
                 (let ((k (map 'vector (lambda (i) (aref ks i)) perm))
                       (l (map 'vector (lambda (i) (aref ls i)) perm))
                       (w (map 'vector (lambda (i) (aref ws i)) perm)))
                   (replace ks k :start1 lo) (replace ls l :start1 lo) (replace ws w :start1 lo))))
    (let ((tmp (format nil "~A.tmp" path)))
      (ensure-directories-exist path)
      (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (let ((head (make-octets (+ +header+ +table+))))
          (replace head (string->octets "CLSN"))
          (put-u64 head 8 covered)
          (put-u64 head 16 n)
          (loop for i from 0 to +buckets+ do (put-u64 head (+ +header+ (* 8 i)) (aref counts i)))
          (write-sequence head out))
        (let ((buf (make-octets (* 24 65536))))
          (loop for start from 0 below n by 65536
                do (let ((m (min 65536 (- n start))))
                     (dotimes (i m)
                       (put-u64 buf (* 24 i) (aref ks (+ start i)))
                       (put-u64 buf (+ 8 (* 24 i)) (aref ls (+ start i)))
                       (put-u64 buf (+ 16 (* 24 i)) (aref ws (+ start i))))
                     (write-sequence buf out :end (* 24 m))))))
      (sb-unix:unix-rename tmp path))
    n))

(defun build-sender-index (&key (dir (chain:index-dir)) (archive-dir (archive:archive-dir)) (threads 16)
                             (to (archive:archive-contiguous-height archive-dir)) (chain-id 4217) (log t))
  "Recover the sender of every transaction in archive blocks 0..TO and write senders.idx.
Returns (values events transactions)."
  (let* ((firsts (coerce (loop for c from 0 to to by archive:+chunk-blocks+ collect c) 'simple-vector))
         (parts (make-array (length firsts)))
         (txs 0) (next 0) (done 0) (lock (sb-thread:make-mutex :name "sender index")))
    (flet ((worker ()
             (loop
               (let ((i (sb-thread:with-mutex (lock) (when (< next (length firsts)) (prog1 next (incf next))))))
                 (unless i (return))
                 (let* ((first (svref firsts i))
                        (chunk (archive:read-chunk first archive-dir))
                        (out (make-array 4096 :element-type '(unsigned-byte 64) :adjustable t :fill-pointer 0))
                        (count 0))
                   (loop for n from first below (min (1+ to) (+ first archive:+chunk-blocks+))
                         for raw = (archive:chunk-block chunk n)
                         do (incf count (length (nth-value 2 (chain:block-parts raw))))
                            (block-events raw n chain-id
                                          (lambda (k l w)
                                            (vector-push-extend k out) (vector-push-extend l out)
                                            (vector-push-extend w out))))
                   (setf (svref parts i) (coerce out '(simple-array (unsigned-byte 64) (*))))
                   (sb-thread:with-mutex (lock)
                     (incf txs count)
                     (incf done)
                     (when (and log (zerop (mod done 100)))
                       (format *error-output* "~&sender index: ~D/~D chunks, ~D transactions~%"
                               done (length firsts) txs)
                       (force-output *error-output*))))))))
      (mapc #'sb-thread:join-thread
            (loop repeat threads collect (sb-thread:make-thread #'worker :name "sender index"))))
    (values (write-sender-index (sender-index-path dir) to parts) txs)))

;;; Reading the index

(defstruct (sindex (:conc-name sindex-)) path stream offsets covered date
  (lock (sb-thread:make-mutex :name "sender index file")))

(defvar *indexes* (make-hash-table :test 'equal :synchronized t))

(defun open-sender-index (path)
  "The open index at PATH, reopened when the file was rebuilt, or NIL when there is none."
  (let ((date (and (probe-file path) (file-write-date path)))
        (old (gethash path *indexes*)))
    (cond ((null date) nil)
          ((and old (eql date (sindex-date old))) old)
          (t (let* ((in (open path :element-type '(unsigned-byte 8)))
                    (head (make-octets (+ +header+ +table+)))
                    (offsets (make-array (1+ +buckets+) :element-type '(unsigned-byte 64))))
               (read-sequence head in)
               (invariant (string= "CLSN" (octets->string head :start 0 :end 4)) "bad sender index ~A" path)
               (dotimes (i (1+ +buckets+)) (setf (aref offsets i) (get-u64 head (+ +header+ (* 8 i)))))
               (when old (close (sindex-stream old)))
               (setf (gethash path *indexes*)
                     (make-sindex :path path :stream in :offsets offsets :covered (get-u64 head 8) :date date)))))))

(defun read-events (index from count)
  (let ((buf (make-octets (* 24 count))) (in (sindex-stream index)))
    (file-position in (+ +header+ +table+ (* 24 from)))
    (read-sequence buf in)
    buf))

(defun index-events (index key)
  "The (location . word) events stored under KEY, in chain order."
  (sb-thread:with-mutex ((sindex-lock index))
    (let* ((b (ash key -48))
           (lo (aref (sindex-offsets index) b)) (hi (aref (sindex-offsets index) (1+ b))))
      ;; first entry with key >= KEY
      (loop while (< lo hi)
            do (let ((mid (floor (+ lo hi) 2)))
                 (if (< (get-u64 (read-events index mid 1) 0) key) (setf lo (1+ mid)) (setf hi mid))))
      (let ((end (aref (sindex-offsets index) (1+ b))) (out '()))
        (loop for pos from lo below end by 4096
              for m = (min 4096 (- end pos))
              for buf = (read-events index pos m)
              do (dotimes (i m)
                   (unless (= key (get-u64 buf (* 24 i))) (return-from index-events (nreverse out)))
                   (push (cons (get-u64 buf (+ 8 (* 24 i))) (get-u64 buf (+ 16 (* 24 i)))) out)))
        (nreverse out)))))

;;; Blocks above the index, scanned on demand and kept per backend

(defparameter *max-tail* 200000
  "The most blocks above the index scanned per request; beyond it the index must be rebuilt.")

(defstruct tail (head -1) (table (make-hash-table)) (lock (sb-thread:make-mutex :name "sender tail")))

(defvar *tails* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defun tail-events (backend covered head key)
  (let ((tail (or (gethash backend *tails*) (setf (gethash backend *tails*) (make-tail)))))
    (sb-thread:with-mutex ((tail-lock tail))
      (when (< (tail-head tail) covered)
        (clrhash (tail-table tail)) (setf (tail-head tail) covered))
      (when (> (- head (tail-head tail)) *max-tail*)
        (internal-error (format nil "sender index covers blocks up to ~D, the head is ~D: run clempo-dev rpc-sender-index"
                                covered head)))
      (loop for n from (1+ (tail-head tail)) to head
            for raw = (chain:backend-block-raw backend n)
            do (invariant raw "block ~D is missing below the head" n)
               (block-events raw n (chain:backend-chain-id backend)
                             (lambda (k l w) (push (cons l w) (gethash k (tail-table tail)))))
               (setf (tail-head tail) n))
      (reverse (gethash key (tail-table tail))))))

(defun sender-events (address &key (backend chain:*backend*) (head chain:*head*) (path (sender-index-path)))
  "Every indexed event of ADDRESS at or below HEAD, as (location . word) in chain order."
  (let* ((index (open-sender-index path))
         (covered (if index (sindex-covered index) -1))
         (key (address-key address)))
    (remove-if (lambda (e) (> (location-block (car e)) head))
               (append (and index (index-events index key))
                       (and (< covered head) (tail-events backend covered head key))))))

;;; The query

(defun sender-nonce-block (events nonce)
  "The first block after which the account nonce replayed from EVENTS exceeds NONCE, or NIL."
  (let ((account 0))
    (loop for ((loc . word) . rest) on events
          do (let ((kind (event-kind word)) (n (event-nonce word)))
               (cond ((= kind +protocol+) (setf account (1+ n)))
                     ((= kind +protocol-create+) (setf account n))
                     ((= kind +authorization+) (when (= account n) (incf account)))
                     ((= kind +create-bump+) (incf account))))
             (when (and (> account nonce)
                        (or (null rest) (/= (location-block (caar rest)) (location-block loc))))
               (return (location-block loc))))))

(defun candidate-p (word nonce)
  (and (member (event-kind word) (list +protocol+ +protocol-create+ +keyed+))
       (= (event-nonce word) (min nonce +nonce-mask+))))

(defrpc "eth_getTransactionBySenderAndNonce" ((address address) (nonce u64))
  "the transaction that used NONCE of ADDRESS, found as reth's nonce binary search finds it"
  (let* ((events (sender-events address))
         (number (and (some (lambda (e) (candidate-p (cdr e) nonce)) events)
                      (sender-nonce-block events nonce)))
         (b (and number (chain-block number))))
    (or (and b
             (loop for i from 0 below (length (cblock-txs b))
                   when (and (eql (cblock-sender b i) address)
                             (= (tx:transaction-nonce (svref (cblock-txs b) i)) nonce))
                     return (tx-json b i)))
        :null)))
