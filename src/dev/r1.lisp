(in-package #:clempo.dev)

(defparameter *r1-mainnet-windows*
  '(("T0" 0) ("T1/T1A" 4494230) ("T1B" 6253936) ("T1C" 8967990)
    ("T2" 12290227) ("T3" 16986044) ("T4" 20636964) ("T5" 24387406)
    ("T6" 26629191) ("T7" 29152418) ("T8" 32434902) ("T9" 33513646)
    ("T10" 35816049) ("T11" 38891821) ("T12" 43957545) ("T13" 47250841)))

(defun r1-cache-path (chain)
  (merge-pathnames (format nil "cache/clempo/r1/~A.jsonl" chain) (user-homedir-pathname)))

(defun r1-even-heights (start end count)
  (remove-duplicates (loop for i below count collect (+ start (floor (* i (- end start)) (max 1 (1- count)))))))

(defun r1-batch (calls url)
  (let ((results (clempo.jsonrpc:rpc-batch calls :url url)))
    (dolist (result results) (when (typep result 'error) (error result)))
    results))

(defun r1-window-sample (start end count url)
  (let* ((base (r1-even-heights start end count))
         (probes (set-difference (r1-even-heights start end (+ 2 count)) base))
         (counts (when probes
                   (r1-batch (mapcar (lambda (n) (list "eth_getBlockTransactionCountByNumber" (list (quantity->hex n)))) probes) url)))
         (busy (when probes
                 (car (sort (mapcar #'cons probes (mapcar #'hex->integer counts)) #'> :key #'cdr)))))
    (sort (if busy (cons (car busy) base) base) #'<)))

(defun r1-fetch-chain (chain url count)
  (let* ((tip (hex->integer (clempo.jsonrpc:rpc-call "eth_blockNumber" nil :url url)))
         (windows (if (string= chain "mainnet") *r1-mainnet-windows* '(("moderato" 0))))
         (path (r1-cache-path chain))
         (seen (make-hash-table :test 'eql))
         (written 0))
    (ensure-directories-exist path)
    ;; Resume only complete JSONL rows; existing samples remain evidence across tip changes.
    (when (probe-file path)
      (with-open-file (in path)
        (loop for line = (read-line in nil) while line
              for row = (clempo.json:parse-json line)
              do (setf (gethash (clempo.json:jget row "number") seen) t))))
    (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create)
      (loop for tail on windows for (fork start) = (first tail)
            for end = (min tip (if (rest tail) (1- (second (second tail))) tip))
            when (<= start end)
              do (dolist (height (r1-window-sample start end count url))
                   (unless (gethash height seen)
                     (let* ((tag (quantity->hex height))
                            (data (r1-batch (list (list "debug_getRawBlock" (list tag))
                                                  (list "eth_getBlockByNumber" (list tag :true))
                                                  (list "eth_getBlockReceipts" (list tag))) url)))
                       (invariant (and (stringp (first data)) (listp (second data)) (vectorp (third data)))
                                  "missing RPC block ~D" height)
                       (write-line (clempo.json:to-json-string
                                    (clempo.json:jobject "chain" chain "fork" fork "number" height
                                                         "source" url "raw" (first data)
                                                         "block" (second data) "receipts" (third data))) out)
                       (finish-output out)
                       (setf (gethash height seen) t)
                       (incf written)
                       (when (zerop (mod written 20)) (log-line "R1 ~A: wrote ~D blocks (~A)" chain written fork)))))))
    (log-line "R1 ~A: ~D new, ~D cached at ~A" chain written (hash-table-count seen) path)))

(defcommand "fetch-r1" (args)
  "fetch/resume cross-fork R1 samples [-chain mainnet|moderato|all] [-per-window 39]"
  (let ((chain (arg args :chain "all")))
    (when (member chain '("all" "mainnet") :test #'string=)
      (r1-fetch-chain "mainnet" (clempo.jsonrpc:local-rpc-url) (arg-int args :per-window 39)))
    (when (member chain '("all" "moderato") :test #'string=)
      (r1-fetch-chain "moderato" clempo.jsonrpc:+moderato-rpc+ (arg-int args :moderato-count 32)))
    (invariant (member chain '("all" "mainnet" "moderato") :test #'string=) "unknown chain ~A" chain))
  0)

(define-condition r1-mismatch (error)
  ((field :initarg :field :reader r1-mismatch-field))
  (:report (lambda (condition stream) (format stream "R1 mismatch: ~A" (r1-mismatch-field condition)))))

(defun r1-check (ok field)
  (unless ok (error 'r1-mismatch :field field)))

(defun r1-check-hash (actual expected field)
  (r1-check (and (stringp expected) (octets= actual (hex->octets expected))) field))

(defun r1-check-row (row)
  (let* ((raw (hex->octets (clempo.json:jget row "raw")))
         (block (clempo.blocks:decode-block raw))
         (header (clempo.headers:header-inner (clempo.blocks:block-header block)))
         (rpc (clempo.json:jget row "block"))
         (rpc-txs (clempo.json:jget rpc "transactions"))
         (rpc-receipts (clempo.json:jget row "receipts"))
         (transactions (clempo.blocks:block-transactions block))
         (receipts (map 'list #'clempo.receipts:receipt-from-rpc rpc-receipts)))
    (r1-check (octets= raw (clempo.blocks:encode-block block)) "block byte roundtrip")
    (r1-check-hash (clempo.blocks:block-hash block) (clempo.json:jget rpc "hash") "block hash")
    (r1-check (= (length transactions) (length rpc-txs) (length receipts)) "transaction/receipt count")
    (r1-check (octets= (clempo.blocks:transactions-root transactions)
                       (clempo.headers:ethereum-header-transactions-root header)) "transactions root")
    (loop for tx in transactions for tx-rpc across rpc-txs for receipt in receipts
          for receipt-rpc across rpc-receipts for index from 0
          do (r1-check-hash (clempo.transactions:transaction-hash tx) (clempo.json:jget tx-rpc "hash")
                            (format nil "transaction ~D hash" index))
             (r1-check (= (clempo.transactions:transaction-sender tx) (hex->integer (clempo.json:jget tx-rpc "from")))
                       (format nil "transaction ~D sender" index))
             (r1-check-hash (clempo.receipts:logs-bloom (clempo.receipts:receipt-logs receipt))
                            (clempo.json:jget receipt-rpc "logsBloom") (format nil "receipt ~D bloom" index))
             (r1-check-hash (clempo.transactions:transaction-hash tx) (clempo.json:jget receipt-rpc "transactionHash")
                            (format nil "receipt ~D transaction order" index))
             (when (= #x76 (clempo.transactions:transaction-type tx))
               (r1-check (eql (clempo.transactions:transaction-fee-payer tx) (clempo.receipts:receipt-fee-payer receipt))
                         (format nil "transaction ~D fee payer" index))
               (let ((key-id (clempo.transactions:transaction-key-id tx))
                     (rpc-key-id (clempo.json:jget* tx-rpc "signature" "keyId")))
                 (r1-check (eql key-id (when (stringp rpc-key-id) (hex->integer rpc-key-id)))
                           (format nil "transaction ~D key id" index)))
               (let ((signer (clempo.transactions:transaction-key-authorization-signer tx)))
                 (when signer
                   ;; RPC authorization metadata omits signer: its authority is the root sender.
                   (r1-check (= signer (hex->integer (clempo.json:jget tx-rpc "from")))
                             (format nil "transaction ~D key authorization signer" index))))))
    (r1-check (octets= (clempo.receipts:receipts-root receipts)
                       (clempo.headers:ethereum-header-receipts-root header)) "receipts root")
    (r1-check (octets= (clempo.receipts:receipts-bloom receipts)
                       (clempo.headers:ethereum-header-logs-bloom header)) "block logs bloom")
    t))

(defun r1-score-chain (chain)
  (let ((path (r1-cache-path chain)) (total 0) (passed 0) (failures nil)
        (forks (make-hash-table :test 'equal)) (start (clempo.io:monotonic-ticks)))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for line = (read-line in nil) while line
              for row = (clempo.json:parse-json line)
              do (incf total)
                 (incf (gethash (clempo.json:jget row "fork") forks 0))
                 (handler-case (when (r1-check-row row) (incf passed))
                   (error (condition)
                     (push (clempo.json:jobject "block" (clempo.json:jget row "number")
                                                "error" (princ-to-string condition)) failures))))))
    (clempo.json:jobject "chain" chain "score" (format nil "~D/~D" passed total)
                        "passed" passed "sampled" total
                        "seconds" (float (/ (- (clempo.io:monotonic-ticks) start) internal-time-units-per-second) 1d0)
                        "forks" (cons :object (sort (loop for k being the hash-keys of forks using (hash-value v) collect (cons k v)) #'string< :key #'car))
                        "failures" (coerce (nreverse failures) 'vector))))

(defrung "R1" (args)
  "check sampled blocks across forks; populate inputs with fetch-r1 [-chain mainnet|moderato|all]"
  (let* ((chain (arg args :chain "all"))
         (chains (if (string= chain "all") '("mainnet" "moderato") (list chain)))
         (results (mapcar (lambda (chain)
                            (let ((result (r1-score-chain chain)))
                              (when (zerop (clempo.json:jget result "sampled"))
                                (score-input-error "No samples in ~A; run bin/clempo-dev fetch-r1 -chain ~A."
                                                   (r1-cache-path chain) chain))
                              result))
                          chains))
         (passed (loop for result in results sum (clempo.json:jget result "passed")))
         (total (loop for result in results sum (clempo.json:jget result "sampled"))))
    (clempo.json:jobject "score" (format nil "~D/~D" passed total) "chains" (coerce results 'vector))))

(defcommand "bench-types" (args)
  "measure R1 decode/re-encode CPU, wall time and allocation [-chain mainnet] [-repeat 100]"
  (let* ((path (r1-cache-path (arg args :chain "mainnet")))
         (repeats (arg-int args :repeat 100))
         (raws (with-open-file (in path)
                 (loop for line = (read-line in nil) while line
                       collect (hex->octets (clempo.json:jget (clempo.json:parse-json line) "raw"))))))
    (invariant (plusp repeats) "repeat count must be positive")
    (let ((cpu (clempo.io:process-cpu-ticks)) (wall (clempo.io:monotonic-ticks))
          (bytes (sb-ext:get-bytes-consed)))
      (dotimes (i repeats)
        (dolist (raw raws) (clempo.blocks:encode-block (clempo.blocks:decode-block raw))))
      (let ((cpu-seconds (float (/ (- (clempo.io:process-cpu-ticks) cpu) internal-time-units-per-second) 1d0))
            (wall-seconds (float (/ (- (clempo.io:monotonic-ticks) wall) internal-time-units-per-second) 1d0))
            (allocated (- (sb-ext:get-bytes-consed) bytes)))
        (write-line
         (clempo.json:to-json-string
          (clempo.json:jobject "blocks" (* repeats (length raws)) "cpuSeconds" cpu-seconds
                               "wallSeconds" wall-seconds "bytesConsed" allocated))))))
  0)
