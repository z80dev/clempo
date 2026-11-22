(defpackage #:clempo.rpc.hardening-tests
  (:use #:cl #:clempo.base #:clempo.rpc.fixture)
  (:local-nicknames (#:json #:clempo.json) (#:rpc #:clempo.rpc) (#:chain #:clempo.rpc.chain)
                    (#:render #:clempo.rpc.render) (#:sender #:clempo.rpc.sender)
                    (#:logs #:clempo.rpc.logs) (#:store #:clempo.store)))
(in-package #:clempo.rpc.hardening-tests)

;;; Limits on what a client can make the server hold, and shared caches under concurrent
;;; requests.

(defun handle (backend body)
  (json:parse-json (chain:with-backend-read (backend) (nth-value 1 (rpc:handle-body body)))))

(defun batch-of (entry n)
  (with-output-to-string (s)
    (write-char #\[ s)
    (dotimes (i n) (when (plusp i) (write-char #\, s)) (write-string entry s))
    (write-char #\] s)))

;;; Batches: jsonrpsee's entry cap, and nesting that costs one bit a level

(deftest batch-entries-are-capped-as-jsonrpsee ()
  ;; The 15 MiB request cap over the shortest request and its comma (reth c82820f
  ;; handle_rpc_call): every batch of valid requests fits, and the entry beyond the cap
  ;; refuses the batch, whatever follows it.
  (is= 524288 rpc::+batch-max+)
  (let ((too-many (json:parse-json
                   "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32010,\"message\":\"The batch request was too large\",\"data\":\"Exceeded max limit of 524288\"}}")))
    (is (json:json-equal too-many (json:parse-json (nth-value 1 (rpc:handle-body (batch-of "0" (1+ rpc::+batch-max+)))))))
    (let ((tail (batch-of "0" (+ 2 rpc::+batch-max+))))
      ;; the batch is refused at the extra entry: the malformed rest is never read
      (is (json:json-equal too-many (json:parse-json (nth-value 1 (rpc:handle-body (concatenate 'string (subseq tail 0 (1- (length tail))) ",}"))))))))
  (let ((answers (json:parse-json (nth-value 1 (rpc:handle-body (batch-of "0" rpc::+batch-max+))))))
    (is= rpc::+batch-max+ (length answers))
    (is (every (lambda (a) (= -32600 (json:jget* a "error" "code"))) answers)))
  ;; a malformed batch below the cap is still a parse error
  (is= -32700 (json:jget* (json:parse-json (nth-value 1 (rpc:handle-body "[0,0,}"))) "error" "code")))

(deftest deep-batch-entries-cost-linear-work ()
  ;; serde_json skips a raw entry with no recursion limit, so the reference answers an entry
  ;; nested to any depth as an invalid request; clempo checks it in one pass with one bit a
  ;; level, building no values. 7 million levels fill the 15 MiB body cap.
  (let* ((depth 7000000)
         (deep (concatenate 'string "[" (make-string depth :initial-element #\[)
                            (make-string depth :initial-element #\]) ",{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"web3_clientVersion\",\"params\":"
                            (make-string 100000 :initial-element #\[) (make-string 100000 :initial-element #\]) "}]"))
         (start (get-internal-real-time))
         (answer (json:parse-json (nth-value 1 (rpc:handle-body deep))))
         (ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
    (is= 2 (length answer))
    (is= -32600 (json:jget* (aref answer 0) "error" "code"))
    (is (json:jget (aref answer 1) "result") "a deep param the method ignores is still answered")
    (is (< ms 5000) "~D ms for ~D levels" ms depth))
  (is= -32700 (json:jget* (json:parse-json (nth-value 1 (rpc:handle-body (format nil "[~A" (make-string 1000 :initial-element #\[)))))
                          "error" "code")))

;;; Installed filters: time eviction and a count cap

(deftest installed-filters-are-capped ()
  (let ((b (make-test-backend)) (old logs::*filters-max*))
    (sb-thread:with-mutex (logs::*filters-lock*) (clrhash logs::*filters*))
    (flet ((call (method &rest params)
             (handle b (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":[~{~S~^,~}]}" method params))))
      (unwind-protect
           (progn
             (setf logs::*filters-max* 3)
             (let ((ids (loop repeat 3 collect (json:jget (call "eth_newBlockFilter") "result"))))
               (is (every #'stringp ids))
               (dolist (method '("eth_newBlockFilter" "eth_newPendingTransactionFilter"))
                 (is (json:json-equal (json:jobject "code" -32000 "message" "too many installed filters")
                                      (json:jget (call method) "error"))))
               (is (json:json-equal (json:jobject "code" -32000 "message" "too many installed filters")
                                    (json:jget (handle b "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_newFilter\",\"params\":[{}]}") "error")))
               (is= :true (json:jget (call "eth_uninstallFilter" (first ids)) "result"))
               (is (stringp (json:jget (call "eth_newBlockFilter") "result")))
               ;; stale filters are evicted before the count is checked
               (let ((logs::*stale-filter-seconds* 0))
                 (setf logs::*filters-max* 1)
                 (is (stringp (json:jget (call "eth_newBlockFilter") "result"))))
               (is= 1 (hash-table-count logs::*filters*))))
        (setf logs::*filters-max* old)
        (sb-thread:with-mutex (logs::*filters-lock*) (clrhash logs::*filters*))))))

;;; Shared decoded blocks: senders and receipts filled in once under concurrent requests

(defun call-counted (name thunk)
  "THUNK's value and how many times the global function NAME ran meanwhile."
  (let ((count (list 0)))
    (sb-int:encapsulate name 'count-calls
                        (lambda (f &rest args)
                          (sb-ext:atomic-incf (car count))
                          (apply f args)))
    (unwind-protect (values (funcall thunk) (car count))
      (sb-int:unencapsulate name 'count-calls))))

(defun run-together (n fn)
  "Run FN on N threads released together; their values in thread order."
  (let* ((gate (sb-thread:make-mutex :name "gate"))
         (threads (sb-thread:with-mutex (gate)
                    (loop for i below n
                          collect (let ((i i))
                                    (sb-thread:make-thread
                                     (lambda () (sb-thread:with-mutex (gate)) (funcall fn i))
                                     :name "racer"))))))
    (mapcar #'sb-thread:join-thread threads)))

(deftest cblock-senders-and-receipts-decode-once ()
  (let* ((b (make-test-backend :head *block*))
         (raw (chain:backend-block-raw b *block*)))
    (dotimes (round 4)
      (let ((cb (render:decoded-cblock (clempo.blocks:decode-block raw) raw)))
        (multiple-value-bind (senders calls)
            (call-counted 'clempo.transactions:transaction-sender
                          (lambda ()
                            (run-together 16 (lambda (i) (render:cblock-sender cb (mod i 2))))))
          (is= 2 calls "round ~D: ~D sender recoveries for 2 transactions" round calls)
          (is (loop for s in senders for i from 0 always (eql s (nth (mod i 2) senders)))))
        (multiple-value-bind (vectors calls)
            (call-counted 'clempo.receipts:decode-receipt
                          (lambda ()
                            (run-together 16 (lambda (i) (declare (ignore i))
                                               (let ((chain:*backend* b)) (render:cblock-receipts cb))))))
          (is= 2 calls "round ~D: ~D receipt decodes for 2 receipts" round calls)
          (is (every (lambda (v) (eq v (first vectors))) vectors)))))))

;;; The sender index file replaced while requests read it

(defun index-path ()
  (format nil "~Aclempo-senders-~(~16,'0x~)/senders.idx" (clempo.io:temp-directory)
          (octets->integer (clempo.io:random-octets 8))))

(deftest sender-index-replaced-under-readers ()
  (let* ((path (index-path))
         (key #x0123456789abcdef)
         (events (loop for i below 300 collect (cons (ash (+ 10 i) 20) (+ (ash 1 60) i))))
         (part (let ((v (make-array (* 3 (length events)) :element-type '(unsigned-byte 64))))
                 (loop for (loc . word) in events for j from 0 by 3
                       do (setf (aref v j) key (aref v (+ j 1)) loc (aref v (+ j 2)) word))
                 v))
         (parts (vector part))
         (stop nil) (failures (list 0)) (reads (list 0)))
    (flet ((rebuild (i)
             (sender::write-sender-index path (+ 1000 i) parts)
             ;; a distinct modification time each time: the reader reopens every rebuild
             (sb-posix:utimes path (+ 1000000 i) (+ 1000000 i))))
      (unwind-protect
           (progn
             (rebuild 0)
             (let ((readers (loop repeat 8
                                  collect (sb-thread:make-thread
                                           (lambda ()
                                             (loop until stop
                                                   do (handler-case
                                                          (unless (equal events (sender::index-events (sender::open-sender-index path) key))
                                                            (sb-ext:atomic-incf (car failures)))
                                                        (error () (sb-ext:atomic-incf (car failures))))
                                                      (sb-ext:atomic-incf (car reads))))
                                           :name "index reader"))))
               (loop for i from 1 to 200 do (rebuild i))
               (setf stop t)
               (mapc #'sb-thread:join-thread readers))
             (is (plusp (car reads)))
             (is= 0 (car failures) "~D of ~D reads failed" (car failures) (car reads))
             (is= 1200 (sender::sindex-covered (sender::open-sender-index path))))
        (let ((old (gethash path sender::*indexes*)))
          (when old (close (sender::sindex-stream old)) (remhash path sender::*indexes*)))
        (sb-ext:delete-directory (directory-namestring path) :recursive t)))))

;;; eth_call executes on a throwaway overlay: no code it creates reaches the state database

(defun code-stored-p (db code)
  (let ((key (octets->integer (clempo.keccak:keccak256 code))) (found nil))
    (store:map-codes (lambda (k v) (declare (ignore v)) (when (= k key) (setf found t))) db)
    found))

(deftest eth-call-code-never-reaches-the-database ()
  (let* ((db (clempo.executor:genesis-db :chain :mainnet))
         (b (make-test-backend :state db))
         (runtime (hex->octets "0xc0dec0dec0de"))
         (override (hex->octets "0x600160005260206000f3"))
         (codes (store::db-code-bytes db)))
    (flet ((call (params) (handle b (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_call\",\"params\":~A}" params))))
      ;; a creation returning RUNTIME as its code
      (is= "0xc0dec0dec0de"
           (json:jget (call "[{\"input\":\"0x65c0dec0dec0de6000526006601af3\",\"gas\":\"0x100000\"},\"latest\"]") "result"))
      ;; code installed by a state override
      (is= "0x0000000000000000000000000000000000000000000000000000000000000001"
           (json:jget (call "[{\"to\":\"0x00000000000000000000000000000000c0dec0de\",\"gas\":\"0x100000\"},\"latest\",{\"0x00000000000000000000000000000000c0dec0de\":{\"code\":\"0x600160005260206000f3\"}}]")
                      "result"))
      (is (not (code-stored-p db runtime)))
      (is (not (code-stored-p db override)))
      (is= codes (store::db-code-bytes db)))))
