(in-package #:clempo.dev)

;;; R5: replay the recorded corpus (rpc-corpus.lisp) against a clempo JSON-RPC server and
;;; compare every answer with the reference's: HTTP status and JSON (json-equal: object key
;;; order ignored). "live" and "node" cases are compared by shape (same keys and types,
;;; same error code), since their values depend on the moment or on the reference node.
;;;
;;;   clempo-dev rpc-index                 build the block and transaction hash index
;;;   clempo-dev rpcdiff [-v] [-method M] [-suite S] [-url U]
;;;   clempo-dev score -rung R5

(defparameter *head-dependent-methods*
  '("eth_blockNumber" "eth_gasPrice" "eth_maxPriorityFeePerGas" "eth_blobBaseFee" "eth_syncing"
    "net_peerCount")
  "Methods whose answer follows the head; their cases are compared by shape.")

(defun corpus-cases (&key suite method)
  (loop for path in (sort (directory (corpus-path "corpus-*.jsonl")) #'string< :key #'namestring)
        append (loop for c in (read-corpus-file path)
                     when (and (or (null suite) (string= suite (clempo.json:jget c "suite")))
                               (or (null method) (string= method (clempo.json:jget c "method"))))
                       collect c)))

(defun mentions-head-p (response)
  "An error message that quotes the reference's head block number (\"..., head 50058714\")."
  (let ((message (clempo.json:jget* response "error" "message")))
    (and (stringp message)
         (let ((at (search "head " message :from-end t)))
           (and at (< (+ at 5) (length message)) (digit-char-p (char message (+ at 5))))))))

(defun case-compare-class (c)
  (let ((class (clempo.json:jget c "class")))
    (if (and (string= class "stable")
             (or (member (clempo.json:jget c "method") *head-dependent-methods* :test #'string=)
                 (mentions-head-p (clempo.json:jget c "response"))))
        "live"
        class)))

(defun json-shape-equal (a b)
  "Same JSON types and object keys; error objects must share their code."
  (cond ((clempo.json:jobject-p a)
         (and (clempo.json:jobject-p b)
              (= (length (cdr a)) (length (cdr b)))
              (loop for (k . v) in (cdr a)
                    always (let ((cell (assoc k (cdr b) :test #'string=)))
                             (and cell
                                  (if (string= k "code") (equal v (cdr cell)) (json-shape-equal v (cdr cell))))))))
        ((and (vectorp a) (not (stringp a)))
         (and (vectorp b) (not (stringp b))
              (or (zerop (length a)) (zerop (length b)) (json-shape-equal (aref a 0) (aref b 0)))))
        ((stringp a) (stringp b))
        ((numberp a) (numberp b))
        (t (eq a b))))

(defun case-matches (c status response)
  (and (eql status (clempo.json:jget c "status"))
       (if (string= (case-compare-class c) "stable")
           (clempo.json:json-equal (clempo.json:jget c "response") response)
           (json-shape-equal (clempo.json:jget c "response") response))))

(defmacro with-clempo-server ((url-var &key url upstream) &body body)
  "Bind URL-VAR to URL, or start an in-process server over the archive for BODY."
  (let ((server (gensym "SERVER")) (backend (gensym "BACKEND")))
    `(if ,url
         (let ((,url-var ,url)) ,@body)
         (let* ((,backend (clempo.rpc.chain:make-archive-backend :upstream ,upstream))
                (,server (clempo.rpcserver:start-server :port 0 :backend ,backend)))
           (unwind-protect
                (let ((,url-var (format nil "http://127.0.0.1:~D/" (clempo.rpcserver:server-port ,server))))
                  ,@body)
             (clempo.rpcserver:stop-server ,server))))))

(defun run-rpcdiff (url cases &key (threads 16))
  "Replay CASES against URL. Returns a list of (case status response matched) in order."
  (parallel-map (lambda (c)
                  (multiple-value-bind (status response)
                      (post-raw url (clempo.json:jget c "request") :retries 0)
                    (list c status response (case-matches c status response))))
                cases threads))

(defun served-reference-methods (url)
  "(values served missing) among the reference's method names."
  (let ((served '()) (missing '()))
    (dolist (m (reference-methods))
      (multiple-value-bind (status response) (post-raw url (rpc-request-string m '()) :retries 0)
        (declare (ignore status))
        (if (method-not-found-p response) (push m missing) (push m served))))
    (values (nreverse served) (nreverse missing))))

(defun rpcdiff-summary (results)
  "Per method: (method cases identical). Returns (values rows methods-ok cases-ok total)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (r results)
      (let* ((m (clempo.json:jget (first r) "method"))
             (cell (or (gethash m table) (setf (gethash m table) (list 0 0)))))
        (incf (first cell))
        (when (fourth r) (incf (second cell)))))
    (let ((rows (sort (loop for m being the hash-keys of table using (hash-value v)
                            collect (list m (first v) (second v)))
                      #'string< :key #'first)))
      (values rows
              (count-if (lambda (row) (= (second row) (third row))) rows)
              (loop for r in results count (fourth r))
              (length results)))))

(defun rpcdiff-suites (results)
  "Per suite, in corpus order: a JSON object of \"suite\": \"identical/total\"."
  (let ((rows '()))
    (dolist (r results)
      (let* ((s (clempo.json:jget (first r) "suite"))
             (cell (or (assoc s rows :test #'string=) (car (push (list s 0 0) rows)))))
        (incf (second cell))
        (when (fourth r) (incf (third cell)))))
    (cons :object (loop for (s n ok) in (reverse rows) collect (cons s (format nil "~D/~D" ok n))))))

(defun json-difference (want got &optional (path "$"))
  "The first place WANT and GOT differ, as \"path: want X got Y\", or NIL."
  (cond ((and (clempo.json:jobject-p want) (clempo.json:jobject-p got))
         (or (loop for (k . v) in (cdr want)
                   for cell = (assoc k (cdr got) :test #'string=)
                   thereis (if cell
                               (json-difference v (cdr cell) (format nil "~A.~A" path k))
                               (format nil "~A.~A: missing" path k)))
             (loop for (k) in (cdr got)
                   unless (assoc k (cdr want) :test #'string=)
                     return (format nil "~A.~A: unexpected" path k))))
        ((and (vectorp want) (not (stringp want)) (vectorp got) (not (stringp got)))
         (or (loop for i from 0 below (min (length want) (length got))
                   thereis (json-difference (aref want i) (aref got i) (format nil "~A[~D]" path i)))
             (unless (= (length want) (length got))
               (format nil "~A: length ~D vs ~D" path (length want) (length got)))))
        ((clempo.json:json-equal want got) nil)
        (t (format nil "~A: want ~A got ~A" path (short-json want 120) (short-json got 120)))))

(defun short-json (v &optional (n 400))
  (let ((s (if (stringp v) v (clempo.json:to-json-string v))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 n) "...") s)))

(defun rpcdiff (&key url suite method verbose (upstream (clempo.jsonrpc:local-rpc-url)) (limit 3))
  "Run the R5 comparison; returns a clempo.json object with the score."
  (let ((cases (corpus-cases :suite suite :method method)))
    (with-clempo-server (server-url :url url :upstream upstream)
      (let ((results (run-rpcdiff server-url cases)))
        (multiple-value-bind (rows methods-ok cases-ok total) (rpcdiff-summary results)
          (multiple-value-bind (served missing) (served-reference-methods server-url)
            (when verbose
              (dolist (row rows)
                (format t "~:[  ~;ok~] ~40A ~D/~D~%" (= (second row) (third row)) (first row) (third row) (second row)))
              (let ((shown (make-hash-table :test 'equal)))
                (dolist (r results)
                  (destructuring-bind (c status response ok) r
                    (let ((m (clempo.json:jget c "method")))
                      (when (and (not ok) (< (gethash m shown 0) limit))
                        (incf (gethash m shown 0))
                        (format t "~&--- ~A [~A] ~A~%  want ~A ~A~%  got  ~A ~A~%  diff ~A~%"
                                m (case-compare-class c) (short-json (clempo.json:jget c "request") 300)
                                (clempo.json:jget c "status") (short-json (clempo.json:jget c "response"))
                                status (short-json response)
                                (json-difference (clempo.json:jget c "response") response)))))))
              (format t "missing reference methods: ~{~A~^ ~}~%" missing))
            (clempo.json:jobject
             "score" (format nil "~D/~D methods, ~D/~D cases" methods-ok (length rows) cases-ok total)
             "methods" methods-ok "methodsTotal" (length rows)
             "cases" cases-ok "casesTotal" total
             "referenceMethodsServed" (length served)
             "referenceMethods" (+ (length served) (length missing))
             "suites" (rpcdiff-suites results))))))))

(defcommand "rpcdiff" (args)
  "R5: replay the RPC corpus against clempo: rpcdiff [-v] [-suite S] [-method M] [-url U] [-limit N]"
  (let ((result (rpcdiff :url (arg args :url) :suite (arg args :suite) :method (arg args :method)
                         :verbose (arg-flag args :v) :limit (arg-int args :limit 3))))
    (write-line (clempo.json:to-json-string result))
    0))

(defrung "R5" (args)
  "share of the reference's RPC methods answering identically on the recorded corpus"
  (rpcdiff :url (arg args :url)))

(defcommand "rpc-index" (args)
  "build the block and transaction hash index over the archive: rpc-index [-threads N]"
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (blocks txs) (clempo.rpc.chain:build-index :threads (arg-int args :threads 16))
      (format t "index: ~D blocks, ~D transactions in ~,1F s -> ~A~%" blocks txs
              (/ (- (get-internal-real-time) start) internal-time-units-per-second)
              (clempo.rpc.chain:index-dir))
      0)))

(defcommand "rpc-serve" (args)
  "serve JSON-RPC over the archive: rpc-serve [-port 8546] [-host H] [-upstream URL]"
  (let* ((backend (clempo.rpc.chain:make-archive-backend :upstream (arg args :upstream)))
         (server (clempo.rpcserver:start-server :host (arg args :host "127.0.0.1")
                                                :port (arg-int args :port 8546) :backend backend)))
    (log-line "rpc: serving blocks 0..~D on port ~D" (clempo.rpc.chain:backend-head backend)
              (clempo.rpcserver:server-port server))
    (loop (sleep 3600))))
