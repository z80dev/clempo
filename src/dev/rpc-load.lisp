(defpackage #:clempo.rpc-load
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:io #:clempo.io) (#:json #:clempo.json) (#:jsonrpc #:clempo.jsonrpc))
  (:documentation "JSON-RPC load and answer diffing between two servers at one height (the
`rpc-load` command, docs/BENCH.md). A corpus of requests is drawn, from a seeded
*ENTROPY*, out of the blocks FROM..TO: balances and TIP-20 balanceOf calls of the
transactions' senders at TO, full blocks, 10-block log ranges of the tokens they touched,
call traces of their transactions and account proofs. Every request is first sent to
both servers and the answers compared; then each method runs at each concurrency on each
server in turn, one keep-alive connection per client thread, measuring requests per
second and latency percentiles.")
  (:export #:build-corpus #:diff-corpus #:load-run))

(in-package #:clempo.rpc-load)

(defparameter *methods*
  '("eth_getBalance" "eth_call" "eth_getBlockByNumber" "eth_getLogs"
    "debug_traceTransaction" "eth_getProof"))

(defparameter +pathusd+ "0x20c0000000000000000000000000000000000000")

(defun hexn (n) (format nil "0x~(~X~)" n))

(defun pick (seq) (elt seq (io:random-below (length seq))))

;;; Corpus

(defun build-corpus (url from to &key (n 200) (seed 1) (methods *methods*))
  "Requests per method, a hash table method -> list of params, drawn from blocks FROM..TO
served at URL."
  (let* ((blocks (loop for start from from to to by 50
                       append (jsonrpc:rpc-batch
                               (loop for b from start to (min to (+ start 49))
                                     collect (list "eth_getBlockByNumber" (vector (hexn b) :true)))
                               :url url)))
         (txs (loop for b in blocks append (coerce (json:jget b "transactions") 'list)))
         (senders (sort (remove-duplicates (mapcar (lambda (tx) (json:jget tx "from")) txs)
                                           :test #'string-equal)
                        #'string<))
         (tokens (or (sort (remove-duplicates
                            (loop for tx in txs
                                  for to-address = (json:jget tx "to")
                                  when (and (stringp to-address)
                                            (string-equal "0x20c0" to-address :end2 6))
                                    collect (string-downcase to-address))
                            :test #'string=)
                           #'string<)
                     (list +pathusd+)))
         (head (hexn to))
         (io:*entropy* (io:make-seeded-entropy seed))
         (corpus (make-hash-table :test 'equal)))
    (unless (and txs senders) (error "no transactions in blocks ~D..~D" from to))
    (dolist (m methods)
      (setf (gethash m corpus)
            (loop repeat n
                  collect
                  (let ((who (pick senders)))
                    (cond
                      ((string= m "eth_getBalance") (vector who head))
                      ((string= m "eth_call")
                       (vector (json:jobject "to" (pick tokens)
                                             "data" (format nil "0x70a08231~24,'0D~A" 0 (subseq who 2)))
                               head))
                      ((string= m "eth_getBlockByNumber")
                       (vector (hexn (+ from (io:random-below (1+ (- to from))))) :true))
                      ((string= m "eth_getLogs")
                       (let ((lo (+ from (io:random-below (max 1 (- to from 8))))))
                         (vector (json:jobject "fromBlock" (hexn lo) "toBlock" (hexn (min to (+ lo 9)))
                                               "address" (pick tokens)))))
                      ((string= m "debug_traceTransaction")
                       (vector (json:jget (pick txs) "hash") (json:jobject "tracer" "callTracer")))
                      ((string= m "eth_getProof")
                       (if (zerop (io:random-below 2))
                           (vector who (vector) head)
                           (vector (pick tokens)
                                   (vector (format nil "0x~64,'0X" (io:random-below 16)))
                                   head)))
                      (t (error "rpc-load has no request shape for ~A" m)))))))
    (values corpus (length blocks) (length txs) (length senders) (length tokens))))

;;; A minimal keep-alive client: one socket per thread, request bytes built up front.

(defstruct (client (:constructor %make-client)) host port socket stream)

(defun make-client (url)
  (multiple-value-bind (scheme host port) (clempo.http:parse-url url)
    (declare (ignore scheme))
    (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (setf (sb-bsd-sockets:sockopt-tcp-nodelay s) t)
      (sb-bsd-sockets:socket-connect
       s (if (string= host "localhost") #(127 0 0 1)
             (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
       port)
      (%make-client :host host :port port :socket s
                    :stream (sb-bsd-sockets:socket-make-stream
                             s :input t :output t :element-type '(unsigned-byte 8)
                               :buffering :full)))))

(defun close-client (c) (ignore-errors (sb-bsd-sockets:socket-close (client-socket c))))

(defun request-octets (c method params &optional (id 1))
  (let ((body (json:to-json-octets (json:jobject "jsonrpc" "2.0" "id" id "method" method
                                                 "params" params))))
    (octets-concat
     (string->octets (format nil "POST / HTTP/1.1~C~CHost: ~A:~D~C~CContent-Type: application/json~C~CContent-Length: ~D~C~C~C~C"
                             #\Return #\Newline (client-host c) (client-port c) #\Return #\Newline
                             #\Return #\Newline (length body) #\Return #\Newline #\Return #\Newline))
     body)))

(defun read-line-crlf (stream)
  (let ((out (make-string-output-stream)))
    (loop for b = (read-byte stream nil nil)
          do (cond ((null b) (error "connection closed"))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   (t (write-char (code-char b) out))))))

(defun read-exact (stream n)
  (let ((o (make-octets n)))
    (unless (= n (read-sequence o stream)) (error "short body"))
    o))

(defun exchange (c octets)
  "Send one prepared request; returns (values status body-octets)."
  (let ((s (client-stream c)))
    (write-sequence octets s)
    (force-output s)
    (let ((status (parse-integer (read-line-crlf s) :start 9 :end 12)) (length nil) (chunked nil))
      (loop for line = (read-line-crlf s)
            until (zerop (length line))
            do (let ((colon (position #\: line)))
                 (when colon
                   (let ((name (string-downcase (subseq line 0 colon)))
                         (value (string-trim " " (subseq line (1+ colon)))))
                     (cond ((string= name "content-length") (setf length (parse-integer value)))
                           ((string= name "transfer-encoding")
                            (setf chunked (search "chunked" (string-downcase value)))))))))
      (values status
              (cond (length (read-exact s length))
                    (chunked
                     (let ((parts '()))
                       (loop for size = (parse-integer (read-line-crlf s) :radix 16 :junk-allowed t)
                             until (zerop size)
                             do (push (read-exact s size) parts) (read-line-crlf s))
                       (read-line-crlf s)
                       (apply #'octets-concat (nreverse parts))))
                    (t (error "response without a length")))))))

(defun error-answer-p (status body)
  (or (/= status 200)
      (search (string->octets "\"error\"") body :end2 (min (length body) 200))))

;;; Diffing

(defun answer (c method params)
  "(:result value) or (:error code message) or (:fail text)."
  (handler-case
      (multiple-value-bind (status body) (exchange c (request-octets c method params))
        (let ((v (json:parse-json body)))
          (cond ((json:jget v "error")
                 (list :error (json:jget* v "error" "code") (json:jget* v "error" "message")))
                ((/= status 200) (list :fail (format nil "HTTP ~D" status)))
                (t (list :result (json:jget v "result"))))))
    (error (e) (list :fail (princ-to-string e)))))

(defun same-answer-p (a b)
  (and (eq (first a) (first b))
       (case (first a)
         (:result (json:json-equal (second a) (second b)))
         (:error (eql (second a) (second b)))
         (t nil))))

(defun diff-corpus (corpus url-a url-b methods &key (log *error-output*))
  "Ask both servers every request. Returns a list of (method same differ examples)."
  (let ((a (make-client url-a)) (b (make-client url-b)))
    (unwind-protect
         (loop for m in methods
               collect (let ((same 0) (differ 0) (examples '()))
                         (dolist (p (gethash m corpus))
                           (let ((x (answer a m p)) (y (answer b m p)))
                             (if (same-answer-p x y)
                                 (incf same)
                                 (progn (incf differ)
                                        (when (< (length examples) 3)
                                          (push (list p x y) examples))))))
                         (format log "~&diff ~A: ~D same, ~D differ~%" m same differ)
                         (list m same differ (nreverse examples))))
      (close-client a) (close-client b))))

;;; Load

(defun percentile (sorted p)
  (aref sorted (min (1- (length sorted)) (round (* p (1- (length sorted))) 100))))

(defun load-run (url method params-list concurrency total)
  "TOTAL requests of METHOD from CONCURRENCY threads, each on its own connection after
one warm-up request, all released together. Returns a plist."
  (let* ((per (max 1 (ceiling total concurrency)))
         (ready (sb-thread:make-semaphore))
         (go (sb-thread:make-semaphore))
         (n (length params-list))
         (params (coerce params-list 'simple-vector))
         (threads
           (loop for w from 0 below concurrency
                 collect
                 (let ((w w))
                   (sb-thread:make-thread
                    (lambda ()
                      (let* ((c (make-client url))
                             (requests (loop for i from 0 below per
                                             collect (request-octets c method (svref params (mod (+ (* w per) i) n)))))
                             (latencies (make-array per :element-type 'double-float))
                             (errors 0))
                        (unwind-protect
                             (progn
                               (exchange c (first requests))
                               (sb-thread:signal-semaphore ready)
                               (sb-thread:wait-on-semaphore go)
                               (let ((start (io:monotonic-ticks)))
                                 (loop for r in requests
                                       for i from 0
                                       do (let ((t0 (io:monotonic-ticks)))
                                            (multiple-value-bind (status body) (exchange c r)
                                              (when (error-answer-p status body) (incf errors)))
                                            (setf (aref latencies i)
                                                  (/ (float (- (io:monotonic-ticks) t0) 1d0)
                                                     (/ internal-time-units-per-second 1000)))))
                                 (list start (io:monotonic-ticks) latencies errors)))
                          (close-client c))))
                    :name "rpc-load client")))))
    (dotimes (i concurrency) (sb-thread:wait-on-semaphore ready))
    (sb-thread:signal-semaphore go concurrency)
    (let* ((results (mapcar #'sb-thread:join-thread threads))
           (lat (sort (apply #'concatenate '(vector double-float) (mapcar #'third results)) #'<))
           (span (/ (- (reduce #'max (mapcar #'second results)) (reduce #'min (mapcar #'first results)))
                    (float internal-time-units-per-second 1d0))))
      (list :method method :c concurrency :n (length lat)
            :errors (reduce #'+ (mapcar #'fourth results))
            :rps (/ (length lat) (max span 1d-9))
            :p50 (percentile lat 50) :p90 (percentile lat 90) :p99 (percentile lat 99)
            :max (aref lat (1- (length lat)))))))

;;; The command

(in-package #:clempo.dev)

(defun split-commas (s)
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, s :start start)
        collect (subseq s start comma)
        while comma))

(defcommand "rpc-load" (args)
  "RPC load and answer diff of two servers at one height: rpc-load -a URL -b URL -from N -to M [-n 200] [-total 400] [-concurrency 1,16,64] [-methods m,...] [-rounds 3] [-seed S] [-result FILE] [-no-diff] [-no-load]"
  (let* ((url-a (or (arg args :a) (return-from command (progn (log-line "rpc-load: -a URL required") 2))))
         (url-b (or (arg args :b) (return-from command (progn (log-line "rpc-load: -b URL required") 2))))
         (from (arg-int args :from)) (to (arg-int args :to))
         (methods (if (arg args :methods) (split-commas (arg args :methods)) clempo.rpc-load::*methods*))
         (concurrency (mapcar #'parse-integer (split-commas (arg args :concurrency "1,16,64"))))
         (total (arg-int args :total 400))
         (results '()) (diffs '()))
    (unless (and from to) (log-line "rpc-load: -from and -to required") (return-from command 2))
    (multiple-value-bind (corpus blocks txs senders tokens)
        (clempo.rpc-load:build-corpus url-b from to :n (arg-int args :n 200)
                                                    :seed (arg-int args :seed 1) :methods methods)
      (log-line "corpus: blocks ~D..~D: ~D blocks, ~D txs, ~D senders, ~D tokens; ~D requests per method"
                from to blocks txs senders tokens (arg-int args :n 200))
      (unless (arg-flag args :no-diff)
        (setf diffs (clempo.rpc-load:diff-corpus corpus url-a url-b methods)))
      (unless (arg-flag args :no-load)
        (dotimes (round (arg-int args :rounds 3))
          (dolist (m methods)
            (dolist (c concurrency)
              ;; alternate which server goes first at each step
              (dolist (side (if (evenp (length results)) '(:a :b) '(:b :a)))
                (let ((r (clempo.rpc-load:load-run (if (eq side :a) url-a url-b) m (gethash m corpus) c total)))
                  (log-line "round ~D ~A ~A c=~D: ~,1F req/s p50 ~,3F ms p99 ~,3F ms errors ~D"
                            round side m c (getf r :rps) (getf r :p50) (getf r :p99) (getf r :errors))
                  (push (list* :side side :round round r) results)))))))
      (setf results (nreverse results))
      (format t "~&Medians over rounds; latency in ms.~%~%| Method | Clients | A req/s | B req/s | A/B | A p50/p99 | B p50/p99 | errors A/B |~%|---|---:|---:|---:|---:|---:|---:|---|~%")
      (flet ((med (side m c key)
               (let ((xs (sort (loop for r in results
                                     when (and (eq (getf r :side) side) (string= (getf r :method) m)
                                               (= (getf r :c) c))
                                       collect (getf r key))
                               #'<)))
                 (and xs (if (eq key :errors) (reduce #'+ xs) (nth (floor (length xs) 2) xs))))))
        (dolist (m methods)
          (dolist (c concurrency)
            (when (and (med :a m c :rps) (med :b m c :rps))
              (format t "| `~A` | ~D | ~:D | ~:D | ~,2F | ~,3F/~,3F | ~,3F/~,3F | ~D/~D |~%"
                      m c (round (med :a m c :rps)) (round (med :b m c :rps))
                      (/ (med :a m c :rps) (med :b m c :rps))
                      (med :a m c :p50) (med :a m c :p99) (med :b m c :p50) (med :b m c :p99)
                      (med :a m c :errors) (med :b m c :errors))))))
      (when diffs
        (format t "~&~%| Method | same | differ |~%|---|---:|---:|~%")
        (loop for (m same differ) in diffs do (format t "| `~A` | ~D | ~D |~%" m same differ)))
      (when (arg args :result)
        (clempo.io:write-file-atomically
         (arg args :result)
         (clempo.json:to-json-octets
          (clempo.json:jobject
           "a" url-a "b" url-b "from" from "to" to "seed" (arg-int args :seed 1)
           "corpus" (clempo.json:jobject "blocks" blocks "txs" txs "senders" senders "tokens" tokens)
           "load" (coerce (loop for r in results
                                collect (clempo.json:jobject
                                         "side" (string-downcase (symbol-name (getf r :side)))
                                         "round" (getf r :round)
                                         "method" (getf r :method) "c" (getf r :c) "n" (getf r :n)
                                         "errors" (getf r :errors) "rps" (getf r :rps)
                                         "p50_ms" (getf r :p50) "p90_ms" (getf r :p90)
                                         "p99_ms" (getf r :p99) "max_ms" (getf r :max)))
                          'simple-vector)
           "diff" (coerce (loop for (m same differ examples) in diffs
                                collect (clempo.json:jobject
                                         "method" m "same" same "differ" differ
                                         "examples" (coerce (loop for (p x y) in examples
                                                                  collect (clempo.json:jobject
                                                                           "params" p
                                                                           "a" (prin1-to-string x)
                                                                           "b" (prin1-to-string y)))
                                                            'simple-vector)))
                          'simple-vector)))))
      (if (some (lambda (d) (plusp (third d))) diffs) 1 0))))
