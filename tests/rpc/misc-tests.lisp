(defpackage #:clempo.rpc.misc-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:filter #:clempo.rpc.filter)))
(in-package #:clempo.rpc.misc-tests)

;;; consensus_getFinalization's Query and trace_filter's matcher and paging. The expected
;;; errors were recorded from rpc.tempo.xyz and the reference node (corpus-consensus.jsonl,
;;; corpus-traces.jsonl and probes of the same methods).

(defun error-data (method params)
  (let ((response (json:parse-json
                   (nth-value 1 (clempo.rpc:handle-body
                                 (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":~A}"
                                         method params))))))
    (json:jget* response "error" "data")))

(deftest consensus-query-errors ()
  (loop for (params data) in
        '(("[\"nope\"]" "unknown variant `nope`, expected `latest` or `height` at line 1 column 6")
          ("[{\"foo\":1}]" "unknown variant `foo`, expected `latest` or `height` at line 1 column 6")
          ("[{\"height\":\"0x3e8\"}]" "invalid type: string \"0x3e8\", expected u64 at line 1 column 17")
          ("[{\"height\":-1}]" "invalid value: integer `-1`, expected u64 at line 1 column 12")
          ("[{\"height\":1.5}]" "invalid type: floating point `1.5`, expected u64 at line 1 column 13")
          ("[{\"height\":5,\"x\":1}]" "expected value at line 1 column 11")
          ("[{\"latest\":1}]" "invalid type: integer `1`, expected unit at line 1 column 11")
          ("[\"height\"]" "invalid type: unit variant, expected newtype variant")
          ("[{}]" "expected value at line 1 column 2")
          ("[1]" "expected value at line 1 column 1")
          ("[null]" "expected value at line 1 column 1")
          ("[]" "No more params"))
        do (is= data (error-data "consensus_getFinalization" params) "~A" params)))

(deftest trace-filter-errors ()
  (is= "unknown field `foo`, expected one of `fromBlock`, `toBlock`, `fromAddress`, `toAddress`, `mode`, `after`, `count`, `blockHash` at line 1 column 6"
       (error-data "trace_filter" "[{\"foo\":1}]"))
  (is= "unknown variant `x`, expected `union` or `intersection` at line 1 column 11"
       (error-data "trace_filter" "[{\"mode\":\"x\"}]"))
  (is= "invalid type: string \"0x1\", expected u64 at line 1 column 14"
       (error-data "trace_filter" "[{\"after\":\"0x1\"}]")))

(defun call-trace (from to)
  (json:jobject "type" "call" "action" (json:jobject "from" (integer->hex from :bytes 20)
                                                     "to" (integer->hex to :bytes 20))))

(deftest trace-filter-matching ()
  (let ((tr (call-trace 1 1)))
    ;; alloy's tests: intersection by default, union when asked
    (is (filter:trace-matches-p tr '() '() :intersection))
    (is (not (filter:trace-matches-p tr '(1) '(2) :intersection)))
    (is (filter:trace-matches-p tr '(1) '(2) :union))
    (is (filter:trace-matches-p (call-trace 3 3) '(1 3) '(2 3) :intersection))
    (is (not (filter:trace-matches-p (call-trace 2 1) '(1 3) '(2 3) :intersection))))
  (let ((create (json:jobject "type" "create" "action" (json:jobject "from" (integer->hex 1 :bytes 20))
                              "result" :null)))
    (is (filter:trace-matches-p create '(1) '() :intersection))
    (is (not (filter:trace-matches-p create '(1) '(2) :intersection)))))

(deftest trace-filter-paging ()
  (is= '(2 3) (filter:paginate '(1 2 3 4) 1 2))
  (is= '(2 3 4) (filter:paginate '(1 2 3 4) 1 nil))
  (is= '() (filter:paginate '(1 2) 2 nil))
  (is= '() (filter:paginate '(1 2) 0 0))
  (is (nth-value 1 (filter:paginate '(1 2 3) 0 2)))
  (is (not (nth-value 1 (filter:paginate '(1 2) 0 2)))))

(deftest fee-history-cache-percentiles ()
  ;; reth approximate_percentile (fee history cache, resolution 4): f64::round, half away from zero
  (loop for (p want) in '((10d0 10d0) (10.1d0 10d0) (10.125d0 10.25d0) (10.375d0 10.5d0)
                          (99.9d0 100d0) (0.124d0 0d0) (2.5d0 2.5d0))
        do (is= want (clempo.rpc.logs::quarter-percentile p) "~A" p)))

;;; Admin peer parameters: the success paths reth acknowledges (never sent to the reference;
;;; its parse errors are corpus suite "admin").

(defun result-of (method params)
  (json:jget (json:parse-json
              (nth-value 1 (clempo.rpc:handle-body
                            (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":~A}" method params))))
             "result"))

(deftest admin-peer-parameters ()
  (let* ((id (make-string 128 :initial-element #\b))
         (key 12345)
         (public (clempo.secp256k1:private->public key))
         (enr (clempo.rpc.state::node-enr key public (octets-of 127 0 0 1) (integer->octets-minimal 30303)
                                          (integer->octets-minimal 30303))))
    (dolist (m '("admin_addPeer" "admin_addTrustedPeer" "admin_removePeer" "admin_removeTrustedPeer"))
      (is (eq :true (result-of m (format nil "[\"enode://~A@127.0.0.1:30303\"]" id))) m)
      (is (eq :true (result-of m (format nil "[\"enode://0x~A@[::1]:1?discport=9\"]" id))) m))
    (dolist (m '("admin_addTrustedPeer" "admin_removePeer" "admin_removeTrustedPeer"))
      (is (eq :true (result-of m (format nil "[\"enode://~A\"]" id))) m)
      (is (eq :true (result-of m (format nil "[~S]" enr))) m)
      ;; one flipped signature bit
      (let ((bad (let ((s (copy-seq enr))) (setf (char s 6) (if (char= (char s 6) #\A) #\B #\A)) s)))
        (is (search "Invalid ENR" (error-data m (format nil "[~S]" bad))) m)))
    (is (equal "Failed to parse id at line 1 column 23"
               (error-data "admin_addPeer" "[\"http://xyz@1.2.3.4:81\"]")))
    (is (equal "Failed to parse url: no port specified at line 1 column 21"
               (error-data "admin_addPeer" "[\"http://x@1.2.3.4:80\"]")))))
