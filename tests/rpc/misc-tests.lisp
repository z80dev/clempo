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
