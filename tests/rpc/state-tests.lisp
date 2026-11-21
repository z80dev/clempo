(defpackage #:clempo.rpc.state.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.rpc.state.tests)

;;; Parameter errors of the state and call methods, as the reference node answered them
;;; (cases from ~/cache/clempo/rpc/corpus-state.jsonl and corpus-calls.jsonl).

(defun answer (method params-json)
  (nth-value 1 (clempo.rpc:handle-body
                (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"~A\",\"params\":~A}" method params-json))))

(defun error-data (method params-json)
  (clempo.json:jget* (clempo.json:parse-json (answer method params-json)) "error" "data"))

(deftest storage-key-errors ()
  (is= "invalid length 66, expected a hex string of at most 64 digits at line 1 column 70"
       (error-data "eth_getStorageAt"
                   "[\"0x20c0000000000000000000000000000000000000\",\"0x000000000000000000000000000000000000000000000000000000000000000000\",\"0x1\"]"))
  (is= "invalid type: string \"0x0\", expected a sequence at line 1 column 5"
       (error-data "eth_getProof" "[\"0x20c0000000000000000000000000000000000000\",\"0x0\",\"latest\"]")))

(deftest multiproof-target-errors ()
  (is= "invalid type: map, expected a tuple of size 2 at line 1 column 1"
       (error-data "eth_getMultiProof"
                   "[[{\"address\":\"0x20c0000000000000000000000000000000000000\",\"storageKeys\":[]}],\"latest\"]")))

(deftest flattened-request-errors-after-object ()
  ;; alloy's TransactionRequest is flattened into Tempo's request: its field errors are
  ;; reported after the closing brace.
  (is= "invalid string length at line 1 column 13"
       (error-data "eth_call" "[{\"to\":\"0x12\"},\"0x1312d4d\"]"))
  (is= "missing field `accounts` at line 1 column 2"
       (error-data "debug_stateRootWithUpdates" "[{},null]")))

(deftest ruint-strings ()
  (is= 255 (clempo.rpc.calls::parse-ruint "0xff" 64))
  (is= 255 (clempo.rpc.calls::parse-ruint "255" 64))
  (is= 5 (clempo.rpc.calls::parse-ruint "0b1_01" 8))
  (is= 0 (clempo.rpc.calls::parse-ruint "0x" 64))
  (is= nil (clempo.rpc.calls::parse-ruint "0x100" 8))
  (is= nil (clempo.rpc.calls::parse-ruint "12a" 64))
  (is= "invalid string length" (clempo.rpc.calls::strip-position "invalid string length at line 1 column 13")))
