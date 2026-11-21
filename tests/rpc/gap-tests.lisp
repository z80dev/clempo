(defpackage #:clempo.rpc.gap-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:archive #:clempo.archive)))
(in-package #:clempo.rpc.gap-tests)

;;; eth_callMany's halt names, eth_capabilities and eth_fillTransaction's simulated
;;; signatures. Expected answers were recorded from the reference node (corpus-gaps.jsonl).

(defun archive-backend (head)
  "The mainnet archive headed at HEAD, or a skip when it is absent."
  (unless (archive:chunk-present-p 0 (archive:archive-dir))
    (skip-test "no mainnet archive under ~/cache/clempo/mainnet"))
  (chain:make-archive-backend :head head))

(defun answer (backend method params)
  (json:parse-json
   (chain:with-backend-read (backend)
     (nth-value 1 (clempo.rpc:handle-body
                   (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":~S,\"params\":~A}" method params))))))

(defparameter *switch-code* "0x600054600c576001600055005bfe"
  "Stores 1 in slot 0 when it is 0, else halts on 0xFE: the second call halts only after the
first one's write.")

(deftest call-many-names-every-invalid-opcode ()
  ;; Recorded from the reference at block 0x10, with and without a transactionIndex.
  (let ((backend (archive-backend 20))
        (want (json:parse-json "[[{\"value\":\"0x\"},{\"error\":\"EVM error: InvalidFEOpcode\"},{\"error\":\"EVM error: OpcodeNotFound\"}],[{\"error\":\"EVM error: InvalidFEOpcode\"}]]")))
    (dolist (context '("{\"blockNumber\":\"0x10\"}" "{\"blockNumber\":\"0x10\",\"transactionIndex\":0}"))
      (let ((a "\"0x00000000000000000000000000000000c0dec0de\"") (g "\"0x100000\""))
        (is (json:json-equal
             want
             (json:jget (answer backend "eth_callMany"
                                (format nil "[[{\"transactions\":[{\"to\":~A,\"gas\":~A},{\"to\":~A,\"gas\":~A},{\"input\":\"0x0c\",\"gas\":~A}]},{\"transactions\":[{\"to\":~A,\"gas\":~A}]}],~A,{~A:{\"code\":~S}}]"
                                        a g a g g a g context a *switch-code*))
                        "result"))
            "context ~A" context)))))
