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

;;; eth_capabilities over backends that keep different things

(defclass head-state-backend (chain:archive-backend) ((db :initarg :db))
  (:documentation "A node's shape: the head's state database and no history."))

(defmethod chain:backend-state ((b head-state-backend) number)
  (when (= number (chain:backend-head b)) (slot-value b 'db)))

(defun capabilities (backend)
  (json:jget (answer backend "eth_capabilities" "[]") "result"))

(defun head-json (backend n)
  (json:jobject "number" (quantity->hex n)
                "hash" (octets->hex (chain:block-hash-of-raw (chain:backend-block-raw backend n)))))

(defun from (n) (json:jobject "disabled" :false "oldestBlock" (quantity->hex n)))
(defun window (n w)
  (json:jobject "disabled" :false "oldestBlock" (quantity->hex n)
                "deleteStrategy" (json:jobject "type" "window" "retentionBlocks" (quantity->hex w))))
(defun off () (json:jobject "disabled" :true))

(defun capabilities-json (head state receipts blocks proofs)
  (json:jobject "head" head "state" state "tx" blocks "logs" receipts "receipts" receipts
                "blocks" blocks "stateproofs" proofs))

(deftest capabilities-follow-retention ()
  (archive-backend 0)
  ;; blocks from genesis; states rebuilt from genesis near it; no tries, no upstream receipts
  (let ((b (chain:make-archive-backend :head 20 :history nil)))
    (is (json:json-equal (capabilities-json (head-json b 20) (from 0) (off) (from 0) (off))
                         (capabilities b))))
  ;; an upstream fills receipts for every block
  (let ((b (chain:make-archive-backend :head 20 :history nil :upstream "http://127.0.0.1:1/")))
    (is (json:json-equal (capabilities-json (head-json b 20) (from 0) (from 0) (from 0) (off))
                         (capabilities b))))
  ;; beyond the rebuildable blocks, no history: no state at all
  (let ((b (chain:make-archive-backend :head 2000 :history nil)))
    (is (json:json-equal (capabilities-json (head-json b 2000) (off) (off) (from 0) (off))
                         (capabilities b))))
  ;; the head's state database only: a window of 0 blocks, provable at the head
  (let ((b (chain:make-archive-backend :class 'head-state-backend :head 2000 :history nil
                                       :db (clempo.executor:genesis-db))))
    (is (json:json-equal (capabilities-json (head-json b 2000) (window 2000 0) (off) (from 0) (window 2000 0))
                         (capabilities b)))))
