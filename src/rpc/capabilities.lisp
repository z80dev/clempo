(defpackage #:clempo.rpc.capabilities
  (:use #:cl #:clempo.base #:clempo.rpc #:clempo.rpc.render)
  (:local-nicknames (#:json #:clempo.json) (#:chain #:clempo.rpc.chain) (#:state #:clempo.rpc.state)
                    (#:tries #:clempo.rpc.tries) (#:history #:clempo.history) (#:store #:clempo.store))
  (:documentation "eth_capabilities (reth EthApiSpec::capabilities, execution-apis PR 755): the
head and, per resource, the oldest block this node serves, read from what the backend really
keeps rather than from prune checkpoints:

  blocks, tx      the block archive and its hash index: everything from genesis when block 0
                  is archived, else nothing
  receipts, logs  the blocks, with receipts from the local store or, when the backend has an
                  upstream, fetched and checked against each header on first use; without an
                  upstream the store is sparse, so the resource is disabled
  state           the run of contiguous states that ends at the head: clempo.history ranges,
                  the blocks rebuilt from genesis, and the head's own state database. A head
                  state with no history behind it is a window of 0 blocks; no state at the
                  head disables the resource
  stateproofs     reth's window of eth_proof_window blocks below the head (never below the
                  oldest state), disabled when the head's state has no tries to prove from")
  (:export #:capabilities))

(in-package #:clempo.rpc.capabilities)

(defun resource (&key oldest window)
  "reth EthCapabilitiesResource: disabled when OLDEST is NIL, a sliding window when WINDOW."
  (cond ((null oldest) (json:jobject "disabled" :true))
        (window (json:jobject "disabled" :false "oldestBlock" (q oldest)
                              "deleteStrategy" (json:jobject "type" "window" "retentionBlocks" (q window))))
        (t (json:jobject "disabled" :false "oldestBlock" (q oldest)))))

(defun state-runs (head-state-p)
  "The block ranges ((from . to) ...) whose states the backend serves, merged, with HEAD-STATE-P
adding the head's."
  (let* ((h (chain:backend-history chain:*backend*))
         (replay (state:replay-coverage))
         (ranges (sort (append (when h (copy-list (history:history-ranges h)))
                               (when replay (list (cons 0 replay)))
                               (when head-state-p (list (cons chain:*head* chain:*head*))))
                       #'< :key #'car))
         (runs '()))
    (dolist (r ranges (nreverse runs))
      (let ((run (first runs)))
        (if (and run (<= (car r) (1+ (cdr run))))
            (setf (cdr run) (max (cdr run) (cdr r)))
            (push (cons (car r) (cdr r)) runs))))))

(defun state-oldest ()
  "(values oldest window-p proofs-p): the first block of the state run ending at the head (NIL
when the head's state is not served), whether that run is the head's state alone, and whether
the head's state can be proven."
  (let* ((head chain:*head*)
         (head-state (chain:backend-state chain:*backend* head))
         (without-head (find-if (lambda (r) (<= (car r) head (cdr r))) (state-runs nil)))
         (run (find-if (lambda (r) (<= (car r) head (cdr r))) (state-runs head-state))))
    (values (car run)
            (and run (not without-head) (= (car run) head))
            (and run (or (typep head-state 'store:statedb)
                         (and (chain:backend-history chain:*backend*)
                              (tries:trie-bases chain:*backend*)))
                 t))))

(defun capabilities ()
  "The eth_capabilities object for the current backend and head."
  (let* ((head chain:*head*)
         (blocks (and (chain:backend-block-raw chain:*backend* 0) 0))
         (receipts (and blocks
                        (typep chain:*backend* 'chain:archive-backend)
                        (chain:archive-backend-upstream chain:*backend*)
                        blocks))
         (window state:*eth-proof-window*))
    (multiple-value-bind (state-oldest state-window-p proofs-p) (state-oldest)
      (json:jobject
       "head" (json:jobject "number" (q head) "hash" (hex32 (cblock-hash (chain-block head))))
       "state" (resource :oldest state-oldest :window (and state-window-p 0))
       "tx" (resource :oldest blocks)
       "logs" (resource :oldest receipts)
       "receipts" (resource :oldest receipts)
       "blocks" (resource :oldest blocks)
       "stateproofs" (resource :oldest (and proofs-p (max (- head (min window head)) state-oldest))
                               :window window)))))

(defrpc "eth_capabilities" () "the head and the oldest block each resource is served from"
  (capabilities))
