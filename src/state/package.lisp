(defpackage #:clempo.state
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.keccak #:keccak256)
  (:documentation "The journaled world state the executor reads and writes.

Committed data lives in a backend reached through generic functions (BACKEND-ACCOUNT,
BACKEND-STORAGE, BACKEND-CODE, BACKEND-STORAGE-EMPTY-P); MEMORY-BACKEND is the in-memory
one. A WORLD overlays a backend with the accounts touched in the current block: present
and transaction-start (original) storage values for EIP-2200/3529, transient storage
(EIP-1153), warm address and slot sets (EIP-2929/2930), the refund counter, logs, the
created and self-destructed sets (EIP-6780) and touched accounts (EIP-161). Every change
inside a transaction goes through the journal, so CHECKPOINT / ROLLBACK undo it.
FINALIZE-TX closes a transaction; DRAIN-CHANGES hands the block's net changes to whoever
writes them down.")
  (:export
   ;; backend protocol
   #:backend-account #:backend-storage #:backend-code #:backend-storage-empty-p
   #:memory-backend #:make-memory-backend #:memory-backend-accounts
   #:memory-backend-put-account #:memory-backend-put-storage #:apply-changes
   #:mem-account #:mem-account-nonce #:mem-account-balance #:mem-account-code-hash
   #:mem-account-storage
   #:+empty-code-hash+ #:+empty-code+ #:code-hash
   ;; accounts
   #:account #:account-address #:account-nonce #:account-balance #:account-exists
   #:account-touched #:account-created #:account-destructed
   #:world #:make-world #:world-backend #:world-refund #:world-logs #:world-observer
   #:get-account #:account-code #:account-code-hash #:account-empty-p #:account-dead-p
   #:account-exists-p #:account-alive-p
   #:get-balance #:set-balance #:add-balance #:sub-balance
   #:get-nonce #:set-nonce #:increment-nonce
   #:get-code #:get-code-hash #:set-code #:code-size
   #:touch-account #:mark-created #:created-p #:mark-destructed #:destructed-p
   #:storage-nonempty-p #:wipe-storage
   ;; storage
   #:sload #:sstore #:storage-original #:account-sload #:account-sstore
   #:account-storage-original
   #:tload #:tstore #:take-transient-storage
   ;; access lists
   #:warm-address #:address-warm-p #:warm-slot #:slot-warm-p
   ;; journal, refunds, logs
   #:checkpoint #:rollback #:add-refund #:set-refund
   #:log-entry #:make-log-entry #:log-entry-address #:log-entry-topics #:log-entry-data
   #:add-log #:logs-since
   #:finalize-tx #:discard-tx
   ;; block-level changes
   #:account-change #:account-change-address #:account-change-deleted-p
   #:account-change-storage-wiped-p #:account-change-nonce #:account-change-balance
   #:account-change-code-hash #:account-change-code #:account-change-storage
   #:drain-changes #:backend-state-root #:storage-root
   ;; EIP-7702
   #:delegation-target #:delegation-designator))
