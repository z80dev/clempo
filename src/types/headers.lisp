(defpackage #:clempo.headers
  (:use #:cl #:clempo.base #:clempo.defrlp)
  (:export #:ethereum-header #:make-ethereum-header #:decode-ethereum-header #:encode-ethereum-header
           #:ethereum-header-from-items #:ethereum-header-to-items
           #:ethereum-header-parent-hash #:ethereum-header-ommers-hash #:ethereum-header-beneficiary
           #:ethereum-header-state-root #:ethereum-header-transactions-root #:ethereum-header-receipts-root
           #:ethereum-header-logs-bloom #:ethereum-header-difficulty #:ethereum-header-number
           #:ethereum-header-gas-limit #:ethereum-header-gas-used #:ethereum-header-timestamp
           #:ethereum-header-extra-data #:ethereum-header-mix-hash #:ethereum-header-nonce
           #:ethereum-header-base-fee-per-gas #:ethereum-header-withdrawals-root
           #:ethereum-header-blob-gas-used #:ethereum-header-excess-blob-gas
           #:ethereum-header-parent-beacon-block-root #:ethereum-header-requests-hash
           #:ethereum-header-block-access-list-hash #:ethereum-header-slot-number
           #:header #:make-header #:decode-header #:encode-header #:header-from-items #:header-to-items
           #:header-general-gas-limit #:header-shared-gas-limit #:header-timestamp-millis-part
           #:header-inner #:header-consensus-context #:header-raw #:header-hash #:header-timestamp-millis
           #:header-extra-data #:header-state-root #:header-parent-hash #:header-number #:header-timestamp
           #:consensus-context #:make-consensus-context #:consensus-context-epoch
           #:consensus-context-view #:consensus-context-parent-view #:consensus-context-proposer))
(in-package #:clempo.headers)

(defrlp ethereum-header
  (parent-hash (:bytes 32) :doc "Parent block hash.")
  (ommers-hash (:bytes 32) :doc "Keccak of the ommer header list.")
  (beneficiary :address :doc "Fee beneficiary address.")
  (state-root (:bytes 32) :doc "Post-block state trie root.")
  (transactions-root (:bytes 32) :doc "Ordered transaction trie root.")
  (receipts-root (:bytes 32) :doc "Ordered receipt trie root.")
  (logs-bloom (:bytes 256) :doc "2048-bit log bloom, big endian.")
  (difficulty :u256)
  (number :u64)
  (gas-limit :u64)
  (gas-used :u64)
  (timestamp :u64 :doc "Whole seconds since Unix epoch.")
  (extra-data :bytes)
  (mix-hash (:bytes 32))
  (nonce (:bytes 8))
  (base-fee-per-gas :u64 :trailing t :doc "London base fee; NIL before London.")
  (withdrawals-root (:bytes 32) :trailing t)
  (blob-gas-used :u64 :trailing t)
  (excess-blob-gas :u64 :trailing t)
  (parent-beacon-block-root (:bytes 32) :trailing t)
  (requests-hash (:bytes 32) :trailing t)
  (block-access-list-hash (:bytes 32) :trailing t)
  (slot-number :u64 :trailing t))

(defrlp consensus-context
  (epoch :u64)
  (view :u64)
  (parent-view :u64)
  (proposer (:bytes 32) :doc "Compressed Ed25519 proposer public key."))

(defrlp header
  (general-gas-limit :u64 :doc "Main block non-payment gas limit.")
  (shared-gas-limit :u64 :doc "Gas limit shared with the subblocks section.")
  (timestamp-millis-part :u64 :doc "Sub-second timestamp milliseconds.")
  (inner (:struct ethereum-header-from-items ethereum-header-to-items))
  (consensus-context (:struct consensus-context-from-items consensus-context-to-items)
                     :trailing t :doc "Consensus metadata; absent in earlier forks."))

(defun header-hash (header)
  (clempo.keccak:keccak256 (or (header-raw header) (encode-header header))))

(defun header-timestamp-millis (header)
  (min #xffffffffffffffff
       (+ (min #xffffffffffffffff (* 1000 (ethereum-header-timestamp (header-inner header))))
          (header-timestamp-millis-part header))))

(defmacro define-inner-header-readers (&rest fields)
  `(progn
     ,@(loop for field in fields
             for outer = (intern (format nil "HEADER-~A" field))
             for inner = (intern (format nil "ETHEREUM-HEADER-~A" field))
             collect `(progn
                        (declaim (inline ,outer))
                        (defun ,outer (header) (,inner (header-inner header)))))))

(define-inner-header-readers extra-data state-root parent-hash number timestamp)
