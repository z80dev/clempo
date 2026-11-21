(defpackage #:clempo.chainspec
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:trie #:clempo.trie) (#:forks #:clempo.forks))
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode)
  (:export #:chain-spec #:load-chainspec #:chain-name #:chain-id #:chain-epoch-length
           #:fork-activation #:fork-active-p #:fork-at
           #:genesis-gas-limit #:genesis-base-fee #:genesis-timestamp
           #:shared-gas-limit-at #:general-gas-limit-at #:tx-gas-limit-cap-at
           #:base-fee-params-at #:next-block-base-fee
           #:map-genesis-accounts #:genesis-state-trie #:genesis-state-root
           #:genesis-header #:genesis-header-rlp #:genesis-block-hash))

(in-package #:clempo.chainspec)

(defstruct (chain-spec (:constructor %make-chain-spec) (:conc-name chain-))
  (name nil :read-only t)
  (id nil :read-only t)
  (epoch-length nil :read-only t)
  (general-gas-limit nil :read-only t)
  (document nil :read-only t)
  (schedule nil :read-only t))

(defvar *mainnet* nil)
(defvar *moderato* nil)

(defun quantity (value)
  (etypecase value
    (integer value)
    (string (if (and (>= (length value) 2) (string-equal "0x" value :end2 2))
                (hex->integer value)
                (parse-integer value)))))

(defun field-quantity (object name &optional (default 0))
  (quantity (json:jget object name default)))

(defun read-chainspec (chain file)
  (let* ((path (asdf:system-relative-pathname "clempo-tempo-chainspec" file))
         (document (json:parse-json (clempo.io:read-file-string path)))
         (config (json:jget document "config"))
         (schedule (list (cons :genesis 0))))
    ;; The shipped JSON is what TempoChainSpec::from_genesis consumes. Walk
    ;; canonical fork order backwards when selecting across tied timestamps.
    (dolist (fork (rest forks:+forks+))
      (let ((timestamp (json:jget config (format nil "~(~A~)Time" fork))))
        (when timestamp (push (cons fork (quantity timestamp)) schedule))))
    (%make-chain-spec :name chain :id (field-quantity config "chainId")
                      :epoch-length (json:jget config "epochLength")
                      :general-gas-limit (json:jget config "generalGasLimit")
                      :document document :schedule schedule)))

(defun load-chainspec (&optional (chain :mainnet))
  "Load and cache the built-in spec for MAINNET/4217 or MODERATO/42431.
CHAIN may also be a spec returned by an earlier call."
  (cond ((chain-spec-p chain) chain)
        ((or (eql chain 4217) (and (typep chain '(or string symbol))
                                  (member (string chain) '("mainnet" "presto") :test #'string-equal)))
         (or *mainnet* (setf *mainnet* (read-chainspec :mainnet "data/presto.json"))))
        ((or (eql chain 42431) (and (typep chain '(or string symbol))
                                   (member (string chain) '("moderato" "testnet") :test #'string-equal)))
         (or *moderato* (setf *moderato* (read-chainspec :moderato "data/moderato.json"))))
        (t (error "Unknown Tempo chain ~S (expected mainnet or moderato)" chain))))

(defun fork-activation (fork chain)
  "Timestamp at which FORK activates; NIL when it is not scheduled."
  (cdr (assoc fork (chain-schedule (load-chainspec chain)))))

(defun fork-active-p (fork timestamp chain)
  (let ((activation (fork-activation fork chain)))
    (and activation (>= timestamp activation))))

(defun fork-at (timestamp chain)
  "Latest Tempo fork active at the block's timestamp (seconds)."
  (loop for (fork . activation) in (chain-schedule (load-chainspec chain))
        when (>= timestamp activation) return fork
        finally (return :genesis)))

(defun genesis-gas-limit (chain)
  (field-quantity (chain-document (load-chainspec chain)) "gasLimit"))

(defun genesis-base-fee (chain)
  (field-quantity (chain-document (load-chainspec chain)) "baseFeePerGas"))

(defun genesis-timestamp (chain)
  (field-quantity (chain-document (load-chainspec chain)) "timestamp"))

(defun shared-gas-limit-at (timestamp chain gas-limit)
  (if (fork-active-p :t4 timestamp chain) 0 (floor gas-limit 10)))

(defun general-gas-limit-at (timestamp chain gas-limit
                             &optional (shared-gas-limit (shared-gas-limit-at timestamp chain gas-limit)))
  (or (chain-general-gas-limit (load-chainspec chain))
      (if (fork-active-p :t1 timestamp chain) 30000000
          (floor (- gas-limit shared-gas-limit) 2))))

(defun tx-gas-limit-cap-at (timestamp chain)
  (if (fork-active-p :t1a timestamp chain) 30000000 16777216))

(define-constant +t7-base-fee-target+ 10000000)
(define-constant +t7-base-fee-floor+ 600000000)
(define-constant +t7-base-fee-cap+ 12000000000)
(define-constant +t7-base-fee-denominator+ 8)

(defun base-fee-params-at (timestamp chain)
  "Return (values fixed-fee target floor cap denominator).
FIXED-FEE is non-NIL before T7; T7 uses an explicit gas target (elasticity 1)."
  (cond ((fork-active-p :t7 timestamp chain)
         (values nil +t7-base-fee-target+ +t7-base-fee-floor+ +t7-base-fee-cap+
                 +t7-base-fee-denominator+))
        ((fork-active-p :t1 timestamp chain) 20000000000)
        (t 10000000000)))

(defun next-block-base-fee (timestamp chain parent-base-fee parent-gas-used)
  "Base fee at the child's timestamp under Tempo's fixed fee or TIP-1067."
  (multiple-value-bind (fixed target minimum maximum denominator) (base-fee-params-at timestamp chain)
    (or fixed
        (let* ((distance (- parent-gas-used target))
               (delta (floor (* parent-base-fee (abs distance)) (* target denominator)))
               (fee (+ parent-base-fee (if (plusp distance) (max 1 delta) (- delta)))))
          (min maximum (max minimum fee))))))

(defun map-genesis-accounts (function chain)
  "Call FUNCTION for each allocation with (ADDRESS NONCE BALANCE CODE STORAGE).
ADDRESS, NONCE, BALANCE and the keys/values of STORAGE are integers. CODE is an
immutable octet vector, including empty code. STORAGE is a fresh hash table with
EQL keys (or NIL when not provided); zero slots are omitted."
  (loop for (address . info) in (cdr (json:jget (chain-document (load-chainspec chain)) "alloc"))
        for code = (hex->octets (json:jget info "code" "0x"))
        for slots = (cdr (json:jget info "storage"))
        for storage = (when slots (make-hash-table :test 'eql :size (length slots)))
        do (dolist (slot slots)
             (let ((word (hex->integer (cdr slot))))
               (unless (zerop word) (setf (gethash (hex->integer (car slot)) storage) word))))
           (funcall function (hex->integer address) (field-quantity info "nonce")
                    (field-quantity info "balance") code storage)))

(defun genesis-storage-root (storage store)
  (if (not storage)
      trie:+empty-trie-root+
      (let ((trie (trie:make-secure-trie :store store)))
        (maphash (lambda (slot word)
                   (trie:trie-put trie (integer->octets slot 32) (rlp-encode word)))
                 storage)
        (trie:trie-root trie))))

(defun genesis-state-trie (chain &key store)
  "Build the secure genesis account trie from the allocation.
With STORE, commit the storage tries as their roots are computed; TRIE-ROOT on
this trie then commits the account nodes to the same store."
  (let ((state (trie:make-secure-trie :store store)))
    (map-genesis-accounts
     (lambda (address nonce balance code storage)
       (trie:trie-put state (integer->octets address 20)
                      (trie:encode-account nonce balance (genesis-storage-root storage store)
                                           (if (zerop (length code)) trie:+empty-code-hash+
                                               (keccak256 code)))))
     chain)
    state))

(defun genesis-state-root (chain)
  (trie:trie-root (genesis-state-trie chain)))

(define-constant +empty-ommers-hash+ (keccak256 (rlp-encode nil)))
;; EIP-7685 requests hash is SHA-256 of the empty byte string at genesis.
(define-constant +empty-requests-hash+
  (hex->octets "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

(defun field-bytes (object name width)
  (let ((value (json:jget object name)))
    (if value
        (let ((bytes (hex->octets value)))
          (invariant (= width (length bytes)) "genesis field ~A must have ~D octets" name width)
          bytes)
        (make-octets width))))

(defun genesis-header (chain &key state-root)
  "Return the nested Tempo genesis header in RLP field order, using STATE-ROOT
when already computed. Both built-in chains activate London through Osaka at
block zero, so the Prague optional-field suffix is present."
  (let* ((spec (load-chainspec chain))
         (doc (chain-document spec))
         (timestamp (genesis-timestamp spec)))
    (list 0 0 (mod timestamp 1000)
          (list (field-bytes doc "parentHash" 32)
                +empty-ommers-hash+
                (field-bytes doc "coinbase" 20)
                (or state-root (genesis-state-root spec))
                trie:+empty-trie-root+ trie:+empty-trie-root+
                (make-octets 256)
                (field-quantity doc "difficulty") (field-quantity doc "number")
                (genesis-gas-limit spec) (field-quantity doc "gasUsed") timestamp
                (hex->octets (json:jget doc "extraData" "0x"))
                (field-bytes doc "mixHash" 32)
                (integer->octets (field-quantity doc "nonce") 8)
                (genesis-base-fee spec)
                trie:+empty-trie-root+
                (field-quantity doc "blobGasUsed") (field-quantity doc "excessBlobGas")
                (make-octets 32) +empty-requests-hash+))))

(defun genesis-header-rlp (chain &key state-root)
  (rlp-encode (genesis-header chain :state-root state-root)))

(defun genesis-block-hash (chain &key state-root)
  (keccak256 (genesis-header-rlp chain :state-root state-root)))
