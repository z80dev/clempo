(defpackage #:clempo.chainspec
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:json #:clempo.json) (#:trie #:clempo.trie) (#:forks #:clempo.forks))
  (:import-from #:clempo.keccak #:keccak256)
  (:import-from #:clempo.rlp #:rlp-encode)
  (:export #:chain-spec #:load-chainspec #:chain-name #:chain-id #:chain-epoch-length #:chain-config
           #:fork-activation #:fork-active-p #:fork-at
           #:genesis-gas-limit #:genesis-base-fee #:genesis-timestamp #:genesis-extra-data
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

(defun field (object name &optional default)
  (let ((value (json:jget object name)))
    (if (or (null value) (eq value :null)) default value)))

(defun field-quantity (object name &optional (default 0))
  (let ((value (field object name default)))
    (when value (quantity value))))

(defun read-chainspec (chain path)
  (let* ((document (json:parse-json (clempo.io:read-file-string path)))
         (config (json:jget document "config"))
         (schedule (list (cons :genesis 0))))
    ;; The shipped JSON is what TempoChainSpec::from_genesis consumes. Walk
    ;; canonical fork order backwards when selecting across tied timestamps.
    (dolist (fork (rest forks:+forks+))
      (let ((timestamp (field-quantity config (format nil "~(~A~)Time" fork) nil)))
        (when timestamp (push (cons fork timestamp) schedule))))
    (%make-chain-spec :name chain :id (field-quantity config "chainId")
                      :epoch-length (field-quantity config "epochLength" nil)
                      :general-gas-limit (field-quantity config "generalGasLimit" nil)
                      :document document :schedule schedule)))

(defun builtin-chainspec (chain file)
  (read-chainspec chain (asdf:system-relative-pathname "clempo-tempo-chainspec" file)))

(defun load-chainspec (&optional (chain :mainnet))
  "Load and cache MAINNET/4217 or MODERATO/42431, or read a custom genesis JSON.
CHAIN may be a pathname, namestring, or spec returned by an earlier call."
  (cond ((chain-spec-p chain) chain)
        ((or (eql chain 4217) (and (typep chain '(or string symbol))
                                  (member (string chain) '("mainnet" "presto") :test #'string-equal)))
         (or *mainnet* (setf *mainnet* (builtin-chainspec :mainnet "data/presto.json"))))
        ((or (eql chain 42431) (and (typep chain '(or string symbol))
                                   (member (string chain) '("moderato" "testnet") :test #'string-equal)))
         (or *moderato* (setf *moderato* (builtin-chainspec :moderato "data/moderato.json"))))
        ((or (pathnamep chain) (stringp chain))
         (let ((path (pathname chain)))
           (read-chainspec (pathname-name path) path)))
        (t (error "Unknown Tempo chain ~S (expected mainnet, moderato or a genesis pathname)" chain))))

(defun chain-config (chain)
  "Return the read-only genesis JSON config object."
  (json:jget (chain-document (load-chainspec chain)) "config"))

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

(defun genesis-fork-active-p (spec key at)
  (let ((activation (field-quantity (chain-config spec) key nil)))
    (and activation (<= activation at))))

(defun genesis-base-fee (chain)
  "Genesis London base fee, or NIL if London has not activated."
  (let* ((spec (load-chainspec chain))
         (doc (chain-document spec)))
    (when (genesis-fork-active-p spec "londonBlock" 0)
      (field-quantity doc "baseFeePerGas" 1000000000))))

(defun genesis-timestamp (chain)
  (field-quantity (chain-document (load-chainspec chain)) "timestamp"))

(defun genesis-extra-data (chain)
  "Genesis header extraData, including the initial DKG outcome on consensus chains."
  (hex->octets (field (chain-document (load-chainspec chain)) "extraData" "0x")))

(defun shared-gas-limit-at (timestamp chain gas-limit)
  (forks:shared-gas-limit (fork-at timestamp chain) gas-limit))

(defun general-gas-limit-at (timestamp chain gas-limit
                             &optional (shared-gas-limit (shared-gas-limit-at timestamp chain gas-limit)))
  (or (chain-general-gas-limit (load-chainspec chain))
      (forks:general-gas-limit (fork-at timestamp chain))
      (floor (- gas-limit shared-gas-limit) 2)))

(defun tx-gas-limit-cap-at (timestamp chain)
  (forks:tx-gas-limit-cap (fork-at timestamp chain)))

(defun base-fee-params-at (timestamp chain)
  "Return (values fixed-fee target floor cap denominator).
FIXED-FEE is non-NIL before T7; T7 uses an explicit gas target (elasticity 1)."
  (let ((fork (fork-at timestamp chain)))
    (cond ((forks:fork>= fork :t7)
           (values nil forks:+t7-base-fee-gas-target+ forks:+t7-base-fee-floor+
                   forks:+t7-base-fee-cap+ 8))
          ((forks:fork>= fork :t1) forks:+t1-base-fee+)
          (t forks:+t0-base-fee+))))

(defun next-block-base-fee (timestamp chain parent-base-fee parent-gas-used)
  "Base fee at the child's timestamp under Tempo's fixed fee or TIP-1067."
  (or (base-fee-params-at timestamp chain)
      (forks:t7-next-block-base-fee parent-base-fee parent-gas-used)))

(defun map-genesis-accounts (function chain)
  "Call FUNCTION for each allocation with (ADDRESS NONCE BALANCE CODE STORAGE).
ADDRESS, NONCE, BALANCE and the keys/values of STORAGE are integers. CODE is an
immutable octet vector, including empty code. STORAGE is a fresh hash table with
EQL keys (or NIL when not provided); zero slots are omitted."
  (loop for (address . info) in (cdr (json:jget (chain-document (load-chainspec chain)) "alloc"))
        for code = (hex->octets (field info "code" "0x"))
        for slots = (cdr (field info "storage"))
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
  (let ((value (field object name)))
    (if value
        (let ((bytes (hex->octets value)))
          (invariant (= width (length bytes)) "genesis field ~A must have ~D octets" name width)
          bytes)
        (make-octets width))))

(defun genesis-header (chain &key state-root)
  "Return the nested Tempo genesis header in RLP field order, using STATE-ROOT
when already computed. The chain's Ethereum fork activations determine the suffix."
  (let* ((spec (load-chainspec chain))
         (doc (chain-document spec))
         (timestamp (genesis-timestamp spec))
         (base-fee (genesis-base-fee spec)))
    (list 0 0 (mod timestamp 1000)
          (append
           (list (field-bytes doc "parentHash" 32)
                 +empty-ommers-hash+
                 (field-bytes doc "coinbase" 20)
                 (or state-root (genesis-state-root spec))
                 trie:+empty-trie-root+ trie:+empty-trie-root+
                 (make-octets 256)
                 (field-quantity doc "difficulty") (field-quantity doc "number")
                 (genesis-gas-limit spec) 0 timestamp
                 (genesis-extra-data spec)
                 (field-bytes doc "mixHash" 32)
                 (integer->octets (field-quantity doc "nonce") 8))
           (when base-fee (list base-fee))
           (when (genesis-fork-active-p spec "shanghaiTime" timestamp)
             (list trie:+empty-trie-root+))
           (when (genesis-fork-active-p spec "cancunTime" timestamp)
             (list (field-quantity doc "blobGasUsed") (field-quantity doc "excessBlobGas")
                   (make-octets 32)))
           (when (genesis-fork-active-p spec "pragueTime" timestamp)
             (list +empty-requests-hash+))
           (when (genesis-fork-active-p spec "amsterdamTime" timestamp)
             ;; Empty block access list and empty ommers both commit to RLP([]).
             (list +empty-ommers-hash+ (field-quantity doc "slotNumber")))))))

(defun genesis-header-rlp (chain &key state-root)
  (rlp-encode (genesis-header chain :state-root state-root)))

(defun genesis-block-hash (chain &key state-root)
  (keccak256 (genesis-header-rlp chain :state-root state-root)))
