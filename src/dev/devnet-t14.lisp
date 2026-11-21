(in-package #:clempo.dev)

;;; T14/T15 transactions sent to a reference devnet (scripts/devnet, --hardfork t15), so the
;;; Rust node accepts, executes and seals them; devnet-replay then runs its blocks in clempo.
;;; Accounts are xtask's localnet mnemonic ("test ... junk"), funded with pathUSD in genesis.

(defparameter *devnet-keys*
  (mapcar #'hex->octets
          '("ac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"
            "59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d"
            "5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a"
            "7c852118294e51e653712a81e05800f419141751be58f605c371e15141b007a6"
            "47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a"))
  "The localnet mnemonic's first five secp256k1 private keys (public test keys).")

(defconstant +owner-1+ #x1111)
(defconstant +owner-2+ #x2222)
(defconstant +path-usd+ #x20c0000000000000000000000000000000000000)
(defconstant +configurable-account+ #xAACC000000000000000000000000000000000000)

(defun key-address (key) (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public key)))

(defun secp-signature (key hash)
  "r || s || v, v = 27/28 as the reference re-encodes it (see clempo.signatures:canonical-signature)."
  (multiple-value-bind (r s recid) (clempo.secp256k1:sign hash key)
    (octets-concat (integer->octets r 32) (integer->octets s 32) (octets-of (+ 27 recid)))))

(defun owners-of (&rest keys)
  (sort (mapcar (lambda (k) (list (key-address k) 1)) keys) #'< :key #'first))

(defun configurable-approval (account salt version threshold owner-keys inner-digest)
  "0x05 || rlp: the configurable signature of ACCOUNT approved by OWNER-KEYS (ascending)."
  (let* ((owners (apply #'owners-of owner-keys))
         (template (clempo.signatures:make-configurable-signature
                    :account account :salt salt :version version :threshold threshold
                    :owners owners))
         (digest (clempo.signatures::configurable-digest template inner-digest)))
    (clempo.signatures:encode-configurable-signature
     (clempo.signatures:make-configurable-signature
      :account account :salt salt :version version :threshold threshold :owners owners
      :signatures (mapcar (lambda (o) (secp-signature (find (first o) owner-keys :key #'key-address)
                                                      digest))
                          owners)))))

(defun derived-account (salt threshold owner-keys)
  (clempo.signatures:configurable-account-address
   (clempo.signatures:make-configurable-signature :salt salt :threshold threshold
                                                  :owners (apply #'owners-of owner-keys))))

(defun upgrade-salt (address)
  (octets->integer (clempo.keccak:keccak256
                    (octets-concat (string->octets "tempo:configurable-account:upgrade")
                                   (integer->octets address 20)))))

(defun devnet-rpc (url method &rest params) (clempo.jsonrpc:rpc-call method params :url url))

(defun quantity-hex (n) (format nil "0x~(~x~)" n))

(defun aa-transaction (url sender calls sign &key sponsor-key key-authorization (gas 3000000))
  "A signed 0x76 envelope from SENDER: SIGN maps the signing hash to signature octets."
  (let* ((nonce (hex->integer (devnet-rpc url "eth_getTransactionCount" (quantity-hex sender) "pending")))
         (fields (append (list 1337 0 100000000000 gas
                               (mapcar (lambda (c) (list (integer->octets (first c) 20) 0 (second c))) calls)
                               '() 0 nonce 0 0 (make-octets 0)
                               (if sponsor-key (list 0 1 1) (make-octets 0))
                               '())
                         (when key-authorization (list key-authorization))))
         (dummy (clempo.transactions:decode-transaction
                 (clempo.transactions::encode-envelope #x76 (append fields (list (make-octets 65))))))
         (signature (funcall sign (clempo.transactions:transaction-signing-hash dummy))))
    (when sponsor-key
      (let ((unsigned (copy-list fields)))
        (setf (nth 11 unsigned) (integer->octets sender 20))
        (multiple-value-bind (r s recid)
            (clempo.secp256k1:sign (clempo.keccak:keccak256 (clempo.transactions::encode-envelope #x78 unsigned))
                                   sponsor-key)
          (setf (nth 11 fields) (list recid r s)))))
    (clempo.transactions::encode-envelope #x76 (append fields (list signature)))))

(defun send-and-wait (url raw label vectors)
  "eth_sendRawTransaction RAW and wait for its receipt; push (label raw status) on VECTORS."
  (let ((hash (devnet-rpc url "eth_sendRawTransaction" (octets->hex raw :prefix t))))
    (loop repeat 120
          for receipt = (devnet-rpc url "eth_getTransactionReceipt" hash)
          when (and receipt (not (eq receipt :null)))
            do (let ((status (clempo.json:jget receipt "status")))
                 (log-line (format nil "~A: ~A status ~A block ~A" label hash status
                                   (clempo.json:jget receipt "blockNumber")))
                 (push (clempo.json:jobject "label" label "raw" (octets->hex raw :prefix t)
                                            "status" status)
                       (car vectors))
                 (return hash))
          do (sleep 0.5)
          finally (error "~A: ~A not included" label hash))))

(defun send-t14-scenarios (url out)
  (destructuring-bind (k0 k1 k2 k3 k4) *devnet-keys*
    (declare (ignore k2))
    (let* ((vectors (list '()))
           (a0 (key-address k0)) (a1 (key-address k1)) (a3 (key-address k3)) (a4 (key-address k4))
           (transfer (lambda (to amount)
                       (clempo.abi:abi-encode-call (clempo.abi:abi-selector "transfer(address,uint256)")
                                                   '(:address (:uint 256)) (list to amount))))
           (primitive (lambda (key) (lambda (hash) (secp-signature key hash)))))
      ;; 1. a primitive root upgrades its own account (upgradeAccount: version 1, owner 1)
      (send-and-wait url (aa-transaction url a4
                                         (list (list +configurable-account+
                                                     (clempo.abi:abi-encode-call
                                                      (clempo.abi:abi-selector "upgradeAccount(uint8,(address,uint8)[])")
                                                      '((:uint 8) (:array (:tuple :address (:uint 8))))
                                                      (list 1 (owners-of +owner-1+)))))
                                         (funcall primitive k4))
                     "upgrade-account" vectors)
      ;; 2. the upgraded account sends with its owner quorum (0x05, version 1)
      (send-and-wait url (aa-transaction url a4 (list (list +path-usd+ (funcall transfer a3 1)))
                                         (lambda (hash) (configurable-approval a4 (upgrade-salt a4) 1 1
                                                                               (list +owner-1+) hash)))
                     "upgraded-owner-signed" vectors)
      ;; 3. its retired primitive root is refused (RootKeyRetired)
      (handler-case
          (progn (devnet-rpc url "eth_sendRawTransaction"
                             (octets->hex (aa-transaction url a4 (list (list +path-usd+ (funcall transfer a3 1)))
                                                          (funcall primitive k4))
                                          :prefix t))
                 (log-line "retired-root: ACCEPTED (expected a rejection)"))
        (clempo.jsonrpc:rpc-error (c) (log-line (format nil "retired-root rejected: ~A" c))))
      ;; 4. a fresh derived 2-of-2 account registers on first use, its fee sponsored by a0
      (let ((d (derived-account 7 2 (list +owner-1+ +owner-2+))))
        (send-and-wait url (aa-transaction url d (list (list +path-usd+ (funcall transfer a3 0)))
                                           (lambda (hash) (configurable-approval d 7 0 2 (list +owner-1+ +owner-2+) hash))
                                           :sponsor-key k0)
                       "derived-first-use-sponsored" vectors)
        ;; 5. a0 grants the derived account as an access key (key type 3) ...
        (let* ((fields (list 1337 3 (integer->octets d 20)))
               (grant (list fields (secp-signature k0 (clempo.keccak:keccak256 (clempo.rlp:rlp-encode fields))))))
          (send-and-wait url (aa-transaction url a0 (list (list +path-usd+ (funcall transfer a3 1)))
                                             (funcall primitive k0) :key-authorization grant)
                         "grant-configurable-access-key" vectors))
        ;; 6. ... and a0 sends with that key: a V2 keychain delegating to the 2-of-2 quorum
        (send-and-wait url (aa-transaction url a0 (list (list +path-usd+ (funcall transfer a3 2)))
                                           (lambda (hash)
                                             (octets-concat
                                              (octets-of 4) (integer->octets a0 20)
                                              (configurable-approval
                                               d 7 0 2 (list +owner-1+ +owner-2+)
                                               (clempo.keccak:keccak256
                                                (octets-concat (octets-of 4) hash (integer->octets a0 20)))))))
                       "keychain-v2-configurable-key" vectors))
      ;; 7. a1 asks the upgraded account isValidSignature: its 0xef marker runs the shim, for
      ;; its owner's approval and for a stranger's (both included; the receipts and roots
      ;; replay the two branches)
      (let ((hash (clempo.keccak:keccak256 (string->octets "devnet message"))))
        (loop for (label signer) in `(("is-valid-signature-shim" ,+owner-1+)
                                      ("is-valid-signature-shim-stranger" ,+owner-2+))
              for call = (clempo.abi:abi-encode-call
                          (clempo.abi:abi-selector "isValidSignature(bytes32,bytes)")
                          '((:bytes-n 32) :bytes)
                          (list (octets->integer hash)
                                (configurable-approval a4 (upgrade-salt a4) 1 1 (list signer) hash)))
              do (log-line (format nil "~A eth_call: ~A" label
                                   (handler-case
                                       (devnet-rpc url "eth_call"
                                                   (clempo.json:jobject "from" (quantity-hex a1)
                                                                        "to" (quantity-hex a4)
                                                                        "data" (octets->hex call :prefix t))
                                                   "latest")
                                     (clempo.jsonrpc:rpc-error (c) (format nil "error ~A" c)))))
                 (send-and-wait url (aa-transaction url a1 (list (list a4 call)) (funcall primitive k1))
                                label vectors)))
      (with-open-file (s out :direction :output :if-exists :supersede)
        (write-line (clempo.json:to-json-string (coerce (reverse (car vectors)) 'vector)) s))
      (length (car vectors)))))

(defcommand "devnet-t14-send" (args)
  "send T14 configurable-account transactions to a T15 reference devnet: -rpc URL -out PATH"
  (let ((url (arg args :rpc)) (out (arg args :out)))
    (unless (and url out)
      (log-line "devnet-t14-send: specify -rpc URL -out PATH")
      (return-from command 2))
    (write-line (format nil "~D transactions included" (send-t14-scenarios url out)))
    0))
