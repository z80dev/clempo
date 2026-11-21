(in-package #:clempo.txpool.fixtures)

(defconstant +fixture-token+ #x20c000000000000000000000b9537d11c60e8b50)
(defconstant +fee-manager+ #xfeec000000000000000000000000000000000000)
(defconstant +nonce-manager+ #x4e4f4e4345000000000000000000000000000000)
(defconstant +keychain+ #xaaaaaaaa00000000000000000000000000000000)
(defconstant +fixture-timestamp+ 1000)
(defconstant +fixture-base-fee+ 1000000000)

(defun mapping-slot (key slot)
  (octets->integer (clempo.keccak:keccak256
                    (octets-concat (integer->octets key 32) (integer->octets slot 32)))))

(defun fixture-key (private)
  "The secp256k1 private key of the small fixture scalar PRIVATE."
  (integer->octets private 32))

(defun fixture-address (private)
  (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public (fixture-key private))))

(defun fixture-fund (db address &optional (amount 1000000000000000000) (nonce 0))
  "Synthetic account funding, explicitly separate from the recorded prestate."
  (clempo.store:db-set-account db address nonce 0 clempo.trie:+empty-code-hash+)
  (clempo.store:db-set-storage db +fixture-token+ (mapping-slot address 9) amount)
  (clempo.store:db-set-storage db +fee-manager+ (mapping-slot address 1) +fixture-token+))

(defun fixture-set-nonce (db address nonce-key nonce)
  (if (zerop nonce-key)
      (let ((account (clempo.store:db-account db address)))
        (clempo.store:db-set-account db address nonce (if account (clempo.store:account-balance account) 0)
                                    (if account (clempo.store:account-code-hash account)
                                        clempo.trie:+empty-code-hash+)))
      (clempo.store:db-set-storage db +nonce-manager+
                                  (mapping-slot nonce-key (mapping-slot address 0)) nonce)))

(defun fixture-set-key (db account key-id expiry &key revoked enforce-limits (signature-type 0))
  "Raw AuthorizedKey packing from Rust account_keychain/mod.rs (bytes 0,1..8,9,10).
This is actual backend storage, not a state-reader override."
  (clempo.store:db-set-storage
   db +keychain+ (mapping-slot key-id (mapping-slot account 0))
   (logior signature-type (ash expiry 8) (if enforce-limits (ash 1 72) 0)
           (if revoked (ash 1 80) 0))))

(defun fixture-change-state (reader function)
  (let ((db (clempo.txpool:state-reader-db reader)))
    (clempo.store:db-begin-block db)
    (handler-case
        (progn (funcall function db)
               (clempo.store:db-commit-block db (1+ (clempo.store:db-number db)) (make-octets 32)))
      (error (condition)
        (when (clempo.store:db-block-open-p db) (clempo.store:db-revert-block db))
        (error condition)))))

(defun load-prestate (db)
  (let* ((path (asdf:system-relative-pathname
                "clempo-tempo-txpool" "../../../tests/tempo/txpool/data/mainnet-49400084-tx0-prestate.json"))
         (document (clempo.json:parse-json (clempo.io:read-file-string path))))
    (dolist (item (cdr document))
      (let* ((address (hex->integer (car item))) (account (cdr item))
             (code (hex->octets (clempo.json:jget account "code" "0x"))))
        (clempo.store:db-set-account db address (clempo.json:jget account "nonce" 0)
                                    (hex->integer (clempo.json:jget account "balance" "0x0"))
                                    (clempo.store:db-put-code db code))
        (dolist (slot (cdr (clempo.json:jget account "storage")))
          (clempo.store:db-set-storage db address (hex->integer (car slot))
                                      (hex->integer (cdr slot))))))))

(defun make-fixture-reader (&key (timestamp +fixture-timestamp+) (fork :t13)
                                 (base-fee +fixture-base-fee+) (chain-id 4217))
  "Load the recorded mainnet fee prestate; explicitly fund private keys 1..8 and select
the recorded token for validator 1. No chain initialization or balances are fabricated
by the validation backend: each synthetic write below is ordinary committed state."
  (let ((db (clempo.store:make-statedb)))
    (clempo.store:db-begin-block db)
    (load-prestate db)
    (let ((marker (clempo.store:db-put-code db (octets-of #xef))))
      (dolist (address (list +nonce-manager+ +keychain+))
        (clempo.store:db-set-account db address 0 0 marker)))
    ;; Synthetic funded accounts use the built-in allow-all transfer policy. Keep
    ;; nextQuoteToken in the same packed slot; recorded policy membership is partial.
    (clempo.store:db-set-storage
     db +fixture-token+ 7
     (logior (ldb (byte 160 0) (clempo.store:db-storage db +fixture-token+ 7)) (ash 1 160)))
    (loop for private from 1 to 8 do (fixture-fund db (fixture-address private)))
    (clempo.store:db-set-storage db +fee-manager+ (mapping-slot 1 0) +fixture-token+)
    (clempo.store:db-commit-block db 0 (make-octets 32))
    (clempo.txpool:make-state-reader
     db (clempo.executor:make-block-env :number 1 :timestamp timestamp :beneficiary 1
                                       :gas-limit 50000000 :base-fee base-fee
                                       :chain-id chain-id :spec fork))))

(defun envelope (type fields)
  (let ((rlp (clempo.rlp:rlp-encode fields)))
    (if (zerop type) rlp (octets-concat (octets-of type) rlp))))

(defun signature-bytes (hash private)
  (multiple-value-bind (r s parity) (clempo.secp256k1:sign hash (fixture-key private))
    (octets-concat (integer->octets r 32) (integer->octets s 32) (octets-of parity))))

(defun fixture-transaction (&key (type #x76) (private-key 1) key-private-key
                                (key-version 4) sponsor-private-key
                                (nonce 0) (nonce-key 0) valid-before valid-after
                                (fee-token +fixture-token+) (fee-cap 2000000000)
                                (priority-fee 1000000000) (chain-id 4217)
                                (gas-limit 2000000) (value 0) calls access-list
                                key-authorization tempo-authorizations authorization-list)
  "Deterministically SIGN a real wire transaction, then decode it through the public codec.
No unchecked transaction constructors, mocked signatures or pool-validation overrides."
  (let* ((sender (fixture-address private-key))
         (to (integer->octets #x1234 20))
         (empty (make-octets 0))
         (fields
           (case type
             (0 (list nonce fee-cap gas-limit to value empty (+ 35 (* chain-id 2)) 1 1))
             (1 (list chain-id nonce fee-cap gas-limit to value empty access-list 0 1 1))
             ((2 4) (append (list chain-id nonce priority-fee fee-cap gas-limit to value empty access-list)
                             (when (= type 4) (list authorization-list)) (list 0 1 1)))
             (#x76
              (append (list chain-id priority-fee fee-cap gas-limit
                            (or calls (list (list to value empty))) access-list nonce-key nonce
                            (or valid-before 0) (or valid-after 0)
                            (if fee-token (integer->octets fee-token 20) empty)
                            (if sponsor-private-key '(0 1 1) empty) tempo-authorizations)
                      (when key-authorization (list key-authorization)) (list (make-octets 65))))
             (otherwise (error "Unsupported fixture tx type"))))
         (unsigned (clempo.transactions:decode-transaction (envelope type fields)))
         (hash (clempo.transactions:transaction-signing-hash unsigned)))
    (if (= type #x76)
        (let* ((key-hash (if (and key-private-key (= key-version 4))
                             (clempo.keccak:keccak256
                              (octets-concat (octets-of 4) hash (integer->octets sender 20))) hash))
               (signature (signature-bytes key-hash (or key-private-key private-key))))
          (when key-private-key
            (setf signature (octets-concat (octets-of key-version) (integer->octets sender 20) signature)))
          (setf (car (last fields)) signature)
          (when sponsor-private-key
            (let ((payer-fields (butlast fields)))
              (setf (nth 11 payer-fields) (integer->octets sender 20))
              (multiple-value-bind (r s parity)
                  (clempo.secp256k1:sign (clempo.keccak:keccak256 (envelope #x78 payer-fields))
                                         (fixture-key sponsor-private-key))
                (setf (nth 11 fields) (list parity r s))))))
        (multiple-value-bind (r s parity) (clempo.secp256k1:sign hash (fixture-key private-key))
          (setf (nth (- (length fields) 3) fields)
                (if (zerop type) (+ 35 (* chain-id 2) parity) parity)
                (nth (- (length fields) 2) fields) r
                (nth (1- (length fields)) fields) s)))
    (clempo.transactions:decode-transaction (envelope type fields))))

(defun fixture-key-authorization (&key (authorizer 1) (key-private 2) (chain-id 4217)
                                      (expiry 2000) limits)
  (let ((fields (append (list chain-id 0 (integer->octets (fixture-address key-private) 20)
                              expiry) (when limits (list limits)))))
    (list fields (signature-bytes (clempo.keccak:keccak256 (clempo.rlp:rlp-encode fields))
                                  authorizer))))

(defun fixture-eip7702-authorization (&key (private-key 2) (chain-id 4217) (nonce 0))
  (let* ((fields (list chain-id (integer->octets #x5678 20) nonce))
         (hash (clempo.keccak:keccak256
                (octets-concat (octets-of 5) (clempo.rlp:rlp-encode fields)))))
    (multiple-value-bind (r s parity) (clempo.secp256k1:sign hash (fixture-key private-key))
      (append fields (list parity r s)))))

(defun recorded-transaction (label)
  (let* ((path (asdf:system-relative-pathname "clempo-types" "../../tests/types/data/transaction-rpc.sexp"))
         (rows (with-open-file (in path) (let ((*read-eval* nil)) (read in))))
         (row (assoc label rows :test #'string=)))
    (unless row (error "Unknown recorded fixture ~A" label))
    (values (clempo.transactions:decode-transaction (hex->octets (second row))) row)))

(defun run-fixture-validation ()
  "Validate signed edge cases against the recorded state. Return actual admission and
rejection counts; any unexpected outcome signals rather than contributing a fake score."
  (let* ((reader (make-fixture-reader))
         (pool (clempo.txpool:make-pool)) (accepted 0) (rejected 0)
         (cases (list (list :aa (fixture-transaction) nil)
                      (list :ethereum (fixture-transaction :type 2 :private-key 2) nil)
                      (list :nonce-key (fixture-transaction :nonce-key 9 :private-key 3) nil)
                      (list :sponsored (fixture-transaction :sponsor-private-key 5 :private-key 4) nil)
                      (list :expiry-buffer (fixture-transaction :valid-before 1003) :expired)
                      (list :future-bound (fixture-transaction :valid-after 1121) :valid-after-too-far)
                      (list :fee-token (fixture-transaction :fee-token #x1234) :invalid-transaction)
                      (list :self-sponsored (fixture-transaction :sponsor-private-key 1) :invalid-transaction)
                      (list :system (recorded-transaction "legacy-system") :system-transaction))))
    (let ((clempo.io:*clock* (lambda () (* +fixture-timestamp+ 1000))))
      (dolist (case cases)
        (destructuring-bind (name tx expected) case
          (let ((actual (handler-case (progn (clempo.txpool:submit-transaction pool tx reader) nil)
                          (clempo.txpool:pool-error (condition)
                            (clempo.txpool:pool-error-code condition)))))
            (unless (eq expected actual)
              (error "Pool fixture ~A expected ~S, got ~S" name expected actual))
            (if actual (incf rejected) (incf accepted))))))
    (values accepted rejected (length (clempo.txpool:pool-best-transactions pool)))))
