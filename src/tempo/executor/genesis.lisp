(in-package #:clempo.executor)

(defun load-genesis (db &key (chain *chain*))
  "Fill an empty state database with CHAIN's genesis allocation and commit it as block 0
under the genesis block hash. Returns the state root."
  (invariant (= (store:db-number db) -1) "genesis into a non-empty state database")
  (store:db-begin-block db)
  (chainspec:map-genesis-accounts
   (lambda (address nonce balance code storage)
     (store:db-set-account db address nonce balance
                           (if (zerop (length code)) clempo.trie:+empty-code-hash+ (store:db-put-code db code)))
     (when storage
       (maphash (lambda (slot value) (store:db-set-storage db address slot value)) storage)))
   chain)
  (let ((root (store:db-state-root db)))
    (store:db-commit-block db 0 (chainspec:genesis-block-hash chain :state-root root))
    root))

(defun genesis-db (&key (chain *chain*))
  "A fresh state database holding CHAIN's genesis."
  (let ((db (store:make-statedb)))
    (load-genesis db :chain chain)
    db))
