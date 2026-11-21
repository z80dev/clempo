(in-package #:clempo.dev)

;;; Differential replay of a reference devnet: every block the Rust validators produced,
;;; fetched with debug_getRawBlock and executed by clempo from the devnet's genesis; the
;;; header's gas used, receipts root, logs bloom and state root must match.

(defun devnet-raw-block (url n)
  (hex->octets (clempo.jsonrpc:rpc-call "debug_getRawBlock" (list (format nil "0x~(~x~)" n)) :url url)))

(defun devnet-head (url)
  (hex->integer (clempo.jsonrpc:rpc-call "eth_blockNumber" '() :url url)))

(defun replay-devnet (url genesis &key (from 1) to (report *standard-output*))
  "Replay blocks FROM..TO (default the head) of the devnet at URL from GENESIS (a genesis
JSON path). Returns (values blocks-matched transactions first-mismatch)."
  (let* ((chain (clempo.chainspec:load-chainspec genesis))
         (db (clempo.executor:genesis-db :chain chain))
         (to (or to (devnet-head url)))
         (txs 0))
    (loop for n from from to to
          do (let* ((block (clempo.blocks:decode-block (devnet-raw-block url n)))
                    (result (clempo.executor:execute-block db block :chain chain))
                    (mismatches (clempo.executor:block-mismatches result (clempo.blocks:block-header block))))
               (incf txs (clempo.executor:block-result-tx-count result))
               (when mismatches
                 (clempo.store:db-revert-block db)
                 (format report "block ~D: ~S~%" n mismatches)
                 (return-from replay-devnet (values (- n from) txs n)))
               (clempo.store:db-commit-block db n (clempo.blocks:block-hash block))))
    (values (1+ (- to from)) txs nil)))

(defcommand "devnet-replay" (args)
  "execute a reference devnet's blocks from genesis and match every header root: -rpc URL -genesis PATH [-to N]"
  (let ((url (arg args :rpc)) (genesis (arg args :genesis)))
    (unless (and url genesis)
      (log-line "devnet-replay: specify -rpc URL -genesis PATH")
      (return-from command 2))
    (multiple-value-bind (blocks txs mismatch)
        (replay-devnet url genesis :to (arg-int args :to nil))
      (write-line (clempo.json:to-json-string
                   (clempo.json:jobject "matched_blocks" blocks "transactions" txs
                                        "first_mismatch" (or mismatch :null))))
      (if mismatch 1 0))))
