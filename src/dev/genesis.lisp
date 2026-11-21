(in-package #:clempo.dev)

(defcommand "genesis" (args)
  "compute the genesis state root: genesis [-chain mainnet|moderato]"
  (write-line (octets->hex (clempo.chainspec:genesis-state-root (arg args :chain :mainnet))
                          :prefix t))
  0)

(defun score-genesis (chain url)
  (let* ((root (clempo.chainspec:genesis-state-root chain))
         (hash (clempo.chainspec:genesis-block-hash chain :state-root root))
         (actual-root (octets->hex root :prefix t))
         (actual-hash (octets->hex hash :prefix t))
         (block (clempo.jsonrpc:rpc-call "eth_getBlockByNumber" '("0x0" :false) :url url))
         (expected-root (clempo.json:jget block "stateRoot"))
         (expected-hash (clempo.json:jget block "hash"))
         (match (and (string-equal actual-root expected-root)
                     (string-equal actual-hash expected-hash))))
    (clempo.json:jobject "score" (if match "yes" "no")
                        "stateRoot" actual-root "referenceStateRoot" expected-root
                        "hash" actual-hash "referenceHash" expected-hash)))

(defrung "R2" (args)
  "compute both genesis state roots and block hashes and compare to live Tempo RPC"
  (let ((mainnet (score-genesis :mainnet (clempo.jsonrpc:local-rpc-url)))
        (moderato (score-genesis :moderato clempo.jsonrpc:+moderato-rpc+)))
    (clempo.json:jobject "score" (if (and (string= "yes" (clempo.json:jget mainnet "score"))
                                         (string= "yes" (clempo.json:jget moderato "score")))
                                    "yes" "no")
                        "mainnet" mainnet "moderato" moderato)))
