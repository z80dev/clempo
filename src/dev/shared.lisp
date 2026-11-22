(in-package #:clempo.dev)

;;; Variables and constants that more than one command file reads. Command files load in
;;; alphabetical order (clempo-dev.asd), so a definition one of them shares with a file
;;; sorted before it lives here, loaded first, and the compiler never meets a free
;;; variable.

;;; Accounts of the reference devnet (devnet-t14.lisp, devnet-rpc.lisp): xtask's localnet
;;; mnemonic ("test ... junk"), funded with pathUSD in genesis.

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

;;; Datadirs of replay.lisp and bench.lisp.

(defparameter *replay-sync* '(:every-ms 1000)
  "The block log's sync policy for replay and bench datadirs: a replay acknowledges no block
to anyone, so it syncs its log at most once a second (and at each flush and on close)
instead of after each block; a crash resumes from the last synced block
\(CLEMPO.STORE:STORE-DURABLE-NUMBER).")
