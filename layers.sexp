;;; The import graph. Each entry is a directory under src/ and one ASDF system
;;; "clempo-<dir>". A file may refer only to packages defined in its own directory or
;;; in a directory listed above it. :io t permits native I/O layer-wide, while
;;; :io-exempt lists the specific files allowed to use native file/process/socket
;;; and thread interfaces. Clock and entropy still come from clempo.io.

((:dir "base" :io-exempt ("test.lisp"))   ; the test harness times tests
 (:dir "io" :io t)
 (:dir "crypto")
 (:dir "net" :io t)                         ; TLS client, trust anchors, HTTP client: sockets and files
 (:dir "codec")
 (:dir "trie")                              ; before types: tx/receipt roots are tries
 (:dir "types")
 ;; Persistence owns WALs/checkpoints and mapped history; statedb coordinates concurrent readers.
 (:dir "store" :io-exempt ("persist.lisp" "history.lisp" "history-write.lisp" "statedb.lisp"))
 (:dir "state")
 (:dir "evm")
 (:dir "tempo")
 (:dir "consensus")
 ;; Socket transport, discovery, and backoff; protocol encoding remains pure.
 (:dir "p2p" :io-exempt ("wire.lisp" "cwp2p.lisp" "discovery.lisp" "eth-source.lisp" "eth.lisp"))
 ;; Archive persistence and fetch/follow boundary.
 (:dir "sync" :io-exempt ("archive.lisp" "jsonrpc.lisp" "follow.lisp" "metrics.lisp" "pool.lisp"))
 ;; Node transport and shared backend/index state; method implementations are otherwise pure.
 (:dir "rpc" :io-exempt ("server.lisp" "chain.lisp" "consensus.lisp" "render.lisp"
                         "sender.lisp" "state.lisp" "tries.lisp" "logs.lisp" "pool.lisp"
                         "websocket.lisp"))
 (:dir "sim")
 ;; Validator process, peer supervision, and persistent configuration/journal.
 (:dir "validator" :io-exempt ("engine.lisp" "journal.lisp" "manifest.lisp" "network.lisp"
                               "keys.lisp"))
 (:dir "node" :io t)
 (:dir "dev" :io t))
