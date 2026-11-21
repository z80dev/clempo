;;; The import graph. Each entry is a directory under src/ and one ASDF system
;;; "clempo-<dir>". A file may refer only to packages defined in its own directory or
;;; in a directory listed above it. :io t marks the directories allowed to touch the
;;; clock, randomness, files, processes, sockets and threads directly; every other
;;; layer gets them through clempo.io's special variables.
;;; scripts/check-layers.lisp enforces this from verify.sh. :io-exempt names files
;;; (relative to the directory) that may use io tokens in a non-io layer; the reason is
;;; recorded next to the entry.

((:dir "base" :io-exempt ("test.lisp"))   ; the test harness times tests
 (:dir "io" :io t)
 (:dir "crypto")
 (:dir "codec")
 (:dir "trie")                              ; before types: tx/receipt roots are tries
 (:dir "types")
 (:dir "store" :io t)
 (:dir "state")
 (:dir "evm")
 (:dir "tempo")
 (:dir "consensus")
 (:dir "p2p" :io t)
 (:dir "sync" :io t)
 (:dir "rpc" :io t)
 (:dir "sim")
 (:dir "node" :io t)
 (:dir "dev" :io t))
