;;; Owner: the executor agent. Tempo block execution, receipts, roots and resumable replay.
(defsystem "clempo-tempo-executor"
  :description "Owner: the executor agent. Tempo block executor and replay with restarts."
  :depends-on ("clempo-base" "clempo-io" "clempo-crypto" "clempo-codec" "clempo-trie" "clempo-types"
               "clempo-store" "clempo-state" "clempo-evm" "clempo-tempo-forks"
               "clempo-tempo-chainspec" "clempo-tempo-contract" "clempo-tempo-precompiles")
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "genesis")
               (:file "block")
               (:file "evm-glue")
               (:file "authorizations")
               (:file "lanes")
               (:file "handler")
               (:file "aa")
               (:file "system-calls")
               (:file "replay")
               (:file "call")))                ; HistoryAgent: call contexts over any state view

(defsystem "clempo-tempo-executor/tests"
  :depends-on ("clempo-tempo-executor" "clempo-io")
  :pathname "../../../tests/tempo/executor/"
  :serial t
  :components ((:file "executor-tests")
               (:file "authorizations-tests")
               (:file "lanes-tests")
               (:file "system-calls-tests")
               (:file "mainnet-blocks-tests")
               (:file "call-tests")))
