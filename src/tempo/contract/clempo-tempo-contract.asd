;;; defcontract, defstorable, defcall and storage backends.
(defsystem "clempo-tempo-contract"
  :description "defcontract, defstorable, defcall and storage backends."
  :depends-on ("clempo-base" "clempo-crypto" "clempo-codec" "clempo-tempo-forks")
  :serial t
  :components ((:file "package")
               (:file "storage")
               (:file "types")
               (:file "layout")
               (:file "calls")
               (:file "backends")))

(defsystem "clempo-tempo-contract/tests"
  :depends-on ("clempo-tempo-contract" "clempo-io")
  :pathname "../../../tests/tempo/contract/"
  :components ((:file "contract-tests")
               (:file "layout-tests")))
