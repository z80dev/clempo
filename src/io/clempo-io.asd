(defsystem "clempo-io"
  :description "Clock, entropy, files, processes and socket polling behind special variables"
  :depends-on ("clempo-base" (:require "sb-bsd-sockets") (:require "sb-posix"))
  :serial t
  :components ((:file "io")
               (:file "clock")
               (:file "poll")
               (:file "locks")
               (:file "guards")))

(defsystem "clempo-io/tests"
  :depends-on ("clempo-io")
  :pathname "../../tests/io/"
  :components ((:file "io-tests") (:file "lock-tests") (:file "guard-tests")))
