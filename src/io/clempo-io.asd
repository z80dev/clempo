(defsystem "clempo-io"
  :description "Clock, entropy, files and processes behind special variables"
  :depends-on ("clempo-base" (:require "sb-bsd-sockets") (:require "sb-posix"))
  :serial t
  :components ((:file "io")
               (:file "clock")))

(defsystem "clempo-io/tests"
  :depends-on ("clempo-io")
  :pathname "../../tests/io/"
  :components ((:file "io-tests")))
