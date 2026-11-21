(defpackage #:clempo.base
  (:use #:cl)
  (:documentation "Foundations every clempo layer builds on: build policy, invariants,
octet vectors, hex, 256-bit integer helpers and the test harness.")
  (:export
   ;; policy
   #:*build-mode* #:*hot* #:*checked*
   #:invariant #:invariant-violation #:unreachable
   #:define-constant
   ;; octets
   #:octet #:octets #:make-octets #:octets-of #:octets-concat #:octets=
   #:copy-octets #:octets-slice
   #:hex->octets #:octets->hex #:hex->integer #:integer->hex #:quantity->hex
   #:integer->octets #:octets->integer #:integer->octets-minimal
   #:string->octets #:octets->string
   ;; u256
   #:u256 #:u160 #:u64 #:+u256-max+ #:+u256-modulus+ #:+u64-max+
   #:wrap256 #:u256->s256 #:s256->u256
   ;; tests
   #:deftest #:is #:is= #:is-equalp #:signals #:skip-test
   #:run-tests #:test-failure-count #:*test-verbose*))
