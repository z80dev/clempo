(defpackage #:clempo.rlp.tests
  (:use #:cl #:clempo.base #:clempo.rlp))

(in-package #:clempo.rlp.tests)

(defun hex (o) (octets->hex o :prefix nil))

(deftest rlp-encode-vectors ()
  ;; ethereum/tests RLPTests basics
  (is= "83646f67" (hex (rlp-encode (string->octets "dog"))))
  (is= "c88363617483646f67" (hex (rlp-encode (list (string->octets "cat") (string->octets "dog")))))
  (is= "80" (hex (rlp-encode (make-octets 0))))
  (is= "c0" (hex (rlp-encode '())))
  (is= "80" (hex (rlp-encode 0)))
  (is= "0f" (hex (rlp-encode 15)))
  (is= "820400" (hex (rlp-encode 1024)))
  (is= "c7c0c1c0c3c0c1c0" (hex (rlp-encode '(() (()) (() (()))))))
  (is= "b8384c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c6974"
       (hex (rlp-encode (string->octets "Lorem ipsum dolor sit amet, consectetur adipisicing elit")))))

(deftest rlp-round-trip ()
  (let ((item (list (string->octets "cat") (list (make-octets 60) (octets-of 1)) (make-octets 0))))
    (is-equalp item (rlp-decode (rlp-encode item)))))

(deftest rlp-rejects-non-canonical ()
  (signals rlp-error (rlp-decode (hex->octets "8105")))      ; single byte as string
  (signals rlp-error (rlp-decode (hex->octets "b80100")))    ; long form for short
  (signals rlp-error (rlp-decode (hex->octets "c3010203ff"))) ; trailing
  (signals rlp-error (rlp-decode (hex->octets "83646f")))    ; truncated
  (signals rlp-error (rlp-decode-integer (octets-of 0 1))))
