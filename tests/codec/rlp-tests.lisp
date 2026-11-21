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

(defun nested-lists (depth)
  "DEPTH lists, each holding only the next, around an empty list: built inside out without
recursion, so the fixture itself never needs a deep stack."
  (let ((payloads (make-array depth :element-type 'fixnum)))
    ;; PAYLOADS[i] is the payload length of the list i levels from the outside.
    (loop with inner = 1                               ; the innermost C0
          for i from (1- depth) downto 0
          do (setf (aref payloads i) inner
                   inner (+ inner (header-length inner))))
    (let* ((total (+ (aref payloads 0) (header-length (aref payloads 0))))
           (out (make-octets total)) (pos 0))
      (dotimes (i depth)
        (setf pos (write-header out pos (aref payloads i) #xc0)))
      (setf (aref out pos) #xc0)
      out)))

(deftest rlp-nesting-depth-is-bounded ()
  ;; 32 levels around the innermost empty list (33 lists in all) exceed the limit.
  (is= 32 *rlp-max-depth*)
  (let ((ok (rlp-decode (nested-lists 31))))
    (loop repeat 31 do (setf ok (first ok)))
    (is (null ok)))
  (signals rlp-error (rlp-decode (nested-lists 32)))
  (is (listp (rlp-decode (nested-lists 32) :max-depth 33)))
  ;; Hostile depth fails at the limit, far from the control stack: 20,000 levels once
  ;; exhausted it.
  (signals rlp-error (rlp-decode (nested-lists 20000)))
  (signals rlp-error (rlp-decode (nested-lists 200000)))
  ;; A run of open list headers with no payload behind them.
  (signals rlp-error (rlp-decode (make-array 100000 :element-type '(unsigned-byte 8)
                                                    :initial-element #xc1))))

(deftest rlp-hostile-lengths ()
  (signals rlp-error (rlp-decode (hex->octets "bfffffffffffffffff00")))  ; 2^64-1 byte string
  (signals rlp-error (rlp-decode (hex->octets "ffffffffffffffffff00")))  ; 2^64-1 byte list
  (signals rlp-error (rlp-decode (hex->octets "b9ffff")))
  (signals rlp-error (rlp-decode (hex->octets "c0") :start 2))
  (signals rlp-error (rlp-decode (hex->octets "c0") :end 5))
  (signals rlp-error (rlp-decode (make-octets 0))))
