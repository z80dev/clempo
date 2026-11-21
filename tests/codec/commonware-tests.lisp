(defpackage #:clempo.cwcodec.tests
  (:use #:cl #:clempo.base #:clempo.cwcodec))

(in-package #:clempo.cwcodec.tests)

(defun hex (o) (octets->hex o :prefix nil))

(defun varint-hex (n &optional (bits 64))
  (let ((w (make-cw-writer))) (write-varint w n bits) (hex (cw-writer-octets w))))

(defun read-varint-hex (h &optional (bits 64))
  (read-varint (make-cw-reader (hex->octets h)) bits))

(deftest cw-varint-vectors ()
  ;; commonware-codec varint.rs tests
  (is= "00" (varint-hex 0))
  (is= "7f" (varint-hex 127))
  (is= "8001" (varint-hex 128))
  (is= "808001" (varint-hex 16384))
  (is= "80808001" (varint-hex 2097152))
  (is= "ffffffff0f" (varint-hex #xffffffff 32))
  (is= "ffffffffffffffffff01" (varint-hex +u64-max+))
  (is= 2314 (read-varint-hex "8a12"))
  (is= #xffffffff (read-varint-hex "ffffffff0f" 32))
  (is= +u64-max+ (read-varint-hex "ffffffffffffffffff01"))
  (is= 3 (varint-size 16384))
  ;; zero continuation byte, overflow, truncation
  (signals cw-error (read-varint-hex "8000"))
  (signals cw-error (read-varint-hex "ffffffff1f" 32))
  (signals cw-error (read-varint-hex "80808080808080808002"))
  (signals cw-error (read-varint-hex "8080808080" 32))
  (signals cw-error (read-varint-hex "80"))
  ;; zigzag
  (let ((w (make-cw-writer))) (write-svarint w -3) (is= "05" (hex (cw-writer-octets w))))
  (is= -3 (read-svarint (make-cw-reader (hex->octets "05"))))
  (is= 64 (read-svarint (make-cw-reader (hex->octets "8001")))))

(deftest cw-union-unique ()
  (is= "0e54454d504f5f46494e414c495a45abcd"
       (hex (union-unique (string->octets "TEMPO_FINALIZE") (hex->octets "abcd"))))
  (is= "54454d504f5f53454544" (hex (namespace-union (string->octets "TEMPO")
                                                     (string->octets "_SEED")))))

;;; A declared struct: commonware simplex's Proposal over a 32-byte digest.

(defcodec test-round (epoch :varint) (view :varint))
(defcodec test-proposal (round test-round) (parent :varint) (payload (:array 32)))

(defcodec test-everything
  (a :u8) (b :u16) (c :u64) (d :bool) (e (:varint 32))
  (f :bytes) (g (:vec :u32 :max 4)) (h (:set (:array 2)))
  (i (:map :varint :bool)) (j (:option test-round)) (k (:option :u8))
  (l (:tuple :u8 :bytes)))

(defcwunion test-vote
  (0 test-proposal)
  (1 :nullify test-round)
  (2 :empty))

(deftest cw-defcodec-proposal ()
  ;; the proposal at the head of mainnet's finalization for block 50,000,000
  (let* ((bytes (hex->octets "8a12e18901e089015a9fe6ea2a084aa07486b8c24a037b98743b3b1e065dfdbf5f11b5ff0e22fd23"))
         (p (cw-decode 'test-proposal bytes)))
    (is= 2314 (test-round-epoch (test-proposal-round p)))
    (is= 17633 (test-round-view (test-proposal-round p)))
    (is= 17632 (test-proposal-parent p))
    (is= "5a9fe6ea2a084aa07486b8c24a037b98743b3b1e065dfdbf5f11b5ff0e22fd23"
         (hex (test-proposal-payload p)))
    (is-equalp bytes (cw-encode 'test-proposal p))
    (signals cw-error (cw-decode 'test-proposal (octets-concat bytes (octets-of 0))))
    (is= (length bytes) (nth-value 1 (cw-decode 'test-proposal (octets-concat bytes (octets-of 0))
                                                :partial t)))))

(deftest cw-defcodec-round-trip ()
  (let* ((v (make-test-everything :a 7 :b 513 :c 1 :d t :e 300
                                  :f (octets-of 1 2 3) :g '(1 2)
                                  :h (list (octets-of 0 1) (octets-of 0 2) (octets-of 1 0))
                                  :i '((1 . t) (5 . nil)) :j (make-test-round :epoch 1 :view 2)
                                  :k nil :l (list 9 (octets-of 4))))
         (bytes (cw-encode 'test-everything v)))
    (is= "070201000000000000000101ac02030102030200000001000000020300010002010002010502010001010200090104"
         (hex bytes))
    (is-equalp v (cw-decode 'test-everything bytes))
    ;; a :max bound, an unsorted set, a bad bool
    (signals cw-error (cw-decode 'test-everything
                                 (hex->octets "070201000000000000000101ac02030102030500")))
    (signals cw-error (cw-decode 'test-everything (hex->octets "070201000000000000000102")))))

(defcodec test-set-holder (s (:set :u8)))

(deftest cw-defcwunion ()
  (let ((p (make-test-proposal :round (make-test-round :epoch 1 :view 2) :parent 1
                               :payload (make-octets 32))))
    (is= (concatenate 'string "00010201" (make-string 64 :initial-element #\0))
         (hex (cw-encode 'test-vote p)))
    (is-equalp p (cw-decode 'test-vote (cw-encode 'test-vote p))))
  (let ((n (cons :nullify (make-test-round :epoch 3 :view 4))))
    (is= "010304" (hex (cw-encode 'test-vote n)))
    (is-equalp n (cw-decode 'test-vote (hex->octets "010304"))))
  (is= :empty (cw-decode 'test-vote (hex->octets "02")))
  (signals cw-error (cw-decode 'test-vote (hex->octets "03")))
  (signals cw-error (cw-decode 'test-set-holder (hex->octets "020101"))))
