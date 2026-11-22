(defpackage #:clempo.abi.tests
  (:use #:cl #:clempo.base #:clempo.abi))

(in-package #:clempo.abi.tests)

(defun hex (o) (octets->hex o :prefix nil))
(defun words (&rest ws) (hex->octets (apply #'concatenate 'string ws)))

(deftest abi-selectors-and-topics ()
  (is= #xa9059cbb (abi-selector "transfer(address,uint256)"))
  (is= #xa9059cbb (abi-selector "transfer(address to, uint256 amount)"))
  (is= #x8be65246 (abi-selector "f(uint256,uint32[],bytes10,bytes)"))
  (is= #x2289b18c (abi-selector "g(uint256[][],string[])"))
  (is= #xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
       (event-topic "Transfer(address,address,uint256)"))
  (is= "foo((address,uint256)[],bytes32)"
       (multiple-value-bind (n ts) (parse-signature "foo(tuple(address a, uint256 b)[] xs, bytes32 h)")
         (canonical-signature n ts))))

(defparameter *f-encoding*
  (words "0000000000000000000000000000000000000000000000000000000000000123"
         "0000000000000000000000000000000000000000000000000000000000000080"
         "3132333435363738393000000000000000000000000000000000000000000000"
         "00000000000000000000000000000000000000000000000000000000000000e0"
         "0000000000000000000000000000000000000000000000000000000000000002"
         "0000000000000000000000000000000000000000000000000000000000000456"
         "0000000000000000000000000000000000000000000000000000000000000789"
         "000000000000000000000000000000000000000000000000000000000000000d"
         "48656c6c6f2c20776f726c642100000000000000000000000000000000000000"))

(deftest abi-solidity-docs-vectors ()
  ;; The two worked examples of the Solidity ABI specification.
  (let ((types (signature-types "f(uint256,uint32[],bytes10,bytes)"))
        (vals (list #x123 '(#x456 #x789) (octets->integer (string->octets "1234567890"))
                    (string->octets "Hello, world!"))))
    (is-equalp *f-encoding* (abi-encode types vals))
    (is-equalp vals (abi-decode types *f-encoding* :strict t)))
  (let* ((types (signature-types "g(uint256[][],string[])"))
         (vals (list '((1 2) (3)) '("one" "two" "three")))
         (enc (abi-encode types vals)))
    (is= (concatenate 'string
                      "0000000000000000000000000000000000000000000000000000000000000040"
                      "0000000000000000000000000000000000000000000000000000000000000140"
                      "0000000000000000000000000000000000000000000000000000000000000002"
                      "0000000000000000000000000000000000000000000000000000000000000040"
                      "00000000000000000000000000000000000000000000000000000000000000a0"
                      "0000000000000000000000000000000000000000000000000000000000000002"
                      "0000000000000000000000000000000000000000000000000000000000000001"
                      "0000000000000000000000000000000000000000000000000000000000000002"
                      "0000000000000000000000000000000000000000000000000000000000000001"
                      "0000000000000000000000000000000000000000000000000000000000000003"
                      "0000000000000000000000000000000000000000000000000000000000000003"
                      "0000000000000000000000000000000000000000000000000000000000000060"
                      "00000000000000000000000000000000000000000000000000000000000000a0"
                      "00000000000000000000000000000000000000000000000000000000000000e0"
                      "0000000000000000000000000000000000000000000000000000000000000003"
                      "6f6e650000000000000000000000000000000000000000000000000000000000"
                      "0000000000000000000000000000000000000000000000000000000000000003"
                      "74776f0000000000000000000000000000000000000000000000000000000000"
                      "0000000000000000000000000000000000000000000000000000000000000005"
                      "7468726565000000000000000000000000000000000000000000000000000000")
         (hex enc))
    (is-equalp vals (abi-decode types enc :strict t))))

(deftest abi-loose-versus-strict ()
  (let ((dirty (words "ff00000000000000000000000000000000000000000000000000000000000001")))
    ;; loose (pre-T11): high bits dropped; strict: rejected
    (is= '(1) (abi-decode '(:address) dirty))
    (signals abi-decode-error (abi-decode '(:address) dirty :strict t))
    (is= '(1) (abi-decode '((:uint 8)) dirty))
    (signals abi-decode-error (abi-decode '((:uint 8)) dirty :strict t)))
  ;; trailing bytes: allowed loose, rejected strict unless allowed
  (let ((data (octets-concat (abi-encode '((:uint 256)) '(5)) (octets-of 0))))
    (is= '(5) (abi-decode '((:uint 256)) data))
    (signals abi-decode-error (abi-decode '((:uint 256)) data :strict t))
    (is= '(5) (abi-decode '((:uint 256)) data :strict t :allow-trailing t)))
  ;; a non-canonical offset is accepted loose and rejected strict
  (let ((data (words "0000000000000000000000000000000000000000000000000000000000000040"
                     "0000000000000000000000000000000000000000000000000000000000000000"
                     "0000000000000000000000000000000000000000000000000000000000000001"
                     "6100000000000000000000000000000000000000000000000000000000000000")))
    (is-equalp (list (octets-of #x61)) (abi-decode '(:bytes) data))
    (signals abi-decode-error (abi-decode '(:bytes) data :strict t)))
  ;; truncated
  (signals abi-decode-error (abi-decode '((:uint 256) (:uint 256)) (make-octets 32)))
  ;; enums: out of range maps to 255 loose, rejected strict
  (is= '(255) (abi-decode '((:enum 3)) (abi-encode '((:uint 8)) '(7))))
  (signals abi-decode-error (abi-decode '((:enum 3)) (abi-encode '((:uint 8)) '(7)) :strict t)))

(deftest abi-utf8 ()
  (is= "hé" (utf8-decode (octets-of #x68 #xc3 #xa9)))
  (signals abi-decode-error (utf8-decode (octets-of #x68 #xff)))
  ;; from_utf8_lossy: one U+FFFD per maximal invalid subpart
  (is= (coerce (list #\a (code-char #xfffd) #\b) 'string)
       (utf8-decode (octets-of #x61 #xe2 #x82 #x62) :lossy t))
  (is= (coerce (list (code-char #xfffd) (code-char #xfffd)) 'string)
       (utf8-decode (octets-of #xff #xfe) :lossy t)))

(deftest abi-errors-and-signed ()
  (is= "08c379a0" (hex (subseq (abi-encode-error "Error(string)" "x") 0 4)))
  (is= (concatenate 'string "4e487b71" (make-string 62 :initial-element #\0) "11")
       (hex (panic-data #x11)))
  (let ((enc (abi-encode '((:int 8)) '(-1))))
    (is= (make-string 64 :initial-element #\f) (hex enc))
    (is= '(-1) (abi-decode '((:int 8)) enc :strict t))))

(deftest abi-zero-size-element-arrays-are-bounded ()
  ;; ()[] and uint256[0][] carry no data per element, so a 64-byte payload can claim
  ;; 2^64-1 elements; alloy-sol-types 1.7 charges them to the memory limit up front and
  ;; rejects any nonzero length in strict mode. Each case once spun allocating NILs.
  (flet ((payload (len)
           (words "0000000000000000000000000000000000000000000000000000000000000020"
                  (format nil "~64,'0x" len))))
    (dolist (types '(((:array (:tuple))) ((:array (:array (:uint 256) 0)))
                     ((:array (:tuple (:tuple) (:array :bool 0))))))
      (is= 0 (abi-min-words (second (first types))))
      (let ((start (get-internal-real-time)))
        (signals abi-decode-error (abi-decode types (payload (1- (expt 2 64)))))
        (signals abi-decode-error (abi-decode types (payload (1- (expt 2 64))) :strict t))
        (signals abi-decode-error (abi-decode types (payload (1+ (expt 2 26)))))
        (is (< (- (get-internal-real-time) start) internal-time-units-per-second)))
      (is= '((nil nil nil)) (abi-decode types (payload 3)))
      (signals abi-decode-error (abi-decode types (payload 3) :strict t))
      (is= '(()) (abi-decode types (payload 0) :strict t))
      (signals abi-decode-error (abi-decode types (payload 4) :memory-limit 63))
      (is= 64 (length (first (abi-decode types (payload 64) :memory-limit 1024)))))))

(deftest abi-huge-lengths-are-overruns ()
  ;; A length word whose head words overflow a fixnum once escaped as a TYPE-ERROR from the
  ;; strict decoder's next-offset slot (found by fuzzing: int56 uint160[] bytes).
  (dolist (len (list (expt 2 60) (expt 2 62) (1- (expt 2 64))))
    (let ((data (words "0000000000000000000000000000000000000000000000000000000000000020"
                       (format nil "~64,'0x" len))))
      (dolist (strict '(nil t))
        (is= :abi-error (handler-case (progn (abi-decode '((:array (:uint 160))) data
                                                         :strict strict :validate t)
                                             :decoded)
                          (abi-decode-error () :abi-error)
                          (error (e) (type-of e)))
             "len ~X strict ~A" len strict)))))
