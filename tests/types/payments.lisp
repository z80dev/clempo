(in-package #:clempo.transactions.tests)

(defun payment-test-input (signature words &optional (extra 0))
  (let ((out (make-octets (+ 4 (* words 32) extra))))
    (replace out (clempo.keccak:keccak256 (string->octets signature)) :end1 4)
    out))

(defun payment-test-tx (type to input &key access auth keyauth calls)
  (let* ((destination (if to (integer->octets to 20) (make-octets 0)))
         (fields
           (ecase type
             (0 (list 0 1 100000 destination 0 input 27 1 1))
             (1 (list 1 0 1 100000 destination 0 input access 0 1 1))
             (2 (list 1 0 1 1 100000 destination 0 input access 0 1 1))
             (4 (list 1 0 1 1 100000 destination 0 input access auth 0 1 1))
             (#x76 (append (list 1 1 1 100000
                                 (or calls (list (list destination 0 input))) access 0 0 0 0
                                 (make-octets 0) (make-octets 0) auth)
                           (when keyauth (list keyauth)) (list (make-octets 65)))))))
    (decode-transaction (clempo.transactions::encode-envelope type fields))))

(defun payment-test-word (data index value)
  (replace data (integer->octets value 32) :start1 (+ 4 (* 32 index)))
  data)

(defun payment-test-dynamic (kind signature &key (gap 0) (trailing 0) unpadded)
  (let* ((close (eq kind :close)) (words (if close 10 9))
         (size (length signature))
         (out (payment-test-input
               (if close
                   "close((address,address,address,address,bytes32,address,bytes32),uint96,uint96,bytes)"
                   "settle((address,address,address,address,bytes32,address,bytes32),uint96,bytes)")
               words (+ gap 32 (if unpadded size (* 32 (ceiling size 32))) trailing))))
    (payment-test-word out (1- words) (+ (* words 32) gap))
    (replace out (integer->octets size 32) :start1 (+ 4 (* words 32) gap))
    (replace out signature :start1 (+ 36 (* words 32) gap))
    out))

(deftest payment-static-selector-and-size-boundaries ()
  ;; Allow-list and sizes recorded from the Rust contract interfaces, not the
  ;; implementation's selector tables. Dirty arguments are intentionally accepted.
  (dolist (surface
           '((#x20c0000000000000000000000000000000000001
              ("transfer(address,uint256)" 2) ("transferWithMemo(address,uint256,bytes32)" 3)
              ("transferFrom(address,address,uint256)" 3)
              ("transferFromWithMemo(address,address,uint256,bytes32)" 4)
              ("approve(address,uint256)" 2) ("mint(address,uint256)" 2)
              ("mintWithMemo(address,uint256,bytes32)" 3) ("burn(uint256)" 1)
              ("burnWithMemo(uint256,bytes32)" 2))
             (#x4d50500000000000000000000000000000000000
              ("open(address,address,address,uint96,bytes32,address)" 6)
              ("topUp((address,address,address,address,bytes32,address,bytes32),uint96)" 8)
              ("requestClose((address,address,address,address,bytes32,address,bytes32))" 7)
              ("withdraw((address,address,address,address,bytes32,address,bytes32))" 7))))
    (dolist (shape (rest surface))
      (destructuring-bind (name words) shape
        (let ((input (payment-test-input name words)))
          (fill input 255 :start 4)
          (dolist (type '(0 1 2 4 #x76))
            (dolist (rules '(:v2 :v3))
              (is (transaction-payment-p (payment-test-tx type (first surface) input) rules))))
          (dotimes (n (length input))
            (is (not (transaction-payment-p
                      (payment-test-tx 2 (first surface) (subseq input 0 n)) :v3))))
          (dolist (extra '(1 32 33))
            (let ((tx (payment-test-tx 2 (first surface)
                                       (octets-concat input (make-octets extra)))))
              (is (not (transaction-payment-p tx :v2)))
              (is= (<= extra 32) (not (null (transaction-payment-p tx :v3)))))))))))

(deftest payment-addresses-selectors-and-v1 ()
  (dolist (to (list nil 0 #x20c1000000000000000000000000000000000000
                   #x20c0000000000000000000010000000000000000))
    (is (not (transaction-payment-p
              (payment-test-tx 2 to (payment-test-input "transfer(address,uint256)" 2)) :v1))))
  (dolist (name '("claimRewards()" "balanceOf(address)" "permit(address,address,uint256,uint256,uint8,bytes32,bytes32)"))
    (let ((tx (payment-test-tx 2 #x20c0000000000000000000000000000000000000
                              (payment-test-input name 7))))
      (is (transaction-payment-p tx :v1))
      (is (not (transaction-payment-p tx :v2)))))
  (let ((tx (payment-test-tx #x76 #x20c0000000000000000000000000000000000000 (make-octets 0))))
    (is (transaction-payment-p tx :v1))
    (setf (nth 4 (transaction-fields tx)) nil)
    (is (transaction-payment-p tx :v1))
    (is (not (transaction-payment-p tx :v2))))
  (let ((tx (payment-test-tx 2 #x4d50500000000000000000000000000000000000
                              (payment-test-input "requestClose((address,address,address,address,bytes32,address,bytes32))" 7))))
    (is (not (transaction-payment-p tx :v1)))
    (signals error (transaction-payment-p tx :unknown))))

(deftest payment-authorization-and-multicall-limits ()
  (let* ((to #x20c0000000000000000000000000000000000000)
         (input (payment-test-input "transfer(address,uint256)" 2))
         (access (list (list (integer->octets 1 20) nil))))
    (dolist (type '(1 2 4 #x76))
      (let ((tx (payment-test-tx type to input :access access)))
        (is (transaction-payment-p tx :v1))
        (is (not (transaction-payment-p tx :v2)))))
    (dolist (type '(4 #x76))
      (let ((tx (payment-test-tx type to input)))
        (setf (nth (if (= type 4) 9 12) (transaction-fields tx)) '(authorization))
        (is (transaction-payment-p tx :v1))
        (is (not (transaction-payment-p tx :v3)))))
    (let* ((tx (payment-test-tx #x76 to input))
           (good (first (transaction-calls tx))))
      (setf (nth 4 (transaction-fields tx)) (list good good))
      (is (transaction-payment-p tx :v2))
      (setf (nth 4 (transaction-fields tx))
            (list good (list (integer->octets 1 20) (make-octets 0) input)))
      (is (not (transaction-payment-p tx :v1)))
      (is (not (transaction-payment-p tx :v2))))
    ;; Exact encoded-size boundary; classifier consumes decoded wire objects and
    ;; does not independently validate key-authorization semantics.
    (dolist (n '(1018 1019))
      (let* ((keyauth (list (make-octets n)))
             (tx (payment-test-tx #x76 to input :keyauth keyauth)))
        (is= (+ n 6) (clempo.rlp:rlp-encoded-length keyauth))
        (is (transaction-payment-p tx :v1))
        (is= (= n 1018) (not (null (transaction-payment-p tx :v2))))))))

(deftest payment-channel-voucher-encoding ()
  (let ((to #x4d50500000000000000000000000000000000000))
    (dolist (kind '(:settle :close))
      (dolist (v '(0 1 2 26 27 28 29 34 35 255))
        (let ((sig (make-octets 65)))
          (setf (aref sig 64) v)
          (is= (or (<= v 1) (= v 27) (= v 28) (>= v 35))
               (not (null (transaction-payment-p
                           (payment-test-tx 2 to (payment-test-dynamic kind sig)) :v2))))))
      (dolist (spec '((1 129 nil) (1 130 t) (1 131 nil)
                      (2 128 nil) (2 129 t) (2 130 t) (3 86 nil) (4 86 nil) (0 66 nil)))
        (destructuring-bind (tag size expected) spec
          (let ((sig (make-octets size)))
            (setf (aref sig 0) tag)
            (is= expected (not (null (transaction-payment-p
                                     (payment-test-tx 2 to (payment-test-dynamic kind sig)) :v3)))))))
      (let ((sig (make-octets 65)))
        (dolist (gap '(0 1 32))
          (let ((input (payment-test-dynamic kind sig :gap gap :trailing 7 :unpadded t)))
            (is (transaction-payment-p (payment-test-tx 2 to input) :v2))))
        (let ((input (payment-test-dynamic kind sig)))
          (dolist (index '(0 1 2 3 5 7))
            (let ((dirty (copy-octets input)))
              (setf (aref dirty (+ 4 (* 32 index))) 1)
              (is (not (transaction-payment-p (payment-test-tx 2 to dirty) :v3)))))
          (let ((bad (copy-octets input)))
            (payment-test-word bad (if (eq kind :close) 9 8) (1- (ash 1 256)))
            (is (not (transaction-payment-p (payment-test-tx 2 to bad) :v2))))
          (dolist (size '(2048 2049))
            (let ((extended (make-octets size)))
              (replace extended input)
              (is= (= size 2048) (not (null (transaction-payment-p
                                           (payment-test-tx 2 to extended) :v2)))))))))))

(defun payment-test-configurable (&key (account 2) (version 0) (threshold 1)
                                      (owners '((1 1))) (signatures (list (make-octets 65))))
  (octets-concat
   (octets-of 5)
   (clempo.rlp:rlp-encode
    (list (integer->octets account 20)
          (list (make-octets 32) version threshold
                (mapcar (lambda (owner) (list (integer->octets (first owner) 20) (second owner))) owners))
          signatures))))

(deftest payment-configurable-voucher-shape ()
  (let ((to #x4d50500000000000000000000000000000000000))
    (dolist (kind '(:settle :close))
      (let ((tx (payment-test-tx 2 to (payment-test-dynamic kind (payment-test-configurable)))))
        (is (not (transaction-payment-p tx :v2)))
        (is (transaction-payment-p tx :v3)))
      (dolist (args '((:account 0) (:threshold 0) (:threshold 2) (:owners nil)
                      (:owners ((0 1))) (:owners ((1 0))) (:owners ((1 1) (1 1)))
                      (:owners ((3 1) (1 1))) (:owners ((2 1)))
                      (:owners ((1 255) (3 1))) (:signatures nil)))
        (is (not (transaction-payment-p
                  (payment-test-tx 2 to (payment-test-dynamic kind (apply #'payment-test-configurable args))) :v3))))
      (is (transaction-payment-p
           (payment-test-tx 2 to (payment-test-dynamic kind (payment-test-configurable :version 1 :owners '((2 1))))) :v3))
      (dolist (sig (list (octets-of 5 #xc0)
                        (octets-concat (payment-test-configurable) (octets-of 0))
                        (payment-test-configurable :signatures (make-list 9 :initial-element (make-octets 65)))
                        (payment-test-configurable :owners (loop for i from 3 to 11 collect (list i 1)) :threshold 9)))
        (is (not (transaction-payment-p (payment-test-tx 2 to (payment-test-dynamic kind sig)) :v3)))))))

(deftest payment-recorded-rpc-examples ()
  ;; Existing transaction-rpc.sexp provenance is recorded in tests/types/data/README.md.
  (dolist (case '(("tempo/p256/keychain-v1" t t)
                  ("tempo/p256/keychain-v1/keyauth-webAuthn" t t)
                  ("tempo/secp256k1/keyauth-secp256k1" t nil)
                  ("tempo/secp256k1" nil nil) ("eth-0x0" nil nil)))
    (destructuring-bind (label v1 v2) case
      (let* ((fixture (find label *rpc-transactions* :key #'first :test #'string=))
             (tx (decode-transaction (hex->octets (second fixture)))))
        (is= v1 (not (null (transaction-payment-p tx :v1))))
        (is= v2 (not (null (transaction-payment-p tx :v2))))
        (is= v2 (not (null (transaction-payment-p tx :v3))))))))
