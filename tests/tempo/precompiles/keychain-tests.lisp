(defpackage #:clempo.keychain.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles
        #:clempo.keychain)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))

(in-package #:clempo.keychain.tests)

;;; The oracle corpus covers the ABI surface; these exercise the executor entry points.

(defmacro with-keychain ((fork &key (timestamp 1000)) &body body)
  `(with-spec (,fork)
     (with-storage ((make-hashmap-storage :block (make-block-info :timestamp ,timestamp)))
       ,@body)))

(defun reverts-with (name thunk)
  (handler-case (progn (funcall thunk) nil)
    (precompile-revert (c) (eq (precompile-revert-name c) name))))

(deftest keychain-authorize-and-spend ()
  (with-keychain (:t7)
    (let ((account #xa11ce) (key #xbeef) (token (tip20-address 1)))
      (set-tx-origin account)
      (authorize-key account key 0 5000 t (list (list token 100 0)) t '() nil)
      (let ((k (validate-keychain-authorization account key 1000 0)))
        (is= 5000 (authorized-key-expiry k))
        (is (authorized-key-enforce-limits k))
        (is= 0 (authorized-key-signature-type k)))
      (is (reverts-with 'clempo.keychain::signature-type-mismatch
                        (lambda () (validate-keychain-authorization account key 1000 1))))
      (is (reverts-with 'clempo.keychain::key-expired
                        (lambda () (validate-keychain-authorization account key 5000 nil))))
      (set-transaction-key key)
      (authorize-transfer account token 60)
      (is (reverts-with 'clempo.keychain::spending-limit-exceeded
                        (lambda () (authorize-transfer account token 41))))
      (refund-spending-limit account token 10)
      (authorize-transfer account token 50)
      (authorize-approve account token 100 90)
      ;; a caller other than tx.origin is not limited
      (authorize-transfer #x1234 token 10000))))

(deftest keychain-call-scopes ()
  (with-keychain (:t11)
    (let ((account #xa11ce) (key #xbeef) (target #xc0ffee))
      (set-tx-origin account)
      (authorize-key account key 0 5000 nil '() nil
                     (list (list target (list (list #x12345678 '())))) nil)
      (validate-call-scope account key target (octets-of #x12 #x34 #x56 #x78))
      (is (reverts-with 'clempo.keychain::call-not-allowed
                        (lambda () (validate-call-scope account key target (octets-of 1 2 3 4)))))
      (is (reverts-with 'clempo.keychain::call-not-allowed
                        (lambda () (validate-call-scope account key #xdead (octets-of 1 2 3 4)))))
      (is (reverts-with 'clempo.keychain::call-not-allowed
                        (lambda () (validate-call-scope account key nil (make-octets 0)))))
      ;; the root key is never scoped
      (validate-call-scope account 0 #xdead (make-octets 0)))))

(deftest signature-verifier-secp256k1 ()
  (with-keychain (:t11)
    (let* ((private 12345)
           (hash (clempo.keccak:keccak256-integer (string->octets "clempo")))
           (signer (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public private))))
      (multiple-value-bind (r s recid) (clempo.secp256k1:sign (integer->octets hash 32) private)
        (let ((sig (octets-concat (integer->octets r 32) (integer->octets s 32)
                                  (octets-of (+ 27 recid)))))
          (is= signer (clempo.signature-verifier:recover hash sig))
          (is (clempo.signature-verifier:verify signer hash sig))
          (is (reverts-with 'clempo.signature-verifier::invalid-format
                            (lambda () (clempo.signature-verifier:recover hash (octets-of 9 9 9))))))))))

(deftest configurable-account-config-rules ()
  (flet ((err (threshold owners &optional (version 0))
           (clempo.configurable-account::config-error
            (clempo.configurable-account::make-config 0 version threshold owners))))
    (is (null (err 1 '((1 1) (2 1)))))
    (is (eq :empty-owners (err 1 '())))
    (is (eq :zero-threshold (err 0 '((1 1)))))
    (is (eq :duplicate-owner (err 1 '((2 1) (1 1) (2 1)))))
    (is (eq :owners-not-ascending (err 1 '((2 1) (1 1)))))
    (is (eq :threshold-exceeds-weight (err 3 '((1 1) (2 1)))))))
