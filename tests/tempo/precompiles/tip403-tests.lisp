(defpackage #:clempo.tip403.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))

(in-package #:clempo.tip403.tests)

(deftest tip403-storage-layout ()
  (let ((errors (clempo.contract.layout-tests:layout-fixture-mismatches
                 "tip403_registry" 'clempo.tip403:tip403-registry)))
    (is (null errors) "~{~A~^~%~}" errors)))

(deftest policy-precompile-activation ()
  (is (eq 'clempo.tip403:tip403-registry
          (precompile-contract +tip403-registry-address+ :genesis)))
  (is (null (precompile-contract +receive-policy-guard-address+ :t5)))
  (is (eq 'clempo.receive-policy-guard:receive-policy-guard
          (precompile-contract +receive-policy-guard-address+ :t6))))

(deftest builtin-policy-authorization ()
  (with-storage ((make-hashmap-storage))
    (with-spec (:t5)
      (is (clempo.tip403:policy-exists-p 0))
      (is (clempo.tip403:policy-exists-p 1))
      (is (not (clempo.tip403:authorized-p 0 123)))
      (is (clempo.tip403:authorized-p 1 123))
      (is (not (clempo.tip403:authorized-p 0 +receive-policy-guard-address+))))
    (with-spec (:t6)
      (is (clempo.tip403:authorized-p 0 +receive-policy-guard-address+)))))

(deftest blacklist-policy-lifecycle ()
  (with-storage ((make-hashmap-storage))
    (with-spec (:t14)
      (let ((id (clempo.tip403:create-policy 123 1 123)))
        (is= 2 id)
        (is (clempo.tip403:policy-exists-p id))
        (is (not (clempo.tip403:policy-exists-p (1+ id))))
        (is (clempo.tip403:authorized-p id 456))
        (clempo.tip403:modify-policy-blacklist 123 id 456 t)
        (is (not (clempo.tip403:authorized-p id 456)))
        (is (clempo.tip403:authorized-p id 789))
        (clempo.tip403:modify-policy-blacklist 123 id 456 nil)
        (is (clempo.tip403:authorized-p id 456))))))

(deftest registry-token-policy-zero-is-set ()
  (with-storage ((make-hashmap-storage))
    (with-spec (:t14)
      (is (null (clempo.tip403:registered-token-transfer-policy-id +path-usd-address+)))
      (clempo.tip403:set-token-transfer-policy +path-usd-address+ 0)
      (is= 0 (clempo.tip403:registered-token-transfer-policy-id +path-usd-address+)))))

(deftest guard-storage-layout ()
  (is= '((0 0) (1 0))
       (mapcar (lambda (f) (list (field-slot f) (field-offset f)))
               (layout-fields 'clempo.receive-policy-guard:receive-policy-guard))))

(deftest guard-receipt-roundtrip ()
  (with-storage ((make-hashmap-storage))
    (with-spec (:t6)
      (multiple-value-bind (nonce at)
          (clempo.receive-policy-guard:store-blocked
           +path-usd-address+ 123 456 456 0 789 1 0 0)
        (is= 1 nonce)
        (let ((receipt (clempo.receive-policy-guard::encode-receipt
                        (clempo.receive-policy-guard::make-receipt
                         +path-usd-address+ 0 123 456 at nonce 1 0 0))))
          (is= 789 (clempo.receive-policy-guard:balance-of receipt))
          (setf (aref receipt 319) 1)
          (is= 0 (clempo.receive-policy-guard:balance-of receipt)))))))
