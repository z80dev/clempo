(defpackage #:clempo.tip20.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:import-from #:clempo.contract.layout-tests #:layout-fixture-mismatches))

(in-package #:clempo.tip20.tests)

(deftest tip20-layout ()
  (let ((errors (layout-fixture-mismatches "tip20" 'clempo.tip20:tip20-token)))
    (is (null errors) "~{~A~^~%~}" errors)))

(deftest tip20-factory-layout ()
  (let ((errors (layout-fixture-mismatches "tip20_factory" 'clempo.tip20-factory:tip20-factory)))
    (is (null errors) "~{~A~^~%~}" errors)))

(deftest tip20-slots ()
  (is= 9 clempo.tip20::+tip20-token/balances+)
  (is= 7 (place-slot (clempo.tip20::token-transfer-policy-id 1)))
  (is= 20 (place-offset (clempo.tip20::token-transfer-policy-id 1)))
  ;; paused shares no slot: a full-slot write
  (is (null (place-offset (clempo.tip20::token-paused 1)))))

(deftest tip20-role-constants ()
  (is= (clempo.keccak:keccak256-integer (clempo.abi:utf8-encode "ISSUER_ROLE"))
       clempo.tip20:+issuer-role+)
  (is= (1- (ash 1 256)) clempo.tip20:+ungrantable-role+))

(deftest tip20-logo-uri-schemes ()
  (flet ((ok (uri) (handler-case (progn (clempo.tip20:validate-logo-uri uri) t)
                     (precompile-revert (c) (precompile-revert-name c)))))
    (is (eq t (ok "")))
    (is (eq t (ok "HTTPS://x")))
    (is (eq t (ok "ipfs:Qm")))
    (is (eq 'clempo.tip20::invalid-logo-uri (ok "javascript:alert(1)")))
    (is (eq 'clempo.tip20::invalid-logo-uri (ok "no-scheme")))
    (is (eq 'clempo.tip20::invalid-logo-uri (ok ":x")))
    (is (eq 'clempo.tip20::logo-uri-too-long (ok (format nil "https:~A" (make-string 251 :initial-element #\a)))))))

(deftest virtual-addresses ()
  (let ((v #x12345678fdfdfdfdfdfdfdfdfdfdaabbccddeeff))
    (is (clempo.address-registry:virtual-address-p v))
    (is (not (clempo.address-registry:virtual-address-p #x1111111111111111111111111111111111111111)))
    (is (not (clempo.address-registry:valid-master-p v)))
    (is (not (clempo.address-registry:valid-master-p (tip20-address 7))))
    (is (clempo.address-registry:valid-master-p #x1111111111111111111111111111111111111111))))

(deftest tip20-precompile-registration ()
  (with-spec (:t0)
    (is (eq 'clempo.tip20:tip20-token (precompile-contract (tip20-address 1))))
    (is (eq 'clempo.tip20-factory:tip20-factory (precompile-contract +tip20-factory-address+)))
    (is (null (precompile-contract +address-registry-address+))))
  (with-spec (:t3)
    (is (eq 'clempo.address-registry:address-registry
            (precompile-contract +address-registry-address+)))))
