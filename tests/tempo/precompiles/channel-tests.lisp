(defpackage #:clempo.channel-reserve.tests
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete))
(in-package #:clempo.channel-reserve.tests)

(defun channel-descriptor ()
  (list #x85cdeaad769e30d6ed756577cf068f4abb829cfc
        #x1111111111111111111111110000000000000002 0 +path-usd-address+
        #xa05e334153147e75f3f416139b5109d1179cb56fef6a4ecb4c4cbc92a7c37b70 0
        #x564d0e88e670e9d4ec9a4f62a143ca49b7bc4f962760e9d990ed8886e777cb7c))

(defun channel-error-name (function)
  (handler-case (progn (funcall function) nil)
    (precompile-revert (condition) (precompile-revert-name condition))))

(deftest channel-reserve-layout-and-activation ()
  (is= 1 (type-slots 'clempo.channel-reserve::packed-channel-state))
  (loop for name in '(clempo.channel-reserve::channel-states
                     clempo.channel-reserve::channel-storage-credits
                     clempo.channel-reserve::opened-this-tx
                     clempo.channel-reserve::channel-open-context-hash)
        for slot from 0
        do (is= slot (field-slot (layout-field 'clempo.channel-reserve:channel-reserve name))))
  (with-spec (:t4) (is (not (precompile-address-p +channel-reserve-address+))))
  (with-spec (:t5) (is (precompile-address-p +channel-reserve-address+))))

(deftest channel-hashes-match-reference ()
  ;; oracle.tar.gz cases channel/{computeChannelId,getVoucherDigest,domainSeparator}/T14.
  (with-spec (:t14)
    (with-storage ((make-hashmap-storage))
      (let ((id (clempo.channel-reserve::channel-id (channel-descriptor))))
        (is= #x5ac331276cfcf7ed3836505d8701844be886c5855df9d0b9e0ff758d2449f5a7 id)
        (is= #x6465ffa0b0c1e7cfc1a894ba54d79615bc06fd7cc9cfec558f3df82189d17367
             (clempo.channel-reserve::domain-separator))
        (is= #xe7856312c0bed4d820f33ef79160c6484130ee66627668d035f68c5e4b6efd1b
             (clempo.channel-reserve::voucher-digest id 7))
        (is= 180 (gas-used))))))

(deftest channel-open-context-is-transient ()
  (with-storage ((make-hashmap-storage))
    (clempo.channel-reserve:set-channel-open-context-hash 99)
    (is= 99 (tload +channel-reserve-address+ 3))
    (is= 0 (sload +channel-reserve-address+ 3))))

(deftest channel-close-request-and-zero-top-up ()
  (with-spec (:t12)
    (with-storage ((make-hashmap-storage :block (make-block-info :timestamp 12345)))
      (let* ((descriptor (channel-descriptor))
             (id (clempo.channel-reserve::channel-id descriptor))
             (place (clempo.channel-reserve::channel-states +channel-reserve-address+ id)))
        (write place (clempo.channel-reserve::make-packed-channel-state :deposit 100))
        (clempo.channel-reserve::request-close-channel (first descriptor) descriptor)
        (is= 12345 (clempo.channel-reserve::packed-channel-state-close-requested-at (read place)))
        (is= 1 (length (backend-logs *storage*)))
        (clempo.channel-reserve::request-close-channel (first descriptor) descriptor)
        (is= 1 (length (backend-logs *storage*)))
        (clempo.channel-reserve::top-up-channel (first descriptor) descriptor 0)
        (is= 0 (clempo.channel-reserve::packed-channel-state-close-requested-at (read place)))
        (is= 3 (length (backend-logs *storage*)))
        (clempo.channel-reserve::top-up-channel (first descriptor) descriptor 0)
        (is= 3 (length (backend-logs *storage*)))))))

(deftest channel-close-request-timestamp-saturates-before-wrapping ()
  (with-spec (:t14)
    (with-storage ((make-hashmap-storage :block (make-block-info :timestamp (ash 1 80))))
      (let* ((descriptor (channel-descriptor))
             (id (clempo.channel-reserve::channel-id descriptor))
             (place (clempo.channel-reserve::channel-states +channel-reserve-address+ id)))
        (write place (clempo.channel-reserve::make-packed-channel-state :deposit 1))
        (clempo.channel-reserve::request-close-channel (first descriptor) descriptor)
        (is= #xffffffff
             (clempo.channel-reserve::packed-channel-state-close-requested-at (read place)))
        (is= 899 (octets->integer (third (first (backend-logs *storage*)))))))))

(deftest channel-checks-precede-token-delivery ()
  (with-spec (:t12)
    (with-storage ((make-hashmap-storage))
      (let* ((descriptor (channel-descriptor))
             (id (clempo.channel-reserve::channel-id descriptor))
             (place (clempo.channel-reserve::channel-states +channel-reserve-address+ id)))
        (is (eq 'clempo.channel-reserve::channel-not-found
                (channel-error-name
                 (lambda () (clempo.channel-reserve::request-close-channel 1 descriptor)))))
        (write place (clempo.channel-reserve::make-packed-channel-state :deposit 10 :settled 3))
        (is (eq 'clempo.channel-reserve::not-payer
                (channel-error-name
                 (lambda () (clempo.channel-reserve::top-up-channel 1 descriptor 0)))))
        (is (eq 'clempo.channel-reserve::not-payee-or-operator
                (channel-error-name
                 (lambda () (clempo.channel-reserve::settle-channel 1 descriptor 4 (make-octets 0))))))
        (is (eq 'clempo.channel-reserve::amount-exceeds-deposit
                (channel-error-name
                 (lambda () (clempo.channel-reserve::settle-channel
                             (second descriptor) descriptor 11 (make-octets 0))))))
        (is (eq 'clempo.channel-reserve::amount-not-increasing
                (channel-error-name
                 (lambda () (clempo.channel-reserve::settle-channel
                             (second descriptor) descriptor 3 (make-octets 0))))))
        (is (eq 'clempo.channel-reserve::capture-amount-invalid
                (channel-error-name
                 (lambda () (clempo.channel-reserve::close-channel
                             (second descriptor) descriptor 4 2 (make-octets 0))))))
        (is (eq 'clempo.channel-reserve::close-not-ready
                (channel-error-name
                 (lambda () (clempo.channel-reserve::withdraw-channel (first descriptor) descriptor)))))
        (is= 0 (length (backend-logs *storage*)))))))

(deftest channel-t14-retirement-funding-gate ()
  (let* ((signer (first (channel-descriptor)))
         (host (make-memory-host))
         (backend (make-metered-storage host :gas-limit 1000000)))
    (memory-host-put-account host signer :commitment 123)
    (with-storage (backend)
      (with-spec (:t13)
        (is (null (clempo.channel-reserve::ensure-voucher-signer-active signer)))
        (is= 0 (gas-used)))
      (with-spec (:t14)
        (is (eq 'clempo.channel-reserve::root-key-retired
                (channel-error-name
                 (lambda () (clempo.channel-reserve::ensure-voucher-signer-active signer)))))
        (is= 2600 (gas-used))))))

(deftest channel-storage-credit-delete-and-reuse ()
  (with-spec (:t7)
    (let* ((host (make-memory-host))
           (backend (make-metered-storage host :gas-limit 2000000))
           (payer #x123)
           (old (clempo.channel-reserve::channel-states +channel-reserve-address+ 1))
           (new (clempo.channel-reserve::channel-states +channel-reserve-address+ 2)))
      (memory-host-put-storage host +channel-reserve-address+ (place-slot old) (ash 17 96))
      (with-storage (backend)
        (clempo.channel-reserve::preserve-credits)
        (clempo.channel-reserve::delete-channel-state-and-credit-payer 1 payer)
        (is= 1 (read (clempo.channel-reserve::channel-storage-credits +channel-reserve-address+ payer)))
        (is= 0 (clempo.storage-credits:balance-of +channel-reserve-address+))
        (clempo.channel-reserve::write-channel-state-spending-credit
         payer 2 (clempo.channel-reserve::make-packed-channel-state :deposit 18))
        (is= 0 (read (clempo.channel-reserve::channel-storage-credits +channel-reserve-address+ payer)))
        (is= 0 (clempo.storage-credits:balance-of +channel-reserve-address+))
        (is= 18 (clempo.channel-reserve::packed-channel-state-deposit (read new)))
        (is= 1 (clempo.storage-credits:mode-of +channel-reserve-address+))
        (is= 0 (clempo.storage-credits:budget-of +channel-reserve-address+))))))

(deftest storage-credits-transient-modes ()
  (with-spec (:t7)
    (with-storage ((make-hashmap-storage))
      (let ((owner #x123))
        (is= 0 (clempo.storage-credits::mode-of owner))
        (is= 0 (clempo.storage-credits::budget-of owner))
        (tstore +storage-credits-address+ owner
                (logior (ash 19 192) (ash 17 128) (ash 1 8)))
        (clempo.storage-credits:set-mode owner 2)
        (is= (logior 2 (ash u64-max 64) (ash 19 192))
             (tload +storage-credits-address+ owner))
        (clempo.storage-credits::set-budget owner 0)
        (is= 2 (clempo.storage-credits::mode-of owner))
        (is= 0 (clempo.storage-credits::budget-of owner))
        (dolist (mode '(1 0))
          (clempo.storage-credits:set-mode owner mode)
          (is= (logior mode (ash 19 192)) (tload +storage-credits-address+ owner)))
        (is (handler-case (progn (clempo.storage-credits:set-mode owner 3) nil)
              (precompile-revert (e)
                (eq 'clempo.storage-credits::invalid-mode (precompile-revert-name e)))))
        (tstore +storage-credits-address+ owner 255)
        (is (handler-case (progn (clempo.storage-credits::budget-of owner) nil)
              (precompile-revert () t)))))))

(deftest storage-credits-callback-accounting ()
  (with-spec (:t7)
    (with-storage ((make-hashmap-storage))
      (let ((owner #x123))
        (sstore +storage-credits-address+ owner 10)
        (multiple-value-bind (value minted)
            (clempo.storage-credits:track-minted-credits
             owner (lambda () (sstore +storage-credits-address+ owner 13) :minted))
          (is (eq value :minted)) (is= 3 minted))
        (clempo.storage-credits:set-mode owner 1)
        (multiple-value-bind (value delta)
            (clempo.storage-credits:with-budget
             owner 2 (lambda ()
                       (is= 2 (clempo.storage-credits::mode-of owner))
                       (is= 2 (clempo.storage-credits::budget-of owner))
                       (sstore +storage-credits-address+ owner 11)
                       :spent))
          (is (eq value :spent)) (is= -2 delta))
        (is= 1 (clempo.storage-credits::mode-of owner))
        (is= 0 (clempo.storage-credits::budget-of owner))
        (is (handler-case
                (progn (clempo.storage-credits:with-budget
                        owner 1 (lambda () (revert 'clempo.storage-credits::invalid-mode))) nil)
              (precompile-revert () t)))
        (is= 1 (clempo.storage-credits::mode-of owner))
        (is= 0 (clempo.storage-credits::budget-of owner))
        (multiple-value-bind (value delta)
            (clempo.storage-credits:with-budget owner 0 (lambda () :untouched))
          (is (eq value :untouched)) (is= 0 delta))
        (is (handler-case
                (progn (clempo.storage-credits:track-minted-credits
                        owner (lambda () (sstore +storage-credits-address+ owner 1))) nil)
              (precompile-fatal () t)))))))

(deftest storage-credits-before-t7 ()
  ;; No backend is needed: both callbacks must bypass all reads/writes before T7.
  (with-spec (:t6)
    (let ((*storage* nil))
      (multiple-value-bind (value delta)
          (clempo.storage-credits:with-budget 1 2 (lambda () :ok))
        (is (eq value :ok)) (is= 0 delta))
      (multiple-value-bind (value delta)
          (clempo.storage-credits:track-minted-credits 1 (lambda () :ok))
        (is (eq value :ok)) (is= 0 delta)))
    (is (not (precompile-address-p +storage-credits-address+))))
  (with-spec (:t7) (is (precompile-address-p +storage-credits-address+))))

(deftest storage-credits-deltas-and-exemptions ()
  (let ((deltas (clempo.storage-credits::make-credit-deltas)) (seen nil))
    (clempo.storage-credits::credit-slots deltas 9 u64-max)
    (clempo.storage-credits::credit-slots deltas 9 1)
    (clempo.storage-credits::credit-slots deltas 2 3)
    (clempo.storage-credits::credit-slots deltas 1 0)
    (clempo.storage-credits::flush-credit-deltas deltas
      (lambda (owner count) (push (list owner count) seen)))
    (is (equal (list (list 9 u64-max) '(2 3)) seen)))
  (let* ((exemptions (clempo.storage-credits::make-credit-exemptions))
         (payer #x123) (token #x456) (balance-slot (mapping-slot payer 9)))
    (is (not (clempo.storage-credits::credit-exempt-slot-p exemptions token balance-slot)))
    (clempo.storage-credits::initialize-credit-exemptions exemptions payer token nil)
    (is (clempo.storage-credits::credit-exempt-slot-p exemptions token balance-slot))
    (is (not (clempo.storage-credits::credit-exempt-slot-p exemptions token 0)))
    (clempo.storage-credits::clear-credit-exemptions exemptions)
    (is (not (clempo.storage-credits::credit-exempt-slot-p exemptions token balance-slot)))))

(deftest storage-credits-abi ()
  (labels ((input (signature types args)
             (clempo.abi:abi-encode-call (clempo.abi:abi-selector signature) types args))
           (invoke (signature types args)
             (call-precompile +storage-credits-address+ (input signature types args) #x123)))
    (with-spec (:t14)
      (with-storage ((make-hashmap-storage))
        (sstore +storage-credits-address+ #x123 7)
        (multiple-value-bind (status bytes)
            (invoke "balanceOf(address)" '(:address) '(#x123))
          (is (eq status :success))
          (is (octets= bytes (integer->octets 7 32))))
        (is (eq :success (invoke "setMode(uint8)" '((:uint 8)) '(2))))
        (multiple-value-bind (status bytes)
            (invoke "modeOf(address)" '(:address) '(#x123))
          (is (eq status :success))
          (is (octets= bytes (integer->octets 2 32))))
        (is (eq :success (invoke "setBudget(uint64)" '((:uint 64)) '(0))))
        (multiple-value-bind (status bytes)
            (invoke "budgetOf(address)" '(:address) '(#x123))
          (is (eq status :success))
          (is (octets= bytes (integer->octets 0 32))))
        (multiple-value-bind (status bytes)
            (invoke "setMode(uint8)" '((:uint 8)) '(3))
          (is (eq status :revert))
          (is (zerop (length bytes))))))
    (dolist (fork '(:t7 :t11 :t12 :t14))
      (with-spec (fork)
        (with-storage ((make-hashmap-storage))
          (multiple-value-bind (status bytes)
              (invoke "setMode(uint8)" '((:uint 8)) '(3))
            (is (eq status :revert))
            (is (octets= bytes (if (fork< fork :t11)
                                  (input "InvalidMode()" nil nil)
                                  (make-octets 0)))))))
      (with-spec (fork)
        (with-storage ((make-hashmap-storage :static t))
          (is (eq (if (fork< fork :t12) :revert :halt)
                  (invoke "setBudget(uint64)" '((:uint 64)) '(1)))))))))
