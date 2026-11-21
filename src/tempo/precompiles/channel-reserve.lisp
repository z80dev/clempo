(in-package #:clempo.channel-reserve)

;;; TIP-1034, crates/precompiles/src/tip20_channel_reserve. Descriptors are ABI lists;
;;; all persistent channel accounting fits in one word, including T12's sender flag.

(defconstant +close-grace-period+ 900)
(define-constant +voucher-typehash+
  (clempo.keccak:keccak256-integer
   (string->octets "Voucher(bytes32 channelId,uint96 cumulativeAmount)")))
(define-constant +domain-typehash+
  (clempo.keccak:keccak256-integer
   (string->octets "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")))
(define-constant +name-hash+
  (clempo.keccak:keccak256-integer (string->octets "TIP20 Channel Reserve")))
(define-constant +version-hash+
  (clempo.keccak:keccak256-integer (string->octets "1")))

(defstorable packed-channel-state
  (settled u96)
  (deposit u96)
  (close-requested-at u32)
  (uses-logical-receive-policy-sender bool))

(defcontract channel-reserve (:address +channel-reserve-address+)
  (channel-states (mapping b256 packed-channel-state))
  (channel-storage-credits (mapping address u64))
  (opened-this-tx (mapping b256 bool))
  (channel-open-context-hash b256))

(deferror channel-already-exists "ChannelAlreadyExists()")
(deferror channel-not-found "ChannelNotFound()")
(deferror not-payer "NotPayer()")
(deferror not-payee-or-operator "NotPayeeOrOperator()")
(deferror invalid-payee "InvalidPayee()")
(deferror zero-deposit "ZeroDeposit()")
(deferror expiring-nonce-hash-not-set "ExpiringNonceHashNotSet()")
(deferror invalid-signature "InvalidSignature()")
(deferror amount-exceeds-deposit "AmountExceedsDeposit()")
(deferror amount-not-increasing "AmountNotIncreasing()")
(deferror capture-amount-invalid "CaptureAmountInvalid()")
(deferror close-not-ready "CloseNotReady()")
(deferror deposit-overflow "DepositOverflow()")
(deferror root-key-retired "RootKeyRetired(address account)")

(defevent channel-opened "ChannelOpened(bytes32 indexed channelId, address indexed payer, address indexed payee, address operator, address token, address authorizedSigner, bytes32 salt, bytes32 expiringNonceHash, uint96 deposit)")
(defevent settled "Settled(bytes32 indexed channelId, address indexed payer, address indexed payee, uint96 cumulativeAmount, uint96 deltaPaid, uint96 newSettled)")
(defevent top-up "TopUp(bytes32 indexed channelId, address indexed payer, address indexed payee, uint96 additionalDeposit, uint96 newDeposit)")
(defevent close-requested "CloseRequested(bytes32 indexed channelId, address indexed payer, address indexed payee, uint256 closeGraceEnd)")
(defevent channel-closed "ChannelClosed(bytes32 indexed channelId, address indexed payer, address indexed payee, uint96 settledToPayee, uint96 refundedToPayer)")
(defevent close-request-cancelled "CloseRequestCancelled(bytes32 indexed channelId, address indexed payer, address indexed payee)")

(defun set-channel-open-context-hash (hash)
  "Seed the enclosing transaction's replay-protected open hash in transient storage."
  (t-write (channel-open-context-hash +channel-reserve-address+) hash))

(defun channel-id (descriptor)
  (keccak (clempo.abi:abi-encode
           '(:address :address :address :address (:bytes-n 32) :address (:bytes-n 32)
             :address (:uint 256))
           (append descriptor (list +channel-reserve-address+ (chain-id))))))

(defun domain-separator ()
  ;; The reference computes this outside runtime metering, even on nonstandard chains.
  (clempo.keccak:keccak256-integer
   (clempo.abi:abi-encode
    '((:bytes-n 32) (:bytes-n 32) (:bytes-n 32) (:uint 256) :address)
    (list +domain-typehash+ +name-hash+ +version-hash+ (chain-id) +channel-reserve-address+))))

(defun voucher-digest (id cumulative)
  (let* ((struct-hash (keccak (clempo.abi:abi-encode
                             '((:bytes-n 32) (:bytes-n 32) (:uint 96))
                             (list +voucher-typehash+ id cumulative))))
         (input (make-octets 66)))
    (setf (aref input 0) #x19 (aref input 1) #x01)
    (replace input (integer->octets (domain-separator) 32) :start1 2)
    (replace input (integer->octets struct-hash 32) :start1 34)
    (keccak input)))

(defun state-to-sol (state)
  (list (packed-channel-state-settled state) (packed-channel-state-deposit state)
        (packed-channel-state-close-requested-at state)))

(defun load-existing-state (id)
  (let ((state (read (channel-states +channel-reserve-address+ id))))
    (when (zerop (packed-channel-state-deposit state)) (revert 'channel-not-found))
    state))

(defun expected-signer (descriptor)
  (let ((signer (sixth descriptor)))
    (if (zerop signer) (first descriptor) signer)))

(defun ensure-voucher-signer-active (signer)
  (fork-if (:since :t14)
    (when (/= 0 (config-commitment signer)) (revert 'root-key-retired signer))))

(defun validate-voucher (descriptor id cumulative signature)
  (let* ((digest (voucher-digest id cumulative))
         ;; Before T15 every recover error, including OOG, becomes InvalidSignature.
         (signer (handler-case (funcall 'clempo.signature-verifier:recover digest signature)
                   (precompile-condition () (revert 'invalid-signature)))))
    (unless (= signer (expected-signer descriptor)) (revert 'invalid-signature))))

(defun ensure-payee-or-operator (sender descriptor)
  (unless (or (= sender (second descriptor))
              (and (/= 0 (third descriptor)) (= sender (third descriptor))))
    (revert 'not-payee-or-operator)))

(defun receive-policy-sender (state payer)
  (if (packed-channel-state-uses-logical-receive-policy-sender state)
      payer +channel-reserve-address+))

(defun preserve-credits ()
  (fork-if (:since :t7)
    (clempo.storage-credits:set-mode +channel-reserve-address+ 1)))

(defun credit-channel-storage-slots (payer slots)
  (unless (zerop slots)
    (let* ((place (channel-storage-credits +channel-reserve-address+ payer))
           (current (read place))
           (updated (min u64-max (+ current slots))))
      (if (zerop current)
          (multiple-value-bind (result delta)
              (clempo.storage-credits:with-budget
               +channel-reserve-address+ 1 (lambda () (write place updated)))
            (declare (ignore result))
            (unless (= delta -1)
              (fatal "channel storage credit bookkeeping spend mismatch: reserved 1, delta ~D" delta)))
          (write place updated)))))

(defun delete-channel-state-and-credit-payer (id payer)
  (multiple-value-bind (result credits)
      (clempo.storage-credits:track-minted-credits
       +channel-reserve-address+
       (lambda () (delete (channel-states +channel-reserve-address+ id))))
    (declare (ignore result))
    (credit-channel-storage-slots payer credits)))

(defun write-channel-state-spending-credit (payer id state)
  (let ((place (channel-states +channel-reserve-address+ id)))
    (fork-if (:before :t7)
      (write place state)
      (let* ((credit-place (channel-storage-credits +channel-reserve-address+ payer))
             (current (read credit-place)))
        (if (zerop current)
            (write place state)
            (progn
              (delete credit-place)
              (multiple-value-bind (result delta)
                  (clempo.storage-credits:with-budget
                   +channel-reserve-address+ current (lambda () (write place state)))
                (declare (ignore result))
                (let ((spent (max 0 (- delta))))
                  (unless (= spent 1)
                    (fatal "channel storage credit spend mismatch: reserved 1, spent ~D" spent))
                  (credit-channel-storage-slots payer (max 0 (- current spent)))))))))))

(defun open-channel (sender payee operator token deposit salt authorized-signer)
  (declare (notinline clempo.tip20:recipient-target))
  (when (or (zerop payee) (tip20-address-p payee)
            (and (= (ldb (byte 80 48) payee) #xfdfdfdfdfdfdfdfdfdfd)
                 (or (zerop operator)
                     (= (ldb (byte 80 48) operator) #xfdfdfdfdfdfdfdfdfdfd))))
    (revert 'invalid-payee))
  (funcall 'clempo.tip20:from-address token)
  (when (zerop deposit) (revert 'zero-deposit))
  (ensure-voucher-signer-active (if (zerop authorized-signer) sender authorized-signer))
  (let ((context (t-read (channel-open-context-hash +channel-reserve-address+))))
    (when (zerop context) (revert 'expiring-nonce-hash-not-set))
    (let ((id (channel-id (list sender payee operator token salt authorized-signer context))))
      (when (or (plusp (packed-channel-state-deposit
                       (read (channel-states +channel-reserve-address+ id))))
                (t-read (opened-this-tx +channel-reserve-address+ id)))
        (revert 'channel-already-exists))
      (fork-if (:since :t12)
        (let ((target (funcall 'clempo.tip20:recipient-target (funcall 'clempo.tip20:recipient-resolve payee))))
          (funcall 'clempo.tip20:ensure-transfer-authorized token sender target)
          (funcall 'clempo.tip20:ensure-receive-policy-authorized token sender target)
          (funcall 'clempo.tip20:channel-reserve-transfer
           token sender (funcall 'clempo.tip20:recipient-direct +channel-reserve-address+) deposit sender))
        (progn
          (funcall 'clempo.tip20:ensure-authorized-as
           token (list (cons (funcall 'clempo.tip20:recipient-target (funcall 'clempo.tip20:recipient-resolve payee))
                             :recipient)))
          (funcall 'clempo.tip20:system-transfer-from token +channel-reserve-address+ sender deposit)))
      (write-channel-state-spending-credit
       sender id (make-packed-channel-state
                  :deposit deposit :uses-logical-receive-policy-sender
                  (fork-if (:since :t12) t nil)))
      (t-write (opened-this-tx +channel-reserve-address+ id) t)
      (emit +channel-reserve-address+ 'channel-opened id sender payee operator token
            authorized-signer salt context deposit)
      id)))

(defun settle-channel (sender descriptor cumulative signature)
  (declare (notinline clempo.tip20:recipient-target))
  (let* ((id (channel-id descriptor))
         (state (load-existing-state id)))
    (ensure-payee-or-operator sender descriptor)
    (when (> cumulative (packed-channel-state-deposit state)) (revert 'amount-exceeds-deposit))
    (when (<= cumulative (packed-channel-state-settled state)) (revert 'amount-not-increasing))
    (validate-voucher descriptor id cumulative signature)
    (let ((delta (- cumulative (packed-channel-state-settled state)))
          (token (funcall 'clempo.tip20:from-address (fourth descriptor)))
          (payer (first descriptor)) (payee (second descriptor)))
      (fork-if (:since :t12)
        (let ((recipient (funcall 'clempo.tip20:recipient-resolve payee)))
          (funcall 'clempo.tip20:ensure-transfer-authorized token payer (funcall 'clempo.tip20:recipient-target recipient))
          (funcall 'clempo.tip20:channel-reserve-transfer
           token +channel-reserve-address+ recipient delta (receive-policy-sender state payer))
          (setf (packed-channel-state-settled state) cumulative)
          (write (channel-states +channel-reserve-address+ id) state))
        (progn
          (funcall 'clempo.tip20:ensure-authorized-as token (list (cons payer :sender)))
          ;; Legacy settlement writes before the fallible delivery.
          (setf (packed-channel-state-settled state) cumulative)
          (write (channel-states +channel-reserve-address+ id) state)
          (funcall 'clempo.tip20:transfer token +channel-reserve-address+ payee delta)))
      (emit +channel-reserve-address+ 'settled id payer payee cumulative delta cumulative))))

(defun top-up-channel (sender descriptor additional)
  (declare (notinline clempo.tip20:recipient-target))
  (let* ((id (channel-id descriptor)) (state (load-existing-state id))
         (had-close (/= 0 (packed-channel-state-close-requested-at state)))
         (payer (first descriptor)) (payee (second descriptor)))
    (unless (= sender payer) (revert 'not-payer))
    (when (and (zerop additional) (not had-close)) (return-from top-up-channel nil))
    (unless (zerop additional)
      (ensure-voucher-signer-active (expected-signer descriptor))
      (let ((next (+ (packed-channel-state-deposit state) additional)))
        (when (>= next (ash 1 96)) (revert 'deposit-overflow))
        (setf (packed-channel-state-deposit state) next))
      (let ((token (funcall 'clempo.tip20:from-address (fourth descriptor))))
        (fork-if (:since :t12)
          (let ((target (funcall 'clempo.tip20:recipient-target (funcall 'clempo.tip20:recipient-resolve payee))))
            (funcall 'clempo.tip20:ensure-transfer-authorized token sender target)
            (funcall 'clempo.tip20:ensure-receive-policy-authorized token (receive-policy-sender state sender) target)
            (funcall 'clempo.tip20:channel-reserve-transfer
             token sender (funcall 'clempo.tip20:recipient-direct +channel-reserve-address+) additional sender))
          (progn
            (funcall 'clempo.tip20:ensure-authorized-as
             token (list (cons (funcall 'clempo.tip20:recipient-target (funcall 'clempo.tip20:recipient-resolve payee))
                               :recipient)))
            (funcall 'clempo.tip20:system-transfer-from token +channel-reserve-address+ sender additional)))))
    (when had-close (setf (packed-channel-state-close-requested-at state) 0))
    (write (channel-states +channel-reserve-address+ id) state)
    (when had-close (emit +channel-reserve-address+ 'close-request-cancelled id payer payee))
    (emit +channel-reserve-address+ 'top-up id payer payee additional (packed-channel-state-deposit state))))

(defun request-close-channel (sender descriptor)
  (let* ((id (channel-id descriptor)) (state (load-existing-state id)))
    (unless (= sender (first descriptor)) (revert 'not-payer))
    (when (plusp (packed-channel-state-close-requested-at state))
      (return-from request-close-channel nil))
    (setf (packed-channel-state-close-requested-at state) (min #xffffffff (block-timestamp)))
    (write (channel-states +channel-reserve-address+ id) state)
    (emit +channel-reserve-address+ 'close-requested id (first descriptor) (second descriptor)
          (ldb (byte 64 0) (+ (min u64-max (block-timestamp)) +close-grace-period+)))))

(defun close-channel (sender descriptor cumulative capture signature)
  (declare (notinline clempo.tip20:recipient-target))
  (let* ((id (channel-id descriptor)) (state (load-existing-state id))
         (previous (packed-channel-state-settled state))
         (payer (first descriptor)) (payee (second descriptor)))
    (ensure-payee-or-operator sender descriptor)
    (when (or (< capture previous) (> capture cumulative)) (revert 'capture-amount-invalid))
    (when (> capture (packed-channel-state-deposit state)) (revert 'amount-exceeds-deposit))
    (when (> capture previous) (validate-voucher descriptor id cumulative signature))
    (let ((delta (- capture previous)) (refund (- (packed-channel-state-deposit state) capture)))
      (fork-if (:since :t12)
        (let ((token (funcall 'clempo.tip20:from-address (fourth descriptor))))
          (unless (zerop delta)
            (let ((recipient (funcall 'clempo.tip20:recipient-resolve payee)))
              (funcall 'clempo.tip20:ensure-transfer-authorized token payer (funcall 'clempo.tip20:recipient-target recipient))
              (funcall 'clempo.tip20:channel-reserve-transfer
               token +channel-reserve-address+ recipient delta (receive-policy-sender state payer))))
          (unless (zerop refund)
            (funcall 'clempo.tip20:channel-reserve-transfer
             token +channel-reserve-address+ (funcall 'clempo.tip20:recipient-resolve payer) refund
             +channel-reserve-address+))
          (delete-channel-state-and-credit-payer id payer))
        (progn
          (delete-channel-state-and-credit-payer id payer)
          (let ((token (funcall 'clempo.tip20:from-address (fourth descriptor))))
            (unless (zerop delta)
              (funcall 'clempo.tip20:ensure-authorized-as token (list (cons payer :sender)))
              (funcall 'clempo.tip20:transfer token +channel-reserve-address+ payee delta))
            (unless (zerop refund)
              (funcall 'clempo.tip20:transfer token +channel-reserve-address+ payer refund)))))
      (emit +channel-reserve-address+ 'channel-closed id payer payee capture refund))))

(defun withdraw-channel (sender descriptor)
  (let* ((id (channel-id descriptor)) (state (load-existing-state id))
         (payer (first descriptor)) (requested (packed-channel-state-close-requested-at state)))
    (unless (= sender payer) (revert 'not-payer))
    (unless (and (plusp requested) (>= (min u64-max (block-timestamp)) (+ requested +close-grace-period+)))
      (revert 'close-not-ready))
    (let ((refund (checked- (packed-channel-state-deposit state) (packed-channel-state-settled state))))
      (fork-if (:since :t12)
        (progn
          (unless (zerop refund)
            (funcall 'clempo.tip20:channel-reserve-transfer
             (funcall 'clempo.tip20:from-address (fourth descriptor)) +channel-reserve-address+
             (funcall 'clempo.tip20:recipient-resolve payer) refund +channel-reserve-address+))
          (delete-channel-state-and-credit-payer id payer))
        (progn
          (delete-channel-state-and-credit-payer id payer)
          (unless (zerop refund)
            (funcall 'clempo.tip20:transfer (funcall 'clempo.tip20:from-address (fourth descriptor))
                                   +channel-reserve-address+ payer refund))))
      (emit +channel-reserve-address+ 'channel-closed id payer (second descriptor)
            (packed-channel-state-settled state) refund))))

(defcall channel-reserve grace-period ()
    (:sol "CLOSE_GRACE_PERIOD()" :returns "uint64") +close-grace-period+)
(defcall channel-reserve voucher-typehash ()
    (:sol "VOUCHER_TYPEHASH()" :returns "bytes32") +voucher-typehash+)
(defcall channel-reserve open (payee operator token deposit salt signer)
    (:sol "open(address,address,address,uint96,bytes32,address)" :returns "bytes32" :mutates t)
  (preserve-credits)
  (open-channel sender payee operator token deposit salt signer))
(defcall channel-reserve settle (descriptor cumulative signature)
    (:sol "settle((address,address,address,address,bytes32,address,bytes32),uint96,bytes)" :mutates t)
  (preserve-credits)
  (settle-channel sender descriptor cumulative signature))
(defcall channel-reserve top-up (descriptor additional)
    (:sol "topUp((address,address,address,address,bytes32,address,bytes32),uint96)" :mutates t)
  (preserve-credits)
  (top-up-channel sender descriptor additional))
(defcall channel-reserve request-close (descriptor)
    (:sol "requestClose((address,address,address,address,bytes32,address,bytes32))" :mutates t)
  (preserve-credits)
  (request-close-channel sender descriptor))
(defcall channel-reserve close (descriptor cumulative capture signature)
    (:sol "close((address,address,address,address,bytes32,address,bytes32),uint96,uint96,bytes)" :mutates t)
  (preserve-credits)
  (close-channel sender descriptor cumulative capture signature))
(defcall channel-reserve withdraw (descriptor)
    (:sol "withdraw((address,address,address,address,bytes32,address,bytes32))" :mutates t)
  (preserve-credits)
  (withdraw-channel sender descriptor))
(defcall channel-reserve get-channel (descriptor)
    (:sol "getChannel((address,address,address,address,bytes32,address,bytes32))"
     :returns "((address,address,address,address,bytes32,address,bytes32),(uint96,uint96,uint32))")
  (let ((id (channel-id descriptor)))
    (list descriptor (state-to-sol (read (channel-states self id))))))
(defcall channel-reserve get-channel-state (id)
    (:sol "getChannelState(bytes32)" :returns "(uint96,uint96,uint32)")
  (state-to-sol (read (channel-states self id))))
(defcall channel-reserve get-channel-states-batch (ids)
    (:sol "getChannelStatesBatch(bytes32[])" :returns "(uint96,uint96,uint32)[]")
  (mapcar (lambda (id) (state-to-sol (read (channel-states self id)))) ids))
(defcall channel-reserve compute-channel-id (payer payee operator token salt signer context)
    (:sol "computeChannelId(address,address,address,address,bytes32,address,bytes32)" :returns "bytes32")
  (channel-id (list payer payee operator token salt signer context)))
(defcall channel-reserve get-voucher-digest (id cumulative)
    (:sol "getVoucherDigest(bytes32,uint96)" :returns "bytes32")
  (voucher-digest id cumulative))
(defcall channel-reserve domain-separator ()
    (:sol "domainSeparator()" :returns "bytes32")
  (domain-separator))
(defcall channel-reserve storage-credits (payer)
    (:sol "storageCredits(address)" :returns "uint64" :since :t7)
  (read (channel-storage-credits self payer)))

(defdispatch channel-reserve (self input sender)
  (charge-input-cost input)
  (dispatch-calls))
(register-precompile +channel-reserve-address+ 'channel-reserve :t5)
