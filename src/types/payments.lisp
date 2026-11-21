(in-package #:clempo.transactions)

(export 'transaction-payment-p)

;; TIP-1045, crates/contracts/src/precompiles/{tip20,tip20_channel_reserve}.rs.
;; Static calls deliberately validate selector and size, not argument values.
(defparameter *payment-token-shapes*
  (loop for (signature words) in
        '(("transfer(address,uint256)" 2) ("transferWithMemo(address,uint256,bytes32)" 3)
          ("transferFrom(address,address,uint256)" 3)
          ("transferFromWithMemo(address,address,uint256,bytes32)" 4)
          ("approve(address,uint256)" 2) ("mint(address,uint256)" 2)
          ("mintWithMemo(address,uint256,bytes32)" 3) ("burn(uint256)" 1)
          ("burnWithMemo(uint256,bytes32)" 2))
        collect (cons (octets->integer (clempo.keccak:keccak256 (string->octets signature)) :end 4)
                      words)))

(defparameter *payment-channel-shapes*
  (loop for (signature shape) in
        '(("open(address,address,address,uint96,bytes32,address)" 6)
          ("topUp((address,address,address,address,bytes32,address,bytes32),uint96)" 8)
          ("requestClose((address,address,address,address,bytes32,address,bytes32))" 7)
          ("withdraw((address,address,address,address,bytes32,address,bytes32))" 7)
          ("settle((address,address,address,address,bytes32,address,bytes32),uint96,bytes)" :settle)
          ("close((address,address,address,address,bytes32,address,bytes32),uint96,uint96,bytes)" :close))
        collect (cons (octets->integer (clempo.keccak:keccak256 (string->octets signature)) :end 4)
                      shape)))

(defun payment-token-address-p (address)
  (and address (= (ash address -64) #x20c000000000000000000000)))

(defun payment-primitive-signature-p (data start end)
  ;; PrimitiveSignature::from_bytes checks encoding, never ECDSA validity or low-s.
  (let ((size (- end start)))
    (cond ((= size 65)
           (let ((v (aref data (+ start 64))))
             (or (<= v 1) (= v 27) (= v 28) (>= v 35))))
          ((< size 2) nil)
          (t (case (aref data start)
               (1 (= size 130))
               (2 (<= 129 size 2049))
               (otherwise nil))))))

(defun payment-configurable-signature-p (data start end)
  ;; ConfigurableAccountSignature::decode validates configuration shape, not quorum.
  (handler-case
      (let ((wire (clempo.rlp:rlp-decode data :start start :end end)))
        (labels ((fixed (x n) (and (typep x 'octets) (= (length x) n)))
                 (uint (x n) (clempo.rlp:rlp-decode-integer x :max-bytes n)))
          (and (listp wire) (= (length wire) 3)
               (destructuring-bind (account config signatures) wire
                 (and (fixed account 20) (plusp (octets->integer account))
                      (listp config) (= (length config) 4)
                      (destructuring-bind (salt version threshold owners) config
                        (and (fixed salt 32)
                             (let ((version (uint version 8)) (threshold (uint threshold 1))
                                   (previous 0) (total 0) (weights nil))
                               (and (plusp threshold) (listp owners) (<= 1 (length owners) 48)
                                    (every (lambda (owner)
                                             (and (listp owner) (= (length owner) 2)
                                                  (fixed (first owner) 20)
                                                  (let ((address (octets->integer (first owner)))
                                                        (weight (uint (second owner) 1)))
                                                    (and (> address previous) (plusp weight)
                                                         (or (plusp version) (/= address (octets->integer account)))
                                                         (progn (setf previous address)
                                                                (incf total weight)
                                                                (push weight weights) t))))) owners)
                                    (<= total 255)
                                    (<= threshold (loop for w in (sort weights #'>) for i below 8 sum w))))))
                      (listp signatures) (<= 1 (length signatures) 8)
                      (every (lambda (sig)
                               (and (typep sig 'octets) (<= (length sig) 2049)
                                    (payment-primitive-signature-p sig 0 (length sig)))) signatures))))))
    (error () nil)))

(defun payment-voucher-signature-p (input start end rules)
  (or (payment-primitive-signature-p input start end)
      (and (eq rules :v3) (< start end) (/= (- end start) 65)
           (= (aref input start) 5)
           (payment-configurable-signature-p input (1+ start) end))))

(defun payment-dynamic-call-p (input shape rules)
  (let* ((words (if (eq shape :settle) 9 10)) (size (length input)))
    (and (<= (+ 4 (* 32 words)) size 2048)
         ;; abi_decode_validate checks narrow integers/address high bytes. It is
         ;; not strict ABI mode: gaps, overlaps, unaligned tails and trailing
         ;; bytes (even missing/nonzero bytes padding) are accepted by Alloy.
         (loop for i in '(0 1 2 3 5)
               always (loop for p from (+ 4 (* i 32)) below (+ 16 (* i 32))
                            always (zerop (aref input p))))
         (loop for i from 7 below (1- words)
               always (loop for p from (+ 4 (* i 32)) below (+ 24 (* i 32))
                            always (zerop (aref input p))))
         (let ((offset (octets->integer input :start (+ 4 (* 32 (1- words)))
                                             :end (+ 4 (* 32 words)))))
           (and (<= (+ offset 36) size)
                (let* ((length (octets->integer input :start (+ offset 4) :end (+ offset 36)))
                       (end (+ offset 36 length)))
                  (and (<= end size)
                       (payment-voucher-signature-p input (+ offset 36) end rules))))))))

(defun payment-call-p (to input rules)
  (if (eq rules :v1)
      (payment-token-address-p to)
      (and to (>= (length input) 4)
           (let* ((selector (octets->integer input :end 4))
                  (shape (cdr (assoc selector
                                    (cond ((payment-token-address-p to) *payment-token-shapes*)
                                          ((= to #x4d50500000000000000000000000000000000000)
                                           *payment-channel-shapes*))))))
             (and shape
                  (if (integerp shape)
                      (<= (+ 4 (* 32 shape)) (length input)
                          (+ 4 (* 32 shape) (if (eq rules :v3) 32 0)))
                      (payment-dynamic-call-p input shape rules)))))))

(defun transaction-payment-p (tx rules)
  "Classify decoded TX for the payment lane under :V1, :V2 or :V3.
V1 checks destination prefixes only; V2 adds TIP-1045 calldata and authorization
limits; V3 permits 32 static trailing bytes and configurable voucher encodings.
Voucher signatures are checked for encoding only, not cryptographic validity or
state authorization, exactly as the reference payment classifier does."
  (check-type rules (member :v1 :v2 :v3))
  (let ((aa (= (transaction-type tx) #x76)))
    (and (or (eq rules :v1)
             (and (null (transaction-access-list tx))
                  (null (transaction-authorization-list tx))
                  (or (not aa)
                      (and (transaction-calls tx)
                           (null (transaction-tempo-authorization-list tx))
                           (or (/= (length (transaction-fields tx)) 15)
                               (<= (clempo.rlp:rlp-encoded-length (nth 13 (transaction-fields tx)))
                                   1024))))))
         (if aa
             (every (lambda (call)
                      (payment-call-p (unless (zerop (length (first call)))
                                        (octets->integer (first call)))
                                      (third call) rules))
                    (transaction-calls tx))
             (payment-call-p (transaction-to tx) (transaction-input tx) rules)))))
