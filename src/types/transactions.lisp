(defpackage #:clempo.transactions
  (:use #:cl #:clempo.base)
  (:import-from #:clempo.rlp #:rlp-decode #:rlp-encode #:rlp-decode-integer)
  (:export #:transaction #:decode-transaction #:encode-transaction
           #:transaction-encoded-length #:encode-transaction-into #:transaction-unique-identifier
           #:transaction-hash #:transaction-signing-hash #:transaction-sender
           #:transaction-fee-payer #:transaction-key-id
           #:transaction-key-authorization-signer #:transaction-type #:transaction-fields
           #:calldata-tokens #:tempo-signature-verification-gas #:validate-calls
           #:validate-signature-version #:tempo-authorization-fields
           #:validate-key-authorization))
(in-package #:clempo.transactions)

(defstruct (transaction (:constructor %make-transaction (type fields raw)))
  (type 0 :type octet)
  (fields nil :type list)
  (raw (make-octets 0) :type octets))

;; Wire layouts: bounded unsigned integers, byte strings, and nested RLP lists.
;; FIELDS deliberately retains the decoded RLP representation, including empty
;; strings versus empty lists. RAW is separate and used only for the received hash.
(defmacro define-transaction-layouts (&rest layouts)
  "Declare wire validation and public, typed field readers together."
  (let ((names (remove-duplicates
                (loop for layout in layouts append (mapcar #'first (rest layout))))))
    `(progn
       (defparameter *layouts*
         ',(loop for (type . fields) in layouts
                 collect (cons type (mapcar #'second fields))))
       ,@(loop for name in names
               for reader = (intern (format nil "TRANSACTION-~A" name))
               collect
               `(progn
                  (export ',reader)
                  (defun ,reader (tx)
                    (case (transaction-type tx)
                      ,@(loop for (type . fields) in layouts
                              for index = (position name fields :key #'first)
                              when index
                                collect
                                (let ((kind (second (nth index fields))))
                                  `(,type
                                    (let ((field (nth ,index (transaction-fields tx))))
                                      ,(cond
                                         ((member name '(valid-before valid-after))
                                          '(let ((value (field-integer field 8)))
                                             (unless (zerop value) value)))
                                         ((integerp kind) `(field-integer field ,kind))
                                         ((member kind '(:address :destination))
                                          '(unless (zerop (length field))
                                             (octets->integer field)))
                                         (t 'field))))))
                      ,@(when (eq name 'chain-id)
                          '((0 (let ((v (field-integer (nth 6 (transaction-fields tx)) 8)))
                                 (when (>= v 35) (floor (- v 35) 2))))))
                      (otherwise nil))))))))

(define-transaction-layouts
  (0 (nonce 8) (gas-price 16) (gas-limit 8) (to :destination)
     (value 32) (input :bytes) (v 8) (r 32) (s 32))
  (1 (chain-id 8) (nonce 8) (gas-price 16) (gas-limit 8)
     (to :destination) (value 32) (input :bytes) (access-list :access)
     (v 1) (r 32) (s 32))
  (2 (chain-id 8) (nonce 8) (max-priority-fee-per-gas 16) (max-fee-per-gas 16)
     (gas-limit 8) (to :destination) (value 32) (input :bytes)
     (access-list :access) (v 1) (r 32) (s 32))
  (4 (chain-id 8) (nonce 8) (max-priority-fee-per-gas 16) (max-fee-per-gas 16)
     (gas-limit 8) (to :address) (value 32) (input :bytes)
     (access-list :access) (authorization-list :authorizations) (v 1) (r 32) (s 32))
  (#x76 (chain-id 8) (max-priority-fee-per-gas 16) (max-fee-per-gas 16)
        (gas-limit 8) (calls :calls) (access-list :access) (nonce-key 32) (nonce 8)
        (valid-before 8) (valid-after 8) (fee-token :destination)
        (fee-payer-signature :sponsor) (tempo-authorization-list :tempo-authorizations)))

(defun field-integer (field &optional (width 32))
  (rlp-decode-integer field :max-bytes width))

(defun require-count (fields count)
  (invariant (and (listp fields) (= (length fields) count))
             "Expected ~D transaction fields" count))

(defun validate-field (field kind)
  (cond
    ((integerp kind) (field-integer field kind))
    ((eq kind :bytes) (invariant (typep field 'octets) "Expected transaction bytes"))
    ((member kind '(:address :destination))
     (invariant (and (typep field 'octets)
                     (or (= (length field) 20)
                         (and (eq kind :destination) (zerop (length field)))))
                "Invalid transaction address"))
    ((eq kind :sponsor)
     (unless (and (typep field 'octets) (zerop (length field)))
       (require-count field 3)
       (invariant (<= (field-integer (first field) 1) 1) "Invalid sponsor parity")
       (field-integer (second field)) (field-integer (third field))))
    (t
     (invariant (listp field) "Expected transaction list")
     (dolist (entry field)
       (ecase kind
         (:access
          (require-count entry 2)
          (validate-field (first entry) :address)
          (invariant (listp (second entry)) "Expected storage keys")
          (dolist (key (second entry))
            (invariant (and (typep key 'octets) (= (length key) 32)) "Invalid storage key")))
         (:calls
          (require-count entry 3)
          (validate-field (first entry) :destination)
          (field-integer (second entry))
          (validate-field (third entry) :bytes))
         (:authorizations
          (require-count entry 6)
          (field-integer (first entry))
          (validate-field (second entry) :address)
          (field-integer (third entry) 8)
          (field-integer (fourth entry) 1)
          (field-integer (fifth entry)) (field-integer (sixth entry)))
         (:tempo-authorizations
          (require-count entry 4)
          (field-integer (first entry))
          (validate-field (second entry) :address)
          (field-integer (third entry) 8)
          (validate-field (fourth entry) :bytes)))))))

(defun validate-calls (calls authorization-list)
  "Check Tempo call policy on decoded call triples. Return T or a rejection keyword."
  (cond ((null calls) :empty-calls)
        ((and authorization-list (zerop (length (first (first calls)))))
         :create-with-authorizations)
        ((some (lambda (call) (zerop (length (first call)))) (rest calls))
         :create-not-first)
        (t t)))
(defun decode-transaction (raw &key (copy-raw t))
  "Decode an EIP-2718 envelope. COPY-RAW NIL transfers ownership of RAW to the result."
  (invariant (and (typep raw 'octets) (plusp (length raw))) "Empty transaction")
  (let* ((type (if (>= (aref raw 0) #xc0) 0 (aref raw 0)))
         (layout (cdr (assoc type *layouts*)))
         (fields (rlp-decode raw :start (if (zerop type) 0 1))))
    (invariant layout "Unsupported transaction type ~D" type)
    (invariant (listp fields) "Transaction must be an RLP list")
    (if (= type #x76)
        (progn
          (invariant (member (length fields) '(14 15)) "Invalid Tempo field count")
          (when (= (length fields) 15)
            (invariant (consp (nth 13 fields)) "Expected key authorization"))
          (validate-field (car (last fields)) :bytes)
          (clempo.signatures:signature-kind (car (last fields))))
        (require-count fields (length layout)))
    (loop for kind in layout for field in fields do (validate-field field kind))
    (cond
      ((zerop type)
       (let ((v (field-integer (nth 6 fields) 8)))
         (invariant (or (= v 27) (= v 28) (>= v 35)) "Invalid legacy v")))
      ((= type #x76)
       (let ((calls (nth 4 fields)) (before (field-integer (nth 8 fields) 8))
             (after (field-integer (nth 9 fields) 8)))
         (let ((result (validate-calls calls (nth 12 fields))))
           (invariant (eq result t) "Invalid Tempo calls: ~A" result))
         (invariant (or (zerop before) (zerop after) (> before after))
                    "Invalid transaction validity window")))
      (t (invariant (<= (field-integer (nth (- (length fields) 3) fields) 1) 1)
                    "Invalid transaction parity")))
    (%make-transaction type fields (if copy-raw (copy-octets raw) raw))))

(defun encode-envelope (type fields)
  (let* ((prefix (if (zerop type) 0 1))
         (out (make-octets (+ prefix (clempo.rlp:rlp-encoded-length fields)))))
    (when (plusp prefix) (setf (aref out 0) type))
    (clempo.rlp:rlp-encode-into fields out prefix)
    out))

(defun transaction-encoded-length (tx)
  (+ (if (zerop (transaction-type tx)) 0 1)
     (clempo.rlp:rlp-encoded-length (transaction-fields tx))))

(defun encode-transaction-into (tx out position)
  "Write the reconstructed EIP-2718 envelope into OUT; return the ending position."
  (declare (type octets out) (type fixnum position))
  (unless (zerop (transaction-type tx))
    (setf (aref out position) (transaction-type tx))
    (incf position))
  (clempo.rlp:rlp-encode-into (transaction-fields tx) out position))

(defun encode-transaction (tx)
  "Reconstruct the envelope from its fields; never reuse the original bytes."
  (let ((out (make-octets (transaction-encoded-length tx))))
    (encode-transaction-into tx out 0)
    out))

(defun transaction-hash (tx)
  (clempo.keccak:keccak256 (transaction-raw tx)))

(defun transaction-signing-fields (tx)
  (let ((type (transaction-type tx)) (fields (transaction-fields tx)))
    (cond
      ((zerop type)
       (let ((v (field-integer (nth 6 fields) 8)))
         (append (subseq fields 0 6)
                 (when (>= v 35) (list (floor (- v 35) 2) 0 0)))))
      ((= type #x76)
       (let ((result (butlast fields)))
         (if (listp (nth 11 fields))
             (setf (nth 10 result) (make-octets 0)
                   (nth 11 result) (octets-of 0))
             (setf (nth 11 result) (make-octets 0)))
         result))
      (t (butlast fields 3)))))

(defun transaction-signing-hash (tx)
  (clempo.keccak:keccak256 (encode-envelope (transaction-type tx) (transaction-signing-fields tx))))

(defun transaction-unique-identifier (tx sender)
  "Sender-scoped replay ID: Keccak(signing envelope || 20-byte sender).
Unlike the received transaction hash, signatures do not affect this identifier."
  (let* ((fields (transaction-signing-fields tx))
         (prefix (if (zerop (transaction-type tx)) 0 1))
         (size (+ prefix (clempo.rlp:rlp-encoded-length fields)))
         (out (make-octets (+ size 20))))
    (when (plusp prefix) (setf (aref out 0) (transaction-type tx)))
    (clempo.rlp:rlp-encode-into fields out prefix)
    (dotimes (i 20) (setf (aref out (+ size i)) (ldb (byte 8 (* 8 (- 19 i))) sender)))
    (clempo.keccak:keccak256 out)))

(defun recover-ecdsa (hash v r s)
  (let ((pub (clempo.secp256k1:ecrecover hash r s v)))
    (invariant pub "Invalid transaction signature")
    (clempo.secp256k1:pubkey->address pub)))

(defun transaction-sender (tx)
  (let ((fields (transaction-fields tx)) (type (transaction-type tx)))
    (if (= type #x76)
        (clempo.signatures:recover-signature (car (last fields)) (transaction-signing-hash tx))
        (let* ((signature (last fields 3))
               (v (field-integer (first signature) 8))
               (r (field-integer (second signature)))
               (s (field-integer (third signature)))
               (parity (if (zerop type) (if (>= v 35) (mod (- v 35) 2) (- v 27)) v)))
          (if (and (zerop type) (zerop parity) (zerop r) (zerop s))
              0
              (recover-ecdsa (transaction-signing-hash tx) parity r s))))))

(defun transaction-key-id (tx)
  (if (= (transaction-type tx) #x76)
      (nth-value 1 (transaction-sender tx))
      nil))

(defun transaction-fee-payer (tx)
  (let* ((sender (transaction-sender tx)) (fields (transaction-fields tx))
         (sponsor (and (= (transaction-type tx) #x76) (nth 11 fields))))
    (if (consp sponsor)
        (let ((unsigned (butlast fields)))
          (setf (nth 11 unsigned) (integer->octets sender 20))
          (recover-ecdsa (clempo.keccak:keccak256 (encode-envelope #x78 unsigned))
                         (field-integer (first sponsor) 1)
                         (field-integer (second sponsor))
                         (field-integer (third sponsor))))
        sender)))

(defun transaction-key-authorization-signer (tx)
  (when (and (= (transaction-type tx) #x76) (= (length (transaction-fields tx)) 15))
    (clempo.signatures:key-authorization-signer (nth 13 (transaction-fields tx)))))
