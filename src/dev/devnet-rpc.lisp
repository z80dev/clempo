(in-package #:clempo.dev)

;;; RPC simulation of T14 configurable signatures against a reference devnet: after
;;; devnet-t14-send, clempo replays the devnet, serves the requests in-process over the
;;; replayed head state, and each answer (result, or error code and message) must equal the
;;; reference's for the same request at the same block. The requests carry
;;; configurableAccountSimulation (mock owner witnesses for the sender or a keyId delegate),
;;; keyId overrides, and an inline keyAuthorization with keyAuthorizationSimulation.

(defclass devnet-backend (clempo.rpc.chain:backend)
  ((db :initarg :db :reader devnet-backend-db)
   (raws :initarg :raws :reader devnet-backend-raws)      ; vector: block n's raw octets at n
   (hashes :initarg :hashes :reader devnet-backend-hashes)))  ; hex hash -> number

(defmethod clempo.rpc.chain:backend-head ((b devnet-backend)) (1- (length (devnet-backend-raws b))))

(defmethod clempo.rpc.chain:backend-block-raw ((b devnet-backend) n)
  (when (and (integerp n) (< 0 n (length (devnet-backend-raws b))))
    (aref (devnet-backend-raws b) n)))

(defmethod clempo.rpc.chain:backend-block-number ((b devnet-backend) hash)
  (gethash (octets->hex hash) (devnet-backend-hashes b)))

(defmethod clempo.rpc.chain:backend-state ((b devnet-backend) number)
  (when (= number (clempo.rpc.chain:backend-head b)) (devnet-backend-db b)))

(defun devnet-backend (url genesis)
  "Replay the devnet at URL from GENESIS to its head into a DEVNET-BACKEND; signals when a
block does not match."
  (let* ((chain (clempo.chainspec:load-chainspec genesis))
         (db (clempo.executor:genesis-db :chain chain))
         (head (devnet-head url))
         (raws (make-array (1+ head) :initial-element nil))
         (hashes (make-hash-table :test 'equal)))
    (loop for n from 1 to head
          do (let* ((raw (devnet-raw-block url n))
                    (block (clempo.blocks:decode-block raw))
                    (result (clempo.executor:execute-block db block :chain chain))
                    (mismatches (clempo.executor:block-mismatches result (clempo.blocks:block-header block))))
               (when mismatches (error "devnet block ~D does not match: ~S" n mismatches))
               (clempo.store:db-commit-block db n (clempo.blocks:block-hash block))
               (setf (aref raws n) raw
                     (gethash (octets->hex (clempo.blocks:block-hash block)) hashes) n)))
    (make-instance 'devnet-backend :chain genesis :db db :raws raws :hashes hashes)))

(defun config-rlp-hex (salt version threshold owner-keys)
  "ConfigurableAccountConfig's RLP, as configurableAccountSimulation.config takes it."
  (octets->hex (clempo.rlp:rlp-encode
                (list (integer->octets salt 32) version threshold
                      (mapcar (lambda (o) (list (integer->octets (first o) 20) (second o)))
                              (apply #'owners-of owner-keys))))
               :prefix t))

(defun simulation-spec (salt version threshold owner-keys &key signer key-type)
  (apply #'clempo.json:jobject
         (append (when signer (list "signer" (address-hex signer)))
                 (list "config" (config-rlp-hex salt version threshold owner-keys)
                       "approvals" (coerce (mapcar (lambda (o)
                                                     (apply #'clempo.json:jobject "owner" (address-hex (first o))
                                                            (when key-type (list "keyType" key-type))))
                                                   (apply #'owners-of owner-keys))
                                           'vector)))))

(defun address-hex (a) (format nil "0x~(~40,'0x~)" a))

(defun t14-simulation-requests (head)
  "(label method params) for the requests compared; HEAD is the block they run at."
  (destructuring-bind (k0 k1 k2 k3 k4) *devnet-keys*
    (declare (ignore k1 k2))
    (let* ((a0 (key-address k0)) (a3 (key-address k3)) (a4 (key-address k4))
           (d (derived-account 7 2 (list +owner-1+ +owner-2+)))
           (block (quantity-hex head))
           (transfer (octets->hex (clempo.abi:abi-encode-call
                                   (clempo.abi:abi-selector "transfer(address,uint256)")
                                   '(:address (:uint 256)) (list a3 1))
                                  :prefix t))
           (calls (vector (clempo.json:jobject "to" (address-hex +path-usd+) "value" "0x0" "input" transfer)))
           (upgraded (simulation-spec (upgrade-salt a4) 1 1 (list +owner-1+) :key-type "secp256k1"))
           (grant-fields (list 1337 0 (integer->octets #xbeef 20)))
           (grant-approval (configurable-approval a4 (upgrade-salt a4) 1 1 (list +owner-1+)
                                                  (clempo.keccak:keccak256 (clempo.rlp:rlp-encode grant-fields)))))
      (flet ((request (from &rest more)
               (apply #'clempo.json:jobject "from" (address-hex from) "calls" calls more)))
        (let ((cases
                `(("upgraded-sender-witness"
                   ,(request a4 "configurableAccountSimulation" upgraded))
                  ("upgraded-sender-webauthn-default"
                   ,(request a4 "configurableAccountSimulation"
                             (simulation-spec (upgrade-salt a4) 1 1 (list +owner-1+))))
                  ("upgraded-sender-without-witness"
                   ,(request a4))
                  ("upgraded-sender-wrong-config"
                   ,(request a4 "configurableAccountSimulation"
                             (simulation-spec (upgrade-salt a4) 1 1 (list +owner-2+) :key-type "secp256k1")))
                  ("configurable-access-key"
                   ,(request a0 "keyId" (address-hex d)
                             "configurableAccountSimulation"
                             (simulation-spec 7 0 2 (list +owner-1+ +owner-2+) :signer d :key-type "secp256k1")))
                  ("configurable-access-key-one-approval"
                   ,(request a0 "keyId" (address-hex d)
                             "configurableAccountSimulation"
                             (simulation-spec 7 0 2 (list +owner-1+) :signer d :key-type "secp256k1")))
                  ("access-key-override-unknown"
                   ,(request a0 "keyId" (address-hex #xdead) "keyType" "secp256k1"))
                  ("grant-signed-by-quorum"
                   ,(request a4 "configurableAccountSimulation" upgraded
                             "keyAuthorization"
                             (clempo.json:jobject "chainId" "0x539" "keyType" "secp256k1"
                                                  "keyId" (address-hex #xbeef) "expiry" :null "limits" :null
                                                  "signature" (octets->hex (subseq grant-approval 1) :prefix t))
                             "keyAuthorizationSimulation" upgraded)))))
          (append
           (loop for (label req) in cases
                 append (list (list (format nil "eth_estimateGas ~A" label) "eth_estimateGas" (list req block))
                              (list (format nil "eth_call ~A" label) "eth_call" (list req block))))
           (list (list "eth_simulateV1 upgraded-sender-witness" "eth_simulateV1"
                       (list (clempo.json:jobject
                              "blockStateCalls"
                              (vector (clempo.json:jobject
                                       "calls" (vector (second (first cases))))))
                             block)))))))))

(defun json-sorted (v)
  "V with every object's keys sorted: JSON object member order is not part of an answer."
  (cond ((clempo.json:jobject-p v)
         (cons :object (sort (mapcar (lambda (c) (cons (car c) (json-sorted (cdr c)))) (copy-list (cdr v)))
                             #'string< :key #'car)))
        ((and (vectorp v) (not (stringp v))) (map 'vector #'json-sorted v))
        (t v)))

(defun answer-of (response)
  "A JSON-RPC response object reduced to what must match: the result, or the error's code
and message."
  (let ((error (clempo.json:jget response "error")))
    (if (and error (not (eq error :null)))
        (list :error (clempo.json:jget error "code") (clempo.json:jget error "message")
              (clempo.json:to-json-string (clempo.json:jget error "data" :null)))
        (list :result (clempo.json:to-json-string (json-sorted (clempo.json:jget response "result")))))))

(defun reference-response (url method params)
  (clempo.json:parse-json
   (nth-value 1 (clempo.http:http-post url (clempo.json:to-json-octets
                                            (clempo.json:jobject "jsonrpc" "2.0" "id" 1 "method" method
                                                                 "params" (coerce params 'vector)))))))

(defun clempo-response (backend method params)
  (let ((text (clempo.json:to-json-string
               (clempo.json:jobject "jsonrpc" "2.0" "id" 1 "method" method
                                    "params" (coerce params 'vector)))))
    (clempo.json:parse-json
     (clempo.rpc.chain:with-backend-read (backend)
       (clempo.rpc:handle-request-object text 0 (length text))))))

(defcommand "devnet-rpc-diff" (args)
  "compare T14 RPC simulations (configurable witnesses, keyId overrides) with a reference devnet: -rpc URL -genesis PATH"
  (let ((url (arg args :rpc)) (genesis (arg args :genesis)))
    (unless (and url genesis)
      (log-line "devnet-rpc-diff: specify -rpc URL -genesis PATH")
      (return-from command 2))
    (let* ((backend (devnet-backend url genesis))
           (head (clempo.rpc.chain:backend-head backend))
           (mismatches 0) (total 0))
      (loop for (label method params) in (t14-simulation-requests head)
            for reference = (answer-of (reference-response url method params))
            for ours = (answer-of (clempo-response backend method params))
            do (incf total)
               (if (equal reference ours)
                   (log-line (format nil "match ~A: ~S" label reference))
                   (progn (incf mismatches)
                          (log-line (format nil "MISMATCH ~A~%  reference ~S~%  clempo    ~S" label reference ours)))))
      (write-line (clempo.json:to-json-string
                   (clempo.json:jobject "block" head "requests" total "mismatches" mismatches)))
      (if (zerop mismatches) 0 1))))
