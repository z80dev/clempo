(in-package #:clempo.dev)

;;; Value-checked proofs for R5. The reference keeps reth's eth_proof_window of 0, so it
;;; answers eth_getProof, eth_getAccount and eth_getMultiProof only at its moving head and no
;;; recorded proof can be compared byte for byte later. A Merkle proof is still checkable for
;;; good: the "proofs" suite records, at fixed historical blocks, the reference's state root
;;; (the block header) and the accounts' balance, nonce, code hash and storage values
;;; (eth_getBalance, eth_getTransactionCount, eth_getCode, eth_getStorageAt, which the archive
;;; reference answers at any block). rpcdiff then asks clempo for the proofs at those blocks,
;;; with its proof window opened, and accepts an answer only when every account proof hashes
;;; up to the recorded state root, every storage proof up to the proven storage root, and
;;; every value equals the reference's. Cases are class "verified", graded as value cases.
;;;
;;;   clempo-dev rpc-record-proofs [-url U]

(defparameter *proof-targets*
  '((14000000 . (("0x20c0000000000000000000000000000000000000" "0x0" "0x1" "0x2" "0x3")
                 ("0xfeec000000000000000000000000000000000000" "0x0" "0x1")
                 ("0x00000000000000000000000000000000deadbeef" "0x0")))
    (14000010 . (("0x20c0000000000000000000000000000000000000" "0x0" "0x5")
                 ("0x4e4f4e4345000000000000000000000000000000")))
    (16000000 . (("0x20c0000000000000000000000000000000000000" "0x0" "0x1" "0x2")
                 ("0xfeec000000000000000000000000000000000000")
                 ("0x0000000000000000000000000000000000000000"))))
  "Blocks (each a history checkpoint or a few blocks past one) and (address slot...) targets;
RECORD-PROOF-CASES adds the sender of each block's first transaction.")

(defun ref-result (url method &rest params)
  (multiple-value-bind (status response) (post-raw url (rpc-request-string method params))
    (let ((result (and (eql status 200) (clempo.json:jobject-p response) (clempo.json:jget response "result"))))
      (or result (error "~A ~S: no result from the reference: ~S" method params response)))))

(defun block-tag (n) (quantity->hex n))

(defun reference-account (url address slots block)
  "The reference's view of ADDRESS at BLOCK, as the expected values of a verified case."
  (let ((tag (block-tag block)))
    (jo "balance" (ref-result url "eth_getBalance" address tag)
        "nonce" (ref-result url "eth_getTransactionCount" address tag)
        "codeHash" (octets->hex (clempo.keccak:keccak256 (hex->octets (ref-result url "eth_getCode" address tag))))
        "storage" (cons :object (loop for s in slots
                                      collect (cons s (ref-result url "eth_getStorageAt" address s tag)))))))

(defun first-sender (url block)
  (let* ((b (ref-result url "eth_getBlockByNumber" (block-tag block) :true))
         (txs (clempo.json:jget b "transactions")))
    (and (plusp (length txs)) (clempo.json:jget (aref txs 0) "from"))))

(defun proof-cases (url)
  "The verified cases for *PROOF-TARGETS*, with the reference's expected values."
  (loop for (block . targets) in *proof-targets*
        append (let* ((sender (first-sender url block))
                      (all (if (and sender (not (find sender targets :key #'car :test #'string-equal)))
                               (append targets (list (list sender)))
                               targets))
                      (tag (block-tag block))
                      (expect (jo "stateRoot" (clempo.json:jget (ref-result url "eth_getBlockByNumber" tag :false)
                                                                "stateRoot")
                                  "accounts" (cons :object
                                                   (loop for (address . slots) in all
                                                         collect (cons address (reference-account url address slots block)))))))
                 (flet ((verified (method request)
                          (jo "suite" "proofs" "method" method "request" request
                              "status" 200 "response" :null "class" "verified" "expect" expect)))
                   (append
                    (loop for (address . slots) in all
                          collect (verified "eth_getProof" (r "eth_getProof" address (coerce slots 'simple-vector) tag))
                          collect (verified "eth_getAccount" (r "eth_getAccount" address tag)))
                    (list (verified "eth_getMultiProof"
                                    (r "eth_getMultiProof"
                                       (coerce (loop for (address . slots) in all
                                                     ;; B256 keys: eth_getMultiProof takes no short form
                                                     collect (jv address (map 'simple-vector
                                                                              (lambda (s) (format nil "0x~(~64,'0x~)" (hex->integer s)))
                                                                              slots)))
                                               'simple-vector)
                                       tag))))))))

(defun record-proof-cases (url)
  (let ((path (corpus-path "corpus-proofs.jsonl"))
        (cases (proof-cases url)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                              :external-format :utf-8)
      (dolist (c cases) (write-line (to-json-string* c) out)))
    (length cases)))

(defcommand "rpc-record-proofs" (args)
  "record the value-checked proof cases (corpus-proofs.jsonl) from the reference: rpc-record-proofs [-url U]"
  (format t "proofs: ~D cases~%" (record-proof-cases (arg args :url (clempo.jsonrpc:local-rpc-url))))
  0)

;;; Checking an answer

(defun hex-octets (s) (hex->octets (if (and (>= (length s) 2) (string-equal "0x" s :end2 2)) (subseq s 2) s)))

(defun proof-nodes (v) (map 'list #'hex-octets v))

(defun quantity= (a b) (and (stringp a) (stringp b) (= (hex->integer a) (hex->integer b))))

(defun verify-storage (storage-root item expected)
  "A storage proof ITEM {key, value, proof} that hashes up to STORAGE-ROOT and whose value is
the reference's (EXPECTED, the slot -> value object)."
  (let* ((key (hex->integer (clempo.json:jget item "key")))
         (want (loop for (k . v) in (cdr expected) when (= (hex->integer k) key) return (hex->integer v))))
    (multiple-value-bind (leaf valid)
        (clempo.trie:verify-proof storage-root (integer->octets key 32)
                                  (proof-nodes (clempo.json:jget item "proof")) :secure t)
      (let ((proven (if leaf (octets->integer (clempo.rlp:rlp-decode leaf)) 0)))
        (and valid want
             (= proven want)
             (= (hex->integer (clempo.json:jget item "value")) want))))))

(defun zero-hash-p (s)
  (and (stringp s) (let ((o (hex-octets s))) (and (= (length o) 32) (every #'zerop o)))))

(defun verify-account-proof (state-root proof expected)
  "An eth_getProof answer PROOF whose account proof hashes up to STATE-ROOT, whose fields are
the ones proven and the reference's (EXPECTED), and whose storage proofs all verify.
The balance is checked only against the proof: the reference's eth_getBalance answers
Tempo's placeholder balance, not the account's state balance, and the proof already binds
the state balance to the reference's state root. An absent account may carry reth's zero
code and storage hashes."
  (let ((address (hex-octets (clempo.json:jget proof "address"))))
    (multiple-value-bind (leaf valid)
        (clempo.trie:verify-proof state-root address (proof-nodes (clempo.json:jget proof "accountProof")) :secure t)
      (and valid
           (destructuring-bind (&optional (nonce 0) (balance 0)
                                  (storage-root clempo.trie:+empty-trie-root+)
                                  (code-hash clempo.trie:+empty-code-hash+) &rest ext)
               (if leaf
                   (mapcar (lambda (field i) (if (< i 2) (octets->integer field) field))
                           (clempo.rlp:rlp-decode leaf) '(0 1 2 3 4))
                   '())
             (declare (ignore ext))
             (flet ((hash-ok (key proven)
                      (let ((got (clempo.json:jget proof key)))
                        (or (octets= proven (hex-octets got))
                            (and (null leaf) (zero-hash-p got))))))
               (and (= nonce (hex->integer (clempo.json:jget proof "nonce")))
                    (= balance (hex->integer (clempo.json:jget proof "balance")))
                    (hash-ok "storageHash" storage-root)
                    (hash-ok "codeHash" code-hash)
                    (quantity= (clempo.json:jget proof "nonce") (clempo.json:jget expected "nonce"))
                    (or (string-equal (clempo.json:jget proof "codeHash") (clempo.json:jget expected "codeHash"))
                        (and (null leaf) (zero-hash-p (clempo.json:jget proof "codeHash"))
                             (string-equal (clempo.json:jget expected "codeHash")
                                           (octets->hex clempo.trie:+empty-code-hash+))))
                    (= (length (clempo.json:jget proof "storageProof"))
                       (length (cdr (clempo.json:jget expected "storage"))))
                    (every (lambda (item) (verify-storage storage-root item (clempo.json:jget expected "storage")))
                           (clempo.json:jget proof "storageProof")))))))))

(defun expected-account (c address)
  (loop for (a . v) in (cdr (clempo.json:jget* c "expect" "accounts"))
        when (string-equal a address) return v))

(defun replay-verified-case (url c)
  "Ask clempo at URL for C's proofs; (values status response ok)."
  (let* ((request (clempo.json:parse-json (clempo.json:jget c "request")))
         (params (clempo.json:jget request "params"))
         (root (hex-octets (clempo.json:jget* c "expect" "stateRoot"))))
    (multiple-value-bind (status response) (post-raw url (clempo.json:jget c "request") :retries 0)
      (let ((result (and (clempo.json:jobject-p response) (clempo.json:jget response "result"))))
        (values
         status response
         (and (eql status 200) result
              (let ((method (clempo.json:jget c "method")))
                (cond
                  ((string= method "eth_getProof")
                   (verify-account-proof root result (expected-account c (aref params 0))))
                  ((string= method "eth_getMultiProof")
                   (and (= (length result) (length (aref params 0)))
                        (every (lambda (proof target)
                                 (and (string-equal (clempo.json:jget proof "address") (aref target 0))
                                      (verify-account-proof root proof (expected-account c (aref target 0)))))
                               result (aref params 0))))
                  ((string= method "eth_getAccount")
                   ;; the storage root is checked through clempo's own proof of the account,
                   ;; itself checked against the header's state root
                   (let* ((address (aref params 0))
                          (expected (expected-account c address))
                          (proof (clempo.json:jget
                                  (nth-value 1 (post-raw url (rpc-request-string
                                                              "eth_getProof" (list address #() (aref params 1)))
                                                         :retries 0))
                                  "result")))
                     (and proof
                          (verify-account-proof root proof (jo "nonce" (clempo.json:jget expected "nonce")
                                                               "codeHash" (clempo.json:jget expected "codeHash")
                                                               "storage" (jo)))
                          (if (eq result :null)
                              ;; reth answers null for an account absent from the state:
                              ;; accepted only when the verified proof proves the absence
                              (null (clempo.trie:verify-proof
                                     root (hex-octets address)
                                     (proof-nodes (clempo.json:jget proof "accountProof")) :secure t))
                              (and (quantity= (clempo.json:jget result "balance") (clempo.json:jget proof "balance"))
                                   (quantity= (clempo.json:jget result "nonce") (clempo.json:jget expected "nonce"))
                                   (string-equal (clempo.json:jget result "codeHash") (clempo.json:jget proof "codeHash"))
                                   (string-equal (clempo.json:jget result "storageRoot")
                                                 (clempo.json:jget proof "storageHash")))))))))))))))
