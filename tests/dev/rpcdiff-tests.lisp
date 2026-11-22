(defpackage #:clempo.dev.rpcdiff-tests
  (:use #:cl #:clempo.base))

(in-package #:clempo.dev.rpcdiff-tests)

(defun parse (s) (clempo.json:parse-json s))

(deftest head-relative-cases-check-every-other-value ()
  ;; an eth_capabilities answer recorded at the reference's head 0x10 (hash 0xaa)
  (let ((c (parse "{\"suite\":\"gaps\",\"method\":\"eth_capabilities\",\"status\":200,\"class\":\"stable\",\"headRelative\":true,\"response\":{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"head\":{\"number\":\"0x10\",\"hash\":\"0xaa\"},\"state\":{\"disabled\":false,\"oldestBlock\":\"0x0\"},\"stateproofs\":{\"disabled\":false,\"oldestBlock\":\"0x10\"}}}}")))
    (flet ((answer (head hash oldest proofs)
             (parse (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"head\":{\"number\":~S,\"hash\":~S},\"state\":{\"disabled\":false,\"oldestBlock\":~S},\"stateproofs\":{\"disabled\":false,\"oldestBlock\":~S}}}"
                            head hash oldest proofs))))
      (is (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x0" "0x20")))
      (is (not (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x1" "0x20"))))
      (is (not (clempo.dev::case-matches c 200 (answer "0x20" "0xbb" "0x0" "0x10"))))
      ;; without the flag the recorded head must match too
      (let ((plain (remove "headRelative" (cdr c) :key #'car :test #'equal)))
        (is (not (clempo.dev::case-matches (cons :object plain) 200 (answer "0x20" "0xbb" "0x0" "0x20"))))))))

(defun case-json (method class &optional (suite "s"))
  (clempo.dev::jo "suite" suite "method" method "status" 200 "class" class
                  "response" (clempo.dev::jo "jsonrpc" "2.0" "id" 1 "result" "0x1")))

(deftest only-value-identical-cases-score ()
  (let* ((known (clempo.dev::reference-methods))
         (m (first known))
         (results (list (list (case-json m "stable") 200 nil t)
                        (list (case-json m "live") 200 nil t)
                        (list (case-json m "node") 200 nil t)
                        (list (case-json "envelope" "stable") 200 nil t)
                        (list (case-json (second known) "live") 200 nil t))))
    (multiple-value-bind (rows methods-ok categories) (clempo.dev::rpcdiff-summary results)
      ;; a fake label is a case, never a method
      (is (null (assoc "envelope" rows :test #'string=)))
      ;; the shape-only method has no value case and does not count
      (is (= 1 methods-ok))
      (is (equal '(2 . 2) (cdr (assoc "value" categories :test #'string=))))
      (is (equal '(2 . 2) (cdr (assoc "shape" categories :test #'string=))))
      (is (equal '(1 . 1) (cdr (assoc "node" categories :test #'string=)))))
    ;; one failing value case fails its method
    (is (= 0 (nth-value 1 (clempo.dev::rpcdiff-summary
                           (list (list (case-json m "stable") 200 nil t)
                                 (list (case-json m "stable") 200 nil nil))))))))

(deftest empty-arrays-match-only-empty-arrays ()
  (is (clempo.dev::json-shape-equal #() #()))
  (is (not (clempo.dev::json-shape-equal #() #("0x1"))))
  (is (not (clempo.dev::json-shape-equal #("0x1") #())))
  (is (clempo.dev::json-shape-equal #("0x1") #("0x2"))))

(defun hexs (list) (map 'simple-vector #'octets->hex list))

(deftest proofs-are-checked-against-the-proven-state ()
  (let* ((present (hex->octets "0x20c0000000000000000000000000000000000000"))
         (absent (hex->octets "0x00000000000000000000000000000000deadbeef"))
         (trie (clempo.trie:make-trie :secure t))
         (zero (octets->hex (make-octets 32)))
         (empty-code (octets->hex clempo.trie:+empty-code-hash+)))
    (clempo.trie:trie-put trie present (clempo.trie:encode-account 3 77 clempo.trie:+empty-trie-root+
                                                                   clempo.trie:+empty-code-hash+))
    (let ((root (clempo.trie:trie-root trie)))
      (flet ((proof (address nonce balance storage-hash code-hash)
               (clempo.dev::jo "address" (octets->hex address) "nonce" nonce "balance" balance
                               "storageHash" storage-hash "codeHash" code-hash
                               "accountProof" (hexs (clempo.trie:trie-proof trie address))
                               "storageProof" #()))
             (expect (nonce code-hash balance)
               (clempo.dev::jo "nonce" nonce "codeHash" code-hash "balance" balance "storage" (clempo.dev::jo))))
        (let ((storage (octets->hex clempo.trie:+empty-trie-root+)))
          ;; the reference's eth_getBalance placeholder does not matter: the proof binds the balance
          (is (clempo.dev::verify-account-proof
               root (proof present "0x3" "0x4d" storage empty-code) (expect "0x3" empty-code "0x96")))
          ;; a balance the proof does not prove fails
          (is (not (clempo.dev::verify-account-proof
                    root (proof present "0x3" "0x4e" storage empty-code) (expect "0x3" empty-code "0x4e"))))
          ;; an absent account: reth's zero hashes, or the empty ones
          (is (clempo.dev::verify-account-proof
               root (proof absent "0x0" "0x0" zero zero) (expect "0x0" empty-code "0x0")))
          (is (clempo.dev::verify-account-proof
               root (proof absent "0x0" "0x0" storage empty-code) (expect "0x0" empty-code "0x0")))
          ;; zero hashes for a present account fail
          (is (not (clempo.dev::verify-account-proof
                    root (proof present "0x3" "0x4d" zero zero) (expect "0x3" empty-code "0x4d")))))))))

(deftest witnesses-prove-their-keys-under-the-pre-state-root ()
  (let* ((witness (parse (clempo.io:read-file-string
                          (asdf:system-relative-pathname "clempo-dev" "../../tests/dev/witness-3463.json"))))
         (root (hex->octets "0x7a91f968a770f34f613d35abc602fad42d51ec26afc7040155b34f7d06a0c70c"))
         (nodes (clempo.json:jget witness "state"))
         (root-node (find-if (lambda (n) (octets= root (clempo.keccak:keccak256 (hex->octets n)))) nodes)))
    (is-equalp root (clempo.dev::witness-pre-state-root witness))
    (is (clempo.dev::witness-reproduces-pre-state-p witness root))
    (is (not (clempo.dev::witness-reproduces-pre-state-p witness (make-octets 32))))
    (is root-node)
    (flet ((without (node)
             (cons :object (loop for (k . v) in (cdr witness)
                                 collect (cons k (if (equal k "state") (remove node v :test #'equal) v))))))
      (is (not (clempo.dev::witness-reproduces-pre-state-p (without root-node))))
      ;; a node below the root that a key's path needs breaks the check as well
      (is (some (lambda (n) (and (not (equal n root-node))
                                 (not (clempo.dev::witness-reproduces-pre-state-p (without n)))))
                nodes)))))
