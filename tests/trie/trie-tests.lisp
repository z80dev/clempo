(defpackage #:clempo.trie.tests
  (:use #:cl #:clempo.base #:clempo.trie))

(in-package #:clempo.trie.tests)

(defun fixture (name)
  (clempo.json:parse-json
   (clempo.io:read-file-string
    (asdf:system-relative-pathname "clempo-trie"
                                  (concatenate 'string "../../tests/trie/data/" name)))))

(deftest ordered-empty-root ()
  (is-equalp (hex->octets "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")
             +empty-trie-root+)
  (is-equalp +empty-trie-root+ (ordered-trie-root nil)))

(deftest ordered-mainnet-transactions-root ()
  (let* ((record (fixture "mainnet_41788340.json"))
         (block (clempo.rlp:rlp-decode (hex->octets (clempo.json:jget record "rawBlock"))))
         (items (loop for tx in (second block)
                      collect (if (listp tx) (clempo.rlp:rlp-encode tx) tx))))
    (is (plusp (length items)))
    (is-equalp (hex->octets (clempo.json:jget record "transactionsRoot"))
               (ordered-trie-root items))))
