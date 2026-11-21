(defpackage #:clempo.rpc.bal.tests
  (:use #:cl #:clempo.base #:clempo.rpc.bal)
  (:local-nicknames (#:json #:clempo.json)))
(in-package #:clempo.rpc.bal.tests)

;;; EIP-7928 block access lists: alloy's JSON and RLP of a list (data/README.md).

(defun changes (vector key)
  "A JSON change array as BAL-ACCOUNT keeps it: (index . value), newest first."
  (reverse (map 'list (lambda (c) (cons (hex->integer (json:jget c "index")) (funcall key c))) vector)))

(defun accounts-from-json (list)
  "The BAL-ACCOUNTs of a BlockAccessList in alloy's JSON."
  (map 'list
       (lambda (j)
         (let ((a (make-bal-account (hex->integer (json:jget j "address")))))
           (loop for s across (json:jget j "storageChanges")
                 do (setf (gethash (hex->integer (json:jget s "key")) (bal-account-slots a))
                          (changes (json:jget s "changes") (lambda (c) (hex->integer (json:jget c "value"))))))
           (loop for s across (json:jget j "storageReads")
                 do (setf (gethash (hex->integer s) (bal-account-slots a)) '()))
           (setf (bal-account-balance a) (changes (json:jget j "balanceChanges")
                                                  (lambda (c) (hex->integer (json:jget c "value"))))
                 (bal-account-nonce a) (changes (json:jget j "nonceChanges")
                                                (lambda (c) (hex->integer (json:jget c "value"))))
                 (bal-account-code a) (changes (json:jget j "codeChanges")
                                               (lambda (c) (hex->octets (json:jget c "code")))))
           a))
       list))

(deftest bal-encodings-as-reference ()
  (let* ((fixture (json:parse-json
                   (clempo.io:read-file-string
                    (asdf:system-relative-pathname "clempo-rpc" "../../tests/rpc/data/bal-12290227.json"))))
         (accounts (reverse (accounts-from-json (json:jget fixture "json")))))
    (is (json:json-equal (json:jget fixture "json") (bal-json accounts)))
    (is= (json:jget fixture "raw") (octets->hex (bal-rlp accounts)))))

(deftest bal-rlp-shape ()
  ;; [[address, [[slot, [[index, value]]]], [read], [[index, balance]], [[index, nonce]], [[index, code]]]]
  (let ((a (make-bal-account 1)))
    (setf (gethash 0 (bal-account-slots a)) (list (cons 1 0))
          (gethash 2 (bal-account-slots a)) '()
          (bal-account-balance a) (list (cons 1 256))
          (bal-account-nonce a) (list (cons 2 1))
          (bal-account-code a) (list (cons 3 (octets-of #xef))))
    (is= (concatenate 'string "0xeeed94" (make-string 38 :initial-element #\0) "01"
                      "c6c580c3c20180" "c102" "c5c401820100" "c3c20201" "c4c30381ef")
         (octets->hex (bal-rlp (list a))))
    (is= "0xc0" (octets->hex (bal-rlp '())))
    (is (json:json-equal
         (json:parse-json
          "[{\"address\":\"0x0000000000000000000000000000000000000001\",
             \"storageChanges\":[{\"key\":\"0x0\",\"changes\":[{\"index\":\"0x1\",\"value\":\"0x0\"}]}],
             \"storageReads\":[\"0x2\"],
             \"balanceChanges\":[{\"index\":\"0x1\",\"value\":\"0x100\"}],
             \"nonceChanges\":[{\"index\":\"0x2\",\"value\":\"0x1\"}],
             \"codeChanges\":[{\"index\":\"0x3\",\"code\":\"0xef\"}]}]")
         (bal-json (list a))))))
