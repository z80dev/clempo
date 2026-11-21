(in-package #:clempo.state)

;;; State roots. A backend that keeps tries answers BACKEND-STATE-ROOT itself; the
;;; in-memory backend rebuilds the secure account trie and every storage trie.

(defgeneric backend-state-root (backend)
  (:documentation "The 32-octet Ethereum state root of BACKEND's committed state."))

(defun storage-root (storage)
  "Root of the secure storage trie of STORAGE (slot -> nonzero value)."
  (let ((trie (clempo.trie:make-secure-trie)))
    (maphash (lambda (slot value)
               (clempo.trie:trie-put trie (integer->octets slot 32) (clempo.rlp:rlp-encode value)))
             storage)
    (clempo.trie:trie-root trie)))

(defmethod backend-state-root ((b memory-backend))
  (let ((trie (clempo.trie:make-secure-trie)))
    (maphash (lambda (address a)
               (clempo.trie:trie-put
                trie (integer->octets address 20)
                (clempo.trie:encode-account (mem-account-nonce a) (mem-account-balance a)
                                            (storage-root (mem-account-storage a))
                                            (mem-account-code-hash a)
                                            (mem-account-commitment a))))
             (memory-backend-accounts b))
    (clempo.trie:trie-root trie)))
