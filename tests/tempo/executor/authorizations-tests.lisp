(defpackage #:clempo.executor.authorizations-tests
  (:use #:cl #:clempo.base)
  (:local-nicknames (#:state #:clempo.state) (#:evm #:clempo.evm) (#:x #:clempo.executor)
                    (#:secp #:clempo.secp256k1) (#:rlp #:clempo.rlp)))

(in-package #:clempo.executor.authorizations-tests)

;;; Hand-built worlds for tempo crates/evm/src/handler.rs apply_authorization_list and evm2's
;;; eip7702.rs apply_auth_list (see src/tempo/executor/authorizations.lisp).

(defparameter *chain-id* 4217)
(defparameter *key* (hex->octets "4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318"))
(defparameter *authority* (secp:pubkey->address (secp:private->public *key*)))
(defparameter *target* #xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)

(defun int-octets (n)
  (if (zerop n) (make-octets 0) (integer->octets n (ceiling (integer-length n) 8))))

(defun sign-authorization (chain-id address nonce &key (key *key*))
  "(values r s recid) over keccak(0x05 || rlp([chain_id, address, nonce]))."
  (secp:sign (clempo.keccak:keccak256
              (octets-concat (octets-of 5)
                             (rlp:rlp-encode (list chain-id (integer->octets address 20) nonce))))
             key))

(defun eth-auth (chain-id address nonce &key (key *key*))
  "A type-4 authorization item as decoded RLP: (chain-id address nonce y-parity r s)."
  (multiple-value-bind (r s recid) (sign-authorization chain-id address nonce :key key)
    (list (int-octets chain-id) (integer->octets address 20) (int-octets nonce)
          (int-octets recid) (int-octets r) (int-octets s))))

(defun secp-signature (chain-id address nonce &key (key *key*))
  (multiple-value-bind (r s recid) (sign-authorization chain-id address nonce :key key)
    (octets-concat (integer->octets r 32) (integer->octets s 32) (octets-of recid))))

(defun tempo-auth (chain-id address nonce &key (key *key*) keychain-user)
  "A 0x76 authorization item: (chain-id address nonce signature). KEYCHAIN-USER wraps the
secp256k1 signature in a V1 keychain envelope for that root account."
  (let ((sig (secp-signature chain-id address nonce :key key)))
    (list (int-octets chain-id) (integer->octets address 20) (int-octets nonce)
          (if keychain-user
              (octets-concat (octets-of 3) (integer->octets keychain-user 20) sig)
              sig))))

(defmacro with-world ((world &key (fork :t0) (nonce 0) (code nil) (exists t)) &body body)
  "Run BODY with EVM:*WORLD* bound to a fresh world holding the authority account."
  (let ((backend (gensym)))
    `(let* ((,backend (state:make-memory-backend))
            (,world (state:make-world ,backend))
            (evm:*world* ,world)
            (evm:*spec* (x::tempo-evm-spec ,fork)))
       (declare (ignorable ,world))
       (when ,exists
         (state:memory-backend-put-account ,backend *authority* :nonce ,nonce :balance 1
                                           :code (or ,code state:+empty-code+)))
       ,@body)))

(defun authority-code () (state:get-code evm:*world* *authority*))
(defun authority-nonce () (state:get-nonce evm:*world* *authority*))

(deftest valid-delegation-sets-code-and-bumps-nonce ()
  (with-world (w :nonce 5)
    (is= 12500 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 5)) *chain-id*))
    (is-equalp (state:delegation-designator *target*) (authority-code))
    (is= 6 (authority-nonce))
    (is (state:address-warm-p w *authority*))))

(deftest chain-id-zero-applies-on-any-chain ()
  (with-world (w)
    (x::apply-ethereum-authorizations (list (eth-auth 0 *target* 0)) *chain-id*)
    (is= *target* (state:delegation-target (authority-code)))))

(deftest wrong-chain-id-is-skipped-without-warming ()
  (with-world (w)
    (is= 0 (x::apply-ethereum-authorizations (list (eth-auth 1 *target* 0)) *chain-id*))
    (is= 0 (length (authority-code)))
    (is= 0 (authority-nonce))
    (is (not (state:address-warm-p w *authority*)))))

(deftest wrong-nonce-is-skipped-but-warms ()
  (with-world (w :nonce 3)
    (is= 0 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 2)) *chain-id*))
    (is= 0 (length (authority-code)))
    (is= 3 (authority-nonce))
    (is (state:address-warm-p w *authority*))))

(deftest max-nonce-is-skipped ()
  (with-world (w)
    (is= 0 (x::apply-ethereum-authorizations
            (list (eth-auth *chain-id* *target* #xffffffffffffffff)) *chain-id*))
    (is (not (state:address-warm-p w *authority*)))))

(deftest non-delegation-code-is-skipped ()
  (let ((code (octets-of #x60 0 #x60 0 #xf3)))
    (with-world (w :code code)
      (is= 0 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 0)) *chain-id*))
      (is-equalp code (authority-code))
      (is= 0 (authority-nonce)))))

(deftest zero-address-clears-an-existing-delegation ()
  (with-world (w :nonce 1 :code (state:delegation-designator #x1234))
    (x::apply-ethereum-authorizations (list (eth-auth *chain-id* 0 1)) *chain-id*)
    (is= 0 (length (authority-code)))
    (is= 2 (authority-nonce))))

(deftest redelegation-over-an-existing-delegation ()
  (with-world (w :code (state:delegation-designator #x1234))
    (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 0)) *chain-id*)
    (is= *target* (state:delegation-target (authority-code)))))

(deftest later-authorizations-see-earlier-ones ()
  ;; two for the same authority: nonce 0 then nonce 1, both apply
  (with-world (w)
    (is= 25000 (x::apply-ethereum-authorizations
                (list (eth-auth *chain-id* #x1234 0) (eth-auth *chain-id* *target* 1)) *chain-id*))
    (is= *target* (state:delegation-target (authority-code)))
    (is= 2 (authority-nonce))))

(deftest refund-requires-an-existing-or-touched-authority ()
  (with-world (w :exists nil)
    (is= 0 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 0)) *chain-id*))
    (is= 1 (authority-nonce)))
  ;; the sender authorizing itself was touched by its nonce bump before the list
  (with-world (w :exists nil)
    (state:touch-account w (state:get-account w *authority*))
    (is= 12500 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 0)) *chain-id*))))

(deftest refund-is-zero-from-t1 ()
  (with-world (w :fork :t1)
    (is= 0 (x::apply-ethereum-authorizations (list (eth-auth *chain-id* *target* 0)) *chain-id*))
    (is= *target* (state:delegation-target (authority-code))))
  (with-world (w :fork :t1)
    (is= 0 (x::apply-tempo-authorizations (list (tempo-auth *chain-id* *target* 0)) *chain-id* :t1))))

(deftest invalid-signature-is-skipped ()
  (with-world (w)
    (let ((auth (eth-auth *chain-id* *target* 0)))
      (setf (fourth auth) (int-octets 2))            ; parity 2
      (is= 0 (x::apply-ethereum-authorizations (list auth) *chain-id*))
      (is= 0 (authority-nonce)))))

(deftest tempo-authorization-applies-and-refunds-pre-t1 ()
  (with-world (w :nonce 7)
    (is= 12500 (x::apply-tempo-authorizations (list (tempo-auth *chain-id* *target* 7)) *chain-id* :t0))
    (is= *target* (state:delegation-target (authority-code)))
    (is= 8 (authority-nonce))))

(deftest tempo-skips-wrong-chain-nonce-and-code ()
  (with-world (w :nonce 1)
    (is= 0 (x::apply-tempo-authorizations
            (list (tempo-auth 9 *target* 1) (tempo-auth *chain-id* *target* 0)) *chain-id* :t0))
    (is= 1 (authority-nonce)))
  (with-world (w :code (octets-of 0))
    (is= 0 (x::apply-tempo-authorizations (list (tempo-auth *chain-id* *target* 0)) *chain-id* :t0))
    (is-equalp (octets-of 0) (authority-code))))

(deftest tempo-skips-keychain-authorizations-from-t0 ()
  ;; a V1 keychain envelope recovers the wrapped root account (here the signer itself)
  (let ((auth (tempo-auth *chain-id* *target* 0 :keychain-user *authority*)))
    (with-world (w :fork :t0)
      (is= 0 (x::apply-tempo-authorizations (list auth) *chain-id* :t0))
      (is= 0 (authority-nonce))
      (is (not (state:address-warm-p w *authority*))))
    (with-world (w :fork :genesis)
      (is= 12500 (x::apply-tempo-authorizations (list auth) *chain-id* :genesis))
      (is= *target* (state:delegation-target (authority-code))))))

;;; Mainnet type-4 transactions (data/README.md): the sender authorizes itself at its bumped
;;; nonce and a second account re-delegates; the authorities' post nonce and code match.

(defparameter *type4-fixture*
  (asdf:system-relative-pathname "clempo-tempo-executor"
                                 "../../../tests/tempo/executor/data/type4-authorizations.sexp"))

(deftest mainnet-type4-authorizations-match-prestate-diff ()
  (with-open-file (in *type4-fixture*)
    (loop for tx = (read in nil) while tx do
      (destructuring-bind (&key hash chain-id sender authorizations accounts &allow-other-keys) tx
        (let* ((backend (state:make-memory-backend))
               (world (state:make-world backend))
               (evm:*world* world)
               (evm:*spec* (x::tempo-evm-spec :t1))
               (sender (hex->integer sender)))
          (loop for (address . fields) in accounts
                do (state:memory-backend-put-account backend (hex->integer address)
                                                     :nonce (getf fields :pre-nonce) :balance 1
                                                     :code (hex->octets (getf fields :pre-code))))
          (state:increment-nonce world sender)        ; the sender's nonce bump comes first
          (x::apply-ethereum-authorizations
           (loop for (c a n y r s) in authorizations
                 collect (list (int-octets (hex->integer c)) (hex->octets a)
                               (int-octets (hex->integer n)) (int-octets (hex->integer y))
                               (int-octets (hex->integer r)) (int-octets (hex->integer s))))
           (hex->integer chain-id))
          (loop for (address . fields) in accounts
                for a = (hex->integer address)
                do (is= (getf fields :post-nonce) (state:get-nonce world a)
                        "~A nonce of ~A" hash address)
                   (is-equalp (hex->octets (getf fields :post-code)) (state:get-code world a)
                              "~A code of ~A" hash address)))))))
