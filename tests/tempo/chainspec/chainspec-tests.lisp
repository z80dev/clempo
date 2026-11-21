(defpackage #:clempo.chainspec.tests
  (:use #:cl #:clempo.base #:clempo.chainspec))

(in-package #:clempo.chainspec.tests)

(deftest genesis-roots-and-hashes ()
  ;; eth_getBlockByNumber("0x0", false) on rpc.tempo.xyz and rpc.moderato.tempo.xyz.
  (dolist (chain '((:mainnet 4217 14
                   "0x83408974323f63ab969b23f0fe1dba30d7ee5dc5c524a975bae38187eaa2c7f6"
                   "0x49d7ec7085e77bf5a403d0fcb4cfc42a4084a89dfff60477579c5e09c9e03c54")
                  (:moderato 42431 16
                   "0x3952ff0c60a3f215ab11a3a2c7698126b7911da84faf5cb66bdbcb0c605738c4"
                   "0xba49153e6f3996a340f3fe3d69e7c0826b717ae672cb115cf0527cada0b78e9e")))
    (destructuring-bind (name id accounts expected-root expected-hash) chain
      (let* ((spec (load-chainspec name)) (count 0)
             (trie (genesis-state-trie spec))
             (root (clempo.trie:trie-root trie)))
        (is= id (chain-id spec))
        (is (eq spec (load-chainspec id)))
        (is= 21600 (chain-epoch-length spec))
        (is= 500000000 (genesis-gas-limit spec))
        (is= 10000000000 (genesis-base-fee spec))
        (is= 0 (genesis-timestamp spec))
        (map-genesis-accounts
         (lambda (address nonce balance code storage)
           (declare (ignore storage))
           (incf count)
           (multiple-value-bind (leaf-nonce leaf-balance storage-root code-hash)
               (clempo.trie:decode-account (clempo.trie:trie-get trie (integer->octets address 20)))
             (declare (ignore storage-root))
             (is= nonce leaf-nonce)
             (is= balance leaf-balance)
             (is-equalp (clempo.keccak:keccak256 code) code-hash)))
         spec)
        (is= accounts count)
        (is-equalp (hex->octets expected-root) root)
        (is-equalp (hex->octets expected-hash) (genesis-block-hash spec :state-root root))))))

(deftest tempo-fork-boundaries ()
  (is (eq :t0 (fork-at 0 :mainnet)))
  (is= 1770908400 (fork-activation :t1 :mainnet))
  (is (eq :t0 (fork-at 1770908399 :mainnet)))
  (is (eq :t1a (fork-at 1770908400 :mainnet)))
  (is (eq :t13 (fork-at 1796741999 :mainnet)))
  (is (eq :t14 (fork-at 1796742000 :mainnet)))
  (is (null (fork-activation :t15 :mainnet)))
  (is (eq :genesis (fork-at 0 :moderato)))
  (is (eq :t1 (fork-at 1770303600 :moderato)))
  (is (eq :t1b (fork-at 1771858800 :moderato)))
  (is (eq :t13 (fork-at 1795532399 :moderato)))
  (is (eq :t14 (fork-at 1795532400 :moderato)))
  (is (not (fork-active-p :t15 2000000000 :moderato))))

(deftest tempo-gas-parameters ()
  (is= 50000000 (shared-gas-limit-at 0 :mainnet 500000000))
  (is= 225000000 (general-gas-limit-at 0 :mainnet 500000000))
  (is= 30000000 (general-gas-limit-at 1770908400 :mainnet 500000000))
  (is= 0 (shared-gas-limit-at 1779112800 :mainnet 500000000))
  (is= 16777216 (tx-gas-limit-cap-at 1770908399 :mainnet))
  (is= 30000000 (tx-gas-limit-cap-at 1770908400 :mainnet))
  (is= 10000000000 (next-block-base-fee 0 :mainnet 1 0))
  (is= 20000000000 (next-block-base-fee 1783605599 :mainnet 10000000000 0))
  (is= 12000000000 (next-block-base-fee 1783605600 :mainnet 20000000000 0))
  (is= 10500000000 (next-block-base-fee 1783605600 :mainnet 12000000000 0))
  (is= 600000007 (next-block-base-fee 1783605600 :mainnet 600000000 10000001))
  (is= 600000000 (next-block-base-fee 1783605600 :mainnet 600000000 0)))

(deftest custom-genesis-file ()
  (let ((path (format nil "/tmp/clempo-genesis-~A.json"
                      (octets->hex (clempo.io:random-octets 8)))))
    (unwind-protect
         (progn
           (clempo.io:write-file-octets
            path (string->octets
                  "{\"config\":{\"chainId\":31337,\"epochLength\":60,\"generalGasLimit\":913,
                      \"londonBlock\":0,\"shanghaiTime\":0,\"cancunTime\":0,\"pragueTime\":0,
                      \"t13Time\":0,\"t14Time\":5,\"t15Time\":null},
                    \"timestamp\":\"0x0\",\"extraData\":\"0xaabbcc\",\"gasLimit\":\"0x1dcd6500\",
                    \"alloc\":{\"0x0000000000000000000000000000000000000001\":{\"balance\":\"0x1\"}}}"))
           (let* ((chain (load-chainspec (pathname path)))
                  (root (genesis-state-root chain))
                  (inner (fourth (genesis-header chain :state-root root))))
             (is= 31337 (chain-id chain))
             (is= 60 (chain-epoch-length chain))
             (is= 913 (general-gas-limit-at 0 chain 500000000))
             (is= 0 (shared-gas-limit-at 0 chain 500000000))
             (is (eq :t13 (fork-at 0 chain)))
             (is (eq :t14 (fork-at 5 chain)))
             (is (null (fork-activation :t15 chain)))
             (is= 21 (length inner))
             (is= 1000000000 (genesis-base-fee chain))
             (is-equalp (octets-of #xaa #xbb #xcc) (genesis-extra-data chain))
             (is-equalp root (fourth inner))
             (is-equalp (genesis-block-hash chain :state-root root) (genesis-block-hash path)))
           (clempo.io:write-file-octets
            path (string->octets "{\"config\":{\"chainId\":31337},\"alloc\":{}}"))
           (let ((chain (load-chainspec path)))
             (is (null (genesis-base-fee chain)))
             (is= 15 (length (fourth (genesis-header chain))))))
      (when (probe-file path) (delete-file path)))))
