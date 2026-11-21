(defpackage #:clempo.digest.tests
  (:use #:cl #:clempo.base #:clempo.ripemd160 #:clempo.blake2f #:clempo.blake3))
(in-package #:clempo.digest.tests)

(deftest ripemd-160-reference ()
  ;; https://homes.esat.kuleuven.be/~bosselae/ripemd160.html
  (is-equalp (hex->octets "9c1185a5c5e9fc54612808977ee8f548b2258d31") (ripemd160 (make-octets 0)))
  (is-equalp (hex->octets "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc") (ripemd160 (string->octets "abc")))
  (is-equalp (hex->octets "52783243c1697bdbe16d37f97f68f08325dc1528") (ripemd160 (fill (make-octets 1000000) 97))))

(deftest eip-152-vector-5 ()
  (let ((input (make-octets 213)))
    (setf (aref input 3) 12 (aref input 196) 3 (aref input 212) 1)
    (replace input (hex->octets "48c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b616263") :start1 4)
    (is-equalp (hex->octets "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923")
               (blake2f-precompile input))
    (setf (aref input 212) 2)
    (is (null (blake2f-precompile input)))
    (is (null (blake2f-precompile (make-octets 212))))))

(deftest blake3-official-vectors ()
  ;; https://github.com/BLAKE3-team/BLAKE3/blob/master/test_vectors/test_vectors.json
  (is-equalp (hex->octets "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262") (blake3 (make-octets 0)))
  (let ((input (make-octets 3073)))
    (dotimes (i (length input)) (setf (aref input i) (mod i 251)))
    (is-equalp (hex->octets "7124b49501012f81cc7f11ca069ec9226cecb8a2c850cfe644e327d22d3e1cd39a27ae3b79d68d89da9bf25bc27139ae65a324918a5f9b7828181e52cf373c84f35b639b7fccbb985b6f2fa56aea0c18f531203497b8bbd3a07ceb5926f1cab74d14bd66486d9a91eba99059a98bd1cd25876b2af5a76c3e9eed554ed72ea952b603bf")
               (blake3 input :length 131))
    (is-equalp (hex->octets "68dede9bef00ba89e43f31a6825f4cf433389fedae75c04ee9f0cf16a427c95a")
               (blake3-keyed (string->octets "whats the Elvish word for friend") input))
    (is-equalp (hex->octets "72613c9ec9ff7e40f8f5c173784c532ad852e827dba2bf85b2ab4b76f7079081")
               (blake3-derive-key "BLAKE3 2019-12-27 16:29:52 test vectors context" input))))
