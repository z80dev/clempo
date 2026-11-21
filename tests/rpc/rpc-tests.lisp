(defpackage #:clempo.rpc.tests
  (:use #:cl #:clempo.base))
(in-package #:clempo.rpc.tests)

(deftest sha1-vectors ()
  (is= "a9993e364706816aba3e25717850c26c9cd0d89d"
       (octets->hex (clempo.websocket:sha1 (string->octets "abc")) :prefix nil))
  (is= "da39a3ee5e6b4b0d3255bfef95601890afd80709"
       (octets->hex (clempo.websocket:sha1 (make-octets 0)) :prefix nil))
  (is= "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
       (octets->hex (clempo.websocket:sha1 (string->octets "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")) :prefix nil))
  ;; RFC 6455 section 1.3
  (is= "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" (clempo.websocket:accept-key "dGhlIHNhbXBsZSBub25jZQ==")))
