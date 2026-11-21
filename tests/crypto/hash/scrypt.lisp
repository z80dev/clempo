(defpackage #:clempo.scrypt.tests (:use #:cl #:clempo.base #:clempo.scrypt))
(in-package #:clempo.scrypt.tests)

;;; Vectors from RFC 7914 sections 8, 11 and 12.

(deftest salsa20/8-rfc-7914 ()
  (is-equalp (hex->octets "a41f859c6608cc993b81cacb020cef05044b2181a2fd337dfd7b1c6396682f29b4393168e3c9e6bcfe6bc5b7a06d96bae424cc102c91745c24ad673dc7618f81")
             (salsa20/8 (hex->octets "7e879a214f3ec9867ca940e641718f26baee555b8c61c1b50df846116dcd3b1dee24f319df9b3d8514121e4b5ac5aa3276021d2909c74829edebc68db8b8c25e"))))

(deftest pbkdf2-hmac-sha256-rfc-7914 ()
  (is-equalp (hex->octets "55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783")
             (pbkdf2-hmac-sha256 (string->octets "passwd") (string->octets "salt") 1 64))
  (is-equalp (hex->octets "4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d")
             (pbkdf2-hmac-sha256 (string->octets "Password") (string->octets "NaCl") 80000 64)))

(deftest scrypt-rfc-7914 ()
  (is-equalp (hex->octets "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906")
             (scrypt (make-octets 0) (make-octets 0) 16 1 1 64))
  (is-equalp (hex->octets "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640")
             (scrypt (string->octets "password") (string->octets "NaCl") 1024 8 16 64))
  (is-equalp (hex->octets "7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887")
             (scrypt (string->octets "pleaseletmein") (string->octets "SodiumChloride") 16384 8 1 64))
  (signals error (scrypt (make-octets 0) (make-octets 0) 15 1 1 32)))
