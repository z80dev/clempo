(defpackage #:clempo.websocket
  (:use #:cl #:clempo.base)
  (:documentation "RFC 6455 framing for the JSON-RPC server: the handshake accept key (SHA-1 and
base64, used for nothing else) and reading and writing frames on a binary stream.")
  (:export #:accept-key #:read-message #:write-frame #:sha1 #:base64-encode
           #:+op-text+ #:+op-binary+ #:+op-close+ #:+op-ping+ #:+op-pong+))

(in-package #:clempo.websocket)

(defconstant +op-continuation+ 0)
(defconstant +op-text+ 1)
(defconstant +op-binary+ 2)
(defconstant +op-close+ 8)
(defconstant +op-ping+ 9)
(defconstant +op-pong+ 10)

(defun sha1 (message)
  "SHA-1 (FIPS 180-4) of an octet vector; only the WebSocket handshake uses it."
  (declare (type octets message))
  (let* ((ml (length message))
         (total (* 64 (ceiling (+ ml 9) 64)))
         (m (make-octets total))
         (w (make-array 80 :element-type '(unsigned-byte 32)))
         (h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE) (h3 #x10325476) (h4 #xC3D2E1F0))
    (declare (type (unsigned-byte 32) h0 h1 h2 h3 h4))
    (replace m message)
    (setf (aref m ml) #x80)
    (loop for i from 0 below 8
          do (setf (aref m (- total 1 i)) (ldb (byte 8 (* 8 i)) (* 8 ml))))
    (flet ((rotl (x n) (logand #xFFFFFFFF (logior (ash x n) (ash x (- n 32))))))
      (loop for chunk from 0 below total by 64
            do (dotimes (i 16)
                 (setf (aref w i) (octets->integer m :start (+ chunk (* 4 i)) :end (+ chunk (* 4 i) 4))))
               (loop for i from 16 below 80
                     do (setf (aref w i) (rotl (logxor (aref w (- i 3)) (aref w (- i 8))
                                                       (aref w (- i 14)) (aref w (- i 16)))
                                               1)))
               (let ((a h0) (b h1) (c h2) (d h3) (e h4))
                 (dotimes (i 80)
                   (multiple-value-bind (f k)
                       (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xFFFFFFFF) d)) #x5A827999))
                             ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                             ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
                             (t (values (logxor b c d) #xCA62C1D6)))
                     (let ((temp (logand #xFFFFFFFF (+ (rotl a 5) f e k (aref w i)))))
                       (setf e d d c c (rotl b 30) b a a temp))))
                 (setf h0 (logand #xFFFFFFFF (+ h0 a)) h1 (logand #xFFFFFFFF (+ h1 b))
                       h2 (logand #xFFFFFFFF (+ h2 c)) h3 (logand #xFFFFFFFF (+ h3 d))
                       h4 (logand #xFFFFFFFF (+ h4 e))))))
    (octets-concat (integer->octets h0 4) (integer->octets h1 4) (integer->octets h2 4)
                   (integer->octets h3 4) (integer->octets h4 4))))

(defun base64-encode (o)
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (loop for i from 0 below (length o) by 3
            do (let* ((n (min 3 (- (length o) i)))
                      (v (ash (octets->integer o :start i :end (+ i n)) (* 8 (- 3 n)))))
                 (dotimes (j 4)
                   (write-char (if (<= j n) (char alphabet (ldb (byte 6 (* 6 (- 3 j))) v)) #\=) out)))))))

(defun accept-key (client-key)
  (base64-encode (sha1 (string->octets (concatenate 'string client-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))))

(defun read-u (stream n)
  (let ((v 0)) (dotimes (i n v) (setf v (logior (ash v 8) (read-byte stream))))))

(defun read-frame (stream)
  "Returns (values fin opcode payload)."
  (let* ((b0 (read-byte stream)) (b1 (read-byte stream))
         (fin (logbitp 7 b0)) (opcode (logand b0 15))
         (masked (logbitp 7 b1)) (len (logand b1 127)))
    (setf len (case len (126 (read-u stream 2)) (127 (read-u stream 8)) (t len)))
    (invariant (< len (ash 1 28)) "websocket frame too large")
    (let ((mask (when masked (let ((m (make-octets 4))) (read-sequence m stream) m)))
          (payload (make-octets len)))
      (read-sequence payload stream)
      (when mask
        (dotimes (i len) (setf (aref payload i) (logxor (aref payload i) (aref mask (mod i 4))))))
      (values fin opcode payload))))

(defun read-message (stream)
  "The next data or control message: (values opcode payload), reassembling fragments.
Returns NIL at end of stream."
  (handler-case
      (let ((parts '()) (first-op nil))
        (loop
          (multiple-value-bind (fin opcode payload) (read-frame stream)
            (cond ((>= opcode 8) (return (values opcode payload)))
                  (t (unless first-op (setf first-op opcode))
                     (push payload parts)
                     (when fin (return (values first-op (apply #'octets-concat (nreverse parts))))))))))
    (end-of-file () nil)))

(defun write-frame (stream opcode payload)
  (let ((len (length payload)))
    (write-byte (logior #x80 opcode) stream)
    (cond ((< len 126) (write-byte len stream))
          ((< len 65536) (write-byte 126 stream) (write-sequence (integer->octets len 2) stream))
          (t (write-byte 127 stream) (write-sequence (integer->octets len 8) stream)))
    (write-sequence payload stream)
    (force-output stream)))
