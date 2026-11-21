;;; Owner: the rpc agent.
(in-package #:clempo.rpc.stubs)

;;; Peer parameters, parsed as reth parses them (recorded from the reference: corpus suite
;;; "admin"). admin_addPeer takes a NodeRecord (reth-network-peers: a URL through the url
;;; crate, then an IPv4/IPv6 host, a port, an optional discport query and the username as
;;; the 64-byte node id); the other three take an AnyNode (an enode URL, a bare
;;; "enode://<id>", or an EIP-778 "enr:" record checked down to its v4 signature).
;;; Each parser signals a string naming reth's error; the parameter appends the position.

(define-condition peer-parse-error (error) ((message :initarg :message :reader peer-parse-message)))

(defun peer-fail (fmt &rest args) (error 'peer-parse-error :message (apply #'format nil fmt args)))

(defun url-fail (fmt &rest args) (peer-fail "Failed to parse url: ~?" fmt args))

(defparameter +special-ports+
  '(("http" . 80) ("https" . 443) ("ws" . 80) ("wss" . 443) ("ftp" . 21) ("file")))

(defun url-debug (scheme base-p username host port path query)
  (flet ((opt (s) (if s (format nil "Some(~A)" (serde:rust-debug-string s)) "None")))
    (format nil "Url { scheme: ~A, cannot_be_a_base: ~:[false~;true~], username: ~A, password: None, host: ~A, port: ~A, path: ~A, query: ~A, fragment: None }"
            (serde:rust-debug-string scheme) base-p (serde:rust-debug-string username)
            (if host (format nil "Some(Domain(~A))" (serde:rust-debug-string host)) "None")
            (if port (format nil "Some(~D)" port) "None")
            (serde:rust-debug-string path) (opt query))))

(defun parse-ipv4-strict (s)
  "Rust's Ipv4Addr::from_str: four decimal octets, no leading zeros, or NIL."
  (let ((parts (split s #\.)))
    (and (= 4 (length parts))
         (every (lambda (p) (and (<= 1 (length p) 3) (every #'digit-char-p p)
                                 (or (= 1 (length p)) (char/= #\0 (char p 0)))
                                 (<= (parse-integer p) 255)))
                parts)
         (map 'octets #'parse-integer parts))))

(defun parse-ipv6 (s)
  "The url crate's IPv6 host (between brackets): 16 octets or a parse failure."
  (flet ((bad () (url-fail "invalid IPv6 address")))
    (let* ((double (search "::" s))
           (groups '()))
      (when (and double (search "::" s :start2 (1+ double))) (bad))
      (flet ((groups-of (text)
               (if (string= text "") '()
                   (let ((items (split text #\:)) (out '()))
                     (loop for (g . more) on items
                           do (cond ((and (null more) (find #\. g))
                                     (let ((v4 (parse-ipv4-strict g)))
                                       (unless v4 (bad))
                                       (push (+ (ash (aref v4 0) 8) (aref v4 1)) out)
                                       (push (+ (ash (aref v4 2) 8) (aref v4 3)) out)))
                                    ((and (<= 1 (length g) 4) (every (lambda (c) (digit-char-p c 16)) g))
                                     (push (parse-integer g :radix 16) out))
                                    (t (bad))))
                     (nreverse out)))))
        (if double
            (let ((left (groups-of (subseq s 0 double)))
                  (right (groups-of (subseq s (+ double 2)))))
              (when (> (+ (length left) (length right)) 7) (bad))
              (setf groups (append left (make-list (- 8 (length left) (length right)) :initial-element 0) right)))
            (progn (setf groups (groups-of s))
                   (unless (= 8 (length groups)) (bad))))
        (let ((o (make-octets 16)))
          (loop for g in groups for i from 0 do (setf (aref o (* 2 i)) (ash g -8) (aref o (1+ (* 2 i))) (logand g 255)))
          o)))))

(defun whatwg-ipv4 (host)
  "The WHATWG IPv4 parser for a special scheme's domain: octets, NIL when HOST does not end in
a number, or a failure."
  (let ((parts (split host #\.)))
    (when (and (rest parts) (string= "" (car (last parts)))) (setf parts (butlast parts)))
    (flet ((number-of (p)
             (cond ((string= p "") nil)
                   ((and (> (length p) 1) (char-equal #\x (char p 1)) (char= #\0 (char p 0)))
                    (if (= (length p) 2) 0
                        (and (every (lambda (c) (digit-char-p c 16)) (subseq p 2)) (parse-integer p :start 2 :radix 16))))
                   ((and (> (length p) 1) (char= #\0 (char p 0)))
                    (and (every (lambda (c) (digit-char-p c 8)) p) (parse-integer p :radix 8)))
                   (t (and (every #'digit-char-p p) (parse-integer p))))))
      (let ((last-part (car (last parts))))
        (unless (or (and (plusp (length last-part)) (every #'digit-char-p last-part))
                    (number-of last-part))
          (return-from whatwg-ipv4 nil))
        (let ((numbers (mapcar #'number-of parts)))
          (when (or (> (length numbers) 4) (some #'null numbers)
                    (some (lambda (n) (> n 255)) (butlast numbers))
                    (>= (car (last numbers)) (expt 256 (- 5 (length numbers)))))
            (url-fail "invalid IPv4 address"))
          (let ((v (car (last numbers))))
            (loop for n in (butlast numbers) for i from 0 do (incf v (* n (expt 256 (- 3 i)))))
            (integer->octets v 4)))))))

(defparameter +forbidden-host-chars+ (coerce '(#\Nul #\Tab #\Newline #\Return #\Space #\# #\/ #\: #\< #\> #\? #\@ #\[ #\\ #\] #\^ #\|) 'string))

(defun parse-url-for-node (input)
  "The parts of INPUT reth's NodeRecord reads: (values address port query username)."
  (let* ((s (remove-if (lambda (c) (member c '(#\Tab #\Newline #\Return)))
                       (string-trim (loop for i below 33 collect (code-char i)) input)))
         (colon (position #\: s)))
    (unless (and colon (plusp colon) (alpha-char-p (char s 0))
                 (every (lambda (c) (or (alphanumericp c) (find c "+-."))) (subseq s 0 colon))
                 (every (lambda (c) (< (char-code c) 128)) (subseq s 0 colon)))
      (url-fail "relative URL without a base"))
    (let* ((scheme (string-downcase (subseq s 0 colon)))
           (special (assoc scheme +special-ports+ :test #'string=))
           (rest (subseq s (1+ colon))))
      (when special (setf rest (string-left-trim "/\\" rest) rest (concatenate 'string "//" rest)))
      (unless (and (>= (length rest) 2) (string= "//" rest :end2 2))
        (let* ((q (position #\? rest)) (path (subseq rest 0 (or q (position #\# rest) (length rest)))))
          (url-fail "invalid host: ~A"
                    (url-debug scheme t "" nil nil path
                               (and q (subseq rest (1+ q) (or (position #\# rest :start q) (length rest))))))))
      (let* ((body (subseq rest 2))
             (end (or (position-if (lambda (c) (or (find c "/?#") (and special (char= c #\\)))) body) (length body)))
             (authority (subseq body 0 end))
             (tail (subseq body end))
             (at (position #\@ authority :from-end t))
             (userinfo (and at (subseq authority 0 at)))
             (username (if userinfo (subseq userinfo 0 (or (position #\: userinfo) (length userinfo))) ""))
             (hostport (if at (subseq authority (1+ at)) authority))
             (host-end (if (and (plusp (length hostport)) (char= #\[ (char hostport 0)))
                           (let ((close (position #\] hostport)))
                             (unless close (url-fail "invalid IPv6 address"))
                             (1+ close))
                           (or (position #\: hostport) (length hostport))))
             (host (subseq hostport 0 host-end))
             (port-text (and (< host-end (length hostport)) (char= #\: (char hostport host-end))
                             (subseq hostport (1+ host-end))))
             (port nil) (address nil))
        (when (and (< host-end (length hostport)) (not port-text)) (url-fail "invalid port number"))
        (when port-text
          (unless (every #'digit-char-p port-text) (url-fail "invalid port number"))
          (when (plusp (length port-text))
            (setf port (parse-integer port-text))
            (when (> port 65535) (url-fail "invalid port number"))))
        (when (string= host "")
          (when (or special at port-text) (url-fail "empty host"))
          (let ((q (position #\? tail)))
            (url-fail "invalid host: ~A"
                      (url-debug scheme nil username nil nil
                                 (subseq tail 0 (or q (position #\# tail) (length tail)))
                                 (and q (subseq tail (1+ q) (or (position #\# tail :start q) (length tail))))))))
        (when (and special port (eql port (cdr special))) (setf port nil))
        (cond ((char= #\[ (char host 0)) (setf address (parse-ipv6 (subseq host 1 (1- (length host))))))
              (t (when (find-if (lambda (c) (find c +forbidden-host-chars+)) host)
                   (url-fail "invalid domain character"))
                 (when special
                   (setf host (string-downcase host))
                   (when (find #\% host) (url-fail "invalid domain character"))
                   (setf address (whatwg-ipv4 host)))
                 (unless address
                   (setf address (or (parse-ipv4-strict host) (url-fail "invalid IPv4 address syntax"))))))
        (let ((q (position #\? tail)))
          (values address port
                  (and q (subseq tail (1+ q) (or (position #\# tail :start q) (length tail))))
                  username))))))

(defun parse-peer-id (s)
  "alloy's B512::from_str: 128 hex digits, optionally 0x-prefixed, or NIL."
  (let ((h (if (and (>= (length s) 2) (string-equal "0x" s :end2 2)) (subseq s 2) s)))
    (and (= 128 (length h)) (every (lambda (c) (digit-char-p c 16)) h) (hex->octets h))))

(defun query-value (query key)
  "The first KEY=value of an application/x-www-form-urlencoded QUERY, decoded."
  (dolist (pair (split query #\&))
    (let* ((eq (position #\= pair))
           (k (form-decode (subseq pair 0 (or eq (length pair))))))
      (when (and (plusp (length pair)) (string= k key))
        (return (form-decode (if eq (subseq pair (1+ eq)) "")))))))

(defun form-decode (s)
  (let ((o (make-array 0 :element-type 'octet :adjustable t :fill-pointer 0)))
    (loop with i = 0 while (< i (length s))
          do (let ((c (char s i)))
               (cond ((char= c #\+) (vector-push-extend 32 o) (incf i))
                     ((and (char= c #\%) (<= (+ i 3) (length s))
                           (digit-char-p (char s (1+ i)) 16) (digit-char-p (char s (+ i 2)) 16))
                      (vector-push-extend (parse-integer s :start (1+ i) :end (+ i 3) :radix 16) o) (incf i 3))
                     (t (loop for b across (string->octets (string c)) do (vector-push-extend b o)) (incf i)))))
    (octets->string (coerce o 'octets))))

(defun parse-u16 (s)
  "Rust's u16::from_str."
  (let ((digits (if (and (plusp (length s)) (char= #\+ (char s 0))) (subseq s 1) s)))
    (cond ((string= s "") (peer-fail "cannot parse integer from empty string"))
          ((or (string= digits "") (notevery #'digit-char-p digits)) (peer-fail "invalid digit found in string"))
          ((> (parse-integer digits) 65535) (peer-fail "number too large to fit in target type"))
          (t (parse-integer digits)))))

(defun parse-node-record (s)
  "reth's NodeRecord::from_str: (values id address tcp-port udp-port)."
  (multiple-value-bind (address port query username) (parse-url-for-node s)
    (unless port (url-fail "no port specified"))
    (let* ((disc (and query (query-value query "discport")))
           (udp (if disc
                    (handler-case (parse-u16 disc)
                      (peer-parse-error (e) (peer-fail "Failed to discport query: ~A" (peer-parse-message e))))
                    port))
           (id (or (parse-peer-id username) (peer-fail "Failed to parse id"))))
      (values id address port udp))))

;;; EIP-778 records

(defun base64url-decode (s)
  "base64's URL_SAFE_NO_PAD decode, with its error messages."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
    (let ((values (loop for c across s for i from 0
                        collect (or (position c alphabet)
                                    (peer-fail "Invalid base64 encoding: Invalid symbol ~D, offset ~D."
                                               (char-code c) i)))))
      (when (= 1 (mod (length s) 4)) (peer-fail "Invalid base64 encoding: Invalid input length: ~D" (length s)))
      (let ((left (mod (length s) 4)) (last-value (car (last values))))
        (when (and (plusp left)
                   (plusp (logand last-value (if (= left 2) #b1111 #b11))))
          (let ((c (char s (1- (length s)))))
            (peer-fail "Invalid base64 encoding: Invalid last symbol 0x~(~2,'0x~) ('~C') at offset ~D, decoded as 0b~8,'0B."
                       (char-code c) c (1- (length s)) last-value))))
      (let ((bits 0) (n 0) (out '()))
        (dolist (v values)
          (setf bits (logior (ash bits 6) v)) (incf n 6)
          (when (>= n 8) (decf n 8) (push (ldb (byte 8 n) bits) out) (setf bits (ldb (byte n 0) bits))))
        (coerce (nreverse out) 'octets)))))

(defun enr-fail (what) (peer-fail "Invalid ENR: ~A" what))

(defun rlp-header (data pos end)
  "alloy-rlp's Header::decode at POS: (values list-p payload-start payload-length)."
  (when (>= pos end) (enr-fail "InputTooShort"))
  (let ((b (aref data pos)))
    (cond ((< b #x80) (values nil pos 1))
          ((<= b #xb7)
           (let ((len (- b #x80)))
             (when (= len 1)
               (when (>= (1+ pos) end) (enr-fail "InputTooShort"))
               (when (< (aref data (1+ pos)) #x80) (enr-fail "NonCanonicalSingleByte")))
             (values nil (1+ pos) len)))
          ((or (<= #xb8 b #xbf) (>= b #xf8))
           (let ((lol (- b (if (>= b #xf8) #xf7 #xb7))))
             (when (> (+ pos 1 lol) end) (enr-fail "InputTooShort"))
             (when (zerop (aref data (1+ pos))) (enr-fail "LeadingZero"))
             (let ((len (octets->integer data :start (1+ pos) :end (+ pos 1 lol))))
               (when (< len 56) (enr-fail "NonCanonicalSize"))
               (values (>= b #xf8) (+ pos 1 lol) len))))
          (t (values t (1+ pos) (- b #xc0))))))

(defun rlp-item (data pos end want-list)
  "Decode_bytes: (values payload-start payload-end next)."
  (multiple-value-bind (list-p start len) (rlp-header data pos end)
    (unless (eq list-p want-list) (enr-fail (if list-p "UnexpectedList" "UnexpectedString")))
    (when (> (+ start len) end) (enr-fail "InputTooShort"))
    (values start (+ start len) (+ start len))))

(defun rlp-raw-item (data pos end)
  "Any item: (values start-of-item next)."
  (multiple-value-bind (list-p start len) (rlp-header data pos end)
    (declare (ignore list-p))
    (when (> (+ start len) end) (enr-fail "InputTooShort"))
    (values pos (+ start len))))

(defun parse-enr (text)
  "The enr crate's Enr::from_str: a v4 record whose signature verifies, or a failure."
  (let* ((data (base64url-decode (subseq text 4))) (end (length data)))
    (multiple-value-bind (pos stop) (rlp-item data 0 end t)
      (when (= pos stop) (enr-fail "Custom(\"Payload is empty\")"))
      (multiple-value-bind (sig-start sig-end after-sig) (rlp-item data pos stop nil)
        (when (= after-sig stop) (enr-fail "Custom(\"Seq is missing\")"))
        (multiple-value-bind (seq-start seq-end next) (rlp-item data after-sig stop nil)
          (when (> (- seq-end seq-start) 8) (enr-fail "Overflow"))
          (when (and (< seq-start seq-end) (zerop (aref data seq-start))) (enr-fail "LeadingZero"))
          (let ((pairs '()) (prev nil))
            (loop while (< next stop)
                  do (multiple-value-bind (k-start k-end after) (rlp-item data next stop nil)
                       (let ((key (subseq data k-start k-end)))
                         (when (and prev (not (octets< prev key))) (enr-fail "Custom(\"Unsorted keys\")"))
                         (multiple-value-bind (v-at v-next) (rlp-raw-item data after stop)
                           (when (octets= key (string->octets "id"))
                             (multiple-value-bind (s e) (rlp-item data v-at stop nil)
                               (unless (octets= (subseq data s e) (string->octets "v4"))
                                 (enr-fail "Custom(\"Unsupported identity scheme\")"))))
                           (push (cons key (subseq data v-at v-next)) pairs)
                           (setf prev key next v-next)))))
            (let ((id (assoc "id" pairs :key #'octets->string :test #'string=))
                  (key (cdr (assoc "secp256k1" pairs :key #'octets->string :test #'string=))))
              (unless (and id key) (enr-fail "Custom(\"Unknown signature\")"))
              (let ((content (subseq data (nth-value 2 (rlp-item data pos stop nil)) stop)))
                (unless (enr-signature-valid-p (subseq data sig-start sig-end) key content)
                  (enr-fail "Custom(\"Invalid Secp256k1 Signature\")"))))))))))

(defun octets< (a b)
  (let ((m (mismatch a b)))
    (cond ((null m) nil) ((= m (length a)) t) ((= m (length b)) nil) (t (< (aref a m) (aref b m))))))

(defun enr-signature-valid-p (signature key-item content)
  "The v4 scheme: SIGNATURE (r || s, low s) over keccak256(rlp(content)) by the compressed
secp256k1 key KEY-ITEM (an RLP string)."
  (let* ((key (handler-case (multiple-value-bind (s e) (rlp-item key-item 0 (length key-item) nil)
                              (subseq key-item s e))
                (peer-parse-error () nil)))
         (list (let ((len (length content)))
                 (concatenate 'octets
                              (if (< len 56)
                                  (octets-of (+ #xc0 len))
                                  (let ((l (integer->octets-minimal len)))
                                    (concatenate 'octets (octets-of (+ #xf7 (length l))) l)))
                              content))))
    (and key (= 33 (length key)) (member (aref key 0) '(2 3)) (= 64 (length signature))
         (let ((r (octets->integer signature :start 0 :end 32))
               (s (octets->integer signature :start 32 :end 64))
               (hash (clempo.keccak:keccak256 list)))
           (and (< 0 r clempo.secp256k1:+secp256k1-n+) (< 0 s) (<= s clempo.secp256k1:+secp256k1-half-n+)
                (loop for recid below 2
                      thereis (let ((public (clempo.secp256k1:ecrecover hash r s recid)))
                                (and public
                                     (octets= key (octets-concat
                                                   (octets-of (if (evenp (octets->integer public :start 32 :end 64)) 2 3))
                                                   (subseq public 0 32)))))))))))

(defun parse-any-node (s)
  "reth's AnyNode::from_str."
  (cond ((and (>= (length s) 8) (string= "enode://" s :end2 8))
         (let ((rem (subseq s 8)))
           (or (ignore-errors (parse-node-record s) t)
               (parse-peer-id rem)
               (peer-fail "invalid public key: ~A" rem))))
        ((and (>= (length s) 4) (string= "enr:" s :end2 4)) (parse-enr s) t)
        (t (peer-fail "missing 'enr:' prefix for base64-encoded record"))))

(defun read-peer-string (src parser)
  (let ((s (if (eq (serde:peek-kind src) :string) (serde:read-string src) (serde:invalid-type src "a string"))))
    (handler-case (progn (funcall parser s) s)
      (peer-parse-error (e) (serde:serde-fail-here src "~A" (peer-parse-message e))))))

(defparam enode-url (src) "reth NodeRecord: enode://<id>@<ip>:<port>[?discport=N]"
  (read-peer-string src #'parse-node-record))

(defparam any-node (src) "reth AnyNode: an enode URL, enode://<id>, or an enr: record"
  (read-peer-string src #'parse-any-node))

;;; The admin peer methods. clempo has no devp2p peer manager to add peers to or remove
;;; them from (DEVIATIONS.md 12): a parsed peer is acknowledged as reth acknowledges it.

(defrpc "admin_addPeer" ((enode enode-url)) "acknowledged; no peer manager (DEVIATIONS 12)" :true)
(defrpc "admin_addTrustedPeer" ((node any-node)) "acknowledged; no peer manager (DEVIATIONS 12)" :true)
(defrpc "admin_removePeer" ((node any-node)) "acknowledged; no peer manager (DEVIATIONS 12)" :true)
(defrpc "admin_removeTrustedPeer" ((node any-node)) "acknowledged; no peer manager (DEVIATIONS 12)" :true)
