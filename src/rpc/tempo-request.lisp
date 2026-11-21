(in-package #:clempo.rpc.calls)

;;; The nested Tempo fields of TempoTransactionRequest (tempo crates/alloy/src/rpc/request.rs):
;;; aaAuthorizationList (Vec<TempoSignedAuthorization>), keyAuthorization
;;; (SignedKeyAuthorization), configurableAccountSimulation and keyAuthorizationSimulation
;;; (ConfigurableAccountSimulationSpec) and feePayerSignature (alloy Signature), read with
;;; serde's exact errors. Signatures become their wire bytes (TempoSignature::to_bytes), and
;;; authorizations the decoded RLP items a 0x76 transaction carries, the shapes
;;; clempo.executor:request-txv takes for :tempo-authorizations and :key-authorization.

(defvar *content* nil
  "True while reading a value serde buffered as Content (a flattened struct's fields, an
untagged enum's candidates): its sequence-length errors differ from serde_json's.")

(defvar *in-seq* nil "True while reading an element of a sequence rather than a map value.")

;;; Derived structs: serde_derive's visit_map and visit_seq.

(defmacro defrecord (name (&key expected deny-unknown) &body fields)
  "A reader for a serde-derived struct EXPECTED (\"struct X\"), as a map or a sequence. Each
field is (KEY \"jsonName\" TYPE &key OPTION DEFAULT). OPTION (Rust Option) reads null as absent;
OPTION T also lets the key be missing, OPTION :WITH (a `with` adapter) does not. DEFAULT (serde
default) may be missing in either form; any other field is required. Returns a plist of the
present fields."
  (let* ((src (gensym "SRC")) (out (gensym "OUT")) (seen (gensym "SEEN")) (key (gensym "KEY"))
         (n (gensym "N")) (count (length fields)))
    (flet ((read-form (kw type opts &optional in-seq)
             `(let ((*in-seq* ,in-seq))
                ,(if (getf opts :option)
                     `(unless (serde:read-null-p ,src) (setf (getf ,out ,kw) (read-param ',type ,src)))
                     `(setf (getf ,out ,kw) (read-param ',type ,src))))))
      `(defparam ,name (,src) ,(format nil "serde ~A" expected)
         (let ((,out '()) (,seen '()))
           (case (serde:peek-kind ,src)
             (:object
              (serde:do-object (,key ,src)
                (cond
                  ,@(loop for (kw json type . opts) in fields
                          collect `((string= ,key ,json)
                                    (when (member ,kw ,seen) (serde:serde-fail-after-key ,src "duplicate field `~A`" ,json))
                                    (push ,kw ,seen)
                                    ,(read-form kw type opts)))
                  (t ,(if deny-unknown
                          `(serde:serde-fail-after-key ,src "unknown field `~A`, ~A" ,key
                                                       ,(one-of (mapcar #'second fields)))
                          `(serde:skip-value ,src)))))
              ,@(loop for (kw json nil . opts) in fields
                      unless (or (getf opts :default) (eq (getf opts :option) t))
                        collect `(unless (member ,kw ,seen) (serde:serde-fail-here ,src "missing field `~A`" ,json))))
             (:array
              (let ((,n 0))
                (serde:do-array (i ,src)
                  (case ,n
                    ,@(loop for (kw nil type . opts) in fields for i from 0
                            collect `(,i ,(read-form kw type opts t)))
                    (t (seq-trailing ,src ,count)))
                  (incf ,n))
                ;; serde_derive takes each missing element's default; the first required one fails
                (cond ,@(loop for (nil nil nil . opts) in fields for i from 0
                              unless (getf opts :default)
                                collect `((<= ,n ,i)
                                          (serde:serde-fail-here ,src "invalid length ~D, expected ~A with ~D element~:P"
                                                                 ,i ,expected ,count))))))
             (t (serde:invalid-type ,src ,expected)))
           ,out)))))

(defun fail-peeked (src fmt &rest args)
  "A custom error raised after a value was read. serde_json positions it after its container's
end_map or end_seq: past a closing brace or bracket, past a sequence's comma, else after the
whitespace before the next character."
  (let ((next (serde:peek src)))
    (cond ((member next '(#\} #\])) (incf (serde:src-pos src)))
          ((and *in-seq* (eql next #\,)) (incf (serde:src-pos src)) (serde:skip-ws src)))
    (apply #'serde:serde-fail-at src (serde:src-pos src) fmt args)))

(defun seq-trailing (src count)
  "A sequence longer than the struct: serde_json's end_seq after the comma, or, for buffered
Content, the count of all the elements."
  (if *content*
      (let ((total count))
        (loop (serde:skip-value src) (incf total)
              (if (eql (serde:peek src) #\,) (incf (serde:src-pos src)) (return)))
        (serde:peek src) (incf (serde:src-pos src))
        (serde:serde-fail "invalid length ~D, expected ~D element~:P in sequence" total count))
      (progn (serde:peek src)
             (serde:serde-fail-at src (1+ (serde:src-pos src)) "trailing characters"))))

(defun read-unit-enum (src name variants)
  "A serde unit-variant enum NAME from one of the camelCase VARIANTS; returns a keyword."
  (flet ((variant (s)
           (or (and (member s variants :test #'string=) (intern (string-upcase s) :keyword))
               (serde:serde-fail-here src "unknown variant `~A`, expected ~A" s
                                      (if (= (length variants) 2)
                                          (format nil "`~A` or `~A`" (first variants) (second variants))
                                          (format nil "one of ~{`~A`~^, ~}" variants))))))
    (case (serde:peek-kind src)
      (:string (variant (serde:read-string src)))
      (:object (if *content*
                   ;; ContentDeserializer: a single-key map names the variant, whose value is unit
                   (let ((result nil) (keys 0))
                     (serde:do-object (key src)
                       (incf keys)
                       (setf result (variant key))
                       (unless (serde:read-null-p src) (serde:invalid-type src "unit variant")))
                     (unless (= keys 1) (serde:serde-fail "invalid value: map, expected map with a single key"))
                     result)
                   (progn (serde:skip-value src) (serde:serde-fail-here src "invalid type: map, expected enum ~A" name))))
      (t (if *content*
             (serde:invalid-type src "string or map")
             (progn (serde:peek src) (incf (serde:src-pos src)) (serde:serde-fail-here src "expected value")))))))

(defparam primitive-signature-type (src) "PrimitiveSignatureType"
  (read-unit-enum src "PrimitiveSignatureType" '("secp256k1" "p256" "webAuthn")))

(defparam key-signature-type (src) "SignatureType, buffered or inline"
  (read-unit-enum src "SignatureType" '("secp256k1" "p256" "webAuthn" "configurableAccount")))

(defparam keychain-version (src) "KeychainVersion"
  (read-unit-enum src "KeychainVersion" '("v1" "v2")))

;;; Untagged enums and flattened structs: serde buffers the value as Content.

(defun read-buffered (type text)
  "Read TEXT, a buffered value, as TYPE; errors propagate without their inner position."
  (let ((*content* t)) (read-param type (serde:make-src text))))

(defun read-untagged (src name types)
  "An untagged enum: the first of TYPES that reads the buffered value, else serde's error
after it. Returns (values value type)."
  (let ((text (serde:read-content src)))
    (dolist (type types (fail-peeked src "data did not match any variant of untagged enum ~A" name))
      (handler-case (return (values (read-buffered type text) type))
        (serde:serde-error () nil)))))

(defun read-flattened-struct (src expected own inner)
  "A struct whose OWN fields ((key \"json\" type) ...) are read inline and whose remaining keys
deserialize the flattened INNER record after the closing brace. Returns (values own-plist
inner-plist)."
  (let ((out '()) (seen '()))
    (multiple-value-bind (rest end)
        (read-flattened src expected (mapcar #'second own)
                        (lambda (key src)
                          (destructuring-bind (kw json type) (find key own :key #'second :test #'string=)
                            (when (member kw seen) (serde:serde-fail-after-key src "duplicate field `~A`" json))
                            (push kw seen)
                            (setf (getf out kw) (let ((*in-seq* nil)) (read-param type src))))))
      (loop for (kw json) in own
            unless (member kw seen) do (serde:serde-fail-at src end "missing field `~A`" json))
      (let ((map (format nil "{~{~A~^,~}}"
                         (mapcar (lambda (e) (format nil "~A:~A" (json:to-json-string (car e)) (cdr e))) rest))))
        (values out
                (handler-case (read-buffered inner map)
                  (serde:serde-error (e)
                    (serde:serde-fail-at src end "~A" (strip-position (serde:serde-error-message e))))))))))

;;; alloy_rlp decoding, with its errors (alloy-rlp src/header.rs, decode.rs; alloy-rlp-derive).

(defun rlp-error (message) (serde:serde-fail "~A" message))

(defun rlp-header (d pos end)
  "Header::decode: (values list-p payload-start payload-length)."
  (when (>= pos end) (rlp-error "input too short"))
  (let ((b (aref d pos)) list len)
    (cond ((< b #x80) (setf len 1))
          ((<= b #xb7)
           (incf pos) (setf len (- b #x80))
           (when (= len 1)
             (when (>= pos end) (rlp-error "input too short"))
             (when (< (aref d pos) #x80) (rlp-error "non-canonical single byte"))))
          ((or (<= #xb8 b #xbf) (>= b #xf8))
           (setf list (>= b #xf8))
           (let ((n (- b (if list #xf7 #xb7))))
             (incf pos)
             (when (< (- end pos) n) (rlp-error "input too short"))
             (when (zerop (aref d pos)) (rlp-error "leading zero"))
             (setf len (octets->integer d :start pos :end (+ pos n)))
             (incf pos n)
             (when (< len 56) (rlp-error "non-canonical size"))))
          (t (incf pos) (setf list t len (- b #xc0))))
    (when (< (- end pos) len) (rlp-error "input too short"))
    (values list pos len)))

(defun rlp-payload (d pos end list)
  "Header::decode_bytes: (values payload-start payload-end)."
  (multiple-value-bind (l start len) (rlp-header d pos end)
    (unless (eq l list) (rlp-error (if list "unexpected string" "unexpected list")))
    (values start (+ start len))))

(defun rlp-uint (d pos end bytes)
  (multiple-value-bind (s e) (rlp-payload d pos end nil)
    (when (> (- e s) bytes) (rlp-error "overflow"))
    (when (and (> e s) (zerop (aref d s))) (rlp-error "leading zero"))
    (values (octets->integer d :start s :end e) e)))

(defun rlp-fixed (d pos end n)
  (multiple-value-bind (s e) (rlp-payload d pos end nil)
    (unless (= (- e s) n) (rlp-error "unexpected length"))
    (values (octets-slice d s e) e)))

(defun rlp-address (d pos end)
  (multiple-value-bind (a p) (rlp-fixed d pos end 20) (values (octets->integer a) p)))

(defmacro rlp-struct ((d pos end) &body fields)
  "A derived RlpDecodable struct: each field (VAR FORM) is decoded by FORM from POS, bound to
the rest of the whole buffer as the derive does, and evaluated with VAR set to the value;
returns (values (list values...) position)."
  (let ((len (gensym "LEN")) (start (gensym "START")) (list-p (gensym "LIST")))
    `(multiple-value-bind (,list-p ,start ,len) (rlp-header ,d ,pos ,end)
       (unless ,list-p (rlp-error "unexpected string"))
       (let* ((,pos ,start)
              ,@(loop for (var form) in fields
                      collect `(,var (multiple-value-bind (v p) ,form (setf ,pos p) v))))
         (unless (= (- ,pos ,start) ,len)
           (rlp-error (format nil "unexpected list length (got ~D, expected ~D)" (- ,pos ,start) ,len)))
         (values (list ,@(mapcar #'first fields)) ,pos)))))

(defun rlp-vec (d pos end fn)
  "Vec<T>: FN (d pos end) -> (values item pos) over the list's payload."
  (multiple-value-bind (s e) (rlp-payload d pos end t)
    (let ((out '()))
      (loop while (< s e) do (multiple-value-bind (v p) (funcall fn d s e) (push v out) (setf s p)))
      (values (nreverse out) e))))

(defun rlp-exact (d fn)
  (multiple-value-bind (v p) (funcall fn d 0 (length d))
    (unless (= p (length d)) (rlp-error "unexpected length"))
    v))

(defun rlp-account-config (d pos end)
  "ConfigurableAccountConfig: (salt version threshold ((owner weight) ...))."
  (rlp-struct (d pos end)
    (salt (rlp-fixed d pos end 32))
    (version (rlp-uint d pos end 8))
    (threshold (rlp-uint d pos end 1))
    (owners (rlp-vec d pos end
                     (lambda (d pos end)
                       (rlp-struct (d pos end)
                         (owner (rlp-address d pos end))
                         (weight (rlp-uint d pos end 1))))))))

(defun valid-v-p (v) (or (<= v 1) (<= 27 v 28) (>= v 35)))

(defun primitive-signature-bytes-p (b)
  "PrimitiveSignature::from_bytes accepts B."
  (let ((n (length b)))
    (cond ((= n 65) (valid-v-p (aref b 64)))
          ((< n 2) nil)
          (t (case (aref b 0)
               (1 (= n 130))
               (2 (<= 128 (1- n) 2048))
               (t nil))))))

(defun account-config-valid-p (config account)
  "ConfigurableAccountConfig::validate_for_account succeeds."
  (destructuring-bind (salt version threshold owners) config
    (declare (ignore salt))
    (let ((weights (sort (mapcar #'second owners) #'>)))
      (and owners (<= (length owners) 48) (plusp threshold)
           (notany (lambda (o) (or (zerop (first o)) (and (zerop version) (eql account (first o))))) owners)
           (notany #'zerop weights)
           (loop for (a b) on owners while b always (< (first a) (first b)))
           (<= (reduce #'+ weights) 255)
           (<= threshold (reduce #'+ (subseq weights 0 (min 8 (length weights)))))))))

(defun configurable-signature-p (b)
  "ConfigurableAccountSignature decodes from B (RLP, decode_exact) and passes try_new."
  (handler-case
      (destructuring-bind (account config signatures)
          (rlp-exact b (lambda (d pos end)
                         (rlp-struct (d pos end)
                           (account (rlp-address d pos end))
                           (config (rlp-account-config d pos end))
                           (signatures (rlp-vec d pos end
                                                (lambda (d pos end)
                                                  (multiple-value-bind (s e) (rlp-payload d pos end nil)
                                                    (let ((sig (octets-slice d s e)))
                                                      (unless (primitive-signature-bytes-p sig) (rlp-error "bad signature"))
                                                      (values sig e)))))))))
        (and (plusp account) (account-config-valid-p config account)
             signatures (<= (length signatures) 8)
             (every (lambda (s) (<= (length s) 2049)) signatures)))
    (serde:serde-error () nil)))

;;; Signatures

(defrecord signature-repr (:expected "struct HumanReadableRepr")
  (:r "r" q256) (:s "s" q256) (:y-parity "yParity" q64 :option t) (:v "v" q64 :option t :default t))

(defparam signature (src) "alloy Signature: (:r r :s s :y-parity bool)"
  (destructuring-bind (&key r s (y-parity nil yp) (v nil vp)) (read-param 'signature-repr src)
    (list :r r :s s
          :y-parity (cond (yp (case y-parity (0 nil) (1 t) (t (fail-peeked src "invalid yParity"))))
                          (vp (if (valid-v-p v) (= (mod v 2) (if (<= v 1) 1 0))
                                  (fail-peeked src "invalid v")))
                          (t (fail-peeked src "missing `yParity` or `v`"))))))

(defrecord p256-signature (:expected "struct P256SignatureWithPreHash")
  (:r "r" hash) (:s "s" hash) (:x "pubKeyX" hash) (:y "pubKeyY" hash) (:pre-hash "preHash" bool))

(defrecord webauthn-signature (:expected "struct WebAuthnSignature")
  (:r "r" hash) (:s "s" hash) (:x "pubKeyX" hash) (:y "pubKeyY" hash) (:data "webauthnData" hex-data))

(defparam primitive-signature (src) "PrimitiveSignature (tag \"type\"), as its wire bytes"
  (let* ((text (serde:read-content src))
         (type (let ((s (serde:make-src text)) (tag nil))
                 (serde:do-object (key s :expected "internally tagged enum PrimitiveSignature")
                   (if (and (string= key "type") (not tag))
                       (setf tag (read-param 'primitive-signature-type s))
                       (serde:skip-value s)))
                 (or tag (serde:serde-fail "missing field `type`")))))
    (ecase type
      (:secp256k1 (destructuring-bind (&key r s y-parity) (read-buffered 'signature text)
                    (octets-concat (integer->octets r 32) (integer->octets s 32) (octets-of (if y-parity 28 27)))))
      (:p256 (destructuring-bind (&key r s x y pre-hash) (read-buffered 'p256-signature text)
               (octets-concat (octets-of 1) r s x y (octets-of (if pre-hash 1 0)))))
      (:webauthn (destructuring-bind (&key r s x y data) (read-buffered 'webauthn-signature text)
                   (octets-concat (octets-of 2) data r s x y))))))

(defparam configurable-account-signature (src) "ConfigurableAccountSignature: RLP bytes, wire form"
  (let ((b (read-param 'hex-data src)))
    (unless (configurable-signature-p b) (serde:serde-fail "invalid configurable account signature"))
    (octets-concat (octets-of 5) b)))

(defparam account-signature (src) "AccountSignature (untagged), as its wire bytes"
  (values (read-untagged src "AccountSignature" '(primitive-signature configurable-account-signature))))

(defrecord keychain-signature-fields (:expected "struct KeychainSignature")
  (:user "userAddress" address) (:signature "signature" account-signature)
  (:version "version" keychain-version :default t))

(defparam keychain-signature (src) "KeychainSignature, as its wire bytes"
  (destructuring-bind (&key user signature version) (read-param 'keychain-signature-fields src)
    (octets-concat (octets-of (if (eq version :v2) 4 3)) (integer->octets user 20) signature)))

(defparam tempo-signature (src) "TempoSignature (untagged), as its wire bytes"
  (values (read-untagged src "TempoSignature"
                         '(primitive-signature keychain-signature configurable-account-signature))))

;;; Authorizations

(defrecord authorization (:expected "struct Authorization")
  (:chain-id "chainId" q256) (:address "address" address) (:nonce "nonce" q64))

(defun rlp-items (list)
  "LIST (integers, octets and lists) as the items a decoder gives for its encoding."
  (values (clempo.rlp:rlp-decode (clempo.rlp:rlp-encode list))))

(defparam tempo-signed-authorization (src) "TempoSignedAuthorization: (chain-id address nonce signature) RLP items"
  (multiple-value-bind (own inner)
      (read-flattened-struct src "struct TempoSignedAuthorization" '((:signature "signature" tempo-signature)) 'authorization)
    (destructuring-bind (&key chain-id address nonce) inner
      (rlp-items (list chain-id (integer->octets address 20) nonce (getf own :signature))))))

(defvec tempo-authorization-list tempo-signed-authorization)

;;; Key authorizations

(defparam selector (src) "SelectorRule's selector: untagged FixedBytes<4> or [u8; 4]"
  (values (read-untagged src "SelectorValue" '(selector-bytes))))

(defparam selector-bytes (src) "FixedBytes<4>"
  (clempo.rpc::read-fixed-hex src 4 "4 bytes, represented as a hex string of length 8, an array of u8, or raw bytes"))

(defparam addresses (src) "Vec<Address>"
  (let ((out '())) (serde:do-array (i src) (push (read-param 'address src) out)) (nreverse out)))

(defrecord token-limit (:expected "struct TokenLimit")
  (:token "token" address) (:limit "limit" q256) (:period "period" q64 :default t))
(defvec token-limits token-limit)

(defrecord selector-rule (:expected "struct SelectorRule")
  (:selector "selector" selector) (:recipients "recipients" addresses :default t))
(defvec selector-rules selector-rule)

(defrecord call-scope (:expected "struct CallScope")
  (:target "target" address) (:selector-rules "selectorRules" selector-rules :default t))
(defvec call-scopes call-scope)

(defrecord key-authorization-fields (:expected "struct KeyAuthorization")
  (:chain-id "chainId" q64) (:key-type "keyType" key-signature-type) (:key-id "keyId" address)
  (:expiry "expiry" nonzero-q64 :option :with) (:limits "limits" token-limits :option t)
  (:allowed-calls "allowedCalls" call-scopes :option t) (:witness "witness" hash :option t)
  (:is-admin "isAdmin" bool :default t) (:account "account" address :option t))

(defun key-authorization-items (a)
  "KeyAuthorizationWire's fields (rlp(trailing)): absent trailing options are left out, absent
inner ones are RLP's empty string."
  (destructuring-bind (&key chain-id key-type key-id expiry (limits nil lp) (allowed-calls nil cp)
                         witness is-admin account)
      a
    (flet ((addr (x) (integer->octets x 20)))
      (let* ((options
               (list (and expiry (list expiry))
                     (and lp (list (mapcar (lambda (l)
                                             (destructuring-bind (&key token limit period) l
                                               (list* (addr token) limit (and period (plusp period) (list period)))))
                                           limits)))
                     (and cp (list (mapcar (lambda (c)
                                             (destructuring-bind (&key target selector-rules) c
                                               (list (addr target)
                                                     (mapcar (lambda (r) (list (getf r :selector) (mapcar #'addr (getf r :recipients))))
                                                             selector-rules))))
                                           allowed-calls)))
                     (and witness (list witness))
                     (and is-admin (list 1))
                     (and account (list (addr account)))))
             (used (subseq options 0 (1+ (or (position-if #'identity options :from-end t) -1)))))
        (list* chain-id
               (position key-type '(:secp256k1 :p256 :webauthn :configurableaccount))
               (addr key-id)
               (mapcar (lambda (o) (if o (first o) (make-octets 0))) used))))))

(defparam signed-key-authorization (src) "SignedKeyAuthorization: (authorization-items signature) RLP items"
  (multiple-value-bind (own inner)
      (read-flattened-struct src "struct SignedKeyAuthorization" '((:signature "signature" account-signature))
                             'key-authorization-fields)
    (rlp-items (list (key-authorization-items inner) (getf own :signature)))))

;;; Configurable account simulations

(defparam account-config (src) "ConfigurableAccountConfig as RLP bytes: (:salt :version :threshold :owners ((owner weight) ...))"
  (let ((b (read-param 'hex-data src)))
    (destructuring-bind (salt version threshold owners)
        (handler-case (rlp-exact b #'rlp-account-config)
          (serde:serde-error (e) (fail-peeked src "~A" (serde:serde-error-message e))))
      (list :salt salt :version version :threshold threshold :owners owners))))

(defrecord simulation-approval (:expected "struct ConfigurableAccountSimulationApproval" :deny-unknown t)
  (:owner "owner" address) (:key-type "keyType" primitive-signature-type :option t :default t)
  (:key-data "keyData" hex-data :option t :default t))
(defvec simulation-approvals simulation-approval)

(defrecord account-simulation (:expected "struct ConfigurableAccountSimulationSpec" :deny-unknown t)
  (:signer "signer" address :option t :default t) (:config "config" account-config)
  (:approvals "approvals" simulation-approvals))
