;;; Tempo consensus key files (crates/consensus-config/src/lib.rs), ValidatorConfigV2
;;; registration payloads, and the `clempo keys` command.
(in-package #:clempo.validator)

(export '(keys-error read-secret read-signing-key write-signing-key read-signing-share
          write-signing-share signing-public-key registration-message sign-registration
          add-validator-calldata rotate-validator-calldata keys-command
          +add-validator-namespace+ +rotate-validator-namespace+))

(define-condition keys-error (error)
  ((message :initarg :message :reader keys-error-message))
  (:report (lambda (c s) (format s "~A" (keys-error-message c)))))

(defun keys-fail (control &rest args)
  (error 'keys-error :message (apply #'format nil control args)))

;;; Strict hex as const_hex::decode reads it: optional 0x, even length, hex digits only.

(defun rust-trim (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11)) string))

(defun strict-hex (string &optional length what)
  "STRING's octets, decoded like const_hex (optional 0x, even length, hex digits only) but in
constant time: key files hold secrets."
  (let* ((h (if (and (>= (length string) 2) (string= "0x" string :end2 2)) (subseq string 2) string))
         (o (or (clempo.ct:hex-to-octets h) (keys-fail "~A is not valid hex" (or what "input")))))
    (when (and length (/= (length o) length))
      (clempo.ct:wipe o)
      (keys-fail "~A must be ~D bytes, got ~D" (or what "input") length (length o)))
    o))

(defun secret-hex-file-octets (octets)
  "The 0x-hex text of the secret OCTETS as file octets, encoded in constant time."
  (string->octets (concatenate 'string "0x" (clempo.ct:octets-to-hex octets))))

(defun read-text-file (path what)
  (let ((octets (handler-case (clempo.io:read-file-octets path)
                  (file-error () (keys-fail "cannot read ~A ~A" what path)))))
    (unless (every (lambda (b) (< b 128)) octets) (keys-fail "~A ~A is not text" what path))
    (octets->string octets)))

;;; Files are created with O_EXCL and mode 0600 in one step; existing files are never replaced.

(defun write-new-file (path octets)
  (let ((fd (handler-case
                (sb-posix:open (namestring path)
                               (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl) #o600)
              (sb-posix:syscall-error (e)
                (keys-fail "refusing to write ~A: ~A" path
                           (sb-int:strerror (sb-posix:syscall-errno e)))))))
    (let ((stream (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                                            :buffering :full :auto-close t)))
      (unwind-protect (progn (write-sequence octets stream) (finish-output stream) (sb-posix:fsync fd))
        (close stream)))
    path))

;;; Passphrases (read_secret): at most 1024 bytes of UTF-8, trailing CR/LF removed.

(defconstant +max-passphrase-bytes+ 1024)

(defun read-secret (path)
  "The passphrase in PATH as octets, read like Tempo's read_secret (files and FIFOs)."
  (let ((octets (with-open-file (in path :element-type '(unsigned-byte 8)
                                         :if-does-not-exist nil)
                  (unless in (keys-fail "cannot read passphrase file ~A" path))
                  (let* ((buffer (make-octets (1+ +max-passphrase-bytes+)))
                         (n (read-sequence buffer in)))
                    (octets-slice buffer 0 n)))))
    (when (> (length octets) +max-passphrase-bytes+)
      (keys-fail "passphrase exceeds ~D byte limit" +max-passphrase-bytes+))
    (unless (clempo.abi:utf8-valid-p octets) (keys-fail "passphrase is not UTF-8"))
    (let ((end (length octets)))
      (loop while (and (plusp end) (member (aref octets (1- end)) '(10 13))) do (decf end))
      (octets-slice octets 0 end))))

;;; Ed25519 signing keys: 32-byte seeds.

(defun read-signing-key (path &key passphrase-file)
  "The 32-octet ed25519 seed in PATH: 0x-hex, or an age passphrase file when
PASSPHRASE-FILE names the passphrase (SigningKey::read_from_file_[un]encrypted)."
  (if passphrase-file
      (let* ((passphrase (read-secret passphrase-file))
             (data (handler-case (clempo.io:read-file-octets path)
                     (file-error () (keys-fail "cannot read signing key ~A" path))))
             (plain (handler-case (clempo.age:age-decrypt data passphrase)
                      (clempo.age:age-error (e)
                        (keys-fail "failed decrypting ~A (wrong passphrase or malformed file?): ~A"
                                   path e)))))
        (unless (= (length plain) 32)
          (keys-fail "decrypted signing key is ~D bytes, expected 32" (length plain)))
        plain)
      (strict-hex (rust-trim (read-text-file path "signing key")) 32 "signing key")))

(defun write-signing-key (path seed &key passphrase-file
                                          (work-factor clempo.age:+age-default-work-factor+))
  "Create PATH (mode 0600, never overwriting) holding SEED as 0x-hex, or age-encrypted
under the passphrase in PASSPHRASE-FILE."
  (check-type seed octets)
  (unless (= (length seed) 32) (keys-fail "signing key must be 32 bytes"))
  (write-new-file path (if passphrase-file
                           (clempo.age:age-encrypt seed (read-secret passphrase-file)
                                                   :work-factor work-factor)
                           (secret-hex-file-octets seed))))

(defun signing-public-key (seed) (clempo.ed25519:ed25519-public-key seed))

;;; BLS signing shares: 0x-hex of commonware Share (varint index || scalar).

(defun read-signing-share (path)
  "The clempo.dkg:share in PATH (SigningShare::read_from_file)."
  (clempo.ct:with-wiped ((octets (strict-hex (rust-trim (read-text-file path "signing share"))
                                             nil "signing share")))
    (handler-case (clempo.cwcodec:cw-decode 'clempo.dkg:share octets)
      (error (e) (keys-fail "signing share is not a valid BLS12-381 share: ~A" e)))))

(defun write-signing-share (path share)
  "Create PATH (mode 0600, never overwriting) holding SHARE as 0x-hex."
  (clempo.ct:with-wiped ((encoded (clempo.cwcodec:cw-encode 'clempo.dkg:share share))
                         (text (secret-hex-file-octets encoded)))
    (write-new-file path text)))

;;; ValidatorConfigV2 registration (crates/precompiles validator_config_v2): Ed25519 over
;;; commonware union_unique(namespace, keccak256(chainId u64 || precompile || validator ||
;;; u8 len || ingress || u8 len || egress [|| feeRecipient])).

(defparameter +add-validator-namespace+ "TEMPO_VALIDATOR_CONFIG_V2_ADD_VALIDATOR")
(defparameter +rotate-validator-namespace+ "TEMPO_VALIDATOR_CONFIG_V2_ROTATE_VALIDATOR")

(defun check-endpoints (ingress egress)
  "Reject what the precompile rejects at T14: non-socket ingress, non-IP egress, >255 bytes,
IPv4-mapped IPv6."
  (flet ((check (string socket-p)
           (unless (stringp string) (keys-fail "endpoint must be a string"))
           (when (> (length (string->octets string)) 255) (keys-fail "~S exceeds 255 bytes" string))
           (let ((parsed (if socket-p
                             (clempo.validator-config-v2::parse-ip-port string)
                             (clempo.validator-config-v2::parse-ip-address string))))
             (unless parsed
               (keys-fail (if socket-p "ingress ~S is not of the form <ip>:<port>"
                              "egress ~S is not a valid IP address")
                          string))
             (when (clempo.validator-config-v2::%mapped-validator-ip-p
                    (if socket-p
                        (clempo.validator-config-v2::parsed-validator-socket-ip parsed)
                        parsed))
               (keys-fail "~S is an IPv4-mapped IPv6 address; use the IPv4 form" string)))))
    (check ingress t)
    (check egress nil)))

(defun registration-message (kind chain-id validator ingress egress &optional fee-recipient)
  "The octets the consensus key signs for KIND :ADD or :ROTATE."
  (check-endpoints ingress egress)
  (check-type chain-id (unsigned-byte 64))
  (check-type validator (unsigned-byte 160))
  (let* ((in (string->octets ingress)) (out (string->octets egress))
         (namespace (string->octets (ecase kind
                                      (:add +add-validator-namespace+)
                                      (:rotate +rotate-validator-namespace+))))
         (digest (clempo.keccak:keccak256
                  (apply #'octets-concat
                         (integer->octets chain-id 8)
                         (integer->octets clempo.precompiles:+validator-config-v2-address+ 20)
                         (integer->octets validator 20)
                         (octets-of (length in)) in (octets-of (length out)) out
                         (when (eq kind :add)
                           (check-type fee-recipient (unsigned-byte 160))
                           (list (integer->octets fee-recipient 20)))))))
    (octets-concat (octets-of (length namespace)) namespace digest)))

(defun sign-registration (seed kind chain-id validator ingress egress &optional fee-recipient)
  "The 64-octet ed25519 signature for addValidator (:ADD) or rotateValidator (:ROTATE)."
  (clempo.ed25519:ed25519-sign
   seed (registration-message kind chain-id validator ingress egress fee-recipient)))

(defparameter +add-validator-signature+ "addValidator(address,bytes32,string,string,address,bytes)")
(defparameter +rotate-validator-signature+ "rotateValidator(uint64,bytes32,string,string,bytes)")

(defun add-validator-calldata (validator public-key ingress egress fee-recipient signature)
  (clempo.abi:abi-encode-call
   (clempo.abi:abi-selector +add-validator-signature+)
   '(:address (:bytes-n 32) :string :string :address :bytes)
   (list validator (octets->integer public-key) ingress egress fee-recipient signature)))

(defun rotate-validator-calldata (index public-key ingress egress signature)
  (clempo.abi:abi-encode-call
   (clempo.abi:abi-selector +rotate-validator-signature+)
   '((:uint 64) (:bytes-n 32) :string :string :bytes)
   (list index (octets->integer public-key) ingress egress signature)))

;;; JSON-RPC helpers for --simulate-rpc and --send.

(defun quantity (n) (format nil "0x~(~X~)" n))
(defun address-hex (n) (integer->hex n :bytes 20))
(defun rpc-quantity (value) (hex->integer value))

(defun registry-owner (url)
  (let ((result (clempo.jsonrpc:rpc-call
                 "eth_call" (list (json:jobject "to" (address-hex clempo.precompiles:+validator-config-v2-address+)
                                                "data" (octets->hex (clempo.abi:abi-selector-octets "owner()")))
                                  "latest")
                 :url url :retries 2)))
    (ldb (byte 160 0) (hex->integer result))))

(defun simulate-call (url from data)
  "eth_call DATA from FROM; returns a JSON object describing the result or revert."
  (handler-case
      (json:jobject "from" (address-hex from) "ok" :true
                    "result" (clempo.jsonrpc:rpc-call
                              "eth_call" (list (json:jobject "from" (address-hex from)
                                                             "to" (address-hex clempo.precompiles:+validator-config-v2-address+)
                                                             "data" (octets->hex data))
                                               "latest")
                              :url url :retries 2))
    (clempo.jsonrpc:rpc-error (e)
      (json:jobject "from" (address-hex from) "ok" :false
                    "error" (princ-to-string e)
                    "data" (or (clempo.jsonrpc:rpc-error-data e) :null)))))

(defun read-owner-key (path)
  "The secp256k1 private key (32 octets) in PATH, as 0x-hex."
  (let ((key (strict-hex (rust-trim (read-text-file path "owner key")) 32 "owner key")))
    (unless (clempo.secp256k1:private-key-p key)
      (clempo.ct:wipe key)
      (keys-fail "owner key is not a valid secp256k1 scalar"))
    key))

(defun encode-1559 (chain-id nonce tip max-fee gas to data &optional signature)
  "EIP-1559 envelope; without SIGNATURE (y r s) the signing payload."
  (octets-concat (octets-of 2)
                 (clempo.rlp:rlp-encode
                  (append (list chain-id nonce tip max-fee gas (integer->octets to 20) 0 data nil)
                          signature))))

(defun send-registration (url chain-id key data &key (timeout-seconds 120))
  "Sign and send an EIP-1559 call of DATA to ValidatorConfigV2 from KEY; return the receipt."
  (flet ((call (method &rest params) (clempo.jsonrpc:rpc-call method params :url url :retries 3)))
    (let* ((from (clempo.secp256k1:pubkey->address (clempo.secp256k1:private->public key)))
           (to clempo.precompiles:+validator-config-v2-address+)
           (remote-chain (rpc-quantity (call "eth_chainId"))))
      (unless (= remote-chain chain-id)
        (keys-fail "RPC chain id ~D does not match --chain-id ~D" remote-chain chain-id))
      (let* ((nonce (rpc-quantity (call "eth_getTransactionCount" (address-hex from) "pending")))
             (block (call "eth_getBlockByNumber" "latest" :false))
             (base-fee (rpc-quantity (json:jget block "baseFeePerGas" "0x0")))
             (tip (handler-case (rpc-quantity (call "eth_maxPriorityFeePerGas"))
                    (clempo.jsonrpc:rpc-error () 0)))
             (max-fee (+ (* 2 base-fee) tip))
             (estimate (rpc-quantity (call "eth_estimateGas"
                                           (json:jobject "from" (address-hex from) "to" (address-hex to)
                                                         "data" (octets->hex data)))))
             (gas (+ estimate (floor estimate 5))))
        (multiple-value-bind (r s y)
            (clempo.secp256k1:sign (clempo.keccak:keccak256
                                    (encode-1559 chain-id nonce tip max-fee gas to data))
                                   key)
          (let* ((raw (encode-1559 chain-id nonce tip max-fee gas to data (list y r s)))
                 (hash (call "eth_sendRawTransaction" (octets->hex raw))))
            (format *error-output* "sent ~A from ~A (nonce ~D, gas ~D)~%" hash (address-hex from) nonce gas)
            (loop with deadline = (+ (clempo.io:now-seconds) timeout-seconds)
                  for receipt = (call "eth_getTransactionReceipt" hash)
                  do (when (and receipt (not (eq receipt :null))) (return receipt))
                     (when (> (clempo.io:now-seconds) deadline)
                       (keys-fail "no receipt for ~A after ~D seconds" hash timeout-seconds))
                     (sleep 0.5))))))))

;;; The `keys` command.

(defparameter +keys-usage+
  "usage: clempo keys <command> [flags]
  generate --output PATH [--encrypt --consensus.secret PATH]
  public --consensus.signing-key PATH [--consensus.secret PATH]
  add-validator --consensus.signing-key PATH [--consensus.secret PATH] --chain-id N
      --validator ADDR --ingress IP:PORT --egress IP --fee-recipient ADDR
      [--simulate-rpc URL] [--send --rpc URL --owner-key PATH]
  rotate-validator --consensus.signing-key PATH [--consensus.secret PATH] --chain-id N
      --index N --validator ADDR --ingress IP:PORT --egress IP
      [--simulate-rpc URL] [--send --rpc URL --owner-key PATH]
")

(defparameter +keys-boolean-flags+ '("encrypt" "send"))

(defun parse-keys-flags (args allowed &key (booleans +keys-boolean-flags+))
  "Alist of flag name -> value (T for the BOOLEANS); --flag value and --flag=value. The
`clempo validator` command parses its flags the same way."
  (let ((flags nil))
    (loop while args do
      (let ((arg (pop args)))
        (unless (and (> (length arg) 2) (string= "--" arg :end2 2)) (keys-fail "unexpected argument ~S" arg))
        (let* ((eq (position #\= arg)) (name (subseq arg 2 eq)))
          (unless (member name allowed :test #'string=) (keys-fail "unknown flag --~A" name))
          (when (assoc name flags :test #'string=) (keys-fail "flag --~A given twice" name))
          (push (cons name (cond (eq (subseq arg (1+ eq)))
                                 ((member name booleans :test #'string=) t)
                                 (args (pop args))
                                 (t (keys-fail "flag --~A needs a value" name))))
                flags))))
    flags))

(defun flag (flags name &optional required)
  (let ((v (cdr (assoc name flags :test #'string=))))
    (when (and required (null v)) (keys-fail "--~A is required" name))
    v))

(defun flag-integer (flags name bits)
  (let ((s (flag flags name t)))
    (unless (and (plusp (length s)) (every #'digit-char-p s)) (keys-fail "--~A must be a decimal integer" name))
    (let ((n (parse-integer s)))
      (unless (< n (expt 2 bits)) (keys-fail "--~A is out of range" name))
      n)))

(defun flag-address (flags name)
  (octets->integer (strict-hex (flag flags name t) 20 (format nil "--~A" name))))

(defun print-json (value)
  (write-string (json:to-json-string value) *standard-output*)
  (terpri *standard-output*)
  (finish-output *standard-output*))

(defun keys-registration (kind flags)
  (let* ((seed (read-signing-key (flag flags "consensus.signing-key" t)
                                 :passphrase-file (flag flags "consensus.secret")))
         (public (signing-public-key seed))
         (chain-id (flag-integer flags "chain-id" 64))
         (validator (flag-address flags "validator"))
         (ingress (flag flags "ingress" t)) (egress (flag flags "egress" t))
         (fee-recipient (when (eq kind :add) (flag-address flags "fee-recipient")))
         (index (when (eq kind :rotate) (flag-integer flags "index" 64)))
         (signature (sign-registration seed kind chain-id validator ingress egress fee-recipient))
         (data (if (eq kind :add)
                   (add-validator-calldata validator public ingress egress fee-recipient signature)
                   (rotate-validator-calldata index public ingress egress signature)))
         (out (list "publicKey" (octets->hex public) "signature" (octets->hex signature)
                    "to" (address-hex clempo.precompiles:+validator-config-v2-address+)
                    "data" (octets->hex data)))
         (status 0))
    (when (flag flags "simulate-rpc")
      (let* ((url (flag flags "simulate-rpc"))
             (simulation (simulate-call url (registry-owner url) data)))
        (unless (eq (json:jget simulation "ok") :true) (setf status 1))
        (setf out (append out (list "simulation" simulation)))))
    (when (flag flags "send")
      (let ((receipt (send-registration (flag flags "rpc" t) chain-id
                                        (read-owner-key (flag flags "owner-key" t)) data)))
        (unless (equal (json:jget receipt "status") "0x1") (setf status 1))
        (setf out (append out (list "receipt" receipt)))))
    (print-json (apply #'json:jobject out))
    status))

(defun keys-command (args)
  "Run `clempo keys ARGS...`; returns the process exit code."
  (let ((command (first args)) (rest (rest args)))
    (handler-case
        (cond
          ((or (null command) (member command '("help" "-h" "--help") :test #'string=))
           (write-string +keys-usage+ *standard-output*)
           (if command 0 2))
          ((string= command "generate")
           (let* ((flags (parse-keys-flags rest '("output" "encrypt" "consensus.secret")))
                  (secret (when (flag flags "encrypt") (flag flags "consensus.secret" t)))
                  (seed (clempo.io:random-octets 32)))
             (when (and (flag flags "consensus.secret") (not secret))
               (keys-fail "--consensus.secret needs --encrypt"))
             (write-signing-key (flag flags "output" t) seed :passphrase-file secret)
             (format t "~A~%" (octets->hex (signing-public-key seed)))
             0))
          ((string= command "public")
           (let ((flags (parse-keys-flags rest '("consensus.signing-key" "consensus.secret"))))
             (format t "~A~%" (octets->hex (signing-public-key
                                            (read-signing-key (flag flags "consensus.signing-key" t)
                                                              :passphrase-file (flag flags "consensus.secret")))))
             0))
          ((member command '("add-validator" "rotate-validator") :test #'string=)
           (let* ((add (string= command "add-validator"))
                  (flags (parse-keys-flags
                          rest (append '("consensus.signing-key" "consensus.secret" "chain-id" "validator"
                                         "ingress" "egress" "simulate-rpc" "send" "rpc" "owner-key")
                                       (if add '("fee-recipient") '("index"))))))
             (when (and (or (flag flags "rpc") (flag flags "owner-key")) (not (flag flags "send")))
               (keys-fail "--rpc and --owner-key need --send"))
             (keys-registration (if add :add :rotate) flags)))
          (t (keys-fail "unknown keys command ~S~%~A" command +keys-usage+)))
      (keys-error (e) (format *error-output* "clempo keys: ~A~%" e) 2)
      (clempo.jsonrpc:rpc-error (e) (format *error-output* "clempo keys: ~A~%" e) 1)
      (clempo.http:http-error (e) (format *error-output* "clempo keys: ~A~%" e) 1))))
