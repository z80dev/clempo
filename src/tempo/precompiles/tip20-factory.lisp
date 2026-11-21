(in-package #:clempo.tip20-factory)

;;; TIP20Factory (crates/precompiles/src/tip20_factory): tokens at
;;; TIP20_PREFIX ‖ keccak256(abi.encode(sender, salt))[0..8]; ids below 1024 are reserved
;;; for protocol tokens.

(defcontract tip20-factory (:address +tip20-factory-address+)
  (token-id-counter u256 :reserved t
                    :doc "Legacy slot 0 of the Solidity model; the reference struct is empty."))

(defconstant +reserved-size+ 1024)

(deferror address-reserved "AddressReserved()")
(deferror address-not-reserved "AddressNotReserved()")
(deferror invalid-quote-token "InvalidQuoteToken()")
(deferror token-already-exists "TokenAlreadyExists(address token)")

(defevent token-created
  "TokenCreated(address indexed token, string name, string symbol, string currency, address quoteToken, address admin, bytes32 salt)")

(defun compute-token-address (sender salt)
  "(values address id): no keccak gas (the reference hashes outside the storage provider)."
  (let* ((hash (clempo.keccak:keccak256-integer
                (clempo.abi:abi-encode '(:address (:bytes-n 32)) (list sender salt))))
         (id (ldb (byte 64 192) hash)))
    (values (tip20-address id) id)))

(defun token-address (sender salt)
  "TIP20Factory::get_token_address: AddressReserved() in the reserved range."
  (multiple-value-bind (address id) (compute-token-address sender salt)
    (when (< id +reserved-size+) (revert 'address-reserved))
    address))

(defun tip20-p (token)
  "TIP20Factory::is_tip20: the TIP-20 prefix and code (one account load)."
  (and (tip20-address-p token) (account-has-code-p (account token))))

(defun check-quote-token (quote-token currency)
  (when (and (string= currency "USD")
             (string/= (clempo.tip20:currency (clempo.tip20:from-address quote-token)) "USD"))
    (revert 'invalid-quote-token)))

(defun create-token (sender name symbol currency quote-token admin salt)
  (multiple-value-bind (token id) (compute-token-address sender salt)
    (when (tip20-p token) (revert 'token-already-exists token))
    (unless (tip20-p quote-token) (revert 'invalid-quote-token))
    (check-quote-token quote-token currency)
    (when (< id +reserved-size+) (revert 'address-reserved))
    (clempo.tip20:initialize-token token sender name symbol currency quote-token admin)
    (emit +tip20-factory-address+ 'token-created token name symbol currency quote-token admin salt)
    token))

(defun create-token-with-logo (sender name symbol currency quote-token admin salt logo-uri)
  "TIP-1026 (T5): create-token, then set the logo URI."
  (unless (string= logo-uri "") (clempo.tip20:validate-logo-uri logo-uri))
  (let ((token (create-token sender name symbol currency quote-token admin salt)))
    (unless (string= logo-uri "") (clempo.tip20:write-logo-uri token sender logo-uri))
    token))

(defun create-token-reserved-address (address name symbol currency quote-token admin)
  "Deploy a protocol token at a reserved ADDRESS (genesis and hardfork bootstrapping)."
  (unless (tip20-address-p address) (revert 'clempo.tip20:invalid-token))
  (when (tip20-p address) (revert 'token-already-exists address))
  (unless (zerop quote-token)
    (when (or (= address +path-usd-address+) (not (tip20-p quote-token)))
      (revert 'invalid-quote-token))
    (check-quote-token quote-token currency))
  (unless (< (tip20-id address) +reserved-size+) (revert 'address-not-reserved))
  (clempo.tip20:initialize-token address admin name symbol currency quote-token admin)
  (emit +tip20-factory-address+ 'token-created address name symbol currency quote-token admin 0)
  address)

(defdispatch tip20-factory (self input sender)
  (charge-input-cost input)
  (dispatch-calls))

(register-precompile +tip20-factory-address+ 'tip20-factory :genesis)

(defcall tip20-factory create-token* ((name string) (symbol string) (currency string)
                                      (quote-token address) (admin address) (salt b256))
    (:sol "createToken(string name, string symbol, string currency, address quoteToken, address admin, bytes32 salt)"
     :returns "address" :mutates t)
  (create-token sender name symbol currency quote-token admin salt))
(defcall tip20-factory create-token-with-logo* ((name string) (symbol string) (currency string)
                                                (quote-token address) (admin address)
                                                (salt b256) (logo-uri string))
    (:sol "createToken(string name, string symbol, string currency, address quoteToken, address admin, bytes32 salt, string logoURI)"
     :returns "address" :mutates t :since :t5)
  (create-token-with-logo sender name symbol currency quote-token admin salt logo-uri))
(defcall tip20-factory is-tip20 ((token address))
    (:sol "isTIP20(address token)" :returns "bool")
  (tip20-p token))
(defcall tip20-factory get-token-address ((sender* address) (salt b256))
    (:sol "getTokenAddress(address sender, bytes32 salt)" :returns "address")
  (token-address sender* salt))
