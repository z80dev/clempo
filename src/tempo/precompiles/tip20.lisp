(in-package #:clempo.tip20)

;;; TIP20Token (crates/precompiles/src/tip20/mod.rs and dispatch.rs): every address with the
;;; 0x20C0 prefix is a TIP-20 token sharing this layout. Roles live in tip20-roles.lisp,
;;; rewards in tip20-rewards.lisp. Every function takes the token's address first.

(defstorable user-reward-info
  (reward-recipient address)
  (reward-per-token u256)
  (reward-balance u256))

(defcontract tip20-token (:prefix "TOKEN")
  (roles (mapping address (mapping b256 bool)))
  (role-admins (mapping b256 b256))
  (name string)
  (symbol string)
  (currency string)
  (logo-uri string :since :t5
            :doc "TIP-1026. Reuses the never-written domain-separator slot.")
  (quote-token address)
  (next-quote-token address)
  (transfer-policy-id u64)
  (total-supply u256)
  (balances (mapping address u256))
  (allowances (mapping address (mapping address u256)))
  (permit-nonces (mapping address u256))
  (paused bool)
  (supply-cap u256)
  (salts (mapping b256 bool) :reserved t :doc "Unused, kept for layout compatibility.")
  (global-reward-per-token u256)
  (opted-in-supply u128)
  (user-reward-info (mapping address user-reward-info)))

;;; Constants.

(defconstant +decimals+ 6)
(defconstant +u128-max+ u128-max)
(defconstant +allow-all-policy-id+ 1)
(defconstant +default-admin-role+ 0)
(define-constant +ungrantable-role+ (1- (ash 1 256)))

(defmacro keccak-of (string)
  `(load-time-value (clempo.keccak:keccak256-integer (clempo.abi:utf8-encode ,string)) t))

(define-constant +pause-role+ (keccak-of "PAUSE_ROLE"))
(define-constant +unpause-role+ (keccak-of "UNPAUSE_ROLE"))
(define-constant +issuer-role+ (keccak-of "ISSUER_ROLE"))
(define-constant +burn-blocked-role+ (keccak-of "BURN_BLOCKED_ROLE"))
(define-constant +burn-at-role+ (keccak-of "BURN_AT_ROLE"))
(define-constant +permit-typehash+
  (keccak-of "Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)"))
(define-constant +eip712-domain-typehash+
  (keccak-of "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"))
(define-constant +version-hash+ (keccak-of "1"))

(defconstant +zone-portal-prefix+ #x5ad000000000000000000000)

;;; Errors and events (crates/contracts/src/precompiles/tip20.rs).

(deferror insufficient-balance "InsufficientBalance(uint256 available, uint256 required, address token)")
(deferror insufficient-allowance "InsufficientAllowance()")
(deferror supply-cap-exceeded "SupplyCapExceeded()")
(deferror invalid-supply-cap "InvalidSupplyCap()")
(deferror policy-forbids "PolicyForbids()")
(deferror invalid-recipient "InvalidRecipient()")
(deferror contract-paused "ContractPaused()")
(deferror invalid-currency "InvalidCurrency()")
(deferror invalid-quote-token "InvalidQuoteToken()")
(deferror invalid-amount "InvalidAmount()")
(deferror no-opted-in-supply "NoOptedInSupply()")
(deferror unauthorized "Unauthorized()")
(deferror protected-address "ProtectedAddress()")
(deferror invalid-token "InvalidToken()")
(deferror uninitialized "Uninitialized()")
(deferror invalid-transfer-policy-id "InvalidTransferPolicyId()")
(deferror permit-expired "PermitExpired()")
(deferror invalid-signature "InvalidSignature()")
(deferror logo-uri-too-long "LogoURITooLong()")
(deferror invalid-logo-uri "InvalidLogoURI()")
(deferror root-key-retired "RootKeyRetired(address account)")
(deferror guard-address-reserved "AddressReserved()") ; ReceivePolicyGuardError::address_reserved

(defevent transfer "Transfer(address indexed from, address indexed to, uint256 amount)")
(defevent approval "Approval(address indexed owner, address indexed spender, uint256 amount)")
(defevent mint "Mint(address indexed to, uint256 amount)")
(defevent burn "Burn(address indexed from, uint256 amount)")
(defevent burn-blocked "BurnBlocked(address indexed from, uint256 amount)")
(defevent burn-at "BurnAt(address indexed burner, address indexed from, uint256 indexed amount)")
(defevent transfer-with-memo "TransferWithMemo(address indexed from, address indexed to, uint256 amount, bytes32 indexed memo)")
(defevent transfer-policy-update "TransferPolicyUpdate(address indexed updater, uint64 indexed newPolicyId)")
(defevent supply-cap-update "SupplyCapUpdate(address indexed updater, uint256 indexed newSupplyCap)")
(defevent pause-state-update "PauseStateUpdate(address indexed updater, bool isPaused)")
(defevent next-quote-token-set "NextQuoteTokenSet(address indexed updater, address indexed nextQuoteToken)")
(defevent quote-token-update "QuoteTokenUpdate(address indexed updater, address indexed newQuoteToken)")
(defevent logo-uri-updated "LogoURIUpdated(address indexed updater, string newLogoURI)")

;;; Plain views.

(defun from-address (address)
  "TIP20Token::from_address: ADDRESS, or InvalidToken() without the TIP-20 prefix."
  (if (tip20-address-p address) address (revert 'invalid-token)))

(defun name (token) (read (token-name token)))
(defun symbol (token) (read (token-symbol token)))
(defun currency (token) (read (token-currency token)))
(defun currency-length (token)
  "The byte length of TOKEN's currency (one SLOAD, BytesLikeHandler::len)."
  (string-length (token-currency token)))
(defun logo-uri (token) (read (token-logo-uri token)))
(defun total-supply (token) (read (token-total-supply token)))
(defun quote-token (token) (read (token-quote-token token)))
(defun next-quote-token (token) (read (token-next-quote-token token)))
(defun supply-cap (token) (read (token-supply-cap token)))
(defun paused-p (token) (read (token-paused token)))
(defun balance-of (token account) (read (token-balances token account)))
(defun allowance (token owner spender) (read (token-allowances token owner spender)))
(defun permit-nonce (token owner) (read (token-permit-nonces token owner)))

(defun balance-slot (token account)
  "The storage slot of ACCOUNT's balance of TOKEN (no storage access)."
  (place-slot (token-balances token account)))

(defun legacy-transfer-policy-id (token)
  "The policy ID in the legacy TIP-20 slot."
  (read (token-transfer-policy-id token)))

(defun delete-legacy-transfer-policy-id (token)
  "Clear the legacy policy slot after its value moved to TIP-403."
  (delete (token-transfer-policy-id token)))

(defun transfer-policy-id (token)
  "The TIP-403 transfer policy of TOKEN: from T9 the registry's binding when it has one."
  (or (fork-if (:since :t9) (funcall 'clempo.tip403:registered-token-transfer-policy-id token))
      (legacy-transfer-policy-id token)))

(defun validate-usd-currency (token)
  "tip20::validate_usd_currency."
  (unless (string= (currency (from-address token)) "USD")
    (revert 'invalid-currency)))

(defun check-not-paused (token)
  (when (paused-p token) (revert 'contract-paused)))

;;; Balances.

(defun increment-balance (token account amount)
  "Slot::sinc on the balance, overflow reported as SupplyCapExceeded()."
  (let* ((place (token-balances token account))
         (new (+ (read place) amount)))
    (when (> new +u256-max+) (revert 'supply-cap-exceeded))
    (write place new)))

(defun decrement-balance (token account amount)
  "Slot::sdec on the balance, underflow reported as InsufficientBalance."
  (let* ((place (token-balances token account))
         (current (read place)))
    (when (< current amount) (revert 'insufficient-balance current amount token))
    (write place (- current amount))))

(defun write-total-supply (token amount) (write (token-total-supply token) amount))

;;; Recipients (TIP-1022): TARGET is credited; VIRTUAL-ADDR, when set, is the virtual
;;; address the sender named.

(defstruct (recipient (:constructor %make-recipient (target virtual-addr)))
  (target 0 :type (integer 0))
  (virtual-addr nil))

(defun recipient-direct (address) (%make-recipient address nil))

(defun recipient-resolve (address)
  "Recipient::resolve through the address registry."
  (let ((effective (clempo.address-registry:resolve-recipient address)))
    (if (= effective address)
        (recipient-direct address)
        (%make-recipient effective address))))

(defun recipient-original (to) (or (recipient-virtual-addr to) (recipient-target to)))

(defun recipient-validate (to)
  "Recipient::validate: not zero, not a TIP-20 address."
  (let ((target (recipient-target to)))
    (when (or (zerop target) (tip20-address-p target))
      (revert 'invalid-recipient))))

(defun emit-transfer (token from to amount)
  (emit token 'transfer from (recipient-original to) amount))

(defun emit-virtual-hop (token to amount)
  "The forwarding Transfer(virtual, master, amount) of a virtual recipient."
  (when (recipient-virtual-addr to)
    (emit token 'transfer (recipient-virtual-addr to) (recipient-target to) amount)))

;;; Policies.

(defun auth-role (role)
  "AuthRole::sender()/recipient()/mint_recipient(): ROLE from T2, Transfer before."
  (fork-if (:since :t2) role :transfer))

(defun transfer-authorized-p (token from to)
  "is_transfer_authorized: from T2 the recipient is not checked when the sender fails."
  (let* ((policy (transfer-policy-id token))
         (sender-ok (funcall 'clempo.tip403:authorized-as-p policy from (auth-role :sender))))
    (if (and (not sender-ok) (fork-if (:since :t2) t nil))
        nil
        (let ((recipient-ok (funcall 'clempo.tip403:authorized-as-p policy to (auth-role :recipient))))
          (and sender-ok recipient-ok)))))

(defun ensure-transfer-authorized (token from to)
  (unless (transfer-authorized-p token from to) (revert 'policy-forbids)))

(defun ensure-receive-policy-authorized (token sender receiver)
  "TIP-1028: RECEIVER accepts TOKEN from SENDER."
  (when (funcall 'clempo.tip403:check-receive-policy token sender receiver)
    (revert 'policy-forbids)))

(defun ensure-authorized-as (token pairs)
  "PAIRS: (user . role) with ROLE passed to the registry as given."
  (let ((policy (transfer-policy-id token)))
    (loop for (user . role) in pairs
          unless (funcall 'clempo.tip403:authorized-as-p policy user role)
            do (revert 'policy-forbids))))

(defun check-and-update-spending-limit (token from amount)
  (clempo.keychain:authorize-transfer from token amount))

;;; The core transfer.

(defun %transfer (token from to amount)
  "TIP20Token::_transfer: debit FROM, credit TO's target, log Transfer."
  (let ((from-balance (fork-if (:before :t8)
                          (let ((balance (balance-of token from)))
                            (when (> amount balance)
                              (revert 'insufficient-balance balance amount token))
                            balance))))
    (handle-rewards-on-transfer token from (recipient-target to) amount)
    (if from-balance
        (write (token-balances token from) (checked- from-balance amount))
        (decrement-balance token from amount))
    (unless (zerop (recipient-target to))
      (increment-balance token (recipient-target to) amount))
    (emit-transfer token from to amount)))

(defun %mint (token to total-supply amount)
  "TIP20Token::_mint."
  (let ((new-supply (checked+ total-supply amount)))
    (when (> new-supply (supply-cap token)) (revert 'supply-cap-exceeded))
    (handle-rewards-on-mint token (recipient-target to) amount)
    (write-total-supply token new-supply)
    (increment-balance token (recipient-target to) amount)
    (emit-transfer token 0 to amount)))

(defun validate-inbound-or-block (token originator to amount mint-total-supply memo)
  "TIP-1028 (T6+): when TO's receive policy blocks the inbound, move the funds into the
guard, store the claim receipt and return T."
  (fork-if (:before :t6) (return-from validate-inbound-or-block nil))
  (let ((target (recipient-target to)))
    (when (= target +receive-policy-guard-address+) (revert 'guard-address-reserved))
    (multiple-value-bind (reason recovery)
        (funcall 'clempo.tip403:check-receive-policy token originator target)
      (unless reason (return-from validate-inbound-or-block nil))
      (when (eq (evm-kind) :zone) (revert 'policy-forbids))
      (let ((guard (recipient-direct +receive-policy-guard-address+))
            (kind 0))
        (if mint-total-supply
            (progn (%mint token guard mint-total-supply amount)
                   (emit token 'mint +receive-policy-guard-address+ amount)
                   (setf kind 1))
            (%transfer token originator guard amount))
        (funcall 'clempo.receive-policy-guard:store-blocked token originator target
                                                   (recipient-original to) recovery amount
                                                   reason kind memo)
        t))))

(defun consume-allowance (token owner spender amount)
  (let ((allowed (allowance token owner spender)))
    (when (> amount allowed) (revert 'insufficient-allowance))
    (unless (= allowed +u256-max+)
      (write (token-allowances token owner spender) (- allowed amount)))))

(defun validate-transfer (token spender from to amount memo)
  "Resolve TO, check pause, recipient, policy, then the allowance (SPENDER) or the access-key
limit, then TIP-1028. The RECIPIENT to pay, or NIL when the funds went to the guard."
  (let ((to (recipient-resolve to)))
    (check-not-paused token)
    (recipient-validate to)
    (ensure-transfer-authorized token from (recipient-target to))
    (if spender
        (consume-allowance token from spender amount)
        (check-and-update-spending-limit token from amount))
    (unless (validate-inbound-or-block token from to amount nil memo)
      to)))

(defun validate-mint (token sender to amount memo)
  "(values total-supply recipient), or NIL when the mint went to the guard."
  (let ((to (recipient-resolve to)))
    (check-role token sender +issuer-role+)
    (let ((total-supply (total-supply token)))
      (fork-if (:since :t3)
               (progn (check-not-paused token)
                      (recipient-validate to)))
      (let ((role (if (and (eq (evm-kind) :zone) (fork-if (:since :t13) t nil))
                      (auth-role :recipient)
                      (auth-role :mint-recipient))))
        (unless (funcall 'clempo.tip403:authorized-as-p (transfer-policy-id token) (recipient-target to) role)
          (revert 'policy-forbids)))
      (unless (validate-inbound-or-block token sender to amount total-supply memo)
        (values total-supply to)))))

;;; Token operations.

(defun transfer (token sender to amount)
  (let ((to (validate-transfer token nil sender to amount 0)))
    (when to
      (%transfer token sender to amount)
      (emit-virtual-hop token to amount))
    t))

(defun transfer-with-memo (token sender to amount memo)
  (let ((r (validate-transfer token nil sender to amount memo)))
    (when r
      (%transfer token sender r amount)
      (emit token 'transfer-with-memo sender to amount memo)
      (emit-virtual-hop token r amount))
    (values)))

(defun transfer-from (token sender from to amount)
  (let ((r (validate-transfer token sender from to amount 0)))
    (when r
      (%transfer token from r amount)
      (emit-virtual-hop token r amount))
    t))

(defun transfer-from-with-memo (token sender from to amount memo)
  (let ((r (validate-transfer token sender from to amount memo)))
    (when r
      (%transfer token from r amount)
      (emit token 'transfer-with-memo from to amount memo)
      (emit-virtual-hop token r amount))
    t))

(defun system-transfer-from (token caller from amount)
  "Move AMOUNT from FROM to CALLER without an allowance (TIP-1035: from T5 only for callers
on the implicit approval list)."
  (when (and (fork-if (:since :t5) t nil)
             (not (clempo.address-registry:implicitly-approved-p caller)))
    (revert 'unauthorized))
  (let ((to (validate-transfer token nil from caller amount 0)))
    (when to
      (%transfer token from to amount)
      (emit-virtual-hop token to amount))
    t))

(defun channel-reserve-transfer (token from to amount receive-policy-sender)
  "TIP-1034 custody movement: one endpoint must be the channel reserve; TO is a RECIPIENT."
  (when (and (/= from +channel-reserve-address+)
             (/= (recipient-original to) +channel-reserve-address+))
    (revert 'unauthorized))
  (check-not-paused token)
  (recipient-validate to)
  (check-and-update-spending-limit token from amount)
  (when (= (recipient-target to) +receive-policy-guard-address+)
    (revert 'guard-address-reserved))
  (ensure-receive-policy-authorized token receive-policy-sender (recipient-target to))
  (%transfer token from to amount)
  (emit-virtual-hop token to amount)
  (values))

(defun mint (token sender to amount)
  (multiple-value-bind (total-supply r) (validate-mint token sender to amount 0)
    (when r
      (%mint token r total-supply amount)
      (emit token 'mint to amount)
      (emit-virtual-hop token r amount))
    (values)))

(defun mint-with-memo (token sender to amount memo)
  (multiple-value-bind (total-supply r) (validate-mint token sender to amount memo)
    (when r
      (%mint token r total-supply amount)
      (emit token 'transfer-with-memo 0 to amount memo)
      (emit token 'mint to amount)
      (emit-virtual-hop token r amount))
    (values)))

(defun reduce-supply (token amount)
  (let ((total-supply (total-supply token)))
    (when (< total-supply amount)
      (revert 'insufficient-balance total-supply amount token))
    (write-total-supply token (- total-supply amount))))

(defun %burn (token sender amount)
  (fork-if (:since :t3) (check-not-paused token))
  (check-role token sender +issuer-role+)
  (%transfer token sender (recipient-direct 0) amount)
  (reduce-supply token amount))

(defun burn (token sender amount)
  (%burn token sender amount)
  (emit token 'burn sender amount)
  (values))

(defun burn-with-memo (token sender amount memo)
  (%burn token sender amount)
  (emit token 'transfer-with-memo sender 0 amount memo)
  (emit token 'burn sender amount)
  (values))

(defun check-burn-address (token from)
  "Protected custody accounts cannot be burned from."
  (when (or (= from +fee-manager-address+)
            (= from +stablecoin-dex-address+)
            (fork-if (:since :t5) (or (= from +channel-reserve-address+) (= from token)) nil)
            (fork-if (:since :t6) (= from +receive-policy-guard-address+) nil)
            (fork-if (:since :t12) (= (ash from -64) +zone-portal-prefix+) nil))
    (revert 'protected-address)))

(defun burn-blocked (token sender owner amount check-protected)
  "Burn from OWNER, which TOKEN's policy blocks from sending (from the receive policy guard's
custody when CHECK-PROTECTED is false)."
  (fork-if (:since :t3) (check-not-paused token))
  (check-role token sender +burn-blocked-role+)
  (when check-protected (check-burn-address token owner))
  (when (funcall 'clempo.tip403:authorized-as-p (transfer-policy-id token) owner (auth-role :sender))
    (revert 'policy-forbids))
  (%transfer token (if check-protected owner +receive-policy-guard-address+)
             (recipient-direct 0) amount)
  (reduce-supply token amount)
  (emit token 'burn-blocked owner amount)
  (values))

(defun burn-at (token sender from amount)
  "TIP-1006 (T12): burn from any unprotected account."
  (check-not-paused token)
  (check-role token sender +burn-at-role+)
  (check-burn-address token from)
  (check-and-update-spending-limit token from amount)
  (%transfer token from (recipient-direct 0) amount)
  (reduce-supply token amount)
  (emit token 'burn-at sender from amount)
  (values))

(defun approve (token sender spender amount)
  (clempo.keychain:authorize-approve sender token (allowance token sender spender) amount)
  (write (token-allowances token sender spender) amount)
  (emit token 'approval sender spender amount)
  t)

;;; EIP-2612 (T2).

(defun domain-separator (token)
  (let ((name-hash (keccak (clempo.abi:utf8-encode (name token)))))
    (keccak (clempo.abi:abi-encode '((:bytes-n 32) (:bytes-n 32) (:bytes-n 32) (:uint 256) :address)
                                   (list +eip712-domain-typehash+ name-hash +version-hash+
                                         (chain-id) token)))))

(defun permit (token owner spender value deadline v r s)
  (when (> (block-timestamp) deadline) (revert 'permit-expired))
  (let* ((nonce (permit-nonce token owner))
         (struct-hash (keccak (clempo.abi:abi-encode
                               '((:bytes-n 32) :address :address (:uint 256) (:uint 256) (:uint 256))
                               (list +permit-typehash+ owner spender value nonce deadline))))
         (digest (keccak (octets-concat (octets-of #x19 #x01)
                                        (integer->octets (domain-separator token) 32)
                                        (integer->octets struct-hash 32))))
         (recovered (recover-signer digest v r s)))
    (unless (and recovered (= recovered owner)) (revert 'invalid-signature))
    (when (retired-root-p owner) (revert 'root-key-retired owner))
    (write (token-permit-nonces token owner) (checked+ nonce 1))
    (write (token-allowances token owner spender) value)
    (emit token 'approval owner spender value)
    (values)))

;;; Administration.

(defun change-transfer-policy-id (token sender policy-id)
  (check-role token sender +default-admin-role+)
  (unless (funcall 'clempo.tip403:policy-exists-p policy-id) (revert 'invalid-transfer-policy-id))
  (fork-if (:since :t9)
           (funcall 'clempo.tip403:set-token-transfer-policy token policy-id)
           (write (token-transfer-policy-id token) policy-id))
  (emit token 'transfer-policy-update sender policy-id)
  (values))

(defun set-supply-cap (token sender new-cap)
  (check-role token sender +default-admin-role+)
  (when (< new-cap (total-supply token)) (revert 'invalid-supply-cap))
  (when (> new-cap +u128-max+) (revert 'supply-cap-exceeded))
  (write (token-supply-cap token) new-cap)
  (emit token 'supply-cap-update sender new-cap)
  (values))

(defun pause (token sender)
  (check-role token sender +pause-role+)
  (write (token-paused token) t)
  (emit token 'pause-state-update sender t)
  (values))

(defun unpause (token sender)
  (check-role token sender +unpause-role+)
  (write (token-paused token) nil)
  (emit token 'pause-state-update sender nil)
  (values))

(defun set-next-quote-token (token sender new-quote-token)
  (check-role token sender +default-admin-role+)
  (when (= token +path-usd-address+) (revert 'invalid-quote-token))
  (unless (clempo.tip20-factory:tip20-p new-quote-token) (revert 'invalid-quote-token))
  (when (and (string= (currency token) "USD")
             (string/= (currency (from-address new-quote-token)) "USD"))
    (revert 'invalid-quote-token))
  (write (token-next-quote-token token) new-quote-token)
  (emit token 'next-quote-token-set sender new-quote-token)
  (values))

(defun complete-quote-token-update (token sender)
  (check-role token sender +default-admin-role+)
  (let ((next (next-quote-token token)))
    (loop for current = next then (quote-token (from-address current))
          until (= current +path-usd-address+)
          when (= current token) do (revert 'invalid-quote-token))
    (write (token-quote-token token) next)
    (emit token 'quote-token-update sender next)
    (values)))

;;; TIP-1026 logo URI.

(defconstant +max-logo-uri-bytes+ 256)

(defun ascii-alpha-p (b) (or (<= 65 b 90) (<= 97 b 122)))
(defun ascii-alnum-p (b) (or (ascii-alpha-p b) (<= 48 b 57)))

(defun allowed-logo-uri-p (bytes)
  (let ((colon (position 58 bytes)))
    (and colon (plusp colon)
         (ascii-alpha-p (aref bytes 0))
         (loop for i from 1 below colon
               always (let ((b (aref bytes i))) (or (ascii-alnum-p b) (member b '(43 45 46)))))
         (let ((scheme (map 'string #'code-char (subseq bytes 0 colon))))
           (some (lambda (s) (string-equal scheme s)) '("https" "http" "ipfs" "data"))))))

(defun validate-logo-uri (uri)
  (let ((bytes (clempo.abi:utf8-encode uri)))
    (when (> (length bytes) +max-logo-uri-bytes+) (revert 'logo-uri-too-long))
    (when (and (plusp (length bytes)) (not (allowed-logo-uri-p bytes)))
      (revert 'invalid-logo-uri))))

(defun write-logo-uri (token updater uri)
  "Validate, store and log URI: no role check (the caller's job)."
  (validate-logo-uri uri)
  (write (token-logo-uri token) uri)
  (emit token 'logo-uri-updated updater uri)
  (values))

;;; Initialization (TIP20Token::initialize, called by the factory).

(defun initialize-token (token sender name symbol currency quote-token admin)
  (set-code token (octets-of #xef))
  (write (token-name token) name)
  (write (token-symbol token) symbol)
  (write (token-currency token) currency)
  (write (token-quote-token token) quote-token)
  (write (token-next-quote-token token) quote-token)
  (write (token-supply-cap token) +u128-max+)
  (fork-if (:since :t9)
           (funcall 'clempo.tip403:set-token-transfer-policy token +allow-all-policy-id+)
           (write (token-transfer-policy-id token) +allow-all-policy-id+))
  (initialize-roles token)
  (grant-default-admin token sender admin)
  (values))

;;; Fees (the fee manager's pre- and post-transaction transfers).

(defun adjust-opted-in-supply (token delta)
  "opted_in_supply += DELTA (signed), checked as U256 then narrowed to u128 (Panic 0x11)."
  (let ((new (+ (opted-in-supply token) delta)))
    (when (or (minusp new) (> new +u128-max+)) (panic +panic-under-overflow+))
    (write (token-opted-in-supply token) new)))

(defun transfer-fee-pre-tx (token from amount)
  (check-not-paused token)
  (check-and-update-spending-limit token from amount)
  (unless (zerop (update-rewards token from))
    (adjust-opted-in-supply token (- amount)))
  (decrement-balance token from amount)
  (increment-balance token +fee-manager-address+ amount)
  (values))

(defun transfer-fee-post-tx (token to refund actual-spending)
  (emit token 'transfer to +fee-manager-address+ actual-spending)
  (unless (zerop refund)
    (fork-if (:since :t1c) (clempo.keychain:refund-spending-limit to token refund))
    (unless (zerop (update-rewards token to))
      (adjust-opted-in-supply token refund))
    (decrement-balance token +fee-manager-address+ refund)
    (increment-balance token to refund))
  (values))

;;; TIP-1028 release from the receive policy guard. RECOVERY-MODE is :originator, :receiver
;;; or :third-party.

(defun release-blocked-funds (token originator receiver to amount recovery-mode recovery-auth)
  (check-not-paused token)
  (let ((destination (recipient-resolve to)))
    (recipient-validate destination)
    (if (or (eq recovery-mode :originator) (/= to receiver))
        (let ((subject (if (eq recovery-mode :originator) originator receiver)))
          (ensure-transfer-authorized token subject (recipient-target destination))
          (when (funcall 'clempo.tip403:check-receive-policy token subject (recipient-target destination))
            (revert 'policy-forbids))
          (unless (eq recovery-mode :third-party)
            (check-and-update-spending-limit token recovery-auth amount)))
        (ensure-authorized-as token (list (cons (recipient-target destination)
                                                (auth-role :recipient)))))
    (%transfer token +receive-policy-guard-address+ destination amount)
    (emit-virtual-hop token destination amount)
    (values)))

;;; The ABI (tip20/dispatch.rs).

(defdispatch tip20-token (self input sender)
  (charge-input-cost input)
  (unless (handler-case (initialized-p self)
            (precompile-condition (c) (fork-if (:since :t4) (error c) nil)))
    (revert 'uninitialized))
  (dispatch-calls))

(register-tip20-precompile 'tip20-token)

(defcall tip20-token name* () (:sol "name()" :returns "string") (name self))
(defcall tip20-token symbol* () (:sol "symbol()" :returns "string") (symbol self))
(defcall tip20-token decimals () (:sol "decimals()" :returns "uint8") +decimals+)
(defcall tip20-token currency* () (:sol "currency()" :returns "string") (currency self))
(defcall tip20-token total-supply* () (:sol "totalSupply()" :returns "uint256") (total-supply self))
(defcall tip20-token supply-cap* () (:sol "supplyCap()" :returns "uint256") (supply-cap self))
(defcall tip20-token transfer-policy-id* () (:sol "transferPolicyId()" :returns "uint64")
  (transfer-policy-id self))
(defcall tip20-token paused* () (:sol "paused()" :returns "bool") (paused-p self))
(defcall tip20-token logo-uri* () (:sol "logoURI()" :returns "string" :since :t5) (logo-uri self))
(defcall tip20-token balance-of* ((account address)) (:sol "balanceOf(address account)" :returns "uint256")
  (balance-of self account))
(defcall tip20-token allowance* ((owner address) (spender address))
    (:sol "allowance(address owner, address spender)" :returns "uint256")
  (allowance self owner spender))
(defcall tip20-token quote-token* () (:sol "quoteToken()" :returns "address") (quote-token self))
(defcall tip20-token next-quote-token* () (:sol "nextQuoteToken()" :returns "address")
  (next-quote-token self))
(defcall tip20-token pause-role () (:sol "PAUSE_ROLE()" :returns "bytes32") +pause-role+)
(defcall tip20-token unpause-role () (:sol "UNPAUSE_ROLE()" :returns "bytes32") +unpause-role+)
(defcall tip20-token issuer-role () (:sol "ISSUER_ROLE()" :returns "bytes32") +issuer-role+)
(defcall tip20-token burn-blocked-role () (:sol "BURN_BLOCKED_ROLE()" :returns "bytes32")
  +burn-blocked-role+)
(defcall tip20-token burn-at-role () (:sol "BURN_AT_ROLE()" :returns "bytes32" :since :t12)
  +burn-at-role+)

(defcall tip20-token transfer-from* ((from address) (to address) (amount u256))
    (:sol "transferFrom(address from, address to, uint256 amount)" :returns "bool" :mutates t)
  (transfer-from self sender from to amount))
(defcall tip20-token transfer* ((to address) (amount u256))
    (:sol "transfer(address to, uint256 amount)" :returns "bool" :mutates t)
  (transfer self sender to amount))
(defcall tip20-token approve* ((spender address) (amount u256))
    (:sol "approve(address spender, uint256 amount)" :returns "bool" :mutates t)
  (approve self sender spender amount))
(defcall tip20-token change-transfer-policy-id* ((policy-id u64))
    (:sol "changeTransferPolicyId(uint64 newPolicyId)" :mutates t)
  (change-transfer-policy-id self sender policy-id))
(defcall tip20-token set-supply-cap* ((cap u256))
    (:sol "setSupplyCap(uint256 newSupplyCap)" :mutates t)
  (set-supply-cap self sender cap))
(defcall tip20-token set-logo-uri* ((uri string))
    (:sol "setLogoURI(string newLogoURI)" :mutates t :since :t5)
  (check-role self sender +default-admin-role+)
  (write-logo-uri self sender uri))
(defcall tip20-token pause* () (:sol "pause()" :mutates t) (pause self sender))
(defcall tip20-token unpause* () (:sol "unpause()" :mutates t) (unpause self sender))
(defcall tip20-token set-next-quote-token* ((quote address))
    (:sol "setNextQuoteToken(address newQuoteToken)" :mutates t)
  (set-next-quote-token self sender quote))
(defcall tip20-token complete-quote-token-update* ()
    (:sol "completeQuoteTokenUpdate()" :mutates t)
  (complete-quote-token-update self sender))
(defcall tip20-token mint* ((to address) (amount u256))
    (:sol "mint(address to, uint256 amount)" :mutates t)
  (mint self sender to amount))
(defcall tip20-token mint-with-memo* ((to address) (amount u256) (memo b256))
    (:sol "mintWithMemo(address to, uint256 amount, bytes32 memo)" :mutates t)
  (mint-with-memo self sender to amount memo))
(defcall tip20-token burn* ((amount u256)) (:sol "burn(uint256 amount)" :mutates t)
  (burn self sender amount))
(defcall tip20-token burn-with-memo* ((amount u256) (memo b256))
    (:sol "burnWithMemo(uint256 amount, bytes32 memo)" :mutates t)
  (burn-with-memo self sender amount memo))
(defcall tip20-token burn-blocked* ((from address) (amount u256))
    (:sol "burnBlocked(address from, uint256 amount)" :mutates t)
  (burn-blocked self sender from amount t))
(defcall tip20-token burn-at* ((from address) (amount u256))
    (:sol "burnAt(address from, uint256 amount)" :mutates t :since :t12)
  (burn-at self sender from amount))
(defcall tip20-token transfer-with-memo* ((to address) (amount u256) (memo b256))
    (:sol "transferWithMemo(address to, uint256 amount, bytes32 memo)" :mutates t)
  (transfer-with-memo self sender to amount memo))
(defcall tip20-token transfer-from-with-memo* ((from address) (to address) (amount u256) (memo b256))
    (:sol "transferFromWithMemo(address from, address to, uint256 amount, bytes32 memo)"
     :returns "bool" :mutates t)
  (transfer-from-with-memo self sender from to amount memo))

(defcall tip20-token permit* ((owner address) (spender address) (value u256) (deadline u256)
                              (v u8) (r b256) (s b256))
    (:sol "permit(address owner, address spender, uint256 value, uint256 deadline, uint8 v, bytes32 r, bytes32 s)"
     :mutates t :since :t2)
  (permit self owner spender value deadline v r s))
(defcall tip20-token nonces ((owner address))
    (:sol "nonces(address owner)" :returns "uint256" :since :t2)
  (permit-nonce self owner))
(defcall tip20-token domain-separator* ()
    (:sol "DOMAIN_SEPARATOR()" :returns "bytes32" :since :t2)
  (domain-separator self))
