;;; Every precompile package, defined up front: the precompiles call each other in cycles
;;; (TIP-20 <-> TIP-403 <-> receive policy guard, keychain <-> signature verifier), so all
;;; packages must exist before any file is read. Each package's block is owned by the file
;;; that implements it; add exports there.

(defpackage #:clempo.precompiles
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "The Tempo precompile set: addresses, activation by fork, and the entry
point the EVM calls (TempoPrecompiles::execute in crates/precompiles/src/lib.rs).")
  (:export
   #:call-precompile #:precompile-address-p #:precompile-contract #:register-precompile
   #:tip20-address-p #:tip20-id #:tip20-address
   #:+path-usd-address+ #:+tip20-factory-address+ #:+tip403-registry-address+
   #:+fee-manager-address+ #:+stablecoin-dex-address+ #:+nonce-address+
   #:+validator-config-address+ #:+validator-config-v2-address+ #:+account-keychain-address+
   #:+address-registry-address+ #:+signature-verifier-address+ #:+receive-policy-guard-address+
   #:+storage-credits-address+ #:+current-committee-address+ #:+configurable-account-address+
   #:+channel-reserve-address+ #:+zone-factory-address+ #:+zone-verifier-address+
   #:+zone-portal-impl-address+ #:+zone-messenger-address+ #:+ousd-address+
   #:+default-fee-token+ #:fallback-fee-tokens #:register-tip20-precompile))

;;; Each precompile package below uses the same packages and shadows READ/WRITE/DELETE
;;; with clempo.contract's storage verbs. Plain DEFPACKAGE forms, so scripts/check-layers.lisp
;;; sees them.

(defpackage #:clempo.nonce
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "NonceManager (0x4E4F4E4345...): 2D nonces and TIP-1009 expiring nonces.")
  (:export #:nonce-manager #:get-nonce #:increment-nonce #:expiring-nonce-seen-p
           #:check-and-mark-expiring-nonce))

;;; Owner of each block: the file(s) named in its documentation. Export the functions other
;;; precompiles or the executor call; names are the reference's method names in kebab case.

(defpackage #:clempo.tip20
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:shadow #:symbol)
  (:documentation "TIP20Token (tip20*.lisp): every 0x20C0... token: ERC-20 core, roles,
rewards, permits, memos, transfer policies, virtual addresses.")
  (:export #:tip20-token #:transfer #:transfer-from #:system-transfer-from #:mint #:burn
           #:burn-blocked #:balance-of #:total-supply #:quote-token #:currency
           #:transfer-policy-id #:legacy-transfer-policy-id #:set-next-quote-token
           #:release-blocked-funds #:write-logo-uri #:initialize-token
           #:transfer-fee-pre-tx #:transfer-fee-post-tx #:has-role #:grant-role))

(defpackage #:clempo.tip20-factory
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "TIP20Factory (tip20-factory.lisp): token creation and the TIP-20 check.")
  (:export #:tip20-factory #:create-token #:tip20-p))

(defpackage #:clempo.address-registry
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "AddressRegistry (address-registry.lisp): TIP-1022 virtual addresses.")
  (:export #:address-registry #:resolve-recipient #:resolve-virtual-address))

(defpackage #:clempo.tip403
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "TIP403Registry (tip403.lisp): transfer and receive policies.")
  (:export #:tip403-registry #:authorized-p #:authorized-as-p #:policy-exists-p #:create-policy
           #:set-token-transfer-policy #:set-receive-policy #:modify-policy-blacklist
           #:registered-token-transfer-policy-id #:check-receive-policy))

(defpackage #:clempo.receive-policy-guard
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ReceivePolicyGuard (receive-policy-guard.lisp): TIP-1028 blocked receipts.")
  (:export #:receive-policy-guard #:balance-of))

(defpackage #:clempo.fee-manager
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "TipFeeManager and the fee AMM (fee-manager*.lisp).")
  (:export #:fee-manager #:user-token #:validator-token #:set-validator-token
           #:collect-fee-pre-tx #:collect-fee-post-tx #:execute-fee-swap #:rebalance-swap
           #:mint #:burn #:get-pool))

(defpackage #:clempo.dex
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "StablecoinDEX (dex*.lisp): the on-chain orderbook.")
  (:export #:stablecoin-dex #:book-key-index #:book-key-for-index))

(defpackage #:clempo.keychain
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "AccountKeychain (keychain*.lisp): access keys, spending limits, call scopes.")
  (:export #:account-keychain #:authorize-key #:revoke-key #:get-key #:authorize-transfer
           #:authorize-approve #:refund-spending-limit #:validate-keychain-authorization
           #:set-transaction-key #:set-tx-origin))

(defpackage #:clempo.signature-verifier
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "SignatureVerifier (signature-verifier.lisp): secp256k1, P-256, WebAuthn.")
  (:export #:signature-verifier #:recover #:verify #:verify-configurable-account))

(defpackage #:clempo.configurable-account
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ConfigurableAccount (configurable-account.lisp): T14 multi-owner accounts.")
  (:export #:configurable-account))

(defpackage #:clempo.storage-credits
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "StorageCredits (storage-credits.lisp): TIP-1060.")
  (:export #:storage-credits #:balance-of #:track-minted-credits #:with-budget #:set-mode))

(defpackage #:clempo.channel-reserve
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "TIP20ChannelReserve (channel-reserve*.lisp): TIP-1034 payment channels.")
  (:export #:channel-reserve #:set-channel-open-context-hash))

(defpackage #:clempo.validator-config
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ValidatorConfig v1 (validator-config.lisp).")
  (:export #:validator-config))

(defpackage #:clempo.validator-config-v2
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ValidatorConfigV2 (validator-config-v2*.lisp).")
  (:export #:validator-config-v2))

(defpackage #:clempo.committee
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "CurrentCommittee (committee.lisp), T8.")
  (:export #:current-committee))

(defpackage #:clempo.zone-factory
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ZoneFactory (zone-factory.lisp), T10.")
  (:export #:zone-factory))

(defpackage #:clempo.zone-verifier
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "ZoneVerifier (zone-verifier*.lisp), T13: Nitro attestations.")
  (:export #:zone-verifier))

(defpackage #:clempo.oracle
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "zempo's reference-oracle corpus of precompile calls (R3p): one case is a
pre-state and a call; the expectation is the reference's status, output, gas, refund, logs,
changed slots and changed accounts. Pure: callers supply the JSON lines.")
  (:export #:run-oracle-case #:run-oracle-lines))
