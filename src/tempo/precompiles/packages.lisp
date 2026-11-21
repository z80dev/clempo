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

(defpackage #:clempo.oracle
  (:use #:cl #:clempo.base #:clempo.forks #:clempo.contract #:clempo.precompiles)
  (:shadowing-import-from #:clempo.contract #:read #:write #:delete)
  (:documentation "zempo's reference-oracle corpus of precompile calls (R3p): one case is a
pre-state and a call; the expectation is the reference's status, output, gas, refund, logs,
changed slots and changed accounts. Pure: callers supply the JSON lines.")
  (:export #:run-oracle-case #:run-oracle-lines))
