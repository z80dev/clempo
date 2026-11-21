(defpackage #:clempo.contract
  (:use #:cl #:clempo.base #:clempo.forks)
  (:shadow #:read #:write #:delete)
  (:documentation "Precompile storage as data (docs/DEFCONTRACT.md): DEFCONTRACT, DEFSTORABLE
and DEFENUM compute Solidity-compatible layouts at macroexpansion time; READ, WRITE and
DELETE are the only verbs that touch storage, through the backend in *STORAGE*; DEFCALL,
DEFERROR, DEFEVENT and DEFDISPATCH generate the ABI dispatcher of
crates/precompiles/src/dispatch.rs. Backends: METERED-STORAGE (exact gas of
storage/evm.rs over an abstract HOST), HASHMAP-STORAGE, RECORDING-STORAGE.")
  (:export
   ;; conditions
   #:precompile-condition #:precompile-revert #:precompile-revert-data #:precompile-revert-name
   #:precompile-revert-args #:precompile-halt #:precompile-halt-reason
   #:precompile-fatal #:precompile-fatal-message
   #:revert #:revert-data #:halt #:fatal #:panic #:out-of-gas #:error-data
   #:+panic-assert+ #:+panic-under-overflow+ #:+panic-division-by-zero+
   #:+panic-enum-conversion+ #:+panic-array-oob+
   #:checked+ #:checked- #:checked* #:checked-div #:checked-mul-div
   #:u64-max #:u128-max
   ;; context and backend protocol
   #:*storage* #:storage-backend
   #:backend-sload #:backend-sstore #:backend-tload #:backend-tstore #:backend-emit-log
   #:backend-deduct-gas #:backend-refund-gas #:backend-gas-limit #:backend-gas-used
   #:backend-gas-refunded #:backend-account #:backend-warm-caller-account #:backend-set-code
   #:backend-static-p #:backend-block #:backend-checkpoint #:backend-commit #:backend-revert
   #:backend-config-commitment #:backend-set-config-commitment #:backend-call
   #:backend-set-tip1060-credits #:backend-set-tip1060-credit-minting #:backend-kind
   #:backend-logs
   #:sload #:sstore #:tload #:tstore #:emit-log #:deduct-gas #:refund-gas
   #:gas-limit #:gas-used #:gas-remaining #:gas-refunded
   #:account-info #:account-info-p #:make-account-info #:account-info-nonce
   #:account-info-balance #:account-info-code #:account-info-code-hash
   #:account-info-commitment #:account-empty-p #:account-has-code-p
   #:account #:warm-caller-account #:initialized-p #:set-code
   #:keccak #:keccak-octets #:recover-signer #:static-p #:ensure-not-static
   #:block-timestamp #:block-number #:block-beneficiary #:chain-id #:block-env
   #:with-checkpoint #:config-commitment #:set-config-commitment #:retired-root-p
   #:nested-call #:set-tip1060-credits #:set-tip1060-credit-minting #:evm-kind
   #:+keccak-empty+ #:+keccak-empty-octets+
   ;; host protocol (implemented by the executor over clempo.state; MEMORY-HOST here)
   #:host-sload #:host-sstore #:host-tload #:host-tstore #:host-account #:host-peek-account
   #:host-set-code #:host-log #:host-logs #:host-checkpoint #:host-commit #:host-revert
   #:host-config-commitment #:host-set-config-commitment #:host-block #:host-call
   #:cold-load-skipped
   #:memory-host #:make-memory-host #:memory-host-put-account #:memory-host-put-storage
   #:memory-host-put-transient #:memory-host-warm-address #:memory-host-storage
   #:memory-host-accounts #:memory-host-account #:memory-host-original
   #:memory-host-commitments #:memory-host-block
   #:block-info #:make-block-info #:block-info-number #:block-info-timestamp
   #:block-info-beneficiary #:block-info-chain-id
   ;; backends
   #:metered-storage #:make-metered-storage #:metered-storage-host #:metered-storage-refunded
   #:metered-storage-spent #:metered-storage-limit
   #:hashmap-storage #:make-hashmap-storage #:hashmap-storage-slots #:hashmap-storage-logs
   #:recording-storage #:make-recording-storage #:recording-storage-trace
   #:non-creditable-slots #:storage-credits-address #:+storage-credits-address+
   #:sstore-storage-credits
   #:with-storage
   ;; storable types and places
   #:address #:bool #:b256 #:bytes #:mapping #:vec #:set #:array
   #:u8 #:u16 #:u24 #:u32 #:u40 #:u48 #:u56 #:u64 #:u72 #:u80 #:u88 #:u96 #:u104
   #:u112 #:u120 #:u128 #:u160 #:u192 #:u224 #:u256
   #:i8 #:i16 #:i24 #:i32 #:i64 #:i128 #:i256
   #:b1 #:b2 #:b3 #:b4 #:b8 #:b12 #:b16 #:b20 #:b24
   #:place #:place-p #:make-place #:place-address #:place-slot #:place-offset #:place-type
   #:read #:write #:delete #:t-read #:t-write #:t-delete #:sinc #:sdec
   #:at #:field #:vec-length #:vec-push #:vec-pop #:vec-at #:vec-element
   #:set-contains #:set-insert #:set-remove #:set-length #:set-at #:set-range
   #:mapping-slot #:mapping-key-word #:type-slots #:type-bytes #:type-packable-p
   #:type-dynamic-p #:string-length
   ;; layouts
   #:defstorable #:defenum #:defcontract #:layout-of #:layout-fields #:layout-field
   #:field-name #:field-type #:field-slot #:field-offset #:field-bytes #:field-access
   #:field-since #:field-label #:field-reserved #:field-doc
   #:layout-json #:layout-markdown #:contract-address #:contract-source-file #:enum-count
   #:enum-value #:enum-name #:contracts
   ;; abi surface
   #:deferror #:defevent #:defcall #:defdispatch #:emit #:dispatch-calls
   #:charge-input-cost #:has-duplicates-metered #:call-entry #:contract-calls
   #:contract-dispatcher #:contract-abi))
