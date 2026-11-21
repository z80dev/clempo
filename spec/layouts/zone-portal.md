| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| admin | clempo.contract:address | 0 | 0 | full |  |
| zone-gas-rate | clempo.contract:u128 | 1 | 0 | packed |  |
| withdrawal-batch-index | clempo.base:u64 | 1 | 16 | packed |  |
| block-hash | clempo.contract:b256 | 2 | 0 | full |  |
| current-deposit-queue-hash | clempo.contract:b256 | 3 | 0 | full |  |
| deposit-count | clempo.base:u64 | 4 | 0 | packed |  |
| last-processed-deposit-number | clempo.base:u64 | 4 | 8 | packed |  |
| last-synced-tempo-block-number | clempo.base:u64 | 4 | 16 | packed |  |
| bounceback-gas | clempo.base:u64 | 4 | 24 | packed |  |
| encryption-keys | (clempo.contract:vec clempo.zone-factory::portal-encryption-key) | 5 | 0 | full |  |
| token-configs | (clempo.contract:mapping clempo.contract:address clempo.zone-factory::portal-token-config) | 6 | 0 | full |  |
| enabled-tokens | (clempo.contract:vec clempo.contract:address) | 7 | 0 | full |  |
| refunds | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:address clempo.contract:u128)) | 8 | 0 | full |  |
| withdrawal-queue | clempo.zone-factory::portal-withdrawal-queue | 9 | 0 | full |  |
| rpc-url | common-lisp:string | 12 | 0 | full |  |
| pending-admin | clempo.contract:address | 13 | 0 | full |  |
| withdrawal-reentrancy-status | clempo.base:u256 | 14 | 0 | full |  |
| zone-id | clempo.contract:u32 | 15 | 0 | packed |  |
| messenger | clempo.contract:address | 15 | 4 | packed |  |
| verifier | clempo.contract:address | 16 | 0 | packed |  |
| initialized | clempo.contract:bool | 16 | 20 | packed |  |
| sequencer-set-version | clempo.base:u64 | 16 | 21 | packed |  |
| sequencer-threshold | clempo.contract:u8 | 16 | 29 | packed |  |
| zone-height | clempo.base:u256 | 17 | 0 | full |  |
| sequencers | (clempo.contract:vec clempo.contract:address) | 18 | 0 | full |  |
| reserved-slot-19 (reserved) | clempo.base:u256 | 19 | 0 | full |  |
| role | (clempo.contract:mapping clempo.contract:address clempo.contract:u8) | 20 | 0 | full |  |
| is-access-enforced | clempo.contract:bool | 21 | 0 | packed |  |
| is-gateway-enforced | clempo.contract:bool | 21 | 1 | packed |  |
| reserved (reserved) | clempo.zone-factory::b30 | 21 | 2 | packed |  |
| max-tempo-gas-rate | clempo.contract:u128 | 22 | 0 | full |  |
| leader | clempo.contract:address | 23 | 0 | packed |  |
| leader-epoch | clempo.base:u64 | 23 | 20 | packed |  |
| leader-activation-tempo-block | clempo.base:u64 | 24 | 0 | packed |  |
| deposit-count-block | clempo.base:u64 | 24 | 8 | packed |  |
| deposits-in-current-block | clempo.base:u64 | 24 | 16 | packed |  |
| token-enable-count-block | clempo.base:u64 | 24 | 24 | packed |  |
| tokens-enabled-in-current-block | clempo.base:u64 | 25 | 0 | packed |  |
| pause-expiry | clempo.base:u64 | 25 | 8 | packed |  |
| token-enablement-hash | clempo.contract:b256 | 26 | 0 | full |  |
| abdication-effective-at | (clempo.contract:mapping clempo.contract:u8 clempo.base:u64) | 27 | 0 | full |  |
| last-processed-enabled-token-count | clempo.base:u64 | 28 | 0 | packed |  |
| token-enablement-cursor-initialized | clempo.contract:bool | 28 | 8 | packed | T13 |
| transfers-enabled | clempo.contract:bool | 28 | 9 | packed |  |
| deposit-buckets | (clempo.contract:vec clempo.zone-factory::portal-deposit-bucket) | 29 | 0 | full |  |
| deposit-weight | clempo.base:u64 | 30 | 0 | packed |  |
| settled-deposit-weight | clempo.base:u64 | 30 | 8 | packed |  |
| deposit-price-block | clempo.base:u64 | 30 | 16 | packed |  |
| deposit-price-tier | clempo.base:u64 | 30 | 24 | packed |  |
| deposit-bucket-base | clempo.base:u64 | 31 | 0 | packed |  |
| next-deposit-bucket-to-prune | clempo.base:u64 | 31 | 8 | packed |  |
