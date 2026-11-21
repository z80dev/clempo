| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| config | clempo.validator-config-v2::config | 0 | 0 | full |  |
| validators | (clempo.contract:vec clempo.validator-config-v2::validator-record) | 1 | 0 | full |  |
| address-to-index | (clempo.contract:mapping clempo.contract:address clempo.base:u64) | 2 | 0 | full |  |
| pubkey-to-index | (clempo.contract:mapping clempo.contract:b256 clempo.base:u64) | 3 | 0 | full |  |
| next-network-identity-rotation-epoch | clempo.base:u64 | 4 | 0 | full |  |
| active-ingress-ips | (clempo.contract:mapping clempo.contract:b256 clempo.contract:bool) | 5 | 0 | full |  |
| active-indices | (clempo.contract:vec clempo.base:u64) | 6 | 0 | full |  |
