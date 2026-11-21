| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| validator-tokens | (clempo.contract:mapping clempo.contract:address clempo.contract:address) | 0 | 0 | full |  |
| user-tokens | (clempo.contract:mapping clempo.contract:address clempo.contract:address) | 1 | 0 | full |  |
| collected-fees | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:address clempo.base:u256)) | 2 | 0 | full |  |
| pools | (clempo.contract:mapping clempo.contract:b256 clempo.fee-manager::pool) | 3 | 0 | full |  |
| total-supply | (clempo.contract:mapping clempo.contract:b256 clempo.base:u256) | 4 | 0 | full |  |
| liquidity-balances | (clempo.contract:mapping clempo.contract:b256 (clempo.contract:mapping clempo.contract:address clempo.base:u256)) | 5 | 0 | full |  |
| pending-fee-swap-reservation | (clempo.contract:mapping clempo.contract:b256 clempo.contract:u128) | 6 | 0 | full | T1C |
| two-hop-intermediate | clempo.contract:address | 7 | 0 | full | T5 |
