| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| roles | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:b256 clempo.contract:bool)) | 0 | 0 | full |  |
| role-admins | (clempo.contract:mapping clempo.contract:b256 clempo.contract:b256) | 1 | 0 | full |  |
| name | common-lisp:string | 2 | 0 | full |  |
| symbol | common-lisp:string | 3 | 0 | full |  |
| currency | common-lisp:string | 4 | 0 | full |  |
| logo-uri | common-lisp:string | 5 | 0 | full | T5 |
| quote-token | clempo.contract:address | 6 | 0 | full |  |
| next-quote-token | clempo.contract:address | 7 | 0 | packed |  |
| transfer-policy-id | clempo.base:u64 | 7 | 20 | packed |  |
| total-supply | clempo.base:u256 | 8 | 0 | full |  |
| balances | (clempo.contract:mapping clempo.contract:address clempo.base:u256) | 9 | 0 | full |  |
| allowances | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:address clempo.base:u256)) | 10 | 0 | full |  |
| permit-nonces | (clempo.contract:mapping clempo.contract:address clempo.base:u256) | 11 | 0 | full |  |
| paused | clempo.contract:bool | 12 | 0 | full |  |
| supply-cap | clempo.base:u256 | 13 | 0 | full |  |
| salts (reserved) | (clempo.contract:mapping clempo.contract:b256 clempo.contract:bool) | 14 | 0 | full |  |
| global-reward-per-token | clempo.base:u256 | 15 | 0 | full |  |
| opted-in-supply | clempo.contract:u128 | 16 | 0 | full |  |
| user-reward-info | (clempo.contract:mapping clempo.contract:address clempo.tip20::user-reward-info) | 17 | 0 | full |  |
