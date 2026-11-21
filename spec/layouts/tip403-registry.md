| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| policy-id-counter | clempo.base:u64 | 0 | 0 | full |  |
| policy-records | (clempo.contract:mapping clempo.base:u64 clempo.tip403::policy-record) | 1 | 0 | full |  |
| policy-set | (clempo.contract:mapping clempo.base:u64 (clempo.contract:mapping clempo.contract:address clempo.contract:bool)) | 2 | 0 | full |  |
| receive-policies | (clempo.contract:mapping clempo.contract:address clempo.tip403::receive-policy) | 3 | 0 | full | T6 |
| token-transfer-policies | (clempo.contract:mapping clempo.contract:address clempo.tip403::token-transfer-policy) | 4 | 0 | full | T9 |
