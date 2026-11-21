| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| keys | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:address clempo.keychain:authorized-key)) | 0 | 0 | full |  |
| spending-limits | (clempo.contract:mapping clempo.contract:b256 (clempo.contract:mapping clempo.contract:address clempo.keychain::spending-limit-state)) | 1 | 0 | full |  |
| key-scopes | (clempo.contract:mapping clempo.contract:b256 clempo.keychain::key-scope) | 2 | 0 | full |  |
| key-authorization-witnesses | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:b256 clempo.contract:bool)) | 3 | 0 | full |  |
| transaction-key | clempo.contract:address | 4 | 0 | full |  |
| tx-origin | clempo.contract:address | 5 | 0 | full |  |
