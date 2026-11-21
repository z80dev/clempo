| Field | Type | Slot | Offset | Access | Since |
|---|---|---|---|---|---|
| books | (clempo.contract:mapping clempo.contract:b256 clempo.dex::orderbook) | 0 | 0 | full |  |
| orders | (clempo.contract:mapping clempo.contract:u128 clempo.dex::order) | 1 | 0 | full |  |
| balances | (clempo.contract:mapping clempo.contract:address (clempo.contract:mapping clempo.contract:address clempo.contract:u128)) | 2 | 0 | full |  |
| next-order-id | clempo.contract:u128 | 3 | 0 | full |  |
| book-keys | (clempo.contract:vec clempo.contract:b256) | 4 | 0 | full |  |
| dex-storage-credits | (clempo.contract:mapping clempo.contract:address clempo.base:u64) | 5 | 0 | full |  |
