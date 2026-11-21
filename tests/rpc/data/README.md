`number-params.jsonl`: JSON numbers (fractions, exponents, `-0`, values beyond u64 and
i64, beyond the double range) given to parameters of 16 method shapes, one request per
line with the reference's answer. Recorded on 2026-11-21 over HTTP from a
Tempo `568b4fb425` mainnet archive node.
Only the requests it answered with -32602 Invalid params are kept, so the answers do not
depend on chain state.

`bal-12290227.json`: the reference's `eth_getBlockAccessList` (`json`) and
`eth_getBlockAccessListRaw` (`raw`) answers for mainnet block 12,290,227 (`0xbb88b3`: a
pre-execution marker code change, storage changes and reads, the system-call accounts),
recorded on 2026-11-21 over HTTP from a Tempo `568b4fb425` mainnet archive node.
