# Executor fixtures

Recorded from the private mainnet archive node (tempo/v1.19.0-568b4fb, `CLEMPO_RPC=http://127.0.0.1:8545`), 2026-11-21.

| File | Source |
|---|---|
| `block-1.hex`, `block-2.hex`, `block-3.hex`, `block-3210.hex`, `block-3211.hex` | `debug_getRawBlock` for mainnet blocks 1, 2, 3, 3210, 3211 (hex, one line) |
| `prestate-diff-3211.json` | `.result` of `debug_traceBlockByNumber 0xc8b {"tracer":"prestateTracer","tracerConfig":{"diffMode":true}}` |

Blocks 1..3210 carry only the end-of-block subblocks metadata system transaction and the genesis
state root; 3211 is the first block with a user transaction (state root
`0x7a91f968a770f34f613d35abc602fad42d51ec26afc7040155b34f7d06a0c70c`).

| `type4-authorizations.sexp` | Mainnet type-4 transactions `0x438d0874…4637` (block 0x19b2495) and `0x5061291e…5896` (block 0x1a99472): `authorizationList`, `chainId`, `from` of `eth_getTransactionByHash`; each delegated account's `nonce`/`code` from `.pre`/`.post` of `debug_traceTransaction {"tracer":"prestateTracer","tracerConfig":{"diffMode":true}}` (a missing post code means unchanged) |
| `block-2000175.hex`, `block-10000020.hex`, `block-22000056.hex`, `block-26000002.hex`, `block-50000005.hex`, `block-30000000.hex`, `block-50049999.hex` | `debug_getRawBlock` (lanes-tests: payment lane per fork, proposer fee recipient) |
| `vc2-<parent>.txt` | `eth_getStorageAt 0xcccccccc00000000000000000000000000000001 <slot> <parent>` for slot 0, the proposer's `pubkey_to_index` slot and its record's fee-recipient slot ("slot value", hex); keys from blocks 30000000 (for parents 29999999 and 13486705) and 50049999 |
