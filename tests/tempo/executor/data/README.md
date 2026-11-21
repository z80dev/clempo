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
| `block-32443199.hex`, `block-35816049.hex`, `block-47250841.hex`, `block-47250842.hex` | `debug_getRawBlock` (system-calls-tests): the first T8 epoch boundary (empty block, epoch 1502 outcome in extraData), the T10 activation block, the T13 activation block and the block after it |
| `system-paths.sexp` | Minimal pre/post states of those four blocks for the system paths (EIP-2935 `0x0000F908…2935`, EIP-4788 `0x000F3df6…Beac02`, CurrentCommittee `0xC077E…`, ZoneFactory `0x5AF2…`, the zone runtimes `0x5AD1…`/`0x5A56…`/`0x5A4D…`): `eth_getTransactionCount` and code hash (`web3_sha3` of `eth_getCode`; the code itself when at most 100 bytes) at block−1 and block, `eth_getStorageAt` at block−1 and block for the written slots (CurrentCommittee slots 0, 1 and keccak(1)+0..15; history slot (block−1) mod 8191; factory slots 0, 1). The EIP-2935 contract's code first appears in block 1,507,667 (`eth_getCode` bisection); the EIP-4788 contract has no code at 50,062,373 |
| `devnet-r6/genesis.json`, `devnet-r6/blocks-1-60.txt` | P2PAgent's T13-genesis devnet (`~/cache/clempo/devnet-r6-1795238891`, four Rust validators at 568b4fb, epoch length 60): `network/genesis.json`, and blocks 1..60 of `raw-through-130.txt` (raw RLP hex, one per line; every block empty, 59 carries the epoch-1 DKG outcome) |
