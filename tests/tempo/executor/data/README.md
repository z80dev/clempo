# Executor fixtures

Recorded from the private mainnet archive node (tempo/v1.19.0-568b4fb, `CLEMPO_RPC=http://127.0.0.1:8545`), 2026-11-21.

| File | Source |
|---|---|
| `block-1.hex`, `block-2.hex`, `block-3.hex`, `block-3210.hex`, `block-3211.hex` | `debug_getRawBlock` for mainnet blocks 1, 2, 3, 3210, 3211 (hex, one line) |
| `prestate-diff-3211.json` | `.result` of `debug_traceBlockByNumber 0xc8b {"tracer":"prestateTracer","tracerConfig":{"diffMode":true}}` |

Blocks 1..3210 carry only the end-of-block subblocks metadata system transaction and the genesis
state root; 3211 is the first block with a user transaction (state root
`0x7a91f968a770f34f613d35abc602fad42d51ec26afc7040155b34f7d06a0c70c`).
