# tests/sync/data

Recorded from Tempo mainnet on 2026-11-21.

- `boundary-44107199.rawblock.hex`: `debug_getRawBlock` of block 44,107,199 (the last block of epoch 2041; its extraData carries the DKG outcome for epoch 2042) from a Rust tempo archive node.
- `boundary-44107199.header.json`: `eth_getBlockByNumber` number, hash and state root of the same block.
- `boundary-44107199.finalization.json`: `consensus_getFinalization {"height": 44107199}` from `https://rpc.tempo.xyz`.
- `boundary-44128799.*`: the same three records for block 44,128,799 (the last block of epoch 2042, whose certificate verifies under the mainnet network identity).
