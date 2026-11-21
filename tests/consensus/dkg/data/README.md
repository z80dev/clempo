# DKG test data

Recorded data only (no code), from [goempo](https://github.com/gakonst/goempo)
fixtures and [Tempo `568b4fb4258c`](https://github.com/tempoxyz/tempo/tree/568b4fb4258c).
Each fixture's origin:

- `mainnet_block_42098399_extradata.hex`: extraData of mainnet block 42098399
  (eth_getBlockByNumber), the boundary DKG outcome that installs epoch 1949.
  From goempo `internal/consensus/testdata`.
- `mainnet_block_44085599_extradata.hex`, `mainnet_block_44107199_extradata.hex`: extraData of
  mainnet blocks 44085599 and 44107199 (debug_getRawBlock from a private mainnet RPC node), the
  boundary DKG outcomes that install epochs 2041 and 2042. From goempo `internal/consensus/testdata`
  (identical copies in `internal/dkg/testdata`).
- `moderato_block_37735199_extradata.hex`: extraData of moderato testnet block 37735199
  (eth_getBlockByNumber from https://rpc.moderato.tempo.xyz on 2026-11-17), the DKG outcome that
  installs epoch 1747, the testnet network identity's `from_epoch`. From goempo
  `internal/consensus/testdata`.
- `mainnet_block_32443199_extradata.bin`: raw extraData of mainnet block 32443199 (the last block
  of epoch 1501, a T8 boundary) from eth_getBlockByNumber on a mainnet node. From goempo
  `internal/tempo/dkg/testdata/extra-32443199.bin`.
- `rust_messages.json`: produced by goempo's Rust harness against commonware 2026.10.0 (tempo's
  pin) with 4 fixed ed25519 keys and a dealt sharing (`output`, `shares`): tempo DKG channel
  messages (`Dealer` = DealerPubMsg || DealerPrivMsg, `Ack` = PlayerAck, each behind the epoch
  mux varint) for a fresh round (epoch 1) and a reshare round (epoch 2), every dealer to every
  player, plus each dealer's SignedDealerLog. From goempo `internal/dkg/testdata`.
- `mainnet-network-identity.json`, `testnet-network-identity.json`: identities published in
  [tempoxyz/tempo](https://github.com/tempoxyz/tempo).
- `devnet_r6_blocks.jsonl`: an all-Rust 4-validator Tempo devnet (`scripts/devnet`,
  Tempo `568b4fb425` release binary, `generate-localnet --seed 42`, hardfork T13,
  epoch length 60, T12 from genesis, no T15), recorded 2026-11-21. Contains, for heights 0..145, the
  finalized block hash and extraData, identical on all four validators' RPCs. Genesis carries
  the epoch-0 outcome, blocks 32..44 and 92..98 signed dealer logs, 59 and 119 the boundary
  outcomes.
