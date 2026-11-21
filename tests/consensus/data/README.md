# Consensus fixtures

Copied from goempo's checked-in test data (data only), with their
original provenance:

- `rust_golden.json` (goempo internal/consensus/wire/testdata): produced by a Rust harness against
  commonware 2026.10.0 (tempo's pin) with `wire-harness gen`: 4 ed25519 keys and a BLS12-381
  MinSig threshold sharing from TestRng seed 0x7e5d0 (dkg::deal, NonZeroCounter), namespace TEMPO;
  every simplex Vote and Certificate variant for 3 rounds with tempo's channel framing (votes and
  certificates behind the epoch mux varint), plus resolver, broadcast and marshal frames.
  `output` is the dealing's DKG Output; `shares` are varint index || 32-byte scalar.
- `mainnet_finalizations.jsonl` (goempo internal/consensus/testdata): consensus_getFinalization
  {"height":N} from https://rpc.tempo.xyz on 2026-11-17 for blocks 42100000, 42098400 (first
  block of epoch 1949) and 44107200 (first block of epoch 2042), and 44107199 (last block of
  epoch 2041, from a private mainnet RPC node).
- `mainnet_block_42098399_extradata.hex`, `mainnet_block_44085599_extradata.hex`,
  `mainnet_block_44107199_extradata.hex`: extraData of those mainnet boundary blocks (the DKG
  outcomes that install epochs 1949, 2041 and 2042).
- `mainnet_finalized_blocks.json` (goempo internal/consensus/wire/testdata): mainnet blocks
  44107200 and 42100000 (debug_getRawBlock from https://rpc.tempo.xyz on 2026-11-17) with their
  consensus_getFinalization certificates.
