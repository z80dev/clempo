# Simplex test data

Recorded data only (no code).

- `mainnet_elector.txt`: copied from goempo `internal/validator/testdata/mainnet_elector.txt`
  (commit 92e996c). One row per mainnet block: `version epoch height finalization rawHeader`
  for blocks 42100000-42100003 (epoch 1949, before T12: elector V0) and 44107200-44107205
  (epoch 2042, after T12: V1, starting at view 1). `finalization` is
  `consensus_getFinalization {"height":N}.certificate` and `rawHeader` is
  `debug_getRawHeader`, both from https://rpc.tempo.xyz on 2026-11-17.

The participants of those epochs come from the boundary DKG outcomes in
`tests/consensus/dkg/data/` (`mainnet_block_42098399_extradata.hex` installs epoch 1949,
`mainnet_block_44107199_extradata.hex` installs epoch 2042); the threshold sharing for the
voter tests is the Rust harness's in `tests/consensus/data/rust_golden.json`.
