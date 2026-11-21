# Validator wire reference data

Codec specification read independently from the local Rust reference:

- Tempo `crates/consensus/src/consensus/block.rs`: channel 3 is raw execution
  block RLP; it is not Bytes-prefixed.
- commonware-resolver 2026.10.1 `src/p2p/wire.rs`: fixed big-endian u64 ID,
  u8 request/response/error tag, request key or usize-prefixed response bytes.
- commonware-consensus 2026.10.1 `src/simplex/actors/resolver/`: channel 2
  uses a fixed-width U64 view key and certificate response; Tempo adds epoch
  varint mux framing.
- commonware-consensus 2026.10.1 `src/marshal/resolver/handler.rs` and
  `src/marshal/standard/mod.rs`: block/digest, finalized/height and
  notarized/round request keys; responses are block or `(certificate, block)`
  tuples, with no enum tag around the certificate. Channel 4 has no epoch mux.

The tests share `tests/consensus/data/rust_golden.json`; its provenance is
documented in `tests/consensus/data/README.md`. No network requests are needed.

Tests also consume existing `tests/p2p/data/tempo_devnet_frames.json` channel 3
frames; their original capture provenance is in `tests/p2p/data/README.md`.

The wire layer checks complete framing, canonical primitive encodings, block
structure, requested digest/height/round and certificate payload commitment.
It does not authenticate signatures or execute blocks: callers must verify
certificates with `clempo.consensus` and validate execution before accepting a
branch. A notarized response is not a finalized-chain assertion.
