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

## Consensus key files (tests/validator/keys.lisp)

- `localnet-key/`: throwaway localnet keys for validator slot 127.0.0.1:21000, copied
  unchanged from `~/cache/clempo/devnet-r6-1795238891/` (written by the Rust reference's
  `tempo-xtask generate-localnet --seed 42` while provisioning a 4-validator devnet;
  `provision.log` there records the run). `signing.key` is the age v1 scrypt-encrypted
  ed25519 key written by `SigningKey::write_to_file_encrypted` (age 0.11, work factor 15)
  under the passphrase in `localnet.secret`; `signing.seed` is the same 32-byte key raw,
  `signing.public` its public key in hex, `signing.share` the BLS SigningShare
  (0x-hex of commonware `Share`). These keys guard nothing.
- `vcv2_mainnet_calls.txt`: every mainnet `addValidator`/`rotateValidator` call to
  ValidatorConfigV2 (0xcccc...0001) from genesis to block 49403945 (14 calls), one per line
  as `chainId block calldata txhash`. Copied from goempo's
  `internal/validator/testdata/vcv2_mainnet_calls.txt`, recorded from mainnet `eth_getLogs`
  and `debug_traceTransaction` at https://rpc.tempo.xyz (data reuse per NOTICE.md).
- `vcv2_mainnet_rotate_validators.txt`: `txhash validatorAddress` for each rotate call above;
  the address (signed over but absent from rotateValidator calldata) is topic 3 of the
  ValidatorRotated log in `eth_getTransactionReceipt`, read from the private mainnet archive
  node (CLEMPO_RPC) on 2026-11-21.
