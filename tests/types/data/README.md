# Type fixtures

`blocks.jsonl` is copied unchanged from goempo's repository, `internal/types/testdata/blocks.jsonl`. The accompanying upstream README records mainnet `https://rpc.tempo.xyz` and moderato `https://rpc.moderato.tempo.xyz` responses fetched 2026-11-17: `debug_getRawBlock`, `eth_getBlockByNumber`, and `eth_getBlockReceipts`. These are the smallest recorded blocks for transaction/signature variants. Only fixture data, not implementation code, was reused.

Transaction and signature fixture provenance is recorded in the adjacent `transaction-provenance.md` and signature provenance file.

Consensus receipts are Ethereum typed receipts (including type 0x76); Tempo `feeToken` and `feePayer` are RPC metadata, not additional consensus RLP fields. This follows `crates/primitives/src/lib.rs` and `crates/alloy/src/rpc/receipt.rs` in the Rust reference.
