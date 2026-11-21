# Type fixtures

`blocks.jsonl` is copied unchanged from goempo's repository, `internal/types/testdata/blocks.jsonl`. The accompanying upstream README records mainnet `https://rpc.tempo.xyz` and moderato `https://rpc.moderato.tempo.xyz` responses fetched 2026-11-17: `debug_getRawBlock`, `eth_getBlockByNumber`, and `eth_getBlockReceipts`. These are the smallest recorded blocks for transaction/signature variants. Only fixture data, not implementation code, was reused.

Transaction and signature fixture provenance is recorded in the adjacent `transaction-provenance.md` and signature provenance file.

Consensus receipts are Ethereum typed receipts (including type 0x76); Tempo `feeToken` and `feePayer` are RPC metadata, not additional consensus RLP fields. This follows `crates/primitives/src/lib.rs` and `crates/alloy/src/rpc/receipt.rs` in the Rust reference.

`t14-devnet-transactions.json` holds every Tempo (0x76) transaction a local reference devnet
(tempoxyz/tempo 568b4fb4 `tempo` release build, `scripts/devnet/devnet.sh provision --hardfork
t15`, chain 1337, 2026-11-21) accepted and sealed after `bin/clempo-dev devnet-t14-send`
submitted them: an upgradeAccount call by a primitive root, a transaction from the upgraded
account signed by its owner quorum (ConfigurableAccount signature, version 1), and a fresh
2-of-2 derived account's first transaction with a sponsored fee. Fields: block, hash, `from`
(the reference's recovered sender), raw envelope (`eth_getRawTransactionByHash`), receipt status
and gas used. `bin/clempo-dev devnet-replay` executed all 338 blocks of that devnet from its
genesis and matched every gas used, receipts root, logs bloom and state root.
