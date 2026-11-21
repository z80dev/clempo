# Type fixtures

`blocks.jsonl` is copied unchanged from goempo's repository, `internal/types/testdata/blocks.jsonl`. The accompanying upstream README records mainnet `https://rpc.tempo.xyz` and moderato `https://rpc.moderato.tempo.xyz` responses fetched 2026-11-17: `debug_getRawBlock`, `eth_getBlockByNumber`, and `eth_getBlockReceipts`. These are the smallest recorded blocks for transaction/signature variants. Only fixture data, not implementation code, was reused.

Transaction and signature fixture provenance is recorded in the adjacent `transaction-provenance.md` and signature provenance file.

Consensus receipts are Ethereum typed receipts (including type 0x76); Tempo `feeToken` and `feePayer` are RPC metadata, not additional consensus RLP fields. This follows `crates/primitives/src/lib.rs` and `crates/alloy/src/rpc/receipt.rs` in the Rust reference.

`t14-devnet-transactions.json` holds every Tempo (0x76) transaction a local reference devnet
(tempoxyz/tempo 568b4fb4 `tempo` release build, `scripts/devnet/devnet.sh provision --hardfork
t15 --base-port 24000`, chain 1337, 2026-11-21) accepted and sealed after `bin/clempo-dev
devnet-t14-send` submitted them: an upgradeAccount call by a primitive root, a transaction from
the upgraded account signed by its owner quorum (ConfigurableAccount signature, version 1), a
fresh 2-of-2 derived account's first transaction with a sponsored fee, a grant of that derived
account as an access key (key type 3), a send through it (V2 keychain wrapping the 2-of-2
quorum), and two isValidSignature calls on the upgraded account's 0xef marker (owner approval:
0x1626ba7e; stranger: 0xffffffff, per the reference's eth_call). Fields: label, block, hash,
`from` (the reference's recovered sender), raw envelope (`eth_getRawTransactionByHash`),
receipt status and gas used. `bin/clempo-dev devnet-replay` executed all 567 blocks of that
devnet from its genesis and matched every gas used, receipts root, logs bloom and state root.
