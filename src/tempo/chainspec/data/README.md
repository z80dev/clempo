`presto.json` (Tempo mainnet) and `moderato.json` (Tempo testnet) are
unchanged copies of `crates/chainspec/src/genesis/{presto,moderato}.json`
from [Tempo commit 568b4fb](https://github.com/tempoxyz/tempo/tree/568b4fb4258c27a08eb193b154f9ca796ce7ff0a/crates/chainspec/src/genesis).

Fork timestamps come from each JSON `config`, the source read by the Rust
Tempo chainspec. Moderato has several timestamps that differ from the
standalone convenience constants in `crates/hardfork/src/constants.rs` at that
revision; the chain JSON is authoritative for block execution.
