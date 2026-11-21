# Frontier

| Rung | Score | Commit | Date |
|---|---|---|---|
| R0 Gate | scaffold: base, io, keccak, RLP, JSON, http, JSON-RPC client, layer check | - | 2026-11-20 |
| R1 Decode | - | | |
| R2 Genesis | yes: matching state roots and block hashes on mainnet and Moderato | `ec20b6b` | 2026-11-21 |
| R3 Replay | - | | |
| R3e EVM | - | | |
| R3p Precompiles | - | | |
| R4 Follow | - | | |
| R5 RPC | - | | |
| R6 Validator | - | | |
| R7 Speed | - | | |

## Evidence

Keccak-256 (macro-unrolled Keccak-f over 25 typed lane variables): 250 MB/s on 64 MiB with no allocation, 0.57 µs per 64-byte hash, SBCL 2.6.8 release build on the dev host (Apple Silicon, 16 cores).

R2: `CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev score -rung R2 -json`
compared both computed state roots and Tempo genesis block hashes against live
`eth_getBlockByNumber("0x0", false)` on mainnet and Moderato.

Merkle Patricia trie: 1,000,000 seeded random 32-byte keys with 32-byte values,
single-threaded SBCL 2.6.8 release build on Apple M3 Max. Insertions took 0.382 s
(2.61 million/s; 196.5 MB consed), the first root took 1.516 s wall time
(0.365 s GC; 808.8 MB consed), and recomputing after one value update took
18 µs CPU. Key generation was excluded. Reproduce with the command at the
top of `tests/trie/benchmark.lisp`.
