# Frontier

| Rung | Score | Commit | Date |
|---|---|---|---|
| R0 Gate | scaffold: base, io, keccak, RLP, JSON, http, JSON-RPC client, layer check | - | 2026-11-20 |
| R1 Decode | - | | |
| R2 Genesis | - | | |
| R3 Replay | - | | |
| R3e EVM | - | | |
| R3p Precompiles | - | | |
| R4 Follow | - | | |
| R5 RPC | - | | |
| R6 Validator | - | | |
| R7 Speed | - | | |

## Evidence

Keccak-256 (macro-unrolled Keccak-f over 25 typed lane variables): 250 MB/s on 64 MiB with no allocation, 0.57 µs per 64-byte hash, SBCL 2.6.8 release build on the dev host (Apple Silicon, 16 cores).
