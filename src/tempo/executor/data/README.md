# Zone runtime bytecode

The shared Zone runtimes the executor installs (`system-calls.lisp`, `*zone-runtimes*`; tempo
`crates/contracts/src/precompiles/zone_factory.rs` `initial_zone_factory_state` and
`t13/t14/t15_zone_factory_state`). One hex string (`0x…`) per file, read when
`system-calls.lisp` is compiled. `tests/tempo/executor/system-calls-tests.lisp`
(`zone-runtimes-pinned-by-code-hash`) pins every file by its keccak-256.

Recorded 2026-11-21 from the private mainnet archive node (tempo/v1.19.0-568b4fb,
`CLEMPO_RPC=http://127.0.0.1:8545`).

| File | Installed at | Source | keccak-256 |
|---|---|---|---|
| `zone-portal-t10.hex` | `0x5AD1…` by T10 | `eth_getCode 0x5AD1000000000000000000000000000000000000 35816049` (unchanged through 47250840) | `0x109c27a8…5fa47b32` |
| `zone-verifier-t10.hex` | `0x5A56…` by T10 | `eth_getCode 0x5A56000000000000000000000000000000000000 35816049` | `0xf3ab166b…7f2822a2` |
| `zone-messenger-t10.hex` | `0x5A4D…` by T10 | `eth_getCode 0x5A4D000000000000000000000000000000000000 35816049` | `0x1ea2e805…90e0a1a0` |
| `zone-portal-t13.hex` | `0x5AD1…` by T13 | `eth_getCode 0x5AD1… 47250841` (unchanged at 50062373) | `0xbb0c0e14…62e4c41e` |
| `zone-verifier-t13.hex` | `0x5A56…` by T13, T14, T15 | `eth_getCode 0x5A56… 47250841` | `0xcf7b19d3…b456de14` |
| `zone-messenger-t13.hex` | `0x5A4D…` by T13, T14, T15 | `eth_getCode 0x5A4D… 47250841` | `0x526c5bb0…045214ee` |
| `zone-portal-t14.hex` | `0x5AD1…` by T14 | `T14_ZONE_PORTAL_RUNTIME`, tempo `crates/contracts/src/zones.rs` at 568b4fb (the node's commit; mainnet activates T14 at 1796742000) | `0x044c1349…697642e6` |
| `zone-portal-t15.hex` | `0x5AD1…` by T15 | `T15_ZONE_PORTAL_RUNTIME`, same file and commit (T15 is not scheduled on mainnet) | `0xc07055d0…01a02f0e` |

The node's bytes for T10 and T13 are identical to the `ZONE_*_RUNTIME` and `T13_ZONE_*_RUNTIME`
constants of `zones.rs` at 568b4fb, and the T13 set is identical to the runtimes allocated in the
T13-genesis devnet (`tests/tempo/executor/data/devnet-r6/genesis.json`). The T13 hashes agree with
`eth_getProof … latest` `codeHash`; every hash in the table is the node's `web3_sha3` of the file.
