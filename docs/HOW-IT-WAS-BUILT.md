# How clempo was built

clempo is a Tempo node written in Common Lisp on SBCL. Agents built the initial implementation on 2026-11-20/21 with one ladder ([GOAL.md](../GOAL.md)), one development loop ([LOOP.md](../LOOP.md)) and one gate (`scripts/verify.sh`). Measurements and their scope are recorded in [FRONTIER.md](../FRONTIER.md). Times below are US Eastern (-0500), from the commit log.

## Timeline

| Time | Step | Commit |
|---|---|---|
| 11-20 23:53 | scaffold: base (policy, invariants, octets, u256, test harness), io, Keccak, RLP, JSON, HTTP, JSON-RPC client, layer check | `7752e88` |
| 11-21 00:11 | R2: mainnet and moderato genesis state roots and block hashes | `ec20b6b` |
| 00:12 | R1: 640/640 mainnet + 33/33 moderato sampled blocks | `d9f62ff` |
| 00:35 | R3e: 61,903/61,903 execution-spec-tests state tests | `c0d893f` |
| 00:49 | R4 certificates 20,001/20,001; R6 simulation (4/5/7 validators) | `b3e6499` |
| 01:07 | R3p: 4,986/4,992 precompile oracle (goempo's frontier) | `4f3dcea` |
| 01:15 | R6 devnet: clempo validators necessary for quorum with Rust validators | `dbe14d1` |
| 03:13 | EVM bytecode compiled to native code through `compile` | `ea7131e` |
| 03:52 | R7 Busy faster than the Rust node (root-checked, same host) | `8d8ffbb` |
| 04:35 | R3: 50,049,999, the archive tip, no heals | `e3bc430` |
| 05:53 | R5: 154/154 methods, 14,287/14,287 recorded cases | `67cdd46` |
| 05:58 | R4: an hour at the live tip, every block executed and certified, RPC served | `5e71c99` |

## Infrastructure

- **A private reference node.** The public RPC allows about 60 raw blocks a second, so the lead built the Rust node from `~/dev/tempo`, downloaded the 28 GB mainnet snapshot and ran it as a `--follow --follow.nocertify` archive node on localhost (`CLEMPO_RPC`). It served about 15,000 raw blocks a second for the archive, every heal, trace and RPC corpus recording, and the Rust side of the benchmarks.
- **The raw block archive.** `clempo-dev fetch-range` (`src/sync/archive.lisp`) filled `~/cache/clempo/mainnet/` with 50,050,000 blocks (46 GB, 10,000-block chunk files) in about 20 minutes at up to 150,000 blocks a second.
- **The host.** One Apple Silicon machine (16 cores, 128 GB) shared by up to 15 agents, the reference node and two unrelated VMs. Memory pressure killed the reference node three times; it now runs under a restart loop and `AGENTS.md` carries the shared-host rules.

## Agents

One agent per directory, each in its own clone of a local bare origin, pushing small verified slices; the lead owned the scaffold, `layers.sexp`, the integration fixes and `FRONTIER.md`'s table.

| Workstream | Built |
|---|---|
| Hash functions and symmetric crypto | SHA-2, RIPEMD-160, BLAKE2f, BLAKE3, AES, ChaCha20-Poly1305, X25519 and Ed25519; ARM64 SHA-256 and Keccak instructions |
| Curves | secp256k1, P-256, P-384, BN254, BLS12-381, KZG and modexp; typed limb kernels |
| Types | `defrlp`, transaction envelopes and signatures, receipts, blocks, pool and payload builder |
| State and replay | Merkle Patricia trie, genesis, persistence, historical state and replay with restarts |
| Execution | journaled state, EVM interpreter and compiler, traces, and Tempo precompiles |
| Consensus and networking | commonware codec and transport, finalization, DKG, Simplex, simulation, devp2p, following and validation |
| RPC and tools | JSON-RPC and WebSocket, corpus comparison, spec generation, benchmarks and profiling |

## Where Lisp changed the work

- **One form, many outputs.** `defop`, `defrlp`, `defcodec`, `defcontract`/`defcall`, `defrpc`, `defprecompile` and `defscenario` generate code or metadata from declarations. `spec/` contains generated storage layouts, ABIs and indexed fork sites; the gate detects drift.
- **Restartable replay.** A mismatching block signals `block-mismatch` with `retry-block`, `heal-from-reference`, `skip-block` and `abort-replay`; the heal survey used `heal-from-reference` to run ahead of the clean replay and classify every divergence class before the replay reached it.
- **Native code from forms.** Keccak and SHA-256 use arm64 crypto instructions through `define-vop`; BLS12-381 and secp256k1 run on typed limb arithmetic with zero allocation; hot EVM contracts are translated to Lisp and compiled at run time.
- **Special variables as context.** RPC threads, the simulator, tests and the executor rebind `*world*`, `*spec*`, `*storage*`, `*tracer*`, `*clock*` and `*entropy*`. Thread bindings keep concurrent execution contexts separate.

## What mainnet caught

The R3 stops, each fixed with a regression test on the real block: 3,850 (stopped on a fee-token balance check), 3,865, 4,516,256, 4,699,557, 4,711,932 (CREATE charged 32,000 instead of Tempo's 500,000), 6,305,645 (key-authorization gas from T1B), and the T7 storage-credit rule for EVM SSTOREs (29,152,422). After those, the replay ran from 14.7M to the archive tip without a mismatch.

## Results

See [FRONTIER.md](../FRONTIER.md) for the table and the evidence paragraphs with exact commands, and `scripts/metrics.sh` for code size and the latest score per rung.
