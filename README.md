# clempo

A [Tempo](https://github.com/tempoxyz/tempo) node in Common Lisp. clempo has replayed mainnet from genesis through block 50,049,999 with every state root checked, followed the live tip with finalization verification, compared JSON-RPC answers against the Rust node, and finalized blocks as a signer alongside Rust validators on a devnet. Its EVM, trie, crypto and protocol implementations use no outside libraries.

clempo is an independent implementation of the Tempo reference node, alongside others such as [goempo](https://github.com/gakonst/goempo) (Go). It measures progress against the same reference ([GOAL.md](GOAL.md)); [FRONTIER.md](FRONTIER.md) records the revisions, inputs and commands behind the results.

## Status

| Rung | What is checked | clempo | goempo |
|---|---|---|---|
| R1 Decode | sampled block/receipt commitments and sender recovery; separate full-archive raw block scan | 673/673 sampled across T0..T13; 50,050,000/50,050,000 mainnet blocks in the raw scan | 632/632 sampled |
| R2 Genesis | genesis state root and block hash | mainnet and moderato | mainnet and moderato |
| R3 Replay | executing mainnet from genesis: gas, receipts root, bloom, state root of every block | **50,049,999** (end of the local archive), no healed blocks | 49,399,999 |
| R3e EVM | execution-spec-tests v5.4.0 state tests, Istanbul..Osaka | 61,903/61,903 | 61,903/61,903 |
| R3p Precompiles | reference-oracle corpus of Tempo precompile calls, T0..T14 | 4,986/4,992 | 4,986/4,992 |
| R4 Follow | live tip: executed, certified, RPC probed independently | 47,451 blocks executed and certified in the historical hour run; peak sampled lag 4 | yes |
| R5 RPC | recorded reference answers, separated by comparison mode | see the [RPC evidence](FRONTIER.md#evidence) for verdicts and coverage | see [goempo's frontier](https://github.com/gakonst/goempo/blob/main/FRONTIER.md) |
| R6 Validator | Simplex and DKG; independent signers with Rust validators | seeded sims (4/5/7 validators, faults, reshares); devnet where clempo signers are required for quorum | seeded simulations and mixed-validator devnet |
| R7 Speed | timed replay/import wall throughput vs Rust on the same Mac | 6.65× (Early), 1.58× (Busy), 2.86× (Recent) | 17.65× (Early), 1.36× (Busy); no same-host Recent result |

The six R3p differences are T14 keychain grants where clempo follows a newer reference commit than the corpus. The R7 ratios come from the revisions and runs in [docs/BENCH.md](docs/BENCH.md#results-on-the-shared-dev-host), before the disk-backed store migration. They use timed replay/import, not end-to-end command time: clempo and goempo check every root; Rust checks import-chunk terminal roots. Clempo used more CPU than Rust on Busy (58.20 s vs 45.76 s), and less on Early and Recent.

## Why Lisp

The bet was that a node in Lisp could be smaller and easier to keep in step with a fast-moving protocol, without being slower. The evidence so far:

**Protocol declarations generate code and documentation.** `scripts/metrics.sh` reports current source size; lines of code are not a measure of protocol coverage.

| Form | Generates |
|---|---|
| `defop` | interpreter dispatch and compiled code from opcode, gas and stack metadata |
| `defrlp` | structs, decoders, encoders and field metadata |
| `defcontract` | Solidity-compatible storage layout and typed storage places |
| `defcall` | a precompile method's selector, ABI, mutability, fork gate and dispatch |
| `fork-if`, `:since` | code and source locations for a declared hardfork change |
| `defrpc` | RPC parameter parsing and error handling |
| `defcodec`, `defprecompile`, `defscenario` | commonware wire codecs, Ethereum precompiles and seeded validator simulations |

`bin/clempo-dev spec` writes precompile slot tables and ABIs, and an index of the fork sites recorded by the declarative forms. The gate fails if `spec/` drifts from the code. A layout excerpt:

```lisp
;; src/tempo/precompiles/tip20.lisp (abridged)
(defcontract tip20-token (:prefix "TOKEN")
  (roles (mapping address (mapping b256 bool)))       ; slot 0
  (role-admins (mapping b256 b256))                   ; slot 1
  (name string)                                       ; slot 2
  ...
  (logo-uri string :since :t5)                        ; slot 5, gated on T5
  (quote-token address)                               ; slot 6
  (next-quote-token address)                          ; slot 7, offset 0  } packed
  (transfer-policy-id u64)                            ; slot 7, offset 20 }
  (total-supply u256)                                 ; slot 8
  (balances (mapping address u256))                   ; slot 9
  ...)

(read (token-balances token account))   ; exactly one SLOAD
(read (token-transfer-policy-id token)) ; one SLOAD, then the field is extracted
```

**Divergences stop in a condition you can resume from.** A block that disagrees with mainnet signals `block-mismatch` with restarts: `retry-block` (after recompiling the fix into the running image, so the warm state survives), `heal-from-reference` (apply the reference's state diff and continue), `skip-block`, `abort-replay`. The heal restart let a survey run ahead of the clean replay and classify every class of divergence before the replay reached it.

**Native code where it counts, still written in Lisp.**
- Keccak-f and SHA-256 use the arm64 crypto instructions through SBCL's `define-vop`, with portable Lisp as the fallback: 799 MB/s and 2.7 GB/s.
- secp256k1, P-256, P-384 and BLS12-381 run on typed limb arithmetic that allocates nothing per call. ecrecover takes 45 µs and a BLS signature check 2.7 ms.
- The optional EVM compiler translates bytecode into Lisp and uses SBCL's own `compile`; the interpreter is also measured and tested independently.

**Context lives in special variables.** The executor, the RPC threads, the tests and the simulator rebind `*world*`, `*spec*`, `*storage*`, `*tracer*`, `*clock*` and `*entropy*`. The simulator is deterministic because the consensus code takes its clock and randomness from `*clock*` and `*entropy*`, and `scripts/check-layers.lisp` forbids any other route in the non-IO layers.

**One image.** `bin/clempo --repl-port` serves an authenticated loopback REPL into the running node: inspect state, redefine a function, and keep following the tip.

## Quick start

Requirements: [SBCL](https://www.sbcl.org) 2.6.8 and `git`. The gate has passed on Apple Silicon macOS and Linux x86-64. Linux evidence also covers mainnet and Moderato genesis, 61,903 execution-spec-tests v5.4.0 state cases and mainnet replay to block 1,000,000. See [FRONTIER.md](FRONTIER.md#evidence) for commands and timings. clempo speaks TLS 1.3 itself (`src/net`), validating certificates against the system roots (macOS keychain or the Linux CA bundle; `CLEMPO_CA_FILE` overrides).

```sh
git clone https://github.com/z80dev/clempo.git && cd clempo
scripts/verify.sh            # cold strict compile, tests, layer check and spec drift
bin/clempo-dev help          # every tool
```

The rungs need mainnet data, and the public RPC is rate limited. For anything beyond spot checks, run the reference node yourself: build `tempo` from [tempoxyz/tempo](https://github.com/tempoxyz/tempo), start from a mainnet snapshot (`tempo download`), run it as an archive follower with the `debug` and `trace` namespaces enabled, and point `CLEMPO_RPC` at it.

```sh
export CLEMPO_RPC=http://127.0.0.1:8545
STATE="$PWD/run/state"
REPORT="$PWD/run/replay.json"
bin/clempo-dev genesis                         # R2: state root of block 0
bin/clempo-dev fetch-range -to 50049999        # raw block archive, ~46 GB under ~/cache/clempo/mainnet
CLEMPO_BUILD=release bin/clempo-dev replay -datadir "$STATE" -checkpoint 500000 -result "$REPORT"
bin/clempo-dev certify -from 49995000 -to 50015000   # finalization certificates
bin/clempo-dev score -help                    # inputs for each rung
bin/clempo-dev score -rung R3 -result "$REPORT" -datadir "$STATE" -json
```

Running the node, its RPC server and the REPL: [docs/NODE.md](docs/NODE.md). Validators on a local devnet with Rust validators: [docs/DEVNET.md](docs/DEVNET.md).

## Layout

`layers.sexp` lists the layers in import order, and the gate enforces it. Each layer is a directory under `src/` with its own ASDF system and its own tests under `tests/`.

```
base io crypto net codec trie types store state evm tempo consensus p2p sync rpc sim validator node dev
```

| Layer | Contents |
|---|---|
| `io`, `net` | deterministic clock/entropy interfaces, native I/O, TLS 1.3 and HTTP |
| `crypto` | Keccak, SHA-2, RIPEMD-160, BLAKE2f/BLAKE3, AES, ChaCha20-Poly1305, X25519, Ed25519, secp256k1, P-256, P-384, BN254, BLS12-381, KZG, modexp |
| `codec`, `types` | RLP, JSON, ABI, commonware codec; headers, every Tempo transaction and signature kind, receipts |
| `trie`, `store`, `state` | Merkle Patricia trie and proofs, disk-backed LSM with bounded working caches and a durable block log, journaled world state |
| `evm` | interpreter, compiler, gas schedules Istanbul..Osaka, Ethereum precompiles, geth and parity tracers |
| `tempo` | chain specs, hardforks, `defcontract`, every Tempo precompile, the block executor, transaction pool, payload builder |
| `consensus`, `p2p`, `validator`, `sim` | certificates, Simplex, DKG; devp2p and commonware networking; the validator; the seeded simulator |
| `sync`, `rpc`, `node`, `dev` | archive and follower, JSON-RPC and WebSocket server, the node binary, the `clempo-dev` tools |

## Limitations

- **RPC coverage is not protocol equivalence.** The recorded corpus distinguishes values, errors, and live or node-local responses. The [frontier](FRONTIER.md#evidence) records the scope; behavior differences are listed in [docs/DEVIATIONS.md](docs/DEVIATIONS.md).
- **Execution and validator scope.** Replay and live-hour evidence are for the recorded mainnet ranges; mixed-validator evidence is for devnets, not mainnet signing. See [the T14/T15 coverage notes](docs/DEVIATIONS.md) and [docs/DEVNET.md](docs/DEVNET.md).
- **Constant time.** Secret-key operations (signing, ECDH) are not hardened against timing or cache side channels.
- **Not for production use.** Treat clempo as an independent implementation for testing and research. It has not run as a mainnet validator.
- **Linux coverage is limited.** The full mainnet replay, live following, mixed-validator devnet and same-host speed comparisons were run on Apple Silicon. Linux arm64 uses the portable Lisp crypto code; the ARM64 VOP paths have not been exercised on Linux arm64.

## How it was built

Agents developed clempo in independent slices and recorded verification against the Rust reference node. [docs/HOW-IT-WAS-BUILT.md](docs/HOW-IT-WAS-BUILT.md) describes the process. [AGENTS.md](AGENTS.md) and [LOOP.md](LOOP.md) give the development rules; [docs/METRICS.md](docs/METRICS.md) explains the current measurements.

## License

Dual-licensed under either of [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE), at your option. Test data recorded or published by others is listed in [NOTICE.md](NOTICE.md).
