# clempo

A [Tempo](https://github.com/tempoxyz/tempo) node in Common Lisp. It replays all of mainnet from genesis with every state root checked, follows the live tip with every finalization certificate verified, answers the reference node's JSON-RPC methods identically, and runs as a validator alongside Rust validators. It uses nothing outside SBCL: no EVM, trie, crypto or networking library, and no foreign code.

clempo is the third independent implementation, after zempo (Zig) and [goempo](https://github.com/gakonst/goempo) (Go). It climbs the same ladder of rungs against the same reference ([GOAL.md](GOAL.md)), with every number in [FRONTIER.md](FRONTIER.md) backed by a command you can re-run.

## Status

| Rung | What is checked | clempo | goempo |
|---|---|---|---|
| R1 Decode | blocks decoded and re-encoded byte for byte; hashes, roots and every sender recovered | 673/673 sampled blocks across T0..T13, and all 50,050,000 mainnet blocks against the reference | 632/632 sampled |
| R2 Genesis | genesis state root and block hash | mainnet and moderato | mainnet and moderato |
| R3 Replay | executing mainnet from genesis: gas, receipts root, bloom, state root of every block | **50,049,999** (end of the local archive), no healed blocks | 49,399,999 |
| R3e EVM | execution-spec-tests v5.4.0 state tests, Istanbul..Osaka | 61,903/61,903 | 61,903/61,903 |
| R3p Precompiles | reference-oracle corpus of Tempo precompile calls, T0..T14 | 4,986/4,992 | 4,986/4,992 |
| R4 Follow | live tip: executed, certified, RPC served, for an hour | yes: 47,451 blocks executed and certified, peak lag 4 | yes |
| R5 RPC | recorded reference answers compared with clempo's | 154/154 methods, 14,287/14,287 cases | 111/111 methods, 3,378 cases |
| R6 Validator | Simplex + DKG; clempo validators with Rust validators | seeded sims (4/5/7 validators, faults, reshares); devnet where clempo signers are required for quorum | the same |
| R7 Speed | blocks with state root per second, against the Rust node on the same host | 6.5× (early), 1.6× (busy), 2.8× (recent) wall throughput | 9.2×, 0.64×, 2.5× |

The six R3p differences are T14 keychain grants where clempo follows a newer reference commit than the corpus. R5 compares a larger corpus than goempo's; its known gaps are listed under [Limitations](#limitations). R7 compares wall time on one Apple Silicon host; clempo checks every block's root, the Rust import checks chunk-terminal roots, and clempo uses more CPU than Rust on the recent range (22.6 s against 21.4 s). Details and commands: [FRONTIER.md](FRONTIER.md), [docs/BENCH.md](docs/BENCH.md).

## Why Lisp

The bet was that a node in Lisp could be smaller and easier to keep in step with a fast-moving protocol, without being slower. The evidence so far:

**The protocol is written as declarations, and the code, tests and documents come from them.** About 50,000 lines of Lisp (`src/`, dev tools included) cover what goempo does in about 71,000 lines of Go.

| Form | Count | One declaration generates |
|---|---|---|
| `defop` | 81 | an EVM opcode: gas, stack effects, the interpreter's dispatch case, and the compiled code for it |
| `defrlp` | — | struct, decoder, encoder and field docs for headers, transactions and receipts |
| `defcontract` | 19 | a precompile's Solidity-compatible storage layout, typed storage places, layout tests against solc's output |
| `defcall` | 209 | a precompile method: selector, ABI decoding, mutability, fork gate, dispatch in the reference's order |
| `fork-if`, `:since` | 117 | a hardfork branch that records where it is |
| `defrpc` | 127 | a JSON-RPC method's parameter parsing and the reference's exact error texts |
| `defcodec`, `defprecompile`, `defscenario` | — | commonware wire codecs, Ethereum precompiles, seeded validator simulations |

From the same forms, `bin/clempo-dev spec` writes `spec/`: every precompile's slot table and ABI, and for every hardfork the list of the 196 places in the code it changes. The gate fails if `spec/` drifts from the code. "What did T8 change?" is a file, not an archaeology project:

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
- Hot EVM contracts are translated from bytecode into Lisp and compiled with SBCL's own `compile` while the node runs. Small-word arithmetic runs 7.7× faster than interpreted, and the whole state-test suite passes with every contract compiled.

**Context lives in special variables.** The executor, the RPC threads, the tests and the simulator rebind `*world*`, `*spec*`, `*storage*`, `*tracer*`, `*clock*` and `*entropy*`. The simulator is deterministic because the consensus code takes its clock and randomness from `*clock*` and `*entropy*`, and `scripts/check-layers.lisp` forbids any other route in the non-IO layers.

**One image.** `bin/clempo --repl-port` serves an authenticated loopback REPL into the running node: inspect state, redefine a function, and keep following the tip.

## Quick start

Requirements: [SBCL](https://www.sbcl.org) 2.6.8 (`brew install sbcl`), `curl`, `git`. Developed and measured on macOS on Apple Silicon. The arm64 assembly paths have portable fallbacks, but Linux and x86-64 have not been run end to end.

```sh
git clone <this repository> clempo && cd clempo
scripts/verify.sh            # the gate: cold strict compile, ~590 tests, layer check, spec drift (about a minute)
bin/clempo-dev help          # every tool
```

The rungs need mainnet data, and the public RPC is rate limited. For anything beyond spot checks, run the reference node yourself: build `tempo` from [tempoxyz/tempo](https://github.com/tempoxyz/tempo), start from a mainnet snapshot (`tempo download`), run it as an archive follower with the `debug` and `trace` namespaces enabled, and point `CLEMPO_RPC` at it.

```sh
export CLEMPO_RPC=http://127.0.0.1:8545
bin/clempo-dev genesis                         # R2: state root of block 0
bin/clempo-dev fetch-range -to 50049999        # raw block archive, ~46 GB under ~/cache/clempo/mainnet
CLEMPO_BUILD=release bin/clempo-dev replay -datadir ~/clempo-state -checkpoint 500000   # R3: hours, not minutes
bin/clempo-dev certify -from 49995000 -to 50015000   # finalization certificates
bin/clempo-dev score                            # every rung
```

Running the node, its RPC server and the REPL: [docs/NODE.md](docs/NODE.md). Validators on a local devnet with Rust validators: [docs/DEVNET.md](docs/DEVNET.md).

## Layout

`layers.sexp` lists the layers in import order, and the gate enforces it. Each layer is a directory under `src/` with its own ASDF system and its own tests under `tests/`.

```
base io crypto codec trie types store state evm tempo consensus p2p sync rpc sim validator node dev
```

| Layer | Contents |
|---|---|
| `crypto` | Keccak, SHA-2, RIPEMD-160, BLAKE2f/BLAKE3, AES, ChaCha20-Poly1305, X25519, Ed25519, secp256k1, P-256, P-384, BN254, BLS12-381, KZG, modexp |
| `codec`, `types` | RLP, JSON, ABI, commonware codec; headers, every Tempo transaction and signature kind, receipts |
| `trie`, `store`, `state` | Merkle Patricia trie and proofs, the on-disk state store with checkpoints and history, the journaled world state |
| `evm` | interpreter, compiler, gas schedules Istanbul..Osaka, Ethereum precompiles, geth and parity tracers |
| `tempo` | chain specs, hardforks, `defcontract`, every Tempo precompile, the block executor, transaction pool, payload builder |
| `consensus`, `p2p`, `validator`, `sim` | certificates, Simplex, DKG; devp2p and commonware networking; the validator; the seeded simulator |
| `sync`, `rpc`, `node`, `dev` | archive and follower, JSON-RPC and WebSocket server, the node binary, the `clempo-dev` tools |

## Limitations

- **R5 is not fully closed.** Three reference methods are not served (`consensus_subscribe` and `consensus_unsubscribe` over WebSocket, `admin_clearTxpool`). `debug_executionWitness` matches the reference's shape but omits the extra trie nodes it includes. Some corpus cases recorded the reference's own database timeout and are being re-recorded.
- **Deviations from the reference** are numbered in [docs/DEVIATIONS.md](docs/DEVIATIONS.md). Examples: TLS goes through `curl`, and EVM memory past 4 GiB is treated as out of gas.
- **Constant time.** Secret-key operations (signing, ECDH) are not hardened against timing or cache side channels.
- **Not for production use.** Treat clempo as an independent implementation for testing and research. It has not been audited, and it has never run as a mainnet validator.
- **One platform measured.** All results are from one Apple Silicon machine.

## How it was built

A lead agent and a swarm of agents wrote clempo in about eight hours. Each agent owned one directory and pushed small verified slices, and every claim was checked against the Rust reference node. [docs/HOW-IT-WAS-BUILT.md](docs/HOW-IT-WAS-BUILT.md) tells the story, [AGENTS.md](AGENTS.md) and [LOOP.md](LOOP.md) give the rules, and `scripts/metrics.sh` prints the current numbers.

## License

Dual-licensed under either of [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE), at your option. Test data recorded or published by others is listed in [NOTICE.md](NOTICE.md).
