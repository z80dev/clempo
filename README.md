# clempo

clempo is a Tempo node written in Common Lisp (SBCL). Tempo's reference node is written in Rust ([tempoxyz/tempo](https://github.com/tempoxyz/tempo)); zempo (Zig) and [goempo](https://github.com/gakonst/goempo) (Go) came before it. clempo climbs the same ladder ([GOAL.md](GOAL.md)) and tries to win where Lisp is different:

- **Declarative protocol.** Opcodes, hardfork gates, wire shapes and precompile storage layouts are forms; macros turn them into code, tests and documentation.
- **Resumable divergences.** A replay that disagrees with mainnet stops in a condition with restarts, not a crash.
- **One image.** The node, the tools and the REPL share one running Lisp.
- **No borrowed code.** clempo implements the EVM, the trie, RLP, the crypto and Tempo's precompiles itself, using only SBCL.

Status: [FRONTIER.md](FRONTIER.md). Rules for contributors (human or agent): [AGENTS.md](AGENTS.md). Running the node: [docs/NODE.md](docs/NODE.md); validators on a devnet: [docs/DEVNET.md](docs/DEVNET.md); benchmarks: [docs/BENCH.md](docs/BENCH.md).

```sh
scripts/verify.sh        # the gate
bin/clempo-dev help      # tools
bin/clempo-dev score     # every rung
```

## Where the Lisp shows

| Form | What one declaration generates | Where |
|---|---|---|
| `defop` | the EVM opcode, its gas and stack effects, and the interpreter's dispatch case | `src/evm/defop.lisp` |
| `defprecompile` | Ethereum precompile registration per fork | `src/evm/precompiles.lisp` |
| `defrlp` | struct, decoder, encoder and field docs for headers, transactions, receipts | `src/types/defrlp.lisp` |
| `defcodec` | byte-exact commonware codecs for consensus messages | `src/codec/commonware.lisp` |
| `defcontract`, `defcall` | Solidity-compatible slot layout, storage accessors, ABI dispatch, layout tests | `src/tempo/contract/` ([design](docs/DEFCONTRACT.md)) |
| `fork-if`, `:since` | hardfork branches that record themselves; `spec/forks/tN.md` lists every site per fork | `src/tempo/forks/`, `spec/` |
| `defrpc` | JSON-RPC parameter parsing and the reference's exact error texts | `src/rpc/defrpc.lisp` |
| `defscenario` | seeded multi-validator simulations | `src/sim/run.lisp` |

`spec/` (slot layouts, ABIs, fork sites) is generated from those forms, and `verify.sh` fails when it drifts.

- **Resumable divergences.** `clempo-dev replay` signals `block-mismatch` with restarts `retry-block`, `heal-from-reference`, `skip-block` and `abort-replay` (`src/tempo/executor/replay.lisp`); `-interactive` leaves the choice to the debugger, so a fix can be recompiled into the running image and the block retried with its warm state.
- **A REPL inside the node.** `bin/clempo --repl-port` serves an authenticated loopback REPL into the running node (`src/node/repl.lisp`).
- **Assembly from Lisp.** SHA-256 and Keccak-f use arm64 crypto instructions through `define-vop` (`src/crypto/hash/sha256-arm64.lisp`, `src/crypto/keccak/keccak-arm64.lisp`), with portable typed Lisp as the fallback.
- **Context as special variables.** `*world*`, `*spec*`, `*storage*`, `*tracer*`, `*clock*` and `*entropy*` are rebound by the executor, RPC threads, tests and the simulator.

## License

Dual-licensed under either of MIT or Apache-2.0, at your option.
