# Agents: how to work on clempo

clempo is a Common Lisp (SBCL) implementation of the Tempo blockchain node. Its reference implementation is the Rust node at `github.com/tempoxyz/tempo`, checked out at `~/dev/tempo`. `GOAL.md` defines the rungs, `LOOP.md` the iteration, `FRONTIER.md` the current results and `FRONTIER.jsonl` the measurement history.

## Rules

- **SBCL 2.6.8, nothing else.** No Quicklisp and no outside Lisp libraries. Allowed: the SBCL runtime and its contribs (`sb-bsd-sockets`, `sb-posix`, `sb-concurrency`, `sb-rotate-byte`, `sb-sprof`) and `curl` as the TLS transport (deviation 1). Anything else needs an entry in `docs/DEVIATIONS.md` with a reason.
- **No outside code for the node's logic.** No code from goempo, zempo, geth, reth or revm, and no FFI to their libraries. Read the Rust reference as the specification. goempo's documents say where to look; never transliterate its Go.
- **Lispy, on purpose** (GOAL.md, "What lispy means here"). Prefer one declarative form plus a macro over repeated code; context in special variables; conditions and restarts for recoverable failures; generic functions for backends and tracers; plain integers for 256-bit words.
- **Fast where it matters.** Hot functions declare their types and use the build policy: `(declare #.clempo.base:*hot*)`. Check builds keep `safety 1` there; `CLEMPO_BUILD=release` drops to `safety 0`. Measure with `time` and `sb-sprof` before and after a speed change, and record the numbers.
- **Invariants stay on.** Use `clempo.base:invariant` and `unreachable`; never `(declare (safety 0))` around code whose inputs come from the network without checking them first.
- **Determinism.** Only directories marked `:io t` in `layers.sexp` may touch the clock, randomness, files, processes, sockets or threads directly. Everything else uses `clempo.io` (`*clock*`, `*entropy*`). `scripts/check-layers.lisp` enforces this and the import order.
- **Differential.** A result the reference can produce is tested against the reference: recorded RPC answers, traces, execution-spec-tests, the precompile oracle. A fixture records where it came from in a README next to it.
- **Record deviations** from the reference in `docs/DEVIATIONS.md`, numbered, never renumbered.

## Layout

One directory under `src/` per layer, in import order (`layers.sexp`). Each has an ASDF system `clempo-<dir>` (and `clempo-<dir>/tests` over `tests/<dir>/`). A directory may hold several packages; name them `clempo.<thing>` and give each file or subsystem its own `defpackage` with an explicit export list.

| Path | What |
|---|---|
| `src/base` | build policy, invariants, octets, hex, u256 helpers, test harness |
| `src/io` | clock, entropy, files, processes, http client |
| `src/crypto` | keccak, sha2, ripemd160, blake2f, blake3, secp256k1, p256, bn254, bls12-381, kzg, ed25519, x25519, aes, chacha20-poly1305 |
| `src/codec` | RLP, JSON, ABI, commonware codec |
| `src/types` | headers, transactions (including Tempo's 0x76), receipts, blocks, signatures |
| `src/trie` | Merkle Patricia trie, state root, proofs |
| `src/store` | the on-disk store |
| `src/state` | the journaled world state the executor reads and writes |
| `src/evm` | interpreter, gas schedule, Ethereum precompiles |
| `src/tempo` | hardforks, chainspec and genesis, `defcontract`, Tempo precompiles, the block executor |
| `src/consensus` | finalization certificates, Simplex, DKG |
| `src/p2p` | devp2p, commonware p2p |
| `src/sync` | JSON-RPC client, block fetch and archive, follow |
| `src/rpc` | JSON-RPC server, tracers |
| `src/sim` | seeded validator simulation |
| `src/node` | the node binary |
| `src/dev` | `clempo-dev` commands: every file here is loaded; add commands with `defcommand` |

## Build and checks

```sh
scripts/verify.sh                 # the gate: cold strict compile, all tests, layer check
bin/clempo-dev help               # list commands
bin/clempo-dev score -rung R1     # measure a rung
```

Run one layer's tests while iterating:

```sh
sbcl --non-interactive --load scripts/build.lisp --eval '(clempo-build:test-all :dirs (list "crypto"))'
```

Warnings and undefined functions fail the strict build. Style warnings other than undefined references are allowed but should be rare.

## Data

- Mainnet block archive, R1 sample, RPC corpus and other large inputs live under `~/cache/clempo/` (never in git). Agents add files and never delete other agents' files.
- `CLEMPO_RPC` names a private mainnet node without rate limits (`http://127.0.0.1:8545` when the local reference node runs). The public RPC allows about 60 `debug_getRawBlock` per second.
- Reference test data: execution-spec-tests fixtures under `~/cache/clempo/fixtures/`, goempo's checked-in fixtures (recorded from mainnet or from the Rust node) under `~/clempo-infra/goempo-ref/`; reuse those data files with their provenance, not their code.

## Working in parallel

- One directory, one agent. Shared files (`layers.sexp`, `FRONTIER.*`, `README.md`, `docs/DEVIATIONS.md`) take small edits; on rebase conflicts keep both sides.
- Clone `~/clempo.git` into `~/clempo-wt/<role>`, commit small verified slices, then `git pull --rebase origin main && scripts/verify.sh && git push origin main`. If origin moved during the gate and the rebase is clean, push without re-gating, then run the gate on what you pushed and fix forward at once if it fails. Unpushed work dies with the agent.
- Ask the owner of another directory for an interface instead of editing their code.
