# Agents: how to work on clempo

clempo is a Common Lisp (SBCL) implementation of the Tempo blockchain node. Its reference implementation is the Rust node at [tempoxyz/tempo](https://github.com/tempoxyz/tempo) (keep a checkout next to this one; the dev tools look in `~/dev/tempo` by default). `GOAL.md` defines the rungs, `LOOP.md` the iteration, `FRONTIER.md` the current results and `FRONTIER.jsonl` the measurement history.

## Rules

- **SBCL 2.6.8, no outside Lisp libraries.** No Quicklisp. Use the SBCL runtime and its contribs; record any extra runtime dependency in `docs/DEVIATIONS.md`.
- **No outside code for the node's logic.** No code from goempo, zempo, geth, reth or revm, and no FFI to their libraries. Read the Rust reference as the specification. goempo's documents say where to look; never transliterate its Go.
- **Lispy, on purpose** (GOAL.md, "What lispy means here"). Prefer one declarative form plus a macro over repeated code; context in special variables; conditions and restarts for recoverable failures; generic functions for backends and tracers; plain integers for 256-bit words.
- **Fast where it matters.** Hot functions declare their types and use the build policy: `(declare #.clempo.base:*hot*)`. Check builds keep `safety 1` there; `CLEMPO_BUILD=release` drops to `safety 0`. Measure with `time` and `sb-sprof` before and after a speed change, and record the numbers.
- **Invariants stay on.** Use `clempo.base:invariant` and `unreachable`; never `(declare (safety 0))` around code whose inputs come from the network without checking them first.
- **Bounds are proven when compiled.** Name a fixed limit (a buffer size, a nesting depth, a limb, lane or round count) with `clempo.base:defbound` and give it the relations that make it safe as proofs: the buffer holds what the loop writes, the product stays a fixnum, the limbs cover the field. `static-assert` relates limits defined apart. A false proof fails the compile. The source gate requires every fixed-count loop in hot code to take its count from a `defbound` limit.
- **Locks and threads.** Take mutexes through `clempo.lock` (`with-lock`, `with-recursive-lock`, `condition-wait`), never `sb-thread`'s own forms, and give each `make-mutex` a literal `:name` of its own: the name is its lock class. Check builds record the order locks are taken in and signal `lock-order-violation` when an acquisition closes a cycle, as Linux's lockdep does. A test joins every thread it starts; a process-wide thread meant to outlive tests is named in `clempo.base:*long-lived-threads*`. The test harness fails a test that leaves another thread running or records a lock-order cycle.
- **Shared state declares its guard.** In rpc, store, sync, node, validator and p2p, define a structure that holds a mutex or is mutated under one with `clempo.guard:define-shared-struct` (for a class, `define-slot-guards`) and give every slot its guard: `(:lock slot)`, `(:lock-writes slot)` for whole values read without the lock, `(:held (self) form)`, `(:thread (self) form)`, `:immutable`, `:atomic` (compare-and-swap only) or `(:unchecked "reason")`. Declare a global mutated at run time with `defshared` and mutate it only inside its lock (`with-held` in a helper whose callers hold it). Check builds check each access; the gate checks the declarations and the global mutations. Never call the raw `%%` accessors.
- **Determinism.** Route raw clock and entropy through `clempo.io`; direct file, process, socket and thread operations belong in the I/O boundary in `layers.sexp`. In non-I/O code, use a finite iteration driver and make hash traversal order-independent. The source gate checks reader forms. `scripts/layer-policy.sexp` records reviewed iteration bounds, order-independent traversals, named thread join owners and critical assertion requirements. There are no inline bypass directives.
- **Differential.** A result the reference can produce is tested against the reference: recorded RPC answers, traces, execution-spec-tests, the precompile oracle. A fixture records where it came from in a README next to it.
- **Record deviations** from the reference in `docs/DEVIATIONS.md`, numbered, never renumbered.

## Layout

One directory under `src/` per layer, in import order (`layers.sexp`). Each has an ASDF system `clempo-<dir>` (and `clempo-<dir>/tests` over `tests/<dir>/`). A directory may hold several packages; name them `clempo.<thing>` and give each file or subsystem its own `defpackage` with an explicit export list.

| Path | What |
|---|---|
| `src/base` | build policy, invariants, octets, hex, u256 helpers, test harness |
| `src/io` | clock, entropy, files, processes |
| `src/crypto` | keccak, sha2, ripemd160, blake2f, blake3, secp256k1, p256, bn254, bls12-381, kzg, ed25519, x25519, aes, aes-gcm, chacha20-poly1305, rsa verification, p256 ecdh, asn.1 der, x.509 |
| `src/net` | TLS 1.3 client, system trust anchors, http(s) client |
| `src/codec` | RLP, JSON, ABI, commonware codec |
| `src/trie` | Merkle Patricia trie, state root, proofs |
| `src/types` | headers, transactions (including Tempo's 0x76), receipts, blocks, signatures |
| `src/store` | the on-disk store |
| `src/state` | the journaled world state the executor reads and writes |
| `src/evm` | interpreter, gas schedule, Ethereum precompiles |
| `src/tempo` | hardforks, chainspec and genesis, `defcontract`, Tempo precompiles, the block executor |
| `src/consensus` | finalization certificates, Simplex, DKG |
| `src/p2p` | devp2p, commonware p2p |
| `src/sync` | JSON-RPC client, block fetch and archive, follow |
| `src/rpc` | JSON-RPC server, tracers |
| `src/sim` | seeded validator simulation |
| `src/validator` | validator lifecycle, signing, network supervision and devnet |
| `src/node` | the node binary |
| `src/dev` | `clempo-dev` commands: every file here is loaded; add commands with `defcommand` |

## Build and checks

```sh
scripts/verify.sh                 # the gate: cold strict compile (bound proofs included), all tests
                                  # (thread-leak and lock-order audits), layer check
bin/clempo-dev help               # list commands
bin/clempo-dev score -help       # find the inputs for a rung
```

Run one layer's tests while iterating:

```sh
sbcl --non-interactive --load scripts/build.lisp --eval '(clempo-build:test-all :dirs (list "crypto"))'
```

Warnings and undefined functions fail the strict build. Style warnings other than undefined references are allowed but should be rare.

## Data

- Mainnet block archive, R1 sample, RPC corpus and other large inputs live under `~/cache/clempo/` (never in git). Agents add files and never delete other agents' files.
- `CLEMPO_RPC` names a private mainnet node without rate limits (`http://127.0.0.1:8545` when the local reference node runs). The public RPC allows about 60 `debug_getRawBlock` per second.
- Reference test data: execution-spec-tests fixtures under `~/cache/clempo/fixtures/`; data reused from goempo's repository (recorded from mainnet or the Rust node) is copied into `tests/**/data/` with its provenance (see `NOTICE.md`), never its code.

## Working in parallel

- One directory, one agent. Shared files (`layers.sexp`, `FRONTIER.*`, `README.md`, `docs/DEVIATIONS.md`) take small edits; on rebase conflicts keep both sides.
- Work in your own clone, commit small verified slices, then `git pull --rebase origin main && scripts/verify.sh && git push origin main`. If origin moved during the gate and the rebase is clean, push without re-gating, then run the gate on what you pushed and fix forward at once if it fails. Unpushed work dies with the agent.
- Ask the owner of another directory for an interface instead of editing their code.
- The host is shared: at most one long-running replay-class job per agent, no overlapping benchmarks, `sysctl vm.swapusage` checked before anything above 4 GB RSS, and every server or background job you start (`rpc-serve`, recorders, followers) stopped before you finish. Heavy tracing against the private node (`rpc-record -suite traces`) runs with `-threads 4` or fewer: memory pressure has killed that node.
