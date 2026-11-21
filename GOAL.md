# Goal: reach goempo's frontier, in Common Lisp

clempo climbs the same ladder as [goempo](https://github.com/gakonst/goempo) and zempo. `bin/clempo-dev score -help` lists the inputs each rung needs. Evidence is revision-specific; a rung counts only when `scripts/verify.sh` passes on the same commit. Use `https://rpc.tempo.xyz` for mainnet, `https://rpc.moderato.tempo.xyz` for testnet, or set `CLEMPO_RPC` to your reference archive node for bulk work.

| Rung | Name | Score (higher is better) | Frontier (goempo, 2026-11-19) |
|---|---|---|---|
| R0 | Gate | `scripts/verify.sh` passes: cold strict compile, every test, layer check | yes |
| R1 | Decode | mainnet blocks decoded from `debug_getRawBlock`, re-encoded byte for byte, block hash, transactions root, every tx hash and sender, receipts root and bloom from `eth_getBlockReceipts`, sampled across every fork T0..T13 | 632/632 |
| R2 | Genesis | mainnet genesis alloc loaded, state root of block 0 matches (and moderato) | yes |
| R3 | Replay | highest mainnet block N such that executing 1..N from genesis matches every block's gas used, receipts root, logs bloom and state root | 49,399,999 (tip of its cache) |
| R3e | EVM | execution-spec-tests state tests, Istanbul..Osaka | 61,903/61,903 |
| R3p | Precompiles | zempo's reference-oracle corpus of Tempo precompile calls, T0..T14 | 4,986/4,992 |
| R4 | Follow | catches the live mainnet tip and follows it for at least an hour: every block executed (gas, receipts, bloom, state root) and every finalization certificate verified; read-only JSON-RPC probes independently match the canonical head and its live state | yes |
| R5 | RPC | share of the reference's RPC methods that answer the same as the reference on a recorded corpus | 111/111 methods, 3,378/3,378 cases |
| R6 | Validator | Simplex consensus and DKG: N clempo validators in a seeded simulation (faults, reshare) whose chain the follower's certificate verifier accepts; then clempo validators finalizing with Rust validators on a devnet | sim yes; devnet with Rust validators yes |
| R7 | Speed | mainnet blocks executed with state root per second, against the Rust node on the same host | varies by range; see [goempo's benchmark notes](https://github.com/gakonst/goempo/blob/main/docs/METRICS.md) for its host and methodology |

The headline number is R3's height. Climb in order: a rung above an unmet one does not count, except that R7 work starts only after R3 reaches the tip.

For R4, the independent RPC window starts at the first correct zero-lag sample. Correct probes must span at least 3,600 seconds with gaps of at most 20 seconds, lag must stay within 0–64 blocks on every correct probe, and no probe in the window may return incorrect state or a noncanonical head. The node must report at least an hour of live execution and certify every block it executes from the anchor. There is no separate end-of-run lag threshold.

## The specification

clempo implements what the reference node does. Where it is unclear, these say it, in order of trust:

1. The reference node's behaviour on the network: RPC results, `debug_traceBlockByNumber`, `debug_traceTransaction` with the prestate tracer.
2. The reference's code: [tempoxyz/tempo](https://github.com/tempoxyz/tempo), with reth and revm.
3. goempo's documents ([gakonst/goempo](https://github.com/gakonst/goempo) `docs/`, `FRONTIER.md`): a tested reading of the reference that says where to look. Never copy its code.

Every difference from the reference goes in `docs/DEVIATIONS.md`.

## What "lispy" means here

clempo is not a transliteration of a Go or Zig node. The point of the exercise is the set of things Common Lisp on SBCL does that the others cannot:

- **Declarations, not boilerplate.** Opcode tables, hardfork gates, RLP and ABI shapes, and precompile storage layouts are written once as forms (`defop`, `defcontract`, `defcall`, `defrlp`, `fork-if`) and macros generate the code, the tests' expectations and the documentation from them. The layout registry answers "what changed in T8" as a query.
- **Divergences you can resume from.** A mismatching block signals a condition with restarts (`retry-block`, `heal-from-reference`, `skip`), so a replay can be repaired in a live image and continued without losing its warm state.
- **Context as special variables.** `*storage*`, `*spec*`, `*clock*`, `*entropy*`, `*tracer*` are rebound by the simulator, the tests and the RPC executor instead of threaded through every call.
- **Native code from forms.** Hot paths (Keccak, field arithmetic, the interpreter loop) are macro-unrolled typed Lisp that SBCL compiles to straight-line machine code; `compile` at run time is available for specialisation by fork and for EVM bytecode.
- **Integers are words.** 256-bit values are Lisp integers; small ones are fixnums and never allocate.
