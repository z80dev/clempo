# Pool fixture provenance and developer command

`mainnet-49400084-tx0-prestate.json` is copied unchanged from
goempo's repository, `internal/txpool/testdata/`. Its upstream README identifies
`debug_traceTransaction` with `prestateTracer` at `https://rpc.tempo.xyz`, transaction
0 (`0x22b20222...7dea`) of mainnet block 49,400,084. This is recorded data, not Go
implementation code. It contains the actual fee-token, TIP-403 registry and fee-manager
storage touched by the fee charge. It is a partial prestate, not a mainnet state snapshot.

The test helper explicitly commits synthetic funding, the built-in allow-all transfer
policy, user/validator token preferences, nonce rows and keychain rows into the real
`clempo.store:statedb`. It signs transactions
using deterministic native secp256k1 and runs the executor's rollback-only validation.
No state-reader methods are replaced and no missing backend is accepted. The recorded
Ethereum/system envelopes are reused in-place from `tests/types/data/transaction-rpc.sexp`;
see that directory's `transaction-provenance.md` for their original networks and blocks.

Slot and policy specifications were read directly from the Rust reference:

- `crates/transaction-pool/src/validator.rs`: field bounds, propagation buffer (> tip+3),
  wall-clock valid-after bound (<= now+120), fee floor, state validation, key expiry.
- `crates/transaction-pool/src/tt_2d_pool.rs`: nonce-lane sequencing, replacement,
  per-authority (64) and per-lane (1024) limits and current-nonce gap filling.
- `crates/transaction-pool/src/ordering.rs`: effective tip saturated to u64; FIFO ties.
- `crates/primitives/src/transaction/tt_signed.rs`: sender-scoped expiring replay identity.
- `crates/precompiles/src/{nonce,account_keychain,tip_fee_manager}/mod.rs` and
  `docs/DEFCONTRACT.md`: actual mapping slots and AuthorizedKey packing. TIP-20
  balances use mapping slot 9; nextQuoteToken/transferPolicyId share packed slot 7.
- `crates/precompiles/src/tip_fee_manager/amm.rs::plan_fee_route` and
  `crates/transaction-pool/src/amm.rs`: fee-manager pool mapping slot 3, high-u128
  validator reserve, quote-token slot 6, 9970/10000 per-hop output, and any-recent-token
  liquidity (including T5 two-hop fallback). Empty pools remain unusable for zero fees.

`bin/clempo-dev pool-fixtures -iterations 10 -json` validates the signed positive and
negative scenarios against the recorded prestate. Counts, elapsed time and allocated
bytes are measured from those actual runs; an unexpected admission/rejection returns
failure rather than a score. This includes fixture/state creation and signing overhead,
so it is an end-to-end development measurement, not a pool-only throughput claim.

The pool requires an explicit `(make-state-reader db env :recent-validator-tokens tokens)`
snapshot. Supply the tokens actually observed across the last ten canonical heads,
plus newly chosen tokens of validators active in that window. This history cannot
be reconstructed from current preferences alone; missing non-genesis history is an
error, not a current-beneficiary-only fallback. Only genesis (db height <= 0 and
environment height <= 1) may derive its sole validator token from actual state.
`timestamp` and `fork` on admission/revalidation must agree with that environment. Revalidate
after canonical state changes before selection. `pool-best-transactions` accepts a
candidate block's timestamp/base fee and returns a nonce-respecting snapshot; a builder
must suppress later transactions with the same `pool-transaction-lane` after one fails
or does not fit. Expiring transactions have independent lane identities. Pool methods
are synchronous; the caller serializes mutation and keeps each state snapshot stable.

Integration status: all 45 pool tests pass without skips, including executor
inline-grant signer/account validation and keychain T3 duplicate token-limit
rejection. All 13 builder tests pass, including recorded-root and pooled-payload
replay.
`pool-fixtures` remains strict; it never turns missing capabilities into a fake score.
