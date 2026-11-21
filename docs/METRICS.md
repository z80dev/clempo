# Reading the measurements

`scripts/metrics.sh` is an offline summary, not a benchmark. It requires `jq`.
It prints the checkout's abbreviated commit; physical lines (including blank
lines and comments) and file count in `src/**/*.lisp`; lines in
`tests/**/*.lisp`; the count of lines beginning with `(deftest ` under `tests/`;
and physical Lisp lines per source layer. A test form count is not an executed
test count: tests may be generated, contain many cases, or skip when inputs are
unavailable. The gate reports executed passes, failures and skips separately.

The rung section shows the last row with a non-null `score` for each rung,
in file order, from [`FRONTIER.jsonl`](../FRONTIER.jsonl). Unscored observations
(e.g. a kernel profile) do not erase a prior score. It does not infer a new
score from historical results; the revision, scope and original grading are
in [`FRONTIER.md`](../FRONTIER.md). A fraction counts cases unless the unit
says blocks, methods or reports. `score -help` describes required inputs.

| Rung | What the number counts |
|---|---|
| R0 | Pass/fail for `scripts/verify.sh`: clean strict compile, Lisp tests, seeded fuzz and corpus replay, source layer/determinism audit and generated-spec drift. The test totals count results reported by the test harness, not assertions or fixture cases. |
| R1 | Sampled blocks passing canonical encoding, hash and transaction/sender comparisons, plus receipt root and bloom. The full-archive scan is separately labeled: it checks every raw block and transaction in its range but does not check receipts or execute blocks. Transaction counts include system transactions; recovery counts only count actual cryptographic recoveries. |
| R2 | Mainnet and Moderato genesis state roots and header hashes against the reference. |
| R3 | Greatest contiguous mainnet height executed from genesis with matching gas, receipt root, bloom and state root; heals and skips end the contiguous score even when a later block matches. `score -result` reads the replay evidence. `-datadir` independently checks the checkpoint/WAL roots and the stored head's hash and state root against the archive. This does not independently re-execute the history. |
| R3e | Passing/supported cases from EEST v5.4.0 state tests, grouped by EVM fork. Cases before Istanbul are outside the denominator. |
| R3p | Oracle cases with the expected return or revert data, storage and gas where recorded. The oracle revision matters when scheduling or gas rules change. |
| R4 | Pass/fail for a certified, root-checked live hour with independent read-only RPC probes (see [GOAL](../GOAL.md)). Anchor/head are block heights; executed and certified are counts since the anchor. RPC sampled seconds span first to last valid live probe; attempts includes failed probes, incorrect counts false comparisons, gap is the largest time between valid probes. Lag is upstream height minus served height at each probe; p95 is the nearest-rank 95th percentile. |
| R5 | Recorded JSON-RPC comparison. Exact value matches, expected errors, live shape checks, node-local behavior and missing reference results are different kinds of evidence; only exact comparisons of values establish value parity. Distinct method names exclude synthetic malformed envelope labels. Coverage is the number of methods with cases or implemented, not the number with a value match. Cite the scorer revision with any legacy aggregate. |
| R6 | Seeded simulation verdicts: certified blocks, certified boundaries and completed DKG ceremonies/reshares. Message, drop and node-height counts describe the scenario; simulated time is not wall time. |
| R6-devnet | Number of validated mixed-validator reports (one and/or two clempo signers needed for quorum); not the number of signers. A report includes compared finalized heights, matching boundary outcomes, proposals and reshare participation. Reproduce the run with [DEVNET](DEVNET.md), then score its reports. |
| R7 | Root-checked execution rate on a fixed block range. A single `score -rung R7` measures clempo blocks per timed replay second. A cross-client ratio divides reference wall time by clempo wall time on the same host, exact range and canonical parent; setup and root-check policies must be reported, not inferred from the ratio. See [BENCH](BENCH.md). |

Throughput is blocks or transactions divided by elapsed seconds; Mgas/s is
gas used divided by elapsed seconds and one million. Wall time measures
elapsed time, and process CPU time includes work across threads. Replay/import
phase time may exclude command setup, state hydration or finalization; use
end-to-end command wall time for that comparison. GC time is runtime-reported
collector time. Allocated/consed bytes are cumulative allocations, not resident
memory; peak RSS is the process lifetime peak. GB and MB are powers of ten;
GiB and MiB are powers of two. Inclusive profile samples overlap and need not
sum to 100%. Historical microbenchmarks, paired optimization experiments and
partial scans are distinct from a cross-client R7 score.
