# Replay benchmarks (R7)

Paths under `logs/` are the measuring host's local run logs and result files; they are not part of the repository. Each evidence paragraph gives the command that regenerates them.

The inclusive ranges are fixed. Clempo and Rust use the same mainnet `CLBK`
archive (Rust imports an RLP export); goempo fetches the same blocks from the
archive RPC into its own chunk format.

| `-range` | First | Last | Parent |
|---|---:|---:|---:|
| `early` | 1 | 1,000,000 | genesis |
| `busy` | 9,890,000 | 9,909,999 | 9,889,999 |
| `recent` | 49,410,000 | 49,449,999 | 49,409,999 |

Busy is goempo's published Busy workload. Recent starts 1,084 blocks later than its historical recent trial, so compare only the measurements on the range above.

## Results on the shared dev host

Same Mac (`Mac15,9`), Clempo `4940311` (SBCL 2.6.8 release, persistent
WAL, eight sender-recovery participants), goempo `92e996c` (Go 1.27.1,
vendored build, `GOMAXPROCS=16` as detected on the host), and Rust Tempo
1.19.0-dev release `568b4fb4258c`. Wall / aggregate CPU seconds:

| Range | Clempo replay | Clempo whole command | goempo whole command | Rust whole import |
|---|---:|---:|---:|---:|
| Early | 8.830868 / 9.646726 | 27.15 / 25.51 (cold compile) | 3.38 / 10.19 | 58.714867 / 36.406385 |
| Busy | 22.748234 / 58.196338 | 23.75 / 59.10 | not measured | 35.929221 / 45.764508 |
| Recent | 19.971085 / 23.816374 | 34.87 / 38.48 | not measured | 57.125123 / 24.420341 |

Run order was Rust → Clempo for Early, Clempo → Rust for Busy, Rust →
Clempo for Recent, then goempo Early in the same exclusive benchmark slot.
Clempo's Early command rebuilt the release fasls in a new checkout; its
replay-only timing shows the warm-code interval, not a second run. Clempo
and goempo checked gas, receipts root, bloom and state root at every block,
without healing or skipping. Rust ran its staged import with
`--fail-on-invalid-block`; it checks state roots only at chunk terminals.
Do not interpret these as equal root-check workloads or isolated executor
benchmarks. No Go timings from another host are substituted for missing
samples.

Raw local results: `logs/r7-parity/{clempo,rust}-{early,busy,recent}.{json,log}`
and `logs/r7-parity/go-early.{json,log}`. For Clempo use the commands below
with `/usr/bin/time -l` before `bin/clempo-dev`; for Rust use the documented
import harness with fresh working copies of the exact parents. The Rust
Busy and Recent working parents were APFS clones of previously completed
imports, unwound to 9,889,999 and 49,409,999 outside the slot. The harness
checked the parent-stage heights and both terminal checkpoints.

Early completed 1,000,000 blocks, 1,000,635 transactions and 135,315,780
gas. Go's scored input compared byte for byte against the Rust import's
1,200,376,462-byte export (`sha256`
`4c4236f4db6ea38916a0b2c5ccd56598888560b6b42a6209d22a3d7312198145`).
Go's own replay interval was 3.326632 s wall / 10.206818 s CPU; process
`time -l` recorded 3.38 real, 8.75 user and 1.44 sys. Busy completed
20,000 blocks, 659,246 transactions and 31,499,321,696 gas; Recent
completed 40,000 blocks, 18,733 transactions and 3,768,403,621 gas.

### Earlier measurements

Apple Silicon `Mac15,9`, SBCL 2.6.8 release, Tempo 1.19.0-dev release
(`568b4fb4258c`). Clempo uses persistent WAL, eight sender-recovery participants,
and checks every block's gas, receipts root, bloom, and state root. Rust checks
the terminal state root of each import chunk and runs additional import/indexing
stages. The timings compare these two workflows, not isolated executors.

| Range | Clempo wall / CPU s | Clempo peak RSS GB / GC s | Rust wall / CPU s | Rust peak RSS GB | Clempo / Rust blocks/s | Comparison |
|---|---:|---:|---:|---:|---:|---|
| Early | 9.01 / 12.73 | 2.33 / 0.083 | 58.22 / 37.25 | 7.22 | 110,986 / 17,176 | 6.46× wall throughput, paired |
| Busy (`b81e3e5`) | 22.59–22.77 / 58.14–58.87 | 3.1 / 1.39–1.50 | 36.00 / 43.22 | 3.89 | 878–885 / 556 | 1.58–1.59× wall throughput, historical and unpaired between clients |
| Recent (`828674f`) | 16.76–17.24 / 22.39–22.85 | 6.42–7.31 / 2.79–3.41 | 47.22 / 21.41 | 3.72 | 2,320–2,386 / 847 | 2.74–2.82× wall throughput, historical Rust baseline (unpaired); CPU 1.05–1.07× Rust |

RSS is in decimal GB; the Rust runner has no GC metric. No other benchmark ran
during the new scored trials. The reference node and the certified Clempo
follower remained up; independent RPC probes saw the latter within three
blocks of tip throughout. Busy and Recent's Rust reference timings were not
taken under the same controlled load as the Clempo samples.

- **Early** (`15c118d`): Rust then Clempo back-to-back, both starting from
  genesis. The freshly exported RLP is byte-identical to the recorded corpus.
  Both reached block 1,000,000 (1,000,635 transactions, 135,315,780 gas).
  A first pair overlapped the follower's final catch-up to the moving tip and
  is excluded. The older, unpaired Rust baseline was 89.14 s wall / 41.67 s
  CPU; in the scored pair, Rust spent 35.30 s in Bodies and 11.45 s in Execution.
  Evidence: `logs/bench/r7-early-recent/early-steady/{rust,clempo}.json`.
- **Busy** (`40521c1` equivalent): two Clempo samples, each with 659,246
  transactions and 31,499,321,696 gas. The Clempo A/Bs were paired with each
  other, not with Rust. See [Busy evidence](../FRONTIER.md#evidence) for the
  earlier samples and replay-thread profile.
- **Recent** (`15c118d`): copy the immutable R3 checkpoint at 49,000,000 and
  replay 409,999 blocks without healing or skipping to obtain the exact parent
  outside the timer. The scored range checked 40,000 blocks (18,733 transactions,
  3,768,403,621 gas). The full Clempo command took **50.71 s wall / 51.61 s CPU**
  including parent copy/open and startup, versus **36.41 s / 37.34 s** in the
  timed replay. Evidence: `logs/bench/r7-early-recent/recent-{prepare,clempo}.json`
  and `logs/bench/recent.json`.

### Recent CPU profile

Recent used **1.74×** Rust's historical CPU time despite faster timed replay.
A separate full-range `-profile-mode cpu` run captured 5,731 samples:

| Samples | Call path |
|---:|---|
| 31.4% inclusive | AA execution → EVM `CALL` → ZoneVerifier `VERIFY-WITH-POLICY` → `NITRO-VERIFY-PARSED` → `P384-VERIFY` (certificate chain and attestation signature) → `P384-JOINT` → point doubling/mixed addition → bignum division |
| 28.4% self | `PIPELINE-TAKE`/producer handoff → `CONDITION-NOTIFY` → Darwin `__ulock_wake`; 25.3% came through the consumer. This identifies the sampled synchronization site, not its isolated CPU cost. |
| 23.8% inclusive | `FINISH-BLOCK` → `WRITE-CHANGES` → `DB-SET-STORAGE` → `ENSURE-STORAGE-TRIE`, split between building and hashing storage tries on first write after checkpoint load |

Source: `logs/bench/r7-early-recent/recent-profile.txt`.
After `b81e3e5` (batched pipeline handoff, bulk-built storage tries; paired against
`6c0f9d2` in [FRONTIER](../FRONTIER.md#evidence)) the replay thread's share of first-write
trie builds fell to 5.0% and the pipeline notify to 6.2%; P-384 is 37%
(`logs/busy/es/recent/prof-17b8646.txt`).
Early was faster in both wall and CPU and needed no diagnostic profile.

### Native P-384 and repeated Nitro signatures

Two alternating pairs (B, A, A, B) isolate `828674f` from its parent `4c2db4d`
on the same host, with no overlapping benchmark. All four used the exact parent
`logs/bench/r7-early-recent/recent-parent`, persistent WAL and eight
recovery participants. Every gas/receipt/bloom/state-root comparison passed:
40,000 blocks, 18,733 transactions, 3,768,403,621 gas through 49,449,999.

| Arm | Replay wall s | Replay CPU s | Allocated GB | Peak RSS GB | GC s |
|---|---:|---:|---:|---:|---:|
| B1, native P-384 | 16.763226 | 22.387755 | 9.874 | 6.418 | 2.792 |
| A1, generic P-384 | 29.809454 | 35.525380 | 36.297 | 5.587 | 2.585 |
| A2, generic P-384 | 30.796571 | 35.349221 | 36.297 | 5.580 | 2.545 |
| B2, native P-384 | 17.243408 | 22.851747 | 9.875 | 7.306 | 3.411 |

Mean replay wall falls **30.303 -> 17.003 s (43.9%)**, CPU
**35.437 -> 22.620 s (36.2%)**, allocation **36.297 -> 9.875 GB (72.8%)**.
Peak RSS and GC increased in these observations despite lower allocation;
they must not be inferred from the consing reduction. JSONs and process logs
are under `logs/p384/recent-{A1,A2,B1,B2}/`.

The uncached RFC6979 microbenchmark (`tests/crypto/ec/p384/speed.lisp`) measures
**1,476.173 -> 198.248 µs CPU**, **1,475.947 -> 198.261 µs wall** and
**2,930,154 -> zero bytes per call** with scratch. It does not memoize its
repeated signature. The read-only width-14 G table is 589,824 bytes; six-word
Montgomery arithmetic and native binary inverses keep hot intermediates unboxed.

The separate repetition diagnostic still executed all original verifications:
9,030 calls, 1,854 distinct complete (pubkey,digest,r,s) tuples, and identical
answers on every repeat. A bounded positive-only 64-entry FIFO would hit 6,093
times (67.5%). Production opts into that cache only in private replay contexts.
Digest bytes are copied; certificate expiry, pinned roots, path/policy checks
and gas are never cached. See [contracts and vectors](../tests/crypto/ec/p384/README.md).

A separate post-change all-thread 5-ms CPU profile records **28/3,361 samples
(0.8%) inclusive P384-VERIFY**, versus **1,789/4,801 (37.3%)** in the prior
`prof-17b8646.txt`. It completed the full range but is excluded from throughput
results. Evidence: `logs/p384/recent-profile/{profile.txt,replay.json}`.
The profile command adds `-profile FILE -profile-mode cpu` to:

```sh
CLEMPO_BUILD=release bin/clempo-dev bench -range recent \
  -parent logs/bench/r7-early-recent/recent-parent \
  -datadir FRESH_DIR -result FILE
```

## Clempo

Create an exact parent (once R3 reaches it) and stop all writers before using it:

```sh
CLEMPO_BUILD=release bin/clempo-dev replay -to 9889999 -datadir "$parent" \
  -checkpoint 500000 -result "$parent-replay.json"
CLEMPO_BUILD=release bin/clempo-dev bench -range busy -parent "$parent" \
  -datadir "$new_work_dir" -result "$result"
```

`-datadir` must not exist. The command copies `-parent` before opening it, preserving the original including any WAL tail. It checks the loaded height, block hash and root against the archive before the clock starts. If the parent is genesis, omit `-parent`; pass `-datadir` for the same WAL mode as Busy/Recent, or leave it out for an in-memory run. No healing, skipping or RPC is used during the measured replay. Every block is checked by the R3 executor; a mismatch records a partial result and exits nonzero. A partial result is never a throughput result for the whole range.

Early and Recent use the same persistent `bench` path:

```sh
CLEMPO_BUILD=release bin/clempo-dev bench -range early \
  -datadir "$early_work" -result "$early_result"

# Capture only the immutable checkpoint; never copy R3's live WAL.
mkdir "$recent_parent"
cp "$r3_dd/checkpoint-000049000000.state" "$recent_parent/"
cp "$r3_dd/replay.sexp" "$recent_parent/"
printf 'clempo-store 1 49000000\n' > "$recent_parent/MANIFEST"
CLEMPO_BUILD=release bin/clempo-dev replay -to 49409999 -datadir "$recent_parent" \
  -checkpoint 49409999 -result "$recent_prepare"
jq -e '.last == 49409999 and .r3_height == 49409999 and .stop == null
       and .heals == [] and .skips == []' "$recent_prepare"
CLEMPO_BUILD=release bin/clempo-dev bench -range recent -parent "$recent_parent" \
  -datadir "$recent_work" -result "$recent_result"
```

Capture the R3 checkpoint while its manifest names 49,000,000. R3 replaces
the old checkpoint at the next interval. The replay command advances the
copy to the exact parent and writes a checkpoint there outside the timed run.

The measured interval includes archive reads, decoding, replay, and (in persistent mode) WAL writes and final fsync. It excludes startup, copying and opening the store, and writing the result. CPU and GC are deltas over the interval; peak RSS is for the whole process (including initialization). Always compare the same persistence mode. Repeat with fresh workdirs and interleave clients rather than reporting one best sample.

`replay`, `bench` and `history-build` default to eight sender-recovery
participants. A producer thread reads blocks up to four ahead of the replay
thread and recovers their senders on itself plus seven workers, so recovery
overlaps execution. Transaction execution stays serial and ordered. Use
`-sender-threads 1` to measure serial, unpipelined sender recovery. The replay
commands raise SBCL's allocation between collections to min(2 GiB, heap/8).

For a separate profiling trial, add `-profile "$profile_report" -profile-mode cpu`
to a fresh run. This uses 5 ms SBCL `sb-sprof` sampling with a flat profile and
call graph. `-profile-mode alloc` samples allocation instead of CPU, and
`-profile-threads main` samples only the replay thread, which is the
critical path once recovery is pipelined. Darwin's profiling timer fires on
process CPU time, so absolute sample counts overstate a single thread's time;
compare shares within one profile. The JSON marks profiled runs; do not use
them as baseline timing.

Measure changes with paired A/B runs from separate checkouts: alternate the
order, and never edit a checkout while a run is using it (`bin/clempo-dev`
recompiles changed files at startup). Never run two benchmarks at once on the
shared host.

### Native curve kernels

`217b0c1` measured **28.467858 s replay wall / 61.244520 s CPU**,
**1,106.487 Mgas/s**, 1.999 GB peak RSS on Busy, with eight recovery participants.
The result is complete, with every block checked:
`logs/busy/20261121-085152-217b0c1-1/replay.json`.
It used the commands above with parent
`logs/busy/parent-9889999`. This single observation versus the older
127.429-s run includes other replay changes; it does not isolate crypto.

The isolated, alternating A/B pairs use baseline `d3aace8` and its `53adb01`
application of `217b0c1`: mean CPU **79.373 -> 57.751 s (27.2% less)**,
wall **24.760 -> 24.627 s**, essentially unchanged because the replay thread
is now critical. All four results completed the same Busy fixture. JSON and
logs are under `logs/busy/es/*A-d3aa*` and `*B-curves*`.

For the native kernel microbench:

```sh
CLEMPO_BUILD=release sbcl --dynamic-space-size 2048 --non-interactive \
  --load scripts/build.lisp \
  --eval '(asdf:load-system "clempo-crypto-ec/tests")' \
  --load tests/crypto/ec/secp256k1/speed.lisp
```

The GC-safe release build measures ecrecover **45.094 µs CPU / 45.097 µs wall**
and **zero bytes consed** with caller-owned output/scratch; P256 verifies in
**152.765 µs CPU**, zero consing with scratch. With persistent scratch, fresh
pubkey results allocate 80 bytes; direct `ecrecover-address` returns one 32-byte address
bignum. Bind `clempo.secp256k1:*recovery-scratch*` and
`clempo.p256:*p256-scratch*` separately per worker, never globally share them.
The 25-µs recovery target remains unmet. See
[arithmetic, GC safety, exact timings and allocation-counter rounding](../tests/crypto/ec/secp256k1/performance.md).

## Go replay on the same host

Use goempo's own archive and state formats; its binary cannot open a Clempo
checkpoint. Starting with an empty block cache and datadir, build and prepare
with goempo's vendored dependencies and the same archive RPC that supplied
Clempo's `CLBK` files:

```sh
(cd "$goempo_src" && go build -mod=vendor -o "$goempo_dev" ./cmd/goempo-dev)
"$goempo_dev" fetch-range -rpc "$CLEMPO_RPC" -dir "$go_chunks" -from 0 -to 49449999
```

For each scored range use goempo's `scripts/bench/export-rlp.py` with
`-chunks "$go_chunks" -from "$first" -to "$last" -out "$go_rlp"`, then
`cmp "$go_rlp" "$rust_rlp"` to check all scored blocks are byte-identical to
the Rust import input. This is not part of the timer.

```sh
# First score Early from a fresh datadir with last=1000000.
# For Busy, clone the completed Early datadir to a preparation datadir.
# For Recent, continue the preparation datadir from the exact Busy parent.
# In both cases replay up to parent (9889999 or 49409999) off the clock:
"$goempo_dev" replay -dir "$go_chunks" -datadir "$prepare" \
  -to "$parent" -checkpoint 100000 -heal 0 -diagnose off -result "$prepared"
cp -cR "$prepare" "$go_work"      # Fresh APFS clone; use cp -R elsewhere.

/usr/bin/time -l "$goempo_dev" replay -dir "$go_chunks" -datadir "$go_work" \
  -to "$last" -checkpoint 100000 -heal 0 -diagnose off -result "$go_result"
jq -e --argjson end "$last" --argjson count "$((last-first+1))" \
  '.height == $end and .target == $end and (has("mismatch") | not)
   and .bench.blocks == $count' "$go_result"
```

Also verify the preparation result reached the exact parent with no mismatch
and no replay I/O error. A zero Go process exit status alone does not establish
that the target was reached. Inspect stderr for cache, flush and close errors:
these are not necessarily reflected in the JSON. At startup the replayer checks
the persisted root against its archived parent; every scored block is checked
for gas used, receipts root, logs bloom and state root.

For process-level comparisons use `real` and `user + sys` from macOS
`/usr/bin/time -l` on the Go and Lisp commands, and the Rust harness's
monotonic wall clock and child `wait4` CPU counters. Go's own replay seconds
exclude opening the datadir and flushing at exit; Clempo's exclude copying and
opening the parent but include final WAL sync. The process-level Clempo time
includes its built-in parent copy, whereas the input Rust and Go working
parents are provisioned before timing. Rust still performs its staged import
and checks chunk-terminal roots rather than every block root. Report these
differences rather than interpreting the timings as an executor-only comparison.

## Rust reference

Use a detached datadir (a clone of a stopped archive node, or extract the immutable download). Never unwind or import against the live reference node:

```sh
mkdir -p "$archive_copy"
# $mainnet_snapshot: the local reference node's mainnet snapshot archive (mainnet.tar.lz4)
lz4 -dc "$mainnet_snapshot" | tar -xf - -C "$archive_copy"
cp -cR "$archive_copy" "$busy_work"             # APFS clone; use cp -R elsewhere
"$tempo" stage unwind --datadir "$busy_work" --chain mainnet to-block 9889999
python3 scripts/bench/export-rlp.py --from 9890000 --to 9909999 --out "$busy_rlp"
python3 scripts/bench/rust-import.py --tempo "$tempo" --datadir "$busy_work" \
  --from 9890000 --to 9909999 --rlp "$busy_rlp" --log "$log" --out "$result"
```

`export-rlp.py` writes original wire bytes with no re-encoding. The Rust runner
checks the starting stage checkpoints and exact input heights before timing the
import process, including its startup and shutdown. It requires both terminal
`MerkleExecute` and `Finish` checkpoints at the requested height. The result
includes wall/CPU, peak RSS, gas from the corpus, and stage times summed across
import chunks. The import may split large files into multiple chunks.

For Early, export from the same archive and initialize a fresh Rust datadir from genesis:

```sh
python3 scripts/bench/export-rlp.py --from 1 --to 1000000 --out "$early_rlp"
python3 scripts/bench/rust-import.py --tempo "$tempo" \
  --datadir "$early_rust_work" --from 1 --to 1000000 \
  --rlp "$early_rlp" --log "$early_rust_log" --out "$early_rust_result"
```

`tempo re-execute` is read-only and starts from historical parent state, but verifies gas/receipts and the resulting change sets rather than recomputing state roots. `tempo import --fail-on-invalid-block` runs the staged pipeline and checks the Merkle root at each import chunk’s terminal checkpoint, **not every block's root**. This follows goempo's Rust baseline and is a lower root-check workload than Clempo. Report that distinction alongside any comparison. Rust includes the import indexing stages; use the per-stage numbers to distinguish execution from full import wall time. Neither client’s metrics include provisioning the input corpus or parent state.

For the separate read-only Busy cross-check reported in `FRONTIER.md`:

```sh
/usr/bin/time -l "$tempo" re-execute --datadir "$archive_copy" --chain mainnet \
  --from 9890000 --to 9909999 --num-tasks 1 --blocks-per-chunk 20000 \
  --color never --log.file.max-files 0
```

One task and one chunk ensure the entire measured range executes from the
same historical parent state. Keep this result separate from staged import.

### Recorded Busy and Recent baselines

The detached archive is `logs/bench/rust-snapshot` (height
50,050,994). The Busy and Recent working copies were unwound to 9,889,999
and 49,409,999 respectively, then imported with:

```sh
root=logs/bench
python3 scripts/bench/rust-import.py --tempo ~/dev/tempo/target/release/tempo \
  --datadir "$root/rust-busy" --from 9890000 --to 9909999 \
  --rlp "$root/busy.rlp" --log "$root/busy.log" --out "$root/busy.json"
python3 scripts/bench/rust-import.py --tempo ~/dev/tempo/target/release/tempo \
  --datadir "$root/rust-recent" --from 49410000 --to 49449999 \
  --rlp "$root/recent.rlp" --log "$root/recent.log" --out "$root/recent.json"
```

The runner refuses to overwrite an existing import log or start from the
wrong parent. To rerun, use a fresh working datadir at the parent and new log
and result paths. Never use the live node's datadir.
