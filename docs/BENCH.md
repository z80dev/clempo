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
| Busy | 22.748234 / 58.196338 | 23.75 / 59.10 | 27.28 / 71.45 | 35.929221 / 45.764508 |
| Recent | 19.971085 / 23.816374 | 34.87 / 38.48 | not measured | 57.125123 / 24.420341 |

Run order was Rust → Clempo for Early, Clempo → Rust for Busy, Rust →
Clempo for Recent, then goempo Early in the same exclusive benchmark slot.
Go Busy ran in a later exclusive slot after the exact parent was ready.
Clempo's Early command rebuilt the release fasls in a new checkout; its
replay-only timing shows the warm-code interval, not a second run. Clempo
and goempo checked gas, receipts root, bloom and state root at every block,
without healing or skipping. Rust ran its staged import with
`--fail-on-invalid-block`; it checks state roots only at chunk terminals.
Do not interpret these as equal root-check workloads or isolated executor
benchmarks. No Go timings from another host are substituted for missing
samples.

### Early after the per-block work (`3327105`, measured on `f5f79c5`)

Three alternating pairs in an exclusive slot (host load average ~10, the
reference node running), goempo `92e996c` first in each pair with
the same replay command documented below (`goempo-dev replay -dir go-chunks -to 1000000
-datadir FRESH -checkpoint 100000 -heal 0 -diagnose off`), then
`CLEMPO_BUILD=release bin/clempo-dev bench -range early -datadir FRESH`
(warm fasls, disk-backed store, `(:every-ms 1000)` log sync). Both checked
every block's roots and reached 1,000,000.

| Pair | goempo replay s / CPU s | Clempo replay s / CPU s | Clempo GC s / peak RSS GB |
|---|---:|---:|---:|
| 1 | 3.286 / 9.67 | 2.658 / 6.69 | 0.061 / 2.32 |
| 2 | 3.287 / 9.90 | 2.796 / 6.73 | 0.056 / 2.31 |
| 3 | 3.383 / 9.67 | 2.766 / 6.45 | 0.036 / 2.31 |

Median **2.766 s vs 3.287 s: Clempo 1.19× goempo's wall throughput** with
0.68× its CPU; against the Rust import above (58.71 s), 21.2× (goempo 17.9×).
What changed for early mainnet, whose blocks carry one system transaction
(main-thread profile before: 7.9 µs/block, ~21 KB consed/block):
the replay thread no longer wakes the block producer (Darwin wakeups cost the
waker tens of µs) and takes the prefetched queue whole; the producer decodes,
hashes and recovers batches of light blocks on the worker pool; one
`clempo.state` world is reused per replay; the subblocks-metadata RLP is
checked in place; system calls to absent non-precompile targets skip the
frame; block hashes, fork times, receipts roots of identical receipts and
the EVM spec are memoized; blooms and `octets=` work a word at a time;
keccak contexts live on the stack. Busy in the same build: 36.2 / 36.8 s
against 37.3 / 36.9 s with `3327105` reverted (paired), so the Early work
costs Busy nothing; Busy's rise from ~23 s was `f5f79c5`'s store path, fixed by
`6354034`: on `2cef7a8` one unpaired sample gives Busy 23.91 s (64.4 s CPU, 3.6 GB)
and Early 3.00 s (load average ~13).

### Disk-backed store, isolated change

Two alternating pairs on the same Mac compared `824be08` with the same
revision plus the store changes through `6354034`. Both builds checked
every block root. Busy replay wall times were **23.04 / 22.27 s** with
in-memory working state versus **23.32 / 23.00 s** with the disk-backed
store; peak RSS was **3.46–3.51 GB** versus **3.31–3.59 GB**. Early replay
wall times were **7.73 / 8.32 s** versus **8.25 / 7.93 s**. These pairs
isolate the storage changes; they are not the per-block optimization
comparison above.

### Bounded-memory Recent replay on Linux

On Linux x86-64 with SBCL 2.6.8 release, `6354034` replayed all 40,000
Recent blocks with every root checked, in **11.4 s** (3,503 blocks/s;
24 s including startup). The process used a 4,096 MiB heap limit and
peaked at **2.58 GB RSS** (Linux `VmHWM`), with 325 MB of live heap at the
end. The in-memory store (`824be08`) exhausted that heap; at 16,384 MiB
it replayed in **34.8 s**, took about **94 s** including checkpoint
load and trie rebuild, and peaked at **5.63 GB RSS**. Both used the
same canonical parent at 49,409,999. The one-time legacy checkpoint
import was excluded from the replay: 532 s, 8.1 GB peak RSS, and a
2.2 GB KV datadir versus a 580 MB checkpoint.

```sh
CLEMPO_HEAP=4096 CLEMPO_BUILD=release bin/clempo-dev bench -range recent \
  -parent "$KV_PARENT" -datadir "$FRESH_DIR" -result "$REPORT" \
  -cache-accounts 65536 -cache-slots 524288 -cache-trie-nodes 262144 \
  -cache-code-mb 64 -cache-dirty 1000000
```

This measures clempo's bounded-memory storage, not R7 against Rust.

Raw local results: `logs/r7-parity/{clempo,rust}-{early,busy,recent}.{json,log}`
and `logs/r7-parity/go-{early,busy}.{json,log}`. For Clempo use the commands below
with `/usr/bin/time -l` before `bin/clempo-dev`; for Rust use the documented
import harness with fresh working copies of the exact parents. The Rust
Busy and Recent working parents were APFS clones of previously completed
imports, unwound to 9,889,999 and 49,409,999 outside the slot. The harness
checked the parent-stage heights and both terminal checkpoints.

Early completed 1,000,000 blocks, 1,000,635 transactions and 135,315,780
gas. Go's scored input compared byte for byte against the Rust import's
1,200,376,462-byte export (`sha256`
`4c4236f4db6ea38916a0b2c5ccd56598888560b6b42a6209d22a3d7312198145`).
Go reported 3.326632 s replay wall and 10.206818 s lifetime CPU at sampling.
`time -l` recorded 3.38 real, 8.75 user and 1.44 sys. Busy completed
20,000 blocks, 659,246 transactions and 31,499,321,696 gas. The Go Busy
preparation replay advanced its Early datadir another 8,889,999 checked
blocks in 161.497949 s replay wall to parent 9,889,999, outside the scored
run. The scored Go input compared byte for byte to the Rust export
(`sha256` `45c1f8b946f9ca7c9268111bc8848e369a31040fb9028c5b5e76c015c1df23c1`).
Go reported 26.508838 s replay wall; `time -l` recorded 27.28 real, 68.56
user and 2.89 sys. Go used less peak RSS on Busy (1.78 GB vs. 3.34 GB
Clempo and 3.55 GB Rust); Clempo used less CPU than Go but more than Rust.
Recent completed 40,000 blocks, 18,733 transactions and 3,768,403,621 gas.

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

# Copy a stopped, unhealed seed at or below 49,409,999, not a running datadir.
# With no seed, omit cp and let replay start from genesis.
cp -R "$recent_seed" "$recent_parent"
CLEMPO_BUILD=release bin/clempo-dev replay -to 49409999 -datadir "$recent_parent" \
  -checkpoint 500000 -result "$recent_prepare"
jq -e '.last == 49409999 and .r3_height == 49409999 and .stop == null
       and .heals == [] and .skips == []' "$recent_prepare"
CLEMPO_BUILD=release bin/clempo-dev bench -range recent -parent "$recent_parent" \
  -datadir "$recent_work" -result "$recent_result"
```

Prepare the exact parent outside the timed run. Current datadirs keep state
in the disk-backed KV store; they do not produce a `checkpoint-N.state` file
at each flush. The store can import stopped format-1 datadirs, and explicit
exports via `clempo.store:write-checkpoint` remain available.

Datadir `replay` and `bench` use `(:every-ms 1000)`: the WAL syncs on
the one-second interval, at a flush, and on close. The replay can resume
from its last durable block (`clempo.store:store-durable-number`). This
differs from the node's default `:block` sync policy.
The measured interval includes archive reads, decoding, execution, WAL
writes and final sync, but excludes state preparation, copying/opening the
datadir and writing the result. CPU and GC are interval deltas; peak RSS
includes initialization. Compare like persistence modes with interleaved
fresh runs. The original three-client table predates the disk-backed store.

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

Rust's MDBX default is durable sync, but the staged import commits at stage
execution boundaries (`Pipeline::execute_stage_to_completion`), not after
each executed block. Its default execution commit limits are 500,000 blocks
or 5,000,000 changes. Clempo's current time-based replay/bench WAL sync
and Rust's stage commits are different durability workloads.

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

# RPC load (`clempo-dev rpc-load`)

Both nodes serve **head 50,049,999** on the shared dev host: Clempo `rpc-serve`
(release build of `9231bf6`, archive backend, history to 50,049,999) and the Rust
reference `tempo node` (568b4fb4258c) on an APFS clone of the immutable snapshot,
unwound to the same height, with networking off. The load generator runs in a third
SBCL process on the same host, one keep-alive connection per client thread. The
corpus is 400 seeded requests per method drawn from blocks 50,049,000..50,049,999
(1,000 blocks, 566 transactions, 180 senders, 3 TIP-20 tokens): `eth_getBalance` and
TIP-20 `balanceOf` `eth_call` of senders at the head, full `eth_getBlockByNumber`,
10-block `eth_getLogs` of one token, `debug_traceTransaction` with `callTracer`, and
`eth_getProof` (sender with no keys, or a token with one slot). Each cell is the
median of 3 interleaved rounds of 400 requests, alternating which node goes first.

**Answers:** all 2,400 requests compared first; **2,400 identical** (after
`3640343` fixed absent-account `eth_getProof`, which differed on 43/400). No errors
in any load run.

| Method | Clients | Clempo req/s | Rust req/s | Clempo/Rust | Clempo p50/p99 ms | Rust p50/p99 ms |
|---|---:|---:|---:|---:|---:|---:|
| `eth_getBalance` | 1 | 28,458 | 37,023 | 0.77 | 0.035/0.067 | 0.026/0.049 |
| `eth_getBalance` | 16 | 140,007 | 119,546 | 1.17 | 0.108/0.143 | 0.133/0.162 |
| `eth_getBalance` | 64 | 115,942 | 148,246 | 0.78 | 0.473/0.697 | 0.386/0.506 |
| `eth_call` | 1 | 2,093 | 13,853 | 0.15 | 0.468/0.693 | 0.068/0.125 |
| `eth_call` | 16 | 21,520 | 99,231 | 0.22 | 0.610/1.216 | 0.146/0.291 |
| `eth_call` | 64 | 22,343 | 77,913 | 0.29 | 2.776/5.158 | 0.520/1.038 |
| `eth_getBlockByNumber` | 1 | 16,034 | 32,903 | 0.49 | 0.051/0.176 | 0.026/0.074 |
| `eth_getBlockByNumber` | 16 | 99,231 | 148,093 | 0.67 | 0.136/0.403 | 0.100/0.170 |
| `eth_getBlockByNumber` | 64 | 104,478 | 137,045 | 0.76 | 0.498/0.829 | 0.374/0.642 |
| `eth_getLogs` | 1 | 6,582 | 9,877 | 0.67 | 0.134/0.446 | 0.085/0.231 |
| `eth_getLogs` | 16 | 19,045 | 70,834 | 0.27 | 0.811/1.406 | 0.189/0.511 |
| `eth_getLogs` | 64 | 18,761 | 74,382 | 0.25 | 3.256/5.841 | 0.698/1.788 |
| `debug_traceTransaction` | 1 | 594 | 1,781 | 0.33 | 1.162/5.702 | 0.417/1.601 |
| `debug_traceTransaction` | 16 | 5,878 | 14,913 | 0.39 | 1.732/6.332 | 0.808/2.414 |
| `debug_traceTransaction` | 64 | 4,794 | 15,487 | 0.31 | 6.926/29.665 | 3.780/5.671 |
| `eth_getProof` | 1 | 9,150 | 7,744 | 1.18 | 0.113/0.202 | 0.135/0.225 |
| `eth_getProof` | 16 | 83,247 | 25,324 | 3.29 | 0.172/0.326 | 0.603/1.763 |
| `eth_getProof` | 64 | 77,002 | 22,653 | 3.40 | 0.729/1.735 | 2.233/7.639 |

Clempo is faster than Rust only on `eth_getProof` and within 0.77–1.17x on `eth_getBalance`. goempo's
published ratios (height 49,449,999, pinned cores, a Python client) are higher on
every shared method: `eth_call` 1.28/0.64/0.57 at 1/16/64 clients, `eth_getLogs`
2.01 at 1 client. An earlier run (`rpc-load-1`, while another agent's replay was still
being paused) put every row on the same side of 1.0 except `eth_getBalance` at 16
clients (0.82), but moved some ratios by up to 1.7x (`eth_getProof` at 16 clients
1.99, `eth_getLogs` at 16 clients 0.38): the host is shared and unpinned, so read the
direction and rough size of each row, not its exact value.

The bottlenecks, from an all-thread `sb-sprof` CPU profile of the same build serving
in-process under 16 clients (`profile-graph.txt`, `profile-trace-block.txt`):

- `eth_getLogs`: 87% of samples are `PROBE-FILE` (`stat`/`getattrlist`) from the
  archive backend's `BACKEND-BLOCK-RAW`, one file-system query per block per request.
  It stops scaling past 16 clients.
- `eth_call` and `debug_traceTransaction`: about 30% in `CLEMPO.HISTORY::SEGMENT-FLOOR`
  (a varint scan per state read), plus `lseek`/`read` per lookup; the traces add about
  12% mutex wait.
- No global state lock: reads share the backend's reader count and `eth_getBalance`
  and `eth_getProof` scale to 16 clients.

Results, logs and profiles: `~/scratch/clempo/fuzzload/results/` on the measuring
host (`rpc-load-{1,2}.{json,txt,log}`, `profile-*.txt`).

### Reproduce

```sh
cp -cR "$rust_snapshot" "$rust_rpc"          # detached archive; never the live node
"$tempo" stage unwind --datadir "$rust_rpc" --chain mainnet to-block 50049999
CLEMPO_BUILD=release bin/clempo-dev rpc-serve -port 18546 -upstream http://127.0.0.1:18545 &
"$tempo" node --chain mainnet --datadir "$rust_rpc" --follow ws://127.0.0.1:18546 \
  --follow.nocertify --http --http.port 18545 --http.api eth,debug,trace,net,web3,rpc \
  --ipcdisable --disable-dns-discovery --disable-discv4-discovery \
  --disable-discv5-discovery --max-outbound-peers 0 --max-inbound-peers 0 \
  --port 30399 --discovery.port 30399 --authrpc.port 18551 &
bin/clempo-dev rpc-load -a http://127.0.0.1:18546 -b http://127.0.0.1:18545 \
  -from 50049000 -to 50049999 -n 400 -total 400 -rounds 3 -concurrency 1,16,64 \
  -result "$result"
```

The Rust node needs a `--follow` upstream that accepts a WebSocket; pointing it at the
Clempo server at the same head keeps it idle. `-upstream` lets Clempo fill its
receipts store (root-checked) on first use; the diff pass warms it before timing.
`rpc-load` exits 1 when any answer differs; `-no-diff` and `-no-load` run one phase.
Never point it at the live reference node: its rule is two threads at most.

### After the RPC speed work (`feffe50`)

Same host, same heads and corpus (`-n 400`), `-total 4000 -rounds 3`, client and server
release builds (`rpc-load-3` in the results directory). Medians of 3 interleaved rounds;
2,400 of 2,400 answers identical. Clempo/Rust req/s, with the first table's ratio and
goempo's published 1-client ratio:

| Method | 1 client | 16 clients | 64 clients | before (1/16/64) | goempo (1) |
|---|---:|---:|---:|---|---:|
| `eth_getBalance` | 0.97 | 1.01 | 0.91 | 0.77 / 1.17 / 0.78 | |
| `eth_call` | **1.33** | **1.43** | 0.83 | 0.15 / 0.22 / 0.29 | 1.28 |
| `eth_getBlockByNumber` | 1.03 | 0.96 | 0.95 | 0.49 / 0.67 / 0.76 | |
| `eth_getLogs` | **2.63** | **1.77** | **2.05** | 0.67 / 0.27 / 0.25 | 2.01 |
| `debug_traceTransaction` | 0.99 | 0.99 | 0.86 | 0.33 / 0.39 / 0.31 | |
| `eth_getProof` | 2.38 | 3.16 | 2.21 | 1.18 / 3.29 / 3.40 | |

An earlier run of the same build minus `feffe50` (load average 14-35 from other agents)
had `eth_call` 1.56 / 1.51 / 2.12 and `eth_getLogs` 2.70 / 2.83 / 1.79: at 64 clients the
ratios move by up to 2.5x between runs (clempo's p99 there is GC-bound), so read the
1- and 16-client columns. `eth_getBalance` is a constant answer: at 16 clients both
servers are bound by the shared SBCL load client and the kernel's socket calls.

What changed (direct per-request time inside the server, warm, 1 thread):

| Method | before | after | Main causes |
|---|---:|---:|---|
| `eth_call` | 439 us | 18 us | history probe cache (`1127661`); mapped hash index, no bucket read per block-by-hash (`3888eb1`) |
| `eth_getLogs` | 43 us | 8 us | bloom bits hashed once per query and tested in place; logs rendered once per block (`2eef1b2`) |
| `eth_getBlockByNumber` | 34 us | 4 us | JSON writer and hex without FORMAT (`acc0cac`); block JSON kept per cached block (`2eef1b2`) |
| `debug_traceTransaction` | 1,554 us | ~620 us | history probes; P-384 memo per connection thread (`feffe50`) |
| `eth_getBalance` (HTTP) | | | request lines scanned in a per-connection buffer, no wakeup syscall per request (`72443d9`, `3888eb1`) |

Open: `debug_traceTransaction` spends ~40% in per-op tracer hooks (CLOS dispatch,
`MAKE-PENDING`, `STEP-STOP-AND-COST`) although `callTracer` keeps no steps; a
no-steps interpreter path would put it well above Rust (EvmJitAgent agreed to the edit
in `src/evm/tracing.lisp`; not done). At 64 clients thread-per-connection loses to
Rust's 16 kqueue workers.
