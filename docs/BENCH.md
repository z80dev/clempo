# Replay benchmarks (R7)

These inclusive ranges use mainnet raw blocks from the same `CLBK` archive on both clients:

| `-range` | First | Last | Parent |
|---|---:|---:|---:|
| `early` | 1 | 1,000,000 | genesis |
| `busy` | 9,890,000 | 9,909,999 | 9,889,999 |
| `recent` | 49,410,000 | 49,449,999 | 49,409,999 |

Busy is goempo's published Busy workload. Recent starts 1,084 blocks later than its historical recent trial, so compare only the measurements on the range above.

## Results on the shared dev host

Apple Silicon `Mac15,9`, SBCL 2.6.8 release, Tempo 1.19.0-dev release
(`568b4fb4258c`). Clempo uses persistent WAL, eight sender-recovery participants,
and checks every block's gas, receipts root, bloom, and state root. Rust checks
the terminal state root of each import chunk and runs additional import/indexing
stages. The timings compare these two workflows, not isolated executors.

| Range | Clempo wall / CPU s | Clempo peak RSS GB / GC s | Rust wall / CPU s | Rust peak RSS GB | Clempo / Rust blocks/s | Comparison |
|---|---:|---:|---:|---:|---:|---|
| Early | 9.01 / 12.73 | 2.33 / 0.083 | 58.22 / 37.25 | 7.22 | 110,986 / 17,176 | 6.46× wall throughput, paired |
| Busy (`b81e3e5`) | 22.59–22.77 / 58.14–58.87 | 3.1 / 1.39–1.50 | 36.00 / 43.22 | 3.89 | 878–885 / 556 | 1.58–1.59× wall throughput, historical and unpaired between clients |
| Recent (`b81e3e5`) | 29.66–32.68 / 35.30–37.46 | 5.58–7.11 / 2.57–4.04 | 47.22 / 21.41 | 3.72 | 1,224–1,349 / 847 | 1.44–1.59× wall throughput, historical Rust baseline (unpaired) |

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
  Evidence: `~/scratch/clempo/bench/r7-early-recent/early-steady/{rust,clempo}.json`.
- **Busy** (`40521c1` equivalent): two Clempo samples, each with 659,246
  transactions and 31,499,321,696 gas. The Clempo A/Bs were paired with each
  other, not with Rust. See [Busy evidence](../FRONTIER.md#evidence) for the
  earlier samples and replay-thread profile.
- **Recent** (`15c118d`): copy the immutable R3 checkpoint at 49,000,000 and
  replay 409,999 blocks without healing or skipping to obtain the exact parent
  outside the timer. The scored range checked 40,000 blocks (18,733 transactions,
  3,768,403,621 gas). The full Clempo command took **50.71 s wall / 51.61 s CPU**
  including parent copy/open and startup, versus **36.41 s / 37.34 s** in the
  timed replay. Evidence: `~/scratch/clempo/bench/r7-early-recent/recent-{prepare,clempo}.json`
  and `~/scratch/clempo/bench/recent.json`.

### Recent CPU profile

Recent used **1.74×** Rust's historical CPU time despite faster timed replay.
A separate full-range `-profile-mode cpu` run captured 5,731 samples:

| Samples | Call path |
|---:|---|
| 31.4% inclusive | AA execution → EVM `CALL` → ZoneVerifier `VERIFY-WITH-POLICY` → `NITRO-VERIFY-PARSED` → `P384-VERIFY` (certificate chain and attestation signature) → `P384-JOINT` → point doubling/mixed addition → bignum division |
| 28.4% self | `PIPELINE-TAKE`/producer handoff → `CONDITION-NOTIFY` → Darwin `__ulock_wake`; 25.3% came through the consumer. This identifies the sampled synchronization site, not its isolated CPU cost. |
| 23.8% inclusive | `FINISH-BLOCK` → `WRITE-CHANGES` → `DB-SET-STORAGE` → `ENSURE-STORAGE-TRIE`, split between building and hashing storage tries on first write after checkpoint load |

Source: `~/scratch/clempo/bench/r7-early-recent/recent-profile.txt`.
After `b81e3e5` (batched pipeline handoff, bulk-built storage tries; paired against
`6c0f9d2` in [FRONTIER](../FRONTIER.md#evidence)) the replay thread's share of first-write
trie builds fell to 5.0% and the pipeline notify to 6.2%; P-384 is 37%
(`~/scratch/clempo/busy/es/recent/prof-17b8646.txt`).
Early was faster in both wall and CPU and needed no diagnostic profile.

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
`~/scratch/clempo/busy/20261121-085152-217b0c1-1/replay.json`.
It used the commands above with parent
`~/scratch/clempo/busy/parent-9889999`. This single observation versus the older
127.429-s run includes other replay changes; it does not isolate crypto.

The isolated, alternating A/B pairs use baseline `d3aace8` and its `53adb01`
application of `217b0c1`: mean CPU **79.373 -> 57.751 s (27.2% less)**,
wall **24.760 -> 24.627 s**, essentially unchanged because the replay thread
is now critical. All four results completed the same Busy fixture. JSON and
logs are under `~/scratch/clempo/busy/es/*A-d3aa*` and `*B-curves*`.

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

## Rust reference

Use a detached datadir (a clone of a stopped archive node, or extract the immutable download). Never unwind or import against the live reference node:

```sh
mkdir -p "$archive_copy"
lz4 -dc ~/clempo-infra/snapshot/mainnet.tar.lz4 | tar -xf - -C "$archive_copy"
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

The detached archive is `~/scratch/clempo/bench/rust-snapshot` (height
50,050,994). The Busy and Recent working copies were unwound to 9,889,999
and 49,409,999 respectively, then imported with:

```sh
root=~/scratch/clempo/bench
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
