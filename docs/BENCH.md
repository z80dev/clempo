# Replay benchmarks (R7)

These inclusive ranges use mainnet raw blocks from the same `CLBK` archive on both clients:

| `-range` | First | Last | Parent |
|---|---:|---:|---:|
| `early` | 1 | 1,000,000 | genesis |
| `busy` | 9,890,000 | 9,909,999 | 9,889,999 |
| `recent` | 49,410,000 | 49,449,999 | 49,409,999 |

Busy is goempo's published Busy workload. Recent starts 1,084 blocks later than its historical recent trial, so compare only the measurements on the range above.

## Clempo

Create an exact parent (once R3 reaches it) and stop all writers before using it:

```sh
CLEMPO_BUILD=release bin/clempo-dev replay -to 9889999 -datadir "$parent" \
  -checkpoint 500000 -result "$parent-replay.json"
CLEMPO_BUILD=release bin/clempo-dev bench -range busy -parent "$parent" \
  -datadir "$new_work_dir" -result "$result"
```

`-datadir` must not exist. The command copies `-parent` before opening it, preserving the original including any WAL tail. It checks the loaded height, block hash and root against the archive before the clock starts. If the parent is genesis, omit `-parent`; pass `-datadir` for the same WAL mode as Busy/Recent, or leave it out for an in-memory run. No healing, skipping or RPC is used during the measured replay. Every block is checked by the R3 executor; a mismatch records a partial result and exits nonzero. A partial result is never a throughput result for the whole range.

The measured interval includes archive reads, decoding, replay, and (in persistent mode) WAL writes and final fsync. It excludes startup, copying and opening the store, and writing the result. CPU and GC are deltas over the interval; peak RSS is for the whole process (including initialization). Always compare the same persistence mode. Repeat with fresh workdirs and interleave clients rather than reporting one best sample.

`replay`, `bench` and `history-build` default to eight sender-recovery
participants. A producer thread reads blocks up to four ahead of the replay
thread and recovers their senders on itself plus seven workers, so recovery
overlaps execution. Transaction execution stays serial and ordered. Use
`-sender-threads 1` to measure serial, unpipelined sender recovery. The replay
commands raise SBCL's allocation between collections to min(2 GiB, heap/8).

For a separate profiling trial, add `-profile "$profile_report"` to a Busy
run. This uses 5 ms SBCL `sb-sprof` sampling with a flat profile and call
graph. `-profile-mode alloc` samples allocation instead of CPU, and
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

`export-rlp.py` writes original wire bytes with no re-encoding. The Rust runner checks that all import stages are at the starting parent checkpoint, validates exact input heights, and checks both terminal `MerkleExecute` and `Finish` checkpoints. For Early, pass a nonexistent datadir to initialize from genesis. The result includes wall/CPU, peak RSS, total gas from the corpus, and stage times summed across import chunks. The import may split large files into multiple chunks.

`tempo re-execute` is read-only and starts from historical parent state, but verifies gas/receipts and the resulting change sets rather than recomputing state roots. `tempo import --fail-on-invalid-block` runs the staged pipeline and checks the Merkle root at each import chunk’s terminal checkpoint, **not every block's root**. This follows goempo's Rust baseline and is a lower root-check workload than Clempo. Report that distinction alongside any comparison. Rust includes the import indexing stages; use the per-stage numbers to distinguish execution from full import wall time. Neither client’s metrics include provisioning the input corpus or parent state.

For the separate read-only Busy cross-check reported in `FRONTIER.md`:

```sh
/usr/bin/time -l "$tempo" re-execute --datadir "$archive_copy" --chain mainnet \
  --from 9890000 --to 9909999 --num-tasks 1 --blocks-per-chunk 20000 \
  --color never --log.file.max-files 0
```

One task and one chunk ensure the entire measured range executes from the
same historical parent state. Keep this result separate from staged import.

### Active Busy baseline on the dev host

The detached archive is `~/scratch/clempo/bench/rust-snapshot` (height
50,050,994). The Busy working copy is `~/scratch/clempo/bench/rust-busy`.
The restarted persistent runner was launched as PID **57382** (also recorded
in `~/scratch/clempo/bench/busy-job.pid`). It first resumes
`tempo stage unwind --datadir "$root/rust-busy" --chain mainnet --color never to-block 9889999`,
appending to `"$root/busy-unwind.log"`. On success it runs:

```sh
root=~/scratch/clempo/bench
python3 scripts/bench/rust-import.py --tempo ~/dev/tempo/target/release/tempo \
  --datadir "$root/rust-busy" --from 9890000 --to 9909999 \
  --rlp "$root/busy.rlp" --log "$root/busy.log" --out "$root/busy.json"
```

The wrapper's diagnostics are in `"$root/busy-job.log"`. If a rerun is
necessary, verify the old process ended and use new log/result paths; the
runner refuses to overwrite an existing import log or start from a wrong
parent. No command uses the live node's datadir.
