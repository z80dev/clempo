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

For a separate profiling trial after R3 exceeds 10M, add `-profile "$profile_report"` to a Busy run. The report uses 5 ms SBCL `sb-sprof` wall-time sampling of the replay thread, with a flat profile and call graph. The JSON marks profiled runs; do not use them as baseline timing. Sort self samples separately from cumulative samples, and compare profiles on the same workload before attributing a regression.

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

`export-rlp.py` writes original wire bytes with no re-encoding. The Rust runner checks the starting `Finish` checkpoint, the exact input heights, and both terminal `MerkleExecute` and `Finish` checkpoints. For Early, pass a nonexistent datadir to initialize from genesis. The result includes wall/CPU, peak RSS, total gas from the corpus, and stage times summed across import chunks. The import may split large files into multiple chunks.

`tempo re-execute` is read-only and starts from historical parent state, but verifies gas/receipts and the resulting change sets rather than recomputing state roots. `tempo import --fail-on-invalid-block` runs the staged pipeline and checks the Merkle root at each import chunk’s terminal checkpoint, **not every block's root**. This follows goempo's Rust baseline and is a lower root-check workload than Clempo. Report that distinction alongside any comparison. Rust includes the import indexing stages; use the per-stage numbers to distinguish execution from full import wall time. Neither client’s metrics include provisioning the input corpus or parent state.
