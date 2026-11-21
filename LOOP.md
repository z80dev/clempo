# LOOP: one hill-climb iteration

Each iteration moves the frontier up by one measured step, and never down.

1. **Measure.** `git pull --rebase`, then `bin/clempo-dev score -json`. Compare with the last row of `FRONTIER.jsonl`.
2. **Pick the lowest rung that is not at its frontier** (GOAL.md). On R3 the work is the first block that does not match: `bin/clempo-dev replay` names it and the field that differs, and stops in a restartable condition.
3. **Find the cause, not the symptom.** For a block that does not match, diff its execution against `debug_traceBlockByNumber` (opcode, gas, stack) and the prestate tracer, and read the reference's code in `~/dev/tempo`. Don't guess.
4. **Fix it with a test that fails before and passes after.** Store the block or transaction as a fixture under `tests/<layer>/data/` when it is small, with its source recorded next to it.
5. **Gate.** `scripts/verify.sh` passes. Layer violations are fixed by moving code, never by widening `layers.sexp` without a reason recorded there.
6. **Prove it went up.** Run the score again. If a rung went down, revert.
7. **Record and push.** Append the score as one JSON line to `FRONTIER.jsonl`, update `FRONTIER.md`, commit with a message that names the rung, the old and new score and the cause, then `git pull --rebase origin main && scripts/verify.sh && git push origin main`.

Work in parallel by rung or by directory, never two agents in one directory. Each agent works in its own clone under `~/clempo-wt/<role>` of the bare origin `~/clempo.git`. Long runs (replay over millions of blocks, fetches, fuzzing) run in the background with their log and result JSON under `~/scratch/clempo/`.
