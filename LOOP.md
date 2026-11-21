# LOOP: one hill-climb iteration

Each iteration makes one measured change and checks for regressions.

1. **Measure.** `git pull --rebase`, then use `bin/clempo-dev score -help` for the inputs to `bin/clempo-dev score -rung NAME -json`. Compare like with like using the revision and inputs in `FRONTIER.jsonl`.
2. **Pick the lowest rung that is not at its frontier** (GOAL.md). On R3 the work is the first block that does not match: `bin/clempo-dev replay` names it and the field that differs, and stops in a restartable condition.
3. **Find the cause, not the symptom.** For a block that does not match, diff its execution against `debug_traceBlockByNumber` (opcode, gas, stack) and the prestate tracer, and read the reference's code in `~/dev/tempo`. Don't guess.
4. **Fix it with a test that fails before and passes after.** Store the block or transaction as a fixture under `tests/<layer>/data/` when it is small, with its source recorded next to it.
5. **Gate.** `scripts/verify.sh` must pass. Move accidental I/O across the declared boundary; record why any new boundary or counted policy entry is necessary.
6. **Measure again.** Fix regressions. If the scoring rules changed, record both methods and don't present the new denominator as comparable to the old one.
7. **Record and push.** Append the score as one JSON line to `FRONTIER.jsonl`, update `FRONTIER.md`, commit with a message that names the rung, the old and new score and the cause, then `git pull --rebase origin main && scripts/verify.sh && git push origin main`.

Work in parallel by rung or directory, one owner per directory and each in a separate clone. Put long-run logs and reports in a local run directory; publish the revision, inputs and reproducible invocation in `FRONTIER.md`.
