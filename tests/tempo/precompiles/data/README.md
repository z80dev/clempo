# Precompile test data

## oracle.tar.gz — the R3p corpus

zempo's reference-oracle corpus (`src/tempo/precompiles/testdata` at zempo commit 6d669bc1):
4,992 generated Tempo precompile calls across T0..T14, each run through the reference's own
precompiles (tempoxyz/tempo, via zempo's `tools/oracle` `precompile_call`) over the same
pre-state. Copied byte for byte from goempo
(goempo's repository, `internal/tempo/precompiles/testdata/oracle.tar.gz`, goempo commit
92e996ca), which took it from zempo.

Members: `cases.jsonl` / `expected.jsonl` and `forks/<area>.cases.jsonl` /
`forks/<area>.expected.jsonl`, one JSON object per line, matched by line and `name`.

- A case: `name`, `spec` (fork), `block` (`number`, `timestamp`, `beneficiary`), `accounts`
  (`address`, `code` or null, optional `commitment`, `storage` as `[slot, value]` pairs),
  `transient` (`address`, `slot`, `value`), `call` (`caller`, `target`, `code_address`,
  `data`, `gas`, `static`).
- An expectation: `status` (`success`, `revert`, `halt`, `fatal`, `not-a-precompile`),
  `output`, `gas_used`, `gas_refunded`, `logs`, `storage` (changed slots) and `accounts`
  (accounts whose code or configuration commitment changed: `code_hash`, `commitment`).

Only the caller is warm when a call starts. Output and gas are compared for success and
revert; refund, logs, slots and accounts for success only; halts and fatal errors by status.

`oracle_known.txt` lists cases expected to differ, with the reason.
