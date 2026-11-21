# Payload builder fixtures

`mainnet-user-blocks-3211-3874.sexp` holds, for every Tempo mainnet block in 1..3874 that
carries a user transaction (26 blocks, 3211 through 3874), the raw block and the raw header of
its parent, one `(:number N :parent-header "0x…" :block "0x…")` form per line.

Source: the raw archive chunk starting at block 0, filled from `debug_getRawBlock`
on the mainnet reference node on 2026-11-21. No transaction, header or block was altered.

Checked while extracting (the script asserted each of these):

- the mainnet genesis state root (`0x83408974…c7f6`, `clempo.chainspec:genesis-state-root`)
  is the state root of every block before 3211, and every block between two fixture blocks
  has the state root of the earlier one: blocks with only the subblocks metadata system
  transaction change no state, so a genesis state database is the state of each fixture
  block's parent once the earlier fixture blocks are applied;
- each fixture block holds exactly one user transaction followed by the reference's
  subblocks metadata system transaction;
- every header of blocks 1..3874 has beneficiary `0x0`.

The tests rebuild each block from a pool holding its user transaction and compare gas used,
state root, logs bloom, and the recorded transactions and receipts roots once the recorded
system transaction (and its receipt: status 1, no gas, no logs) is appended; the reference
head's builder emits no system transaction (tempo #7449).
