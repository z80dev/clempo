`mainnet_41788340.json` is copied from goempo's `internal/trie/testdata/blocks/`.
It records `debug_getRawBlock`, `eth_getBlockByNumber` (transactions and
receipts roots), and `eth_getBlockReceipts` (type, status, cumulative gas used,
and logs) from `https://rpc.tempo.xyz` on 2026-11-17.

`trietest.json`, `trieanyorder.json`, `trietest_secureTrie.json`,
`trieanyorder_secureTrie.json`, and `hex_encoded_securetrie_test.json` are
copied from goempo's `internal/trie/testdata/`, originally from the
[`ethereum/tests` TrieTests](https://github.com/ethereum/tests/tree/develop/TrieTests)
develop branch, fetched 2026-11-17.

`mainnet-pathusd-proof.json` pairs `eth_getBlockByNumber` (number, hash,
stateRoot) with `eth_getProof` for pathUSD and storage slots 0x0, 0x1, and
0x100 at that block on the local Tempo 1.19.0-dev mainnet reference node.

`mainnet-dirty-sets.json` is a normalized measurement from the private Rust
Tempo mainnet archive (`http://127.0.0.1:8545`), not state data copied into the
benchmark. `tempo db --datadir DIR stats --skip-consistency-checks` opened the
local reference datadir read-only near block 50,063,775; the `HashedAccounts`
and `HashedStorages` entry counts set the synthetic database size. The three
workloads record `debug_traceBlockByNumber` using `prestateTracer` and
`tracerConfig: {"diffMode":true}`:

- `tip`: every other block in [50,063,519, 50,063,775), 128 reports.
- `busyUniform`: 64 evenly spaced blocks in [9,890,000, 9,909,999].
- `busyHeaviest`: the 16 blocks with the largest header gas-used in that
  archived range; an intentionally biased stress sample, not prevalence data.

Each report is `[height, unique dirty accounts, descending nonzero
dirty-slot counts by account]`; the stress reports append header gas-used.
Address and slot cardinalities are unions over both `pre` and `post` in
every transaction's diff. The synthetic benchmark does not assume the
recorded keys or values are known and instead rotates owner and slot keys.
