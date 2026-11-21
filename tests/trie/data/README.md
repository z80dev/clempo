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
