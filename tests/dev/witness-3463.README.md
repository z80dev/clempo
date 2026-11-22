`witness-3463.json`: the reference's `debug_executionWitness` answer (the `result` object) for
mainnet block 3,463 (`0xd87`), recorded on 2026-11-21 over HTTP from a Tempo `568b4fb425`
mainnet archive node. Its one header is block 3,462, whose state root is
`0x7a91f968a770f34f613d35abc602fad42d51ec26afc7040155b34f7d06a0c70c`
(`eth_getBlockByNumber`). `rpcdiff-tests.lisp` checks that the witness proves every key it
lists under that root, and that it does not once the node at the root is removed or the root is another.
