# Deviations from the reference

Numbered, never renumbered.

1. **TLS through curl.** clempo has no TLS stack. `clempo.http` speaks plain HTTP/1.1 itself (pooled keep-alive sockets) and runs `curl` for `https://` URLs. Upstreams on localhost (the private reference node) take the native path.

2. **Simplex voter without a journal.** Resolved: `clempo.simplex` journals votes, certificates and certification verdicts before publishing and replays them on restart, as commonware's voter does (the journal is a protocol; the validator supplies the file backend). Still different: missing-certificate requests are retried with each nullify rebroadcast instead of the resolver's fetch timers, and the voter blocks no peers itself (it reports :invalid resolver responses through `voter-deliver`; the network layer blocks).

3. **EVM memory capped at 4 GiB.** `clempo.evm` treats any memory access whose end offset exceeds 2^32 bytes as out of gas (`:memory-out-of-gas`) instead of pricing the expansion. Below the cap the quadratic memory cost is charged exactly. Reaching the cap would take over 3.5·10^13 gas, far beyond any block gas limit, so consensus results are unchanged, but a tracer could in principle report a different failure for a transaction with an absurd gas limit. Code: `+max-memory+` and `mem-end` in `src/evm/frame.lisp`.

4. **No TIP-1016 state-gas split.** clempo meters the unsplit regular-gas schedule rather than maintaining separate regular/state gas pools. This matches all currently configured Tempo networks: the Rust reference leaves `amsterdam_eip8037_enabled` disabled in `crates/hardfork/src/gas_params.rs`. Enabling that flag in a future network requires implementing the split before replaying it.

5. **Consensus RPC from a certificate archive.** `consensus_getFinalization` and `consensus_getLatest` (`src/rpc/consensus.lisp`) answer from the finalization certificates `clempo-dev certify` has fetched and verified into `~/cache/clempo/finalizations/<chain>/`, not from a marshal that follows consensus. A height whose certificate is not archived gets the reference's 503 "certificate" answer although the reference would serve it, and "latest" is the highest archived height at or below the served head. `consensus_subscribe` is not served.

6. **eth_fillTransaction without a pool or simulated account signatures.** clempo keeps no transaction pool, so the filled nonce is the head state's (account nonce or 2D lane) with no pending transactions added. Requests carrying `configurableAccountSimulation` or `keyAuthorizationSimulation` fail with an internal error instead of being signed with the simulated witness signature (`src/rpc/fill.lisp`).
