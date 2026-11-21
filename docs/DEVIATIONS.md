# Deviations from the reference

Numbered, never renumbered.

1. **TLS through curl.** Resolved: clempo has its own TLS 1.3 client (`src/net/tls.lisp`: X25519, or P-256 after a HelloRetryRequest; AES-128/256-GCM and ChaCha20-Poly1305; RSA-PSS, ECDSA and Ed25519 CertificateVerify) and validates server chains itself (`src/crypto/pki/x509.lisp`) against the system roots: `CLEMPO_CA_FILE` if set, else macOS's `SystemRootCertificates.keychain` (exported once per process with `/usr/bin/security`), else the usual Linux CA bundles. `clempo.http` and the `wss://` client in `rpcdiff` use it; curl is not used. Still different from a full TLS stack: no TLS 1.2 (every server clempo uses speaks 1.3), no session resumption or 0-RTT, no client certificates (an empty Certificate answers a CertificateRequest), no revocation checking, certificate policies are not processed, and a critical name constraint on a form other than DNS names rejects the chain.

2. **Simplex voter without a journal.** Resolved: `clempo.simplex` journals votes, certificates and certification verdicts before publishing and replays them on restart, as commonware's voter does (the journal is a protocol; the validator supplies the file backend). Still different: missing-certificate requests are retried with each nullify rebroadcast instead of the resolver's fetch timers, and the voter blocks no peers itself (it reports :invalid resolver responses through `voter-deliver`; the network layer blocks).

3. **EVM memory capped at 4 GiB.** `clempo.evm` treats any memory access whose end offset exceeds 2^32 bytes as out of gas (`:memory-out-of-gas`) instead of pricing the expansion. Below the cap the quadratic memory cost is charged exactly. Reaching the cap would take over 3.5·10^13 gas, far beyond any block gas limit, so consensus results are unchanged, but a tracer could in principle report a different failure for a transaction with an absurd gas limit. Code: `+max-memory+` and `mem-end` in `src/evm/frame.lisp`.

4. **No TIP-1016 state-gas split.** clempo meters the unsplit regular-gas schedule rather than maintaining separate regular/state gas pools. This matches all currently configured Tempo networks: the Rust reference leaves `amsterdam_eip8037_enabled` disabled in `crates/hardfork/src/gas_params.rs`. Enabling that flag in a future network requires implementing the split before replaying it.

5. **Consensus RPC from a certificate archive.** `consensus_getFinalization` and `consensus_getLatest` (`src/rpc/consensus.lisp`) answer from verified finalizations, not a marshal that follows consensus. A height whose certificate is not archived gets the reference's 503 "certificate" answer although the reference may serve it, and "latest" is the highest archived height at or below the served head. `consensus_subscribe` and `consensus_unsubscribe` are available over WebSocket.

6. **eth_fillTransaction and simulated account signatures.** Resolved: configurable-account and key-authorization simulation use the reference's simulated witness signatures (`src/rpc/witness.lisp`). The follower keeps a local pool: `eth_sendRawTransaction` validates and queues transactions before forwarding, and `admin_clearTxpool` clears it. Archive-only backends without a pool do not queue submissions.

7. **Invalid-opcode halt names after earlier calls on one overlay.** Resolved: RPC refines invalid-opcode errors to the reference's halt names (`InvalidFEOpcode`, `NotActivated`, `OpcodeNotFound`). For a later call in `eth_callMany`, RPC rebuilds the overlay by rerunning the earlier calls before tracing the halt.

8. **T14/T15 execution coverage.** T14 transaction execution includes 0x05 configurable-account signatures, account configuration commitments, retired root keys, upgrade authorization and the 0xef shim opcode. T14 execution is not established by the mainnet replay evidence. The block-level T14 proposer fee-recipient check and T15 boundary DKG-outcome check are not implemented.

9. **Recent-validator fee-token cache after startup.** The follower seeds the cache from its anchor beneficiary and learns the remaining recent validators as heads arrive, rather than preloading the entire window.

10. **ABI loose decoding of zero-size array elements.** clempo charges 16 bytes per element for its list representation; alloy charges one byte per zero-size token. With the default 1 GiB allocation limit, clempo accepts no more than 2^26 such elements.

11. **Very long JSON integers.** Integer tokens with more than 1,000 digits are parsed as double floats, not arbitrary-precision integers. Tokens beyond the finite double range yield signed infinity.

12. **No devp2p peer manager behind the admin peer methods.** clempo's node serves devp2p peers on `--p2p-listen` and syncs from one rotating outbound connection, but keeps no runtime peer set to add to or remove from. `admin_addPeer`, `admin_addTrustedPeer`, `admin_removePeer` and `admin_removeTrustedPeer` (`src/rpc/peers.lisp`) parse their parameter exactly as reth does (NodeRecord through the url crate's rules; AnyNode as an enode URL, a bare `enode://<id>` or an `enr:` record verified down to its v4 signature; every parse error recorded from the reference in corpus suite `admin`), then answer `true` as reth does without changing any connection, so `admin_peers` never shows the peer.
