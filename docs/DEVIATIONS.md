# Deviations from the reference

Numbered, never renumbered.

1. **TLS through curl.** clempo has no TLS stack. `clempo.http` speaks plain HTTP/1.1 itself (pooled keep-alive sockets) and runs `curl` for `https://` URLs. Upstreams on localhost (the private reference node) take the native path.

2. **Simplex voter without a journal.** Resolved: `clempo.simplex` journals votes, certificates and certification verdicts before publishing and replays them on restart, as commonware's voter does (the journal is a protocol; the validator supplies the file backend). Still different: missing-certificate requests are retried with each nullify rebroadcast instead of the resolver's fetch timers, and the voter blocks no peers itself (it reports :invalid resolver responses through `voter-deliver`; the network layer blocks).
