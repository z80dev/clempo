# Deviations from the reference

Numbered, never renumbered.

1. **TLS through curl.** clempo has no TLS stack. `clempo.http` speaks plain HTTP/1.1 itself (pooled keep-alive sockets) and runs `curl` for `https://` URLs. Upstreams on localhost (the private reference node) take the native path.

2. **Simplex voter without a journal.** `clempo.simplex` keeps votes and certificates in memory only. commonware's voter journals every local vote, certificate and certification verdict before publishing it and replays them on restart, so a restarted validator never signs a conflicting vote; a restarted clempo voter starts its epoch afresh. It also retries missing-certificate requests with each nullify rebroadcast instead of the resolver's fetch timers, and does not block peers (equivocators, invalid signatures) because it has no network of its own.
