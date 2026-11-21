# Deviations from the reference

Numbered, never renumbered.

1. **TLS through curl.** clempo has no TLS stack. `clempo.http` speaks plain HTTP/1.1 itself (pooled keep-alive sockets) and runs `curl` for `https://` URLs. Upstreams on localhost (the private reference node) take the native path.
