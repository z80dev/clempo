# Supplemental RPC differential cases

`rpc-extra.jsonl` supplements the HTTP recorder's cache corpus. The consensus
cases were recorded read-only against `wss://rpc.tempo.xyz` on 2026-11-21.
`setup` opens that many consensus subscriptions on the same WebSocket before
the recorded request. `bindSubscription` substitutes the first live subscription
ID for the recorded ID. The event fixture compares the complete notification
shape, not the time-varying certificate or block contents. With an owned server,
`eventHeight` publishes a historical head using the local finalization archive;
with `-url`, replay waits for an actual event from that server.

`admin_clearTxpool` is derived from reth's `crates/rpc/rpc/src/admin.rs`
(`c82820f`): remove all local pool transactions and return their count as a JSON
number. Clempo's archive and follower RPC backends do not admit transactions,
so their local pool is empty and the expected count is zero. The recorder never
calls this method on the reference. Differential replay and served-method probes
send it only to the in-process clempo server, never to an external `-url`.
