# Supplemental RPC differential cases

`rpc-extra.jsonl` supplements the HTTP recorder's cache corpus. The consensus
cases were recorded read-only against `wss://rpc.tempo.xyz` on 2026-11-21.
`setup` opens that many consensus subscriptions on the same WebSocket before
the recorded request. `bindSubscription` substitutes the first live subscription
ID for the recorded ID. The event fixture compares the complete notification
shape, not the time-varying certificate or block contents. With an owned server,
`eventHeight` publishes a historical head using the local finalization archive;
with `-url`, replay waits for an actual event from that server. The recorded
event's block (0x2fc6d9a) has an empty body, and shape comparison tells an empty
array from a filled one, so `eventHeight` is 50015002: a certified archive block
whose body is also empty (the reference's `eth_getBlockTransactionCountByNumber`
answers 0 for it; 50015001 holds 2 transactions).
The lead recorded `admin_clearTxpool` on the private reth node: `[]`, omitted
params, and `[1]` each answered numeric `0`. This agrees with reth's
`crates/rpc/rpc/src/admin.rs` (`c82820f`): remove all local pool transactions
and return the count as a JSON number. Clempo's archive and follower backends
do not admit transactions, so their local pool is empty. The automated
recorder never calls this method on the reference. Differential replay and
served-method probes send it only to the in-process clempo server, never to
an external `-url`.
