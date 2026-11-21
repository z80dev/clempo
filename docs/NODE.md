# Following Tempo mainnet

An empty `--datadir` starts at genesis (`--chain mainnet` by default; use
`--chain moderato` for testnet). To start near the tip instead, pass a stopped,
unhealed `clempo-dev replay -datadir DIR` checkpoint. The node checks the
loaded checkpoint and log, rejects recorded heals or skips, and compares the
head hash and state root with the configured upstream before serving.

Set `STATE` to the datadir to open.

```sh
bin/clempo --upstream http://127.0.0.1:8545 \
  --datadir "$STATE" --rpc-port 18545
```

The follower uses the local raw-block archive and fetches missing blocks.
It checks canonical header commitments and compares gas, receipts root, bloom
and state root before publishing a block to JSON-RPC. RPC binds to loopback by
default. Certification is enabled unless `--no-certify` is passed; `--cert-rpc`
selects a certificate endpoint independently of `--upstream` (default: the
public Tempo endpoint for the selected chain).

The starting epoch is authenticated against already executed state or the
published network identity. When the anchor predates that identity, the
follower can check its ancestry against a certified descendant. Unless
`--certify-from` is supplied, the follower probes the certificate provider
for the first available height. Below that height, blocks are checked by
parent linkage and execution only. An invalid or
missing certificate cannot advance the head. Transport and certificate
failures are retried with capped backoff (bounded attempts for `--once` or
`--seconds`); execution failures stop the follower.

Use `--source devp2p` to fetch missing raw blocks over authenticated public
ETH connections instead of raw-block RPC. `--peers FILE` tries explicit
enodes (one per line, blank/comment lines ignored) before the official
bootnodes, without UDP discovery. Without the file, the source uses public
discovery. `--upstream` still supplies the canonical header commitments, and
certification still verifies finalization; peer status alone proves neither.
For a header/body-only probe, run
`bin/clempo-dev follow -source devp2p -peers FILE -last 10 -once -no-archive`.

`--once` exits at the initial target, and `--seconds N` exits after N seconds
of live following, starting when the initial target is reached. Both print a
JSON run summary. Without either, the node runs until SIGINT or SIGTERM,
stops its services in order, then flushes history and closes the store.
Build a standalone executable with `scripts/build-node.sh` (default output
`build/clempo`). `CLEMPO_HEAP` sets the heap size in MiB.

Large dirty trie roots hash independent subtrees on four workers by default.
Use `--root-workers 1` to select serial hashing. Small updates use the serial
path with cached hashes, regardless of worker count.

## Caches and services

The hash index, receipts, historical state and finalizations default to
subdirectories of the datadir. Override them with `--index-dir`,
`--receipts-dir`, `--history-dir` and `--finalization-dir`. The raw-block
archive remains separate (`--archive`). Receipts are persisted as blocks
execute. Block- or transaction-hash queries outside the most recent 256
blocks use the hash index built by `clempo-dev index`.

The datadir uses an exclusively locked (`LOCK`) disk-backed key/value store:
checksummed immutable segments hold accounts, slots, code and reference-counted
trie nodes. Memory caches are trimmed after flushes. Legacy format-1 checkpoint
datadirs are imported on open. The node syncs each block to stable storage before
commit returns. Recovery discards a torn final record; corruption preceding later
valid records is an error, not a reason to discard committed history.

`eth_sendRawTransaction` validates a transaction against an overlay of the
committed head, queues valid transactions and forwards them asynchronously
to the upstream. A refusal drops it locally; new heads revalidate the pool.
`--no-forward` keeps valid submissions local. `txpool_*`,
`eth_pendingTransactions` and `admin_clearTxpool` operate on this pool.

`clempo_status` reports follower progress and pool state. Set
`--metrics-address HOST:PORT` for Prometheus-format metrics and
`--p2p-listen HOST:PORT` to serve local blocks and receipts to devp2p peers.
The P2P identity is stored in `<datadir>/p2p.key`. Validator key operations
are listed by `clempo keys --help`.

## Local Lisp REPL

The REPL is off by default. Create a private authentication file and enable it
when launching the node:

```sh
(umask 077; openssl rand -hex 32 > "$HOME/.clempo-repl-token")
bin/clempo --datadir "$STATE" --rpc-port 18545 \
  --repl-port 4005 --repl-token-file "$HOME/.clempo-repl-token"
```

Connect with `nc 127.0.0.1 4005`, send the token on its own line, then enter
Common Lisp forms. For example, `(+ 20 22)` prints `42`. The loopback REPL
allows up to eight concurrent sessions, with a 5 s authentication timeout
and a 60 s idle timeout. EOF closes a session. To inspect the committed head:

```lisp
(let ((b clempo.node:*node-backend*))
  (clempo.rpc.chain:with-backend-read (b)
    (clempo.rpc.chain:backend-head b)))
```

The token grants arbitrary code execution inside the node. Keep it private, do
not expose the socket with a tunnel or port forward, and take the backend read
lock for state inspection while the follower is running.
