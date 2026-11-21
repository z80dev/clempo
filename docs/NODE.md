# Following Tempo mainnet

Use a **verified** `clempo-dev replay -datadir DIR` checkpoint. Stop the replay
process before opening its datadir in the node. The node rechecks the loaded
checkpoint and log, rejects recorded heals or skips, and compares the stored
head hash and state root against the configured upstream before serving.

```sh
CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo \
  --datadir "$HOME/cache/clempo/state" --rpc-port 18545 --certify
```

The node reads the local raw chunk archive before fetching missing blocks by
RPC. It checks each batch against the canonical upstream, verifies finalization
when `--certify` is enabled, and publishes a block to JSON-RPC only after its
execution outputs match and the state is committed. The RPC listener binds to
loopback by default. `--cert-rpc` can select a different certificate-enabled
endpoint (default: `https://rpc.tempo.xyz`); the private raw-block RPC might not
serve the `consensus` namespace. For certified startup, use a checkpoint within
the certificate provider's retention window. A missing newest certificate
pauses progress; a bad certificate or state root stops the node.

Use `--source devp2p` to fetch missing raw blocks over authenticated public
ETH connections instead of raw-block RPC. `--peers FILE` tries explicit
enodes (one per line, blank/comment lines ignored) before the official
bootnodes, without UDP discovery. Without the file, the source uses public
discovery. `--upstream` still supplies the canonical header commitments, and
`--certify` still checks finalization; peer status alone proves neither.
For a header/body-only probe, run
`bin/clempo-dev follow -source devp2p -peers FILE -last 10 -once -no-archive`.

`--once` exits at the initial target, and `--seconds N` exits after N seconds
of live following, starting when the initial target is reached. Both print a
JSON run summary. Without either, the node runs until interrupted. Build a
standalone SBCL executable with `scripts/build-node.sh` (default output:
`build/clempo`). `CLEMPO_HEAP` sets the heap size in MiB for the build or source
launcher.

Large dirty trie roots hash independent subtrees on four workers by default.
Use `--root-workers 1` to select serial hashing. Small updates use the serial
path with cached hashes, regardless of worker count.

## Local Lisp REPL

The REPL is off by default. Create a private authentication file and enable it
when launching the node:

```sh
(umask 077; openssl rand -hex 32 > "$HOME/.clempo-repl-token")
bin/clempo --datadir "$HOME/cache/clempo/state" --rpc-port 18545 --certify \
  --repl-port 4005 --repl-token-file "$HOME/.clempo-repl-token"
```

Connect with `nc 127.0.0.1 4005`, send the token on its own line, then enter
Common Lisp forms. For example, `(+ 20 22)` prints `42`. A single developer
connection is served at a time; EOF closes it. To inspect the committed head:

```lisp
(let ((b clempo.node:*node-backend*))
  (clempo.rpc.chain:with-backend-read (b)
    (clempo.rpc.chain:backend-head b)))
```

The token grants arbitrary code execution inside the node. Keep it private, do
not expose the socket with a tunnel or port forward, and take the backend read
lock for state inspection while the follower is running.
