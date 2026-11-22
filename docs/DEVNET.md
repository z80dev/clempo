# Isolated validator devnet

`scripts/devnet/devnet.sh` provisions four local validator identities and manages
Rust and clempo processes. This is a development harness, not a node dependency.
Requires Python 3.9+, Cargo, a local Tempo source checkout, and built `tempo` and
`tempo-xtask` binaries. No Go implementation is read or used.

## Provision and run

Choose a new directory outside the repository. Provision refuses existing roots;
there is no force/reset/delete command. Existing datadirs are always preserved.

```sh
NET="$HOME/cache/clempo/devnet-t13"
D=scripts/devnet/devnet.sh
$D --root "$NET" provision --tempo "$HOME/dev/tempo/target/release/tempo"
$D --root "$NET" start
$D --root "$NET" status
$D --root "$NET" capture --seconds 120 --report "$NET/baseline.jsonl"
$D --root "$NET" stop
```

`--tempo-source` defaults to `~/dev/tempo`; `--xtask` defaults to its standard
`~/dev/tempo/target/debug/tempo-xtask` build location. Explicit overrides support
other checkouts/builds. Provisioning does not build either node binary. It builds
only a small standalone reference key-export helper under the devnet root, using
`tempo-consensus-config` from the selected checkout. Cargo runs offline against its
existing registry/cache (populate that by building Tempo beforehand), with a copy
of Tempo's Cargo.lock preserving pinned/yanked dependencies. It is not linked
into clempo and never modifies the Tempo source.
The helper writes secrets directly to mode-0600 files, never stdout or logs.
All generated files are private under umask 077. These deterministic keys and the
fixed xtask localnet passphrase must **never** be used on a shared network.

Defaults: four validators, seed 42, epoch length 60, `--hardfork t13` (all later
forks disabled by xtask), five funded accounts, initial DKG outcome in genesis.
The generator must include the EIP-2935 history contract; provisioning rejects a
missing allocation rather than silently patching the genesis. `--seed`,
`--epoch-length`, `--hardfork`, and `--base-port` are provision options. Key material
and genesis are deterministic; encrypted key ciphertext is not required to be.

| Slot | Consensus TCP | Execution TCP | Consensus metrics | Auth RPC | HTTP RPC |
|---|---:|---:|---:|---:|---:|
| 0 | 21000 | 21001 | 21002 | 21003 | 21004 |
| 1 | 21100 | 21101 | 21102 | 21103 | 21104 |
| 2 | 21200 | 21201 | 21202 | 21203 | 21204 |
| 3 | 21300 | 21301 | 21302 | 21303 | 21304 |

All listeners bind 127.0.0.1; discovery and IPC are disabled. Only explicit local
trusted enodes are supplied. Defaults never touch the unrelated mainnet
30311/8545 node. Alternate port groups must be >=20000, fit within 65535, and
exclude 30311. Startup checks all selected TCP ports before spawning anything.

`start --slots 0,1,2` and `stop --slots 1` select slots. `start --clempo 2,3`
runs `bin/clempo validator` for those selected slots, Rust for the rest.
`--clempo` is intersected with `--slots`. Readiness waits for each Rust RPC and
each clempo signing-enabled epoch event, not quorum; `--ready started` waits only
for clempo's `started` event (a restarted validator that is still catching up, or
one that joins at the next epoch). Readiness/shutdown default
to 60 seconds and accept `--timeout`. Provisioning/helper build defaults to 1800
seconds. Rust uses `--consensus.use-local-defaults` for short local ceremonies.
Failed startup terminates only children launched by that invocation. Stop sends SIGTERM
only to recorded PIDs whose process start-time and complete command still match;
no process-name matching, process-group kill, SIGKILL, or datadir deletion occurs.
PID reuse fails closed. `status` reports those ownership checks, not consensus
health. Logs append to `rust-N.log` or `clempo-N.log`; provisioning and helper
build logs are separate. Clempo datadirs are `ROOT/clempo-N`, separate from Rust
datadirs; starting a clempo slot again resumes from its datadir. Mutating commands use a
nonblocking root-local operation lock. Launch attempts (including probes) are
persisted in `launch-history.json`, which is never cleared by stop.

## Manifest contract

`manifest.json` version 1 has `genesis` (absolute path), `epoch_length`, `hardfork`,
`seed`, `tempo_binary`, and a `validators` array of four entries. Each entry has:

- `slot`: 0..3, matching the order of xtask's supplied addresses.
- `public_key`: unprefixed Ed25519 public-key hex.
- `signing_key`: encrypted xtask key path; `signing_seed`: **raw 32-byte** seed path
  (`network/127.0.0.1:PORT/signing.seed`); `signing_share`: xtask share file path.
- `datadir`, `consensus_address`, `execution_port`, `metrics_url`, `authrpc_port`,
  `rpc_url`, `enode_key`, `enode_identity`.

There are no private keys inline in the JSON. Fee recipients are not overridden;
consumers resolve them from genesis ValidatorConfigV2 by public key. The helper
uses `SigningKey::read_from_file_encrypted` with xtask's fixed localnet passphrase
and `to_writer_unencrypted`; Python converts its temporary hex representation to
raw seed bytes in place. It does not implement age or duplicate key derivation.

## Independent finalized/DKG capture

Normal `capture` is read-only and can run alongside launch/stop/probe commands.
It polls **finalized**, never `latest`; at every height from `--from-height` (default 0)
to the minimum finalized head it fetches blocks from every selected Rust validator
and compares hashes. Every block record includes each slot's hash and extra data.
At heights `(height + 1) % epoch_length == 0` it requires nonempty boundary DKG
outcome bytes. Rust DKG evidence comes from terminal success/failure log messages,
because its metrics endpoint need not expose the DKG actor counters. Each
`dkg_log` row records the counts and Rust finalized height. Nonzero failures,
diverging hashes, missing blocks, RPC errors, or a stall longer than `--stall`
(default 30 seconds) fail. Success requires at least one compared boundary and
a successful ceremony log on every selected Rust validator. Reports are
exclusive-created JSONL and flushed incrementally; only success writes a summary.

With `--clempo 2,3`, capture reads those slots' JSON events rather than trying
their nonexistent RPC endpoints. At least one selected Rust validator is
mandatory. Every clempo finalization reached by the common Rust finalized head
must match its independently fetched hash. Each clempo must also have an own
`proposed` hash finalized by Rust, with its public key as reported proposer.
DKG evidence requires a new boundary outcome, clempo's successful reshare/share,
its key in the dealer list, signing in the new epoch, and a matching finalized
block after that boundary. Each selected clempo must keep advancing and remain
the same live owned process. Mixed capture requires `--from-height 0`.
Partial concurrent log lines are deferred until complete.

This correlates node-reported participation with independent Rust finalization;
it does not independently decode or cryptographically verify DKG outcomes.
Only the fresh-network quorum mode below establishes that clempo votes were
necessary. Historical baseline capture is not proof that every historical
ceremony succeeded.
Each network request has a three-second timeout; the duration bounds sampling,
with at most one in-flight iteration's bounded requests beyond that duration.

## Side-by-side clempo probe

Reserve a slot so two processes never use the same consensus identity:

```sh
$D --root "$NET" start --slots 1,2,3
$D --root "$NET" probe --slot 0 --seconds 20
# Independently, in another terminal:
$D --root "$NET" capture --slots 1,2,3 --seconds 120 --report "$NET/side-by-side.jsonl"
```

The probe runs the real repository command:
`bin/clempo-dev devnet-probe -manifest PATH -slot N -seconds 20`.
It refuses a slot currently owned by a running validator. A probe is not a
validator and three Rust signers can finalize without it.

## Mixed validators and quorum

Use a fresh T13 root for every mixed run. A clempo slot runs

```sh
bin/clempo validator --chain GENESIS --datadir ROOT/clempo-N \
  --consensus.signing-key KEY --consensus.secret ROOT/localnet.secret \
  --consensus.signing-share SHARE --consensus.listen-address 127.0.0.1:PORT \
  --consensus.use-local-defaults --metrics 127.0.0.1:PORT+2 \
  --http --http.addr 127.0.0.1 --http.port PORT+4
```

with the slot's identity, share and ports (the Rust node's flag names). It executes
proposals on speculative branches, persists finalized blocks to a synced block log and its
votes and DKG state to journals, and resumes from the datadir after a stop or crash (the
journal keeps it from signing against its own earlier votes). Peers and the next DKG's
players come from `ValidatorConfigV2` at the parent state. Its JSON-RPC feeds the
transaction pool its proposals draw from; `/metrics` serves `clempo_validator_*`
(head, epoch, member, peers, proposed, committed, DKG dealt and acks).
`clempo_validatorStatus` on the JSON-RPC port returns the same view as one object
(`head`, `epoch`, `member`, `peers`, `proposed`, `committed`, `epochs` entered,
DKG `dealt` and `acks`), refreshed with the metrics once a second, or null before
the first refresh.

```sh
MIX="$HOME/cache/clempo/devnet-mixed"
$D --root "$MIX" provision --tempo "$HOME/dev/tempo/target/release/tempo"
$D --root "$MIX" start --clempo 2,3
$D --root "$MIX" capture --clempo 2,3 --seconds 180 --report "$MIX/mixed.jsonl"
$D --root "$MIX" stop
```

### Restarts, catch-up, joining and transactions

```sh
$D --root "$MIX" stop --slots 3                       # a restart mid-run …
$D --root "$MIX" start --slots 3 --clempo 3 --ready started
$D --root "$MIX" send --clempo 2,3 --seconds 120      # no-op calls into clempo's pools (cast)
$D --root "$MIX" join                                 # a fifth, clempo, validator
$D --root "$MIX" audit --clempo 2,3 --joiner --report "$MIX/audit.jsonl"
$D --root "$MIX" stop --joiner
```

`join` generates a key with `clempo keys generate`, registers it with `clempo keys
add-validator --send` signed by the localnet registry owner (the first funded account;
loopback devnets only) through a Rust slot's RPC, and starts `bin/clempo validator`
without a share on ports base+400…404; it becomes a player at the next DKG. `send`
submits no-op calls from funded accounts 1 and 2 through clempo's JSON-RPC.
`audit` compares every node's finalized block hash (clempo nodes through their own
JSON-RPC) with Rust slot's at every height up to the lowest finalized head, then splits
each clempo log into process runs and reports for each: the head it started at, the
replayed range, the largest gap one `behind` event reported (a catch-up target is at most
the current epoch's last block), its last `caught-up` head, the epochs it entered
holding a share, its DKG outcomes (player counts), and its proposals that are in the
Rust chain, with transaction counts and the first such height.

For a necessary-vote proof, start exactly three identities from a **fresh**
provisioning, leaving the fourth identity unused from genesis:

```sh
QUORUM="$HOME/cache/clempo/devnet-quorum"
$D --root "$QUORUM" provision --tempo "$HOME/dev/tempo/target/release/tempo"
CLEMPO_BUILD=release $D --root "$QUORUM" start --slots 0,2,3 --clempo 2,3
$D --root "$QUORUM" capture --slots 0,2,3 --clempo 2,3 --quorum \
  --seconds 180 --report "$QUORUM/quorum.jsonl"
$D --root "$QUORUM" stop
```

With a 3-of-4 threshold and only one Rust signer, finalization needs both clempo
signers. `--quorum` requires exactly the selected three owned live processes,
their expected implementations and unchanged identities, no launch-history
entry for the excluded slot (even a probe), no excluded datadir, and an unused
excluded consensus port. Older roots lacking launch history are rejected.
Capture holds the operation lock in this mode so harness start/stop/probe cannot
change the signer set during measurement. Do not run unmanaged processes using
these keys: process history cannot attest to activity outside this harness.

The summary contract includes `clempo_slots` (array), `quorum` (boolean),
`compared`, `boundary_outcomes`, `clempo_proposals` (per-slot positive counts),
and `clempo_reshare_epochs` (per-slot counts of reshares with matched
post-boundary advancement). `clempo` includes finalized counts and epoch lists;
`quorum_active_slots` / `quorum_excluded_slots` describe the necessary-vote
window. A failed requirement produces no summary and exits nonzero. Baseline
summaries have empty clempo counts and `quorum: false` and cannot score mixed R6.

For the one-Clempo necessary-vote report, use a separate fresh root and replace
`--clempo 2,3` with `--clempo 3` in the three-slot run. Score the two reports with:

```sh
bin/clempo-dev score -rung R6-devnet -json -one SINGLE.jsonl -two DOUBLE.jsonl
```

## Reference provenance and acceptance

The harness follows the local Rust sources `xtask/src/generate_localnet.rs`,
`xtask/src/genesis_args.rs`, `crates/chainspec/src/cli.rs`,
`crates/consensus-config/src/lib.rs`, and the consensus DKG manager. These define
key encryption, fork CLI spelling, history allocation, and boundary/metric
semantics. goempo's repository, `docs/DEVNET.md` and `HOW-IT-WAS-BUILT.md`
provide historical acceptance guidance only; none of their Go code or reported
results are treated as clempo results.

Baseline and probe reports establish reference behavior and wire interoperability,
not mixed-validator acceptance. Use independent quorum reports for the latter.

## Moderato readiness (read-only)

```sh
CLEMPO_BUILD=release bin/clempo-dev moderato-readiness -last 3000 -timeout 8
```

The command verifies recent Moderato certificates under the published testnet
network identity, reads the live ValidatorConfigV2 owner and simulates
`addValidator` with owner as the `eth_call` sender. It uses an ephemeral
Ed25519 key and documentation-only registration endpoints, and does not
submit a transaction or retain a signing key. The reported validator index
is the simulation result, not a registration.

It also opens one short Commonware transport attempt to a public consensus
ingress advertised by the live Moderato registry, using only the unregistered
ephemeral identity. A reset or timeout does not prove why admission failed.
The report does not claim committee membership, signing, or execution sync.
