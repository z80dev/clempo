# Frontier

These measurements are revision-specific. Paths under `logs/` describe local artifacts, not files in the checkout; replace them with paths to your own run directory. See [metric definitions](docs/METRICS.md) before comparing rungs or clients.

| Rung | Score | Commit | Date |
|---|---|---|---|
| R0 Gate | yes: `scripts/verify.sh` (cold strict compile of every layer, 825 tests passed, 0 failed, 0 skipped, incl. the precompile oracle; fixed-seed fuzzing of every target; layer and determinism check; spec/ drift check) | `37db9c6` + path/label cleanup | 2026-11-21 |
| R1 Decode | mainnet 640/640 (T0..T13), moderato 33/33; full archive 50,050,000/50,050,000 blocks (35,979,413 txs) re-encoded and checked against eth_getBlockByNumber, 0 failures | 1edd52f, 4e28f4d | 2026-11-21 |
| R2 Genesis | yes: matching state roots and block hashes on mainnet and Moderato | `ec20b6b` | 2026-11-21 |
| R3 Replay | **50,049,999** (the archive tip; goempo's frontier 49,399,999): every mainnet block executed from genesis matches gas used, receipts root, logs bloom and state root. The disk-backed Linux replay reached the same height with no heals or skips. | `6354034` (disk-backed), `d1d277e` (earlier replay), `0fad06f` | 2026-11-21 |
| R3e EVM | 61,903/61,903 (100%; Istanbul..Osaka, Osaka 19,517/19,517) | `750c66b` | 2026-11-21 |
| R3p Precompiles | 4,986/4,992 (goempo's frontier; every Tempo precompile T0..T14; the 6 differences are T14 keychain grants where clempo follows tempo c8b89f9, newer than the corpus: +2,600 gas root-retirement probe) | `fd2d4cf` | 2026-11-21 |
| R4 Follow | **Yes:** 88,831 mainnet blocks executed and certified through 50,138,830; 3,820.1 s independently sampled, peak lag 1 block, p95 1, no incorrect responses. The longest gap between valid samples was 5.0001 s (limit 20 s). [evidence](evidence/r4-d561820/) | `d561820` | 2026-11-21 |
| R5 RPC | **14,697/14,697 value-identical cases**, with no reference timeouts left in the corpus and no timeout category. 135/156 methods fully match, 26 of them only on error answers (19 where the reference only ever errors, 7 whose success would write to the reference); the 21 that aren't value-graded are listed below with reasons. 156/156 reference methods served. Shape 108/113 and node 13/15 are reported separately (head-following and node-local answers). | `eebfce9` + timeout-rule deletion | 2026-11-21 |
| R6 Validator | Sims: 4, 5 and 7 validators, three epochs each. Mixed devnet: one clempo signer necessary through two reshares, two clempo signers necessary through three. | `b680aaf`, `dbe14d1` | 2026-11-21 |
| R6a–R6e Validator on the Rust devnet | `bin/clempo validator` with Tempo's flags: a) clempo signers necessary for quorum (1 of 3: 260 heights, 4 reshares; 2 of 3: 480 heights, 7 reshares); b) restart mid-run resumes from its datadir and signs; c) restart ~2,100 blocks / 35 epochs behind catches up by marshal backfill in under 90 s and proposes again; d) a fifth, clempo, validator registered with `clempo keys add-validator --send` joins the next DKG (5 players) and signs; e) clempo proposals with transactions finalized by Rust. Every node's chain matches Rust at every height. | `179e3db` | 2026-11-21 |
| R7 Speed | Same Mac, whole-command wall seconds (clempo / goempo / Rust): Early **2.66–2.83 / 2.97–2.98 / 58.16–64.91**; Busy **21.08–21.31 / 24.33–24.74 / 32.57–33.09**. Recent read-ahead rerun, wall / CPU seconds (clempo vs goempo): cold proxy 1 **7.75 / 8.10 vs 10.77 / 8.97**, cold proxy 2 **7.42 / 8.27 vs 11.46 / 8.98**; warm 1 **6.52 / 7.95 vs 6.54 / 8.46**, warm 2 **6.55 / 7.96 vs 6.30 / 8.39**. Warm wall times split within run-to-run variation; clempo used less CPU in every pair. No Rust rerun under these cache conditions. [Recent methodology](docs/BENCH.md#recent-with-matched-parent-cache-preparation); [three-client sweep](docs/BENCH.md#matched-prepared-parent-comparison). | clempo `91ab4d2` + `-prepared` (Early/Busy), `c190f13` (Recent); goempo `92e996c`; Rust `568b4fb4258c` | 2026-11-21 |
| R7-RPC | clempo/Rust req/s at 1/16/64 clients, event-loop server, newphy pinned (taskset, load avg 0.2-3.9, medians of 5, all 2,400 answers identical): `eth_getBalance` **1.08 / 1.23 / 1.16** (longer run: **1.09 / 1.21 / 1.14**), `eth_call` **1.40 / 1.26 / 1.23**, `eth_getBlockByNumber` **1.51 / 1.84 / 2.05**, `eth_getLogs` **3.92 / 5.27 / 4.91**, `debug_traceTransaction` **1.79 / 2.66 / 2.63**, `eth_getProof` **4.58 / 4.86 / 4.77**. Every method ≥ 1.0 at every concurrency. Per request clempo now uses 1.56 µs user CPU against Rust's 2.07. [evidence](docs/BENCH.md#per-request-cost-of-a-constant-answer-eth_getbalance-5165cd2) | `5165cd2` | 2026-11-21 |

## Evidence

**Linux x86-64** (`498155e`, Arch Linux, AMD Ryzen 7 9700X): the cold strict
`scripts/verify.sh` gate passes with SBCL 2.6.8 built from the official source
with `--fancy`. The stalled-WebSocket test caps the server's send buffer to
exercise backpressure on both platforms; no production code changed. With `CLEMPO_BUILD=release`,
`bin/clempo-dev genesis` and `genesis -chain moderato` computed the state roots;
`bin/clempo-dev score -rung R2 -json` matched both roots and block hashes
against public RPC. Using `fixtures_develop.tar.gz` from execution-spec-tests
v5.4.0, `bin/clempo-dev statetest "$FIXTURES/state_tests/" -threads 4` passed
61,903/61,903 in 10.3 s; `bin/clempo-dev score -rung R3e -threads 4 -json`
confirmed the same score with the fixtures in the default cache.
With archive chunks 0–100 in `~/cache/clempo/mainnet/` and a fresh datadir,
`bin/clempo-dev replay -to 1000000 -datadir "$D" -result "$RESULT" -sender-threads 4`
matched gas used, receipts root, logs bloom and state root at every block
from 1 to 1,000,000, with no heals or skips: 9.12 s replay time (109,621
blocks/s), 9.44 s command wall time. The release microbenchmarks below
measured **417 MB/s** for Keccak-256 on 64 MiB (0.161 s wall) and **62.396 µs
CPU / 62.452 µs wall** per ecrecover with caller-owned output and scratch
([command](docs/BENCH.md#native-curve-kernels)). These use the portable crypto paths.

**Linux arm64** (`eebfce9`, Ubuntu 24.04, kernel 6.8, colima VM with 10
vCPUs on Apple Silicon; SBCL 2.6.8 built from the official source with
`--fancy`): the cold strict `scripts/verify.sh` gate passed (808 tests, 0
failed, 0 layer violations, 3 m 34 s) with no code change. The kernel reports
`sha2` and `sha3` in AT_HWCAP, so `clempo.io:cpu-feature-p` selects the VOPs:
after loading every layer, `clempo.keccak-arm64:**native**` and
`clempo.sha256-arm64:**native**` were both `T`, `keccak256("abc")` and
`sha256("abc")` gave the standard digests, and one 64 MiB pass ran at
828 MB/s (Keccak-256) and 2,485 MB/s (SHA-256) in a release build. The
secp256k1 and P-384 field words compile to their `#+arm64` VOPs
unconditionally. With `CLEMPO_BUILD=release`: `score -rung R1 -json` 673/673,
`score -rung R2 -json` yes for mainnet and Moderato, `statetest
"$FIXTURES/state_tests/" -threads 4` 61,903/61,903 in 10.7 s, and
`replay -to 1000000 -datadir "$D" -sender-threads 4` from a fresh datadir
matched every block 1–1,000,000 with no heals or skips (3.2 s replay,
310,845 blocks/s).

**Full disk-backed R3 replay** (`6354034`, Linux x86-64, SBCL 2.6.8
release): a fresh datadir replayed blocks 1–50,049,999 from genesis with
every root checked, no heals and no skips. It processed 35,979,413
transactions and 3,513,348,765,570 gas in 6,909.253 s (7,243.9 blocks/s,
508.5 Mgas/s). The final datadir occupied about 4.6 GiB. Linux `VmHWM`
was 15,787,332 KiB (about 15.1 GiB); this includes resident, file-backed
segment mappings and is not the Lisp heap, which was capped at 8 GiB.
With the mainnet archive available in the default cache:

```sh
CLEMPO_HEAP=8192 CLEMPO_BUILD=release bin/clempo-dev replay \
  -to 50049999 -datadir "$FRESH_DIR" -result "$REPORT" -sender-threads 8 \
  -cache-accounts 262144 -cache-slots 2097152 \
  -cache-trie-nodes 1048576 -cache-code-mb 128
```

**Store read-ahead** (`c190f13`): opening a datadir asks the kernel to
read segments sequentially on a background thread (`madvise(WILLNEED)`),
newest first, with a default target of one quarter of physical RAM,
rounded to whole segments. In [cache-conditioned Recent runs](docs/BENCH.md#recent-store-read-ahead),
whole-command cold wall time fell from 13.67–17.42 s before read-ahead
to 7.42–7.75 s with it; goempo took 10.43–11.46 s. Warm runs took
6.30–6.61 s across both clients. An earlier, separate lookup change
(`9a80d5b`) reduced Recent replay CPU from 8.99–9.24 s to 7.85–8.05 s
in paired runs.

T14/T15 reference devnet (2026-11-21): `scripts/devnet/devnet.sh --root NET provision
--tempo ~/dev/tempo/target/release/tempo --hardfork t15 --base-port 24000` (tempo
568b4fb4, chain 1337, every fork through T15 from genesis, epoch length 60), then
`bin/clempo-dev devnet-t14-send -rpc http://127.0.0.1:24004 -out V.json`: the Rust node
accepted and sealed all 7 transactions clempo built (upgradeAccount; an owner-quorum send
from the upgraded account; a sponsored 2-of-2 derived account's first use; a key-type-3
grant of that account as an access key; a V2 keychain send through it; isValidSignature on
the upgraded account's 0xef marker for its owner, eth_call `0x1626ba7e`, and for a
stranger, `0xffffffff`) and refused the retired root. `bin/clempo-dev devnet-replay` then
matched gas used, receipts root, logs bloom and state root for every block, 801/801 at
the last run, with the T14 beneficiary and T15 boundary-outcome checks on. The earlier
key-type-3 refusal ("admin-signed key authorization account mismatch") was a clempo
signing-hash bug: the reference hashes its re-encoding, where a secp256k1 parity nested in
the key authorization (or a Tempo authorization) reads 27/28; clempo hashed the received
octets. The shim run exposed an interpreter bug (an extra opcode that halts lost the gas
it charged, 4,808 here). `bin/clempo-dev devnet-rpc-diff -rpc URL -genesis
NET/network/genesis.json` replays the devnet in-process and sends 17 simulation
requests (eth_estimateGas, eth_call, eth_simulateV1 with configurableAccountSimulation
mock witnesses, keyId overrides, and keyAuthorization with keyAuthorizationSimulation) to
both nodes at the same block: 17/17 identical results or error code and message, after
mapping the commitment-mismatch error. R3p stays 4,986/4,992 (SignatureVerifier 318/318)
with the verifier on clempo.signatures' configurable-signature code.

Zero fee recipient (2026-11-21): on a fresh T15 devnet (`--base-port 25000`, `start
--clempo 3`), a0 (the V2 owner) set slot 3's fee recipient to the zero address at block 61.
Every block slot 3 proposed after that, 46 by head 237, has miner `0x0`, and the Rust
validators finalized all of them (finalized = head = 237 on all three Rust nodes). The
clempo builder now takes any recorded recipient, zero included, as lib.rs
maybe_override_fee_recipient does. Test: `zero-fee-recipient-overrides-suggested-beneficiary`.

R6 sim (b680aaf): `CLEMPO_BUILD=release bin/clempo-dev score -rung R6 -json` (or
`bin/clempo-dev sim -scenario r6-4`) runs src/sim/scenarios.lisp: clempo.simplex voters
(commonware 2026.10.1 voter, Random V1 elector) and one clempo.dkg manager per node, driven
as Tempo drives its DKG actor (every finalized block in order, the DKG channel over the
simulated network, dealer logs and boundary outcomes in extraData, Enter/Exit starting and
stopping each epoch's voter), on a seeded network; the produced chain goes through the same
certify-blocks used for mainnet, anchored at the genesis DKG outcome. Release build:
- r6-4: 4 validators, epochs of 24, latency 5-60 ms, 2% drops, node 3 down 1.5-4 s: 71/71
  blocks certified, 3/3 ceremonies concluded, group key unchanged, all 4 nodes at height 71
  (4,286 messages, 246 dropped; 6.7 s simulated, 4 s real). With epochs of 20 (cb8ae2e)
  epoch 0's second half had only two distinct leaders, so its reshare carried the previous
  output forward: 2/3 ceremonies, not the 3/3 recorded then.
- r6-5: 5 validators, epochs of 25, node 1 down 1-3 s, partition {0,1}|{2,3,4} 5-6.5 s:
  74/74 blocks certified (73 certificates, one block covered by its child's), 3/3 ceremonies,
  all 5 nodes at height 74 (7,737 messages, 395 dropped; 9.8 s simulated, 6 s real).
- r6-7: 7 validators, epochs of 35, node 6 crashed for the whole run, node 2 down 3-6 s:
  104/104 blocks certified, 3/3 ceremonies, the 6 live nodes at height 104 (19,798 messages,
  3,870 dropped; 15.2 s simulated, 11 s real).
verify.sh runs smoke-4 (2 epochs of 12, both reshares concluded).

R6 mixed Rust devnet (`dbe14d1` on main): separate loopback T13 networks,
four-member committees and a 3-of-4 threshold, with only slots `0,2,3`
launched from genesis. On each fresh root, provisioned with
`scripts/devnet/devnet.sh --root "$NET" provision --tempo
"$HOME/dev/tempo/target/release/tempo" --base-port "$PORT"` (31100 and 32100)
then ran:

```sh
D=scripts/devnet/devnet.sh
# Set C=3 for two Rust + one Clempo; C=2,3 for one Rust + two Clempo.
CLEMPO_BUILD=release $D --root "$NET" start --slots 0,2,3 --clempo "$C"
$D --root "$NET" capture --slots 0,2,3 --clempo "$C" --quorum \
  --seconds 115 --stall 20 --report "$NET/mixed-proof.jsonl"
$D --root "$NET" stop --slots 0,2,3
```

With two Rust signers, 178 heights (0–177) matched. Clempo slot 3 authored
56 finalized proposals and kept signing after both reshares (boundaries
59 and 119). With one Rust signer, 209 heights (0–208) matched; Clempo
slots 2 and 3 authored 71 and 68 finalized proposals and kept signing
through all three reshares (59, 119 and 179). Every Clempo signer was
necessary for quorum and was selected as an on-chain dealer at each boundary.
`CLEMPO_BUILD=release bin/clempo-dev score -rung R6-devnet -json -one "$Q1"
-two "$Q2"` returned `2`; `Q1` and `Q2` are the `mixed-proof.jsonl` reports
from the one- and two-clempo-signer runs above. On the same
commit, `scripts/verify.sh` reported 470 passed, 0 failed, 2 skipped
(pool inline-grant checks), and zero layer violations. See `docs/DEVNET.md`.

Reference baseline: four Rust validators agreed on 146 heights (0–145),
including two 684-byte outcomes; each logged two successful ceremonies and
no failures. Clempo independently re-derived both DKG outcome byte strings
from dealer logs and replayed Rust raw blocks 0–130 with matching execution
commitments. The separate zero-vote probe authenticated three Rust peers and
received 156 votes, 156 certificates, 23 broadcast blocks and 69 DKG messages
in 15 seconds.

R6a–R6e (`179e3db`, main; `scripts/verify.sh` 796 passed on macOS and newphy Linux,
0 layer violations). Each root was provisioned with `scripts/devnet/devnet.sh --root
"$NET" provision --tempo ~/dev/tempo/target/release/tempo --base-port $PORT` (T13,
epochs of 60); clempo slots run `bin/clempo validator` (docs/DEVNET.md).

- **R6a quorum** (code identical to `179e3db` apart from the layer policy), roots
  `~/cache/clempo/devnet-vf-q1` (port 27000, `C=3`) and `devnet-vf-q2` (28000, `C=2,3`):
  `start --slots 0,2,3 --clempo $C`, `capture --slots 0,2,3 --clempo $C --quorum --seconds 150
  --stall 30 --report $NET/mixed-proof.jsonl`. One clempo signer: 260 heights matched, 4
  boundary outcomes, 82 clempo proposals finalized, 4 reshares. Two: 480 heights, 8
  boundaries, 163 and 167 proposals, 7 reshares each. Slot 1 never launched, so every
  clempo vote was needed.
- **R6b, R6d, R6e** on the final code, root `~/cache/clempo/devnet-vf-final` (port 29000),
  slots 0,1 Rust and 2,3 clempo: `start --clempo 2,3`; after 120 s `stop --slots 3`, 20 s
  later `start --slots 3 --clempo 3 --ready started`; `join`; `send --clempo 2,3 --seconds
  150` (210 transactions); `audit --clempo 2,3 --joiner --report $NET/audit.jsonl`.
  All five nodes matched Rust's block hash at every height 0–1214.
  b) slot 3 stopped at 268 and resumed from its datadir, signing epochs 4–20; 196 of
  its proposals finalized by Rust, the first at 315.
  d) `addValidator` (key `0x00a1ec56…`) executed in block 306; the joiner, started
  without a share, became a player in epoch 7's DKG (5 players), signed epochs 7–20 and
  authored 148 finalized blocks.
  e) Rust finalized 31 slot-2 proposals carrying 58 transactions and 25 slot-3
  proposals carrying 47.
- **R6c far behind**, root `~/cache/clempo/devnet-vf-main` (port 26000, the final engine
  code for the restart): slot 3 stopped at 1329 and restarted with Rust at 3446 (2,117
  blocks, 35 epochs). It backfilled finalized blocks over marshal, verifying each under its
  epoch's identity and resharing epoch by epoch (holding a share in epochs 22–67), reached
  the tip in under 90 s, and had 68 proposals finalized from 3662 on;
  `audit-1.jsonl` there matched all five nodes at every height 0–4019. The same root
  recorded an earlier R6b/R6d/R6e (registration in block 831, joiner signing from epoch 15).

R6f Moderato readiness (read-only): `CLEMPO_BUILD=release bin/clempo-dev
moderato-readiness -last 3000 -timeout 8` verified 3,000 finalized blocks
(45,212,047–45,215,046) under the published testnet identity (from epoch
1747). The live ValidatorConfigV2 owner-origin `eth_call` accepted the
disposable Ed25519 registration payload and returned simulated index 46.
TCP reached the public ingress `34.148.31.115:8000`, but the connection was
reset during SAKE authentication; an unregistered identity did not join and
no transaction was sent. `score -rung R2 -json` also matched the Moderato
genesis state root and block hash against live RPC. The JSON report is in
`logs/public-p2p/moderato-3000.log`; see `docs/DEVNET.md`.

R4 hour (historical run at `15c118d`): the harness cloned the unhealed R3
datadir at 50,049,999 and built a release node. It served local JSON-RPC
while following the archive RPC with `--certify --seconds 3840`,
using the public mainnet finalization endpoint. To rerun with the tracked harness:

```sh
CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev score -rung R4 \
  -datadir "$STOPPED_STATE" -run-dir "$NEW_RUN_DIR" -json
```

The historical node
executed and certified all 47,451 blocks through 50,097,450, checking gas used,
receipts root, logs bloom and state root at each block.

An independent 5 s RPC probe first matched the moving tip at 50,090,509.
Over the next 3,730.456 s, 744 of 746 attempts matched the canonical block
hash, state root and live EIP-2935 parent-hash storage slot; two attempts
returned errors, none returned an incorrect value, and the longest gap
between successful samples was 10.020 s. Observed lag was at most 4 blocks
(p95 3). The historical harness returned `no`: its extra end-sample cutoff
was 2 blocks, and its last sample saw 3. The committed harness uses the
criteria in [GOAL.md](GOAL.md), without an extra cutoff. It writes `r4.json`,
`node.json`, `node.log` and `rpc.jsonl` in the supplied run directory.

R4 rerun with the committed harness (`6763415`):

```sh
CLEMPO_R4_PORT=38545 CLEMPO_RPC=http://127.0.0.1:8545 \
  scripts/r4/run-hour.sh "$STOPPED_STATE" "$NEW_RUN_DIR"
```

From the same anchor at 50,049,999 the node executed and certified all
74,779 blocks through 50,124,778 (9,384 blocks during the node's 3,839 s
live window). The independent RPC window lasted 3,783.879 s: 750 of 757
attempts matched and none returned an incorrect value. Peak lag was 4
blocks, p95 was 1, and 446 samples had no lag. The seven failed probes
requested storage at a head the node had already advanced past; that
revision did not retain the earlier state. The longest gap between valid
samples was 20.019501 s against the 20 s limit, so the verdict was **no**.

R4 passed on Linux x86-64 (`d561820`, SBCL 2.6.8) after the datadir gained
state history for past-head RPC reads. The harness cloned a stopped R3
datadir with store head 50,049,999. With no pre-existing local archive chunks,
it fetched blocks from the Rust archive RPC and obtained certificates from
the public mainnet finalization endpoint:

```sh
CLEMPO_R4_PORT=38545 CLEMPO_RPC="$RUST_RPC" \
  CLEMPO_MAINNET_ARCHIVE="$EMPTY_ARCHIVE" \
  scripts/r4/run-hour.sh "$STOPPED_STATE" "$NEW_RUN_DIR"
```

The node executed and certified all 88,831 blocks through 50,138,830
(8,569 blocks during the node's 3,840 s live window). The independent
probe matched 765 of 767 attempts across 3,820.055 s and 7,111 upstream
blocks after catchup; none returned an incorrect value. The longest gap
between valid samples was 5.0001 s. Measured lag peaked at 1 block, was
0 for 487 samples, and ended at 0. The run's result, command, node log and probe log
are in [`evidence/r4-d561820/`](evidence/r4-d561820/).

R4 certificates (bba3dfe): `CLEMPO_RPC=http://127.0.0.1:8545 CLEMPO_BUILD=release bin/clempo-dev certify -from 49995000 -to 50015000 -threads 16`
verified all 20,001 consecutive mainnet finalization certificates across the epoch 2314/2315
boundary in 38 s (16 threads; one random linear combination per batch of 250, two pairings
each). The identity of epoch 2314 is anchored at the DKG outcome in boundary block 49982399
(= the published network identity 0x8627d337...); epoch 2315's is installed from block
50003999's outcome only after that block's own certificate is in the verified run. Blocks
come from the archive (hash, parent links, transactions root checked); certificates from
https://rpc.tempo.xyz consensus_getFinalization (cached in ~/cache/clempo/finalizations).
Fixture tests: votes and certificates signed/assembled with the Rust harness's shares are
byte-identical to Rust's golden vectors; goempo's recorded mainnet finalizations verify under
the outcomes of blocks 42098399/44085599/44107199 and across the 2041->2042 full-DKG boundary.

Public devp2p follower (`f9ab7d3`): `CLEMPO_BUILD=release bin/clempo-dev
follow -source devp2p -peers FILE -last 10 -batch 5 -once -no-archive -json`
verified 10 blocks through 50,069,812 (0 lag, 330 ms), using a working public
peer and trusted RPC headers. This checks body transport, not execution or R4.

Clempo release Busy: blocks 9,890,000–9,909,999 from the canonical parent
9,889,999, with a fresh persisted copy and no heals/skips in the range.
Every block's gas, receipts, bloom, and state root matched (659,246
transactions and 31,499,321,696 gas).

| Revision / path | Wall s | CPU s | Mgas/s | Peak RSS GB |
|---|---:|---:|---:|---:|
| `648e0a5` serial `bench` | 201.729 | 195.935 | 156.146 | 1.563 |
| `3d99e1b` persistent 8-participant `bench` | 127.429 | 145.884 | 247.191 | 1.612 |
| `217b0c1` guarded native recovery, 8 participants | 28.468 | 61.245 | 1106.487 | 1.999 |

See [commands](docs/BENCH.md). These are single observations under active
development workloads, not a paired comparison or an R7 score. An interim
per-block thread implementation (`1e291bd`, `replay`) took 88.218 s; it was
not timed under the same conditions as the persistent pool.
The separate CPU profile (`92ec1f3`, 23,706 samples including worker
threads) attributes **55.7%** inclusive to secp256k1 recovery, **5.5%** to
state-root update, **3.2%** to trie hashing, **0.5%** to block decoding and
**0.4%** to EVM bytecode execution. Inclusive costs overlap.

Native curves (`217b0c1`): ecrecover fell from **94.147 to 45.094 µs CPU**
and 288 KB to **zero bytes consed** with caller-owned output/scratch.
Direct address recovery takes 44.992 µs and allocates only its 32-byte result;
P256 verification fell from 307.102 to **152.765 µs CPU**, 648 KB to zero
with supplied scratch. The 25-µs recovery target is not attained. The separate
28.468-s Busy observation above is
`logs/busy/20261121-085152-217b0c1-1/replay.json`, complete through
9,909,999. Its improvement over 127.429 s includes other replay changes;
the paired kernel-only CPU comparison is below. See
[commands and allocation contracts](docs/BENCH.md#native-curve-kernels).

Busy replay-thread work (paired, alternating, root-checked on all 20,000 blocks,
`logs/busy/es/`). Original `e55a532` vs `d3aace8 + 217b0c1` (= pushed
`40521c1`), back to back: **58.14 s wall / 105.63 s CPU / 97.5 GB consed** ->
**24.52 / 24.73 s wall, 57.75 / 57.81 s CPU, 22.2 GB consed, GC 2.37 -> 1.25 s**
(an earlier, busier-host `e55a532` run took 79.18 s / 123.93 s CPU). Peak RSS
3.1 GB. Separately, Rust's staged import of the same range on this host took
36.00 s wall and 43.22 s CPU (`logs/bench/busy.json`; Execution
stage 15.89 s). The Rust run was not paired with these Clempo A/B runs.
Per step, each paired Clempo A/B on the previous step:

| Step | A wall s | B wall s |
|---|---:|---:|
| word-wise `octets->integer` (14% of the replay thread was `CONTRACT:AT` -> bignum shifts) | 79.18 | 49.35 |
| pipelined sender recovery (producer + 7 workers overlap execution) | 49.35 | 47.84 |
| cached mapping slots, digit-wise packed fields, consing-free gas checks | 47.84 | 36.51 |
| account/warm-slot memos, one-pass slot access, blooms once | 28.11 / 28.52 | 27.35 / 27.49 |
| metered storage as a structure | 27.24 / 27.56 | 26.60 / 26.88 |
| 2 GiB nursery (GC 1.78 -> 1.19 s) | 26.53 / 26.73 / 27.24 | 26.24 / 26.37 |
| memoized struct fields, last warm address | 26.41 / 26.47 | 25.45 / 25.68 |
| 0x76 unique ids on recovery threads | 25.77 / 25.70 | 24.92 / 24.74 |
| Native recovery `217b0c1` (CPU 79.4 -> 57.8 s) | 24.81 / 24.71 | 24.53 / 24.73 |

The first three rows are single runs taken while the host load was changing. Parallel
storage-trie hashing (`7075d08`) was reverted: paired runs with 1 vs 4 state-root
participants were equal (27.20/27.50/27.68 vs 27.17/27.88/27.70 s). Replay-thread
profile at that revision (`-profile-threads main`): TIP-20 transfer and fee
pre/post paths ~35%, metered SLOAD ~20% (bignum-keyed lookups ~15%), state root
~12%, expiring-nonce ring ~10%, receipts root and bloom ~6%, and the then
synchronous EVM JIT ~2%. The node no longer compiles on the execution thread.

R7 pipeline handoff and storage-trie build (`b81e3e5`, paired against `6c0f9d2`,
alternating, all blocks root-checked; `logs/busy/es/recent/` and `es/`):
**Recent** 36.16 / 35.98 s wall (37.27 / 37.34 s CPU) -> **32.68 / 29.91 s** (37.46 / 35.64 s
CPU), with two further `b81e3e5` runs at 30.84 / 29.66 s; **Busy** 24.83 / 24.62 s ->
**22.77 / 22.59 s** wall (CPU 57.9 / 57.7 -> 58.9 / 58.1 s). The block pipeline now signals
only a side that waits and wakes a full producer once per 32 blocks (64 ahead) instead of
broadcasting per block; a storage trie a checkpoint load left unbuilt is built top down from
its slots (`clempo.trie:trie-from-items`), its keys hashed and the root's subtrees built and
hashed on a 4-participant pool. Recent profile after (`es/recent/prof-17b8646.txt`): first-write
trie builds on the replay thread 23.8% -> 5.0% of samples (the rest runs on the pool), pipeline
`CONDITION-NOTIFY` 28.4% -> 6.2%; P-384 Nitro verification is now 37%. Three of seven `b81e3e5`
Recent runs had a late collection (GC 3.7–4.0 s, peak RSS 7.1 GB instead of 2.6 s / 5.6 GB) and
account for the spread. A 256-deep pipeline (refill 128) measured the same (29.87 / 29.91 s vs
30.84 / 29.66 s) and was not kept.

R7 Early and Recent (`15c118d`): the Early pair repeated from genesis after the
certified follower reached steady state, Rust then Clempo, with no overlapping
benchmarks. The historical Rust Early import (89.14 s wall / 41.67 s CPU,
`logs/bench/early.log`) is not used for the paired comparison.
For Recent, a copy of the 49,000,000 R3 checkpoint replayed all 409,999
intervening blocks to the exact parent 49,409,999 without heals or skips,
off the clock. The Recent timed replay used 1.74× Rust's historical CPU;
the CPU profile sampled Nitro P-384 verification (31.4% inclusive),
pipeline wakeups (28.4% in Darwin `__ulock_wake`), and lazy storage-trie
hydration (23.8% inclusive). See [the comparisons and call paths](docs/BENCH.md#results-on-the-shared-dev-host).

Busy historical execution cross-check, **not a root-checked import**:
`tempo re-execute --from 9890000 --to 9909999 --num-tasks 1 --blocks-per-chunk 20000`
on the detached archive used parent 9,889,999 and passed receipt/gas and
change-set checks for all 20,000 blocks (31,499,321,696 gas). `/usr/bin/time -l`
reported **49.37 s wall**, **20.08 s CPU**, **1.65 GB peak RSS**,
**405 blocks/s** and **638.03 Mgas/s** end to end (Rust's internal interval:
651.01 Mgas/s). The full staged Busy import requires a roughly 40M-block
unwind of the detached archive to provision its exact parent.

EVM bytecode compiled to native code (R7 evidence; SBCL 2.6.8 release, Apple
Silicon M3 Max unless noted). `src/evm/compile.lisp` turns a contract into Lisp
(blocks as TAGBODY tags, a symbolic stack in lexical variables, the DEFOP bodies
themselves, static gas charged once per segment, interpreter resume on short gas
or broken stack bounds) and `compile`s it; `compile-cache.lisp` shares the
functions by (code hash, fork). Differential: `bin/clempo-dev statetest -jit`
(every code of any size compiled at its first call) passed **61,903/61,903** at
`4c479c7` (50,193 compilations, 89,803 shared-cache uses, 46,179 LRU evictions,
no compilation failures, 561 s); `-interp` passed the same suite in 9.9 s. On
Linux x86-64 (newphy, release, 12 threads, 16 GiB heap) `-jit` also passed
**61,903/61,903** at `6dc2b56`, peak RSS 4.6 GB. `jit-latency` compiles all 9,480
distinct fixture codes without a warning. `evm-bench -n 10000000` at `6dc2b56`,
median of three, interpreter (`-interp`) to compiled (`-jit`), Mgas/s (the host
had a load average near 10 from other validation runs):

| Workload | Interpreter | Compiled | |
|---|---:|---:|---:|
| arith (small-word MUL/ADD/DIV) | 1,605 | 11,825 | 7.4x |
| bignum (256-bit MUL/MULMOD) | 508 | 1,862 | 3.7x |
| memory (MSTORE/MLOAD/KECCAK256) | 301 | 328 | 1.09x |
| storage (SSTORE/SLOAD) | 1,548 | 1,605 | 1.04x |

Compile latency (`jit-latency`, Osaka, all fixture codes, 252.9 s in total): mean
2.7 ms under 64 bytes, 4.9 ms to 256, 12.8 ms to 1 KiB, 98 ms to 4 KiB, 0.95 s
to 16 KiB, 7.1 s above (max 13.2 s); 34–150 µs/byte. `clempo.evm` loads with
compilation off. The node starts a compiler thread unless `--no-jit` is
supplied; dev tools can select `-jit-bg`. A code becomes eligible after 100,000
interpreted gas per byte of its own work, excluding callees. Execution never
waits for compilation. At most 16 jobs wait, code larger than 24 KiB is not
compiled, and the shared LRU cache is capped at 4,096 functions and 256 MiB.
That byte cap is real memory since `6dc2b56`: SBCL's `compile` kept each
region's whole Lisp source in its debug source (and, at debug 1, on the
function), about 6 times the native code, so the cache held roughly six times
what it counted (14.7 MB for 2.4 MB reported on 397 fixture codes). Regions now
compile with `store-source-form 0` and drop that form, and the count includes
the debug info: a release build retains 2.05 MB for 2.48 MB reported, a check
build 4.5 MB for 3.7 MB.

Check builds (`safety 1`) cannot run `statetest -jit` with 12 threads in a
16 GiB heap: it exhausted the heap after about 21 minutes on Linux, before these
changes as well (`4940311`). It is not a leak. With a full collection every
minute the live heap was in-flight compiler IR (`sb-c::equality-constraint`,
lvars, refs, and 1.5 GB of constraint-set bit vectors), reached from
`sb-c::*constraint-universe*` bound in the threads that were mid-compile. One
region held 133,119 constraints. The JIT cache stayed under 4,096 entries and
110 MB, and the analysis store under 700 entries. Safety 1 bounds and type
checks make constraint propagation superlinear in a region, and 12 such
compilations run at once. Release builds fit, and a node compiles on one thread
with code capped at 24 KiB.

Keccak-256 (SBCL 2.6.8 release, Apple Silicon): the ARM SHA3 permutation and
copy-free 32/64-byte paths reduced 64-byte hashes from **626 ns to 178 ns**
(1 million calls, about 3.5× faster) and 480 to 272 bytes consed per hash.
On 64 MiB, throughput rose from **256 to 799 MB/s** (262 to 84 ms), with no
measurable consing. The typed scalar permutation remains the fallback.

`keccak256-into` now initializes all lanes directly in registers for 32- and
64-byte slices and writes the digest to caller memory. A release run of one
million 64-byte calls without a context measured **142 ns CPU/call, zero bytes
consed**. The scalar fallback also measured zero consing for the same run.

```sh
CLEMPO_BUILD=release sbcl --noinform --non-interactive --load scripts/build.lisp \
  --eval '(asdf:load-system "clempo-crypto-keccak")' \
  --eval '(let ((small (clempo.base:make-octets 64)) (big (clempo.base:make-octets (* 64 1024 1024))))
            (time (dotimes (i 1000000) (clempo.keccak:keccak256 small)))
            (time (clempo.keccak:keccak256 big)))'
```

P256 verification: native four-limb multiply and signed 32-bit field
reduction reduced CPU time from 637 to **339 µs per signature** at 1,000
calls (same inputs as `tests/crypto/ec/benchmark.lisp`, SBCL 2.6.8 release),
and allocation from 1.51 MB to **648 KB** per signature.

R3e = 61,903/61,903 (750c66b): `bin/clempo-dev score -rung R3e -json` (or
`bin/clempo-dev statetest ~/cache/clempo/fixtures/fixtures/state_tests/`) runs every
execution-spec-tests v5.4.0 `fixtures_develop` state test for Istanbul..Osaka (pre-Istanbul
cases skipped, as goempo) on 12 threads in 33.5 s: Istanbul 608, Berlin 1,249, London 1,504,
Paris 1,564, Shanghai 1,745, Cancun 16,847, Prague 18,869, Osaka 19,517, all passing. Each
case checks validity (expectException), the full post alloc, the logs hash and the state
root (clempo.trie over the in-memory backend). EIP-7702 authorities are recovered with
clempo.secp256k1 (the fixtures' `signer` is ignored).

R3 = 3,210 (b74f4d8): `bin/clempo-dev replay -datadir logs/r3/dd -checkpoint 500000 -diagnose -result logs/r3/replay.json`
executes mainnet from the R2 genesis over the archive (contiguous to 50,049,999), state in a
clempo.store datadir. Blocks 1..3210 carry only the subblocks metadata system transaction;
all 3210 match gas used, receipts root, logs bloom and state root (22k blocks/s). Block 3211
(the first 0x76 transaction) stops in `block-mismatch` (execution: the EVM and Tempo
precompiles are not wired in yet). `-heal K` heals from the reference's prestate diff and
continues (tested on 3211: the healed root equals the header's).

R2: `CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev score -rung R2 -json`
compared both computed state roots and Tempo genesis block hashes against live
`eth_getBlockByNumber("0x0", false)` on mainnet and Moderato.

Merkle Patricia trie: 1,000,000 seeded random 32-byte keys with 32-byte values,
single-threaded SBCL 2.6.8 release build on Apple M3 Max. Insertions took 0.382 s
(2.61 million/s; 196.5 MB consed), the first root took 1.516 s wall time
(0.365 s GC; 808.8 MB consed), and recomputing after one value update took
18 µs CPU. Key generation was excluded. Reproduce with the command at the
top of `tests/trie/benchmark.lisp`.

R1: `bin/clempo-dev score -rung R1 -json` reports 673/673: mainnet 640/640
(40 blocks in each of 16 fork windows, T1/T1A combined), moderato 33/33.
`fetch-r1` records raw blocks, full RPC transactions and receipts in
`~/cache/clempo/r1/{mainnet,moderato}.jsonl`; each window uses 39 evenly spaced
heights plus the busiest distinct probe. Mainnet source was the private Rust
archive at `http://127.0.0.1:8545`; moderato used `https://rpc.moderato.tempo.xyz`.
Checks cover reconstructed block bytes, block/transaction hashes, transaction
roots, recovered senders, sponsorship, keychain IDs, key-authorization signers,
receipt roots and rebuilt per-receipt/block blooms. The sample contains 448
mainnet and 1,270 moderato transactions; additional checked-in RPC fixtures pin
each transaction/signature variant. The cold gate passed 70 tests, zero skips,
and zero layer violations. Scoring took 0.526 s mainnet and 1.784 s moderato.
The check-build codec benchmark (`time`, 100 decode/re-encode passes over the
640 mainnet raw blocks, excluding JSON/file I/O) processed 64,000 blocks in
0.320 s (200,000 blocks/s), consing 593,425,824 bytes, with 0.005 s GC.

R4 devp2p path: `CLEMPO_BUILD=release bin/clempo-dev p2p-sync -from 0 -to 9999`
connected to the local Rust Tempo node at `127.0.0.1:30311` and negotiated
eth/69. All 10,000 mainnet blocks matched `clempo.archive` exactly (0.239 s,
41,784 blocks/s). With `-version 68`, the same 10,000 blocks matched (0.244 s,
40,971 blocks/s). Both versions also served 30 receipts for blocks 9970..9999;
the first wire receipt was 265 bytes on eth/68 and 5 bytes on eth/69 (no bloom).
The cold gate passed 116 tests, including reference Snappy, RLPx, SAKE/CUPS and
Tempo Commonware lookup vectors. This demonstrates download, not live replay
or finalization; R4 remains unscored. Public validator interoperability was not
attempted because Tempo admits registered validator identities and addresses.

R4 public devp2p source: `bin/clempo-dev p2p-discover -protocol v4 -seconds 25`
and `-protocol v5 -seconds 24` proved 12 and 15 public UDP endpoints,
respectively, starting from Tempo's published mainnet bootnodes. Discovery
authenticates endpoints, not chain membership; RLPx and ETH Status separately
check peer identity, network, fork ID and genesis. With automatic v5-first
discovery, `CLEMPO_BUILD=release bin/clempo-dev p2p-follow-proof -upstream
http://127.0.0.1:8545 -seconds 90` authenticated eth/69 on `40.160.67.13:30303`
and matched all 167 finalized blocks (50,069,660–50,069,826) byte for byte.
The public peer supplied headers and bodies; RPC supplied only the finalized
heights and comparison bytes. A forced eth/68 session against
`109.94.98.205:30056` matched a further 16 finalized blocks (50,070,617–50,070,632)
byte for byte. The integrated `bin/clempo-dev follow -source devp2p -upstream
http://127.0.0.1:8545 -last 10 -batch 5 -once -no-archive -json` reached
height 50,070,139 with 10 validated blocks and no lag, using default discovery.
Logs are in `logs/public-p2p/`.

For a byte-by-byte public follow soak with operators' known public peers,
pass `-peers FILE` (one enode per line); the reference node's read-only
`admin_peers` can supply the enodes, but all RLPx block payloads come over
separate public TCP connections. The observed run used:

```sh
CLEMPO_BUILD=release bin/clempo-dev p2p-follow-proof \
  -upstream http://127.0.0.1:8545 -seconds 1800 \
  -peers reference-peer-enodes
```

All 3,350 consecutive finalized blocks (50,068,595–50,071,944) matched byte
for byte in 1,800 seconds over one public eth/69 session to
`109.94.98.205:30056` (1.86 blocks/s). All 60 progress samples were caught
up to the RPC finalized head. Log: `logs/public-p2p/configured-soak.log`.

Inbound devp2p: the archive-backed listener advertised `0..50,049,999`
and accepted the local Rust node's `admin_addPeer` connection as eth/69.
`tempo p2p header 3211` and `tempo p2p body <hash>` with only Clempo as a
trusted peer downloaded the expected header and its two-transaction body.
A separate client requested block 3211 over both eth/68 and eth/69. Header
and body matched the archive, and the two receipt envelopes matched the
Rust node's devp2p replies byte for byte for each version.

Hash/25519 crypto (SBCL 2.6.8 release, Apple Silicon, single thread): on
64 MiB, SHA-256 **2,684 MB/s** (25 ms, no consing), BLAKE3 **317 MB/s**
(212 ms, no consing), ChaCha20 **257 MB/s** (261 ms, one result buffer).
Ed25519 averaged **142 µs verify** and **62 µs sign** over 1,000 calls
each with a 32-byte message, including GC. SHA-256 uses a four-round ARM
SHA-256 VOP on Apple Silicon and a scalar SBCL core elsewhere; Ed25519
uses unboxed radix-2^51 field limbs and a fixed-base table.

```sh
CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --noinform --non-interactive \
  --load scripts/build.lisp --eval '(asdf:load-system "clempo-crypto-hash/tests")' \
  --eval '(assert (clempo.base:run-tests))' \
  --eval '(let ((a (clempo.base:make-octets (* 64 1024 1024))) (key (clempo.base:make-octets 32)) (nonce (clempo.base:make-octets 12)))
            (time (clempo.sha2:sha256 a)) (time (clempo.blake3:blake3 a))
            (time (clempo.chacha20poly1305:chacha20-xor key nonce a)))' \
  --eval '(let* ((seed (clempo.base:hex->octets "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
                 (msg (clempo.base:make-octets 32)) (pk (clempo.ed25519:ed25519-public-key seed))
                 (sig (clempo.ed25519:ed25519-sign seed msg)))
            (assert (clempo.ed25519:ed25519-verify pk msg sig))
            (time (dotimes (i 1000) (clempo.ed25519:ed25519-verify pk msg sig)))
            (time (dotimes (i 1000) (clempo.ed25519:ed25519-sign seed msg))))'
```

Commonware 2026.10.1 interoperability: DKG transcript forks/resumes use
keyed BLAKE3, and transcript noise uses the BLAKE3 XOF. Ed25519 decoding
accepts noncanonical point encodings and verification checks the cofactored
equation with canonical `S` (ZIP-215). CUPS uses RFC 8439 ChaCha20-Poly1305
with empty AAD and an implicit, per-key 96-bit little-endian nonce counter
starting at zero. The Lisp AEAD takes explicit nonces. The AES and 25519
secret-key paths have not been hardened against timing or cache side channels.

R1 codec optimization (2026-11-21): `defrlp` now generates bounded direct
decoders and single-buffer writers; block decoding avoids a duplicate RLP
tree and transfers newly allocated transaction envelopes without copying them
again. The same 64,000-block check-build benchmark now takes 0.228 s wall /
0.228035 s CPU versus 0.320 s / 0.320180 s: **1.40x faster**. Allocation falls
from 593,425,824 to 393,454,144 bytes (**33.7% less**, about 6.15 KB/block).
R1 remains 640/640 mainnet and 33/33 moderato. The shared body verifier also
checks ommer hashes and withdrawal presence/roots; sender-scoped transaction
identifiers hash the signing envelope plus the sender for pool replay checks.

The shared types layer also exposes payment classification (V1/V2/V3), parsed
key-authorization views, intrinsic signature gas and calldata-token counting,
call/CREATE rules, T1C signature-version checks, and stateless grant validation
through the T3/T5/T6 policy changes. These helpers do not replace execution's
state-dependent permission, balance or expiry checks. The types suite passes
34 tests, including recorded grants and signed fork-boundary cases:
`sbcl --dynamic-space-size 8192 --non-interactive --load scripts/build.lisp --eval '(clempo-build:test-all :dirs (list "types") :strict t)'`.

R4 upstream RPC path (partial, `e1fbffd`):
`CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev follow -last 1000 -seconds 20 -no-archive -json`
linked and checked 1,038 blocks through height 50,059,685, with zero lag
against each polled tip after catch-up. A release run of
`bin/clempo-dev follow -last 21000 -once -batch 500 -archive DIR -json`
against the same upstream verified 21,000 blocks (11,401 transactions)
in 1.312 s and wrote two complete 10,000-block chunks. These runs checked
canonical linkage and transaction, ommer, and withdrawal roots; they did
not execute blocks or check finalization.

`CLEMPO_BUILD=release CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev follow -certify -last 64 -seconds 12 -no-archive -json`
verified all 91 real mainnet finalization certificates through block
50,062,027, with zero sampled tip lag. Raw blocks came from the private
node; certificates came from `https://rpc.tempo.xyz`. Separately, `bin/clempo`
reopened a verified replay datadir at block 4, executed through a pinned
canonical block 8, and served its hash and state root over HTTP, matching
the Rust RPC. Tempo's constant native-balance response also matched.
The standalone image and authenticated loopback REPL were tested.
The one-hour executed-and-certified live run still depends on R3 reaching the tip.

Crypto EC (2026-11-21): secp256k1 recovery/signing/ECDH, EIP-7951 P256,
BN254 EIP-196/197, all seven BLS EIP-2537 operations, RFC9380, commonware
MinSig/MinPk/threshold signatures, KZG point evaluation and modular
exponentiation pass their recorded fixtures. `scripts/verify.sh` passed
173 tests with zero failures/skips and zero layer violations.
`CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive --load scripts/build.lisp --eval '(asdf:load-system "clempo-crypto-ec")' --load tests/crypto/ec/benchmark.lisp`
measures recovery at 87.74 us (under its 150 us target), P256 verification at
636.70 us, BN254 pairing (two pairs) at 9.974 ms, BLS pairing (two pairs) at
24.718 ms, BLS G1 MSM (128 points) at 205.979 ms, and BLS MinSig verification
at 61.807 ms. Pairing/MSM measurements include byte decoding and subgroup
checks. APIs, fixture/setup provenance, representation measurements and
side-channel limitations are documented in `tests/crypto/ec/README.md`.
These crypto-vector results alone do not score the Tempo precompile oracle.

Incremental trie hashing: on the same seeded 1M-key release benchmark,
reusable RLP and Keccak buffers cut first-root allocation from 808.8 MB to
65.3 MB (92% less). The serial first root took 0.906 s wall / 0.587 s CPU;
four subtree workers returned the identical root in 0.210 s wall /
0.588 s CPU. After changing one value, cached paths gave the same new root
at both worker counts (39 µs and 11 µs CPU respectively; both intentionally
serial). Inserts took 0.637 s and 0.695 s in those runs. See
`tests/trie/benchmark.lisp` for the command and thread argument.
This is a trie-only measurement; replay has not reached the requested
9,890,000–9,909,999 execution benchmark.

Mainnet-scale **synthetic state-root benchmark** (`tests/trie/state-benchmark.lisp`):
356,056 accounts and 10,666,802 slots, sized from the live reference's
`HashedAccounts` and `HashedStorages` table counts. Real `prestateTracer`
dirty-set cardinalities drive synthetic values and keys in `clempo.store`.
Setup took 51.77 s; the live state retained 3.31 GB Lisp heap after a full GC
(6.36 GB peak process RSS, including setup and speculative benchmark undo).
Serial and four-worker runs matched every root on the same 832 block updates.
Only dirty account/storage trie flush and hashing are timed; block execution,
flat writes, commits and disk I/O are excluded.

| Workload (four repeats) | Dirty blocks / total | Peak dirty slots | Serial dirty median / p95 | Four-worker dirty median / p95 |
|---|---:|---:|---:|---:|
| Recent mainnet | 160 / 512 | 21 | 108 / 239 µs | 91 / 183 µs |
| Evenly spaced busy-range sample | 20 / 256 | 596 | 0.400 / 17.449 ms | 0.369 / 2.394 ms |
| 16 highest-gas busy-range blocks | 64 / 64 | 29,020 | 173 / 409 ms | 49.7 / 116 ms |

The heaviest sample improved about 3.5× in median and p95 (mean root consing
9.64 MB/block); the recent workload never reaches the parallel threshold.
Timings are wall-clock, including scheduling and any GC in the measured root
call. This is not the full busy-range replay benchmark or an R4/R7 score.

Pool/builder integration: strict combined tests report **58 passed, 0 failed,
0 skipped**: 45 pool tests and all 13 builder tests. Executor inline-grant
signer/account binding and T3 duplicate token-limit rejection are integrated and
their tests enabled. Builder coverage includes rebuilding 26 recorded mainnet
blocks and a multi-transaction payload that re-executes to identical commitments.
`bin/clempo-dev pool-fixtures -iterations 10 -json` measures 40 admissions,
50 expected rejections and 40 selections in 0.139344 s (645.9 validations/s),
allocating 79,780,352 bytes including state construction and signing. These are
end-to-end fixture costs, not pool-only throughput. No production validation is
bypassed.

Archive R1 command: `CLEMPO_RPC=http://127.0.0.1:8545 nice -n 10 bin/clempo-dev r1-archive
-threads 6 -batch 100 -result logs/r1-archive/full.json` checks archived
raw blocks against independent full-block RPC responses, including byte-exact
round trips, header/body commitments, transaction hashes, recovered senders and
keychain key IDs. It records every failed block in JSONL and fails closed on
missing archive chunks or transport errors. It does not check receipts or execute
transactions; sampled R1 retains those receipt checks. A two-worker run over
blocks 0..19,999 checked **20,000/20,000**, with 20,095 transactions and 96 actual
cryptographic recoveries, in 4.369541 s: **4,577.1 blocks/s, 22.0 recoveries/s**.
These are end-to-end rates including archive I/O, RPC, JSON, codecs, roots and
recovery, not dense-transaction or crypto-only benchmarks. Evidence:
`logs/r1-archive/check-1795242754.json`.

The full-archive run uses six workers (at most six concurrent HTTP batches) and
niceness 10 to share the reference node. Repeat the same command with `-resume`
and the same `-result` to resume the atomically saved completed-chunk checkpoint.
The original range is retained even if the archive grows. Interrupted chunks are
rechecked without double-counting; failure/chunk logs append across attempts.
Resumed throughput includes committed checkpoint time, not downtime or trailing
work lost before a checkpoint.
A real-data missing-chunk/resume check retained 10,000 committed blocks, resumed
to exactly 20,000 unique blocks, and preserved the original end height despite
the isolated archive view gaining another chunk. It finished with zero failed
blocks and no duplicate completed chunks; evidence is
`logs/r1-archive/resume-test.eI4u3S/{first.log,resumed.log,result.json}`.

R1 full archive (2026-11-21, 4e28f4d): `CLEMPO_RPC=http://127.0.0.1:8545 nice -n 10 bin/clempo-dev r1-archive -threads 6 -batch 100 -result logs/r1-archive/full-1795243500.json` checked every mainnet block 0..50,049,999 (50,050,000 blocks, 35,979,413 transactions, 15,342,450 signature recoveries, 48.6 GB of raw blocks) against the reference's eth_getBlockByNumber(full): byte-identical re-encoding, block hash, transactions root, every tx hash and sender. 0 failures in 4,493 s (11,139 blocks/s, 6 threads, sharing the host with replays).

Full archive result at **4e28f4d**: **50,050,000/50,050,000 blocks** (0..50,049,999)
passed raw round-trip, block/body/transaction commitments, sender and key-ID checks
against independent full-block Rust RPC responses. The run covered **35,979,413
transactions**, including **15,342,450 actual cryptographic recoveries**; legacy
zero-signature system senders were compared but excluded from recovery counts.
All 5,005 chunks completed; there were **zero failed blocks and zero chunk errors**.
Measured scan time was **4,493.361169 s**, CPU time 4,460.574188 s:
**11,138.655 blocks/s and 3,414.471 cryptographic recoveries/s** end-to-end,
including archive I/O, RPC, JSON, codecs and roots. It processed 48,649,547,789 raw
bytes and cumulatively allocated 8,527,808,667,808 bytes (not peak memory).
This six-worker, niceness-10 release run predates the direct-address recovery
migration; it does not measure that optimization. Receipts and execution remain
outside this archive check, so it does not replace sampled R1 or score R7.
Result: `logs/r1-archive/full-1795243500.json`; its `.failures.jsonl`
is empty, with per-chunk evidence in `.chunks.jsonl` and console output in `.log`.

R3 = 50,049,999 (2026-11-21): `CLEMPO_BUILD=release bin/clempo-dev replay -datadir "$STATE" -checkpoint 500000 -diagnose -result "$REPORT"` reached the end of the archive. The final segment (from the 14.7M checkpoint) ran 35,354,196 blocks and 18,773,211 transactions in 9,612.5 s (3,678 blocks/s, 280 Mgas/s); `replay.sexp` records no heals and no skips since genesis. `bin/clempo-dev score -rung R3 -result "$REPORT" -datadir "$STATE" -json` reads the replay report and checks the stopped datadir's head against the archive.

R5 final (2026-11-21, `eebfce9` plus the deletion of the reference-timeout rule): the last 11 reference-timeout cases were re-recorded one at a time on the reference with `--db.read-transaction-timeout 0` (545–2,781 s each), so the corpus has no reference timeouts left and rpcdiff no longer has a timeout category. `CLEMPO_HEAP=40000 CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev rpcdiff -v` ran in 687 s over the archive through block 50,049,999, with the trie-cursor methods replayed one at a time.
- **Value: 14,697/14,697 identical** (the 14,686 of the pre-check below plus the 11 re-recorded `debug_intermediateRoots` cases, every one an exact match). 135/156 methods fully match; 156/156 reference methods are served. Shape 108/113 and node 13/15 are the misses explained below.

The pre-check that preceded it (`418a146`, 647 s, 16 threads):
- **Value: 14,686/14,686 identical.** By suite, all categories: admin 352/352, blocks 5,124/5,126, calls 853/853, consensus 16/16, envelope 50/50, filters 8/8, gaps 71/71, impl 46/46, logs 505/508, proofs 23/23, state 3,542/3,542, static 121/123, success 52/52, tempo-params 101/101, traces 2,927/2,927, txs 1,027/1,027.
- **11 cases then still graded under the timeout rule:** `debug_intermediateRoots` of blocks 1, 2, 4,494,230, 4,494,231, 5,593,984, 5,594,035, 6,033,924, 6,253,936, 6,253,937, 6,254,004 and 6,254,005. All 11 are now re-recorded and value-identical in the final run.
- **Shape and node misses, not counted:**
  - `eth_getBlockByNumber` `safe`/`finalized` and `eth_getLogs` `{}`/`latest`/`pending` describe the reference's head block at recording time, which had transactions and logs. clempo answers for its own head, block 50,049,999. The reference itself gives 0 transactions and `[]` logs for block 50,049,999.
  - `admin_peers` (the reference had 44 peers) and `eth_pendingTransactions` (its pool held a transaction) depend on the reference node. clempo's archive backend has neither peers nor a pool.
- **The 39 value mismatches of the previous full run (`3c43b9e`)** came from three causes:
  - **4 consensus WebSocket cases:** a type error in the WebSocket path of 72443d9's per-connection buffer. They pass since the ce7e7e6 server.
  - **35 trie-cursor cases** (28 `debug_intermediateRoots`, 5 `debug_executionWitness`, 2 `debug_stateRootWithUpdates`) were client read timeouts, not wrong answers. These calls take turns on clempo's single trie-state cursor, so at 16 threads they queued past the client's 120 s limit. A timeout on a pooled connection also re-sent the call, which lengthened the queue.
  - On the later ce7e7e6 server, a success-suite run also timed out trivial calls (`debug_seedHash`, `debug_dbGet`): a slow call stalled every other connection on its worker. That stall was fixed in 54fc44d and c34ac4c. With the stall fixed (on 54fc44d), 16-way replay still queues the cursor calls: the success suite scored 36/48 value cases. Each of the 12 misses was a cursor call that never answered, 11 by client timeout and 1 by a closed connection; none was a wrong answer. The reference fails these calls under the same load too, so rpcdiff now replays them one at a time (418a146). Replayed one at a time, all 130 `debug_intermediateRoots` cases take 253 s.
- **Fixed while triaging:**
  - `eth_feeHistory` now answers a range in the last 1,124 blocks the way reth's fee-history cache does: a reward row per block, empty for empty percentiles, at percentiles rounded to 0.25 (a6fdf57). This was probed on the reference: `[1, head-1123, []]` returns `[[]]` and `[1, head-1124, []]` returns `[]`.
  - The consensus event fixture now publishes an empty certified block, matching the empty block it recorded.
  - A WebSocket descriptor was closed twice, which made `consensus_getFinalization {"height":49000000}` read a reused descriptor ("Is a directory"). Fixed in 54fc44d.
- **The 21 methods not value-graded:**
  - *Head-following (live):* `eth_blockNumber`, `eth_gasPrice`, `eth_maxPriorityFeePerGas`, `eth_blobBaseFee`, `consensus_getLatest`.
  - *Depend on the reference node:* `eth_syncing`, `net_peerCount`, `web3_clientVersion` (version string), `admin_nodeInfo` (its enode, ports and ENR), `admin_validatorKey`, `admin_peers`, `eth_pendingTransactions`, `txpool_status`, `txpool_contentFrom`, and `txpool_content` and `txpool_inspect` (no recorded case: the reference's pool contents).
  - *Filter creation:* `eth_newFilter`, `eth_newBlockFilter` and `eth_newPendingTransactionFilter` return random ids. Their polls (`eth_getFilterChanges`, `eth_getFilterLogs`) are value-graded in the filters suite, 8/8.
  - *Live head-content misses above:* `eth_getBlockByNumber` (value 359/359) and `eth_getLogs` (value 842/842) are value-identical, but each has a live case that fails, so neither counts as a method.
- **The reference only ever returns an error,** so these methods are value-graded on their error answers:
  - Tempo stubs answering `unimplemented`: `eth_coinbase`, `eth_mining`, `eth_getWork`, `eth_submitWork`, `eth_getTransactions`, `token_getTokens`, `token_getTokensByAddress`, `token_getRoleHistory`.
  - No local accounts (`unknown account`): `eth_sign`, `eth_signTransaction`, `eth_sendTransaction`; `eth_signTypedData` was recorded only with malformed typed data (`Invalid params`).
  - Subscriptions over HTTP (`Internal error`): `eth_subscribe`, `eth_unsubscribe`, `admin_peerEvents`, `reth_subscribeChainNotifications`, `reth_subscribePersistedBlock`, `reth_unsubscribeChainNotifications`.
  - `debug_traceBadBlock`: `bad block not found in cache`.
- **A success case would write to the reference,** so only error paths are recorded:
  - `eth_sendRawTransaction` and `eth_sendRawTransactionSync` would broadcast a transaction.
  - `debug_setHead` would rewind the chain.
  - `admin_addPeer`, `admin_addTrustedPeer`, `admin_removePeer` and `admin_removeTrustedPeer` would change its peer set (352 parse-error cases).
  - `admin_clearTxpool` is the exception: the operator was allowed to call it and recorded 3 success cases.

R5 historical run (2026-11-21, `4c2db4d`): `CLEMPO_HEAP=40000 CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev rpcdiff` ran in 880 s with 16 threads and a complete history through block 50,049,999. Its legacy aggregate of 154 labels and 14,287 cases is not a value-match score. Technical observations from the run:
- State tries (src/rpc/tries.lisp): the archive backend loads clempo.store checkpoints (the history build's job-N.state files at 13,999,999 .. 50,049,999, or `$CLEMPO_TRIE_STATE`) and moves a state database between blocks with clempo.history's changed keys (`history-map-changes`; across the three gap seeds 14M/16M/20M, whose deletions the history does not list, every key is re-read). Every move is checked against the header's state root. Measured: checkpoint load 14 s (356k accounts, 10.7M slots, 1.4 GB heap), moves of 10k/50k/400k blocks 1.3–9.5 s. A pinned head state serves eth_getProof/getAccount/getMultiProof at "latest".
  Current datadirs no longer emit checkpoint files on flush; historical trie
  seeds come from history-build `job-N.state` files or explicit checkpoints
  exported with `clempo.store:write-checkpoint`.
- debug_intermediateRoots: the block re-executed on the parent's trie state with the state root after each transaction (`executor:flush-transactions`; a test checks flushing keeps gas, receipts, bloom and root). 132 cases in ~24 min at 16 threads, one cursor (the cases jump across 0..50M).
- debug_stateRootWithUpdates: reth's TrieUpdates (account_nodes/removed_nodes/storage_tries) emulated from reth's walker and alloy's HashBuilder store rules over `trie:trie-node-at`; both recorded cases identical.
- debug_executionWitness*: the full reth TrieWitness (`src/trie/witness.lisp`). It covers the multiproof of every loaded account and slot, the sibling proofs reth's sparse trie requests when a removal collapses a branch onto a blinded child (including the subtrie pre-check), and in legacy mode the storage root of every loaded account. Codes come from CacheDB's code-by-hash reads (plus deployed code in legacy mode). Keys skip slots of accounts loaded as absent. Both modes, by value: 21/21 witness cases match. The cases span 18 newly recorded blocks: genesis-era, system-only, empty T13, T7+ storage-credit blocks at 25M–50M, and busy-range blocks 9,892,716 (1,919 txs), 9,903,677, 9,903,676 and 9,904,550 (234 sibling-proof fetches), in legacy and canonical modes. rpcdiff compares state and codes as sets and keys grouped by account, because reth draws them from hash maps with a random hasher. Two reference calls on one block answer in different orders. Latency: clempo 0.0–50 s against the reference's 0.6–108 s (single-threaded). Block 9,904,453 is left out because the reference fails on it ("state trie overlay … cannot be anchored"). Also: debug_traceBlock works over raw RLP, and trace_replayTransaction of an unknown hash answers null.
- Reference timeouts: 56 `debug_intermediateRoots` cases originally returned MDBX error `-96000` at concurrency 16. Re-sent one at a time, all 19 tried answered in 60–284 s; those answers replaced the errors. The other 37 in this historical report had no result to compare and do not demonstrate parity.

R7 native P-384 (`828674f`): uncached verification **1,476.173 -> 198.248 µs CPU**
and **2,930,154 -> zero bytes/call** with caller scratch. The bounded opt-in
positive math cache was justified by 9,030 original verifications of 1,854
distinct tuples (6,093 potential hits in 64 slots); validity/policy/gas are
still evaluated every time. All 280 Wycheproof and NIST vectors remain.
Isolated Recent pairs against parent `4c2db4d` completed every comparison on
40,000 blocks: A **29.809/30.797 s wall, 35.525/35.349 s CPU**, B
**16.763/17.243 s wall, 22.388/22.852 s CPU**. Means improve 43.9% wall,
36.2% CPU, 72.8% allocated bytes; peak RSS/GC rose (reported in
[BENCH](docs/BENCH.md#native-p-384-and-repeated-nitro-signatures)).
Sampled inclusive P384 share fell 37.3% -> 0.8%. Evidence:
`logs/p384/recent-{A1,A2,B1,B2}/replay.json`.

Gate and rung check (`dc82753` series, 2026-11-21): R1 673/673 sampled, R2 yes, R3 report 50,049,999, R3e 61,903/61,903, R3p 4,986/4,992, and R6 simulation passed. `scripts/verify.sh`: 589 passed, 0 failed, 0 layer violations.
