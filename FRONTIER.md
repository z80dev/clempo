# Frontier

| Rung | Score | Commit | Date |
|---|---|---|---|
| R0 Gate | scaffold: base, io, keccak, RLP, JSON, http, JSON-RPC client, layer check | - | 2026-11-20 |
| R1 Decode | mainnet 640/640 (T0..T13), moderato 33/33 | 1edd52f | 2026-11-21 |
| R2 Genesis | yes: matching state roots and block hashes on mainnet and Moderato | `ec20b6b` | 2026-11-21 |
| R3 Replay | 3,210 (every block 1..3210 matches gas, receipts root, bloom, state root; stops at 3211, the first user transaction, until the EVM lands) | `b74f4d8` | 2026-11-21 |
| R3e EVM | 61,903/61,903 (100%; Istanbul..Osaka, Osaka 19,517/19,517) | `750c66b` | 2026-11-21 |
| R3p Precompiles | 4,986/4,992 (goempo's frontier; every Tempo precompile T0..T14; the 6 differences are T14 keychain grants where clempo follows tempo c8b89f9, newer than the corpus: +2,600 gas root-retirement probe) | `fd2d4cf` | 2026-11-21 |
| R4 Follow | - | | |
| R5 RPC | 109/132 methods, 5,426/8,221 cases; 107/156 reference methods served (blocks 3,890/3,890, logs 366/366, envelope 50/50, static 106/119; state, calls and traces wait on the executor) | rpc slice 4 | 2026-11-21 |
| R6 Validator | Sims: 4, 5 and 7 validators, three epochs each. Rust devnet: observed necessary Clempo votes with both one and two Clempo signers through live reshares (integration branch) | `cb8ae2e`, `0f026a5` | 2026-11-21 |
| R7 Speed | - | | |

## Evidence

R6 sim (cb8ae2e): `bin/clempo-dev score -rung R6 -json` (or `bin/clempo-dev sim -scenario r6-4`)
runs src/sim/scenarios.lisp: clempo.simplex voters (commonware 2026.10.1 voter, Random V1
elector) and clempo.dkg (commonware Feldman/Desmedt reshare, Tempo's dealer logs in extraData
and boundary outcomes) on a seeded network; the produced chain goes through the same
certify-blocks used for mainnet, anchored at the genesis DKG outcome. Release build:
- r6-4: 4 validators, epochs of 20, latency 5-60 ms, 2% drops, node 3 down 1.5-4 s: 59/59
  blocks certified, 3/3 ceremonies concluded, group key unchanged, all 4 nodes at height 59
  (6,263 messages, 272 dropped; 7.9 s simulated, 131 s real).
- r6-5: 5 validators, epochs of 25, node 1 down 1-3 s, partition {0,1}|{2,3,4} 5-6.5 s:
  74/74 blocks certified (73 certificates, one block covered by its child's), 3/3 ceremonies,
  all 5 nodes at height 74 (183 s real).
- r6-7: 7 validators, epochs of 35, node 6 crashed for the whole run, node 2 down 3-6 s:
  104/104 blocks certified, 3/3 ceremonies, the 6 live nodes at height 104 (474 s real).
Real time is BLS12-381 field arithmetic (bignum mod); verify.sh runs smoke-4 (2 epochs).

R6 mixed Rust devnet (`origin/r6-validator`, built against the `e378f45`
builder checkpoint): separate loopback T13 networks, four-member committee and
3-of-4 threshold, with only slots `0,2,3` launched from genesis. On each fresh
root, provisioned with `scripts/devnet/devnet.sh --root "$NET" provision --tempo
"$HOME/dev/tempo/target/release/tempo" --base-port "$PORT"` (ports 26100 and 27100)
then ran:

```sh
D=scripts/devnet/devnet.sh
# Set C=3 for two Rust + one Clempo; C=2,3 for one Rust + two Clempo.
CLEMPO_BUILD=release $D --root "$NET" start --slots 0,2,3 --clempo "$C"
$D --root "$NET" capture --slots 0,2,3 --clempo "$C" --quorum \
  --seconds 115 --stall 20 --report "$NET/mixed-proof.jsonl"
$D --root "$NET" stop --slots 0,2,3
```

The two-Rust run compared 147 heights (0–146); Clempo slot 3 authored 48
proposals independently finalized by Rust. The one-Rust run compared 142
heights (0–141); Clempo slots 2 and 3 authored 47 and 46 finalized proposals
respectively. Both runs crossed boundaries 59 and 119. Every Clempo signer
was necessary for quorum, was selected as an on-chain dealer at both
boundaries, and finalized blocks with a new share in epochs 1 and 2.
`CLEMPO_BUILD=release bin/clempo-dev score -rung R6-devnet -json -one "$Q1"
-two "$Q2"` returned `2`; the reports (`mixed-proof.jsonl`) and node logs are
under `~/scratch/clempo/devnet/topic-quorum-{1,2}/`. The CLI is on the integration branch
pending the builder and validator merge.

Reference baseline: four Rust validators agreed on 146 heights (0–145),
including two 684-byte outcomes; each logged two successful ceremonies and
no failures. Clempo independently re-derived both DKG outcome byte strings
from dealer logs and replayed Rust raw blocks 0–130 with matching execution
commitments. The separate zero-vote probe authenticated three Rust peers and
received 156 votes, 156 certificates, 23 broadcast blocks and 69 DKG messages
in 15 seconds.

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

R7 Rust reference samples (Apple Silicon `Mac15,9`, Tempo 1.19.0-dev
`568b4fb4258c`, release). Both imports read canonical RLP and completed
`MerkleExecute` and `Finish` at the requested heights:

| Range | Gas | Wall s | CPU s | Peak RSS GB | Blocks/s | Mgas/s | Execution + MerkleExecute s |
|---|---:|---:|---:|---:|---:|---:|---:|
| Early: 1..1,000,000 | 135,315,780 | 89.14 | 41.67 | 7.22 | 11,218 | 1.52 | 17.35 |
| Recent: 49,410,000..49,449,999 | 3,768,403,621 | 47.22 | 21.41 | 3.72 | 847 | 79.80 | 15.67 |

`tempo import --fail-on-invalid-block` checks each import chunk's terminal
state root, not every block’s root. Clempo checks every block. Recent began
at the canonical parent obtained by unwinding a detached archive snapshot
(height 50,050,994). Provisioning time is excluded. See [benchmark
commands](docs/BENCH.md); R7 remains unscored.

Busy historical execution cross-check, **not a root-checked import**:
`tempo re-execute --from 9890000 --to 9909999 --num-tasks 1 --blocks-per-chunk 20000`
on the detached archive used parent 9,889,999 and passed receipt/gas and
change-set checks for all 20,000 blocks (31,499,321,696 gas). `/usr/bin/time -l`
reported **49.37 s wall**, **20.08 s CPU**, **1.65 GB peak RSS**,
**405 blocks/s** and **638.03 Mgas/s** end to end (Rust's internal interval:
651.01 Mgas/s). The full staged Busy import requires a roughly 40M-block
unwind of the detached archive to provision its exact parent.

Keccak-256 (SBCL 2.6.8 release, Apple Silicon): the ARM SHA3 permutation and
copy-free 32/64-byte paths reduced 64-byte hashes from **626 ns to 178 ns**
(1 million calls, about 3.5× faster) and 480 to 272 bytes consed per hash.
On 64 MiB, throughput rose from **256 to 799 MB/s** (262 to 84 ms), with no
measurable consing. The typed scalar permutation remains the fallback.

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

R3 = 3,210 (b74f4d8): `bin/clempo-dev replay -datadir ~/scratch/clempo/r3/dd -checkpoint 500000 -diagnose -result ~/scratch/clempo/r3/replay.json`
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

Pool/builder integration: strict combined tests report **56 passed, 0 failed,
2 explicitly skipped** for executor inline-grant signer/account binding and the
keychain precompile's T3 duplicate token-limit rejection. The pool has 43 passing
tests; all 13 builder tests pass, including rebuilding 26 recorded mainnet blocks
and a multi-transaction payload that re-executes to identical commitments.
`bin/clempo-dev pool-fixtures -iterations 10 -json` measures 40 admissions,
50 expected rejections and 40 selections in 0.139344 s (645.9 validations/s),
allocating 79,780,352 bytes including state construction and signing. These are
end-to-end fixture costs, not pool-only throughput. No production validation is
bypassed; the two named test skips remain explicit integration limitations.
