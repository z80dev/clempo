# Frontier

| Rung | Score | Commit | Date |
|---|---|---|---|
| R0 Gate | scaffold: base, io, keccak, RLP, JSON, http, JSON-RPC client, layer check | - | 2026-11-20 |
| R1 Decode | mainnet 640/640 (T0..T13), moderato 33/33 | 1edd52f | 2026-11-21 |
| R2 Genesis | yes: matching state roots and block hashes on mainnet and Moderato | `ec20b6b` | 2026-11-21 |
| R3 Replay | 3,210 (every block 1..3210 matches gas, receipts root, bloom, state root; stops at 3211, the first user transaction, until the EVM lands) | `b74f4d8` | 2026-11-21 |
| R3e EVM | - | | |
| R3p Precompiles | - | | |
| R4 Follow | - | | |
| R5 RPC | - | | |
| R6 Validator | - | | |
| R7 Speed | - | | |

## Evidence

Keccak-256 (macro-unrolled Keccak-f over 25 typed lane variables): 250 MB/s on 64 MiB with no allocation, 0.57 µs per 64-byte hash, SBCL 2.6.8 release build on the dev host (Apple Silicon, 16 cores).

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

R4 upstream RPC path (partial, `e1fbffd`):
`CLEMPO_RPC=http://127.0.0.1:8545 bin/clempo-dev follow -last 1000 -seconds 20 -no-archive -json`
linked and checked 1,038 blocks through height 50,059,685, with zero lag
against each polled tip after catch-up. A release run of
`bin/clempo-dev follow -last 21000 -once -batch 500 -archive DIR -json`
against the same upstream verified 21,000 blocks (11,401 transactions)
in 1.312 s and wrote two complete 10,000-block chunks. These runs checked
canonical linkage and transaction, ommer, and withdrawal roots; they did
not execute blocks or check finalization.
