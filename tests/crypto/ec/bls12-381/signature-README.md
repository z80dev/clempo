# BLS signatures

Specification: `commonware-cryptography` 2026.10.1, Rust registry source
`src/bls12381/primitives/{group.rs,sharing.rs,variant.rs,ops/}` and
`commonware-utils` 2026.10.1 `union_unique`.

`signature-min-sig-vectors.lisp` and `signature-min-pk-vectors.lisp` embed every
line of Rust's `ops/test_vectors/min_sig.txt` and `min_pk.txt`, respectively,
unchanged as strings. Upstream attribution in `ops/mod.rs`: noble-curves commit
`bee1ffe0000095f95b982a969d06baaa3dd8ce73`, files
`test/bls12-381/bls12-381-g1-test-vectors.txt` and
`test/bls12-381/bls12-381-g2-test-vectors.txt`. Fields are big-endian secret scalar
hex, message hex, compressed signature hex, separated by colons. These fixtures
use the NUL DST, not Commonware's normal POP message DST.

## API

This component explicitly exports its `bls-*` symbols from `clempo.bls12-381`.
Every operation takes variant `:min-sig` (G2 public/G1 signature) or `:min-pk`
(G1 public/G2 signature) first. Secret scalars are integers in `[1,r)`.
Public keys and signatures are compressed octet vectors; external points are
always decoded and subgroup validated. Verification rejects infinity and returns
NIL for invalid-point encodings. Invalid API shapes/scalars signal errors.

- `bls-public-key(variant, secret)`
- `bls-sign(variant, secret, message, :namespace namespace, :dst dst)`
- `bls-verify(variant, public, message, signature, :namespace namespace, :dst dst)`
- `bls-pop-prove(variant, secret, namespace)` and
  `bls-pop-verify(variant, public, namespace, proof)`
- `bls-aggregate(variant, signatures, :public-keys t)` sums keys instead of
  signatures when requested. Input must be nonempty.
- `bls-aggregate-verify(variant, entries, signature)` accepts entries
  `(public namespace message)`.
- `bls-fast-aggregate-verify(variant, publics, namespace, message, signature)`
  rejects duplicate public keys. **Callers must first verify each key's POP**;
  neither aggregate verifier establishes possession. Aggregate validity does not
  imply individual signature validity.
- `bls-batch-verify(variant, entries, scalar-source)` accepts entries
  `(public namespace message signature)` and verifies their random linear
  combination. SOURCE is a required zero-argument function returning independent,
  cryptographically uniform unsigned 128-bit integers, **including zero** as in
  Rust's SmallScalar. The caller is responsible for unpredictability. No clock,
  global randomness, or hidden entropy is accessed. Duplicate keys/messages are
  safe. `bls-batch-verify-same-message(variant, entries, namespace, message,
  scalar-source)` takes `(public signature)` entries and returns invalid
  zero-based entry positions (NIL when valid), individually identifying failures
  after the randomized check fails.

NIL namespace means raw message bytes. An empty octet vector namespace instead
includes a zero length prefix. Commonware message APIs require an explicit octet
namespace: `bls-union-unique` encodes unsigned LEB128 namespace length followed by
namespace and message. POP hashes the framed **compressed public key**, using the
POP rather than message DST.

Threshold partials are `(participant-index . compressed-signature)` conses.
Participant indices are unsigned 32-bit and zero-based, strictly below TOTAL.
Default `:mode :nonzero-counter` maps index i to scalar i+1. The optional
`:mode :roots-of-unity` maps i to `w^i`, where w is `7^((r-1)/next-power-of-two(total))`,
matching Rust's root orientation.

- `bls-participant-scalar(index,total,:mode mode)` exposes this mapping.
- `bls-partial-public-key(variant,coefficients,index,total,:mode mode)` evaluates
  compressed public polynomial coefficients (constant first).
- `bls-threshold-sign(variant,secret,index,total,namespace,message,:mode mode)`
  signs with an already evaluated private share.
- `bls-threshold-verify(variant,coefficients,total,namespace,message,partial,:mode mode)`
- `bls-threshold-pop-prove(variant,secret,index,total,group-public,namespace,:mode mode)`
  signs the group public key, not the individual share key.
- `bls-threshold-pop-verify(variant,coefficients,total,namespace,partial,:mode mode)`
- `bls-threshold-recover(variant,threshold,total,partials,:mode mode)` selects the
  lowest threshold indices, ignoring remaining evaluations; rejects duplicate
  indices per Rust's stated caller contract. It interpolates at zero and returns
  an **unverified candidate**. Verify the candidate against the expected group
  public key and exact signing context before accepting it. Recovery does not
  establish correctness of individual shares.

Load signature-tests.lisp before the two signature-*-vectors.lisp files. The main
integrator owns ASDF registration. No checks were run by this component agent.

`bls-batch-verify-same-signer(variant, public, entries, scalar-source)` accepts
nonempty `(namespace message signature)` entries and returns T/NIL. It decodes
PUBLIC once, accumulates hashes and signatures with matching independent 128-bit
weights, then makes one pairing-check call with exactly two terms. Point sums
currently use the core scalar-multiply/add operations rather than an optimized
simultaneous MSM implementation. Namespace values must be octet vectors.
