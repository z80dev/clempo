# BLS12-381

## Components and representation

`clempo-crypto-ec` loads `bls12-381`, its raw field/tower/group/pairing files,
then `bls12-381-hash` and `bls12-381-signatures`, after SHA-256. No external
library, FFI, subprocess, filesystem, clock, or entropy is used by these sources.
The complete subsystem export list is declared once in the core DEFPACKAGE;
hash/signature components do not mutate it. Forced/repeated ASDF loads therefore
keep the same package contract under strict package-variance checks.

The package is `clempo.bls12-381`. `+p+` and `+r+` are the base-field modulus
and prime subgroup order. At public boundaries Fp elements are canonical
integers in `[0,p)`; Fp2 is a cons `(real . imaginary)` with `i^2=-1`.
Affine points are `(x . y)`; NIL is infinity. Treat points as immutable.
The curve equations are `y^2=x^3+4` and `y^2=x^3+(4+4i)`. Internally each Fp
occupies six little-endian 64-bit Montgomery limbs; Jacobian groups, the
Fp2/Fp6/Fp12 tower, projective Miller loop and cyclotomic final exponent use
caller-owned word arenas without allocating per operation. MSM is Pippenger
with reused buckets. See [field](field-performance.md), [group](group-performance.md)
and [end-to-end measurements](performance.md) for contracts and tradeoffs.
Neither scalar multiplication nor MSM reduces the scalar modulo r: this is
necessary for cofactor clearing and subgroup checks on arbitrary curve points.

Exported arithmetic (both `g1-` and `g2-` prefixes):

- `generator()` returns the standard subgroup generator.
- `add(p,q)`, `neg(p)`, `mul(p,integer)` return affine points.
- `msm(points,scalars)` accepts equal-length sequences, nonnegative scalars;
  empty inputs produce NIL. Scalar multiplication additionally accepts negatives.
- `on-curve-p(point)` and `subgroup-p(point)` include infinity. Arithmetic assumes
  well-shaped canonical curve points; validate untrusted representations first.
- `compress(point)` returns 48/96-byte Zcash/IETF compressed encoding. G2 places
  the imaginary x component first, unlike EIP-2537. The sort bit uses the
  lexicographically larger y, imaginary first for Fp2. Infinity is `c0` followed
  by zero bytes.
- `decompress(octets)` rejects wrong length/flags, noncanonical coordinates,
  off-curve points and wrong subgroup with `invalid-point`; infinity is allowed.
- `encode(point)` returns the EIP-2537 uncompressed 128/256-byte encoding.
- `decode(octets,:start offset,:subgroup t)` reads one EIP point at OFFSET,
  validating canonical fields and curve membership, plus subgroup by default.
  Larger surrounding buffers are allowed for precompile tuple decoding.

`pairing-check(input)` accepts either a list of `(g1-point . g2-point)` pairs
or EIP-2537 octets. It returns **`(values boolean valid-p)`**: a valid but false
pairing equation is `(NIL,T)`, invalid input is `(NIL,NIL)`. Every point is
subgroup checked. An empty mathematical list has product one; empty encoded
precompile input is invalid. Pairings use the negative BLS loop parameter,
projective denominator elimination in the M twist and the tower
`Fp2[v,w]/(v^3-(1+i),w^2-v)`. All pairs share Miller squarings and one exact
final exponentiation: Frobenius easy factors and a cyclotomic short-x hard chain.

## EIP-2537 byte APIs

`g1-add-precompile`, `g2-add-precompile`, `g1-msm-precompile`,
`g2-msm-precompile`, `pairing-precompile`, `map-g1-precompile`, and
`map-g2-precompile` each accept one octet vector and return
`(values output-octets valid-p)`, with NIL output on invalid input. Gas belongs
to the caller. ADD checks curve membership **without** a subgroup check; MSM
and PAIRING require the prime subgroup, including for zero scalar or paired
infinity. Scalars occupy 32 bytes and need not be less than r. Fp coordinates
occupy the full 64 bytes, including checked zero padding, and must be below p.
Fp2 order is real then imaginary; points are x then y. Only all-zero coordinates
encode infinity. Exact tuple lengths and nonempty variable inputs are enforced.

Hash-to-curve and signature contracts are in `hash-README.md` and
`signature-README.md`.

## Provenance and test integration

Curve, twist, generators, encodings and subgroup boundaries follow EIP-2537:
<https://eips.ethereum.org/EIPS/eip-2537>. The local specification sources read
were Rust `commonware-cryptography-2026.10.1` and the bundled blst 0.3.17
C mathematical specification (`consts.c`, `e1.c`, `e2.c`, `pairing.c`), and the
Arkworks subgroup criteria cited in `group-performance.md`. The Lisp implementation
is independently written, not copied library code. No Go source was read or copied.

All 15 `bls*.sexp` / `fail-bls*.sexp` files are DATA ONLY converted from every
`.json.gz` in goempo's repository, `internal/crypto/bls12381/testdata/`.
Each row retains `(Name Input Expected Valid)`. That fixture README records
upstream go-ethereum `core/vm/testdata/precompiles`, master fetched 2026-11-17.
The imported MSM files are the upstream fixture set's recorded subsets (up to
four points, plus large 60--128-point cases), not a newly filtered selection.
Every success and failure row is exercised, including the historical G1 MUL
file through the one-tuple MSM API. RFC9380 and commonware vector provenance
is documented by the two companion READMEs.

The ASDF test system registers the precompile, hash, signature and both
commonware signature-vector components. Keep the adjacent `.sexp` files.
The test files exercise subgroup rejection versus addition acceptance,
noncanonical padding, compressed flags/infinity, scalar identities, pairing
bilinearity, RFC hash vectors, namespace framing, POP, aggregation, randomized
batch verification and threshold interpolation in both participant modes.

The strict crypto suite passed all imported success/failure vectors, RFC9380
vectors and commonware signature vectors. `scripts/verify.sh` is the full gate.
