# BLS12-381 native arithmetic rebuild

## Representation decision

Public affine points, integer coordinates, Fp2 conses, encodings and exported
functions are unchanged. The former bignum/list pairing tower and group engine
have been removed, not retained as a second production backend. The hot engine
uses six little-endian 64-bit Montgomery limbs per Fp, with `R=2^384`. Macro
expansion generates typed SBCL word arithmetic. This avoids per-operation
bignum allocation and division; the release dependent-chain measurement is
45 ns CPU per Fp multiplication, with zero bytes consed.

Fp2 uses 12 words, Fp6 36, and Fp12 72. The tower is
`i^2=-1`, `v^3=1+i`, `w^2=v`, laid out as `[A:Fp6 B:Fp6]`.
Karatsuba products, complex squaring and sparse 014 line multiplication operate
in caller-owned scratch. Jacobian G1/G2 points occupy 18/36 words. Pairing,
scalar multiplication and Pippenger each allocate their arena once; no loop
allocates an intermediate point, tower object, bignum or bucket array. There
is no global or thread-local mutable scratch. Read-only field conversion,
Frobenius and endomorphism tables are shared safely across callers.

See `field-performance.md` and `group-performance.md` for exact offset,
aliasing and scratch contracts. Integer exports use one Montgomery multiply
by ordinary one and construct just the result bignum. Fixed 32/48-byte readers
likewise construct one result rather than accumulating bignum shifts per byte.
Hash-to-curve retains declarative integer boundary formulas, with field powers
and inversions delegated to native stack-local arenas. Arithmetic is variable
time; this is not a constant-time secret-key implementation.

## Pairing and subgroup algorithms

The optimal-ate loop uses the magnitude of `x=-0xd201000000010000`, shared
Fp12 squarings across all input pairs, projective tangent/addition lines,
sparse 014 multiplication, and conjugation for the negative loop parameter.
Projective line denominators are Fp2 factors eliminated by final exponentiation;
there is no inversion inside the Miller loop.

The easy final factor is `(p^6-1)(p^2+1)`. The exact hard exponent is

```
(p^4-p^2+1)/r = 1 + ((x-1)^2/3) * (p+x) * (p^2+x^2-1).
```

This follows from `p-x=(x-1)^2*r/3` and `r=x^4-x^2+1`.
A short signed-x chain, Frobenius maps and Granger-Scott cyclotomic squaring
replace a full 1268-bit generic hard power. The tests compare the complete
final result to exponentiation by `(p^12-1)/r`, not merely a pairing boolean.

G1 membership uses the cubic endomorphism and `-[x^2]P`, with the nonzero
`[|x|]P=P` exclusion. G2 checks `psi(P)=[x]P`. Independent unreduced `[r]P`
reference checks cover full-curve inputs. Pippenger reuses one set of buckets
across windows. Byte-level MSM still validates **every** input subgroup,
including zero-scalar inputs. Public pairing likewise validates both points
of every pair, even when the other point is infinity. Signature-only internal
pairing accepts points already checked by decompression or derived from
subgroup hashes/arithmetic, avoiding redundant validation without weakening
any public boundary.

## Reproduction

SBCL 2.6.8, Darwin ARM64, release build, 8 GiB heap:

```sh
CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive \
  --eval '(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))' \
  --load scripts/build.lisp \
  --eval '(asdf:load-system "clempo-crypto-ec")' \
  --load tests/crypto/ec/bls12-381/speed.lisp
```

Each case warms up once, collects before timing and reports process CPU time,
wall time and allocation counters. Pairing uses two non-infinity pairs with
opposite G1 points. MSM uses 128 distinct generator multiples and deterministic
full-width Keccak scalars. MinSig uses compressed public key/signature and the
`tempo` namespace. Arena creation, canonical validation, decoding, allocation
and result encoding are included in public API numbers. The kernel case uses
one million dependent multiplications on preconverted operands.

## Measured results

The historical integer implementation's original release wall/API baseline is
recorded in `../README.md`: pairing2 **24.718 ms**, MinSig **61.807 ms**,
MSM128 **205.979 ms**. Those historical measurements did not separately record
CPU time, so they are not presented as a controlled wall-time speedup ratio.

The integrated native rebuild, one benchmark process with no simultaneous test
process launched by this agent. The 16-core host was shared with approximately
15 agents during measurement:

| Operation | CPU/call | Wall/call | Bytes/call |
|---|---:|---:|---:|
| BLS pairing2, EIP byte API, 300 calls | 1.845 ms | 5.040 ms | 38,013 |
| BLS MinSig verify, 300 calls | 2.689 ms | 6.744 ms | 133,000 |
| G1 MSM128, EIP byte API, 100 calls | 13.769 ms | 35.102 ms | 449,990 |
| G1 MSM128, raw public point API, 100 calls | 5.731 ms | 14.960 ms | 29,792 |
| 128 G1 subgroup checks, 100 calls | 8.157 ms | 25.818 ms | 222,622 |
| Fp multiplication, 1,000,000 dependent calls | 45 ns | 193 ns | 0 |

The API runs reported only **31.6–39.9% CPU**; the Fp run reported 23.4%.
Thus the 3/4/15-ms pairing/MinSig/MSM targets are met in process CPU time,
**not in the observed contended wall time**. No uncontended wall attainment
is claimed. Allocation fell from the historical 60.06/163.52/391.68 MB per
pairing/MinSig/MSM call to approximately 38/133/450 KB respectively.

## Verification

Strict crypto command:

```sh
sbcl --dynamic-space-size 8192 --non-interactive \
  --eval '(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))' \
  --load scripts/build.lisp \
  --eval '(clempo-build:test-all :dirs (list "crypto") :strict t)'
```

Observed: **95 passed, 0 failed, 0 skipped**. All original EIP-2537 success and
failure rows, RFC9380 vectors, commonware MinSig/MinPk, aggregation, randomized
batch, threshold and KZG fixtures remain enabled and pass. New independent
properties cover 4,096 field operand pairs, limb boundaries, aliases, scratch
guards, extension-field oracle products, sparse lines, Frobenius, exact final
exponents, full-curve subgroup comparisons, signed unreduced scalars, and MSM
window boundaries. Allocation counters assert zero consing for preallocated
raw field/tower/group kernels and the complete Miller-plus-final-exponent hot
region. The repository-wide cold gate remains `scripts/verify.sh`.

BN254 was not mechanically converted: its four-limb base field, `9+i` twist
and distinct loop require a separate engine and validation effort, rather than
a cheap instantiation of this BLS-specific six-limb backend.
