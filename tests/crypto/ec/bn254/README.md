# BN254 / EIP-196 and EIP-197

## Components

Production: `src/crypto/ec/bn254.lisp`, package `clempo.bn254`.
Tests: `tests/crypto/ec/bn254/bn254-tests.lisp`.
Both components are registered in `clempo-crypto-ec` and its test system.
Production depends only on Common Lisp and `clempo.base`, not a hash package,
FFI, processes, or libraries.

## Public byte API

Inputs and encoded outputs are simple unsigned-byte-8 arrays. Words are unsigned
32-byte big-endian integers.

- `bn254-add(input)` returns 64 octets or `nil` for invalid input points. It reads
  two G1 points from the first 128 bytes, right-zero-pads missing bytes, and
  ignores excess bytes.
- `bn254-mul(input)` returns 64 octets or `nil` for an invalid input point. It
  reads one G1 point and one scalar from the first 96 bytes, with the same byte
  padding/truncation rules. Every 256-bit scalar is accepted, including values
  at or above the group order; multiplication reduces the scalar modulo `r`.
- `bn254-pairing(input)` returns two values, `(boolean, valid-p)`. The input is
  an exact multiple of 192 bytes, each comprising G1 then G2. `(nil,t)` is a
  valid false pairing equation; `(nil,nil)` is invalid encoding/point/order.
  Empty input and equations containing only infinity factors return `(t,t)`.
  Both points in every pair are validated, even when the other is infinity.

G1 encodes `x || y`. G2 encodes `x_im || x_re || y_im || y_re` as EIP-197
requires, **not** real-part first. All wire field coordinates must be strictly
less than the base-field prime, without reduction. The all-zero encoding is
infinity; other points must satisfy their curve equation. G1 has cofactor one.
G2 additionally requires `[r]Q = infinity`; the multiplication used for that
check deliberately does not reduce `r` modulo the scalar order.

Exported coordinate helpers:

- `decode-g1(input, &optional start=0)` returns `(point, valid-p)` with EIP-196
  right-padding. `encode-g1(point)` returns 64 octets.
- `decode-g2(input, &optional start=0)` returns `(point, valid-p)` and requires
  128 available bytes, canonical coordinates, the twist equation, and subgroup
  membership. The second value distinguishes infinity from invalid input.
- `g1-add(a,b)` and `g1-mul(point,scalar)` operate on already-validated affine
  points; the scalar contract is a nonnegative integer.
- `g2-in-subgroup-p(point)` assumes an already-validated on-twist point.
- `+field-modulus+` and `+group-order+` expose `p` and `r`.

Offsets are nonnegative byte offsets. Decoding helpers ignore following bytes;
only the pairing entrypoint enforces its complete-input length constraint.
Affine points are `(x . y)` and infinity is `nil`. G1 coordinates are ordinary
canonical Lisp integers. G2 coordinates are Fp2 pairs `(real . imaginary)`, so
an affine G2 point is `((x_re . x_im) . (y_re . y_im))`. Internal Jacobian
coordinates use three-element vectors and are not a public representation.

## Arithmetic

The tower is `Fp2 = Fp[i]/(i^2+1)`, `Fp6 = Fp2[v]/(v^3-(9+i))`,
`Fp12 = Fp6[w]/(w^2-v)`. Fp6 is a three-element coefficient vector; Fp12 is
`(even-coefficients . odd-coefficients)` in powers of `w`. No Montgomery or
machine-endian representations escape field arithmetic.

G1 multiplication and G2 order checks use specialised Jacobian group laws.
Pairing uses an unsigned optimal-Ate loop with `6u+2`,
`u = 4965661367192848881`, followed by the Frobenius correction points. A
single Miller accumulator shares squarings across all pairs. Affine line
slopes use extended-Euclidean field inversion; sparse line multiplication
avoids multiplying zero tower coefficients. Vertical denominators lie in Fp6
and disappear under final exponentiation. The final exponent is split into
the easy conjugation/inversion/Frobenius part and a four-bit sliding-window
hard exponent. The integration gate exercises all imported reference vectors.

## Specification and data provenance

Rust sources were read as specifications, not incorporated as production code:

- The node's installed EVM implementation:
  `~/.cargo/git/checkouts/evm2-e7f3df04b1900f38/9b03744/crates/evm2/src/precompiles/bn254/{mod,substrate,arkworks}.rs`.
  This specifies padding, scalar interpretation, coordinate order, infinity,
  validation before infinity skipping, and pairing-product behaviour.
- [substrate-bn](https://github.com/paritytech/bn/tree/63f8c587356a67b33c7396af98e065b66fca5dda),
  `src/lib.rs` and `src/groups/mod.rs`, specifies canonical field decoding,
  the G2 order check, and the optimal-Ate Frobenius correction.
- Installed `ark-bn254-0.6.0` curve and extension-field parameters and
  `ark-ec-0.6.0/src/models/bn/{mod,g2}.rs` specify the same twist, tower,
  Frobenius constants, and final-exponent factorisation.

Fixture source directory:
`~/clempo-infra/goempo-ref/internal/crypto/bn254/testdata/`.
**Only fixture data and its README were read there; no Go source was read.**

| Imported data | Cases | Source | Source SHA-256 |
| --- | ---: | --- | --- |
| `bn256Add.sexp` | 16 | `bn256Add.json.gz` | `0469543417e9506a08f303249315d6b2f9edf4746bd4add6c78a785299169987` |
| `bn256ScalarMul.sexp` | 19 | `bn256ScalarMul.json.gz` | `ea22b13bc4e70205a8f9418029dfe51981b2e27220fc35ec6c1240a054050796` |
| `bn256Pairing.sexp` | 14 | `bn256Pairing.json.gz` | `1f79ef4b8a2374cb15dac0abdbc43584c9379ee919fbda904647a516bf54a8fd` |
| `g2-subgroup.sexp` | 12 | `g2_subgroup.txt` | `1396cad41223766424bf97996d495bfdd1500d004e61565658e17bf1f57804ce` |

The source README attributes the three JSON files to go-ethereum
`core/vm/testdata/precompiles`, master fetched 2026-11-17. They contain EIP-196/197
inputs, expected outputs, and Istanbul gas metadata. Every JSON object and all
five fields were mechanically retained in S-expression data; gas and benchmark
metadata are recorded but not asserted by this arithmetic-only package.

The subgroup file contains six independently generated `[k]G2` points and six
on-twist points outside the order-`r` subgroup, checked by its producer with
`[r]Q`. The conversion preserves every classification and encoded coordinate.
The tests independently assert that these twelve points are on the twist before
checking membership and byte-level validation.

Tests cover all 49 JSON vectors and 12 subgroup vectors without external tools
at test time, plus canonical-coordinate boundaries, invalid curve points,
G2 coefficient order, invalid lengths, infinity handling, arbitrary scalars,
every EIP-196 partial-input byte length, ignored trailing bytes, true/false vs
invalid pairing results, and field-tower square/inverse/sparse/Frobenius identities.
Fixture reading disables reader evaluation. Fixture files are runtime test data,
not Lisp components to compile or load as code.

The strict crypto suite passed all vectors and validity boundaries. Run
`scripts/verify.sh` for the cold compile, full test suite and layer checks.
