# BLS12-381 raw Fp kernels

## Representation and ownership

`src/crypto/ec/bls12-381-field.lisp` defines internal functions in
`clempo.bls12-381`; it loads after the subsystem package and `+p+` definition.
It changes neither the exported API nor public affine integer representations.

An Fp value occupies six consecutive little-endian `(unsigned-byte 64)` words
in a `(simple-array (unsigned-byte 64) (*))`. The stored integer is canonical
in `[0,p)` and represents `x * R mod p`, with `R = 2^384`. Every offset is a
word offset. Callers supply valid arena extents and canonical operands.

All mutating functions return zero values. Output may exactly alias either
input or be disjoint; partial overlaps are unsupported. Scratch must be
disjoint from output and all inputs, but can occupy another interval of the
same arena. There is no shared mutable scratch. Conversion operands R² and
ordinary one are read-only after initialization.

## Complete interface

| Function | Arguments after function name | Scratch |
|---|---|---|
| `fp-zero!`, `fp-one!` | `out out-offset` | none |
| `fp-copy!`, `fp-neg!`, `fp-square!` | `out out-offset a a-offset` | none |
| `fp-add!`, `fp-sub!`, `fp-mul!` | `out out-offset a a-offset b b-offset` | none |
| `fp-zero-p` | `a a-offset` | none; returns boolean |
| `fp-equal-p` | `a a-offset b b-offset` | none; returns boolean |
| `fp-from-integer!` | `out out-offset integer` | none |
| `fp-to-integer` | `a a-offset` | none; returns canonical integer |
| `fp-pow!` | `out out-offset a a-offset exponent work work-offset` | reserve 24 words; first 12 used |
| `fp-inv!` | `out out-offset a a-offset work work-offset` | reserve 24 words; first 12 used |

`fp-pow!` accepts nonnegative integer exponents, including bignums, and defines
`0^0 = 1`. Negative exponents signal an error. `fp-inv!` uses Fermat's theorem
and signals `invalid-point` on zero, preserving the existing field convention.
Both keep the original exponent immutable and inspect its bits instead of
allocating a shifted bignum or a fresh array on every bit.

Canonical integer import reads SBCL bignum digits directly and performs one
Montgomery multiplication by R², without allocating intermediate arrays or
integers. Import of negative/out-of-range integers reduces at the integer
boundary with `mod`, which may allocate. Export uses one Montgomery multiplication
by ordinary one into stack-local scratch and constructs only the resulting
integer. No arithmetic kernel or power/inversion loop calls an integer conversion.

## Implementation

Multiplication is macro-unrolled six-word coarsely integrated operand scanning.
Each of six rounds adds one operand digit's product, cancels the low word with
`m = low * (-p^-1) mod 2^64`, then shifts by one word. Since `p < 2^381`, the
round's top carry sum fits a word and the final result is below `2p`. One
conditional subtraction restores canonical form. Inputs are not overwritten
until the final stores, including when both inputs and output are identical.

The implementation uses SBCL's 64-bit digit intrinsics on ARM64 and other
64-bit SBCL targets; there is no architecture-specific VOP or FFI dependency
requiring a separate fallback. It deliberately uses the three-argument
`%multiply-and-add` followed by `%add-with-carry`, avoiding SBCL 2.6.8 ARM64's
four-argument register-alias bug. Squaring uses the same multiplication kernel.
This is variable-time arithmetic, not a constant-time cryptographic primitive.

## Validation and performance

The integration owner ran the strict crypto suite, including every old precompile,
RFC9380 and commonware fixture. Native field properties and zero-allocation
assertions passed. End-to-end and kernel measurements are in
[performance.md](performance.md); `speed.lisp` contains the reproducible harness.

`tests/crypto/ec/bls12-381/field-tests.lisp` provides self-contained deterministic
checks without external fixtures:

- conversion and raw Montgomery representation, including signed/1024-bit input;
- all arithmetic over limb/bit boundaries, `p-1`, alternating-byte patterns, and
  direct raw-residue carry/borrow chains;
- 4,096 deterministic operand pairs against integer modular arithmetic;
- output aliases for either operand, all-identical aliases, nonzero offsets,
  disjoint intervals within one arena, untouched inputs, and guard words;
- powers with zero/small/multiword exponents, inverses, and error cases;
- allocation-counter assertions around raw arithmetic, canonical import,
  multiword powers, and inversions with preallocated caller scratch.

The benchmark's final case performs one million dependent in-place multiplications
on preconverted operands. The timed region excludes import, arena creation and
warmup, and reports both wall/process CPU time and bytes consed. The same harness
measures the public byte-level pairing/MSM APIs and compressed-key MinSig verify.
