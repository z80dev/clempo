# BLS12-381 Montgomery groups

## Representation and ownership

`src/crypto/ec/bls12-381-group.lisp` replaces the integer/list Jacobian group
implementation without changing the public integer affine `(x . y)` API. Fp2
coordinates remain `(real . imaginary)` and infinity remains `NIL`. Curve and
subgroup predicates require canonical coordinates; byte encodings retain their
existing canonical validation.

Internal points occupy contiguous `[X,Y,Z]` Montgomery coordinates in a
`(simple-array (unsigned-byte 64) (*))`: 18 words for G1, 36 for G2. Offsets are
word offsets, including nonzero/nonaligned offsets. Infinity is `Z=0`; its X/Y
words need not be canonical or initialized.

Every public operation owns its arena. Raw arithmetic accepts caller-provided
scratch. There is no shared mutable scratch, random source, scalar callback,
thread-local cache, or per-window bucket allocation. A 30-word, read-only-after-load
endomorphism table is only read as a field operand, never written or used as
scratch. Public integer/Montgomery conversions and returned affine conses are
boundary allocations; raw double/add/multiply loops do not allocate Lisp points
or bignums. Negative scalar multiplication reads magnitude bits directly from
the signed integer rather than constructing a positive bignum.

## Internal integration contract

For either prefix `g1` or `g2`:

| Helper | Arguments |
|---|---|
| `jdouble!` | `(out o p po work wo)` |
| `jadd!` | `(out o p po q qo work wo)` |
| `jadd-affine!` | Same as `jadd!`; Q must have Z=1 or Z=0 |
| `jcopy!`, `jneg!` | `(out o p po)` |
| `jzero!` | `(out o)` |
| `jfrom-affine!` | `(out o public-point)` |
| `jto-affine` | `(p po work wo)`, returns the public point |
| `jequal-p` | `(p po q qo work wo)`, compares projective values |
| `jmul!` | `(out o p po signed-integer-scalar work wo)` |
| `jaffine-on-curve-p` | `(p po work wo)`; imported affine Z=1 or Z=0 only |

Output may exactly alias either input, or be disjoint. Partial overlaps are not
supported. Work must be disjoint from all input and output points. Mutating
helpers return no meaningful values. Affine conversion does not modify its input.

`+g1-jwork-words+` is 144; `+g2-jwork-words+` is 240. These bounds cover all
work-taking helpers except `jmul!`, whose bounds are
`+g1-jmul-work-words+` = 162 and `+g2-jmul-work-words+` = 276. The multiplier
reserves one copied point followed by ordinary group scratch, preserving exact
input/output aliasing even for negative scalars.

The multiplier never reduces a scalar modulo r. This includes scalars larger
than r, negative scalars, and non-subgroup points. MSM accepts equal-length
sequences of nonnegative integer scalars and likewise does not reduce them.
Pippenger uses one arena for imported points, buckets, accumulator, running sum,
and field scratch across every window. Mixed additions are used for bucket
insertion; ordinary projective additions combine bucket sums. Window widths are
2/3/5/6/8 for fewer than 8/32/128/512/more points respectively.

## Subgroup criteria and constant derivation

The specification read is Rust `ark-bls12-381` 0.6.0,
[`g1.rs`](https://github.com/arkworks-rs/curves/blob/v0.6.0/bls12_381/src/curves/g1.rs)
and [`g2.rs`](https://github.com/arkworks-rs/curves/blob/v0.6.0/bls12_381/src/curves/g2.rs).
These give the criteria from sections 6 and 4 of
[2021/1130](https://eprint.iacr.org/2021/1130), respectively. The implementation
uses field equations and Jacobian formulas, not copied Rust implementation code.

Let `x = -0xd201000000010000`, `r = x^4-x^2+1`, and
`p = (x-1)^2*r/3+x`. For G1, beta is the specified nontrivial cubic root of unity.
The test rejects nonzero P fixed by multiplication by `|x|`, then checks
`(beta*X,Y,Z) = -[x^2]P`, entirely projectively. For G2, with `xi=1+i`,

- `psi_x = xi^((1-p)/3)`;
- `psi_y = xi^((1-p)/2)`;
- `psi(P) = (conjugate(X)*psi_x, conjugate(Y)*psi_y, conjugate(Z))`;
- membership requires `psi(P) = [x]P`.

Exact independent integer/Fp2 calculations confirmed the BLS p/r polynomial
identities, `p mod r = x mod r`, `beta != 1`, `beta^3 mod p = 1`, and both psi
constants derived by exponentiating xi. Independent affine arithmetic also
confirmed the chosen beta root's `-[x^2]` eigenvalue and psi's signed `[x]`
eigenvalue on the canonical generators. These calculations do not load or run
the new Lisp implementation.

## Regression and performance gate

`tests/crypto/ec/bls12-381/group-tests.lisp` adds independent integer-affine
reference additions, signed scalar and scalar-boundary properties, exact aliasing
and nonzero-offset scratch guards, nonunit Jacobian Z checks, canonical rejection,
full-curve subgroup checks against unreduced `[r]P`, mathematical constant
identities, and vector/list MSM checks across every window-width boundary.
The order-three G1 point explicitly catches accidental scalar reduction, including
MSM with scalar r. Existing fixture vectors are unchanged.

The integration owner ran these properties with every unchanged EIP-2537,
hash-to-curve and commonware signature fixture under the strict crypto gate.
Independent affine oracles, alias guards, no-allocation assertions and unreduced
subgroup comparisons passed. See [performance.md](performance.md) for measured
release timings of the public byte APIs, rather than raw-group-only estimates.
