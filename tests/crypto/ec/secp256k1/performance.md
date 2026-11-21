# Native secp256k1 recovery and P256 verification

## Allocation contract

`clempo.secp256k1:ecrecover(hash,r,s,recid,&optional output,scratch)` preserves
its four-argument API and fresh 64-octet result by default. A supplied OUTPUT
must have length 64. With both caller-owned output and scratch, the complete
recovery call allocates no Lisp objects. Invalid r/s, nonresidue R, recid outside
0..3, x overflow and an infinity recovered key still return NIL. High-s remains
accepted, as required by Ethereum's recovery precompile.

`ecrecover-address(hash,r,s,recid,&optional scratch)` hashes the pubkey directly
from scratch and constructs only the returned positive 160-bit integer.
`make-recovery-scratch` creates private mutable word/digit/pubkey/digest arenas.
Bind `*recovery-scratch*` once per worker; never install a shared arena in its
global value. NIL safely requests fresh storage. Supplied scratch cannot be
used concurrently or reentrantly. No global/thread-local auto-cache is hidden
inside the crypto layer. `src/types/signatures.lisp` uses the direct address
API, avoiding both a returned pubkey vector and a second digest conversion.

`ecdh(private,peer,:compressed t)` returns the SEC1 compressed shared point
needed by discovery v5; the two-argument raw x32 result is unchanged. Discovery
uses this public API rather than depending on retired private curve internals.

P256 exports `make-p256-scratch` and `*p256-scratch*`; both `p256-verify` and
`p256-verify-low-s` accept optional sixth scratch arguments. The former accepts
high-s; the latter preserves Tempo's low-s rule. With a supplied or dynamically
bound arena, verification allocates zero objects. NIL retains a fresh, isolated
fallback. SBCL 2.6.8 ARM64 does not stack-allocate these specialized vectors from
a dynamic-extent declaration, so the allocation-free contract explicitly uses
caller-owned persistent storage rather than assuming such an optimization.

## Arithmetic

The old per-operation boxed field/Jacobian/GLV production engine is removed.
secp Fp uses four canonical ordinary little-endian 64-bit words. A macro emits
unrolled Comba products, symmetric squares and special-form reductions using
`2^256 = 2^32+977`. ARM64 keeps carry chains inside generated SBCL VOPs;
other targets use the real portable word implementation.
P256 Fp and scalar fields use four-word Montgomery residues. Public scalar,
address and coordinate representations remain plain integers.

ARM64's unboxed register bank is small, so the VOPs borrow descriptor registers
for arithmetic scratch. **No allocation/call inside a VOP is not sufficient GC
protection**: signals can interrupt it. Each such VOP is pseudo-atomic and scrubs
every borrowed descriptor back to NIL before checking for pending interrupts.
The guard needs no heap-initialization memory barrier because it only computes
register results. No foreign code is called. The concurrent-GC regression uses
an intermediate deliberately shaped like an interior heap pointer, not just
random words that almost never resemble one. See SBCL's
[ARM64 guard macro](https://github.com/sbcl/sbcl/blob/master/src/compiler/arm64/macros.lisp)
and [precise register roots](https://github.com/sbcl/sbcl/blob/master/src/runtime/gencgc.c).

Recovery parses the digest directly into words, uses a native binary scalar
inverse, Montgomery scalar products, exact signed GLV lattice identities and
four signed wNAF streams. Immutable G/phi(G) tables use width 8; variable R/phi(R)
use width 5. The Strauss loop shares 128-bit doublings across all streams.
Variable odd R points use prefix/suffix Z products to obtain a common denominator
without inversions. Mixed additions run on an isomorphic a=0 curve; multiplying
the final Z by the common denominator restores original-curve coordinates.
There is exactly one field inversion at the affine output boundary. Fp inverse
and square-root addition chains use fixed word scratch; no bignum FLOOR occurs
in the recovery arithmetic.

P256 compares the resulting projective X to `r Z²` and, when `r+n<p`, `(r+n)Z²`,
so it requires no final affine inverse and preserves EIP-7951's x-mod-n rule.
Neither implementation is constant-time secret-key arithmetic.

## Reproduction and measurements

```sh
CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive \
  --load scripts/build.lisp \
  --eval '(asdf:load-system "clempo-crypto-ec/tests")' \
  --load tests/crypto/ec/secp256k1/speed.lisp
```

SBCL 2.6.8, Darwin ARM64, 8 GiB heap. Initial arena cutover measurements:

| Operation | CPU/call | Wall/call | Allocated bytes/call |
|---|---:|---:|---:|
| Previous ecrecover, 10,000 calls | 94.147 us | 172.906 us | 288,148 |
| Arena ecrecover, supplied output/scratch, 20,000 calls | 73.008 us | 90.324 us | 0 |
| Arena ecrecover, fresh pubkey, 10,000 calls | 73.346 us | 98.483 us | about 80 |
| Previous P256, 1,000 calls | 307.102 us | 379.740 us | 648,362 |
| P256 verification, supplied scratch, 2,000 calls | 182.005 us | not recorded | 0 |
| Dependent Fp multiplication, 1,000,000 calls | 20.914 ns | 20.883 ns | 0 |

The P256 baseline loaded the historical field/curve sources from commit
`f6a2b6a166` in an isolated SBCL process, including its old field packing helper,
and used the same valid signature as the native benchmark. No working source
was reverted or modified for that measurement.

These initial CPU timings do not meet the 25-us recovery target. The standalone
post-cutover CPU profile (1,656 samples) attributes 66.2% self to Fp multiplication,
6.4% to field addition, 5.6% to subtraction and 8.6% inclusive to scalar inversion.
The earlier shared Busy profile remains the end-to-end baseline; it attributed
55.7% inclusive to recovery and 22.7% exclusive to Darwin mutex wait/drop. The
standalone profile does not establish causality for those multiworker waits.

SBCL's allocation counter counts closed regions and may omit the current partial
region, so per-call nonzero allocation estimates are approximate. The zero-cons
checks use repeated preallocated operations; address tests allow one allocation
region of counter rounding around one 32-byte result bignum per call. Default
fresh pubkeys are never claimed to allocate zero bytes.

## Regression coverage

All original mainnet/EEST sender, RFC6979, ECDH and P256 fixture rows are retained.
Existing random field/reduction datasets now exercise the native operations
through test-only integer adapters, not a retained production fallback. New
properties cover native scalar products and inverses, signed GLV reconstruction,
129-bit bounds, inverse/square-root chains, common-Z joint multiplication against
an independent affine oracle, aliases, guards, scratch poisoning/reuse, optional
output freshness, rare recid=2/3 recovery, and zero allocation with supplied arenas.
The full integration gate is `scripts/verify.sh`.
