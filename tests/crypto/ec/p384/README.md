# Nitro P-384 verification

`clempo.p384:p384-verify(hash48,r,s,qx,qy,&optional scratch)` verifies ECDSA over an already computed 48-byte SHA-384 digest. Both s and n−s are accepted. Scalars must satisfy 0<r,s<n; public coordinates must be canonical and on NIST P-384, and the verification result must not be infinity. `p384-valid-public-key-p(x,y,&optional scratch)` exposes the same canonical-coordinate/on-curve validation. Cofactor one makes an extra subgroup-order multiplication unnecessary. Integer p/n constants are exported as `+p384-p+` and `+p384-n+`.

The caller parses SEC1 public keys and DER/P1363 signatures and constructs the digest (`clempo.sha2:sha384`); no hashing or ASN.1 parsing occurs in this component. These boundaries follow `crates/nitro-attestation/src/lib.rs::P384Verifier`, which explicitly requires high-s acceptance and precomputed SHA-384 digests.

## Native arithmetic and contexts

The generic production engine is removed. Fp and scalar N use six little-endian 64-bit Montgomery limbs. Full-width CIOS keeps a seventh accumulator word and both top carries: both moduli approach 2^384, so the BLS381 headroom shortcut is invalid. ARM64 multiply/add intrinsics use only unboxed registers with explicit non-aliasing constraints; the portable backend uses SBCL word arithmetic. Native binary inversion uses twelve caller scratch words. The a=−3 Jacobian Strauss loop uses signed wNAF, immutable width-14 G tables and width-5 variable-key tables. One batch inversion normalizes the latter; verification compares projective X against rZ² or (r+n)Z² and needs no final affine inverse.

`make-p384-scratch` creates reusable private arithmetic storage. Supply it explicitly or bind `*p384-scratch*` once per thread; the global value remains NIL. Contexts cannot be shared concurrently or used reentrantly. Without a supplied/bound context, the API creates isolated storage. The arithmetic is not a constant-time secret-key API.

`make-p384-scratch :memoize t` opts into a bounded, positive-only 64-entry FIFO cache keyed by the complete public coordinates, digest and r/s. Mutable digest bytes are copied into preallocated storage. Default contexts do not memoize. Only the pure mathematical predicate is cached: Nitro timestamp/expiry, chain/root/path constraints, parsing, policy and gas checks still execute every time. This follows the stateless prehashed reference verifier in `crates/nitro-attestation/src/aws_lc.rs`; no attestation/certificate result is cached.

An instrumented Recent replay on `6c0f9d2` still executed all 9,030 original verifications (7,224 certificate, 1,806 attestation). It observed 1,854 distinct tuples, identical answers on every repetition, and 6,093 potential positive hits in a simulated 64-entry FIFO (67.5%). Evidence is `logs/p384/repetitions.json`. The separate uninstrumented baseline completed all 40,000 blocks in 36.364805 s replay wall / 37.733447 s CPU (`baseline-recent.json` in the same directory).

Use `speed.lisp` in this directory for the uncached arithmetic microbenchmark; its default context does not memoize, so repeated input cannot turn that measurement into a cache-hit timing. The generic baseline was 1,476.173 µs CPU / 1,475.947 µs wall and 2,930,154 bytes per call over 1,000 calls.

The release native kernel measured **198.248 µs CPU / 198.261 µs wall, zero bytes consed** over 1,000 uncached calls with scratch (7.45× less CPU than the generic baseline). A first full-product Montgomery version took 470.645 µs; carry-safe seven-word CIOS plus register-constrained word MACs reduced it to 248.148 µs. Binary inversion and special all-ones modulus products reduced width-8 G to 205.480 µs; width 12 measured 200.839 µs and width 14 measured 198.248 µs. The final read-only G table is 589,824 bytes shared across contexts; mutable arithmetic storage remains caller-owned. These are CPU and wall observations on the shared Apple Silicon host, not cache-hit timings.

Recent isolation (`828674f` versus parent `4c2db4d`, B-A-A-B order): B wall
16.763/17.243 s, CPU 22.388/22.852 s; A wall 29.809/30.797 s, CPU
35.525/35.349 s. Every root, gas, receipt and bloom matched for all 40,000
blocks (18,733 transactions, 3,768,403,621 gas). Mean wall/CPU fell 43.9%/36.2%;
allocation fell 72.8%, while peak RSS and GC increased in these observations.
Full process/replay metrics are in [BENCH](../../../../docs/BENCH.md#native-p-384-and-repeated-nitro-signatures)
and `logs/p384/recent-{A1,A2,B1,B2}/replay.json`.

The real portable field source branch was also recompiled with `:arm64`
disabled; all 28 P384 tests passed, including uncached, cold-cache and warm-cache
zero-consing properties. This tests that arithmetic path on the host, not a
claim of execution on a different CPU architecture.

## Fixtures

- Every one of the 280 C2SP Wycheproof P384/SHA384/P1363 records is retained in `wycheproof.sexp`, with original tcId and wire-validity expectation, public coordinates, r/s and the SHA-384 digest. Source: <https://github.com/C2SP/wycheproof/blob/main/testvectors_v1/ecdsa_secp384r1_sha384_p1363_test.json>, retrieved 2026-11-21; raw JSON SHA-256 `e27344bf6daae75fa19663620883809f91c79fc4b4420eff9eae8036b34476be`. Messages were hashed independently with Python standard-library `hashlib.sha384` while preparing fixture data, not at test/runtime. Signatures are split into equal-width halves, preserving oversized scalars rather than truncating them. No record or original expected result was removed or rewritten.
- `wire-boundaries.sexp` preserves all 19 non-96-byte encodings as `(tcId byte-length valid-equivalent-tcId)`. Ten short encodings are numerically identical to separately valid 96-byte records: they are valid *mathematical* signatures but invalid P1363 encodings. Tests prove that identity against the referenced valid row, test the integer API accordingly, and compose the mandatory caller-side 96-byte check to retain every original wire-level rejection. The integer-only API cannot distinguish a short encoding from its correctly padded counterpart.
- Embedded known answers are the NIST P-384 SHA-384 `sample` and `test` signatures and public key from [RFC6979 appendix A.2.6](https://www.rfc-editor.org/rfc/rfc6979#appendix-A.2.6). Their already-computed digests were independently recorded with `hashlib.sha384`.
- Curve parameters are [SEC 2 secp384r1](https://www.secg.org/sec2-v2.pdf), the NIST P-384 curve. No reference implementation code was incorporated.

Tests exercise every Wycheproof record, high-/low-s twins of valid signatures, both RFC known answers, modified signatures/digests, scalar/key/digest-length boundaries, native carry/alias/group properties, and positive-cache isolation/mutation/eviction cases. Run through `scripts/verify.sh` or load `clempo-crypto-ec/tests` and run tests with package prefix `CLEMPO.P384` (including the field-test package).

## Word backend portability

secp256k1, P256, P384 and BLS381 share `clempo.ec.word`'s carry, no-borrow and
high/low multiplication contract. Its reader probes optional SBCL symbols with
`find-symbol`, never interns a missing name into a locked implementation package.
Recognized 64-bit ARM64/x86-64 intrinsics expand directly when present; otherwise
real Common Lisp 32-bit-half arithmetic is compiled. Compiler-only intrinsics
need not be `fboundp`. Outgoing subtraction carry is one for **no** borrow on
both backends. Native ARM64 VOPs remain reader-gated by `#+arm64`; non-ARM64
compiles the arithmetic fallback. ARM-only opcode and
register names use `find-symbol` rather than foreign-package reader tokens, so
the layer check can inspect both branches even when the ARM64 package is absent.
Coordinate import and export still require 64-bit SBCL bignum digits; this is not
32-bit SBCL support.
