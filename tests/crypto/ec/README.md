# Elliptic-curve and modular-exponentiation layer

System: `clempo-crypto-ec`; tests: `clempo-crypto-ec/tests`. All arithmetic is pure Lisp on SBCL 2.6.8. There is no external crypto library, FFI, or subprocess in production. Gas charging and EVM/Tempo signature framing are the caller's responsibility.

## Prime curves

- `clempo.secp256k1:ecrecover(hash32,r,s,recid)` returns 64-byte x||y or NIL; scalar ranges are strict and high-s is accepted. Recovery IDs are 0..3, not 27/28. Ethereum's caller accepts only v=27/28 and subtracts 27.
- `private->public(private-integer)` returns x||y or NIL; `pubkey->address(pub64)` returns the low 160 bits of Keccak256 as an integer.
- `sign(hash32,private-integer)` returns r, low-s, recid using RFC6979/HMAC-SHA256. Hash inputs are already 32-byte digests; no implicit rehash.
- `ecdh(private-integer,peer64)` validates the peer and returns the raw 32-byte shared x, not libsecp256k1's hashed ECDH output.
- `clempo.p256:p256-verify(hash32,r,s,qx,qy)` has EIP-7951 semantics, including high-s acceptance and x modulo n comparison. `p256-verify-low-s` adds Tempo's transaction/WebAuthn requirement s<=n/2 (`crates/primitives/src/transaction/tt_signature.rs::verify_p256_signature_internal`). The caller constructs the pre_hash or WebAuthn digest.

Coordinates and scalars remain plain integers at every API boundary. `define-prime-curve` generates specialised Jacobian formulas, signed-window multiplication, and batch-normalised odd tables. P256 uses native SBCL bignum arithmetic. Secp256k1 additionally uses the GLV endomorphism (four approximately 128-bit components for recovery), a fixed square-root addition chain, and macro-generated four-word field operations. The field operators use unboxed 64-bit SBCL digit intrinsics internally, return a canonical integer, and allocate only the resulting integer; there are no temporary limb vectors or mutable shared scratch buffers. Odd-point tables require one field inversion.

This choice was measured rather than assumed. For one million dependent 256-bit products on this Apple Silicon host:

| Reduction strategy | CPU time | Allocation |
|---|---:|---:|
| Generic bignum folds using p=2^256-2^32-977 | 195.9 ms | 784.0 MB |
| Native bignum multiplication + MOD | 182.4 ms | 344.0 MB |
| Native multiplication + unboxed-word folds | 58.6 ms | 136.0 MB |

`sb-sprof` then showed multiplication, zeroing and normalisation dominating recovery. Generating the four-word product and field add/subtract/scale directly removed the temporary product and most normalisation calls. Recovery improved from 734.7 us CPU / 2.67 MB per call (initial bignum/window implementation) to 89.3 us CPU / 295.8 KB per call (10,000-call release run). SBCL 2.6.8 arm64's four-argument `%multiply-and-add` VOP can alias an input with its high output; the implementation deliberately uses the three-argument VOP plus a carry addition instead. Tests cover carries, canonical boundaries, 10,000 independent deterministic product/reduction comparisons and GLV identities.

**Secret-key operations are variable-time**, including bignum inversion, signed windows and table access. This is not a side-channel-hardened signing/HSM implementation; do not expose a long-lived secret to a hostile co-resident timing observer.

## Pairing curves and KZG

- `clempo.bn254:bn254-add`, `bn254-mul`: byte inputs, 64-byte output or NIL. `bn254-pairing`: byte input, `(values boolean valid-p)`.
- `clempo.bls12-381`: G1/G2 arithmetic, strict compressed and EIP-2537 encodings, seven EIP-2537 byte APIs, RFC9380 hash-to-curve, commonware MinSig/MinPk signatures, POP, aggregation, randomized batching and threshold recovery.
- `clempo.kzg:point-evaluation(input192)` returns the EIP-4844 64-byte result or NIL. `verify-kzg-proof(commitment48,z32,y32,proof48)` returns a boolean and signals `invalid-kzg-input` for malformed inputs. Only the required G2 tau setup constant is shipped.

`clempo.modexp:modular-expt(base,exponent,modulus)` takes integers (nonnegative exponent, positive modulus) and uses a 1–6-bit sliding window. `modexp(base-octets,exponent-octets,modulus-octets)` returns exactly the modulus byte length, including leading zeros; a zero modulus produces zero bytes of that length. EIP-198 header parsing, right padding and gas belong to the caller.

See `bn254/README.md`, `bls12-381/{README,hash-README,signature-README}.md`, `kzg/README.md`, and `data/README.md` for exact contracts, validity rules and fixture/setup provenance. All source uses immutable mathematical points; pairing accumulators and MSM buckets are call-local, so independent calls can run concurrently.

## Reproduction

```
scripts/verify.sh
CLEMPO_BUILD=release sbcl --dynamic-space-size 8192 --non-interactive \
  --load scripts/build.lisp --eval '(asdf:load-system "clempo-crypto-ec")' \
  --load tests/crypto/ec/benchmark.lisp
```

The benchmark checks each operation once before timing. Pairing and MSM timings include byte decoding and subgroup validation; MinSig verification includes compressed decoding and hash-to-curve. The 128-point MSM uses distinct generator multiples and full-width deterministic scalar inputs, not small scalars or infinities.

Measured release results (same benchmark invocation, warm call before each timed batch):

| Operation | Wall / call | CPU / call | Allocation / call |
|---|---:|---:|---:|
| secp256k1 recovery | 87.74 us | 87.58 us | 295.4 KB |
| P256 verification | 636.70 us | 636.65 us | 1.51 MB |
| BN254 pairing, two pairs | 9.974 ms | 9.971 ms | 27.27 MB |
| BLS12-381 pairing, two pairs | 24.718 ms | 24.714 ms | 60.06 MB |
| BLS G1 MSM, 128 points | 205.979 ms | 205.696 ms | 391.68 MB |
| BLS MinSig verification | 61.807 ms | 61.794 ms | 163.52 MB |

The cold strict full-repository gate after integration passed 173 tests, zero
failures/skips, and zero layer violations. Pairing fields currently use native
bignums; unlike the secp field they still allocate arithmetic intermediates.
