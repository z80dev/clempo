# BLS12-381 hash-to-curve fixtures

`hash-vectors.sexp` contains all 220 records from these DATA files in
`~/clempo-infra/goempo-ref/internal/crypto/bls12381/testdata/`:

- `bls12_381_h2c_g1.json` and `bls12_381_h2c_g2.json`: RFC 9380
  BLS12381G1/G2_XMD:SHA-256_SSWU_RO_ vectors, including their exact QUUX DSTs.
- `blsMapG1.json.gz` and `blsMapG2.json.gz`: EIP-2537 map vectors. Inputs and
  expected outputs retain the original 64-byte-per-Fp hex encoding.

Only JSON/gzip decoding and number/string transcription were used to construct
this fixture; no reference implementation was executed. Every record is consumed
by `hash-tests.lisp`. Additional tests cover zero and boundary field elements and
verify that cofactor-cleared outputs are annihilated by the subgroup order.

The implementation follows RFC 9380 sections 5.3.1, 5.4.1, 6.6.2, 8.8.1,
8.8.2 and appendix E. Polynomial constants were read as ordinary field values
from Rust `ark-bls12-381` 0.6.0 `g1_swu_iso.rs` and `g2_swu_iso.rs`.
The map/isogeny/cofactor composition was cross-read against blst 0.3.17
`blst/src/map_to_g2.c`. No Go implementation code was read.

## Component interface

Load `src/crypto/ec/bls12-381-hash.lisp` after the BLS core and SHA-256.
It depends only on `clempo.bls12-381` and `clempo.sha2:sha256`.

- `(map-fp-to-g1 integer)` reduces modulo p and returns a cofactor-cleared point.
- `(map-fp2-to-g2 (cons real imaginary))` reduces each component modulo p.
- `(hash-to-g1 message &key dst)` and `(hash-to-g2 message &key dst)` take
  octet vectors. Defaults are the respective RFC suite identifiers (not a BLS
  signature protocol's DST). Protocol callers must supply their protocol DST.
- `(expand-message-xmd message dst length)` returns SHA-256 XMD octets;
  output length must be 1..8160 and oversized DSTs use RFC DST hashing.

Points use the core's affine cons representation, with NIL for infinity.
Core scalar multiplication must retain the complete scalar, not reduce modulo r:
cofactor clearing operates on points outside the prime-order subgroup.

`map-g1-precompile` and `map-g2-precompile` take exactly 64/128 bytes and return
`(values output-octets t)` or `(values nil nil)` for invalid length, nonzero
padding, or a noncanonical field element. G2 input is real then imaginary.
The tests exercise both wrappers against every EIP map vector and malformed
length, padding, and field-boundary inputs.

No builds, tests, linters or external cryptographic runtimes were run for this
change. The integrating agent should run the strict crypto build and tests,
including all RFC and EIP map fixtures, after registering these components.
