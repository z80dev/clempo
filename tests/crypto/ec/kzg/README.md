# clempo.kzg tests and fixtures

Code: `src/crypto/ec/kzg.lisp` (package `clempo.kzg`). Tests: `kzg-tests.lisp` (package `clempo.kzg.tests`).
Depends on `clempo.bls12-381` (curve/pairing; `g1-decompress`, `g2-decompress`, `g1/g2-add/neg/mul`,
`g1/g2-generator`, `pairing-check`, `+r+`, condition `invalid-point`) and `clempo.sha2:sha256`.
No field arithmetic lives in kzg.lisp.

## API

Representation: plain integers for scalars, `octets` for all byte inputs/outputs; points are whatever
`clempo.bls12-381` returns (NIL = infinity).

| Symbol | Meaning |
|---|---|
| `point-evaluation input` | EIP-4844 precompile 0x0a. Input = versioned_hash(32) ‖ z(32) ‖ y(32) ‖ commitment(48) ‖ proof(48). Returns 64-octet `FIELD_ELEMENTS_PER_BLOB ‖ BLS_MODULUS` on success, NIL on any failure (length ≠ 192, hash mismatch, z or y ≥ r, invalid/off-subgroup/non-canonical point, false proof). Caller charges `+point-evaluation-gas+` (50000) and treats NIL as precompile failure. |
| `verify-kzg-proof commitment z y proof` | c-kzg `verify_kzg_proof`: T/NIL for a well-formed proof; signals `invalid-kzg-input` for non-canonical scalars, wrong lengths or invalid G1 encodings. Checks `e(C − [y]G1, −G2) · e(π, [τ−z]G2) = 1`. |
| `kzg-to-versioned-hash commitment` | `0x01 ‖ sha256(commitment)[1:]`. |
| `point-evaluation-return-value` | the fixed 64-octet success output. |
| `tau-g2`, `+tau-g2-hex+` | [τ]G2 (lazily decompressed, cached) and its compressed hex. |
| `+versioned-hash-version-kzg+` 1, `+field-elements-per-blob+` 4096, `+point-evaluation-input-length+` 192, `+point-evaluation-gas+` 50000 | constants (EIP-4844). |

## Constant provenance

`+tau-g2-hex+` is line 4100 of `src/trusted_setup.txt` in crate `c-kzg-2.1.8`
(`~/.cargo/registry/src/index.crates.io-*/c-kzg-2.1.8/src/trusted_setup.txt`), the Ethereum mainnet
KZG ceremony output. File layout: line 1 = 4096 (G1 count), line 2 = 65 (G2 count), lines 3–4098 G1
Lagrange points, then 65 monomial G2 points [τ^i]G2: line 4099 = G2 generator, line 4100 = [τ]G2.
Only [τ]G2 is needed to verify a single-point proof. BLS modulus r =
`0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001`.

## Fixtures (data only, copied from `~/clempo-infra/goempo-ref/internal/crypto/kzg/testdata/`)

- `pointEvaluation.json`: go-ethereum `core/vm/testdata/precompiles` (master, fetched 2026-11-17); one valid vector.
- `eest_point_evaluation.txt`: 125 point-evaluation (0x0a) inputs from ethereum/execution-spec-tests develop
  `state_tests/cancun/eip4844_blobs` (contract calls 0x0a with 50000 gas, stores success and returndatasize).
  Columns: call success, returndatasize, input hex. 44 succeed (returndatasize 64), 81 fail.

Hand-written invalid cases in `kzg-invalid-cases`: wrong lengths, bad version byte, bad hash, wrong z/y,
corrupted proof, z = r / y = r (non-canonical), cleared compression flag on commitment and proof.
