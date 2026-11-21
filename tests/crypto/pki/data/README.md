# Test vectors

- `gcm-nist.sexp`: NIST CAVP `gcmtestvectors.zip` (gcmEncryptExtIV128/256, gcmDecrypt128/256),
  https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/mac/gcmtestvectors.zip;
  96-bit IVs, 128-bit tags, the first three cases of every length section.
- `rsa_*.sexp`: Wycheproof (C2SP/wycheproof, `testvectors_v1/<name>.json`), every test case,
  reduced to `(tcId hash salt-length n e msg sig result)`.

X.509 parsing and validation are tested over real chains in `tests/net/`.
