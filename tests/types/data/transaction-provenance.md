# Transaction fixtures

`transaction-rpc.sexp` is a lossless projection of the recorded RPC data in
`~/clempo-infra/goempo-ref/internal/types/testdata/txs.jsonl` (data only, no Go source).
Rows contain label, raw EIP-2718 envelope, expected transaction hash, sender, fee payer, and key ID.

- eth-0x0: moderato block 1891911, 0xf7a97b3650dd69c7cc13591a27748af329c6e9282579b2e0b4e1d1663e32d979
- eth-0x2: moderato block 5854254, 0xe1a9a39cf5aa6100ed763f31ce8013fed449823e08ed962962d41ef84c050775
- eth-0x4: moderato block 4063501, 0x5e303d89105553b4ef813e420656b880b0040e8ec8bf4b7f71246086a9bfa7d5
- legacy-system: mainnet block 1, 0x6c80b11764e6020da93126a323b29596b342d2697c54ffc550645924da53e67c
- tempo/p256/keychain-v1: moderato block 4688660, 0xb8222c5e0a450d5ec9657dedb3c72a04c4138b5370f219ac2681e3b91fc9f69c
- tempo/p256/keychain-v1/keyauth-webAuthn: moderato block 2843742, 0x184da1f383fa65579939583a397cf8687082e741c264a17ce0b819137ef5543d
- tempo/p256/keychain-v2: moderato block 19924451, 0x7855fdb1b4695f5cd17c89b0348a1e77dc4aa747b2f4b67633496f8ff4b5a7eb
- tempo/p256/keychain-v2/sponsored: mainnet block 36083507, 0x1f32d200e931ab76144d59819fe17149bce7ae6c24f5b789b666f937516e62b8
- tempo/secp256k1: moderato block 7918190, 0x204fa727698f1a8053b79f8add2c0d681a43033ff20d11ec11cf795feed6f6af
- tempo/secp256k1/aa-auth: moderato block 43671736, 0xea11f0bdfbddca9427fcda83dc24b51854b2d446515dca3cd10c6ada690d3dc2
- tempo/secp256k1/keyauth-secp256k1: moderato block 20573626, 0x6341d7357f8e369125bd2929f7f820a229d0d426dbb25fe19329a3988e417d81
- tempo/secp256k1/keychain-v2: mainnet block 26667129, 0xc326d6c4135a663ec429dc15c1f2bfb5ea2e601a6feb796b6b319f8ebb751522
- tempo/secp256k1/keychain-v2/keyauth-secp256k1: moderato block 20309103, 0xe28076fcbca3d0249a35581d59719ed06db97582729c7aa9e71183a9adcf2a0d
- tempo/secp256k1/keychain-v2/sponsored: mainnet block 26667129, 0x4c63b59e891a39bea824ccd8b52d7a4b9ecdadfa76674fa4586750dafee598e0
- tempo/secp256k1/keychain-v2/sponsored/keyauth-secp256k1: moderato block 34549933, 0xb872c4f8969ef9e3cc119f385080582ae40fb415e2eff59ddca4467ad14bd037
- tempo/secp256k1/keychain-v2/sponsored/keyauth-webAuthn: mainnet block 10123550, 0xc4d023cc0296f42abb0bf55679f096bb3a994f80bc5c997643149830c9813383
- tempo/secp256k1/sponsored: mainnet block 41392933, 0x8d33060072af3b27df9ede9ea13bef5d335816c7ecc98c2bb374b17d0e137f96
- tempo/webAuthn: moderato block 4688661, 0xf12140bfb60cdd3ee045a6cd5ab4d5f93c5920f4a6aa9d5ff788d7d9e4dee671
- tempo/webAuthn/sponsored: moderato block 21599013, 0x8360810994ef6699661440e99549e14a7cf178753187284315259e311c9e30e5

`transaction-2930.sexp` records the signed envelope and signing preimage from
alloy-consensus 2.5.0 `src/transaction/eip2930.rs::test_decode_create`
and alloy-primitives 1.7.3 `src/signature/sig.rs::test_signature`; that vector is an
upstream encode/decode fixture, not a real-world signed transaction.
The EIP-155 example comes from https://eips.ethereum.org/EIPS/eip-155 . The pre-155
test intentionally varies its v/r/s to check the six-field signing preimage only.

Implementation wire layouts and sponsorship preimages follow
`~/dev/tempo/crates/primitives/src/transaction/{tempo_transaction,tt_signed,envelope}.rs`.
Legacy system transactions use the zero-r/zero-s/even-parity marker and zero sender.
Transaction fields expose decoded RLP strings/lists; address and key results are integers.
Encoding reconstructs those fields, while hashing retains the exact received envelope.

`define-transaction-layouts` generates public `transaction-<field>` readers from
the named wire layouts. Chain ID, nonce, gas limit, gas price, maximum fees,
destination (`transaction-to`), value, nonce key, validity bounds, and fee token
are integers. Absent fields, contract-creation destinations, unprotected legacy
chain IDs, and absent validity bounds return NIL. Calls, access lists, and
authorization lists retain decoded RLP lists; input is an octet vector.
