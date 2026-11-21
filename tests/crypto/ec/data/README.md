# Prime-curve and modular-exponentiation fixtures

Only data were copied from goempo's repository, `internal/crypto/`.

- `secp256k1/tempo_mainnet_senders.txt`: 24 legacy/EIP-1559 mainnet transactions from blocks 0x2000000–0x2f19028, recorded from rpc.tempo.xyz.
- `secp256k1/eest_senders.txt`: 160 Ethereum execution-spec-tests transactions (legacy, EIP-155, types 1/2) and 40 EIP-7702 authorizations.
- `secp256k1/ecdh.txt`: six independent affine Python ECDH cases, including private scalar n−1.
- `p256/eest_p256verify.txt`: decompressed from `eest_p256verify.txt.gz`, 379 EIP-7951 execution-spec-tests cases (including Wycheproof and modular comparison).
- `modexp/*.json`: go-ethereum precompile vectors fetched 2026-11-17, EIP-2565/EIP-7883; only the arithmetic outputs are used here, not gas.

The deterministic secp256k1 known answer embedded in `prime-curves.lisp` is the vector in RustCrypto k256 0.13.4 `src/ecdsa.rs::ethereum_end_to_end_example` (original ethers-rs vector linked there). Its private key, signing payload, signature and recovery ID are test data, not implementation code.
